library(shiny)
source("./qsi.R")
source("./pr.R")

server <- function(input, output) {
    values <- reactiveValues(qsiInputtedCSV=FALSE, qsiMtrx=NULL, qsiVerbose=TRUE, qsiInputType=NULL, prInputtedCSV=FALSE, prMtrx=NULL, prVerbose=TRUE, prDegreeSlider=1, prDegreeSliderChanged=FALSE, prInputType=NULL)
    
    # Quadratic-Spline-Interpolation------------------------------------------------
    observeEvent(input$qsiCheckBox, {
        values$qsiVerbose <- input$qsiCheckBox
    })
    printQsiOutput <- function(func, args) {
        values$qsiPrint <- capture.output(do.call(func, args))
    }
    output$qsiOutput <- renderText({
        return(paste(values$qsiPrint, collapse="\n"))
    })
    observeEvent(input$file1$datapath, {
        if(is.null(input$file1$datapath)) return(NULL)
        err <- readQsiCSV(input$file1$datapath, values$qsiVerbose)
        if(err){
            values$qsiPrint <- "Error: Multiple values of Y are not allowed for each X."
            return(NULL)
        }
        tempMtrx <- t(mtrx)
        colnames(tempMtrx) <- c("X", "Y")
        values$qsiMtrx <- tempMtrx
        values$qsiInputtedCSV <- TRUE
    })
    output$qsiTable <- renderDT(values$qsiMtrx, selection='none', editable = T)
    output$qsiNumInput <- renderUI({
        if(values$qsiInputtedCSV) {
            numericInput("qsiNumInput", NULL, values$NumValueQsi, min=values$minQsi, max=values$maxQsi, step=0.01)
        } else {
            numericInput("qsiNumInputTemp", NULL, 1, min=1, max=2, step=0.01)
        }
    })
    output$qsiNumSlider <- renderUI({
        if(values$qsiInputtedCSV) {
            sliderInput("qsiNumSlider", "or Choose X:", value=values$NumValueQsi, min=values$minQsi, max=values$maxQsi, step=0.01)
        } else {
            sliderInput("qsiNumSliderTemp", "or Choose X:", value=1, min=1, max=2, step=0.01)
        }
    })
    output$qsiHelpText <- renderUI({
        if(!is.null(values$qsiMtrx)) {
            fluidRow(
                column(12, h3("Table Values")),
                column(12, helpText("Double click on a cell to edit."))
            )
        } else {
            helpText("Please choose a CSV file.")
        }
    })
    observeEvent(input$qsiNumInput, {
        values$qsiInputType <- "num"
    })
    observeEvent(input$qsiNumSlider, {
        values$qsiInputType <- "slider"
        if(!is.null(input$qsiNumSlider)) {
            printQsiOutput(getQSI, list(input$qsiNumSlider, values$qsiVerbose))
        }
    })
    proxy = dataTableProxy('qsiTable')
    observeEvent(input$qsiTable_cell_edit, {
        info = input$qsiTable_cell_edit
        i = info$row
        if(info$col == 0) j = "X"
        else j="Y"
        v = info$value
        values$qsiMtrx[i, j] <<- DT::coerceValue(v, values$qsiMtrx[i, j])
        replaceData(proxy, values$qsiMtrx, resetPaging = FALSE)
    })
    observeEvent(values$qsiMtrx, {
        values$minQsi <- min(values$qsiMtrx[,1])
        values$NumValueQsi <- min(values$qsiMtrx[,1])
        values$maxQsi <- max(values$qsiMtrx[,1])
        printQsiOutput(updateQsiMtrx, list(values$qsiMtrx, values$qsiVerbose))
    })
    observeEvent(input$estimateQsi, {
        if(!(is.null(input$qsiNumInput) || is.na(input$qsiNumInput))) {
            printQsiOutput(getQSI, list(input$qsiNumInput, values$qsiVerbose))
        }
    })
    output$qsiPlot <- renderPlot({
        if(is.null(values$qsiMtrx)) return(NULL)
        x <- values$qsiMtrx[, 1]
        y <- values$qsiMtrx[, 2]
        xVals <- seq(values$minQsi, values$maxQsi, 0.1)
        yVals <- sapply(xVals, getQSI, forPlot=TRUE)
        plot(x, y, xlab="X", ylab="Y", ylim=range(min(yVals), max(yVals)))
        lines(xVals, yVals, col="blue")
        if(!is.null(values$qsiInputType) && values$qsiInputType == "num") points(c(input$qsiNumInput), c(getQSI(input$qsiNumInput, forPlot=TRUE)), col="red", pch=16)
        else if(!is.null(values$qsiInputType) && values$qsiInputType == "slider") points(c(input$qsiNumSlider), c(getQSI(input$qsiNumSlider, forPlot=TRUE)), col="red", pch=16)
    })
    
    # Polynomial-Regression------------------------------------------------
    observeEvent(input$prCheckBox, {
        values$prVerbose <- input$prCheckBox
    })
    printPrOutput <- function(func, args)({
        values$prPrint <- capture.output(do.call(func, args))
    })
    output$prOutput <- renderText({
        return(paste(values$prPrint, collapse="\n"))
    })
    observeEvent(input$file2$datapath, {
        if(is.null(input$file2$datapath)) return(NULL)
        printPrOutput(readPrCSV, list(input$file2$datapath, values$prDegreeSlider, values$prVerbose))
        values$prEquation <- prEquation
        tempMtrx <- t(prMtrx)
        colnames(tempMtrx) <- c("X", "Y")
        values$prMtrx <- tempMtrx
        values$prInputtedCSV <- TRUE
        values$prInputType = "num"
    })
    output$prTable <- renderDT(values$prMtrx, selection='none', editable = T)
    output$prNumInput <- renderUI({
        if(values$prInputtedCSV) {
            numericInput("prNumInput", NULL, values$numValuePr, min=values$minPr, max=values$maxPr, step=0.01)
        } else {
            numericInput("prNumInputTemp", NULL, 1, min=1, max=2, step=0.01)
        }
    })
    output$prNumSlider <- renderUI({
        if(values$prInputtedCSV) {
            sliderInput("prNumSlider", "or Choose X:", value=values$numValuePr, min=values$minPr, max=values$maxPr, step=0.01)
        } else {
            sliderInput("prNumSliderTemp", "or Choose X:", value=1, min=1, max=2, step=0.01)
        }
    })
    output$prDegreeSlider <- renderUI({
        if(values$prInputtedCSV) {
            sliderInput("prDegreeSlider", "Degree:", 3, value=1, min=1, max=values$prMaxDegree, step=1)
        } else {
            sliderInput("prDegreeSliderTemp", "Degree:", value=1, min=1, max=2, step=1)
        }
    })
    observeEvent(input$prDegreeSlider, {
        values$prDegreeSlider <- input$prDegreeSlider
        values$prDegreeSliderChanged <- TRUE
    })
    output$prHelpText <- renderUI({
        if(!is.null(values$prMtrx)) {
            fluidRow(
                column(12, h3("Table Values")),
                column(12, helpText("Double click on a cell to edit."))
            )
        } else {
            helpText("Please choose a CSV file.")
        }
    })
    observeEvent(input$prNumInput, {
        values$prInputType <- "num"
    })
    observeEvent(input$prNumSlider, {
        values$prInputType <- "slider"
        if(values$prDegreeSliderChanged) {
            printPrOutput(updatePrMtrx, list(values$prMtrx, values$prDegreeSlider, values$prVerbose))
            values$prEquation <- prEquation
        }
        if(!is.null(input$prNumSlider)) printPrOutput(getPr, list(input$prNumSlider, values$prVerbose))
    })
    proxy = dataTableProxy('prTable')
    observeEvent(input$prTable_cell_edit, {
        info = input$prTable_cell_edit
        i = info$row
        j = info$col
        v = info$value
        if(info$col == 0) j = "X"
        else j="Y"
        values$prMtrx[i, j] <<- DT::coerceValue(v, values$prMtrx[i, j])
        replaceData(proxy, values$prMtrx, resetPaging = FALSE)
    })
    observeEvent(values$prMtrx, {
        values$minPr <- min(values$prMtrx[,1])
        values$numValuePr <- min(values$prMtrx[,1])
        values$maxPr <- max(values$prMtrx[,1])
        values$prMaxDegree <- dim(values$prMtrx)[1] - 1
        
    })
    observeEvent(input$estimatePr, {
        if(!(is.null(input$prNumInput) || is.na(input$prNumInput))) {
            if(values$prDegreeSliderChanged) printPrOutput(updatePrMtrx, list(values$prMtrx, values$prDegreeSlider, values$prVerbose))
            values$prEquation <- prEquation
            printPrOutput(getPr, list(input$prNumInput, values$prVerbose))
        }
        values$prDegreeSliderChanged <- FALSE
    })
    output$prPlot <- renderPlot({
        if(is.null(values$prMtrx)) return(NULL)
        dep <- values$prMtrx[, 1]
        indep <- values$prMtrx[, 2]
        xVals <- seq(min(values$prMtrx[,1]), max(values$prMtrx[,1]), 0.1)
        yVals <- sapply(xVals, values$prEquation)
        plot(dep, indep, xlab="X", ylab="Y")
        lines(xVals, yVals, col="blue")
        if(!is.null(values$prInputType) && values$prInputType == "num") points(c(input$prNumInput), c(prEquation(input$prNumInput)), col="red", pch=16)
        else if(!is.null(values$prInputType) && values$prInputType == "slider") points(c(input$prNumSlider), c(prEquation(input$prNumSlider)), col="red", pch=16)
    })
}