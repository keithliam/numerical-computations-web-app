library(shiny)
source("./qsi.R")

server <- function(input, output) {
    values <- reactiveValues(qsiInputtedCSV=FALSE, qsiMtrx=NULL, qsiVerbose=TRUE, qsiInputType=NULL)
    
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
            numericInput("qsiNumInput", "Enter X:", values$NumValueQsi, min=values$minQsi, max=values$maxQsi, step=0.01)
        } else {
            numericInput("qsiNumInputTemp", "Enter X:", 1, min=1, max=2, step=0.01)
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
        if(!(is.null(input$qsiNumInput) || is.na(input$qsiNumInput)) || !is.null(input$qsiNumSlider)) {
            if(values$qsiInputType == "num") printQsiOutput(getQSI, list(input$qsiNumInput, values$qsiVerbose))
            else printQsiOutput(getQSI, list(input$qsiNumSlider, values$qsiVerbose))
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
}