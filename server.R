library(shiny)
source("./qsi.R")

server <- function(input, output) {
    values <- reactiveValues(inputted=FALSE, qsiMtrx=NULL, qsiVerbose=TRUE)
    
    observeEvent(input$qsiCheckBox, {
        values$qsiVerbose <- input$qsiCheckBox
    })
    printOutput <- function(func, args)({
        values$print <- capture.output(do.call(func, args))
    })
    output$qsiOutput <- renderText({
        return(paste(values$print, collapse="\n"))
    })
    observeEvent(input$file1$datapath, {
        if(is.null(input$file1$datapath)) return(NULL)
        values$qsiMtrx <- readCSV(input$file1$datapath, values$qsiVerbose)
        values$inputted <- TRUE
    })
    output$qsiTable <- renderDT(values$qsiMtrx, selection='none', editable = T)
    output$qsiNumInput <- renderUI({
        if(values$inputted) {
            numericInput("qsiNumInput", "Enter X:", values$numValue, min=values$min, max=values$max, step=0.01)
        } else {
            numericInput("qsiNumInputTemp", "Enter X:", 1, min=1, max=2, step=0.01)
        }
    })
    output$qsiNumSlider <- renderUI({
        if(values$inputted) {
            sliderInput("qsiNumSlider", "or Choose X:", value=values$numValue, min=values$min, max=values$max, step=0.01)
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
        j = info$col
        v = info$value
        values$qsiMtrx[i, j] <<- DT::coerceValue(v, values$qsiMtrx[i, j])
        replaceData(proxy, values$qsiMtrx, resetPaging = FALSE)
    })
    observeEvent(values$qsiMtrx, {
        values$min <- min(values$qsiMtrx[,1])
        values$numValue <- min(values$qsiMtrx[,1])
        values$max <- max(values$qsiMtrx[,1])
        printOutput(updateQsiMtrx, list(values$qsiMtrx, values$qsiVerbose))
    })
    observeEvent(input$estimateQSI, {
        if(!(is.null(input$qsiNumInput) || is.na(input$qsiNumInput)) || !is.null(input$qsiNumSlider)) {
            if(values$qsiInputType == "num") printOutput(getQSI, list(input$qsiNumInput, values$qsiVerbose))
            else printOutput(getQSI, list(input$qsiNumSlider, values$qsiVerbose))
        }
    })
}