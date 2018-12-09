library(DT)

qsiTab <- sidebarLayout(
    sidebarPanel(
        fileInput("file1", "Choose CSV File", accept=c("text/csv", ".csv")),
        uiOutput("qsiNumInput"),
        uiOutput("qsiNumSlider"),
        fluidRow(
            column(6,
                actionButton("estimateQsi", "Estimate")
            ),
            column(6,
                checkboxInput("qsiCheckBox", "Verbose", TRUE)
            )
        ),
        uiOutput("qsiHelpText"),
        DTOutput("qsiTable")
    ),
    mainPanel(
        verbatimTextOutput("qsiOutput"),
        plotOutput("qsiPlot")
    )
)

ui <- navbarPage(
    "CMSC 150",
    inverse = TRUE,
    tabPanel("Quadratic Spline Interpolation", qsiTab),
    tabPanel("Polynomial Regression"),
    tabPanel("Simplex")
)