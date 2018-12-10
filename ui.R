library(DT)

qsiTab <- sidebarLayout(
    sidebarPanel(
        fileInput("file1", "Choose CSV File", accept=c("text/csv", ".csv")),
        fluidRow(
            column(6,
                h5("Enter X:")
            ),
            column(6,
                checkboxInput("qsiCheckBox", "Verbose", TRUE)
            )
        ),
        fluidRow(
            column(6,
                   uiOutput("qsiNumInput")
            ),
            column(6,
                   actionButton("estimateQsi", "Estimate")
            )
        ),
        uiOutput("qsiNumSlider"),
        uiOutput("qsiHelpText"),
        DTOutput("qsiTable")
    ),
    mainPanel(
        plotOutput("qsiPlot"),
        verbatimTextOutput("qsiOutput")
    )
)

prTab <- sidebarLayout(
    sidebarPanel(
        fileInput("file2", "Choose CSV File", accept=c("text/csv", ".csv")),
        fluidRow(
            column(6,
                   h5("Enter X:")
            ),
            column(6,
                   checkboxInput("prCheckBox", "Verbose", TRUE)
            )
        ),
        fluidRow(
            column(6,
                   uiOutput("prNumInput")
            ),
            column(6,
                   actionButton("estimatePr", "Estimate")
            )
        ),
        uiOutput("prNumSlider"),
        uiOutput("prDegreeSlider"),
        uiOutput("prHelpText"),
        DTOutput("prTable")
    ),
    mainPanel(
        plotOutput("prPlot"),
        verbatimTextOutput("prOutput")
    )
)

ui <- navbarPage(
    "Numerical Methods",
    inverse = TRUE,
    tabPanel("Quadratic Spline Interpolation", qsiTab),
    tabPanel("Polynomial Regression", prTab),
    tabPanel("Simplex")
)