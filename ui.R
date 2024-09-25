# Define UI for app that draws a histogram ----
fluidPage(

  # App title ----
  headerPanel(
    "Mohr Circle"
  ),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      h4("Stress magnitudes"),
      sliderInput(
        inputId = "S1",
        label = "Sigma 1 (MPa)",
        min = -100,
        max = 1200,
        value = 1025
      ),
      sliderInput(
        inputId = "S2",
        label = "Sigma 2 (MPa)",
        min = -100,
        max = 1200,
        value = 400
      ),
      sliderInput(
        inputId = "S3",
        label = "Sigma 3 (MPa)",
        min = -100,
        max = 1200,
        value = 250
      ),
      fluidRow(
        h4("Mean and differential stress"),
        checkboxInput(inputId = "useSM", label = "Use mean and differential stress?", value = FALSE),
        sliderInput(
          inputId = "SM",
          label = "Mean stress (MPa)",
          min = -100,
          max = 1200,
          value = 638
        ),
        sliderInput(
          inputId = "SD",
          label = "Differential stress (MPa)",
          min = 0,
          max = 1200,
          value = 775
        ),
      ),
      fluidRow(
        h4("Pore fluid pressure"),
        sliderInput(
          inputId = "pf",
          label = "Pore fluid pressure (MPa)",
          min = 0,
          max = 1200,
          value = 0
        )
      ),
      fluidRow(
        h4("Coulomb criteria"),
        sliderInput(
          inputId = "coulomb1",
          label = "Cohesion",
          min = -500,
          max = 500,
          value = 70,
          round = FALSE,
          step = 1
        ),
        sliderInput(
          inputId = "coulomb2",
          label = "Coefficient of sliding friction",
          min = 0,
          max = 2,
          value = .6,
          round = FALSE,
          step = 0.01
        ),
        sliderInput(
          inputId = "sliding",
          label = "Sliding criteria",
          min = 0,
          max = 2,
          value = .81,
          round = FALSE,
          step = 0.01
        )
      ),
      fluidRow(
        h4("Input range"),
        sliderInput(
          inputId = "x_range",
          label = "Normal stress",
          min = -100,
          max = 1300,
          value = c(-100, 1300)
        ),
        sliderInput(
          inputId = "y_range",
          label = "Shear stress",
          min = -750,
          max = 750,
          value = c(0, 500)
        )
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Interactive map ----
      plotOutput(outputId = "mohr", height = "600px")
    )
  ),
  h4("Tobias Stephan (2024)",
    style = "position: absolute; bottom: 0;right:0;"
  )
)
