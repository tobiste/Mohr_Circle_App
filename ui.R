# Define UI for app that draws a histogram ----
fluidPage(

  # App title ----
  headerPanel(
    "Mohr circle"
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
      sliderInput(
        inputId = "pf",
        label = "Pore fluid pressure (MPa)",
        min = 0,
        max = 1200,
        value = 0
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
          label = "Slope",
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
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Interactive map ----
      plotOutput(outputId = "mohr", height = "600px")
    )
  )
)
