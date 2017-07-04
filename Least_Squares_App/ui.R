library(shiny)

# Define UI
shinyUI(fluidPage(
     tags$head(
          tags$style(HTML('#new{background-color:SandyBrown}'),
                     HTML('#setm{background-color:LightGreen}'),
                     HTML('#setb{background-color:LightGreen}'),
                     HTML('body {background-color:Beige;}'))
     ),
     titlePanel("Least Squares Fitting"),
     
     sidebarLayout(
          # Sidebar
          sidebarPanel(
               "Change the slope and intercept of the green line:",
               sliderInput("slope", label="Slope:", min=-6, max=6, value=0, step=.1),
               sliderInput("intercept", label="Intercept:", min=-3, max=3, value=0, step=.1),
               fluidRow(
               actionButton(inputId = "setm", label="Set Slope"),
               actionButton(inputId = "setb", label="Set Intercept")
               ),
               HTML("<HR>"),
               fluidRow(
                    actionButton(inputId = "new", label="New Data")
               )
          ),
          # Main
          mainPanel(
               plotOutput("scatterplot"),
               "Errors (distance of data points from the line) are shown in grey.",
               textOutput("error"), 
               HTML("<BR>"), HTML("<B>"),
               textOutput("minE"), HTML("</b>"),
               textOutput("bestparam")
          )
     )
))


