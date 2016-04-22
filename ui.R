library(shiny)
library(Rcpp)




shinyUI(fluidPage(
  titlePanel( title="Predicted Healthcare Costs", windowTitle="Predicted Healthcare Costs"),
  sidebarLayout(
    sidebarPanel(("By: Richard Kennedy"),
                 numericInput("age", "Enter your age",""),
                 tableOutput("percent_table")
    ),
    mainPanel( plotOutput("uninsured_plot"), plotOutput("insured_plot")
    ))
)
)  