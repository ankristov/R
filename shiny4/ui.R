#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
    titlePanel("Visualize Many Models"),
    sidebarLayout(
        sidebarPanel(
            h3("Slope"),
            textOutput("slopeOut"),
            h3("Intercept"),
            textOutput("interceptOut")
        ),
        mainPanel(
            plotOutput("plot1", brush = brushOpts(
                id = "brush1"
            ))
        )
    )
))   


