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
    titlePanel("Plot Random Number"),
    sidebarLayout(
        sidebarPanel(
            numericInput("n", "How many random numbers should be plotted?",
                         value = 1000, min = 1, max = 1000, step = 1),
            sliderInput("sliderX", "Pick Minimum and Maximum X Values",
                        -100, 100, value = c(-50,50)),
            sliderInput("sliderY", "Pick Minimum and Maximum Y Values",
                        -100, 100, value = c(-50,50)),
            checkboxInput("show_xlab", "Show/Hide X Axis Label", value = T),
            checkboxInput("show_ylab", "Show/Hide Y Axis Label", value = T),
            checkboxInput("show_title", "Show/Hide Title")
        ),
        mainPanel(
            h3("Graph of Random Points"),
            plotOutput("plot1")
        )
    )
))