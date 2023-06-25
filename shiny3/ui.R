#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Example: Tabs"),
    sidebarLayout(
        sidebarPanel(
            textInput("text1", "Enter Tab 1 Text:", value = "Text for Tab1"),
            textInput("text2", "Enter Tab 2 Text:", value = "Text for Tab2"),
            textInput("text3", "Enter Tab 3 Text:", value = "Text for Tab3")
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Tab 1", br(), textOutput("out1")),
                        tabPanel("Tab 2", br(), textOutput("out2")),
                        tabPanel("Tab 3", br(), textOutput("out3"))
            )
        )
    )

))
