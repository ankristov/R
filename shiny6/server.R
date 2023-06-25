#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(UsingR)
data(galton)
head(galton)

shinyServer(
    function(input, output) {
        output$newHist <- renderPlot({
            hist(galton$child, xlab = 'child height', col = 'lightblue',
                 main = 'Histogram')
            mu <- input$mu
            mu_true <- mean(galton$child)
            lines(c(mu, mu), c(0, 200), col = 'red', lwd=5)
            lines(c(mu_true, mu_true), c(0,200), col = 'grey', lwd = 1)
            mse <- mean((galton$child - input$mu)^2)
            text(63,150, paste("mu = ", input$mu))
            text(63,140, paste("MSE = ", round(mse,2)))
        })

    

})
