install.packages("shiny")
library(shiny)

# Shiny Gadgets: simple gadget
library(miniUI)

myFirstGadget <- function(){
  ui <- miniPage(
    gadgetTitleBar("My First Gadget")
  )
  server <- function(input, output, session) {
    observeEvent(input$done, {stopApp()})
  }
  runGadget(ui, server)
}
myFirstGadget()

# Shiny Gadget: select numbers from two lists and output their product in console
library(miniUI)
multiplyNumbers <- function(n1, n2) {
  ui <- miniPage(
    gadgetTitleBar("Myltiply Two Numbers"),
    miniContentPanel(
      selectInput("num1", "First Number", choices = n1),
      selectInput("num2", "Second Number", choices = n2)
    )
  )
  server <- function(input, output, session) {
    observeEvent(input$done, {
      num1 <- as.numeric(input$num1)
      num2 <- as.numeric(input$num2)
      stopApp(num1 * num2)
    })
  }
  runGadget(ui, server)
}
multiplyNumbers(1:10, 1:10)

# Shiny Gadget: select points on a graph and output their values in console
pickTrees <- function(){
  ui <- miniPage(
    gadgetTitleBar("Select Points by Dragging your Mouse"),
    miniContentPanel(
      plotOutput("plot", height = "100%", brush = "brush")
    )
  )
  server <- function(input, output, session){
    output$plot <- renderPlot({
      plot(trees$Girth, trees$Volume, main = "Trees!",
           xlab = "Girth", ylab = "Volume")
    })
    observeEvent(input$done, {
      stopApp(brushedPoints(trees, input$brush,
                            xvar = "Girth", yvar = "Volume"))
    })
  }
  runGadget(ui, server)
}
pickTrees()

# GoogleVis
install.packages("googleVis")
library(googleVis)
M <- gvisMotionChart(Fruits, "Fruit", "Year",
                     options = list(width=600, height=400))
plot(M)
print(M, "chart")
print(M)
write(M, file = "gvis1.txt")
class(M)
write(unlist(M), file = "gvis1.html")

# gvisGeoChart
head(Exports)
dim(Exports)
G <- gvisGeoChart(data = Exports, locationvar = "Country",
                  colorvar = "Profit", options = list(width = 600, height = 400))
plot(G)
# specifying a region
G2 <- gvisGeoChart(data = Exports, locationvar = "Country",
                   colorvar = "Profit", options = list(width = 600,
                                                       height = 400,
                                                       region = "150"))
plot(G2)

# gvisLineChart
df <- data.frame(label = c("US", "GB", "BR"), val1 = c(1,3,4), val2 = c(23,12,32))
Line <- gvisLineChart(data = df, xvar = "label", yvar = c("val1", "val2"))
plot(Line)
Line1 <- gvisLineChart(data = df, xvar = "label", yvar = c("val1", "val2"),
             options = list(title = "Hello world", legend = "bottom",
                            titleTextStyle = "{color: 'red', fontSize:18}",
                            vAxis = "{gridlines:{color:'red', count:3}}",
                            hAxis = "{title: 'My Label', titleTextStyle:{color:'blue'}}",
                            series = "[{color:'green', targetAxisIndex:0},
                            {color:'blue', targetAxisIndex:1}]",
                            vAxes = "[{title: 'Value 1 (%)', format:'##,######%'},
                            {title: 'Value 2 (\U00A3)'}]",
                            curveType = "function", width=500, height=300)         
)
plot(Line1)

# Combine multiple plots together
G <- gvisGeoChart(data = Exports, locationvar = "Country", colorvar = "Profit",
                  options = list(width = 200, height = 100))
T1 <- gvisTable(data = Exports, options = list(width=200, height = 270))
M <- gvisMotionChart(data = Fruits, idvar = "Fruit", timevar = "Year", options=list(width=400, height=370))
GT <- gvisMerge(G, T1, horizontal = F); plot(GT)
GTM <- gvisMerge(GT, M, horizontal = T, tableOptions = "bgcolor=\"#CCCCCC\" cellspacing = 10)")
plot(GTM)

######################################
### Plotly
######################################
install.packages("plotly")
library(plotly)
plot_ly(mtcars, x = mtcars$wt, y = mtcars$mpg, mode = "markers")
plot_ly(x = mtcars$wt, y = mtcars$mpg, mode = "markers",
        color = as.factor(mtcars$cyl))
plot_ly(x = mtcars$wt, y = mtcars$mpg, mode = "markers",
        color = disp)
plot_ly(x = mtcars$wt, y = mtcars$mpg, mode = "markers",
        color = as.factor(mtcars$cyl), size = mtcars$hp)
# 3D Scatterplot
set.seed(2016-07-21)
temp <- rnorm(100, mean = 30, sd = 5)
pressure <- rnorm(100)
dtime <- 1:100
plot_ly(x = temp, y = pressure, z = dtime,
        type = "scatter3d", mode = "markers", color = temp)

# Line Graph
data("airmiles")
airmiles
time(airmiles)
plot_ly(x = time(airmiles), y = airmiles)

# Multiple Line Graph
library(plotly)
library(tidyr)
library(dplyr)
data("EuStockMarkets")
head(EuStockMarkets)
class(EuStockMarkets)
typeof(EuStockMarkets)
stocks <- as.data.frame(EuStockMarkets) %>%
  gather(index, price) %>%
  mutate(time = rep(time(EuStockMarkets), 4))
head(stocks)
plot_ly(x = stocks$time, y = stocks$price, color = stocks$index)
# let's try to plot without gather
stocks <- as.data.frame(EuStockMarkets) %>%
  mutate(time = time(EuStockMarkets))
head(stocks)
plot_ly(x = stocks$time, y = stocks$DAX)
plot_ly(x = stocks$time, y = select(stocks, -time))

# Histogram
plot_ly(x = precip, type = "histogram")
# Boxplot
plot_ly(y = iris$Petal.Length, color = iris$Species, type = "box")
# Heatmap
terrain1 <- matrix(rnorm(100*100), nrow = 100, ncol = 100)
plot_ly(z = terrain1, type = "heatmap")
# 3D Surface
terrain2 <- matrix(sort(rnorm(100*100)), nrow = 100, ncol = 100)
plot_ly(z = terrain2, type = "surface")
# Choropleth Maps
state_pop <- data.frame(State = state.abb, Pop = as.vector(state.x77[,1]))
state_pop$hover <- with(state_pop, paste(State, '<br>', "Population: ", Pop))
head(state_pop)
borders <- list(color = toRGB("red"))
map_options <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = T,
  lakecolor = toRGB('white')
)
plot_ly(data = state_pop, z = state_pop$Pop, text = state_pop$hover, 
        location = state_pop$State,
        type = 'choropleth', locationmode = 'USA-states',
        color = state_pop$Pop, colors = 'Blues', marker = list(line = borders)) %>%
  layout(title = 'US Population in 1975', geo = map_options)

#
set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000),]
p <- ggplot(data = d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity), color = cut), size = 1, alpha = 0.7) +
  geom_smooth(aes(color = cut, fill = cut), se = F) +
  facet_wrap(~ cut)
gg <- ggplotly(p)
gg
# post graph on plot.ly
# top post on plot.ly website we need to setup environment variables
Sys.setenv("plotly_username" = "your_plotly_username")
Sys.setenv("plotly_apt_key" = "your_apt_key")
plotly_POST(gg)

# 




