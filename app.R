#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Annual Balances of Savings-mode"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
      column(4,
             sliderInput("initial_amount",
                    "Initial Amount",
                    post = "$",
                    min = 0,
                    max = 100000,
                    value = 1000,
                    step = 500),
             sliderInput("annual_contribution",
                    "Annual Contribution",
                    post = "$",
                    min = 0,
                    max = 50000,
                    value = 2000,
                    step = 500)
      ),         
      column(4,
             sliderInput("return_rate",
                    "Return Rate in percentage",
                    min = 0,
                    max = 20,
                    value = 5,
                    step = 0.1),
             sliderInput("growth_rate",
                    "Growth Rate in percentage",
                    min = 0,
                    max = 20,
                    value = 2,
                    step = 0.1)
      ),
        column(4,
              sliderInput("years",
                    "Years",
                    min = 0,
                    max = 50,
                    value = 20,
                    step = 1),
              selectInput("facet",
                    "Facet",
                    choices = c("Yes","No"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h4("Timelines"),
         plotOutput("distPlot"),
         h4("Balances"),
         tableOutput("datatable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   balance <- reactive({
     balance = matrix(0,input$years+1,4)
     balance = data.frame(balance)
     colnames(balance) <- c("year","no_contrib","fixed_contrib","growing_contrib")
     future_value <- function(amount,rate,years){
       a <- amount *(1+rate/100)^years
       return(a)
     }
     annuity <- function(contrib,rate,years){
       a <- contrib *((1+rate/100)^years-1)/(rate/100)
       return(a)
     }
     growing_annuity <- function(contrib,rate,growth,years){
       a <- contrib *((1+rate/100)^years-(1+growth/100)^years)/(rate/100-growth/100)
       return(a)
     }
     for(i in 0:input$years){
       balance[i+1,1] = i
       balance[i+1,2] = future_value(amount = input$initial_amount, rate = input$return_rate, years = i)
       balance[i+1,3] = annuity(contrib = input$annual_contribution, rate = input$return_rate,  years = i) + future_value(amount = input$initial_amount, rate = input$return_rate,years = i)
       balance[i+1,4] = growing_annuity(contrib = input$annual_contribution, rate = input$return_rate, growth = input$growth_rate, years = i) + future_value(amount = input$initial_amount, rate = input$return_rate,years = i)
       
     } 
     return(balance)
   })
   data <- reactive({
     data = matrix(0,(input$years+1)*3,3)
     data = data.frame(data)
     colnames(data) <- c("year","balances","mode")
     future_value <- function(amount,rate,years){
       a <- amount *(1+rate/100)^years
       return(a)
     }
     annuity <- function(contrib,rate,years){
       a <- contrib *((1+rate/100)^years-1)/(rate/100)
       return(a)
     }
     growing_annuity <- function(contrib,rate,growth,years){
       a <- contrib *((1+rate/100)^years-(1+growth/100)^years)/(rate/100-growth/100)
       return(a)
     }
     for(i in 0:input$years){
       data[i+1,1] = i
       data[i+1,2] = future_value(amount = input$initial_amount, rate = input$return_rate, years = i)
       data[i+1,3] = "no_contrib"
     } 
     for(i in 0:input$years){
       data[i+2+input$years,1] = i
       data[i+2+input$years,2] = annuity(contrib = input$annual_contribution, rate = input$return_rate,  years = i) + future_value(amount = input$initial_amount, rate = input$return_rate,years = i)
       data[i+2+input$years,3] = "fixed_contrib"
     }
     for(i in 0:input$years){
       data[i+3+2*input$years,1] = i
       data[i+3+2*input$years,2] = growing_annuity(contrib = input$annual_contribution, rate = input$return_rate, growth = input$growth_rate, years = i) + future_value(amount = input$initial_amount, rate = input$return_rate,years = i)
       data[i+3+2*input$years,3] = "growing_contrib"
     }
     data$mode = factor(data$mode,levels = c("no_contrib","fixed_contrib","growing_contrib"))
     return(data)
   })
   output$distPlot <- renderPlot({
     if(input$facet == "No"){
       gg <- ggplot(data = balance(), aes(x = year)) +
         geom_line(aes(y = no_contrib,color = "no_contrib")) +
         geom_point(aes(y = no_contrib,color = "no_contrib")) +
         geom_line(aes(y = fixed_contrib,color = "fixed_contrib")) +
         geom_point(aes(y = fixed_contrib,color = "fixed_contrib")) +
         geom_line(aes(y = growing_contrib, color = "growing_contrib")) +
         geom_point(aes(y = growing_contrib, color = "growing_contrib")) +
         labs(title = "Three modes of investing",x ="year", y = "value") 
     }
     if(input$facet == "Yes"){
       gg <- ggplot(data = data(), aes(x = year)) +
         geom_line(aes(y = balances,color = mode))  +
         geom_point(aes(y = balances,color = mode)) +
         geom_area(aes(x=year, y=balances, fill=mode, alpha = mode)) +
         labs(title = "Three modes of investing",x ="year", y = "value") +
         facet_grid(~mode)
     }
     gg
     })
   output$datatable <- renderTable({
     balance()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

