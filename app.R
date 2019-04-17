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

#' @title future_value
#' @description compute future value of an investment given amount, rate and year
#' @param amount initial invested amount
#' @param rate annual rate of return
#' @param years number of years
#' @return the future value of the investment
future_value <- function(amount, rate, years){
  FV = amount*(1+rate)^years
  return(FV)
}

#' @title annuity
#' @description compute future value of an annuity given amount, rate and year
#' @param contrib contributed amount
#' @param rate annual rate of return
#' @param years number of years
#' @return the future value of the annuity
annuity <- function(contrib, rate, years){
  FVA = contrib*((1+rate)^years - 1)/rate
  return(FVA)
}

#' @title growing_annuity
#' @description compute future value of growing annuity given amount, rate and year
#' @param contrib contributed amount
#' @param rate annual rate of return
#' @param growth annual growth rate
#' @param years number of years
#' @return the future value of the growing annuity
growing_annuity <- function(contrib, rate, growth, years){
  FVGA = contrib*((1+rate)^years - (1+growth)^years)/(rate-growth)
  return(FVGA)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("Investment Panacea"),
   fluidRow(
     column(width = 4, 
            sliderInput(inputId = "initial", label = "Initial Amount",
                           min = 0, max = 100000,
                           value = 1000, step = 500),
            sliderInput(inputId = "annual", label = "Annual Contribution",
                        min = 0, max = 50000,
                        value = 2000, step = 500)),
     column(width = 4,
            sliderInput(inputId = "rtn", label = "Return Rate (in %)",
                        min = 0, max = 20,
                        value = 5, step = 0.1),
            sliderInput(inputId = "growth", label = "Growth Rate (in %)",
                        min = 0, max = 20,
                        value = 2, step = 0.1)),
     column(width = 4,
            sliderInput(inputId = "year", label = "Years",
                        min = 0, max = 50,
                        value = 10),
            selectInput(inputId = "facet", label = "Facet?",
                        c("No", "Yes")))
     ),
      mainPanel(width = 12,
         hr(),
         h4("Timelines"),
         plotOutput(outputId = "future_value"), 
         h4("Balances"),
         # tableOutput(outputId = "view_value")
         verbatimTextOutput(outputId = "view_value")
      )

)

# Define server logic required to draw a histogram
server <- function(input, output){
  datasetInput_no <- reactive({
    modalities <- as.data.frame(matrix(0, ncol = 4, nrow = (input$year + 1)))
    colnames(modalities) <- c("year", "no_contrib", "fixed_contrib", "growing_contrib")
    # modalities[1,] <- c(0, input$initial, input$initial, input$initial)
    for(i in 0:input$year){
      modalities$year[i+1] = i
      modalities$no_contrib[i+1] = future_value(input$initial, input$rtn/100, i)
      modalities$fixed_contrib[i+1] = (future_value(input$initial, input$rtn/100, i) +
                                         annuity(input$annual, input$rtn/100, i))
      modalities$growing_contrib[i+1] = (future_value(input$initial, input$rtn/100, i) +
                                           growing_annuity(input$annual, input$rtn/100, input$growth/100, i))
    }
    modalities$year <- as.integer(modalities$year)
    modalities
  })
  datasetInput_yes <- reactive({
    modalities <- as.data.frame(matrix(0, ncol = 3, nrow = (input$year + 1)*3))
    colnames(modalities) <- c("year", "value", "type")
    modalities$year <- rep(0:input$year, 3)
    modalities$type = rep(c("no_contri", "fixed_contri", "growing_contri"),
                          each = (input$year + 1))
    for(i in 0:input$year){
      modalities$value[i+1] = future_value(input$initial, input$rtn/100, i)
      modalities$value[input$year+1+i+1] = (future_value(input$initial, input$rtn/100, i) +
                                              annuity(input$annual, input$rtn/100, i))
      modalities$value[2*(input$year + 1)+i+1] = (future_value(input$initial, input$rtn/100, i) +
                                                    growing_annuity(input$annual, input$rtn/100, input$growth/100, i))
    }
    modalities
  })
  output$future_value <- renderPlot({
    if(input$facet == "No"){
      ggplot(data = datasetInput_no()) +
        geom_point(aes(x = year, y = no_contrib, col = "no contribution"), alpha = 0.6, size = 0.7) +
        geom_point(aes(x = year, y = fixed_contrib, col = "fixed contribution"), alpha = 0.6, size = 0.7) +
        geom_point(aes(x = year, y = growing_contrib, col = "growing contribution"), alpha = 0.6, size = 0.7) +
        geom_line(aes(x = year, y = no_contrib, col = "no contribution")) +
        geom_line(aes(x = year, y = fixed_contrib, col = "fixed contribution")) +
        geom_line(aes(x = year, y = growing_contrib, col = "growing contribution")) +
        ggtitle("Three models of investing") +
        xlab("year") +
        ylab("future value") +
        scale_x_continuous(limits = c(0, input$year), breaks = 0:input$year) +
        theme_bw()
    }else{
      ggplot(data = datasetInput_yes()) +
        geom_point(aes(x = year, y = value, col = type), alpha = 0.6, size = 0.7) +
        geom_line(aes(x = year, y = value, col = type)) +
        geom_area(aes(x = year, y = value, fill = type), alpha = 0.6) +
        ggtitle("Three models of investing") +
        xlab("year") +
        ylab("future value") +
        scale_x_continuous(limits = c(0, input$year), 
                           breaks = seq(0,input$year, length.out = round(input$year/4))) +
        facet_grid(.~type) +
        theme_bw()
    }
  })
  # output$view_value <- renderTable({
  output$view_value <- renderPrint({
      datasetInput_no()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

