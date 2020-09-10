##ShinyApp_ICC for IRT Logistic Models (11.09.2020)

library(shiny)

ui <- fluidPage(

  titlePanel("ICC Graphs for IRT Logistic Models"),

  sidebarLayout(
   
  sidebarPanel(
    h4(strong("Enter the item parameters:")),
    helpText("IRT logistic models have four well-known item parameters; a, b, c, and d."),
    numericInput("a", "a parameter", 1, 0, 5, step = .1),
    numericInput("b", "b parameter", 0, -5, 5, step = .1),
    numericInput("c", "c parameter", 0, 0, .5, step = .01),
    numericInput("d", "d parameter", 1, .5, 1, step = .01),
    br(),
    br(),
    h4(strong("Select interval for theta:")),
    helpText("You can see the best theta interval for model-fit"),
    sliderInput("theta", "Interval for Theta", -3.5, 3.5, c(-3.5, 3.5)),
    br(),
    br(),
    actionButton("go", "Draw")
    ),
    
  mainPanel(
    plotOutput("icc"),
    )
  )
)


server <- function(input, output){
 
  output$icc <- renderPlot({
    
    f.theta <- function(t, a = 1, b = 0, c = 0, d = 1){
      c + (d-c) * 1 / (1 + exp(-1.7*a*(t-b)))}
    
    input$go
    isolate({
      a = input$a
      b = input$b
      c = input$c
      d = input$d

      xv = seq(-3.5, 3.5, .01)
      yv = f.theta(xv, a, b, c, d)
    
    if(a == 1 & c == 0 & d == 1) {model <- "1PL"
      } else if(a != 1 & c == 0 & d == 1) {model <- "2PL"
      } else if(c != 0 & d == 1) {model <- "3PL"
      } else if(d != 1) {model <- "4PL"
      }

    plot(xv, yv, type = "l", lwd = 3.5, ylim = c(0, 1), yaxt = "n",
        ylab = "Porbability", xlab = "Theta",
        main = paste("Item Characteristic Curve for", model, "model.")
      )  
    axis(2, c(0, c, f.theta(b, a, b, c, d), d, 1), las = 1)
    axis(1, b)
    
    t1 <- input$theta[1]
    t2 <- input$theta[2]
    xv1 <- seq(t1, t2, .01)
    yv1 <- f.theta(xv1, a, b, c, d)
    polygon(c(t1, xv1, t2), c(c, yv1, c), col = "lightgreen")

    lines(c(b, b), c(0, (d+c)/2), lty = 3, lwd = 2, col = "red")
    lines(c(-4, b), c((d+c)/2, (d+c)/2), lty = 3, lwd = 2, col = "red")
    points(b, f.theta(b, a, b, c, d), pch = 16)
    
    if(c != 0) abline(h = c, lty = 3, lwd =2, col = "red")
    if(d != 1) abline(h = d, lty = 3, lwd = 2, col = "red")
    
    })
  })
}

shinyApp(ui, server)
    