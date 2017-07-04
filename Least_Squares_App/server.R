library(shiny)

# Define the server side function
shinyServer({function(input, output, session) {
     # Create reactive values to hold parameters that might change
     # when generating a new data set
     rv <- reactiveValues(
          # slope and intercept
          m = round(runif(1, min=-5, max=5), 1),
          b = round(runif(1, min=-2, max=2), 1),
          # Create cloud of points
          num = 40
     )
     # These reactive values depend on others: The simulated data x
     rv$x <- isolate(runif(rv$num, min=0, max=10))
     # and the outputs y, with a built-in noise component
     rv$y <- isolate(rv$m*rv$x + rv$b + rnorm(rv$num, sd=2))
     # Least squares fit coefficients.  Probably different from 
     # actual slope/intercept
     rv$fit.coef <- isolate( coef(lm(rv$y~rv$x)))
     
     # Current SSE value:  Reactive expression
     curSSE <- reactive(round(sum((input$slope*rv$x+input$intercept-rv$y)^2),1))
     
     # Create an inital minimum value for min MSE
     rv$minSSE <- isolate(curSSE())
     rv$bestb <- isolate(rv$b)
     rv$bestm <- isolate(rv$m)
     
     # Check for changes in minSSE:
     observe({
               if (rv$minSSE > curSSE()) {
                    rv$minSSE <- curSSE()
                    rv$bestb <- isolate(input$intercept)
                    rv$bestm <- isolate(input$slope)
               }
          })
     
     # Button to create new set of data:
     observeEvent(input$new, {
          # slope and intercept
          rv$m <- round(runif(1, min=-5, max=5), 1)
          rv$b <- round(runif(1, min=-2, max=2), 1)
          # Create cloud of points
          rv$num <- 40
          rv$x <- runif(rv$num, min=0, max=10)
          rv$y <- m*x + b + rnorm(rv$num, sd=2)
          # Actual least squares fit
          rv$fit.coef <- coef(lm(y~x))
          # Reset the sliderss:  (That's why I needed the "session" variable above)
          updateSliderInput(session, "slope", value=0)
          updateSliderInput(session, "intercept", value=0)
          # And reset minimum SSE:
          rv$minSSE <- curSSE()
          
     })
     
     # Button to set the intercept to the right value:
     observeEvent(input$setb, {
          updateSliderInput(session, "intercept", value=rv$fit.coef[[1]])
     })
     # Button to set the slope to the right value:
     observeEvent(input$setm, {
          updateSliderInput(session, "slope", value=rv$fit.coef[[2]])
     })

     # Create the plot
     output$scatterplot <- renderPlot({
          plot(rv$x, rv$y, main="Linear Least Squares Approximation", xlab="x",
               ylab="y", pch=16, col="tan")
          abline(coef=c(input$intercept, input$slope), col="darkgreen")
          preds <- input$slope*rv$x+input$intercept
          # Create the error lines.  Code details borrowed from 
          # 01_06_residualVariation lecture code in regression models
          # course
          for (i in 1 : rv$num) 
               lines(c(rv$x[i], rv$x[i]), c(rv$y[i], preds[i]), col = "grey")
     })
     
     output$info <- renderText({
          paste0("Intercept: ", rv$b, "\tSlope: ", rv$m, 
                 "\n\nFit Intercept: ", round(rv$fit.coef[1], 1), "\tFit Slope: ",
                 round(rv$fit.coef[2],1), "\n", "Squared Errors: ", curSSE())
     })
     
     output$truem <- renderText({
          paste0("Actual m: ", rv$m)
     })
     output$trueb <- renderText({
          paste0("Actual b:", rv$b)
     })
     output$lsm <- renderText({
          paste("Least squares slope:", round(rv$fit.coef[2], 1))
     })
     output$lsb <- renderText({
          paste("Least squares intercept:", round(rv$fit.coef[1], 1))
     })
     output$error <- renderText({
          paste("Sum of Squared Error (SSE):", 
                prettyNum(curSSE(), big.mark=","))
     })
     # Display the minimum SSE found so far:
     output$minE <- renderText({
          paste("Minimum SSE so far: ", prettyNum(rv$minSSE, big.mark=","))
     })
     output$bestparam <- renderText({
          paste0("achieved with slope ", rv$bestm, " and intercept ", rv$bestb, ".")
     })
     
     }
})
