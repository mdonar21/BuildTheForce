library(shiny)

shinyServer(function(input, output, session) {
  
  # Example Hall of Fame data
  hallOfFameData <- data.frame(
    "Call Sign" = c("Steve","Druid","Tara","Bryan","Hughbanks","Jib"),
    "Total Failure Points" = c(0,0,0,0,0,0),
    "Total Losses" = c(3.3,3.3,3.3,7.5,7.5,7.5),
    check.names = FALSE
  )
  
  # Send data to JS when requested
  observeEvent(input$getHallOfFame, {
    session$sendCustomMessage("hallOfFameData", hallOfFameData)
  })
  
  # Similarly handle other inputs from JS
  observeEvent(input$techLevel, {
    # process tech level and maybe send back updates
    message("Tech level selected:", input$techLevel)
  })
})
