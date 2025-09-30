library(shiny)
library(shinyMaterialUI)

Header <- TabContext.shinyInput(
  inputId = "context",
  value = "1",
  Box(sx = list(borderBottom = 1, borderColor = 'divider'),
      TabList.shinyInput(
        inputId = "tabList",
        value = "1",
        Tab(label="Item One", value = "1"),
        Tab(label="Item Two", value = "2"),
        Tab(label="Item Three", value = "3")
      )
  ),
  TabPanel.shinyInput(inputId = "tab1", value = "1", "Content 1"),
  TabPanel.shinyInput(inputId = "tab2", value = "2", "Content 2"),
  TabPanel.shinyInput(inputId = "tab3", value = "3", "Content 3")
)

ui <- shinyMaterialUIPage(
  suppressBootstrap = FALSE,
  Header
)

server <- function(input, output) {
  observeEvent(input$tabList, {
    updateTabContext.shinyInput(inputId = "context", value = input$tabList)
  })
}

if (interactive()) {
  shinyApp(ui = ui, server = server)
}