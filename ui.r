library(DT)
library(plotly)
library(tidyverse)
library(shiny)
library(shinythemes)

rd_text <- "A higher tech level is necessary to prevail against the crises, but spending too much on R&D reduces your ability to spend on force structure"
readiness_funding_text <- "You can save money by reducing readiness funding, but too much cutting reduces the effectiveness of your force"
select_forces_text <- "RC is cheaper but is subject to a capability dice roll below ranging from 50-100% effective; ensure sufficient sustainment by checking the % on the left sidebar"
upgrade_cancel_text <- "If you make an upgrade mistake, clicking the button twice will zero out all upgrades and upgrades costs"

banner_function <- function(text,
                            icon_name = NULL,
                            icon_text = NULL) {
  fluidRow(column(12, tags$div(
    class = "banner", text, make_info_box(icon_name, icon_text)
  )))
}

make_info_box <- function(icon_name, text) {
  icon(icon_name, title = text)
}
shinyUI(
  navbarPage(
    # Set the title for the navbar
    title = div(
      style = "display: inline-flex; align-items: center; text-align:center;color: white; height: 100%;",
      img(
        src = "awc.png",
        height = "40",
        width = "40",
        style = "margin-right: 10px;" # Add space after the left logo
      ),
      tags$span("Build the Force", class = "navtext", ),
      img(
        src = "dsw.png",
        height = "40",
        width = "40"
      )
    ),
    
    theme = shinythemes::shinytheme("flatly"),
    header = includeCSS("www/custom.css"),
    # player information collect
    tabPanel(
      "Player Information",
      fluidRow(
        div(
        class = "class-dropbox v-center-row main-page-div",
          
        
        column(
          4,
          # Removed 'br()' here
          selectInput(
            "class",
            "Select class",
            c(
              "Choose One" = "",
              "BSAP",
              "Wargame Designer Course",
              "ORSA Q Course",
              "Other"
            )
          ),
          textInput("name", "Player call sign", placeholder = "Enter call sign here"),
        ),
        
        column(8, tags$div(
          class = "hof-modern-box",
          div(class = "banner", h3(class = "banner-h3", "Build the Force Hall of Fame")),
          fluidRow(
            column(
              2,
              align = "center",
              br(),
              # Displays Hof image commented out for now as it gets messy with the table uiOutput("hof_image")
            ),
            column(
              class = "hof-table",
              6,
              offset = 1,
              align = "center",
              br(),
              tableOutput("hof")
            )
          )
        )
        ))
      ),
      br(),
      fluidRow(column(
        12, div(class = "unclass_bottom", "(U) Unclassified")
      ))
    ),
    
    # Round 1 code
    
    tabPanel(
      "Round 1",
      value = "round1",
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class="sidebar",
          # error alerts box
          
          fluidRow(
            column(
              12,
              style = "background-color:yellow; color:black; border-style: solid; border-color: black; border-radius: 8px",
              tags$strong("Error Alerts"),
              textOutput("error1"),
              textOutput("error2"),
              textOutput("error3"),
              textOutput("error4"),
              textOutput("NA_error"),
              textOutput("no_error")
            )
          ),
          br(),
          
          # funding box
          
          fluidRow(
            column(
              12,
              style = "background-color:#FDF0EE; border-style: solid; border-radius: 8px;",
              fluidRow(
                style = "text-align: center;",
                column(6, tags$strong("Funding Allocated"), textOutput("allocated"), ),
                column(6, tags$strong("Funding Remaining"), textOutput("remaining"))
              )
            )
          ),
          br(),
          
          #  game dashboard box
          
          fluidRow(
            column(
              12,
              style = "height: 40vh; overflow-y: auto; background-color:#FDF0EE; border-style: solid; border-radius: 8px;",
              br(),
              textOutput("rd_costs"),
              textOutput("rd_costs2"),
              tags$hr(style = "border-color: black;"),
              "Selected forces costs",
              tableOutput("forces_cost"),
              tags$hr(style = "border-color: black;"),
              textOutput("upgrades_cost"),
              tags$hr(style = "border-color: black;"),
              textOutput("ena_cap"),
              textOutput("ena_req"),
              textOutput("sustainment_percent"),
              tags$hr(style = "border-color: black;"),
              textOutput("offense"),
              textOutput("defense"),
              textOutput("meet_priority"),
              textOutput("power_ratio")
            )
          ),
          
          br(),
          
          # failure points box
          
          fluidRow(
            column(12, style = "background-color:#ff8080; border-style: solid; border-radius: 8px;", div(style =
             "text-align:center;", fluidRow(
               column(
                 6,
                 
                 tags$strong("Readiness Failure Points"), textOutput("read_failure")
               ), column(6,
                         tags$strong("Crisis Failure Points"),
                         textOutput("crisis_failure"))
             ))), )
        ),
        # ends sidebar panel
        
        mainPanel(
          width = 9,
          style = "height: 100%; overflow-y: auto;",
          br(),
          
          # randomness determination
          
          fluidRow(
            column(
              4,
              style = "background-color:#F8F8FF; border-style: dashed; border-radius: 8px;",
              br(),
              radioButtons(
                inputId = "dice",
                label = "How will be the random events be chosen?",
                choices = c(
                  "Manual die rolls" = "manual",
                  "Computer-generated die rolls" = "computer"
                )
              ),
              conditionalPanel(
                condition = "input.dice == 'computer'",
                numericInput(
                  "seed",
                  "Enter seed for random number generator (entire class should use the same seed)",
                  value = 1
                )
              )
            )
          ),
          
          # political event
          
          #"(1) 
          br(),
          banner_function(
            h4(
              "Select the political event" 
            ),
          ),
          class="section-banner",
          br(),
          
          fluidRow(
            column(
              4,
              conditionalPanel(
                condition = "input.dice == 'computer'",
                br(),
                actionButton(
                  "pol_button",
                  "Roll for political event",
                  icon("dice", "fa-shake")
                ),
                textOutput("pol_die_roll")
              ),
              selectInput(
                "political_event_1",
                "Select the political event",
                choices = c(
                  "None",
                  "Congressional Interests" = "pol1",
                  "Unfriendly Administration" = "pol2",
                  "Friendly Administration" = "pol3",
                  "Cyber Attack" = "pol4",
                  "White House Priority" = "pol5",
                  "Uprising" = "pol6"
                )
              ),
              conditionalPanel(condition = "input.political_event_1 == 'pol1'", img(src =
                                                                                      "pol1.png")),
              conditionalPanel(condition = "input.political_event_1 == 'pol2'", img(src =
                                                                                      "pol2.png")),
              conditionalPanel(condition = "input.political_event_1 == 'pol3'", img(src =
                                                                                      "pol3.png")),
              conditionalPanel(condition = "input.political_event_1 == 'pol4'", img(src =
                                                                                      "pol4.png")),
              conditionalPanel(condition = "input.political_event_1 == 'pol5'", img(src =
                                                                                      "pol5.png")),
              conditionalPanel(condition = "input.political_event_1 == 'pol6'", img(src =
                                                                                      "pol6.png"))
            ),
            
            # manual die rolls
            
            column(
              3,
              conditionalPanel(
                condition = "input.dice == 'manual'",
                conditionalPanel(
                  condition = "input.political_event_1 == 'pol1'",
                  numericInput(
                    "pol1_die_roll1",
                    "Enter force type roll",
                    value = 2,
                    min = 2,
                    max = 12
                  ),
                  numericInput(
                    "pol1_die_roll2",
                    "Enter quantity roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.political_event_1 == 'pol4'",
                  numericInput(
                    "pol4_die_roll1",
                    "Enter force upgrade roll reduction roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.political_event_1 == 'pol6'",
                  numericInput(
                    "pol6_die_roll1",
                    "Enter crisis outcome roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                )
              ),
              
              # computer-generated die rolls
              
              conditionalPanel(
                condition = "input.dice == 'computer'",
                conditionalPanel(
                  condition = "input.political_event_1 == 'pol1'",
                  actionButton("pol1_button1", "Roll for force type", icon("dice", "fa-shake")),
                  actionButton("pol1_button2", "Roll for quantity", icon("dice", "fa-shake")),
                  textOutput("pol1_die_roll1"),
                  textOutput("pol1_die_roll2")
                ),
                conditionalPanel(
                  condition = "input.political_event_1 == 'pol4'",
                  actionButton(
                    "pol4_button1",
                    "Roll for reduction to force upgrade rolls",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("pol4_die_roll1")
                ),
                conditionalPanel(
                  condition = "input.political_event_1 == 'pol5'",
                  actionButton(
                    "pol5_button1",
                    "Roll to determine crisis requiring additional requirements",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("pol5_die_roll1")
                ),
                conditionalPanel(
                  condition = "input.political_event_1 == 'pol6'",
                  actionButton(
                    "pol6_button1",
                    "Roll to determine impact to crisis outcome",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("pol6_die_roll1")
                )
              ),
              
              # White House priority crisis and requirements
              
              conditionalPanel(
                condition = "input.political_event_1 == 'pol5'",
                selectInput(
                  "priority_crisis",
                  "Select the White House priority crisis",
                  choices = c(
                    "None",
                    "Steady State" = "steady",
                    "Humanitarian Crisis" = "human",
                    "Rogue State" = "rogue",
                    "ISIS 2.0" = "isis",
                    "Defensive Campaign (vs Peer Competitor)" = "defense",
                    "Offensive Campaign (vs Peer Competitor)" = "offense"
                  )
                )
              )
            ),
            
            column(
              4,
              conditionalPanel(
                condition = "!(input.political_event_1 == 'None')",
                class = "notice",
                tags$strong(textOutput("pol_event"))
              )
            )
          ),
          
          # research and development
          
          br(),
          shiny::tagList(
            shiny::h4("(2) Make your research and development choices"),
            banner_function(
              NULL, # The title is now in the h3, so you might pass NULL or "" here
              "circle-info",
              rd_text
            )
          ),
          br(),
          fluidRow(
            column(
              6,
              uiOutput("tech_target"),
              htmlOutput("expedited"),
              br(),
              uiOutput("risk_target"),
              div(class = "notice", textOutput("rd_min_roll"))
            ),
            column(
              4,
              div(
                numericInput(
                  "tech_die_roll1",
                  "First R&D die roll",
                  min = 1,
                  max = 6,
                  value = 3
                ),
                class = "dice"
              ),
              checkboxInput(
                "tech_retry",
                div(
                  "Do you wish to retry if you fail to fund your target tech level?",
                  class = "inputs"
                ),
                width = "100%"
              ),
              conditionalPanel(condition = "input.tech_retry==1", div(div(
                class = "dice",
                numericInput(
                  "tech_die_roll2",
                  "Second R&D die roll",
                  min = 1,
                  max = 6,
                  value = 3
                ),
              ), br())),
              div(class = "notice", tags$strong(textOutput("tech_level")))
            )
          ),
          
          # readiness
          
          br(),
          banner_function("(3a) Select the readiness events"),
          br(),
          
          # readiness1
          
          fluidRow(column(
            12,
            selectInput(
              "readiness_event",
              "Select the readiness event",
              choices = c(
                "None",
                "Public Opinion" = "read1",
                "Training Environment" = "read2",
                "Health and Quality of Life" = "read3",
                "Infrastructure" = "read4",
                "Supply Chain" = "read5",
                "Corruption and Scandals" = "read6"
              )
            ),
          )),
          
          # manual selections
          
          fluidRow(
            column(
              4,
              conditionalPanel(
                condition = "input.dice == 'manual'",
                class = "dice",
                conditionalPanel(
                  condition = "input.readiness_event == 'read1'",
                  numericInput(
                    "read1_die_roll",
                    "Enter MILPERS effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.readiness_event == 'read2'",
                  numericInput(
                    "read2_die_roll",
                    "Enter O&M effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.readiness_event == 'read3'",
                  numericInput(
                    "read3_die_roll",
                    "Enter MILPERS and MILCON effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.readiness_event == 'read4'",
                  numericInput(
                    "read4_die_roll",
                    "Enter MILCON effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.readiness_event == 'read5'",
                  numericInput(
                    "read5_die_roll",
                    "Enter procurement and O&M effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.readiness_event == 'read6'",
                  numericInput(
                    "read6_die_roll1",
                    "Enter type roll",
                    value = 1,
                    min = 1,
                    max = 6
                  ),
                  numericInput(
                    "read6_die_roll2",
                    "Enter effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                )
              ),
              
              # readiness1 computer-generated rolls
              
              conditionalPanel(
                condition = "input.dice == 'computer'",
                conditionalPanel(
                  condition = "input.readiness_event == 'read1'",
                  actionButton(
                    "read1_button",
                    "Roll for MILPERS effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("read1_die_roll")
                ),
                conditionalPanel(
                  condition = "input.readiness_event == 'read2'",
                  actionButton("read2_button", "Roll for O&M effect", icon("dice", "fa-shake")),
                  textOutput("read2_die_roll")
                ),
                conditionalPanel(
                  condition = "input.readiness_event == 'read3'",
                  actionButton(
                    "read3_button",
                    "Roll for MILPERS and MILCON effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("read3_die_roll")
                ),
                conditionalPanel(
                  condition = "input.readiness_event == 'read4'",
                  actionButton(
                    "read4_button",
                    "Roll for MILCON effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("read4_die_roll")
                ),
                conditionalPanel(
                  condition = "input.readiness_event == 'read5'",
                  actionButton(
                    "read5_button",
                    "Roll for procurement and O&M effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("read5_die_roll")
                ),
                conditionalPanel(
                  condition = "input.readiness_event == 'read6'",
                  actionButton("read6_button1", "Roll for type", icon("dice", "fa-shake")),
                  actionButton("read6_button2", "Roll for effect", icon("dice", "fa-shake")),
                  textOutput("read6_die_roll1"),
                  textOutput("read6_die_roll2")
                )
              ),
            ),
            conditionalPanel(condition = "input.readiness_event == 'read1'", column(
              4, img(src = "readiness1a.png"), img(src = "readiness1b.png")
            )),
            conditionalPanel(condition = "input.readiness_event == 'read2'", column(
              4, img(src = "readiness2a.png"), img(src = "readiness2b.png")
            )),
            conditionalPanel(condition = "input.readiness_event == 'read3'", column(
              4, img(src = "readiness3a.png"), img(src = "readiness3b.png")
            )),
            conditionalPanel(condition = "input.readiness_event == 'read4'", column(
              4, img(src = "readiness4a.png"), img(src = "readiness4b.png")
            )),
            conditionalPanel(condition = "input.readiness_event == 'read5'", column(
              4, img(src = "readiness5a.png"), img(src = "readiness5b.png")
            )),
            conditionalPanel(condition = "input.readiness_event == 'read6'", column(4, img(src =
                                                                                             "readiness6.png"))),
            
            conditionalPanel(condition = "!(input.readiness_event == 'None')", column(3, class = "notice", strong(
              textOutput("readiness_impact")
            )))
          ),
          # end readiness 1 fluidRow
          
          # readiness event #2
          
          tags$hr(style = "border-color: black; border-style: dotted;"),
          
          fluidRow(column(
            12,
            selectInput(
              "readiness_event2",
              "Select the second readiness event",
              choices = c(
                "None",
                "Public Opinion" = "read1",
                "Training Environment" = "read2",
                "Health and Quality of Life" = "read3",
                "Infrastructure" = "read4",
                "Supply Chain" = "read5",
                "Corruption and Scandals" = "read6"
              )
            ),
          )),
          
          # manual selections
          
          fluidRow(
            column(
              4,
              conditionalPanel(
                condition = "input.dice == 'manual'",
                class = "dice",
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read1'",
                  numericInput(
                    "read2_1_die_roll",
                    "Enter MILPERS effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read2'",
                  numericInput(
                    "read2_2_die_roll",
                    "Enter O&M effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read3'",
                  numericInput(
                    "read2_3_die_roll",
                    "Enter MILPERS and MILCON effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read4'",
                  numericInput(
                    "read2_4_die_roll",
                    "Enter MILCON effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read5'",
                  numericInput(
                    "read2_5_die_roll",
                    "Enter procurement and O&M effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read6'",
                  numericInput(
                    "read2_6_die_roll1",
                    "Enter type roll",
                    value = 1,
                    min = 1,
                    max = 6
                  ),
                  numericInput(
                    "read2_6_die_roll2",
                    "Enter effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                )
              ),
              
              #  computer-generated rolls
              
              conditionalPanel(
                condition = "input.dice == 'computer'",
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read1'",
                  actionButton(
                    "read2_1_button",
                    "Roll for MILPERS effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("read2_1_die_roll")
                ),
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read2'",
                  actionButton(
                    "read2_2_button",
                    "Roll for O&M effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("read2_2_die_roll")
                ),
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read3'",
                  actionButton(
                    "read2_3_button",
                    "Roll for MILPERS and MILCON effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("read2_3_die_roll")
                ),
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read4'",
                  actionButton(
                    "read2_4_button",
                    "Roll for MILCON effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("read2_4_die_roll")
                ),
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read5'",
                  actionButton(
                    "read2_5_button",
                    "Roll for procurement and O&M effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("read2_5_die_roll")
                ),
                conditionalPanel(
                  condition = "input.readiness_event2 == 'read6'",
                  actionButton("read2_6_button1", "Roll for type", icon("dice", "fa-shake")),
                  actionButton("read2_6_button2", "Roll for effect", icon("dice", "fa-shake")),
                  textOutput("read2_6_die_roll1"),
                  textOutput("read2_6_die_roll2")
                )
              ),
            ),
            conditionalPanel(condition = "input.readiness_event2 == 'read1'", column(
              4, img(src = "readiness1a.png"), img(src = "readiness1b.png")
            )),
            conditionalPanel(condition = "input.readiness_event2 == 'read2'", column(
              4, img(src = "readiness2a.png"), img(src = "readiness2b.png")
            )),
            conditionalPanel(condition = "input.readiness_event2 == 'read3'", column(
              4, img(src = "readiness3a.png"), img(src = "readiness3b.png")
            )),
            conditionalPanel(condition = "input.readiness_event2 == 'read4'", column(
              4, img(src = "readiness4a.png"), img(src = "readiness4b.png")
            )),
            conditionalPanel(condition = "input.readiness_event2 == 'read5'", column(
              4, img(src = "readiness5a.png"), img(src = "readiness5b.png")
            )),
            conditionalPanel(condition = "input.readiness_event2 == 'read6'", column(4, img(src =
                                                                                              "readiness6.png"))),
            
            conditionalPanel(condition = "!(input.readiness_event2 == 'None')", column(3, class = "notice", strong(
              textOutput("readiness_impact2")
            )))
          ),
          br(),
          
          # MILCON, MILPERS, O&M, procurement funding
          
          banner_function(
            "(3b) Set your readiness funding levels",
            "circle-info",
            readiness_funding_text
          ),
          
          fluidRow(
            column(
              5,
              br(),
              sliderInput(
                "milcon_fund",
                "MILCON (Military Construction) % Funded",
                min = 0,
                max = 100,
                value = 100
              ),
              div(class = "notice", textOutput("milcon_min")),
              br(),
              div(
                numericInput(
                  "milcon_roll",
                  "MILCON dice roll",
                  min = 2,
                  max = 12,
                  value = 6
                ),
                class = "dice"
              ),
              tags$hr(style = "border-color: black; border-style: dotted;"),
              sliderInput(
                "milpers_fund",
                "MILPERS (Military Pesonnel) % Funded",
                min = 0,
                max = 100,
                value = 100
              ),
              div(class = "notice", textOutput("milpers_min")),
              br(),
              div(
                numericInput(
                  "milpers_roll",
                  "MILPERS dice roll",
                  min = 2,
                  max = 12,
                  value = 6
                ),
                class = "dice"
              )
            ),
            column(
              5,
              offset = 1,
              br(),
              sliderInput(
                "om_fund",
                "O&M (Operations and Maintenance) % Funded",
                min = 0,
                max = 100,
                value = 100
              ),
              div(class = "notice", textOutput("om_min")),
              br(),
              div(
                numericInput(
                  "om_roll",
                  "O&M dice roll",
                  min = 2,
                  max = 12,
                  value = 6
                ),
                class = "dice"
              ),
              tags$hr(style = "border-color: black; border-style: dotted;"),
              sliderInput(
                "proc_fund",
                "Procurement (other than new forces) % Funded",
                min = 0,
                max = 100,
                value = 100
              ),
              div(class = "notice", textOutput("proc_min")),
              br(),
              div(
                numericInput(
                  "proc_roll",
                  "Procurement dice roll",
                  min = 2,
                  max = 12,
                  value = 6
                ),
                class = "dice"
              )
            )
          ),
          
          br(),
          
          fluidRow(column(
            4,
            offset = 4,
            align = "center",
            div(
              class = "notice",
              tags$strong(
                "Overall Readiness % Funded",
                make_info_box(
                  "circle-info",
                  "This is the readiness factor for the combat power calculation"
                ),
                ":"
              ),
              textOutput("overall_readiness")
            )
          )),
          
          # forces information table: costs and capabilities
          
          br(),
          banner_function(
            "(4a) Info: Force Capabilities Table",
            "circle-info",
            "Table will update to include any selected upgrades"
          ),
          br(),
          
          fluidRow(
            column(
              7,
              offset = 2,
              style = "background-color:#def0de; border-style: solid;",
              br(),
              "Readiness Cost: Total (MILCON / MILPERS / O&M / Procurement)",
              br(),
              "Artillery provides + 1 offense and Aviation provides + 1 defense if either is paired with Armor, Infantry, or Stryker",
              br(),
              "Each Enabler force meets 10 Enabler Requirements",
              tableOutput("readiness_info")
            )
          ),
          
          br(),
          banner_function(
            "(4b) Select your forces and upgrades",
            "circle-info",
            select_forces_text
          ),
          
          fluidRow(
            br(),
            column(
              3,
              numericInput(
                "inf_AC",
                "Infantry AC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              3,
              numericInput(
                "inf_RC",
                "Infantry RC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              2,
              tags$div(style = "text-align: right; padding: 20px", "Upgrades to Infantry")
            ),
            column(4, checkboxGroupInput(
              "inf_upgrade",
              NULL,
              choices = list(
                "+2 Offense" = 1,
                "+2 Defense" = 2,
                "-2 Enabler requirement" = 3
              )
            ))
          ),
          tags$hr(style = "border-color: black; border-style: dotted;"),
          fluidRow(
            column(
              3,
              numericInput(
                "arm_AC",
                "Armor AC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              3,
              numericInput(
                "arm_RC",
                "Armor RC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              2,
              tags$div(style = "text-align: right; padding: 20px", "Upgrades to Armor")
            ),
            column(4, checkboxGroupInput(
              "arm_upgrade",
              NULL,
              choices = list(
                "+2 Offense" = 1,
                "+2 Defense" = 2,
                "-2 Enabler requirement" = 3
              )
            ))
          ),
          tags$hr(style = "border-color: black; border-style: dotted;"),
          fluidRow(
            column(
              3,
              numericInput(
                "str_AC",
                "Stryker AC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              3,
              numericInput(
                "str_RC",
                "Stryker RC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              2,
              tags$div(style = "text-align: right; padding: 20px", "Upgrades to Stryker")
            ),
            column(4, checkboxGroupInput(
              "str_upgrade",
              NULL,
              choices = list(
                "+2 Offense" = 1,
                "+2 Defense" = 2,
                "-2 Enabler requirement" = 3
              )
            ))
          ),
          tags$hr(style = "border-color: black; border-style: dotted;"),
          fluidRow(
            column(
              3,
              numericInput(
                "avi_AC",
                "Aviation AC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              3,
              numericInput(
                "avi_RC",
                "Aviation RC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              2,
              tags$div(style = "text-align: right; padding: 20px", "Upgrades to Aviation")
            ),
            column(4, checkboxGroupInput(
              "avi_upgrade",
              NULL,
              choices = list(
                "+2 Offense" = 1,
                "+2 Defense" = 2,
                "-2 Enabler requirement" = 3
              )
            ))
          ),
          tags$hr(style = "border-color: black; border-style: dotted;"),
          fluidRow(
            column(
              3,
              numericInput(
                "art_AC",
                "Artillery AC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              3,
              numericInput(
                "art_RC",
                "Artillery RC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              2,
              tags$div(style = "text-align: right; padding: 20px", "Upgrades to Artillery")
            ),
            column(4, checkboxGroupInput(
              "art_upgrade",
              NULL,
              choices = list(
                "+2 Offense" = 1,
                "+2 Defense" = 2,
                "-2 Enabler requirement" = 3
              )
            ))
          ),
          tags$hr(style = "border-color: black; border-style: dotted;"),
          fluidRow(
            column(
              3,
              numericInput(
                "so_AC",
                "Special Operations AC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              3,
              numericInput(
                "so_RC",
                "Special Operations RC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              2,
              tags$div(style = "text-align: right; padding: 20px", "Upgrades to Special Operations")
            ),
            column(4, checkboxGroupInput(
              "so_upgrade",
              NULL,
              choices = list(
                "+2 Offense" = 1,
                "+2 Defense" = 2,
                "-2 Enabler requirement" = 3
              )
            ))
          ),
          tags$hr(style = "border-color: black; border-style: dotted;"),
          fluidRow(
            column(
              3,
              numericInput(
                "ad_AC",
                "Air Defense AC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              3,
              numericInput(
                "ad_RC",
                "Air Defense RC Quantity",
                min = 0,
                max = 1000,
                value = 0
              )
            ),
            column(
              2,
              tags$div(style = "text-align: right; padding: 20px", "Upgrades to Air Defense")
            ),
            column(4, checkboxGroupInput(
              "ad_upgrade",
              NULL,
              choices = list(
                "+2 Offense" = 1,
                "+2 Defense" = 2,
                "-2 Enabler requirement" = 3
              )
            ))
          ),
          tags$hr(style = "border-color: black; border-style: dotted;"),
          fluidRow(column(
            3,
            numericInput(
              "ena_AC",
              "Enablers AC Quantity",
              min = 0,
              max = 1000,
              value = 0
            )
          ), column(
            3,
            numericInput(
              "ena_RC",
              "Enablers RC Quantity",
              min = 0,
              max = 1000,
              value = 0
            ),
          ), ),
          tags$hr(style = "border-color: black; border-style: dotted;"),
          
          # pie charts for force structure and AC/RC
          
          fluidRow(column(
            12, align = "center", column(6, plotlyOutput("force_pie")), column(6, plotlyOutput("ACRC_pie"))
          )),
          
          # reserve component and upgrades dice rolls
          
          banner_function("(5) Reserve component and upgrades dice rolls"),
          br(),
          
          fluidRow(
            column(
              4,
              offset = 1,
              div(
                numericInput(
                  "RC_factor_die_roll",
                  "RC capability dice roll",
                  value = 12,
                  min = 2,
                  max = 12
                ),
                class = "dice"
              ),
              br(),
              div(class = "notice", textOutput("RC_factor2"))
            ),
            column(
              5,
              offset = 1,
              div(class = "notice", textOutput("upgrades_min")),
              conditionalPanel(condition = "input.political_event_1 == 'pol4'", br(), div(class = "notice", textOutput("upgrade_reminder"))),
              br(),
              div(
                numericInput(
                  "upgrades_die_roll",
                  "Force upgrades dice roll",
                  min = 2,
                  max = 12,
                  value = 12
                ),
                class = "dice"
              ),
              htmlOutput("overrun"),
              br(),
              actionButton(
                "upgrades_cancel",
                "Cancel all upgrades (receive 50% of costs back)",
                icon("arrows-rotate", "fa-spin")
              ),
              make_info_box("circle-info", upgrade_cancel_text)
            )
          ),
          br(),
          
          # phase 2: actual capabilities and crisis outcomes
          
          banner_function("(6) Crisis and crisis outcome"),
          br(),
          
          # R&D computer rolls
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton("rd_button1", "Roll for R&D", icon("dice", "fa-shake")),
              textOutput("rd_die_roll1"),
              conditionalPanel(
                condition = "input.tech_retry == 1",
                actionButton("rd_button2", "Roll for R&D retry", icon("dice", "fa-shake")),
                textOutput("rd_die_roll2")
              ),
            ),
          ),
          
          # readiness computer rolls
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "read_button",
                "Roll for readiness event",
                icon("dice", "fa-shake")
              ),
              textOutput("readiness_die_roll"),
              actionButton(
                "read_button2",
                "Roll for second readiness event",
                icon("dice", "fa-shake")
              ),
              textOutput("readiness2_die_roll")
            ),
          ),
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton("milcon_button", "Roll for MILCON", icon("dice", "fa-shake")),
              textOutput("milcon_die_roll")
            ),
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton("milpers_button", "Roll for MILPERS", icon("dice", "fa-shake")),
              textOutput("milpers_die_roll")
            ),
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton("om_button", "Roll for O&M", icon("dice", "fa-shake")),
              textOutput("om_die_roll")
            ),
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton("proc_button", "Roll for procurement", icon("dice", "fa-shake")),
              textOutput("proc_die_roll")
            ),
          ),
          
          # force upgrades computer  roll
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "upgrades_button",
                "Roll for force upgrades",
                icon("dice", "fa-shake")
              ),
              textOutput("upgrades_die_roll")
            )
          ),
          
          # RC capability factor computer roll
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "RC_factor",
                "Roll for RC capability factor",
                icon("dice", "fa-shake")
              ),
              textOutput("RC_die_roll")
            )
          ),
          
          # crisis determination computer roll
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "crisis_button",
                "Roll to determine your crisis",
                icon("dice", "fa-shake")
              ),
              textOutput("crisis_die_roll")
            )
          ),
          
          # crisis determination
          
          fluidRow(
            column(
              4,
              offset = 1,
              selectInput(
                "crisis",
                "Select the crisis",
                choices = c(
                  "None",
                  "Steady State" = "steady",
                  "Humanitarian Crisis" = "human",
                  "Rogue State" = "rogue",
                  "ISIS 2.0" = "isis",
                  "Defensive Campaign (vs Peer Competitor)" = "defense",
                  "Offensive Campaign (vs Peer Competitor)" = "offense"
                )
              ),
              br(),
              div(
                align = "center",
                conditionalPanel(condition = "input.crisis == 'steady'", img(src =
                                                                               "crisis1.png")),
                conditionalPanel(condition = "input.crisis == 'human'", img(src =
                                                                              "crisis2.png")),
                conditionalPanel(condition = "input.crisis == 'rogue'", img(src =
                                                                              "crisis3.png")),
                conditionalPanel(condition = "input.crisis == 'isis'", img(src =
                                                                             "crisis4.png")),
                conditionalPanel(condition = "input.crisis == 'defense'", img(src =
                                                                                "crisis5.png")),
                conditionalPanel(condition = "input.crisis == 'offense'", img(src =
                                                                                "crisis6.png"))
              )
            ),
            column(5, offset = 1, tableOutput("crisis_reqs"))
          ),
          
          fluidRow(
            column(4, offset = 1, div(class = "notice", textOutput("min_crisis_roll"))),
            column(
              5,
              offset = 1,
              conditionalPanel(condition = "input.political_event_1 == 'pol6'", div(div(
                class = "notice", textOutput("crisis_reminder")
              ), br())),
              div(
                numericInput(
                  "crisis_outcome_die_roll",
                  "Crisis outcome dice roll",
                  value = 12,
                  min = 2,
                  max = 12
                ),
                class = "dice"
              ),
              conditionalPanel(
                condition = "input.dice == 'computer'",
                actionButton(
                  "crisis_outcome_button",
                  "Roll to determine your crisis outcome",
                  icon("dice", "fa-shake")
                ),
                textOutput("crisis_outcome_die_roll")
              )
            )
          ),
          
          # end of round loss adjustments
          
          br(),
          
          banner_function(
            "(7) End of round 1: losses to your force and setting the stage for round 2"
          ),
          br(),
          
          fluidRow(column(
            4, offset = 4, div(
              class = "notice",
              textOutput("force_losses"),
              br(),
              textOutput("force_losses2")
            )
          )),
          
          tags$hr(style = "border-color: black; border-style: dotted;"),
          
          fluidRow(br(), column(
            4, div(style = "text-align:center;", tags$strong("Current force structure"))
          ), column(
            4, div(style = "text-align:center;", tags$strong("Forces to lose"))
          ), column(
            4, div(style = "text-align:center;", tags$strong("Force structure heading into round 2"))
          )),
          
          fluidRow(
            column(4, align = "center", div(tableOutput("forces_selected"), style =
                                              "font-size:100%")),
            column(
              2,
              numericInput(
                "inf_AC_lose",
                "Infantry AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "arm_AC_lose",
                "Armor AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "str_AC_lose",
                "Stryker AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "avi_AC_lose",
                "Aviation AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "art_AC_lose",
                "Artillery AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "so_AC_lose",
                "Special Operations AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "ad_AC_lose",
                "Air Defense AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "ena_AC_lose",
                "Enablers AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              )
            ),
            column(
              2,
              numericInput(
                "inf_RC_lose",
                "Infantry RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "arm_RC_lose",
                "Armor RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "str_RC_lose",
                "Stryker RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "avi_RC_lose",
                "Aviation RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "art_RC_lose",
                "Artillery RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "so_RC_lose",
                "Special Operations RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "ad_RC_lose",
                "Air Defense RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "ena_RC_lose",
                "Enablers RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
            ),
            column(4, align = "center", tableOutput("forces_selected2"))
          ),
          
          tags$hr(style = "border-color: black; border-style: dotted;"),
          
          fluidRow(column(
            6,
            offset = 3,
            align = "center",
            uiOutput("end_round1"),
            htmlOutput("end_round1_2")
          )),
          br(),
          fluidRow(column(
            12, div(class = "unclass_bottom", "(U) Unclassified")
          ))
          
        ) # ends main panel
      )   # ends sidebarlayout
      
      
    ),
    #ends tab panel
    
    # Round 2 code
    
    tabPanel(
      "Round 2",
      value = "round2",
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          style = "background-color: white; height: 100%; overflow-y: auto;",
          
          # rd2: error alerts box
          
          fluidRow(
            column(
              12,
              style = "background-color:yellow; color:black; border-style: solid; border-color: black; border-radius: 8px",
              tags$strong("Error Alerts"),
              textOutput("tw_error1"),
              textOutput("tw_error2"),
              textOutput("tw_error3"),
              textOutput("tw_error4"),
              textOutput("tw_error5"),
              textOutput("tw_NA_error"),
              textOutput("tw_no_error")
            )
          ),
          br(),
          
          # rd2: funding box
          
          fluidRow(
            column(
              width = 12,
              style = "background-color:#FDF0EE; border-style: solid; border-radius: 8px;",
              fluidRow(
                style = "text-align: center;",
                column(
                  6,
                  tags$strong("Funding Allocated"),
                  textOutput("tw_allocated")
                ),
                column(
                  6,
                  tags$strong("Funding Remaining"),
                  textOutput("tw_remaining")
                )
              )
            ),
          ),
          br(),
          
          
          # rd2: game dashboard box
          
          fluidRow(
            column(
              12,
              style = "height: 40vh; overflow-y: auto; background-color:#FDF0EE; border-style: solid; border-radius: 8px;",
              br(),
              textOutput("tw_rd_costs"),
              textOutput("tw_rd_costs2"),
              tags$hr(style = "border-color: black;"),
              "Selected forces costs",
              tableOutput("tw_forces_cost"),
              tags$hr(style = "border-color: black;"),
              textOutput("tw_upgrades_cost"),
              tags$hr(style = "border-color: black;"),
              textOutput("tw_ena_cap"),
              textOutput("tw_ena_req"),
              textOutput("tw_sustainment_percent"),
              tags$hr(style = "border-color: black;"),
              textOutput("tw_offense"),
              textOutput("tw_defense"),
              textOutput("tw_meet_priority"),
              textOutput("tw_power_ratio")
            )
          ),
          
          br(),
          
          # rd2: failure points box
          
          fluidRow(
            column(12, style = "background-color:#ff8080; border-style: solid; border-radius: 8px;", div(style =
                                                                                                           "text-align:center;", fluidRow(
                                                                                                             column(
                                                                                                               6,
                                                                                                               tags$strong("Readiness Failure Points"),
                                                                                                               textOutput("tw_read_failure"),
                                                                                                             ),
                                                                                                             column(
                                                                                                               6,
                                                                                                               tags$strong("Crisis Failure Points"),
                                                                                                               textOutput("tw_crisis_failure")
                                                                                                             )
                                                                                                           )))
          )
        ),
        # rd2: end sidebar
        
        mainPanel(
          width = 9,
          style = "height:100%; overflow-y: auto;",
          
          fluidRow(column(12, strong(
            htmlOutput("pre_rd2_reminder")
          ))),
          
          # rd2: political event
          
          br(),
          banner_function("(1) Select the political event"),
          br(),
          
          fluidRow(
            column(
              4,
              conditionalPanel(
                condition = "input.dice == 'computer'",
                br(),
                actionButton(
                  "tw_pol_button",
                  "Roll for political event",
                  icon("dice", "fa-shake")
                ),
                textOutput("tw_pol_die_roll")
              ),
              selectInput(
                "tw_political_event_1",
                "Select the political event",
                choices = c(
                  "None",
                  "Congressional Interests" = "pol1",
                  "Unfriendly Administration" = "pol2",
                  "Friendly Administration" = "pol3",
                  "Cyber Attack" = "pol4",
                  "White House Priority" = "pol5",
                  "Uprising" = "pol6"
                )
              ),
              conditionalPanel(condition = "input.tw_political_event_1 == 'pol1'", img(src =
                                                                                         "pol1.png")),
              conditionalPanel(condition = "input.tw_political_event_1 == 'pol2'", img(src =
                                                                                         "pol2.png")),
              conditionalPanel(condition = "input.tw_political_event_1 == 'pol3'", img(src =
                                                                                         "pol3.png")),
              conditionalPanel(condition = "input.tw_political_event_1 == 'pol4'", img(src =
                                                                                         "pol4.png")),
              conditionalPanel(condition = "input.tw_political_event_1 == 'pol5'", img(src =
                                                                                         "pol5.png")),
              conditionalPanel(condition = "input.tw_political_event_1 == 'pol6'", img(src =
                                                                                         "pol6.png"))
            ),
            
            # rd2: manual die rolls
            
            column(
              3,
              conditionalPanel(
                condition = "input.dice == 'manual'",
                conditionalPanel(
                  condition = "input.tw_political_event_1 == 'pol1'",
                  numericInput(
                    "tw_pol1_die_roll1",
                    "Enter force type roll",
                    value = 2,
                    min = 2,
                    max = 12
                  ),
                  numericInput(
                    "tw_pol1_die_roll2",
                    "Enter quantity roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_political_event_1 == 'pol4'",
                  numericInput(
                    "tw_pol4_die_roll1",
                    "Enter force upgrade roll reduction roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_political_event_1 == 'pol6'",
                  numericInput(
                    "tw_pol6_die_roll1",
                    "Enter crisis outcome roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                )
              ),
              
              # rd2: computer-generated die rolls
              
              conditionalPanel(
                condition = "input.dice == 'computer'",
                conditionalPanel(
                  condition = "input.tw_political_event_1 == 'pol1'",
                  actionButton(
                    "tw_pol1_button1",
                    "Roll for force type",
                    icon("dice", "fa-shake")
                  ),
                  actionButton(
                    "tw_pol1_button2",
                    "Roll for quantity",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_pol1_die_roll1"),
                  textOutput("tw_pol1_die_roll2")
                ),
                conditionalPanel(
                  condition = "input.tw_political_event_1 == 'pol4'",
                  actionButton(
                    "tw_pol4_button1",
                    "Roll for reduction to force upgrade rolls",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_pol4_die_roll1")
                ),
                conditionalPanel(
                  condition = "input.tw_political_event_1 == 'pol5'",
                  actionButton(
                    "tw_pol5_button1",
                    "Roll to determine crisis requiring additional requirements",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_pol5_die_roll1")
                ),
                conditionalPanel(
                  condition = "input.tw_political_event_1 == 'pol6'",
                  actionButton(
                    "tw_pol6_button1",
                    "Roll to determine impact to crisis outcome",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_pol6_die_roll1")
                )
              ),
              
              # rd2: White House priority crisis and requirements
              
              conditionalPanel(
                condition = "input.tw_political_event_1 == 'pol5'",
                selectInput(
                  "tw_priority_crisis",
                  "Select the White House priority crisis",
                  choices = c(
                    "None",
                    "Steady State" = "steady",
                    "Humanitarian Crisis" = "human",
                    "Rogue State" = "rogue",
                    "ISIS 2.0" = "isis",
                    "Defensive Campaign (vs Peer Competitor)" = "defense",
                    "Offensive Campaign (vs Peer Competitor)" = "offense"
                  )
                )
              )
            ),
            
            column(
              4,
              conditionalPanel(
                condition = "!(input.tw_political_event_1 == 'None')",
                class = "notice",
                tags$strong(textOutput("tw_pol_event"))
              )
            )
          ),
          
          # rd2: research and development
          
          br(),
          banner_function(
            "(2) Make your research and development choices",
            "circle-info",
            rd_text
          ),
          br(),
          
          fluidRow(
            column(
              6,
              uiOutput("tw_tech_target"),
              htmlOutput("tw_expedited"),
              br(),
              uiOutput("tw_risk_target"),
              div(class = "notice", textOutput("tw_rd_min_roll"))
            ),
            column(
              4,
              div(
                numericInput(
                  "tw_tech_die_roll1",
                  "First R&D die roll",
                  min = 1,
                  max = 6,
                  value = 3
                ),
                class = "dice"
              ),
              checkboxInput(
                "tw_tech_retry",
                div(
                  "Do you wish to retry if you fail to fund your target tech level?",
                  class = "inputs"
                ),
                width = "100%"
              ),
              conditionalPanel(condition = "input.tw_tech_retry==1", div(div(
                class = "dice",
                numericInput(
                  "tw_tech_die_roll2",
                  "Second R&D die roll",
                  min = 1,
                  max = 6,
                  value = 3
                )
              ), br())),
              div(class = "notice", tags$strong(textOutput("tw_tech_level")))
            )
          ),
          
          # rd2: readiness
          
          br(),
          banner_function("(3a) Select the readiness events"),
          br(),
          
          # rd2: readiness1
          
          fluidRow(column(
            12,
            selectInput(
              "tw_readiness_event",
              "Select the readiness event",
              choices = c(
                "None",
                "Public Opinion" = "read1",
                "Training Environment" = "read2",
                "Health and Quality of Life" = "read3",
                "Infrastructure" = "read4",
                "Supply Chain" = "read5",
                "Corruption and Scandals" = "read6"
              )
            ),
          )),
          
          # rd2: manual selections
          
          fluidRow(
            column(
              4,
              conditionalPanel(
                condition = "input.dice == 'manual'",
                class = "dice",
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read1'",
                  numericInput(
                    "tw_read1_die_roll",
                    "Enter MILPERS effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read2'",
                  numericInput(
                    "tw_read2_die_roll",
                    "Enter O&M effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read3'",
                  numericInput(
                    "tw_read3_die_roll",
                    "Enter MILPERS and MILCON effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read4'",
                  numericInput(
                    "tw_read4_die_roll",
                    "Enter MILCON effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read5'",
                  numericInput(
                    "tw_read5_die_roll",
                    "Enter procurement and O&M effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read6'",
                  numericInput(
                    "tw_read6_die_roll1",
                    "Enter type roll",
                    value = 1,
                    min = 1,
                    max = 6
                  ),
                  numericInput(
                    "tw_read6_die_roll2",
                    "Enter effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                )
              ),
              
              # rd2: readiness1 computer-generated rolls
              
              conditionalPanel(
                condition = "input.dice == 'computer'",
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read1'",
                  actionButton(
                    "tw_read1_button",
                    "Roll for MILPERS effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read1_die_roll")
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read2'",
                  actionButton(
                    "tw_read2_button",
                    "Roll for O&M effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read2_die_roll")
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read3'",
                  actionButton(
                    "tw_read3_button",
                    "Roll for MILPERS and MILCON effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read3_die_roll")
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read4'",
                  actionButton(
                    "tw_read4_button",
                    "Roll for MILCON effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read4_die_roll")
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read5'",
                  actionButton(
                    "tw_read5_button",
                    "Roll for procurement and O&M effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read5_die_roll")
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event == 'read6'",
                  actionButton("tw_read6_button1", "Roll for type", icon("dice", "fa-shake")),
                  actionButton("tw_read6_button2", "Roll for effect", icon("dice", "fa-shake")),
                  textOutput("tw_read6_die_roll1"),
                  textOutput("tw_read6_die_roll2")
                )
              ),
            ),
            conditionalPanel(condition = "input.tw_readiness_event == 'read1'", column(
              4, img(src = "readiness1a.png"), img(src = "readiness1b.png")
            )),
            conditionalPanel(condition = "input.tw_readiness_event == 'read2'", column(
              4, img(src = "readiness2a.png"), img(src = "readiness2b.png")
            )),
            conditionalPanel(condition = "input.tw_readiness_event == 'read3'", column(
              4, img(src = "readiness3a.png"), img(src = "readiness3b.png")
            )),
            conditionalPanel(condition = "input.tw_readiness_event == 'read4'", column(
              4, img(src = "readiness4a.png"), img(src = "readiness4b.png")
            )),
            conditionalPanel(condition = "input.tw_readiness_event == 'read5'", column(
              4, img(src = "readiness5a.png"), img(src = "readiness5b.png")
            )),
            conditionalPanel(condition = "input.tw_readiness_event == 'read6'", column(4, img(src =
                                                                                                "readiness6.png"))),
            
            conditionalPanel(condition = "!(input.tw_readiness_event == 'None')", column(3, class = "notice", strong(
              textOutput("tw_readiness_impact")
            )))
          ),
          # rd2: end readiness 1 row
          
          tags$hr(style = "border-color: black; border-style: dotted;"),
          
          # rd2: readiness event #2
          
          fluidRow(column(
            12,
            selectInput(
              "tw_readiness_event2",
              "Select the second readiness event",
              choices = c(
                "None",
                "Public Opinion" = "read1",
                "Training Environment" = "read2",
                "Health and Quality of Life" = "read3",
                "Infrastructure" = "read4",
                "Supply Chain" = "read5",
                "Corruption and Scandals" = "read6"
              )
            ),
          )),
          
          # rd2: manual selections
          
          fluidRow(
            column(
              4,
              conditionalPanel(
                condition = "input.dice == 'manual'",
                class = "dice",
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read1'",
                  numericInput(
                    "tw_read2_1_die_roll",
                    "Enter MILPERS effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read2'",
                  numericInput(
                    "tw_read2_2_die_roll",
                    "Enter O&M effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read3'",
                  numericInput(
                    "tw_read2_3_die_roll",
                    "Enter MILPERS and MILCON effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read4'",
                  numericInput(
                    "tw_read2_4_die_roll",
                    "Enter MILCON effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read5'",
                  numericInput(
                    "tw_read2_5_die_roll",
                    "Enter procurement and O&M effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read6'",
                  numericInput(
                    "tw_read2_6_die_roll1",
                    "Enter type roll",
                    value = 1,
                    min = 1,
                    max = 6
                  ),
                  numericInput(
                    "tw_read2_6_die_roll2",
                    "Enter effect roll",
                    value = 1,
                    min = 1,
                    max = 6
                  )
                )
              ),
              
              # rd2: computer-generated rolls
              
              conditionalPanel(
                condition = "input.dice == 'computer'",
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read1'",
                  actionButton(
                    "tw_read2_1_button",
                    "Roll for MILPERS effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read2_1_die_roll")
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read2'",
                  actionButton(
                    "tw_read2_2_button",
                    "Roll for O&M effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read2_2_die_roll")
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read3'",
                  actionButton(
                    "tw_read2_3_button",
                    "Roll for MILPERS and MILCON effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read2_3_die_roll")
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read4'",
                  actionButton(
                    "tw_read2_4_button",
                    "Roll for MILCON effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read2_4_die_roll")
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read5'",
                  actionButton(
                    "tw_read2_5_button",
                    "Roll for procurement and O&M effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read2_5_die_roll")
                ),
                conditionalPanel(
                  condition = "input.tw_readiness_event2 == 'read6'",
                  actionButton("tw_read2_6_button1", "Roll for type", icon("dice", "fa-shake")),
                  actionButton(
                    "tw_read2_6_button2",
                    "Roll for effect",
                    icon("dice", "fa-shake")
                  ),
                  textOutput("tw_read2_6_die_roll1"),
                  textOutput("tw_read2_6_die_roll2")
                )
              )
            ),
            conditionalPanel(condition = "input.tw_readiness_event2 == 'read1'", column(
              4, img(src = "readiness1a.png"), img(src = "readiness1b.png")
            )),
            conditionalPanel(condition = "input.tw_readiness_event2 == 'read2'", column(
              4, img(src = "readiness2a.png"), img(src = "readiness2b.png")
            )),
            conditionalPanel(condition = "input.tw_readiness_event2 == 'read3'", column(
              4, img(src = "readiness3a.png"), img(src = "readiness3b.png")
            )),
            conditionalPanel(condition = "input.tw_readiness_event2 == 'read4'", column(
              4, img(src = "readiness4a.png"), img(src = "readiness4b.png")
            )),
            conditionalPanel(condition = "input.tw_readiness_event2 == 'read5'", column(
              4, img(src = "readiness5a.png"), img(src = "readiness5b.png")
            )),
            conditionalPanel(condition = "input.tw_readiness_event2 == 'read6'", column(4, img(src =
                                                                                                 "readiness6.png"))),
            
            conditionalPanel(condition = "!(input.tw_readiness_event2 == 'None')", column(3, class = "notice", strong(
              textOutput("tw_readiness_impact2")
            )))
          ),
          br(),
          
          # rd2: MILCON, MILPERS, O&M, procurement funding
          
          banner_function(
            "(3b) Set your readiness funding levels",
            "circle-info",
            readiness_funding_text
          ),
          
          fluidRow(
            column(
              5,
              br(),
              sliderInput(
                "tw_milcon_fund",
                "MILCON (Military Construction) % Funded",
                min = 0,
                max = 100,
                value = 100
              ),
              div(class = "notice", textOutput("tw_milcon_min")),
              br(),
              div(
                numericInput(
                  "tw_milcon_roll",
                  "MILCON dice roll",
                  min = 2,
                  max = 12,
                  value = 6
                ),
                class = "dice"
              ),
              tags$hr(style = "border-color: black; border-style: dotted;"),
              sliderInput(
                "tw_milpers_fund",
                "MILPERS (Military Pesonnel) % Funded",
                min = 0,
                max = 100,
                value = 100
              ),
              div(class = "notice", textOutput("tw_milpers_min")),
              br(),
              div(
                numericInput(
                  "tw_milpers_roll",
                  "MILPERS dice roll",
                  min = 2,
                  max = 12,
                  value = 6
                ),
                class = "dice"
              )
            ),
            
            column(
              5,
              offset = 1,
              br(),
              sliderInput(
                "tw_om_fund",
                "O&M (Operations and Maintenance) % Funded",
                min = 0,
                max = 100,
                value = 100
              ),
              div(class = "notice", textOutput("tw_om_min")),
              br(),
              div(
                numericInput(
                  "tw_om_roll",
                  "O&M dice roll",
                  min = 2,
                  max = 12,
                  value = 6
                ),
                class = "dice"
              ),
              tags$hr(style = "border-color: black; border-style: dotted;"),
              sliderInput(
                "tw_proc_fund",
                "Procurement (other than new forces) % Funded",
                min = 0,
                max = 100,
                value = 100
              ),
              div(class = "notice", textOutput("tw_proc_min")),
              br(),
              div(
                numericInput(
                  "tw_proc_roll",
                  "Procurement dice roll",
                  min = 2,
                  max = 12,
                  value = 6
                ),
                class = "dice"
              )
            )
          ),
          
          br(),
          
          fluidRow(column(
            4,
            offset = 4,
            align = "center",
            div(
              class = "notice",
              tags$strong(
                "Overall Readiness % Funded",
                make_info_box(
                  "circle-info",
                  "This is the readiness factor for the combat power calculation"
                ),
                ":"
              ),
              textOutput("tw_overall_readiness")
            )
          )),
          
          # rd2: forces information table: costs and capabilities
          
          br(),
          banner_function(
            "(4a) Info: Force Capabilities Table",
            "circle-info",
            "Table will update to include any selected upgrades"
          ),
          br(),
          
          fluidRow(
            column(
              7,
              offset = 2,
              style = "background-color:#def0de; border-style: solid;",
              br(),
              "Readiness Cost: Total (MILCON / MILPERS / O&M / Procurement)",
              br(),
              "Artillery provides + 1 offense and Aviation provides + 1 defense if either is paired with Armor, Infantry, or Stryker",
              br(),
              "Each Enabler force meets 10 Enabler Requirements",
              tableOutput("tw_readiness_info")
            )
          ),
          
          br(),
          banner_function(
            "(4b) Select your forces and upgrades",
            "circle-info",
            select_forces_text
          ),
          br(),
          
          fluidRow(
            br(),
            column(3, div(
              style = "text-align:center;", tags$strong("Force structure from round 1")
            )),
            column(
              3,
              div(
                style = "text-align:center; border-style: dashed; background-color: #f2f2f2;",
                tags$strong("Units to disband"),
                make_info_box("circle-info", "Receive 50% of readiness costs back")
              )
            ),
            column(
              3,
              div(
                style = "text-align:center; border-style: solid;",
                tags$strong("New units and upgrades"),
                make_info_box(
                  "circle-info",
                  "Past upgrades remain; new upgrades apply to all units selected for the round"
                )
              )
            ),
            column(3, div(
              style = "text-align:center;", tags$strong("Updated round 2 force structure")
            ))
          ),
          
          fluidRow(
            column(3, tableOutput("tw_forces_selected2")),
            column(
              6,
              br(),
              fluidRow(
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "inf_AC_disband",
                    "INF AC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "inf_RC_disband",
                    "INF RC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_inf_AC",
                    "New INF AC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_inf_RC",
                    "New INF RC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(2, checkboxGroupInput(
                  "tw_inf_upgrade",
                  NULL,
                  choices = list(
                    "+2 Off" = 1,
                    "+2 Def" = 2,
                    "-2 Enabler" = 3
                  )
                ))
              ),
              fluidRow(
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "arm_AC_disband",
                    "ARM AC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "arm_RC_disband",
                    "ARM RC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_arm_AC",
                    "New ARM AC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_arm_RC",
                    "New ARM RC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(2, checkboxGroupInput(
                  "tw_arm_upgrade",
                  NULL,
                  choices = list(
                    "+2 Off" = 1,
                    "+2 Def" = 2,
                    "-2 Enabler" = 3
                  )
                ))
              ),
              fluidRow(
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "str_AC_disband",
                    "STR AC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "str_RC_disband",
                    "STR RC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_str_AC",
                    "New STR AC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_str_RC",
                    "New STR RC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(2, checkboxGroupInput(
                  "tw_str_upgrade",
                  NULL,
                  choices = list(
                    "+2 Off" = 1,
                    "+2 Def" = 2,
                    "-2 Enabler" = 3
                  )
                ))
              ),
              fluidRow(
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "avi_AC_disband",
                    "AVI AC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "avi_RC_disband",
                    "AVI RC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_avi_AC",
                    "New AVI AC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_avi_RC",
                    "New AVI RC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(2, checkboxGroupInput(
                  "tw_avi_upgrade",
                  NULL,
                  choices = list(
                    "+2 Off" = 1,
                    "+2 Def" = 2,
                    "-2 Enabler" = 3
                  )
                ))
              ),
              fluidRow(
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "art_AC_disband",
                    "ART AC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "art_RC_disband",
                    "ART RC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_art_AC",
                    "New ART AC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_art_RC",
                    "New ART RC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(2, checkboxGroupInput(
                  "tw_art_upgrade",
                  NULL,
                  choices = list(
                    "+2 Off" = 1,
                    "+2 Def" = 2,
                    "-2 Enabler" = 3
                  )
                ))
              ),
              fluidRow(
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "so_AC_disband",
                    "SO AC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "so_RC_disband",
                    "SO RC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_so_AC",
                    "New SO AC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_so_RC",
                    "New SO RC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(2, checkboxGroupInput(
                  "tw_so_upgrade",
                  NULL,
                  choices = list(
                    "+2 Off" = 1,
                    "+2 Def" = 2,
                    "-2 Enabler" = 3
                  )
                ))
              ),
              fluidRow(
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "ad_AC_disband",
                    "AD AC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "ad_RC_disband",
                    "AD RC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_ad_AC",
                    "New AD AC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_ad_RC",
                    "New AD RC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(2, checkboxGroupInput(
                  "tw_ad_upgrade",
                  NULL,
                  choices = list(
                    "+2 Off" = 1,
                    "+2 Def" = 2,
                    "-2 Enabler" = 3
                  )
                ))
              ),
              fluidRow(
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "ena_AC_disband",
                    "ENA AC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  3,
                  style = "background-color: #f2f2f2;",
                  numericInput(
                    "ena_RC_disband",
                    "ENA RC disband",
                    min = 0,
                    max = 100,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_ena_AC",
                    "New ENA AC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                ),
                column(
                  2,
                  numericInput(
                    "tw_ena_RC",
                    "New ENA RC",
                    min = 0,
                    max = 1000,
                    value = 0,
                    width = "75px"
                  )
                )
              )
            ),
            column(3, tableOutput("forces_selected_rd2")),
          ),
          
          tags$hr(style = "border-color: black; border-style: dotted;"),
          
          # rd2: pie charts for force structure and AC/RC
          
          fluidRow(column(
            12, align = "center", column(6, plotlyOutput("tw_force_pie")), column(6, plotlyOutput("tw_ACRC_pie"))
          )),
          
          # rd2: reserve component and upgrades dice rolls
          
          br(),
          banner_function("(5) Reserve component and upgrades dice rolls"),
          br(),
          
          fluidRow(
            column(
              4,
              offset = 1,
              div(
                numericInput(
                  "tw_RC_factor_die_roll",
                  "RC capability dice roll",
                  value = 12,
                  min = 2,
                  max = 12
                ),
                class = "dice"
              ),
              br(),
              div(class = "notice", textOutput("tw_RC_factor2"))
            ),
            column(
              5,
              offset = 1,
              div(class = "notice", textOutput("tw_upgrades_min")),
              conditionalPanel(condition = "input.tw_political_event_1 == 'pol4'", br(), div(
                class = "notice", textOutput("tw_upgrade_reminder")
              )),
              br(),
              div(
                numericInput(
                  "tw_upgrades_die_roll",
                  "Force upgrades dice roll",
                  min = 2,
                  max = 12,
                  value = 12
                ),
                class = "dice"
              ),
              htmlOutput("tw_overrun"),
              br(),
              actionButton(
                "tw_upgrades_cancel",
                "Cancel all upgrades (receive 50% of costs back)",
                icon("arrows-rotate", "fa-spin")
              ),
              make_info_box("circle-info", upgrade_cancel_text)
            )
          ),
          br(),
          
          # rd2: phase 2: actual capabilities and crisis outcomes
          
          banner_function("(6) Crisis and crisis outcomes"),
          br(),
          
          # rd2: R&D rolls
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton("tw_rd_button1", "Roll for R&D", icon("dice", "fa-shake")),
              textOutput("tw_rd_die_roll1"),
              conditionalPanel(
                condition = "input.tw_tech_retry == 1",
                actionButton("tw_rd_button2", "Roll for R&D retry", icon("dice", "fa-shake")),
                textOutput("tw_rd_die_roll2")
              ),
            ),
          ),
          
          # rd2: readiness computer rolls
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "tw_read_button",
                "Roll for readiness event",
                icon("dice", "fa-shake")
              ),
              textOutput("tw_readiness_die_roll"),
              actionButton(
                "tw_read_button2",
                "Roll for second readiness event",
                icon("dice", "fa-shake")
              ),
              textOutput("tw_readiness2_die_roll")
            ),
          ),
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton("tw_milcon_button", "Roll for MILCON", icon("dice", "fa-shake")),
              textOutput("tw_milcon_die_roll")
            ),
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "tw_milpers_button",
                "Roll for MILPERS",
                icon("dice", "fa-shake")
              ),
              textOutput("tw_milpers_die_roll")
            ),
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton("tw_om_button", "Roll for O&M", icon("dice", "fa-shake")),
              textOutput("tw_om_die_roll")
            ),
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "tw_proc_button",
                "Roll for procurement",
                icon("dice", "fa-shake")
              ),
              textOutput("tw_proc_die_roll")
            ),
          ),
          
          # rd2: force upgrades computer roll
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "tw_upgrades_button",
                "Roll for force upgrades",
                icon("dice", "fa-shake")
              ),
              textOutput("tw_upgrades_die_roll")
            )
          ),
          
          # rd2: RC capability factor computer roll
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "tw_RC_factor",
                "Roll for RC capability factor",
                icon("dice", "fa-shake")
              ),
              textOutput("tw_RC_die_roll")
            )
          ),
          
          # rd2: crisis determination computer roll
          
          fluidRow(
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "tw_crisis_button",
                "Roll to determine your crisis",
                icon("dice", "fa-shake")
              ),
              textOutput("tw_crisis_die_roll")
            )
          ),
          
          # rd2: crisis determination
          
          fluidRow(
            column(
              4,
              offset = 1,
              selectInput(
                "tw_crisis",
                "Select the crisis",
                choices = c(
                  "None",
                  "Steady State" = "steady",
                  "Humanitarian Crisis" = "human",
                  "Rogue State" = "rogue",
                  "ISIS 2.0" = "isis",
                  "Defensive Campaign (vs Peer Competitor)" = "defense",
                  "Offensive Campaign (vs Peer Competitor)" = "offense"
                )
              ),
              br(),
              div(
                align = "center",
                conditionalPanel(condition = "input.tw_crisis == 'steady'", img(src =
                                                                                  "crisis1.png")),
                conditionalPanel(condition = "input.tw_crisis == 'human'", img(src =
                                                                                 "crisis2.png")),
                conditionalPanel(condition = "input.tw_crisis == 'rogue'", img(src =
                                                                                 "crisis3.png")),
                conditionalPanel(condition = "input.tw_crisis == 'isis'", img(src =
                                                                                "crisis4.png")),
                conditionalPanel(condition = "input.tw_crisis == 'defense'", img(src =
                                                                                   "crisis5.png")),
                conditionalPanel(condition = "input.tw_crisis == 'offense'", img(src =
                                                                                   "crisis6.png"))
              )
            ),
            column(5, offset = 1, tableOutput("tw_crisis_reqs"))
          ),
          
          fluidRow(
            column(4, offset = 1, div(
              class = "notice", textOutput("tw_min_crisis_roll")
            )),
            column(
              5,
              offset = 1,
              conditionalPanel(condition = "input.tw_political_event_1 == 'pol6'", div(div(
                class = "notice", textOutput("tw_crisis_reminder")
              ), br())),
              div(
                numericInput(
                  "tw_crisis_outcome_die_roll",
                  "Crisis outcome dice roll",
                  value = 12,
                  min = 2,
                  max = 12
                ),
                class = "dice"
              ),
              conditionalPanel(
                condition = "input.dice == 'computer'",
                actionButton(
                  "tw_crisis_outcome_button",
                  "Roll to determine your crisis outcome",
                  icon("dice", "fa-shake")
                ),
                textOutput("tw_crisis_outcome_die_roll")
              )
            )
          ),
          
          # rd2: end of round loss adjustments
          
          br(),
          banner_function(
            "(7) End of round 2: losses to your force and setting the stage for the final round"
          ),
          br(),
          
          fluidRow(column(
            4, offset = 4, div(
              class = "notice",
              textOutput("tw_force_losses"),
              br(),
              textOutput("tw_force_losses2")
            )
          )),
          
          tags$hr(style = "border-color: black; border-style: dotted;"),
          
          fluidRow(br(), column(
            4, div(style = "text-align:center;", tags$strong("Current force structure"))
          ), column(
            4, div(style = "text-align:center;", tags$strong("Forces to lose"))
          ), column(
            4, div(style = "text-align:center;", tags$strong("Force structure heading into round 3"))
          )),
          
          fluidRow(
            column(4, align = "center", div(
              tableOutput("tw_forces_selected"), style = "font-size:100%"
            )),
            column(
              2,
              numericInput(
                "tw_inf_AC_lose",
                "Infantry AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_arm_AC_lose",
                "Armor AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_str_AC_lose",
                "Stryker AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_avi_AC_lose",
                "Aviation AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_art_AC_lose",
                "Artillery AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_so_AC_lose",
                "Special Operations AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_ad_AC_lose",
                "Air Defense AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_ena_AC_lose",
                "Enablers AC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              )
            ),
            column(
              2,
              numericInput(
                "tw_inf_RC_lose",
                "Infantry RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_arm_RC_lose",
                "Armor RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_str_RC_lose",
                "Stryker RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_avi_RC_lose",
                "Aviation RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_art_RC_lose",
                "Artillery RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_so_RC_lose",
                "Special Operations RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_ad_RC_lose",
                "Air Defense RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
              numericInput(
                "tw_ena_RC_lose",
                "Enablers RC to lose",
                min = 0,
                max = 100,
                value = 0,
                width = '150px'
              ),
            ),
            column(4, align = "center", tableOutput("tw_forces_selected_rd2"))
          ),
          tags$hr(style = "border-color: black; border-style: dotted;"),
          
          fluidRow(column(
            6,
            offset = 3,
            align = "center",
            uiOutput("tw_end_round1"),
            htmlOutput("tw_end_round1_2")
          )),
          br(),
          fluidRow(column(
            12, div(class = "unclass_bottom", "(U) Unclassified")
          ))
          
        ) # rd2: end main panel
      ) # rd2: sidebar layout
    ),
    # end round 2 tab
    
    # Round 3 code
    
    tabPanel("Round 3", value = "round3", sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "background-color: white; height: 100%; over-flow-y: auto;",
        
        # rd3: error checks
        
        fluidRow(
          column(
            12,
            style = "background-color:yellow; color:black; border-style: solid; border-color: black; border-radius: 8px;",
            tags$strong("Error Alerts"),
            textOutput("th_error1"),
            textOutput("th_error2"),
            textOutput("th_error3"),
            textOutput("th_error5"),
            textOutput("th_NA_error"),
            textOutput("th_no_error")
          )
        ),
        
        br(),
        
        # rd3: funding box
        
        fluidRow(
          column(
            12,
            style = "background-color:#FDF0EE; border-style: solid; border-radius: 8px",
            fluidRow(
              style = "text-align: center;",
              column(
                6,
                tags$strong("Funding Allocated"),
                textOutput("th_allocated")
              ),
              column(
                6,
                tags$strong("Funding Remaining"),
                textOutput("th_remaining")
              )
            )
          )
        ),
        br(),
        
        # rd3: game dashboard box
        
        fluidRow(
          column(
            12,
            style = "height: 40vh; overflow-y: auto; background-color: #FDF0EE; border-style: solid; border-radius: 8px;",
            br(),
            textOutput("th_rd_costs"),
            textOutput("th_rd_costs2"),
            tags$hr(style = "border-color: black;"),
            "Selected forces costs",
            tableOutput("th_forces_cost"),
            tags$hr(style = "border-color: black;"),
            textOutput("th_upgrades_cost"),
            tags$hr(style = "border-color: black;"),
            textOutput("th_ena_cap"),
            textOutput("th_ena_req"),
            textOutput("th_sustainment_percent"),
            tags$hr(style = "border-color: black;"),
            textOutput("th_offense"),
            textOutput("th_defense"),
            textOutput("th_meet_priority"),
            textOutput("th_power_ratio")
          )
        ),
        br(),
        
        # rd3: failure points
        
        fluidRow(
          column(12, style = "background-color:#ff8080; border-style: solid; border-radius: 8px;", div(style = "text-align: center;", fluidRow(
            column(
              6,
              tags$strong("Readiness Failure Points"),
              textOutput("th_read_failure")
            ),
            column(
              6,
              tags$strong("Crisis Failure Points"),
              textOutput("th_crisis_failure")
            )
          )))
        )
      ),
      # rd3: end sidebar
      
      mainPanel(
        width = 9,
        style = "height:100%; overflow-y: auto;",
        
        fluidRow(column(12, strong(
          htmlOutput("pre_rd3_reminder")
        ))),
        
        # political event
        
        br(),
        banner_function("(1) Select the political event"),
        br(),
        
        fluidRow(
          column(
            4,
            conditionalPanel(
              condition = "input.dice == 'computer'",
              br(),
              actionButton(
                "th_pol_button",
                "Roll for political event",
                icon("dice", "fa-shake")
              ),
              textOutput("th_pol_die_roll")
            ),
            selectInput(
              "th_political_event_1",
              "Select the political event",
              choices = c(
                "None",
                "Congressional Interests" = "pol1",
                "Unfriendly Administration" = "pol2",
                "Friendly Administration" = "pol3",
                "Cyber Attack" = "pol4",
                "White House Priority" = "pol5",
                "Uprising" = "pol6"
              )
            ),
            conditionalPanel(condition = "input.th_political_event_1 == 'pol1'", img(src =
                                                                                       "pol1.png")),
            conditionalPanel(condition = "input.th_political_event_1 == 'pol2'", img(src =
                                                                                       "pol2.png")),
            conditionalPanel(condition = "input.th_political_event_1 == 'pol3'", img(src =
                                                                                       "pol3.png")),
            conditionalPanel(condition = "input.th_political_event_1 == 'pol4'", img(src =
                                                                                       "pol4.png")),
            conditionalPanel(condition = "input.th_political_event_1 == 'pol5'", img(src =
                                                                                       "pol5.png")),
            conditionalPanel(condition = "input.th_political_event_1 == 'pol6'", img(src =
                                                                                       "pol6.png"))
          ),
          
          # rd3: manual die rolls
          
          column(
            3,
            conditionalPanel(
              condition = "input.dice == 'manual'",
              conditionalPanel(
                condition = "input.th_political_event_1 == 'pol1'",
                numericInput(
                  "th_pol1_die_roll1",
                  "Enter force type roll",
                  value = 2,
                  min = 2,
                  max = 12
                ),
                numericInput(
                  "th_pol1_die_roll2",
                  "Enter quantity roll",
                  value = 2,
                  min = 2,
                  max = 12
                )
              ),
              conditionalPanel(
                condition = "input.th_political_event_1 == 'pol4'",
                numericInput(
                  "th_pol4_die_roll1",
                  "Enter force upgrade roll reduction roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_political_event_1 == 'pol6'",
                numericInput(
                  "th_pol6_die_roll1",
                  "Enter crisis outcome roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              )
            ),
            
            # rd3: computer-generated die rolls
            
            conditionalPanel(
              condition = "input.dice == 'computer'",
              conditionalPanel(
                condition = "input.th_political_event_1 == 'pol1'",
                actionButton(
                  "th_pol1_button1",
                  "Roll for force type",
                  icon("dice", "fa-shake")
                ),
                actionButton(
                  "th_pol1_button2",
                  "Roll for quantity",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_pol1_die_roll1"),
                textOutput("th_pol1_die_roll2")
              ),
              conditionalPanel(
                condition = "input.th_political_event_1 == 'pol4'",
                actionButton(
                  "th_pol4_button1",
                  "Roll for reduction to force upgrade rolls",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_pol4_die_roll1")
              ),
              conditionalPanel(
                condition = "input.th_political_event_1 == 'pol5'",
                actionButton(
                  "th_pol5_button1",
                  "Roll to determine crisis requiring additional requirements",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_pol5_die_roll1")
              ),
              conditionalPanel(
                condition = "input.th_political_event_1 == 'pol6'",
                actionButton(
                  "th_pol6_button1",
                  "Roll to determine impact to crisis outcome",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_pol6_die_roll1")
              )
            ),
            
            # rd3: White House priority crisis and requirements
            
            conditionalPanel(
              condition = "input.th_political_event_1 == 'pol5'",
              selectInput(
                "th_priority_crisis",
                "Select the White House priority crisis",
                choices = c(
                  "None",
                  "Steady State" = "steady",
                  "Humanitarian Crisis" = "human",
                  "Rogue State" = "rogue",
                  "ISIS 2.0" = "isis",
                  "Defensive Campaign (vs Peer Competitor)" = "defense",
                  "Offensive Campaign (vs Peer Competitor)" = "offense"
                )
              )
            )
          ),
          
          column(
            4,
            conditionalPanel(
              condition = "!(input.th_political_event_1 == 'None')",
              class = "notice",
              tags$strong(textOutput("th_pol_event"))
            )
          )
        ),
        
        # rd3: research and development
        
        br(),
        banner_function(
          "(2) Make your research and development choices",
          "circle-info",
          rd_text
        ),
        br(),
        
        fluidRow(
          column(
            6,
            uiOutput("th_tech_target"),
            htmlOutput("th_expedited"),
            br(),
            uiOutput("th_risk_target"),
            div(class = "notice", textOutput("th_rd_min_roll"))
          ),
          column(
            4,
            div(
              numericInput(
                "th_tech_die_roll1",
                "First R&D die roll",
                min = 1,
                max = 6,
                value = 3
              ),
              class = "dice"
            ),
            checkboxInput(
              "th_tech_retry",
              div(
                "Do you wish to retry if you fail to fund your target tech level?",
                class = "inputs"
              ),
              width = "100%"
            ),
            conditionalPanel(condition = "input.th_tech_retry==1", div(div(
              class = "dice",
              numericInput(
                "th_tech_die_roll2",
                "Second R&D die roll",
                min = 1,
                max = 6,
                value = 3
              )
            ), br())),
            div(class = "notice", tags$strong(textOutput("th_tech_level")))
          )
        ),
        
        # rd3: readiness
        
        br(),
        banner_function("(3a) Select the readiness events"),
        br(),
        
        fluidRow(column(
          12,
          selectInput(
            "th_readiness_event",
            "Select the readiness event",
            choices = c(
              "None",
              "Public Opinion" = "read1",
              "Training Environment" = "read2",
              "Health and Quality of Life" = "read3",
              "Infrastructure" = "read4",
              "Supply Chain" = "read5",
              "Corruption and Scandals" = "read6"
            )
          ),
        )),
        
        # rd3: manual selections
        
        fluidRow(
          column(
            4,
            conditionalPanel(
              condition = "input.dice == 'manual'",
              class = "dice",
              conditionalPanel(
                condition = "input.th_readiness_event == 'read1'",
                numericInput(
                  "th_read1_die_roll",
                  "Enter MILPERS effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_readiness_event == 'read2'",
                numericInput(
                  "th_read2_die_roll",
                  "Enter O&M effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_readiness_event == 'read3'",
                numericInput(
                  "th_read3_die_roll",
                  "Enter MILPERS and MILCON effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_readiness_event == 'read4'",
                numericInput(
                  "th_read4_die_roll",
                  "Enter MILCON effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_readiness_event == 'read5'",
                numericInput(
                  "th_read5_die_roll",
                  "Enter procurement and O&M effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_readiness_event == 'read6'",
                numericInput(
                  "th_read6_die_roll1",
                  "Enter type roll",
                  value = 1,
                  min = 1,
                  max = 6
                ),
                numericInput(
                  "th_read6_die_roll2",
                  "Enter effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              )
            ),
            
            # rd3: readiness1 computer-generated rolls
            
            conditionalPanel(
              condition = "input.dice == 'computer'",
              conditionalPanel(
                condition = "input.th_readiness_event == 'read1'",
                actionButton(
                  "th_read1_button",
                  "Roll for MILPERS effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read1_die_roll")
              ),
              conditionalPanel(
                condition = "input.th_readiness_event == 'read2'",
                actionButton(
                  "th_read2_button",
                  "Roll for O&M effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read2_die_roll")
              ),
              conditionalPanel(
                condition = "input.th_readiness_event == 'read3'",
                actionButton(
                  "th_read3_button",
                  "Roll for MILPERS and MILCON effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read3_die_roll")
              ),
              conditionalPanel(
                condition = "input.th_readiness_event == 'read4'",
                actionButton(
                  "th_read4_button",
                  "Roll for MILCON effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read4_die_roll")
              ),
              conditionalPanel(
                condition = "input.th_readiness_event == 'read5'",
                actionButton(
                  "th_read5_button",
                  "Roll for procurement and O&M effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read5_die_roll")
              ),
              conditionalPanel(
                condition = "input.th_readiness_event == 'read6'",
                actionButton("th_read6_button1", "Roll for type", icon("dice", "fa-shake")),
                actionButton("th_read6_button2", "Roll for effect", icon("dice", "fa-shake")),
                textOutput("th_read6_die_roll1"),
                textOutput("th_read6_die_roll2")
              )
            ),
          ),
          conditionalPanel(condition = "input.th_readiness_event == 'read1'", column(
            4, img(src = "readiness1a.png"), img(src = "readiness1b.png")
          )),
          conditionalPanel(condition = "input.th_readiness_event == 'read2'", column(
            4, img(src = "readiness2a.png"), img(src = "readiness2b.png")
          )),
          conditionalPanel(condition = "input.th_readiness_event == 'read3'", column(
            4, img(src = "readiness3a.png"), img(src = "readiness3b.png")
          )),
          conditionalPanel(condition = "input.th_readiness_event == 'read4'", column(
            4, img(src = "readiness4a.png"), img(src = "readiness4b.png")
          )),
          conditionalPanel(condition = "input.th_readiness_event == 'read5'", column(
            4, img(src = "readiness5a.png"), img(src = "readiness5b.png")
          )),
          conditionalPanel(condition = "input.th_readiness_event == 'read6'", column(4, img(src =
                                                                                              "readiness6.png"))),
          
          conditionalPanel(condition = "!(input.th_readiness_event == 'None')", column(3, class = "notice", strong(
            textOutput("th_readiness_impact")
          )))
        ),
        # rd3: end readiness 1 row
        
        tags$hr(style = "border-color: black; border-style: dotted;"),
        
        # rd3: readiness event #2
        
        fluidRow(column(
          12,
          selectInput(
            "th_readiness_event2",
            "Select the second readiness event",
            choices = c(
              "None",
              "Public Opinion" = "read1",
              "Training Environment" = "read2",
              "Health and Quality of Life" = "read3",
              "Infrastructure" = "read4",
              "Supply Chain" = "read5",
              "Corruption and Scandals" = "read6"
            )
          ),
        )),
        
        # rd3: manual selections
        
        fluidRow(
          column(
            4,
            conditionalPanel(
              condition = "input.dice == 'manual'",
              class = "dice",
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read1'",
                numericInput(
                  "th_read2_1_die_roll",
                  "Enter MILPERS effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read2'",
                numericInput(
                  "th_read2_2_die_roll",
                  "Enter O&M effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read3'",
                numericInput(
                  "th_read2_3_die_roll",
                  "Enter MILPERS and MILCON effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read4'",
                numericInput(
                  "th_read2_4_die_roll",
                  "Enter MILCON effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read5'",
                numericInput(
                  "th_read2_5_die_roll",
                  "Enter procurement and O&M effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              ),
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read6'",
                numericInput(
                  "th_read2_6_die_roll1",
                  "Enter type roll",
                  value = 1,
                  min = 1,
                  max = 6
                ),
                numericInput(
                  "th_read2_6_die_roll2",
                  "Enter effect roll",
                  value = 1,
                  min = 1,
                  max = 6
                )
              )
            ),
            
            # rd3: computer-generated rolls
            
            conditionalPanel(
              condition = "input.dice == 'computer'",
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read1'",
                actionButton(
                  "th_read2_1_button",
                  "Roll for MILPERS effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read2_1_die_roll")
              ),
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read2'",
                actionButton(
                  "th_read2_2_button",
                  "Roll for O&M effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read2_2_die_roll")
              ),
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read3'",
                actionButton(
                  "th_read2_3_button",
                  "Roll for MILPERS and MILCON effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read2_3_die_roll")
              ),
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read4'",
                actionButton(
                  "th_read2_4_button",
                  "Roll for MILCON effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read2_4_die_roll")
              ),
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read5'",
                actionButton(
                  "th_read2_5_button",
                  "Roll for procurement and O&M effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read2_5_die_roll")
              ),
              conditionalPanel(
                condition = "input.th_readiness_event2 == 'read6'",
                actionButton("th_read2_6_button1", "Roll for type", icon("dice", "fa-shake")),
                actionButton(
                  "th_read2_6_button2",
                  "Roll for effect",
                  icon("dice", "fa-shake")
                ),
                textOutput("th_read2_6_die_roll1"),
                textOutput("th_read2_6_die_roll2")
              )
            ),
          ),
          conditionalPanel(condition = "input.th_readiness_event2 == 'read1'", column(
            4, img(src = "readiness1a.png"), img(src = "readiness1b.png")
          )),
          conditionalPanel(condition = "input.th_readiness_event2 == 'read2'", column(
            4, img(src = "readiness2a.png"), img(src = "readiness2b.png")
          )),
          conditionalPanel(condition = "input.th_readiness_event2 == 'read3'", column(
            4, img(src = "readiness3a.png"), img(src = "readiness3b.png")
          )),
          conditionalPanel(condition = "input.th_readiness_event2 == 'read4'", column(
            4, img(src = "readiness4a.png"), img(src = "readiness4b.png")
          )),
          conditionalPanel(condition = "input.th_readiness_event2 == 'read5'", column(
            4, img(src = "readiness5a.png"), img(src = "readiness5b.png")
          )),
          conditionalPanel(condition = "input.th_readiness_event2 == 'read6'", column(4, img(src =
                                                                                               "readiness6.png"))),
          
          conditionalPanel(condition = "!(input.th_readiness_event2 == 'None')", column(3, class = "notice", strong(
            textOutput("th_readiness_impact2")
          )))
        ),
        
        # rd3: MILCON, MILPERS, O&M, procurement funding
        
        br(),
        banner_function(
          "(3b) Set your readiness funding levels",
          "circle-info",
          readiness_funding_text
        ),
        br(),
        
        fluidRow(
          column(
            5,
            br(),
            sliderInput(
              "th_milcon_fund",
              "MILCON (Military Construction) % Funded",
              min = 0,
              max = 100,
              value = 100
            ),
            div(class = "notice", textOutput("th_milcon_min")),
            br(),
            div(
              numericInput(
                "th_milcon_roll",
                "MILCON dice roll",
                min = 2,
                max = 12,
                value = 6
              ),
              class = "dice"
            ),
            tags$hr(style = "border-color: black; border-style: dotted;"),
            sliderInput(
              "th_milpers_fund",
              "MILPERS (Military Pesonnel) % Funded",
              min = 0,
              max = 100,
              value = 100
            ),
            div(class = "notice", textOutput("th_milpers_min")),
            br(),
            div(
              numericInput(
                "th_milpers_roll",
                "MILPERS dice roll",
                min = 2,
                max = 12,
                value = 6
              ),
              class = "dice"
            )
          ),
          column(
            5,
            offset = 1,
            br(),
            sliderInput(
              "th_om_fund",
              "O&M (Operations and Maintenance) % Funded",
              min = 0,
              max = 100,
              value = 100
            ),
            div(class = "notice", textOutput("th_om_min")),
            br(),
            div(
              numericInput(
                "th_om_roll",
                "O&M dice roll",
                min = 2,
                max = 12,
                value = 6
              ),
              class = "dice"
            ),
            tags$hr(style = "border-color: black; border-style: dotted;"),
            sliderInput(
              "th_proc_fund",
              "Procurement (other than new forces) % Funded",
              min = 0,
              max = 100,
              value = 100
            ),
            div(class = "notice", textOutput("th_proc_min")),
            br(),
            div(
              numericInput(
                "th_proc_roll",
                "Procurement dice roll",
                min = 2,
                max = 12,
                value = 6
              ),
              class = "dice"
            )
          )
        ),
        
        br(),
        
        fluidRow(column(
          4,
          offset = 4,
          align = "center",
          div(
            class = "notice",
            strong(
              "Overall Readiness % Funded",
              make_info_box(
                "circle-info",
                "This is the readiness factor for the combat power calculation"
              ),
              ":",
            ),
            textOutput("th_overall_readiness")
          )
        )),
        
        # rd3: forces information table: costs and capabilities
        
        br(),
        banner_function(
          "(4a) Info: Force Capabilities Table",
          "circle-info",
          "Table will update to include any selected upgrades"
        ),
        br(),
        
        fluidRow(
          column(
            7,
            offset = 2,
            style = "background-color: #def0de; border-style: solid;",
            br(),
            "Readiness Cost: Total (MILCON / MILPERS / O&M / Procurement)",
            br(),
            "Artillery provides + 1 offense and Aviation provides + 1 defense if either is paired with Armor, Infantry, or Stryker",
            br(),
            "Each Enabler force meets 10 Enabler Requirements",
            tableOutput("th_readiness_info")
          )
        ),
        
        br(),
        banner_function(
          "(4b) Select your forces and upgrades",
          "circle-info",
          select_forces_text
        ),
        br(),
        
        fluidRow(
          br(),
          column(3, div(
            style = "text-align:center;", tags$strong("Force structure from round 2")
          )),
          column(
            3,
            div(
              style = "text-align:center; border-style: dashed; background-color: #f2f2f2;",
              tags$strong("Units to disband"),
              make_info_box("circle-info", "Receive 50% of readiness costs back")
            )
          ),
          column(
            3,
            div(
              style = "text-align:center; border-style: solid;",
              tags$strong("New units and upgrades"),
              make_info_box(
                "circle-info",
                "Past upgrades remain; new upgrades apply to all units selected for the round"
              )
            )
          ),
          column(3, div(
            style = "text-align:center;", tags$strong("Updated round 3 force structure")
          ))
        ),
        
        fluidRow(
          column(3, tableOutput("th_forces_selected2")),
          column(
            6,
            br(),
            fluidRow(
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_inf_AC_disband",
                  "INF AC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_inf_RC_disband",
                  "INF RC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_inf_AC",
                  "New INF AC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_inf_RC",
                  "New INF RC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(2, checkboxGroupInput(
                "th_inf_upgrade",
                NULL,
                choices = list(
                  "+2 Off" = 1,
                  "+2 Def" = 2,
                  "-2 Enabler" = 3
                )
              ))
            ),
            fluidRow(
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_arm_AC_disband",
                  "ARM AC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_arm_RC_disband",
                  "ARM RC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_arm_AC",
                  "New ARM AC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_arm_RC",
                  "New ARM RC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(2, checkboxGroupInput(
                "th_arm_upgrade",
                NULL,
                choices = list(
                  "+2 Off" = 1,
                  "+2 Def" = 2,
                  "-2 Enabler" = 3
                )
              ))
            ),
            fluidRow(
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_str_AC_disband",
                  "STR AC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_str_RC_disband",
                  "STR RC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_str_AC",
                  "New STR AC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_str_RC",
                  "New STR RC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(2, checkboxGroupInput(
                "th_str_upgrade",
                NULL,
                choices = list(
                  "+2 Off" = 1,
                  "+2 Def" = 2,
                  "-2 Enabler" = 3
                )
              ))
            ),
            fluidRow(
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_avi_AC_disband",
                  "AVI AC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_avi_RC_disband",
                  "AVI RC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_avi_AC",
                  "New AVI AC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_avi_RC",
                  "New AVI RC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(2, checkboxGroupInput(
                "th_avi_upgrade",
                NULL,
                choices = list(
                  "+2 Off" = 1,
                  "+2 Def" = 2,
                  "-2 Enabler" = 3
                )
              ))
            ),
            fluidRow(
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_art_AC_disband",
                  "ART AC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_art_RC_disband",
                  "ART RC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_art_AC",
                  "New ART AC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_art_RC",
                  "New ART RC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(2, checkboxGroupInput(
                "th_art_upgrade",
                NULL,
                choices = list(
                  "+2 Off" = 1,
                  "+2 Def" = 2,
                  "-2 Enabler" = 3
                )
              ))
            ),
            fluidRow(
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_so_AC_disband",
                  "SO AC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_so_RC_disband",
                  "SO RC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_so_AC",
                  "New SO AC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_so_RC",
                  "New SO RC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(2, checkboxGroupInput(
                "th_so_upgrade",
                NULL,
                choices = list(
                  "+2 Off" = 1,
                  "+2 Def" = 2,
                  "-2 Enabler" = 3
                )
              ))
            ),
            fluidRow(
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_ad_AC_disband",
                  "AD AC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_ad_RC_disband",
                  "AD RC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_ad_AC",
                  "New AD AC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_ad_RC",
                  "New AD RC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(2, checkboxGroupInput(
                "th_ad_upgrade",
                NULL,
                choices = list(
                  "+2 Off" = 1,
                  "+2 Def" = 2,
                  "-2 Enabler" = 3
                )
              ))
            ),
            fluidRow(
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_ena_AC_disband",
                  "ENA AC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                3,
                style = "background-color: #f2f2f2;",
                numericInput(
                  "th_ena_RC_disband",
                  "ENA RC disband",
                  min = 0,
                  max = 100,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_ena_AC",
                  "New ENA AC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              ),
              column(
                2,
                numericInput(
                  "th_ena_RC",
                  "New ENA RC",
                  min = 0,
                  max = 1000,
                  value = 0,
                  width = "75px"
                )
              )
            )
          ),
          column(3, tableOutput("forces_selected_rd3"))
        ),
        
        tags$hr(style = "border-color: black; border-style: dotted;"),
        
        # rd3: pie charts for force structure and AC/RC
        
        fluidRow(column(
          12, align = "center", column(6, plotlyOutput("th_force_pie")), column(6, plotlyOutput("th_ACRC_pie"))
        )),
        
        # rd3: reserve component and upgrades dice rolls
        
        br(),
        banner_function("(5) Reserve component and upgrades dice rolls"),
        br(),
        
        fluidRow(
          column(
            4,
            offset = 1,
            div(
              numericInput(
                "th_RC_factor_die_roll",
                "RC capability dice roll",
                value = 12,
                min = 2,
                max = 12
              ),
              class = "dice"
            ),
            br(),
            div(class = "notice", textOutput("th_RC_factor2"))
          ),
          column(
            5,
            offset = 1,
            div(class = "notice", textOutput("th_upgrades_min")),
            conditionalPanel(condition = "input.th_political_event_1 == 'pol4'", br(), div(
              class = "notice", textOutput("th_upgrade_reminder")
            )),
            br(),
            div(
              numericInput(
                "th_upgrades_die_roll",
                "Force upgrades dice roll",
                min = 2,
                max = 12,
                value = 12
              ),
              class = "dice"
            ),
            htmlOutput("th_overrun"),
            br(),
            actionButton(
              "th_upgrades_cancel",
              "Cancel all upgrades (receive 50% of costs back)",
              icon("arrows-rotate", "fa-spin")
            ),
            make_info_box("circle-info", upgrade_cancel_text)
          )
        ),
        
        # rd3: phase 2: actual capabilities and crisis outcomes
        
        br(),
        banner_function("(6) Crisis and crisis outcomes"),
        br(),
        
        # rd3: R&D computer rolls
        
        fluidRow(
          conditionalPanel(
            condition = "input.dice == 'computer'",
            br(),
            actionButton("th_rd_button1", "Roll for R&D", icon("dice", "fa-shake")),
            textOutput("th_rd_die_roll1"),
            conditionalPanel(
              condition = "input.th_tech_retry == 1",
              actionButton("th_rd_button2", "Roll for R&D retry", icon("dice", "fa-shake")),
              textOutput("th_rd_die_roll2")
            ),
          ),
        ),
        
        # rd3: readiness computer rolls
        
        fluidRow(
          conditionalPanel(
            condition = "input.dice == 'computer'",
            br(),
            actionButton(
              "th_read_button",
              "Roll for readiness event",
              icon("dice", "fa-shake")
            ),
            textOutput("th_readiness_die_roll"),
            actionButton(
              "th_read_button2",
              "Roll for second readiness event",
              icon("dice", "fa-shake")
            ),
            textOutput("th_readiness2_die_roll")
          ),
        ),
        
        fluidRow(
          conditionalPanel(
            condition = "input.dice == 'computer'",
            br(),
            actionButton("th_milcon_button", "Roll for MILCON", icon("dice", "fa-shake")),
            textOutput("th_milcon_die_roll")
          ),
          conditionalPanel(
            condition = "input.dice == 'computer'",
            br(),
            actionButton(
              "th_milpers_button",
              "Roll for MILPERS",
              icon("dice", "fa-shake")
            ),
            textOutput("th_milpers_die_roll")
          ),
          conditionalPanel(
            condition = "input.dice == 'computer'",
            br(),
            actionButton("th_om_button", "Roll for O&M", icon("dice", "fa-shake")),
            textOutput("th_om_die_roll")
          ),
          conditionalPanel(
            condition = "input.dice == 'computer'",
            br(),
            actionButton(
              "th_proc_button",
              "Roll for procurement",
              icon("dice", "fa-shake")
            ),
            textOutput("th_proc_die_roll")
          ),
        ),
        
        # rd3: force upgrades computer roll
        
        fluidRow(
          conditionalPanel(
            condition = "input.dice == 'computer'",
            br(),
            actionButton(
              "th_upgrades_button",
              "Roll for force upgrades",
              icon("dice", "fa-shake")
            ),
            textOutput("th_upgrades_die_roll")
          )
        ),
        
        # rd3: RC capability factor computer roll
        
        fluidRow(
          conditionalPanel(
            condition = "input.dice == 'computer'",
            br(),
            actionButton(
              "th_RC_factor",
              "Roll for RC capability factor",
              icon("dice", "fa-shake")
            ),
            textOutput("th_RC_die_roll")
          )
        ),
        
        # rd3: crisis determination computer roll
        
        fluidRow(
          conditionalPanel(
            condition = "input.dice == 'computer'",
            br(),
            actionButton(
              "th_crisis_button",
              "Roll to determine your crisis",
              icon("dice", "fa-shake")
            ),
            textOutput("th_crisis_die_roll")
          )
        ),
        
        # rd3: crisis determination
        
        fluidRow(
          column(
            4,
            offset = 1,
            selectInput(
              "th_crisis",
              "Select the crisis",
              choices = c(
                "None",
                "Steady State" = "steady",
                "Humanitarian Crisis" = "human",
                "Rogue State" = "rogue",
                "ISIS 2.0" = "isis",
                "Defensive Campaign (vs Peer Competitor)" = "defense",
                "Offensive Campaign (vs Peer Competitor)" = "offense"
              )
            ),
            br(),
            div(
              align = "center",
              conditionalPanel(condition = "input.th_crisis == 'steady'", img(src =
                                                                                "crisis1.png")),
              conditionalPanel(condition = "input.th_crisis == 'human'", img(src =
                                                                               "crisis2.png")),
              conditionalPanel(condition = "input.th_crisis == 'rogue'", img(src =
                                                                               "crisis3.png")),
              conditionalPanel(condition = "input.th_crisis == 'isis'", img(src =
                                                                              "crisis4.png")),
              conditionalPanel(condition = "input.th_crisis == 'defense'", img(src =
                                                                                 "crisis5.png")),
              conditionalPanel(condition = "input.th_crisis == 'offense'", img(src =
                                                                                 "crisis6.png"))
            )
          ),
          column(5, offset = 1, tableOutput("th_crisis_reqs"))
        ),
        
        fluidRow(
          column(4, offset = 1, div(
            class = "notice", textOutput("th_min_crisis_roll")
          )),
          column(
            5,
            offset = 1,
            conditionalPanel(condition = "input.th_political_event_1 == 'pol6'", div(div(
              class = "notice", textOutput("th_crisis_reminder")
            ), br())),
            div(
              numericInput(
                "th_crisis_outcome_die_roll",
                "Crisis outcome dice roll",
                value = 12,
                min = 2,
                max = 12
              ),
              class = "dice"
            ),
            conditionalPanel(
              condition = "input.dice == 'computer'",
              actionButton(
                "th_crisis_outcome_button",
                "Roll to determine your crisis outcome",
                icon("dice", "fa-shake")
              ),
              textOutput("th_crisis_outcome_die_roll")
            )
          )
        ),
        
        # rd3: end of round losses
        
        br(),
        banner_function("(7) End of the game: final round losses"),
        br(),
        
        fluidRow(column(
          4,
          offset = 4,
          align = "center",
          div(class = "notice", textOutput("th_force_losses"))
        )),
        
        br(),
        
        # final download and save
        
        fluidRow(column(
          6,
          offset = 3,
          align = "center",
          uiOutput("th_end_round1"),
        )),
        tags$hr(style = "border-color: black; border-style: dotted;"),
        fluidRow(column(
          6,
          offset = 3,
          align = "center",
          htmlOutput("th_end_round1_2")
        )),
        
        br(),
        
        # end of game failure points by round and total and tie breakers
        
        fluidRow(column(
          5, offset = 2, uiOutput("failure_table_with_text")
        ), column(
          3, uiOutput("tie_breakers_table_with_text")
        )),
        
        br(),
        
        fluidRow(column(
          12, div(class = "unclass_bottom", "(U) Unclassified")
        ))
        
      ),
      # end main panel
      
    )),
    # end round 3 tab
    
    # Crisis tabs
    
    
    #navbar
    navbarMenu(
      "Events",
      tabPanel(
        "Political Events",
        br(),
        htmlOutput("political"),
        fluidRow(column(
          12, div(class = "unclass_bottom", "(U) Unclassified")
        ))
      ),
      tabPanel(
        "Readiness Events",
        br(),
        htmlOutput("readiness"),
        fluidRow(column(
          12, div(class = "unclass_bottom", "(U) Unclassified")
        ))
      ),
      tabPanel("Crises", br(), htmlOutput("crisis"), fluidRow(column(
        12, div(class = "unclass_bottom", "(U) Unclassified")
      )))
    ),
    
    # Game analytics
    
    tabPanel(
      "Class Data",
      
      br(),
      
      fluidRow(column(12, uiOutput("db_button"))),
      
      br(),
      
      # select class data to view
      
      fluidRow(column(12, uiOutput("class_filter"))),
      
      # graphs
      
      fluidRow(column(12, uiOutput("graph_filters"))),
      
      fluidRow(column(12, plotlyOutput("class_graphs"))),
      
      # failure points scatterplots
      
      fluidRow(column(12, uiOutput("scatter_filters"))),
      
      fluidRow(column(12, plotlyOutput("scatter_graphs"))),
      
      # tables
      
      fluidRow(column(12, uiOutput("data_filters"))),
      
      fluidRow(column(12, dataTableOutput("class_data"))),
      
      fluidRow(column(
        12, div(class = "unclass_bottom", "(U) Unclassified")
      ))
      
    ) # ends game analytics tab
  ) # ends navbarPage
)   # ends shiny ui