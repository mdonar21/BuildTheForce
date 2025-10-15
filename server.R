library(DT)
library(plotly)
library(tidyverse)
library(shiny)
library(RMySQL)

# free SQL database credentials

db_user <- "sql5741960"
db_name <- "sql5741960"
db_host <- "sql5.freesqldatabase.com"
db_password <- "bgIQUblvGT"

# hall of fame information

hof <- data.frame(
  Call_Sign = c(
    "Steve", "Druid", "Tara", "Bryan", "Hughbanks", "Jib", "Quit Me Man", "ITF 6", "Hands", "DontHassleTheHoff"
  ),
  Failure_Points = 0,
  Losses = c(3.3, 3.3, 3.3, 7.5, 7.5, 7.5, 7.5, 7.5, 7.5, 7.5)
)
colnames(hof) <- c("Call Sign", "Total Failure Points", "Total Losses")

# create initial tables used in game

# funding
  
rd_costs <- data.frame(c(0,0,0,0,0),c(150,100,80,50,20),c(225,150,120,75,30),c(300,200,160,100,40))
readiness_info_initial <- data.frame("Force Structure Cost" = c(12,14,13,15,10,17,9,8), "Readiness Cost" = c("7 (2/3/1/1)","10 (3/3/2/2)", "9 (2/3/2/2)", "11 (3/2/3/3)", "7 (1/2/2/2)", "6 (1/1/2/2)", "8 (2/1/2/3)", "5 (1/1/1/2)"), "Offense" = c(4,9,7,6,5,10,1,0), "Defense" = c(6,5,5,4,2,3,8,0), "Enabler Requirement" = c(5,10,6,8,5,3,4,0), check.names=F, row.names = c("Infantry","Armor","Stryker","Aviation","Artillery","Special Operations","Air Defense","Enablers"))
forces_cost_table <- matrix(c(12,14,13,15,10,17,9,8,2,3,2,3,1,1,2,1,3,3,3,2,2,1,1,1,1,2,2,3,2,2,2,1,1,2,2,3,2,2,3,2), nrow=8, dimnames = list(c("Infantry","Armor","Stryker","Aviation","Artillery","Special Operations","Air Defense","Enablers"),c("Force Structure","MILCON","MILPERS","O&M","Procurement")))
upgrade_min_roll <- data.frame("1" = c(7,9,10,11), "2" = c(6,8,9,10), "3" = c(5,6,7,8), "4" = c(4,5,6,7), check.names=F)
crisis_reqs <- data.frame("defense" = c(120,20,10,2,3,150,50,20,3,5,200,100,30,4,7,"Stryker, Air Defense"), "human" = c(80,20,100,2,3,50,20,50,3,5,120,20,150,3,6,"Aviation, Infantry"), "isis" = c(20,80,20,3,4,40,100,50,4,6,80,100,100,4,7,"Aviation, Special Operations"), "offense" = c(20,120,10,2,3,50,200,50,3,5,50,150,80,4,7,"Armor, Artillery"), "rogue" = c(80,40,20,2,3,100,60,40,4,6,50,60,80,4,7,"Air Defense, Special Operations"), "steady" = c(20,20,10,1,2,20,20,10,2,4,20,20,10,3,6,"Air Defense, Infantry"))

# other variables used in game
  
colors <- rainbow(8)
  

shinyServer(function(input, output, session) {
  
  output$hof_image <- renderUI({
    div(style = "vertical-align: middle;", 
      br(),
      img(src="hof.jpg", height="250", width="292")
    )
  })
  
  output$hof <- renderTable({
    hof
  }, striped = TRUE, bordered = T)
  
  #showModal(modalDialog(
    #title = "Consent to Monitoring",
    #"You are accessing a U.S. Government (USG) Information System (IS) that is provided for USG-authorized use only.", p(),
    #"By using this IS (which includes any device attached to this IS), you consent to the following conditions:", p(),
    #"The USG routinely intercepts and monitors communications on this IS for purposes including, but not limited to, penetration testing, COMSEC monitoring, network operations and defense, personnel misconduct (PM), law enforcement (LE), and counterintelligence (CI) investigations.", p(),
    #"At any time, the USG may inspect and seize data stored on this IS.", p(),
    #"Communications using, or data stored on, this IS are not private, are subject to routine monitoring, interception, and search, and may be disclosed or used for any USG authorized purpose.", p(),
    #"This IS includes security measures (e.g., authentication and access controls) to protect USG interests - not for your personal benefit or privacy.", p(),
    #"Notwithstanding the above, using this IS does not constitute consent to PM, LE or CI investigative searching or monitoring of the content of privileged communications, or work product, related to personal representation or services by attorneys, psychotherapists, or clergy, and their assistants. Such communications and work product are private and confidential.",
    #easyClose = F,
    #footer = actionButton("close", "I accept")
  #))
  #observeEvent(input$close, {removeModal()})
  
  # round parameters
  
  round1 <- reactive({if (input$tab == "round1"){1} else {0} })
  round2 <- reactive({if (input$tab == "round2"){1} else {0} })
  round3 <- reactive({if (input$tab == "round3"){1} else {0} })
  
  # funding reactive variables
  
  allocated <- reactive({ 650*pol_fund1() })
  tw_allocated <- reactive({ allocated()*1.02*pol_fund2() })
  th_allocated <- reactive({ tw_allocated()*1.02*pol_fund3() })
  
  remaining <- reactive({ allocated() -
    sum(forces_cost2()[6,1:2]) -
    rd_round1() -
    total_upgrades_cost()*overrun() -
    upgrades_cancel()
    })
  
  tw_remaining <- reactive({ tw_allocated() -
    sum(tw_forces_cost2()[6,1:2]) -  # incorporates round 1 and 2 unit costs less disbands
    rd_round2() -
    tw_total_upgrades_cost()*tw_overrun() - tw_upgrades_cancel()
  })
  
  th_remaining <- reactive({ th_allocated() -
    sum(th_forces_cost2()[6,1:2]) - 
    rd_round3() - 
    th_total_upgrades_cost()*th_overrun() - th_upgrades_cancel()
  })
  
  rd_round1 <- reactive({rd_cost()*rd_exp() + rd_retry_costs()})
  rd_round2 <- reactive({tw_rd_cost()*tw_rd_exp() + tw_rd_retry_costs()})
  rd_round3 <- reactive({th_rd_cost()*th_rd_exp() + th_rd_retry_costs()})
  
  output$allocated <- renderText({format(allocated(), digits=4)})
  output$tw_allocated <- renderText({format(tw_allocated(), digits=4)})
  output$th_allocated <- renderText({format(th_allocated(), digits=4)})
  
  output$remaining <- renderText({format(remaining(), digits=4)})
  output$tw_remaining <- renderText({format(tw_remaining(), digits=4)})
  output$th_remaining <- renderText({format(th_remaining(), digits=4)})

  # setting seed for computer-generated die rolls
  
  observeEvent(input$seed, {
    req(input$seed)
    if(is.numeric(input$seed) == T & input$seed < 2147483647) {
      set.seed(input$seed)
    } else {set.seed(1)}
    die_rolls <<- rbind(floor(runif(20,1,6.99999)),floor(runif(20,2,12.99999)),floor(runif(20,1,6.99999)),floor(runif(20,2,12.99999)),floor(runif(20,1,6.99999)),floor(runif(20,2,12.99999)))
  })
  
  # political computer rolls
  
  observeEvent(input$pol_button,{output$pol_die_roll <- renderText({paste("You rolled a ", die_rolls[2,1])})  })
  observeEvent(input$tw_pol_button,{output$tw_pol_die_roll <- renderText({paste("You rolled a ", die_rolls[4,1])})  })
  observeEvent(input$th_pol_button,{output$th_pol_die_roll <- renderText({paste("You rolled a ", die_rolls[6,1])})  })
  
  observeEvent(input$pol1_button1,{output$pol1_die_roll1 <- renderText({paste("You rolled a ", die_rolls[2,2])})  })
  observeEvent(input$tw_pol1_button1,{output$tw_pol1_die_roll1 <- renderText({paste("You rolled a ", die_rolls[4,2])})  })
  observeEvent(input$th_pol1_button1,{output$th_pol1_die_roll1 <- renderText({paste("You rolled a ", die_rolls[6,2])})  })
  
  observeEvent(input$pol1_button2,{output$pol1_die_roll2 <- renderText({paste("You rolled a ", die_rolls[1,1])})  })
  observeEvent(input$tw_pol1_button2,{output$tw_pol1_die_roll2 <- renderText({paste("You rolled a ", die_rolls[3,1])})  })
  observeEvent(input$th_pol1_button2,{output$th_pol1_die_roll2 <- renderText({paste("You rolled a ", die_rolls[6,13])})  })
  
  observeEvent(input$pol4_button1,{output$pol4_die_roll1 <- renderText({paste("You rolled a ", die_rolls[1,2])})  })
  observeEvent(input$tw_pol4_button1,{output$tw_pol4_die_roll1 <- renderText({paste("You rolled a ", die_rolls[3,2])})  })
  observeEvent(input$th_pol4_button1,{output$th_pol4_die_roll1 <- renderText({paste("You rolled a ", die_rolls[5,2])})  })
  
  observeEvent(input$pol5_button1,{output$pol5_die_roll1 <- renderText({paste("You rolled a ", die_rolls[2,3])})  })
  observeEvent(input$tw_pol5_button1,{output$tw_pol5_die_roll1 <- renderText({paste("You rolled a ", die_rolls[4,3])})  })
  observeEvent(input$th_pol5_button1,{output$th_pol5_die_roll1 <- renderText({paste("You rolled a ", die_rolls[6,3])})  })
  
  observeEvent(input$pol6_button1,{output$pol6_die_roll1 <- renderText({paste("You rolled a ", die_rolls[1,3])})  })
  observeEvent(input$tw_pol6_button1,{output$tw_pol6_die_roll1 <- renderText({paste("You rolled a ", die_rolls[3,3])})  })
  observeEvent(input$th_pol6_button1,{output$th_pol6_die_roll1 <- renderText({paste("You rolled a ", die_rolls[5,3])})  })
 
  
  # political events 2 and 3: friendly/unfriendly administration funding adjustments
  
  pol_fund1 <- reactive({ pol_fund(input$political_event_1) })
  pol_fund2 <- reactive({ pol_fund(input$tw_political_event_1) })
  pol_fund3 <- reactive({ pol_fund(input$th_political_event_1) })
  
  
  pol_fund <- function(event){
    if (event == "pol2"){
      0.9
    } else if (event == "pol3"){
      1.1
    } else {1}
  }
  
  # impact and error checks for political events 1, 4-6

  output$pol_event <- renderText({
    pol_event(input$political_event_1, input$pol1_die_roll1, input$pol1_die_roll2, input$pol1_button1, input$pol1_button2, input$pol4_die_roll1, input$pol4_button1, input$priority_crisis, input$pol6_die_roll1, input$pol6_button1)
  })
  output$tw_pol_event <- renderText({
    pol_event(input$tw_political_event_1, input$tw_pol1_die_roll1, input$tw_pol1_die_roll2, input$tw_pol1_button1, input$tw_pol1_button2, input$tw_pol4_die_roll1, input$tw_pol4_button1, input$tw_priority_crisis, input$tw_pol6_die_roll1, input$tw_pol6_button1)
  })
  output$th_pol_event <- renderText({
    pol_event(input$th_political_event_1, input$th_pol1_die_roll1, input$th_pol1_die_roll2, input$th_pol1_button1, input$th_pol1_button2, input$th_pol4_die_roll1, input$th_pol4_button1, input$th_priority_crisis, input$th_pol6_die_roll1, input$th_pol6_button1)
  })

  pol1_error1 <- reactiveVal(0)	# for political event 1, is the current # of the required forces selected
  pol1_error2 <- reactiveVal(0)	# for political event 1, is the required quantity of forces
  
  pol5_error1 <- reactiveVal(0)	# for political event 5, is the current # of one of the priority forces
  pol5_error2 <- reactiveVal(0)	# for political event 5, is the current # of second of the priority forces
  pol5_error3 <- reactiveVal(0)	# for political event 5, is the priority force requirements
  
  pol_event <- function (event, p1dr1, p1dr2, p1b1, p1b2, p4dr1, p4b1, pc, p6dr1, p6b1){
    if (event == "pol1"){
      if (input$dice == "manual"){
        temp1 <- p1dr1
        temp2 <- p1dr2
        pol1_error2(p1dr2)
      } else if (p1b1 > 0 & p1b2 > 0){
        temp1 <- die_rolls[2,2]*round1()+die_rolls[4,2]*round2()+die_rolls[6,2]*round3()
        temp2 <- die_rolls[1,1]*round1()+die_rolls[3,1]*round2()+die_rolls[6,13]*round3()
        pol1_error2(die_rolls[1,1]*round1()+die_rolls[3,1]*round2()+die_rolls[6,13]*round3())
      } else {
        temp1 <- 1
        temp2 <- 1
      }
      if (temp1 == 5){
        temp3 <- "Infantry"
        pol1_error1(sum(forces_selected$d1[1,1:2])*round1()+sum(forces_selected$d2[1,1:2])*round2()+sum(forces_selected$d3[1,1:2])*round3() )
      } else if (temp1 == 2 | temp1 == 6){
        temp3 <- "Armor"
        pol1_error1(sum(forces_selected$d1[2,1:2])*round1()+sum(forces_selected$d2[2,1:2])*round2()+sum(forces_selected$d3[2,1:2])*round3() )
      } else if (temp1 == 3 | temp1 == 4){
        temp3 <- "Stryker"
        pol1_error1(sum(forces_selected$d1[3,1:2])*round1()+sum(forces_selected$d2[3,1:2])*round2()+sum(forces_selected$d3[3,1:2])*round3() )
      } else if (temp1 == 7){
        temp3 <- "Aviation"
        pol1_error1(sum(forces_selected$d1[4,1:2])*round1()+sum(forces_selected$d2[4,1:2])*round2()+sum(forces_selected$d3[4,1:2])*round3() )
      } else if (temp1 == 9){
        temp3 <- "Artillery"
        pol1_error1(sum(forces_selected$d1[5,1:2])*round1()+sum(forces_selected$d2[5,1:2])*round2()+sum(forces_selected$d3[5,1:2])*round3() )
      } else if (temp1 == 8){
        temp3 <- "Special Operations"
        pol1_error1(sum(forces_selected$d1[6,1:2])*round1()+sum(forces_selected$d2[6,1:2])*round2()+sum(forces_selected$d3[6,1:2])*round3() )
      } else if (temp1 == 10 | temp1 == 11){
        temp3 <- "Air Defense"
        pol1_error1(sum(forces_selected$d1[7,1:2])*round1()+sum(forces_selected$d2[7,1:2])*round2()+sum(forces_selected$d3[7,1:2])*round3() )
      } else if (temp1 == 12) {
        temp3 <- "Enablers"
        pol1_error1(sum(forces_selected$d1[8,1:2])*round1()+sum(forces_selected$d2[8,1:2])*round2()+sum(forces_selected$d3[8,1:2])*round3() )
      } else {
        temp2 <- NULL
        temp3 <- NULL
      }
      paste("You must procure at least ", temp2, " unit(s) of ", temp3)
    } else if (event == "pol4"){
      if (input$dice == "manual"){
        temp1 <- p4dr1
      } else if (p4b1 > 0){
        temp1 <- die_rolls[1,2]*round1()+die_rolls[3,2]*round2()+die_rolls[5,2]*round3()
      } else {temp1 <- 0
      }
      if (temp1 == 1 ){temp2 <- 3
      } else if (temp1 == 2 | temp1 == 3){temp2 <- 2
      } else if (temp1 == 4 | temp1 == 5){temp2 <- 1
      } else {temp2 <- 0
      }
      if (!(temp2==0) & !(temp1==6)){
        output$upgrade_reminder <- renderText({paste("Due to cyber attacks, remember to reduce upgrade roll by ", temp2)})
        output$tw_upgrade_reminder <- renderText({paste("Due to cyber attacks, remember to reduce upgrade roll by ", temp2)})
        output$th_upgrade_reminder <- renderText({paste("Due to cyber attacks, remember to reduce upgrade roll by ", temp2)})
        paste("Reduce upgrade roll by ", temp2, " this round")
      } else if (temp1==6){
        output$upgrade_reminder <- renderText({"Cyber attack had no impact on upgrades dice roll"})
        output$tw_upgrade_reminder <- renderText({"Cyber attack had no impact on upgrades dice roll"})
        output$th_upgrade_reminder <- renderText({"Cyber attack had no impact on upgrades dice roll"})
        paste("No reduction to upgrade rolls")
      }
    } else if (event =="pol5"){
      if (pc == "steady"){
        pol5_error1(sum(forces_selected$d1[7,1:2])*round1()+sum(forces_selected$d2[7,1:2])*round2()+sum(forces_selected$d3[7,1:2])*round3() )
        pol5_error2(sum(forces_selected$d1[1,1:2])*round1()+sum(forces_selected$d2[1,1:2])*round2()+sum(forces_selected$d3[1,1:2])*round3() )
        pol5_error3(4*round1()+6*round2()+8*round3())
        paste("You are required to have ", pol5_error3(), " of Air Defense and/or Infantry")
      } else if (pc == "human"){
        pol5_error1(sum(forces_selected$d1[4,1:2])*round1()+sum(forces_selected$d2[4,1:2])*round2()+sum(forces_selected$d3[4,1:2])*round3() )
        pol5_error2(sum(forces_selected$d1[1,1:2])*round1()+sum(forces_selected$d2[1,1:2])*round2()+sum(forces_selected$d3[1,1:2])*round3() )
        pol5_error3(5*round1()+7*round2()+8*round3())
        paste("You are required to have ", pol5_error3(), " of Aviation and/or Infantry")
      } else if (pc == "rogue"){
        pol5_error1(sum(forces_selected$d1[7,1:2])*round1()+sum(forces_selected$d2[7,1:2])*round2()+sum(forces_selected$d3[7,1:2])*round3() )
        pol5_error2(sum(forces_selected$d1[6,1:2])*round1()+sum(forces_selected$d2[6,1:2])*round2()+sum(forces_selected$d3[6,1:2])*round3() )
        pol5_error3(5*round1()+8*round2()+9*round3())
        paste("You are required to have ", pol5_error3(), " of Air Defense and/or Special Operations")
      } else if (pc == "isis"){
        pol5_error1(sum(forces_selected$d1[4,1:2])*round1()+sum(forces_selected$d2[4,1:2])*round2()+sum(forces_selected$d3[4,1:2])*round3() )
        pol5_error2(sum(forces_selected$d1[6,1:2])*round1()+sum(forces_selected$d2[6,1:2])*round2()+sum(forces_selected$d3[6,1:2])*round3() )
        pol5_error3(6*round1()+8*round2()+9*round3())
        paste("You are required to have ", pol5_error3(), " of Aviation and/or Special Operations")
      } else if (pc == "defense"){
        pol5_error1(sum(forces_selected$d1[3,1:2])*round1()+sum(forces_selected$d2[3,1:2])*round2()+sum(forces_selected$d3[3,1:2])*round3() )
        pol5_error2(sum(forces_selected$d1[7,1:2])*round1()+sum(forces_selected$d2[7,1:2])*round2()+sum(forces_selected$d3[7,1:2])*round3() )
        pol5_error3(5*round1()+7*round2()+9*round3())
        paste("You are required to have ", pol5_error3(), " of Stryker and/or Air Defense")
      } else if (pc == "offense"){
        pol5_error1(sum(forces_selected$d1[2,1:2])*round1()+sum(forces_selected$d2[2,1:2])*round2()+sum(forces_selected$d3[2,1:2])*round3() )
        pol5_error2(sum(forces_selected$d1[5,1:2])*round1()+sum(forces_selected$d2[5,1:2])*round2()+sum(forces_selected$d3[5,1:2])*round3() )
        pol5_error3(5*round1()+7*round2()+9*round3())
        paste("You are required to have ", pol5_error3(), " of Armor and/or Artillery")
      }
    } else if (event =="pol6"){
      if (input$dice == "manual"){
        temp1 <- p6dr1
      } else if (p6b1 > 0){
        temp1 <- die_rolls[1,3]*round1()+die_rolls[3,3]*round2()+die_rolls[5,3]*round3()
      } else {temp1 <- 0
      }
      if (temp1 > 4){
        output$crisis_reminder <- renderText({"Due to uprising, remember to increase crisis roll by 2"})
        output$tw_crisis_reminder <- renderText({"Due to uprising, remember to increase crisis roll by 2"})
        output$th_crisis_reminder <- renderText({"Due to uprising, remember to increase crisis roll by 2"})
        paste("Increase crisis outcome roll by 2")
      } else if (temp1 < 3 & temp1 > 0){
        output$crisis_reminder <- renderText({"Due to uprising, remember to decrease crisis roll by 2"})
        output$tw_crisis_reminder <- renderText({"Due to uprising, remember to decrease crisis roll by 2"})
        output$th_crisis_reminder <- renderText({"Due to uprising, remember to decrease crisis roll by 2"})
        paste("Decrease crisis outcome roll by 2")
      } else if (temp1 == 0){
        NULL
      } else {
        output$crisis_reminder <- renderText({"Uprising political event had no impact on the crisis dice roll"})
        output$tw_crisis_reminder <- renderText({"Uprising political event had no impact on the crisis dice roll"})
        output$th_crisis_reminder <- renderText({"Uprising political event had no impact on the crisis dice roll"})
        paste("No impact to crisis roll")}
    } else if (event =="pol2"){
      paste("Budget decreased by 10%")
    } else if (event =="pol3"){
      paste("Budget increased by 10%")
    }
  }
  
  # R&D 
  
  # RD: tech level slider
  
  output$tech_target <- renderUI({
    temp <- "tech_level"
    tech(1, temp)
  })
  
  output$tw_tech_target <- renderUI({
    temp <- "tw_tech_level"
    tech(tech_level(), temp)
  })
  
  output$th_tech_target <- renderUI({
    temp <- "th_tech_level"
    tech(tw_tech_level(), temp)
  })
  
  tech <- function(level, rd){
    if (level == 1){
      sliderInput(rd, "Select target tech level", min=1, max=3, value=1)
    } else if (level == 2){
      sliderInput(rd, "Select target tech level", min=2, max=4, value=2)
    } else if (level == 3){
      sliderInput(rd, "Select target tech level", min=3, max=4, value=3, step=1)
    } else if (level == 4){
      sliderInput(rd, "Select target tech level", min=4, max=4, value=4)
    }
  }
  
  # RD: expedited RD messages
  
  output$expedited <- renderUI({
    req(input$tech_level)
    if (input$tech_level == 3){
      tags$div(
        style = "background-color: yellow; color:black; border-color: black; border-style: dashed; padding: 10px;
          border-radius: 8px;",
        "You selected an expedited level of research and development (tech level increase of 2 steps); costs are doubled"  
      )
    }
  })
  
  output$tw_expedited <- renderUI({
    req(input$tw_tech_level)
    if ((tech_level() == 1 & input$tw_tech_level == 3) | (tech_level() == 2 & input$tw_tech_level == 4)){
      tags$div(
        style = "background-color: yellow; color:black; border-color: black; border-style: dashed; padding: 10px;
          border-radius: 8px;",
        "You selected an expedited level of research and development (tech level increase of 2 steps); costs are doubled"
      )
    }
  })
  
  output$th_expedited <- renderUI({
    req(input$th_tech_level)
    if ((tw_tech_level() == 1 & input$th_tech_level == 3) | (tw_tech_level() == 2 & input$th_tech_level == 4)){
      tags$div(
        style = "background-color: yellow; color:black; border-color: black; border-style: dashed; padding: 10px;
          border-radius: 8px;",
        "You selected an expedited level of research and development (tech level increase of 2 steps); costs are doubled"
      )
    }
  })
  
  # RD: risk level sliders
  
  output$risk_target <- renderUI({
    req(input$tech_level)
    temp <- "risk_level"
    risk(1, input$tech_level, temp)
  })  
  
  output$tw_risk_target <- renderUI({
    req(input$tw_tech_level)
    temp <- "tw_risk_level"
    risk(tech_level(), input$tw_tech_level, temp)
  })  
  
  output$th_risk_target <- renderUI({
    req(input$th_tech_level)
    temp <- "th_risk_level"
    risk(tw_tech_level(), input$th_tech_level, temp)
  })
  
  risk <- function(min, target, name){
    if ((min == 1 & target == 3) | (min == 2 & target == 4)){
      sliderInput(name, "Select risk level (3: moderate, 5: high)", min=3, max=5, value=3)
    } else {
      sliderInput(name, "Select risk level (1: low, 5: high)", min=1, max=5, value=1)
    } 
  }

  # RD: minimum rolls   
  
  output$rd_min_roll <- renderText({
    req(input$risk_level)
    paste("Given your current selections, you require a minimum R&D roll of ", input$risk_level+1)
  })

  output$tw_rd_min_roll <- renderText({
    req(input$tw_risk_level)
    paste("Given your current selections, you require a minimum R&D roll of ", input$tw_risk_level+1)
  })
  
  output$th_rd_min_roll <- renderText({
    req(input$th_risk_level)
    paste("Given your current selections, you require a minimum R&D roll of ", input$th_risk_level+1)
  })
  
  # RD: cost calculations
  
  # expedited doubles costs
  
  rd_exp <- reactive({
    req(input$tech_level)
    expedited(1,input$tech_level)
  })

  tw_rd_exp <- reactive({
    req(input$tw_tech_level)
    expedited(tech_level(),input$tw_tech_level)
  })
  
  th_rd_exp <- reactive({
    req(input$tw_tech_level)
    expedited(tw_tech_level(),input$th_tech_level)
  })
  
  expedited <- function(min, target){
    if ( (min == 1 & target == 3) | (min == 2 & target == 4)){
      2
    } else{1}
  }
  
  #sidebar RD costs output
  
  rd_cost <- reactive({
    rd(1, req(input$tech_level),req(input$risk_level))
  })
  
  tw_rd_cost <- reactive({
    rd(tech_level(), req(input$tw_tech_level),req(input$tw_risk_level))
  })
  
  th_rd_cost <- reactive({
    rd(tw_tech_level(), req(input$th_tech_level),req(input$th_risk_level))
  })
  
  rd <- function (initial, target, risk){
    if (initial == target){
      0
    } else {
      rd_costs[risk,target]
    }
  }
  
  output$rd_costs <- renderText({
    paste("Current R&D cost: ", rd_cost()*rd_exp())
  })
  
  output$tw_rd_costs <- renderText({
    paste("Current R&D cost: ", tw_rd_cost()*tw_rd_exp())
  })
  
  output$th_rd_costs <- renderText({
    paste("Current R&D cost: ", th_rd_cost()*th_rd_exp())
  })
  
  # RD retry calculation and costs display in sidebar
  
  rd_retry_costs <- reactive({
    rd_exp()*rd_retry(input$tech_retry, req(input$risk_level), req(input$tech_die_roll1), req(input$tech_level))
  })

  tw_rd_retry_costs <- reactive({
    tw_rd_exp()*rd_retry(input$tw_tech_retry, req(input$tw_risk_level), req(input$tw_tech_die_roll1), req(input$tw_tech_level))
  })
  
  th_rd_retry_costs <- reactive({
    th_rd_exp()*rd_retry(input$th_tech_retry, req(input$th_risk_level), req(input$th_tech_die_roll1), req(input$th_tech_level))
  })
  
  output$rd_costs2 <- renderText({
    rd_costs2(input$tech_retry, req(input$risk_level), req(input$tech_die_roll1), rd_retry_costs() )
  })
    
  output$tw_rd_costs2 <- renderText({
    rd_costs2(input$tw_tech_retry, req(input$tw_risk_level), req(input$tw_tech_die_roll1), tw_rd_retry_costs() )
  })
  
  output$th_rd_costs2 <- renderText({
    rd_costs2(input$th_tech_retry, req(input$th_risk_level), req(input$th_tech_die_roll1), th_rd_retry_costs() )
  })
  
  rd_retry <- function (checkbox, risk, roll, target){
    if(checkbox == T & ((risk + 1) > roll) ){
      rd_costs[6-(risk + 1 - roll), target]
    } else{0}
  }
  
  rd_costs2 <- function(checkbox, risk, roll, retry){
    if(checkbox == T & ((risk+1) > roll) ){
      paste("R&D retry cost: ", retry)      
    } else {NULL}
  }
  
  # ultimate tech level attained
  
  tech_level <- reactive({
    tech_level_attained(req(input$tech_die_roll1), req(input$risk_level), req(input$tech_level), input$tech_retry, req(input$tech_die_roll2), 1)
  })
  
  tw_tech_level <- reactive({
    tech_level_attained(req(input$tw_tech_die_roll1), req(input$tw_risk_level), req(input$tw_tech_level), input$tw_tech_retry, req(input$tw_tech_die_roll2), tech_level() )
  })
  
  th_tech_level <- reactive({
    tech_level_attained(req(input$th_tech_die_roll1), req(input$th_risk_level), req(input$th_tech_level), input$th_tech_retry, req(input$th_tech_die_roll2), tw_tech_level() )
  })
  
  output$tech_level <- renderText({
    paste("You attained tech level ", tech_level())
  })
  
  output$tw_tech_level <- renderText({
    paste("You attained tech level ", tw_tech_level())
  })
  
  output$th_tech_level <- renderText({
    paste("You attained tech level ", th_tech_level())
  })
  
  tech_level_attained <- function(roll1, risk, tech, checkbox, roll2, default){
    if(roll1 >= risk+1){
      tech
    } else if (checkbox == T & roll2 >= risk+1){
      tech
    } else {default}
  }
  
  # readiness event impact dice rolls
  
  observeEvent(input$read1_button,{output$read1_die_roll <- renderText({paste("You rolled a ", die_rolls[1,6])})  })
  observeEvent(input$read2_button,{output$read2_die_roll <- renderText({paste("You rolled a ", die_rolls[1,7])})  })
  observeEvent(input$read3_button,{output$read3_die_roll <- renderText({paste("You rolled a ", die_rolls[1,8])})  })
  observeEvent(input$read4_button,{output$read4_die_roll <- renderText({paste("You rolled a ", die_rolls[1,9])})  })
  observeEvent(input$read5_button,{output$read5_die_roll <- renderText({paste("You rolled a ", die_rolls[1,10])})  })
  observeEvent(input$read6_button1,{output$read6_die_roll1 <- renderText({paste("You rolled a ", die_rolls[1,11])})  })
  observeEvent(input$read6_button2,{output$read6_die_roll2 <- renderText({paste("You rolled a ", die_rolls[1,12])})  })
  
  observeEvent(input$tw_read1_button,{output$tw_read1_die_roll <- renderText({paste("You rolled a ", die_rolls[3,6])})  })
  observeEvent(input$tw_read2_button,{output$tw_read2_die_roll <- renderText({paste("You rolled a ", die_rolls[3,7])})  })
  observeEvent(input$tw_read3_button,{output$tw_read3_die_roll <- renderText({paste("You rolled a ", die_rolls[3,8])})  })
  observeEvent(input$tw_read4_button,{output$tw_read4_die_roll <- renderText({paste("You rolled a ", die_rolls[3,9])})  })
  observeEvent(input$tw_read5_button,{output$tw_read5_die_roll <- renderText({paste("You rolled a ", die_rolls[3,10])})  })
  observeEvent(input$tw_read6_button1,{output$tw_read6_die_roll1 <- renderText({paste("You rolled a ", die_rolls[3,11])})  })
  observeEvent(input$tw_read6_button2,{output$tw_read6_die_roll2 <- renderText({paste("You rolled a ", die_rolls[3,12])})  })
  
  observeEvent(input$th_read1_button,{output$th_read1_die_roll <- renderText({paste("You rolled a ", die_rolls[5,6])})  })
  observeEvent(input$th_read2_button,{output$th_read2_die_roll <- renderText({paste("You rolled a ", die_rolls[5,7])})  })
  observeEvent(input$th_read3_button,{output$th_read3_die_roll <- renderText({paste("You rolled a ", die_rolls[5,8])})  })
  observeEvent(input$th_read4_button,{output$th_read4_die_roll <- renderText({paste("You rolled a ", die_rolls[5,9])})  })
  observeEvent(input$th_read5_button,{output$th_read5_die_roll <- renderText({paste("You rolled a ", die_rolls[5,10])})  })
  observeEvent(input$th_read6_button1,{output$th_read6_die_roll1 <- renderText({paste("You rolled a ", die_rolls[5,11])})  })
  observeEvent(input$th_read6_button2,{output$th_read6_die_roll2 <- renderText({paste("You rolled a ", die_rolls[5,12])})  })
  
  # readiness event impact notifications
  
  output$readiness_impact <- renderText({
    temp <- NULL; temp2 <- NULL; temp3 <- NULL; button <- NULL
    if (input$readiness_event == "read1") {
      if (input$dice == "manual"){temp <- req(input$read1_die_roll)
      } else {temp <- die_rolls[1,6]
        button <- input$read1_button} 
    } else if (input$readiness_event == "read2") {
      if (input$dice == "manual"){temp <- req(input$read2_die_roll)
      } else {temp <- die_rolls[1,7]
        button <- input$read2_button}
    } else if (input$readiness_event == "read3") {
      if (input$dice == "manual"){temp <- req(input$read3_die_roll)
      } else {temp <- die_rolls[1,8]
        button <- input$read3_button}
    } else if (input$readiness_event == "read4") {
      if (input$dice == "manual"){temp <- req(input$read4_die_roll)
      } else {temp <- die_rolls[1,9]
        button <- input$read4_button}
    } else if (input$readiness_event == "read5") {
      if (input$dice == "manual"){temp <- req(input$read5_die_roll)
      } else {temp <- die_rolls[1,10]
        button <- input$read5_button}
    } else if (input$readiness_event == "read6") {
      temp <- 0
      if (input$dice == "manual"){
        temp2 <- req(input$read6_die_roll1)
        temp3 <- req(input$read6_die_roll2)
      } else {
        temp2 <- die_rolls[1,11]
        temp3 <- die_rolls[1,12]
        button <- input$read6_button1 + input$read6_button2 - 1
      }
    }
    if (!(input$readiness_event == "None")){
      read_impact(input$readiness_event, temp, temp2, temp3, button)
    } else {NULL}
  })
  
  output$tw_readiness_impact <- renderText({
    temp <- NULL; temp2 <- NULL; temp3 <- NULL; button <- NULL
    if (input$tw_readiness_event == "read1") {
      if (input$dice == "manual"){temp <- req(input$tw_read1_die_roll)
      } else {temp <- die_rolls[3,6]
      button <- input$tw_read1_button} 
    } else if (input$tw_readiness_event == "read2") {
      if (input$dice == "manual"){temp <- req(input$tw_read2_die_roll)
      } else {temp <- die_rolls[3,7]
      button <- input$tw_read2_button}
    } else if (input$tw_readiness_event == "read3") {
      if (input$dice == "manual"){temp <- req(input$tw_read3_die_roll)
      } else {temp <- die_rolls[3,8]
      button <- input$tw_read3_button}
    } else if (input$tw_readiness_event == "read4") {
      if (input$dice == "manual"){temp <- req(input$tw_read4_die_roll)
      } else {temp <- die_rolls[3,9]
      button <- input$tw_read4_button}
    } else if (input$tw_readiness_event == "read5") {
      if (input$dice == "manual"){temp <- req(input$tw_read5_die_roll)
      } else {temp <- die_rolls[3,10]
      button <- input$tw_read5_button}
    } else if (input$tw_readiness_event == "read6") {
      temp <- 0
      if (input$dice == "manual"){
        temp2 <- req(input$tw_read6_die_roll1)
        temp3 <- req(input$tw_read6_die_roll2)
      } else {
        temp2 <- die_rolls[3,11]
        temp3 <- die_rolls[3,12]
        button <- input$tw_read6_button1 + input$tw_read6_button2 - 1
      }
    }
    if (!(input$tw_readiness_event == "None")){
      read_impact(input$tw_readiness_event, temp, temp2, temp3, button)
    } else {NULL}
  })
  
  output$th_readiness_impact <- renderText({
    temp <- NULL; temp2 <- NULL; temp3 <- NULL; button <- NULL
    if (input$th_readiness_event == "read1") {
      if (input$dice == "manual"){temp <- req(input$th_read1_die_roll)
      } else {temp <- die_rolls[5,6]
      button <- input$th_read1_button} 
    } else if (input$th_readiness_event == "read2") {
      if (input$dice == "manual"){temp <- req(input$th_read2_die_roll)
      } else {temp <- die_rolls[5,7]
      button <- input$th_read2_button}
    } else if (input$th_readiness_event == "read3") {
      if (input$dice == "manual"){temp <- req(input$th_read3_die_roll)
      } else {temp <- die_rolls[5,8]
      button <- input$th_read3_button}
    } else if (input$th_readiness_event == "read4") {
      if (input$dice == "manual"){temp <- req(input$th_read4_die_roll)
      } else {temp <- die_rolls[5,9]
      button <- input$th_read4_button}
    } else if (input$th_readiness_event == "read5") {
      if (input$dice == "manual"){temp <- req(input$th_read5_die_roll)
      } else {temp <- die_rolls[5,10]
      button <- input$th_read5_button}
    } else if (input$th_readiness_event == "read6") {
      temp <- 0
      if (input$dice == "manual"){
        temp2 <- req(input$th_read6_die_roll1)
        temp3 <- req(input$th_read6_die_roll2)
      } else {
        temp2 <- die_rolls[5,11]
        temp3 <- die_rolls[5,12]
        button <- input$th_read6_button1 + input$th_read6_button2 - 1
      }
    }
    if (!(input$th_readiness_event == "None")){
      read_impact(input$th_readiness_event, temp, temp2, temp3, button)
    } else {NULL}
  })
  
  read_impact <- function(event, roll, roll2, roll3, button){
    if (event == "read1"){ temp1 <- "MILPERS roll"
    } else if (event == "read2") {temp1 <- "O&M roll"
    } else if (event == "read3") {temp1 <- "MILPERS and MILCON rolls"
    } else if (event == "read4") {temp1 <- "MILCON roll"
    } else if (event == "read5") {temp1 <- "procurement and O&M rolls"
    } else if (event == "read6") {
      if (roll2 == 1) {temp1 <- "MILCON roll"
      } else if (roll2 == 2) {temp1 <- "MILPERS roll"
      } else if (roll2 == 3) {temp1 <- "procurement roll"
      } else if (roll2 == 4) {temp1 <- "O&M roll"
      } else if (roll2 == 5) {temp1 <- "all readiness rolls"
      } else if (roll2 == 6) {
        temp1 <- "No effect on rolls"
        temp2 <- NULL
      }
      if ((roll3 == 1 | roll3 == 2) & !(roll2 == 6)){temp2 <- "Subtract 1 from"
        } else if ((roll3 == 3 | roll3 == 4) & !(roll2 == 6)){temp2 <- "Subtract 2 from "
        } else if ((roll3 == 5 | roll3 == 6) & !(roll2 == 6)){temp2 <- "Subtract 3 from "
        }
      }
    if (roll == 1){temp2 <- "Subtract 2 from"
      } else if (roll == 2 | roll == 3){temp2 <- "Subtract 1 from"
      } else if (roll == 4 | roll == 5){temp2 <- "Add 1 to"
      } else if (roll == 6){temp2 <- "Add 2 to"
      }
    if (input$dice == "manual"){
      paste(temp2, temp1)
    } else if (input$dice == "computer" & button > 0){
      paste(temp2, temp1)
    }
  }
    
  # readiness event #2 impact dice rolls 
  
  observeEvent(input$read2_1_button,{output$read2_1_die_roll <- renderText({paste("You rolled a ", die_rolls[1,13])})  })
  observeEvent(input$read2_2_button,{output$read2_2_die_roll <- renderText({paste("You rolled a ", die_rolls[1,14])})  })
  observeEvent(input$read2_3_button,{output$read2_3_die_roll <- renderText({paste("You rolled a ", die_rolls[1,15])})  })
  observeEvent(input$read2_4_button,{output$read2_4_die_roll <- renderText({paste("You rolled a ", die_rolls[1,16])})  })
  observeEvent(input$read2_5_button,{output$read2_5_die_roll <- renderText({paste("You rolled a ", die_rolls[1,17])})  })
  observeEvent(input$read2_6_button1,{output$read2_6_die_roll1 <- renderText({paste("You rolled a ", die_rolls[1,18])})  })
  observeEvent(input$read2_6_button2,{output$read2_6_die_roll2 <- renderText({paste("You rolled a ", die_rolls[1,19])})  })
  
  observeEvent(input$tw_read2_1_button,{output$tw_read2_1_die_roll <- renderText({paste("You rolled a ", die_rolls[3,13])})  })
  observeEvent(input$tw_read2_2_button,{output$tw_read2_2_die_roll <- renderText({paste("You rolled a ", die_rolls[3,14])})  })
  observeEvent(input$tw_read2_3_button,{output$tw_read2_3_die_roll <- renderText({paste("You rolled a ", die_rolls[3,15])})  })
  observeEvent(input$tw_read2_4_button,{output$tw_read2_4_die_roll <- renderText({paste("You rolled a ", die_rolls[3,16])})  })
  observeEvent(input$tw_read2_5_button,{output$tw_read2_5_die_roll <- renderText({paste("You rolled a ", die_rolls[3,17])})  })
  observeEvent(input$tw_read2_6_button1,{output$tw_read2_6_die_roll1 <- renderText({paste("You rolled a ", die_rolls[3,18])})  })
  observeEvent(input$tw_read2_6_button2,{output$tw_read2_6_die_roll2 <- renderText({paste("You rolled a ", die_rolls[3,19])})  })
  
  observeEvent(input$th_read2_1_button,{output$th_read2_1_die_roll <- renderText({paste("You rolled a ", die_rolls[5,13])})  })
  observeEvent(input$th_read2_2_button,{output$th_read2_2_die_roll <- renderText({paste("You rolled a ", die_rolls[5,14])})  })
  observeEvent(input$th_read2_3_button,{output$th_read2_3_die_roll <- renderText({paste("You rolled a ", die_rolls[5,15])})  })
  observeEvent(input$th_read2_4_button,{output$th_read2_4_die_roll <- renderText({paste("You rolled a ", die_rolls[5,16])})  })
  observeEvent(input$th_read2_5_button,{output$th_read2_5_die_roll <- renderText({paste("You rolled a ", die_rolls[5,17])})  })
  observeEvent(input$th_read2_6_button1,{output$th_read2_6_die_roll1 <- renderText({paste("You rolled a ", die_rolls[5,18])})  })
  observeEvent(input$th_read2_6_button2,{output$th_read2_6_die_roll2 <- renderText({paste("You rolled a ", die_rolls[5,19])})  })
  
  # readiness event #2 impact notifications
  
  output$readiness_impact2 <- renderText({
    temp <- NULL; temp2 <- NULL; temp3 <- NULL; button <- NULL
    if (input$readiness_event2 == "read1") {
      if (input$dice == "manual"){temp <- req(input$read2_1_die_roll)
      } else {temp <- die_rolls[1,13]
      button <- input$read2_1_button} 
    } else if (input$readiness_event2 == "read2") {
      if (input$dice == "manual"){temp <- req(input$read2_2_die_roll)
      } else {temp <- die_rolls[1,14]
      button <- input$read2_2_button}
    } else if (input$readiness_event2 == "read3") {
      if (input$dice == "manual"){temp <- req(input$read2_3_die_roll)
      } else {temp <- die_rolls[1,15]
      button <- input$read2_3_button}
    } else if (input$readiness_event2 == "read4") {
      if (input$dice == "manual"){temp <- req(input$read2_4_die_roll)
      } else {temp <- die_rolls[1,16]
      button <- input$read2_4_button}
    } else if (input$readiness_event2 == "read5") {
      if (input$dice == "manual"){temp <- req(input$read2_5_die_roll)
      } else {temp <- die_rolls[1,17]
      button <- input$read2_5_button}
    } else if (input$readiness_event2 == "read6") {
      temp <- 0
      if (input$dice == "manual"){
        temp2 <- req(input$read2_6_die_roll1)
        temp3 <- req(input$read2_6_die_roll2)
      } else {
        temp2 <- die_rolls[1,18]
        temp3 <- die_rolls[1,19]
        button <- input$read2_6_button1 + input$read2_6_button2 - 1
      }
    }
    if (!(input$readiness_event2 == "None")){
      read_impact(input$readiness_event2, temp, temp2, temp3, button)
    } else {NULL}
  })
  
  output$tw_readiness_impact2 <- renderText({
    temp <- NULL; temp2 <- NULL; temp3 <- NULL; button <- NULL
    if (input$tw_readiness_event2 == "read1") {
      if (input$dice == "manual"){temp <- req(input$tw_read2_1_die_roll)
      } else {temp <- die_rolls[3,13]
      button <- input$tw_read2_1_button} 
    } else if (input$tw_readiness_event2 == "read2") {
      if (input$dice == "manual"){temp <- req(input$tw_read2_2_die_roll)
      } else {temp <- die_rolls[3,14]
      button <- input$tw_read2_2_button}
    } else if (input$tw_readiness_event2 == "read3") {
      if (input$dice == "manual"){temp <- req(input$tw_read2_3_die_roll)
      } else {temp <- die_rolls[3,15]
      button <- input$tw_read2_3_button}
    } else if (input$tw_readiness_event2 == "read4") {
      if (input$dice == "manual"){temp <- req(input$tw_read2_4_die_roll)
      } else {temp <- die_rolls[3,16]
      button <- input$tw_read2_4_button}
    } else if (input$tw_readiness_event2 == "read5") {
      if (input$dice == "manual"){temp <- req(input$tw_read2_5_die_roll)
      } else {temp <- die_rolls[3,17]
      button <- input$tw_read2_5_button}
    } else if (input$tw_readiness_event2 == "read6") {
      temp <- 0
      if (input$dice == "manual"){
        temp2 <- req(input$tw_read2_6_die_roll1)
        temp3 <- req(input$tw_read2_6_die_roll2)
      } else {
        temp2 <- die_rolls[3,18]
        temp3 <- die_rolls[3,19]
        button <- input$tw_read2_6_button1 + input$tw_read2_6_button2 - 1
      }
    }
    if (!(input$tw_readiness_event2 == "None")){
      read_impact(input$tw_readiness_event2, temp, temp2, temp3, button)
    } else {NULL}
  })
  
  output$th_readiness_impact2 <- renderText({
    temp <- NULL; temp2 <- NULL; temp3 <- NULL; button <- NULL
    if (input$th_readiness_event2 == "read1") {
      if (input$dice == "manual"){temp <- req(input$th_read2_1_die_roll)
      } else {temp <- die_rolls[5,13]
      button <- input$th_read2_1_button} 
    } else if (input$th_readiness_event2 == "read2") {
      if (input$dice == "manual"){temp <- req(input$th_read2_2_die_roll)
      } else {temp <- die_rolls[5,14]
      button <- input$th_read2_2_button}
    } else if (input$th_readiness_event2 == "read3") {
      if (input$dice == "manual"){temp <- req(input$th_read2_3_die_roll)
      } else {temp <- die_rolls[5,15]
      button <- input$th_read2_3_button}
    } else if (input$th_readiness_event2 == "read4") {
      if (input$dice == "manual"){temp <- req(input$th_read2_4_die_roll)
      } else {temp <- die_rolls[5,16]
      button <- input$th_read2_4_button}
    } else if (input$th_readiness_event2 == "read5") {
      if (input$dice == "manual"){temp <- req(input$th_read2_5_die_roll)
      } else {temp <- die_rolls[5,17]
      button <- input$th_read2_5_button}
    } else if (input$th_readiness_event2 == "read6") {
      temp <- 0
      if (input$dice == "manual"){
        temp2 <- req(input$th_read2_6_die_roll1)
        temp3 <- req(input$th_read2_6_die_roll2)
      } else {
        temp2 <- die_rolls[5,18]
        temp3 <- die_rolls[5,19]
        button <- input$th_read2_6_button1 + input$th_read2_6_button2 - 1
      }
    }
    if (!(input$th_readiness_event2 == "None")){
      read_impact(input$th_readiness_event2, temp, temp2, temp3, button)
    } else {NULL}
  })
  
  # readiness minimum dice rolls
  
  output$milcon_min <- renderText({paste("Your MILCON funding requires a minimum roll of ", read_min_roll(input$milcon_fund))})
  output$milpers_min <- renderText({paste("Your MILPERS funding requires a minimum roll of ", read_min_roll(input$milpers_fund))})
  output$om_min <- renderText({paste("Your O&M funding requires a minimum roll of ", read_min_roll(input$om_fund))})  
  output$proc_min <- renderText({paste("Your procurement funding requires a minimum roll of ", read_min_roll(input$proc_fund))})
  
  output$tw_milcon_min <- renderText({paste("Your MILCON funding requires a minimum roll of ", read_min_roll(input$tw_milcon_fund))})
  output$tw_milpers_min <- renderText({paste("Your MILPERS funding requires a minimum roll of ", read_min_roll(input$tw_milpers_fund))})
  output$tw_om_min <- renderText({paste("Your O&M funding requires a minimum roll of ", read_min_roll(input$tw_om_fund))})  
  output$tw_proc_min <- renderText({paste("Your procurement funding requires a minimum roll of ", read_min_roll(input$tw_proc_fund))})
  
  output$th_milcon_min <- renderText({paste("Your MILCON funding requires a minimum roll of ", read_min_roll(input$th_milcon_fund))})
  output$th_milpers_min <- renderText({paste("Your MILPERS funding requires a minimum roll of ", read_min_roll(input$th_milpers_fund))})
  output$th_om_min <- renderText({paste("Your O&M funding requires a minimum roll of ", read_min_roll(input$th_om_fund))})  
  output$th_proc_min <- renderText({paste("Your procurement funding requires a minimum roll of ", read_min_roll(input$th_proc_fund))})
  
  read_min_roll <- function(input){
    if(input < 20) {12
    } else if (input > 19 && input <30) {11
    } else if (input > 29 && input <40) {10
    } else if (input > 39 && input <50) {9
    } else if (input > 49 && input <60) {8
    } else if (input > 59 && input <70) {7
    } else if (input > 69 && input <80) {6
    } else if (input > 79 && input <90) {5
    } else if (input > 89 && input <100){4
    } else {3}
  }
  
  # readiness overall funded %
  # round 1 overall readiness
  
  overall_readiness2 <- reactive ({ overall_readiness(forces_selected$d1, forces_cost2()) })
  output$overall_readiness <- renderText({ 
    temp <- overall_readiness2()
    if(is.na(temp)==T) {paste(0,"%",sep="")
    } else {paste(format(temp, digits=3),"%",sep="")}
  })
  overall_readiness <- function(selected, current_costs){
    temp <- t(selected) %*% forces_cost_table
    100*((sum(current_costs[2:5,1:2]))/(sum(temp[1,2:5])+0.3*sum(temp[2,2:5])))
  }
  
  # rounds 2 and 3 overall readiness
  
  tw_overall_readiness2 <- reactive ({ 100*req(sum(tw_forces_cost2()[2:5,1:2]))/req(sum(tw_forces_cost_100()[2:5,1:2])) })
  output$tw_overall_readiness <- renderText({ 
    if(is.na(tw_overall_readiness2())) {
      paste(0,"%",sep="")
    } else {paste(format(tw_overall_readiness2(), digits=3),"%",sep="")}
  })
  
  th_overall_readiness2 <- reactive ({ 100*req(sum(th_forces_cost2()[2:5,1:2]))/req(sum(th_forces_cost_100()[2:5,1:2])) })
  output$th_overall_readiness <- renderText({ 
    if(is.na(th_overall_readiness2())) {
      paste(0,"%",sep="")
    } else {paste(format(th_overall_readiness2(), digits=3),"%",sep="")}
  })
  
  # forces information table; d4, d5, d6 contains all updated off/def/ena changes due to upgrades to carry throughout rounds
  
  readiness_info <- reactiveValues(
    d4 = data.frame(readiness_info_initial, check.names=F), 
    d5 = data.frame(readiness_info_initial, check.names=F),
    d6 = data.frame(readiness_info_initial, check.names=F),
  )

  #d4: round1 capabilities with upgrades
  
  observeEvent( c(input$inf_upgrade, input$arm_upgrade, input$str_upgrade, input$avi_upgrade, input$art_upgrade, input$so_upgrade, input$ad_upgrade),{
    for (i in 1:3){
      readiness_info$d4[1,i+2] <- readiness_info_initial[1,i+2] + upgrade_check(input$inf_upgrade,i)
      readiness_info$d4[2,i+2] <- readiness_info_initial[2,i+2] + upgrade_check(input$arm_upgrade,i)
      readiness_info$d4[3,i+2] <- readiness_info_initial[3,i+2] + upgrade_check(input$str_upgrade,i)
      readiness_info$d4[4,i+2] <- readiness_info_initial[4,i+2] + upgrade_check(input$avi_upgrade,i)
      readiness_info$d4[5,i+2] <- readiness_info_initial[5,i+2] + upgrade_check(input$art_upgrade,i)
      readiness_info$d4[6,i+2] <- readiness_info_initial[6,i+2] + upgrade_check(input$so_upgrade,i)
      readiness_info$d4[7,i+2] <- readiness_info_initial[7,i+2] + upgrade_check(input$ad_upgrade,i)
    }
  }, ignoreNULL=F)
  
  readiness_info2 <- reactive ({
    readiness_info$d4
  })
  
  output$readiness_info <- renderTable(
    readiness_info$d4,
    rownames=T, digits=0, , bordered = TRUE, striped = TRUE)
  
  
  #d5: round2 capabilities with upgrades, including from round1
  
  observeEvent( c(input$inf_upgrade, input$arm_upgrade, input$str_upgrade, input$avi_upgrade, input$art_upgrade, input$so_upgrade, input$ad_upgrade,
                  input$tw_inf_upgrade, input$tw_arm_upgrade, input$tw_str_upgrade, input$tw_avi_upgrade, input$tw_art_upgrade, input$tw_so_upgrade, input$tw_ad_upgrade),{
    for (i in 1:3){
      readiness_info$d5[1,i+2] <- readiness_info$d4[1,i+2] + upgrade_check(input$tw_inf_upgrade,i)
      readiness_info$d5[2,i+2] <- readiness_info$d4[2,i+2] + upgrade_check(input$tw_arm_upgrade,i)
      readiness_info$d5[3,i+2] <- readiness_info$d4[3,i+2] + upgrade_check(input$tw_str_upgrade,i)
      readiness_info$d5[4,i+2] <- readiness_info$d4[4,i+2] + upgrade_check(input$tw_avi_upgrade,i)
      readiness_info$d5[5,i+2] <- readiness_info$d4[5,i+2] + upgrade_check(input$tw_art_upgrade,i)
      readiness_info$d5[6,i+2] <- readiness_info$d4[6,i+2] + upgrade_check(input$tw_so_upgrade,i)
      readiness_info$d5[7,i+2] <- readiness_info$d4[7,i+2] + upgrade_check(input$tw_ad_upgrade,i)
    }
    readiness_info$d5[1,3] <- min(readiness_info$d5[1,3],6); readiness_info$d5[1,4] <- min(readiness_info$d5[1,4],8); readiness_info$d5[1,5] <- max(readiness_info$d5[1,5],3)
    readiness_info$d5[2,3] <- min(readiness_info$d5[2,3],11); readiness_info$d5[2,4] <- min(readiness_info$d5[2,4],7); readiness_info$d5[2,5] <- max(readiness_info$d5[2,5],8)
    readiness_info$d5[3,3] <- min(readiness_info$d5[3,3],9); readiness_info$d5[3,4] <- min(readiness_info$d5[3,4],7); readiness_info$d5[3,5] <- max(readiness_info$d5[3,5],4)
    readiness_info$d5[4,3] <- min(readiness_info$d5[4,3],8); readiness_info$d5[4,4] <- min(readiness_info$d5[4,4],6); readiness_info$d5[4,5] <- max(readiness_info$d5[4,5],6)
    readiness_info$d5[5,3] <- min(readiness_info$d5[5,3],7); readiness_info$d5[5,4] <- min(readiness_info$d5[5,4],4); readiness_info$d5[5,5] <- max(readiness_info$d5[5,5],3)
    readiness_info$d5[6,3] <- min(readiness_info$d5[6,3],12); readiness_info$d5[6,4] <- min(readiness_info$d5[6,4],5); readiness_info$d5[6,5] <- max(readiness_info$d5[6,5],1)
    readiness_info$d5[7,3] <- min(readiness_info$d5[7,3],3); readiness_info$d5[7,4] <- min(readiness_info$d5[7,4],10); readiness_info$d5[7,5] <- max(readiness_info$d5[7,5],2)
  }, ignoreNULL=F)
  
  tw_readiness_info2 <- reactive ({
    readiness_info$d5
  })
  
  output$tw_readiness_info <- renderTable(
    readiness_info$d5,
    rownames=T, digits=0, bordered = TRUE, striped = TRUE)
  
  #d6: round3 capabilities with upgrades, including from rounds 1 and 2
  
  observeEvent( c(input$th_inf_upgrade, input$th_arm_upgrade, input$th_str_upgrade, input$th_avi_upgrade, input$th_art_upgrade, input$th_so_upgrade, input$th_ad_upgrade,
                  input$inf_upgrade, input$arm_upgrade, input$str_upgrade, input$avi_upgrade, input$art_upgrade, input$so_upgrade, input$ad_upgrade,
                  input$tw_inf_upgrade, input$tw_arm_upgrade, input$tw_str_upgrade, input$tw_avi_upgrade, input$tw_art_upgrade, input$tw_so_upgrade, input$tw_ad_upgrade),{
    for (i in 1:3){
      readiness_info$d6[1,i+2] <- readiness_info$d5[1,i+2] + upgrade_check(input$th_inf_upgrade,i)
      readiness_info$d6[2,i+2] <- readiness_info$d5[2,i+2] + upgrade_check(input$th_arm_upgrade,i)
      readiness_info$d6[3,i+2] <- readiness_info$d5[3,i+2] + upgrade_check(input$th_str_upgrade,i)
      readiness_info$d6[4,i+2] <- readiness_info$d5[4,i+2] + upgrade_check(input$th_avi_upgrade,i)
      readiness_info$d6[5,i+2] <- readiness_info$d5[5,i+2] + upgrade_check(input$th_art_upgrade,i)
      readiness_info$d6[6,i+2] <- readiness_info$d5[6,i+2] + upgrade_check(input$th_so_upgrade,i)
      readiness_info$d6[7,i+2] <- readiness_info$d5[7,i+2] + upgrade_check(input$th_ad_upgrade,i)
    }
    readiness_info$d6[1,3] <- min(readiness_info$d6[1,3],6); readiness_info$d6[1,4] <- min(readiness_info$d6[1,4],8); readiness_info$d6[1,5] <- max(readiness_info$d6[1,5],3)
    readiness_info$d6[2,3] <- min(readiness_info$d6[2,3],11); readiness_info$d6[2,4] <- min(readiness_info$d6[2,4],7); readiness_info$d6[2,5] <- max(readiness_info$d6[2,5],8)
    readiness_info$d6[3,3] <- min(readiness_info$d6[3,3],9); readiness_info$d6[3,4] <- min(readiness_info$d6[3,4],7); readiness_info$d6[3,5] <- max(readiness_info$d6[3,5],4)
    readiness_info$d6[4,3] <- min(readiness_info$d6[4,3],8); readiness_info$d6[4,4] <- min(readiness_info$d6[4,4],6); readiness_info$d6[4,5] <- max(readiness_info$d6[4,5],6)
    readiness_info$d6[5,3] <- min(readiness_info$d6[5,3],7); readiness_info$d6[5,4] <- min(readiness_info$d6[5,4],4); readiness_info$d6[5,5] <- max(readiness_info$d6[5,5],3)
    readiness_info$d6[6,3] <- min(readiness_info$d6[6,3],12); readiness_info$d6[6,4] <- min(readiness_info$d6[6,4],5); readiness_info$d6[6,5] <- max(readiness_info$d6[6,5],1)
    readiness_info$d6[7,3] <- min(readiness_info$d6[7,3],3); readiness_info$d6[7,4] <- min(readiness_info$d6[7,4],10); readiness_info$d6[7,5] <- max(readiness_info$d6[7,5],2)
  }, ignoreNULL=F)
  
  th_readiness_info2 <- reactive ({
    readiness_info$d6
  })
  
  output$th_readiness_info <- renderTable(
    readiness_info$d6,
    rownames=T, digits=0, bordered = TRUE, striped = TRUE)  
  
  # upgrade checks for forces information table
  
  upgrade_check <- function(a,b){ if (b %in% a) {if (b==3) {-2} else {2} } else {0} }
  
  # tally of # of upgrades; 
  
  upgrade_table <- reactiveValues(
    d1 = matrix(0, nrow=8),
    d2 = matrix(0, nrow=8),
    d3 = matrix(0, nrow=8)
  )
  
  observeEvent(c(forces_selected$d1, input$inf_upgrade, input$arm_upgrade, input$str_upgrade, input$avi_upgrade, input$art_upgrade, input$so_upgrade, input$ad_upgrade), {
    upgrade_table$d1 <- matrix(0, nrow=8)
    for (i in 1:3){
      if (i %in% input$inf_upgrade & sum(forces_selected$d1[1,]) > 0){
        upgrade_table$d1[1,1] <- upgrade_table$d1[1,1] + 1
      }
      if (i %in% input$arm_upgrade & sum(forces_selected$d1[2,]) > 0){
        upgrade_table$d1[2,1] <- upgrade_table$d1[2,1] + 1
      }
      if (i %in% input$str_upgrade & sum(forces_selected$d1[3,]) > 0){
        upgrade_table$d1[3,1] <- upgrade_table$d1[3,1] + 1
      }
      if (i %in% input$avi_upgrade & sum(forces_selected$d1[4,]) > 0){
        upgrade_table$d1[4,1] <- upgrade_table$d1[4,1] + 1
      }
      if (i %in% input$art_upgrade & sum(forces_selected$d1[5,]) > 0){
        upgrade_table$d1[5,1] <- upgrade_table$d1[5,1] + 1
      }
      if (i %in% input$so_upgrade & sum(forces_selected$d1[6,]) > 0){
        upgrade_table$d1[6,1] <- upgrade_table$d1[6,1] + 1
      }
      if (i %in% input$ad_upgrade & sum(forces_selected$d1[7,]) > 0){
        upgrade_table$d1[7,1] <- upgrade_table$d1[7,1] + 1
      }
    }
  }, ignoreNULL=F)
  
  observeEvent(c(input$tw_inf_upgrade, input$tw_arm_upgrade, input$tw_str_upgrade, input$tw_avi_upgrade, input$tw_art_upgrade, input$tw_so_upgrade, input$tw_ad_upgrade), {
    upgrade_table$d2 <- matrix(0, nrow=8)
    for (i in 1:3){
      if (i %in% input$tw_inf_upgrade & !(i %in% input$inf_upgrade) & sum(forces_selected$d2[1,]) > 0){
        upgrade_table$d2[1,1] <- upgrade_table$d2[1,1] + 1
      }
      if (i %in% input$tw_arm_upgrade & !(i %in% input$arm_upgrade) & sum(forces_selected$d2[2,]) > 0){
        upgrade_table$d2[2,1] <- upgrade_table$d2[2,1] + 1
      }
      if (i %in% input$tw_str_upgrade & !(i %in% input$str_upgrade) & sum(forces_selected$d2[3,]) > 0){  
        upgrade_table$d2[3,1] <- upgrade_table$d2[3,1] + 1
      }
      if (i %in% input$tw_avi_upgrade & !(i %in% input$avi_upgrade) & sum(forces_selected$d2[4,]) > 0){
        upgrade_table$d2[4,1] <- upgrade_table$d2[4,1] + 1
      }
      if (i %in% input$tw_art_upgrade & !(i %in% input$art_upgrade) & sum(forces_selected$d2[5,]) > 0){
        upgrade_table$d2[5,1] <- upgrade_table$d2[5,1] + 1
      }
      if (i %in% input$tw_so_upgrade & !(i %in% input$so_upgrade) & sum(forces_selected$d2[6,]) > 0){  
        upgrade_table$d2[6,1] <- upgrade_table$d2[6,1] + 1
      }
      if (i %in% input$tw_ad_upgrade & !(i %in% input$ad_upgrade) & sum(forces_selected$d2[7,]) > 0){
        upgrade_table$d2[7,1] <- upgrade_table$d2[7,1] + 1
      }
    }
  }, ignoreNULL=F)
  
  
  observeEvent(c(input$th_inf_upgrade, input$th_arm_upgrade, input$th_str_upgrade, input$th_avi_upgrade, input$th_art_upgrade, input$th_so_upgrade, input$th_ad_upgrade), {
    upgrade_table$d3 <- matrix(0, nrow=8)
    for (i in 1:3){
      if (i %in% input$th_inf_upgrade & !(i %in% input$tw_inf_upgrade) & !(i %in% input$inf_upgrade) & sum(forces_selected$d3[1,]) > 0){ 
        upgrade_table$d3[1,1] <- upgrade_table$d3[1,1] + 1
      }
      if (i %in% input$th_arm_upgrade & !(i %in% input$tw_arm_upgrade) & !(i %in% input$arm_upgrade) & sum(forces_selected$d3[2,]) > 0){
        upgrade_table$d3[2,1] <- upgrade_table$d3[2,1] + 1
      }
      if (i %in% input$th_str_upgrade & !(i %in% input$tw_str_upgrade) & !(i %in% input$str_upgrade) & sum(forces_selected$d3[3,]) > 0){
        upgrade_table$d3[3,1] <- upgrade_table$d3[3,1] + 1
      }
      if (i %in% input$th_avi_upgrade & !(i %in% input$tw_avi_upgrade) & !(i %in% input$avi_upgrade) & sum(forces_selected$d3[4,]) > 0){
        upgrade_table$d3[4,1] <- upgrade_table$d3[4,1] + 1
      }
      if (i %in% input$th_art_upgrade & !(i %in% input$tw_art_upgrade) & !(i %in% input$art_upgrade) & sum(forces_selected$d3[5,]) > 0){
        upgrade_table$d3[5,1] <- upgrade_table$d3[5,1] + 1
      }
      if (i %in% input$th_so_upgrade & !(i %in% input$tw_so_upgrade) & !(i %in% input$so_upgrade) & sum(forces_selected$d3[6,]) > 0){
        upgrade_table$d3[6,1] <- upgrade_table$d3[6,1] + 1
      }
      if (i %in% input$th_ad_upgrade & !(i %in% input$tw_ad_upgrade) & !(i %in% input$ad_upgrade) & sum(forces_selected$d3[7,]) > 0){
        upgrade_table$d3[7,1] <- upgrade_table$d3[7,1] + 1
      }
    }
  }, ignoreNULL=F)
  
  # tally units that are being upgraded
  
  rd1_units_upgraded <- reactive({
    temp <- 0
    for (i in 1:7){
      if (upgrade_table$d1[i,] > 0){
        temp <- temp + sum(forces_selected$d1[i,])
      }
    }
    temp
  })
  
  rd2_units_upgraded <- reactive({
    temp <- 0
    for (i in 1:7){
      if (upgrade_table$d2[i,] > 0){
        temp <- temp + sum(forces_selected$d2[i,])
      }
    }
    temp
  })
  
  rd3_units_upgraded <- reactive({
    temp <- 0
    for (i in 1:7){
      if (upgrade_table$d3[i,] > 0){
        temp <- temp + sum(forces_selected$d3[i,])
      }
    }
    temp
  })
  
  # upgrades min roll based on number of upgrades and tech level
  
  upgrades_min <- reactiveVal(0)
  tw_upgrades_min <- reactiveVal(0)
  th_upgrades_min <- reactiveVal(0)
  
  upgrades_cost <- reactiveVal(0)
  tw_upgrades_cost <- reactiveVal(0)
  th_upgrades_cost <- reactiveVal(0)
  
  output$upgrades_min <- renderText({upgrades_minimum(rd1_units_upgraded()*sum(upgrade_table$d1[1:8]), upgrades_cost, tech_level(), upgrades_min)})
  output$tw_upgrades_min <- renderText({upgrades_minimum(rd2_units_upgraded()*sum(upgrade_table$d2[1:8]), tw_upgrades_cost, tw_tech_level(), tw_upgrades_min)})
  output$th_upgrades_min <- renderText({upgrades_minimum(rd3_units_upgraded()*sum(upgrade_table$d3[1:8]), th_upgrades_cost, th_tech_level(), th_upgrades_min)})
  
  upgrades_minimum <- function (upgrades, cost, tech, min){
    if (upgrades == 0){
      cost(0)
    } else if (upgrades < 6){
      cost(5)
      min(upgrade_min_roll[1, tech])
    } else if (upgrades < 11){
      cost(4)
      min(upgrade_min_roll[2, tech])
    } else if (upgrades < 16){
      cost(3)
      min(upgrade_min_roll[3, tech])
    } else if (upgrades > 15){
      cost(2)
      min(upgrade_min_roll[4, tech])
    }
    if (upgrades > 0){
      paste("Your selected upgrades require a minimum roll of ", min())
    } else {paste("No upgrades selected")}
  }
  
  # upgrades costs
  
  upgrades_cancel <- reactiveVal(0)
  tw_upgrades_cancel <- reactiveVal(0)
  th_upgrades_cancel <- reactiveVal(0)
  
  total_upgrades_cost <- reactive({
    rd1_units_upgraded()*sum(upgrade_table$d1)*upgrades_cost()
  })
  output$upgrades_cost <- renderText({ paste("Current upgrades cost: ", total_upgrades_cost()*overrun()+upgrades_cancel() ) })
  
  tw_total_upgrades_cost <- reactive({
    rd2_units_upgraded()*sum(upgrade_table$d2)*tw_upgrades_cost()
  })
  output$tw_upgrades_cost <- renderText({ paste("Current upgrades cost: ", tw_total_upgrades_cost()*tw_overrun()+tw_upgrades_cancel()) })

  th_total_upgrades_cost <- reactive({
    rd3_units_upgraded()*sum(upgrade_table$d3)*th_upgrades_cost()
  })
  output$th_upgrades_cost <- renderText({ paste("Current upgrades cost: ", th_total_upgrades_cost()*th_overrun()+th_upgrades_cancel()) })  
  
  # upgrades overrun
  
  overrun <- reactive({ upgrades_overrun(upgrades_min(), req(input$upgrades_die_roll)) })
  output$overrun <- renderUI({
    if (overrun() > 1 & (sum(upgrade_table$d1[1:8]) > 0)){
      div(
        br(),
        div(class = "notice",
          paste("Because you failed to achieve your minimum dice roll, your overrun costs for upgrades are now ", total_upgrades_cost()*overrun() )    
        )
      )
    }
  })
  
  tw_overrun <- reactive({ upgrades_overrun(tw_upgrades_min(), req(input$tw_upgrades_die_roll)) })
  output$tw_overrun <- renderUI({
    if (tw_overrun() > 1 & (sum(upgrade_table$d2[1:8]) > 0)){
      div(
        br(),  
        div(class = "notice", 
          paste("Because you failed to achieve your minimum dice roll, your overrun costs for upgrades are now ", tw_total_upgrades_cost()*tw_overrun() )
        )
      )
    }
  })
  
  th_overrun <- reactive({ upgrades_overrun(th_upgrades_min(), req(input$th_upgrades_die_roll)) })
  output$th_overrun <- renderUI({
    if (th_overrun() > 1 & (sum(upgrade_table$d3[1:8]) > 0)){
      div(
        br(),
        div(class = "notice",
          paste("Because you failed to achieve your minimum dice roll, your overrun costs for upgrades are now ", th_total_upgrades_cost()*th_overrun() )
        )
      )
    }
  })
  
  upgrades_overrun <- function (min,roll){
    if (min > roll){
      1+((min - roll + 1)/10)
    } else {1}
  }
  
  # upgrades for subsequent rounds reflect past round upgrades
  # round 1 upgrades apply to rounds 2 and 3

  observeEvent(input$inf_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$inf_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "tw_inf_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$arm_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$arm_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "tw_arm_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$str_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$str_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "tw_str_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$avi_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$avi_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "tw_avi_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$art_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$art_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "tw_art_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$so_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$so_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "tw_so_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$ad_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$ad_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "tw_ad_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  # round 2 upgrades apply to round 3
  
  observeEvent(input$tw_inf_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$tw_inf_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "th_inf_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$tw_arm_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$tw_arm_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "th_arm_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$tw_str_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$tw_str_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "th_str_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$tw_avi_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$tw_avi_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "th_avi_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$tw_art_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$tw_art_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "th_art_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$tw_so_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$tw_so_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "th_so_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  observeEvent(input$tw_ad_upgrade,{
    temp <- NULL; temp1 <- 1
    for (i in 1:3){ if (i %in% input$tw_ad_upgrade){ temp[temp1] <- i; temp1 <- temp1 + 1 } }
    updateCheckboxGroupInput(session, "th_ad_upgrade", choices=list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=temp)
  }, ignoreNULL=F)
  
  # upgrades cancellation
  
  observeEvent(input$upgrades_cancel,{
    upgrades_cancel(0.5*total_upgrades_cost())
    updateCheckboxGroupInput(session, "inf_upgrade", choices = list("+2 Offense" = 1, "+2 Defense" = 2, "-2 Enabler requirement" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "arm_upgrade", choices = list("+2 Offense" = 1, "+2 Defense" = 2, "-2 Enabler requirement" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "str_upgrade", choices = list("+2 Offense" = 1, "+2 Defense" = 2, "-2 Enabler requirement" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "avi_upgrade", choices = list("+2 Offense" = 1, "+2 Defense" = 2, "-2 Enabler requirement" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "art_upgrade", choices = list("+2 Offense" = 1, "+2 Defense" = 2, "-2 Enabler requirement" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "so_upgrade", choices = list("+2 Offense" = 1, "+2 Defense" = 2, "-2 Enabler requirement" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "ad_upgrade", choices = list("+2 Offense" = 1, "+2 Defense" = 2, "-2 Enabler requirement" = 3), selected=NULL)
  })
  
  observeEvent(input$tw_upgrades_cancel,{
    tw_upgrades_cancel(0.5*tw_total_upgrades_cost())
    updateCheckboxGroupInput(session, "tw_inf_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "tw_arm_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "tw_str_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "tw_avi_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "tw_art_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "tw_so_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "tw_ad_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
  })
  
  observeEvent(input$th_upgrades_cancel,{
    th_upgrades_cancel(0.5*th_total_upgrades_cost())
    updateCheckboxGroupInput(session, "th_inf_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "th_arm_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "th_str_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "th_avi_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "th_art_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "th_so_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
    updateCheckboxGroupInput(session, "th_ad_upgrade", choices = list("+2 Off" = 1, "+2 Def" = 2, "-2 Enabler" = 3), selected=NULL)
  })

  # rounds 2 and 3 starting matrices from previous round
  
  output$tw_forces_selected2 <- renderTable({format(round2_start(), digits=1)}, rownames=T, striped = T)
  output$th_forces_selected2 <- renderTable({format(round3_start(), digits=1)}, rownames=T, striped = T) 
    
  # rounds 2 and 3 disband matrices
  
  disband_matrix <- reactiveValues(
    d2 = matrix(rep(0,16), nrow=8),
    d3 = matrix(rep(0,16), nrow=8)
  )
  
  observeEvent(c(input$inf_AC_disband, input$inf_RC_disband, input$arm_AC_disband, input$arm_RC_disband, input$str_AC_disband, input$str_RC_disband, input$avi_AC_disband, input$avi_RC_disband, input$art_AC_disband, input$art_RC_disband, input$so_AC_disband, input$so_RC_disband, input$ad_AC_disband, input$ad_RC_disband, input$ena_AC_disband, input$ena_RC_disband ),{
    disband_matrix$d2[1,1] <- req(input$inf_AC_disband); disband_matrix$d2[1,2] <- req(input$inf_RC_disband)
    disband_matrix$d2[2,1] <- req(input$arm_AC_disband); disband_matrix$d2[2,2] <- req(input$arm_RC_disband)
    disband_matrix$d2[3,1] <- req(input$str_AC_disband); disband_matrix$d2[3,2] <- req(input$str_RC_disband)
    disband_matrix$d2[4,1] <- req(input$avi_AC_disband); disband_matrix$d2[4,2] <- req(input$avi_RC_disband)
    disband_matrix$d2[5,1] <- req(input$art_AC_disband); disband_matrix$d2[5,2] <- req(input$art_RC_disband)
    disband_matrix$d2[6,1] <- req(input$so_AC_disband); disband_matrix$d2[6,2] <- req(input$so_RC_disband)
    disband_matrix$d2[7,1] <- req(input$ad_AC_disband); disband_matrix$d2[7,2] <- req(input$ad_RC_disband)
    disband_matrix$d2[8,1] <- req(input$ena_AC_disband); disband_matrix$d2[8,2] <- req(input$ena_RC_disband)
  })
  
  observeEvent(c(input$th_inf_AC_disband, input$th_inf_RC_disband, input$th_arm_AC_disband, input$th_arm_RC_disband, input$th_str_AC_disband, input$th_str_RC_disband, input$th_avi_AC_disband, input$th_avi_RC_disband, input$th_art_AC_disband, input$th_art_RC_disband, input$th_so_AC_disband, input$th_so_RC_disband, input$th_ad_AC_disband, input$th_ad_RC_disband, input$th_ena_AC_disband, input$th_ena_RC_disband ),{
    disband_matrix$d3[1,1] <- req(input$th_inf_AC_disband); disband_matrix$d3[1,2] <- req(input$th_inf_RC_disband)
    disband_matrix$d3[2,1] <- req(input$th_arm_AC_disband); disband_matrix$d3[2,2] <- req(input$th_arm_RC_disband)
    disband_matrix$d3[3,1] <- req(input$th_str_AC_disband); disband_matrix$d3[3,2] <- req(input$th_str_RC_disband)
    disband_matrix$d3[4,1] <- req(input$th_avi_AC_disband); disband_matrix$d3[4,2] <- req(input$th_avi_RC_disband)
    disband_matrix$d3[5,1] <- req(input$th_art_AC_disband); disband_matrix$d3[5,2] <- req(input$th_art_RC_disband)
    disband_matrix$d3[6,1] <- req(input$th_so_AC_disband); disband_matrix$d3[6,2] <- req(input$th_so_RC_disband)
    disband_matrix$d3[7,1] <- req(input$th_ad_AC_disband); disband_matrix$d3[7,2] <- req(input$th_ad_RC_disband)
    disband_matrix$d3[8,1] <- req(input$th_ena_AC_disband); disband_matrix$d3[8,2] <- req(input$th_ena_RC_disband)
  })
  
  # rounds 2 and 3 new unit matrices
  
  new_units <- reactiveValues(
    d2 = matrix(rep(0,16), nrow=8),
    d3 = matrix(rep(0,16), nrow=8)
  )
  
  observeEvent(c(input$tw_inf_AC, input$tw_inf_RC, input$tw_arm_AC, input$tw_arm_RC, input$tw_str_AC, input$tw_str_RC, input$tw_avi_AC, input$tw_avi_RC, input$tw_art_AC, input$tw_art_RC, input$tw_so_AC, input$tw_so_RC, input$tw_ad_AC, input$tw_ad_RC, input$tw_ena_AC, input$tw_ena_RC),{
    new_units$d2[1,1] <- req(input$tw_inf_AC); new_units$d2[1,2] <- req(input$tw_inf_RC)
    new_units$d2[2,1] <- req(input$tw_arm_AC); new_units$d2[2,2] <- req(input$tw_arm_RC)
    new_units$d2[3,1] <- req(input$tw_str_AC); new_units$d2[3,2] <- req(input$tw_str_RC)
    new_units$d2[4,1] <- req(input$tw_avi_AC); new_units$d2[4,2] <- req(input$tw_avi_RC)
    new_units$d2[5,1] <- req(input$tw_art_AC); new_units$d2[5,2] <- req(input$tw_art_RC)
    new_units$d2[6,1] <- req(input$tw_so_AC); new_units$d2[6,2] <- req(input$tw_so_RC)
    new_units$d2[7,1] <- req(input$tw_ad_AC); new_units$d2[7,2] <- req(input$tw_ad_RC)
    new_units$d2[8,1] <- req(input$tw_ena_AC); new_units$d2[8,2] <- req(input$tw_ena_RC)
  })
  
  observeEvent(c(input$th_inf_AC, input$th_inf_RC, input$th_arm_AC, input$th_arm_RC, input$th_str_AC, input$th_str_RC, input$th_avi_AC, input$th_avi_RC, input$th_art_AC, input$th_art_RC, input$th_so_AC, input$th_so_RC, input$th_ad_AC, input$th_ad_RC, input$th_ena_AC, input$th_ena_RC),{
    new_units$d3[1,1] <- req(input$th_inf_AC); new_units$d3[1,2] <- req(input$th_inf_RC)
    new_units$d3[2,1] <- req(input$th_arm_AC); new_units$d3[2,2] <- req(input$th_arm_RC)
    new_units$d3[3,1] <- req(input$th_str_AC); new_units$d3[3,2] <- req(input$th_str_RC)
    new_units$d3[4,1] <- req(input$th_avi_AC); new_units$d3[4,2] <- req(input$th_avi_RC)
    new_units$d3[5,1] <- req(input$th_art_AC); new_units$d3[5,2] <- req(input$th_art_RC)
    new_units$d3[6,1] <- req(input$th_so_AC); new_units$d3[6,2] <- req(input$th_so_RC)
    new_units$d3[7,1] <- req(input$th_ad_AC); new_units$d3[7,2] <- req(input$th_ad_RC)
    new_units$d3[8,1] <- req(input$th_ena_AC); new_units$d3[8,2] <- req(input$th_ena_RC)
  })
  
  # forces selected for each round
  
  forces_selected <- reactiveValues(
    d1 = matrix(rep(0,16), nrow=8, dimnames = list(c("Infantry","Armor","Stryker","Aviation","Artillery","Special Operations","Air Defense","Enablers"),c("AC","RC"))),  
    d2 = matrix(rep(0,16), nrow=8, dimnames = list(c("Infantry","Armor","Stryker","Aviation","Artillery","Special Operations","Air Defense","Enablers"),c("AC","RC"))),
    d3 = matrix(rep(0,16), nrow=8, dimnames = list(c("Infantry","Armor","Stryker","Aviation","Artillery","Special Operations","Air Defense","Enablers"),c("AC","RC")))
  )
  
  # forces_selected$d1  is the round 1 reactive table
  
  observeEvent(c(input$inf_AC,input$inf_RC,input$arm_AC,input$arm_RC,input$str_AC,input$str_RC,input$avi_AC,input$avi_RC,input$art_AC,input$art_RC,input$so_AC,input$so_RC,input$ad_AC,input$ad_RC,input$ena_AC,input$ena_RC),{
    forces_selected$d1[1,1] <- req(input$inf_AC); forces_selected$d1[1,2] <- req(input$inf_RC)
    forces_selected$d1[2,1] <- req(input$arm_AC); forces_selected$d1[2,2] <- req(input$arm_RC)
    forces_selected$d1[3,1] <- req(input$str_AC); forces_selected$d1[3,2] <- req(input$str_RC)
    forces_selected$d1[4,1] <- req(input$avi_AC); forces_selected$d1[4,2] <- req(input$avi_RC)
    forces_selected$d1[5,1] <- req(input$art_AC); forces_selected$d1[5,2] <- req(input$art_RC)
    forces_selected$d1[6,1] <- req(input$so_AC);  forces_selected$d1[6,2] <- req(input$so_RC)
    forces_selected$d1[7,1] <- req(input$ad_AC);  forces_selected$d1[7,2] <- req(input$ad_RC)
    forces_selected$d1[8,1] <- req(input$ena_AC); forces_selected$d1[8,2] <- req(input$ena_RC)
  })
  
  # forces_selected$d2 is the round2 reactive table for round2 forces selected
  
  output$forces_selected_rd2 <- renderTable({format(forces_selected$d2, digits=1)}, rownames=T, striped = T)
  
  observeEvent(c(round2_start(), disband_matrix$d2, new_units$d2),{
    forces_selected$d2[1,1] <- round2_start()[1,1] - disband_matrix$d2[1,1] + new_units$d2[1,1]
    forces_selected$d2[1,2] <- round2_start()[1,2] - disband_matrix$d2[1,2] + new_units$d2[1,2]
    forces_selected$d2[2,1] <- round2_start()[2,1] - disband_matrix$d2[2,1] + new_units$d2[2,1]
    forces_selected$d2[2,2] <- round2_start()[2,2] - disband_matrix$d2[2,2] + new_units$d2[2,2]
    forces_selected$d2[3,1] <- round2_start()[3,1] - disband_matrix$d2[3,1] + new_units$d2[3,1]
    forces_selected$d2[3,2] <- round2_start()[3,2] - disband_matrix$d2[3,2] + new_units$d2[3,2]
    forces_selected$d2[4,1] <- round2_start()[4,1] - disband_matrix$d2[4,1] + new_units$d2[4,1]
    forces_selected$d2[4,2] <- round2_start()[4,2] - disband_matrix$d2[4,2] + new_units$d2[4,2]
    forces_selected$d2[5,1] <- round2_start()[5,1] - disband_matrix$d2[5,1] + new_units$d2[5,1]
    forces_selected$d2[5,2] <- round2_start()[5,2] - disband_matrix$d2[5,2] + new_units$d2[5,2]
    forces_selected$d2[6,1] <- round2_start()[6,1] - disband_matrix$d2[6,1] + new_units$d2[6,1]
    forces_selected$d2[6,2] <- round2_start()[6,2] - disband_matrix$d2[6,2] + new_units$d2[6,2]
    forces_selected$d2[7,1] <- round2_start()[7,1] - disband_matrix$d2[7,1] + new_units$d2[7,1]
    forces_selected$d2[7,2] <- round2_start()[7,2] - disband_matrix$d2[7,2] + new_units$d2[7,2]
    forces_selected$d2[8,1] <- round2_start()[8,1] - disband_matrix$d2[8,1] + new_units$d2[8,1]
    forces_selected$d2[8,2] <- round2_start()[8,2] - disband_matrix$d2[8,2] + new_units$d2[8,2]
  })
  
  # forces_selected$d3 is the round3 reactive table for round3 forces selected
  
  output$forces_selected_rd3 <- renderTable({format(forces_selected$d3, digits=1)}, rownames=T, striped=T)
  
  observeEvent(c(round3_start(), disband_matrix$d3, new_units$d3),{
    forces_selected$d3[1,1] <- round3_start()[1,1] - disband_matrix$d3[1,1] + new_units$d3[1,1]
    forces_selected$d3[1,2] <- round3_start()[1,2] - disband_matrix$d3[1,2] + new_units$d3[1,2]
    forces_selected$d3[2,1] <- round3_start()[2,1] - disband_matrix$d3[2,1] + new_units$d3[2,1]
    forces_selected$d3[2,2] <- round3_start()[2,2] - disband_matrix$d3[2,2] + new_units$d3[2,2]
    forces_selected$d3[3,1] <- round3_start()[3,1] - disband_matrix$d3[3,1] + new_units$d3[3,1]
    forces_selected$d3[3,2] <- round3_start()[3,2] - disband_matrix$d3[3,2] + new_units$d3[3,2]
    forces_selected$d3[4,1] <- round3_start()[4,1] - disband_matrix$d3[4,1] + new_units$d3[4,1]
    forces_selected$d3[4,2] <- round3_start()[4,2] - disband_matrix$d3[4,2] + new_units$d3[4,2]
    forces_selected$d3[5,1] <- round3_start()[5,1] - disband_matrix$d3[5,1] + new_units$d3[5,1]
    forces_selected$d3[5,2] <- round3_start()[5,2] - disband_matrix$d3[5,2] + new_units$d3[5,2]
    forces_selected$d3[6,1] <- round3_start()[6,1] - disband_matrix$d3[6,1] + new_units$d3[6,1]
    forces_selected$d3[6,2] <- round3_start()[6,2] - disband_matrix$d3[6,2] + new_units$d3[6,2]
    forces_selected$d3[7,1] <- round3_start()[7,1] - disband_matrix$d3[7,1] + new_units$d3[7,1]
    forces_selected$d3[7,2] <- round3_start()[7,2] - disband_matrix$d3[7,2] + new_units$d3[7,2]
    forces_selected$d3[8,1] <- round3_start()[8,1] - disband_matrix$d3[8,1] + new_units$d3[8,1]
    forces_selected$d3[8,2] <- round3_start()[8,2] - disband_matrix$d3[8,2] + new_units$d3[8,2]
  })
  
  # pie charts for force structure and AC/RC
  
  force_pie_function <- function(df){
    
    if (sum(df) <= 0 ){
      temp_df <- data.frame(Type = "Empty", Totals = 1)
    } else {
      temp <- NULL; temp2 <- 1; 
      temp3 <- rownames(df)
      temp4 <- NULL
      
      for (i in 1:8){
        if (df[i,1] + df[i,2] > 0){
          temp[temp2] = df[i,1] + df[i,2]
          temp4[temp2] <- temp3[i]
          temp2 <- temp2+1
        }
      }
      
      temp_df <- data.frame(Type = temp4, Totals = temp)
    }
    
    temp_df %>% plot_ly(labels = ~Type, values = ~Totals, textinfo = "label") %>%
      add_pie(hole = 0.4, inherit = TRUE) %>%
      layout(title = "Force Structure", showlegend = F)

  }
    
  AC_RC_pie_function <- function(df){
    
    if (sum(df) <=0){ 
      temp_df <- data.frame(Type = "Empty", Totals = 1)
    } else { 
      temp_df <- data.frame(
        Type = c("AC", "RC"),
        Totals = c(sum(df[,1]), sum(df[,2]))
      )
    }
    
    temp_df %>% plot_ly(labels = ~Type, values = ~Totals, textinfo = "label") %>%
      add_pie(hole = 0.4, inherit = TRUE) %>%
      layout(title = "AC/RC Composition", showlegend = F)
  }
  
  output$force_pie <- renderPlotly({     force_pie_function(forces_selected$d1)  })
  output$ACRC_pie <- renderPlotly({     AC_RC_pie_function(forces_selected$d1)  })
  
  output$tw_force_pie <- renderPlotly({     force_pie_function(forces_selected$d2)  })
  output$tw_ACRC_pie <- renderPlotly({    AC_RC_pie_function(forces_selected$d2)  })
  
  output$th_force_pie <- renderPlotly({    force_pie_function(forces_selected$d3)  })
  output$th_ACRC_pie <- renderPlotly({     AC_RC_pie_function(forces_selected$d3)  })
  
  # round costs and cost sidebar tables
  
  forces_cost2 <- reactive ({
    sidebar_costs(forces_selected$d1, forces_cost$d1, input$milcon_fund, input$milpers_fund, input$om_fund, input$proc_fund)
  })
  output$forces_cost <- renderTable(forces_cost2(), rownames=T, digits=1)
  
  tw_forces_cost2 <- reactive ({
    temp1 <- sidebar_costs(new_units$d2, forces_cost$d2, input$tw_milcon_fund, input$tw_milpers_fund, input$tw_om_fund, input$tw_proc_fund)
    temp2 <- sidebar_costs(disband_matrix$d2, forces_cost$d2, input$tw_milcon_fund, input$tw_milpers_fund, input$tw_om_fund, input$tw_proc_fund)
    temp2 <- temp2*0.5
    temp2[1,1] <- 0
    temp2[1,2] <- 0
    temp4 <- sidebar_costs(round2_start(), forces_cost$d2, input$tw_milcon_fund, input$tw_milpers_fund, input$tw_om_fund, input$tw_proc_fund)
    temp4[1,1] <- 0
    temp4[1,2] <- 0
    temp3 <- (temp1 + temp4) - temp2
    temp3[6,1] <- sum(temp3[1:5,1])
    temp3[6,2] <- sum(temp3[1:5,2])
    temp3
  })
  tw_forces_cost_100 <- reactive ({
    temp1 <- sidebar_costs(round2_start()+new_units$d2, forces_cost$d2, 100, 100, 100, 100)
    temp2 <- sidebar_costs(disband_matrix$d2, forces_cost$d2, 100, 100, 100, 100)
    temp2 <- temp2*0.5
    temp2[1,1] <- 0
    temp2[1,2] <- 0
    temp3 <- temp1 - temp2
    temp3[6,1] <- sum(temp3[1:5,1])
    temp3[6,2] <- sum(temp3[1:5,2])
    temp3
  })
  output$tw_forces_cost <- renderTable(tw_forces_cost2(), rownames=T, digits=1)

  th_forces_cost2 <- reactive ({
    temp1 <- sidebar_costs(new_units$d3, forces_cost$d3, input$th_milcon_fund, input$th_milpers_fund, input$th_om_fund, input$th_proc_fund)
    temp2 <- sidebar_costs(disband_matrix$d3, forces_cost$d3, input$th_milcon_fund, input$th_milpers_fund, input$th_om_fund, input$th_proc_fund)
    temp2 <- temp2*0.5
    temp2[1,1] <- 0
    temp2[1,2] <- 0
    temp4 <- sidebar_costs(round3_start(), forces_cost$d3, input$th_milcon_fund, input$th_milpers_fund, input$th_om_fund, input$th_proc_fund)
    temp4[1,1] <- 0
    temp4[1,2] <- 0
    temp3 <- (temp1 + temp4) - temp2
    temp3[6,1] <- sum(temp3[1:5,1])
    temp3[6,2] <- sum(temp3[1:5,2])
    temp3
  })
  th_forces_cost_100 <- reactive ({
    temp1 <- sidebar_costs(round3_start()+new_units$d3, forces_cost$d3, 100, 100, 100, 100)
    temp2 <- sidebar_costs(disband_matrix$d3, forces_cost$d3, 100, 100, 100, 100)
    temp2 <- temp2*0.5
    temp2[1,1] <- 0
    temp2[1,2] <- 0
    temp3 <- temp1 - temp2
    temp3[6,1] <- sum(temp3[1:5,1])
    temp3[6,2] <- sum(temp3[1:5,2])
    temp3
  })
  output$th_forces_cost <- renderTable(th_forces_cost2(), rownames=T, digits=1)
  
  
  forces_cost <- reactiveValues(
    d1 = matrix(1:12,nrow=6, dimnames = list(c("Force Structure","MILCON","MILPERS","O&M","Procurement","Total"),c("AC","RC"))),
    d2 = matrix(1:12,nrow=6, dimnames = list(c("Force Structure","MILCON","MILPERS","O&M","Procurement","Total"),c("AC","RC"))),
    d3 = matrix(1:12,nrow=6, dimnames = list(c("Force Structure","MILCON","MILPERS","O&M","Procurement","Total"),c("AC","RC")))
  )
  
  sidebar_costs <- function(selected, sidebar, milcon, milpers, om, proc){
    temp <- t(selected) %*% forces_cost_table
    sidebar[1,1] <- temp[1,1]
    sidebar[1,2] <- temp[2,1]
    sidebar[2,1] <- temp[1,2] * (milcon/100)
    sidebar[2,2] <- temp[2,2] * 0.3 * (milcon/100)
    sidebar[3,1] <- temp[1,3] * (milpers/100)
    sidebar[3,2] <- temp[2,3] * 0.3 * (milpers/100)
    sidebar[4,1] <- temp[1,4] * (om/100)
    sidebar[4,2] <- temp[2,4] * 0.3 * (om/100)
    sidebar[5,1] <- temp[1,5] * (proc/100)
    sidebar[5,2] <- temp[2,5] * 0.3 * (proc/100)
    sidebar[6,1] <- sum(sidebar[1:5,1])
    sidebar[6,2] <- sum(sidebar[1:5,2])
    sidebar
  }
  
  # determining extra art offense, avi defense
  
  art_support <- reactive({ paired(forces_selected$d1,5) })
  avi_support <- reactive({ paired(forces_selected$d1,4) })
  
  tw_art_support <- reactive({ paired(forces_selected$d2,5) })
  tw_avi_support <- reactive({ paired(forces_selected$d2,4) })
  
  th_art_support <- reactive({ paired(forces_selected$d3,5) })
  th_avi_support <- reactive({ paired(forces_selected$d3,4) })
  
  paired <- function(forces, type){
    if ( (sum(forces[type,1:2]) > 0 ) & (sum(forces[1:3,1:2]) > 0) ){
      if (sum(forces[1:3,1:2]) > sum(forces[type,1:2])){
        sum(forces[type,1:2])
      } else {sum(forces[1:3,1:2])}
    } else {0}
  }

  # phase 2: actual capabilities and crisis outcomes
  
  # r&d die rolls
  
  observeEvent(input$rd_button1,{ output$rd_die_roll1 <- renderText({ paste("You rolled a ", die_rolls[1,4]) }) })
  observeEvent(input$rd_button2,{ output$rd_die_roll2 <- renderText({ paste("You rolled a ", die_rolls[1,5]) }) })
  
  observeEvent(input$tw_rd_button1,{ output$tw_rd_die_roll1 <- renderText({ paste("You rolled a ", die_rolls[3,4]) }) })
  observeEvent(input$tw_rd_button2,{ output$tw_rd_die_roll2 <- renderText({ paste("You rolled a ", die_rolls[3,5]) }) })
  
  observeEvent(input$th_rd_button1,{ output$th_rd_die_roll1 <- renderText({ paste("You rolled a ", die_rolls[5,4]) }) })
  observeEvent(input$th_rd_button2,{ output$th_rd_die_roll2 <- renderText({ paste("You rolled a ", die_rolls[5,5]) }) })
  
  # readiness dice rolls
  
  observeEvent(input$read_button,{ output$readiness_die_roll <- renderText({ paste("You rolled a ", die_rolls[2,5]) }) })
  observeEvent(input$read_button2,{ output$readiness2_die_roll <- renderText({ paste("You rolled a ", die_rolls[2,13]) }) })
  observeEvent(input$milcon_button,{ output$milcon_die_roll <- renderText({ paste("You rolled a ", die_rolls[2,7]) }) })
  observeEvent(input$milpers_button,{ output$milpers_die_roll <- renderText({ paste("You rolled a ", die_rolls[2,8]) }) })
  observeEvent(input$om_button,{ output$om_die_roll <- renderText({ paste("You rolled a ", die_rolls[2,9]) }) })
  observeEvent(input$proc_button,{ output$proc_die_roll <- renderText({ paste("You rolled a ", die_rolls[2,10]) }) })
  
  observeEvent(input$tw_read_button,{ output$tw_readiness_die_roll <- renderText({ paste("You rolled a ", die_rolls[4,5]) }) })
  observeEvent(input$tw_read_button2,{ output$tw_readiness2_die_roll <- renderText({ paste("You rolled a ", die_rolls[4,13]) }) })
  observeEvent(input$tw_milcon_button,{ output$tw_milcon_die_roll <- renderText({ paste("You rolled a ", die_rolls[4,7]) }) })
  observeEvent(input$tw_milpers_button,{ output$tw_milpers_die_roll <- renderText({ paste("You rolled a ", die_rolls[4,8]) }) })
  observeEvent(input$tw_om_button,{ output$tw_om_die_roll <- renderText({ paste("You rolled a ", die_rolls[4,9]) }) })
  observeEvent(input$tw_proc_button,{ output$tw_proc_die_roll <- renderText({ paste("You rolled a ", die_rolls[4,10]) }) })
  
  observeEvent(input$th_read_button,{ output$th_readiness_die_roll <- renderText({ paste("You rolled a ", die_rolls[6,5]) }) })
  observeEvent(input$th_read_button2,{ output$th_readiness2_die_roll <- renderText({ paste("You rolled a ", die_rolls[6,13]) }) })
  observeEvent(input$th_milcon_button,{ output$th_milcon_die_roll <- renderText({ paste("You rolled a ", die_rolls[6,7]) }) })
  observeEvent(input$th_milpers_button,{ output$th_milpers_die_roll <- renderText({ paste("You rolled a ", die_rolls[6,8]) }) })
  observeEvent(input$th_om_button,{ output$th_om_die_roll <- renderText({ paste("You rolled a ", die_rolls[6,9]) }) })
  observeEvent(input$th_proc_button,{ output$th_proc_die_roll <- renderText({ paste("You rolled a ", die_rolls[6,10]) }) })
  
  # force upgrades dice roll
  
  observeEvent(input$upgrades_button,{ output$upgrades_die_roll <- renderText({ paste("You rolled a ", die_rolls[2,6]) }) })
  
  observeEvent(input$tw_upgrades_button,{ output$tw_upgrades_die_roll <- renderText({ paste("You rolled a ", die_rolls[4,6]) }) })
  
  observeEvent(input$th_upgrades_button,{ output$th_upgrades_die_roll <- renderText({ paste("You rolled a ", die_rolls[6,6]) }) })
  
  # RC capability factor dice roll
  
  observeEvent(input$RC_factor,{ output$RC_die_roll <- renderText({ paste("You rolled a ", die_rolls[2,4]) }) })
  
  observeEvent(input$tw_RC_factor,{ output$tw_RC_die_roll <- renderText({ paste("You rolled a ", die_rolls[4,4]) }) })
  
  observeEvent(input$th_RC_factor,{ output$th_RC_die_roll <- renderText({ paste("You rolled a ", die_rolls[6,4]) }) })
  
  RC_factor_percent <- reactive({ 0.5+(req(input$RC_factor_die_roll-2))*0.05 })
  output$RC_factor2 <- renderText({ paste("Your RC capability is ", RC_factor_percent()*100,"%", sep="") })
  
  tw_RC_factor_percent <- reactive({ 0.5+(req(input$tw_RC_factor_die_roll-2))*0.05 })
  output$tw_RC_factor2 <- renderText({ paste("Your RC capability is ", tw_RC_factor_percent()*100,"%", sep="") })
  
  th_RC_factor_percent <- reactive({ 0.5+(req(input$th_RC_factor_die_roll-2))*0.05 })
  output$th_RC_factor2 <- renderText({ paste("Your RC capability is ", th_RC_factor_percent()*100,"%", sep="") })
  
  # crisis die roll, requirements table, reactive table
  
  observeEvent(input$crisis_button,{ output$crisis_die_roll <-renderText({ paste("You rolled a  ", die_rolls[2,11]) }) }) 
  
  observeEvent(input$tw_crisis_button,{ output$tw_crisis_die_roll <-renderText({ paste("You rolled a  ", die_rolls[4,11]) }) }) 
  
  observeEvent(input$th_crisis_button,{ output$th_crisis_die_roll <-renderText({ paste("You rolled a  ", die_rolls[6,11]) }) }) 
  
  # crisis requirements and table output
  
  crisis_reqs2 <- reactiveValues(
    d1 = matrix(0, nrow=6),
    d2 = matrix(0, nrow=6),
    d3 = matrix(0, nrow=6)
  )
  
  crisis_reqs3 <- reactive({select_crisis(input$crisis, crisis_reqs2$d1)})
  output$crisis_reqs <- renderTable({
    if (input$crisis == "None") {NULL
    } else { crisis_reqs3() } }, rownames=T, bordered = TRUE, striped = TRUE)
  
  tw_crisis_reqs3 <- reactive({select_crisis(input$tw_crisis, crisis_reqs2$d2)})
  output$tw_crisis_reqs <- renderTable({
    if (input$tw_crisis == "None") {NULL
    } else { tw_crisis_reqs3() } }, rownames=T, bordered = TRUE, striped = TRUE)
  
  th_crisis_reqs3 <- reactive({select_crisis(input$th_crisis, crisis_reqs2$d3)})
  output$th_crisis_reqs <- renderTable({
    if (input$th_crisis == "None") {NULL
    } else { th_crisis_reqs3() } }, rownames=T, bordered = TRUE, striped = TRUE)
  
  select_crisis <- function(crisis, crisis_matrix){
    if(!(crisis == "None")){
      temp1 <- 1 + round2()*5 + round3()*10
      temp2 <- temp1 + 4
      temp <- as.matrix(crisis_reqs[c(temp1:temp2,16), crisis], nrow=6)
      colnames(crisis_matrix) <- "Crisis Requirements"
      rownames(crisis_matrix) <- c("Offense","Defense","Additional enablers","Technology level","Total # of priority forces required","Priority forces")
      crisis_matrix[1:6,] <- temp[1:6,]
      crisis_matrix
    } else {
      crisis_matrix[1:6,] <- rep(0,6)
      crisis_matrix
    }
  }
  
  # technology factor calculation

  tech_factor <- reactive({ tech_factor_calc(req(crisis_reqs3()), tech_level()) })
  
  tw_tech_factor <- reactive({ tech_factor_calc(req(tw_crisis_reqs3()), tw_tech_level()) })
  
  th_tech_factor <- reactive({ tech_factor_calc(req(th_crisis_reqs3()), th_tech_level()) })
  
  tech_factor_calc <- function(crisis_table, tech_level){
    temp <- as.numeric(crisis_table[4,])
    if (tech_level == temp ){
      1
    } else if (tech_level > temp){
      if (tech_level - temp == 1){1.15
      } else if (tech_level - temp == 2){1.25
      } else {1.33}
    } else{
      if (tech_level - temp == -1){0.85
      } else if (tech_level - temp == -2){0.75
      } else {0.66}
    }
  }
  
  # priority forces requirement check per crisis; value is 1 or 0.5
  
  priority_forces <- reactive({ priority_forces_check(crisis_reqs3(), input$crisis, forces_selected$d1) })
  output$meet_priority <- renderText({ paste("Met priority force requirements?", priority_forces_check2(priority_forces())) })
  
  tw_priority_forces <- reactive({ priority_forces_check(tw_crisis_reqs3(), input$tw_crisis, forces_selected$d2) })
  output$tw_meet_priority <- renderText({ paste("Met priority force requirements?", priority_forces_check2(tw_priority_forces())) })
  
  th_priority_forces <- reactive({ priority_forces_check(th_crisis_reqs3(), input$th_crisis, forces_selected$d3) })
  output$th_meet_priority <- renderText({ paste("Met priority force requirements?", priority_forces_check2(th_priority_forces())) })
    
  priority_forces_check <- function(crisis_req, crisis, forces){
    temp <- as.numeric(crisis_req[5,])
    if (crisis == "steady"){
      if (sum(forces[7,1:2]) > 0 & sum(forces[1,1:2]) > 0 & (sum(forces[1,1:2])+sum(forces[7,1:2])) >= temp){
        1 } else {0.5}
    } else if (crisis == "human"){
      if (sum(forces[4,1:2]) > 0 & sum(forces[1,1:2]) > 0 & (sum(forces[1,1:2])+sum(forces[4,1:2])) >= temp){
        1 } else {0.5}
    } else if (crisis == "rogue"){
      if (sum(forces[7,1:2]) > 0 & sum(forces[6,1:2]) > 0 & (sum(forces[7,1:2])+sum(forces[6,1:2])) >= temp){
        1 } else {0.5}
    } else if (crisis == "isis"){
      if (sum(forces[4,1:2]) > 0 & sum(forces[6,1:2]) > 0 & (sum(forces[6,1:2])+sum(forces[4,1:2])) >= temp){
        1 } else {0.5}
    } else if (crisis == "defense"){
      if (sum(forces[3,1:2]) > 0 & sum(forces[7,1:2]) > 0 & (sum(forces[3,1:2])+sum(forces[7,1:2])) >= temp){
        1 } else {0.5}
    } else if (crisis == "offense"){
      if (sum(forces[2,1:2]) > 0 & sum(forces[5,1:2]) > 0 & (sum(forces[2,1:2])+sum(forces[5,1:2])) >= temp){
        1 } else {0.5}
    } else {1}
  }
  
  priority_forces_check2 <- function(check){
    if (check == 0.5){"No"} else {"Yes"}
  }
  
  # sustainment calculations
  
  enabler_capability <- reactive({ req(forces_selected$d1[8,1])*10 + req(forces_selected$d1[8,2])*10*req(RC_factor_percent()) })
  enabler_requirement <- reactive({
    enabler_req(crisis_reqs3(), req(forces_selected$d1), req(readiness_info2()), req(RC_factor_percent()) )
  })
  sustainment_factor <- reactive({ sustainment_factor_calc(enabler_requirement(), enabler_capability()) })
  
  output$ena_cap <- renderText({ paste("Current enabler capabilities: ", format(enabler_capability(), digits=3)) })
  output$ena_req <- renderText({ paste("Current enabler requirements: ", format(enabler_requirement(), digits=3)) })
  output$sustainment_percent <- renderText({ paste("Current % sustainment needs met: ", format(100*sustainment_factor(), digits=3), "%", sep="") })
  
  tw_enabler_capability <- reactive({ req(forces_selected$d2[8,1])*10 + req(forces_selected$d2[8,2])*10*req(tw_RC_factor_percent()) })
  tw_enabler_requirement <- reactive({
    enabler_req(tw_crisis_reqs3(), req(forces_selected$d2), req(tw_readiness_info2()), req(tw_RC_factor_percent()) )
  })
  tw_sustainment_factor <- reactive({ sustainment_factor_calc(tw_enabler_requirement(), tw_enabler_capability()) })
  
  output$tw_ena_cap <- renderText({ paste("Current enabler capabilities: ", format(tw_enabler_capability(), digits=3)) })
  output$tw_ena_req <- renderText({ paste("Current enabler requirements: ", format(tw_enabler_requirement(), digits=3)) })
  output$tw_sustainment_percent <- renderText({ paste("Current % sustainment needs met: ", format(100*tw_sustainment_factor(), digits=3), "%", sep="") })

  th_enabler_capability <- reactive({ req(forces_selected$d3[8,1])*10 + req(forces_selected$d3[8,2])*10*req(th_RC_factor_percent()) })
  th_enabler_requirement <- reactive({
    enabler_req(th_crisis_reqs3(), req(forces_selected$d3), req(th_readiness_info2()), req(th_RC_factor_percent()) )
  })
  th_sustainment_factor <- reactive({ sustainment_factor_calc(th_enabler_requirement(), th_enabler_capability()) })
  
  output$th_ena_cap <- renderText({ paste("Current enabler capabilities: ", format(th_enabler_capability(), digits=3)) })
  output$th_ena_req <- renderText({ paste("Current enabler requirements: ", format(th_enabler_requirement(), digits=3)) })
  output$th_sustainment_percent <- renderText({ paste("Current % sustainment needs met: ", format(100*th_sustainment_factor(), digits=3), "%", sep="") })
    
  enabler_req <- function(crisis_reqs, forces, readiness, RC){
    as.numeric(crisis_reqs[3,]) +
    ( t(forces[1:7,1]) %*% readiness[1:7,5] ) +
    RC*(t(forces[1:7,2]) %*% readiness[1:7,5])
  }
  
  sustainment_factor_calc <- function(req,cap){
    if (!(req==0)){
      if ((cap/req)>1){1
        } else {cap/req}
    } else {1}
  }
  
  # offense/defense calculations
  
  offense <- reactive({
    off_def_calc(priority_forces(), tech_factor(), sustainment_factor(), overall_readiness2(), forces_selected$d1, readiness_info2(), RC_factor_percent(), art_support(),3)
  })
  output$offense <- renderText({
    if (is.na(offense())){"Current net offense estimate: 0"
    } else { paste("Current net offense estimate: ", format(offense(), digits=1)) }
  })
  defense <- reactive({
    off_def_calc(priority_forces(), tech_factor(), sustainment_factor(), overall_readiness2(), forces_selected$d1, readiness_info2(), RC_factor_percent(), avi_support(),4)
  })
  output$defense <- renderText({
    if (is.na(defense())){"Current net defense estimate: 0"
    } else { paste("Current net defense estimate: ", format(defense(), digits=1)) }
  })
  
  tw_offense <- reactive({
    off_def_calc(tw_priority_forces(), tw_tech_factor(), tw_sustainment_factor(), tw_overall_readiness2(), forces_selected$d2, tw_readiness_info2(), tw_RC_factor_percent(), tw_art_support(),3)
  })
  output$tw_offense <- renderText({
    if (is.na(tw_offense())){"Current net offense estimate: 0"
    } else { paste("Current net offense estimate: ", format(tw_offense(), digits=1)) }
  })
  tw_defense <- reactive({
    off_def_calc(tw_priority_forces(), tw_tech_factor(), tw_sustainment_factor(), tw_overall_readiness2(), forces_selected$d2, tw_readiness_info2(), tw_RC_factor_percent(), tw_avi_support(),4)
  })
  output$tw_defense <- renderText({
    if (is.na(tw_defense())){"Current net defense estimate: 0"
    } else { paste("Current net defense estimate: ", format(tw_defense(), digits=1)) }
  })
  
  th_offense <- reactive({
    off_def_calc(th_priority_forces(), th_tech_factor(), th_sustainment_factor(), th_overall_readiness2(), forces_selected$d3, th_readiness_info2(), th_RC_factor_percent(), th_art_support(),3)
  })
  output$th_offense <- renderText({
    if (is.na(th_offense())){"Current net offense estimate: 0"
    } else { paste("Current net offense estimate: ", format(th_offense(), digits=1)) }
  })
  th_defense <- reactive({
    off_def_calc(th_priority_forces(), th_tech_factor(), th_sustainment_factor(), th_overall_readiness2(), forces_selected$d3, th_readiness_info2(), th_RC_factor_percent(), th_avi_support(),4)
  })
  output$th_defense <- renderText({
    if (is.na(th_defense())){"Current net defense estimate: 0"
    } else { paste("Current net defense estimate: ", format(th_defense(), digits=1)) }
  })
  
  off_def_calc <- function (priority, tech, sustainment, overall_read, forces, readiness, RC, extra, num){
    priority*
      (tech*sustainment*(overall_read/100)*
         ( (t(forces[1:7,1]) %*% readiness[1:7,num]) + RC*(t(forces[1:7,2]) %*% readiness[1:7,num]) + extra )
      )
  }
  
  # power ratio calculation
  
  power_ratio <- reactive({power_ratio_calc(offense(), defense(), crisis_reqs3())})
  output$power_ratio <- renderText({
    if (is.na(power_ratio())){
      paste("Current power ratio estimate: 0")
    } else { paste("Current power ratio estimate: ", format(power_ratio(), digits=2)) }
  })
  
  tw_power_ratio <- reactive({power_ratio_calc(tw_offense(), tw_defense(), tw_crisis_reqs3())})
  output$tw_power_ratio <- renderText({
    if (is.na(tw_power_ratio())){
      paste("Current power ratio estimate: 0")
    } else { paste("Current power ratio estimate: ", format(tw_power_ratio(), digits=2)) }
  })
  
  th_power_ratio <- reactive({power_ratio_calc(th_offense(), th_defense(), th_crisis_reqs3())})
  output$th_power_ratio <- renderText({
    if (is.na(th_power_ratio())){
      paste("Current power ratio estimate: 0")
    } else { paste("Current power ratio estimate: ", format(th_power_ratio(), digits=2)) }
  })
  
  power_ratio_calc <- function(off, def, crisis){
    temp1 <- as.numeric(crisis[1,])
    temp2 <- as.numeric(crisis[2,])
    if(is.na((off+def)>=(temp1+temp2))) {0
    } else{
        if((off+def)>=(temp1+temp2)){1
        } else if ((off+def)<(temp1+temp2)){
          (off+def)/(temp1+temp2)
        } else {0}
    }
  }
  
  # minimum crisis roll
  
  min_crisis_roll <- reactive({ min_crisis_roll_calc(power_ratio())})
  output$min_crisis_roll <- renderText({paste("Your minimum dice roll to win the crisis is ", min_crisis_roll()) })

  tw_min_crisis_roll <- reactive({ min_crisis_roll_calc(tw_power_ratio())})
  output$tw_min_crisis_roll <- renderText({paste("Your minimum dice roll to win the crisis is ", tw_min_crisis_roll()) })
  
  th_min_crisis_roll <- reactive({ min_crisis_roll_calc(th_power_ratio())})
  output$th_min_crisis_roll <- renderText({paste("Your minimum dice roll to win the crisis is ", th_min_crisis_roll()) })
    
  min_crisis_roll_calc <- function(power_ratio){
    if (power_ratio < 0.04){12
    } else if (power_ratio < 0.17){11
    } else if (power_ratio < 0.28 ){10
    } else if (power_ratio < 0.42){9
    } else if (power_ratio < 0.58){8
    } else if (power_ratio < 0.72){7
    } else if (power_ratio < 0.83){6
    } else if (power_ratio < 0.92){5
    } else if (power_ratio < 0.97){4
    } else {3}
  }
  
  # crisis outcome die roll
  
  observeEvent(input$crisis_outcome_button,{
    output$crisis_outcome_die_roll <-renderText({
      paste("You rolled a  ", die_rolls[2,12])
    })
  })
  
  observeEvent(input$tw_crisis_outcome_button,{
    output$tw_crisis_outcome_die_roll <-renderText({
      paste("You rolled a  ", die_rolls[4,12])
    })
  })
  
  observeEvent(input$th_crisis_outcome_button,{
    output$th_crisis_outcome_die_roll <-renderText({
      paste("You rolled a  ", die_rolls[6,12])
    })
  })
  
  # crisis failure points
  
  crisis_failure <- reactive({crisis_failure_calc(req(input$crisis_outcome_die_roll), min_crisis_roll())})
  output$crisis_failure <- renderText({
    if (is.na(crisis_failure())){
      0} else {crisis_failure()} 
  })
  
  tw_crisis_failure <- reactive({crisis_failure_calc(req(input$tw_crisis_outcome_die_roll), tw_min_crisis_roll())})
  output$tw_crisis_failure <- renderText({
    if (is.na(tw_crisis_failure())){
      0} else {tw_crisis_failure()} 
  })
  
  th_crisis_failure <- reactive({crisis_failure_calc(req(input$th_crisis_outcome_die_roll), th_min_crisis_roll())})
  output$th_crisis_failure <- renderText({
    if (is.na(th_crisis_failure())){
      0} else {th_crisis_failure()} 
  })
  
  crisis_failure_calc <- function(crisis_roll, crisis_min){
    if (crisis_roll < crisis_min){
      3 * (crisis_min - crisis_roll)
    } else {0}
  }
  
  # readiness failure points
  
  read_failure <- reactive({readiness_failure_calc(input$milcon_fund, req(input$milcon_roll), input$milpers_fund, req(input$milpers_roll), input$om_fund, req(input$om_roll), input$proc_fund, req(input$proc_roll))})
  output$read_failure <- renderText({read_failure()})
  
  tw_read_failure <- reactive({readiness_failure_calc(input$tw_milcon_fund, req(input$tw_milcon_roll), input$tw_milpers_fund, req(input$tw_milpers_roll), input$tw_om_fund, req(input$tw_om_roll), input$tw_proc_fund, req(input$tw_proc_roll))})
  output$tw_read_failure <- renderText({tw_read_failure()})
  
  th_read_failure <- reactive({readiness_failure_calc(input$th_milcon_fund, req(input$th_milcon_roll), input$th_milpers_fund, req(input$th_milpers_roll), input$th_om_fund, req(input$th_om_roll), input$th_proc_fund, req(input$th_proc_roll))})
  output$th_read_failure <- renderText({th_read_failure()})
  
  readiness_failure_calc <- function(milcon_fund, milcon_roll, milpers_fund, milpers_roll, om_fund, om_roll, proc_fund, proc_roll){
    temp1 <- 0; temp2 <- 0; temp3 <- 0; temp4 <- 0
    if (read_min_roll(milcon_fund) > milcon_roll){temp1 <- read_min_roll(milcon_fund) - milcon_roll}
    if (read_min_roll(milpers_fund) > milpers_roll){temp2 <- read_min_roll(milpers_fund) - milpers_roll}
    if (read_min_roll(om_fund) > om_roll){temp3 <- read_min_roll(om_fund) - om_roll}
    if (read_min_roll(proc_fund) > proc_roll){temp4 <- read_min_roll(proc_fund) - proc_roll}
    temp1+temp2+temp3+temp4
  }
  
  # value of units to lose
  
  units_lost <- reactive({ units_lost_calc(req(crisis_reqs3()), req(min_crisis_roll()), req(input$crisis_outcome_die_roll), req(power_ratio()), req(defense())) })

  tw_units_lost <- reactive({ units_lost_calc(req(tw_crisis_reqs3()), req(tw_min_crisis_roll()), req(input$tw_crisis_outcome_die_roll), req(tw_power_ratio()), req(tw_defense())) })  
  
  th_units_lost <- reactive({ units_lost_calc(req(th_crisis_reqs3()), req(th_min_crisis_roll()), req(input$th_crisis_outcome_die_roll), req(th_power_ratio()), req(th_defense())) })  
  
  units_lost_calc <- function(crisis, crisis_min, crisis_roll, power, defense){
    temp1 <- as.numeric(crisis[1,])
    temp2 <- as.numeric(crisis[2,])
    penalty <- if (temp1 - defense < 0){0} else {temp1-defense}
    if (crisis_min > crisis_roll){
      if (power < 0.08){(0.97 * (temp1 + temp2)) + penalty
      } else if (power < 0.17){(0.92 * (temp1 + temp2)) + penalty
      } else if (power < 0.28){(0.83 * (temp1 + temp2)) + penalty
      } else if (power < 0.42){(0.72 * (temp1 + temp2)) + penalty
      } else if (power < 0.58){(0.58 * (temp1 + temp2)) + penalty
      } else if (power < 0.72){(0.42 * (temp1 + temp2)) + penalty
      } else if (power < 0.83){(0.28 * (temp1 + temp2)) + penalty
      } else if (power < 0.92){(0.17 * (temp1 + temp2)) + penalty
      } else if (power < 0.97){(0.08 * (temp1 + temp2)) + penalty
      } else {(0.03 * (temp1 + temp2)) + penalty}
    } else{
      if (power < 0.08){0.97 * (temp1 + temp2)
      } else if (power < 0.17){0.92 * (temp1 + temp2)
      } else if (power < 0.28){0.83 * (temp1 + temp2)
      } else if (power < 0.42){0.72 * (temp1 + temp2)
      } else if (power < 0.58){0.58 * (temp1 + temp2)
      } else if (power < 0.72){0.42 * (temp1 + temp2)
      } else if (power < 0.83){0.28 * (temp1 + temp2)
      } else if (power < 0.92){0.17 * (temp1 + temp2)
      } else if (power < 0.97){0.08 * (temp1 + temp2)
      } else {0.03 * (temp1 + temp2)}
    }
  }
  
  # forces to lose
  
  output$force_losses <- renderText({
    req(units_lost())
    if (is.na(units_lost())){
      "Force losses calculation pending"
    } else if (units_lost() == 0){
      "You do not have to lose any units"  
    } else if (units_lost() <= 8){
      if (input$crisis == "steady"){
        "You do not have to lose any forces"
      } else {"You only have to lose 1 force of your choosing"}
    } else {
      paste("You have to lose a total value of ", format(units_lost(), digits=4), " in force structure")
    }
  })
  
  output$tw_force_losses <- renderText({
    req(tw_units_lost())
    if (is.na(tw_units_lost())){
      "Force losses calculation pending"
    } else if (tw_units_lost() == 0){
      "You do not have to lose any units"  
    } else if (tw_units_lost() <= 8){
      if (input$tw_crisis == "steady"){
        "You do not have to lose any forces"
      } else {"You only have to lose 1 force of your choosing"}
    } else {
      paste("You have to lose a total value of ", format(tw_units_lost(), digits=4), " in force structure")
    }
  })
  
  output$th_force_losses <- renderText({
    req(th_units_lost())
    if (is.na(th_units_lost())){
      "Force losses calculation pending"
    } else if (th_units_lost() == 0){
      "You did not lose any units this final round"  
    } else if (th_units_lost() <= 8){
      if (input$th_crisis == "steady"){
        "You did not lose any units this final round"  
      } else {"You lost 1 force"}
    } else {
      paste("You lost a total value of ", format(th_units_lost(), digits=4), " in force structure")
    }
  })
  
  # 3 columns: current forces | forces to lose inputs | forces into next round
  
  # current forces
  
  output$forces_selected <- renderTable({
    format(forces_selected$d1, digits=1)}, rownames=T, striped = T)
  
  output$tw_forces_selected <- renderTable({
    format(forces_selected$d2, digits=1)}, rownames=T, striped = T)
  
  # forces to lose
  
  forces_lose <- reactiveValues(
    d1 = matrix(1:16, nrow=8),
    d2 = matrix(1:16, nrow=8)
  )
  
  observeEvent(c(input$inf_AC_lose, input$inf_RC_lose, input$arm_AC_lose, input$arm_RC_lose, input$str_AC_lose, input$str_RC_lose, input$avi_AC_lose, input$avi_RC_lose, input$art_AC_lose, input$art_RC_lose, input$so_AC_lose, input$so_RC_lose, input$ad_AC_lose, input$ad_RC_lose, input$ena_AC_lose, input$ena_RC_lose),{
    forces_lose$d1[1,1] <- req(input$inf_AC_lose) ; forces_lose$d1[1,2] <- req(input$inf_RC_lose)
    forces_lose$d1[2,1] <- req(input$arm_AC_lose) ; forces_lose$d1[2,2] <- req(input$arm_RC_lose)
    forces_lose$d1[3,1] <- req(input$str_AC_lose) ; forces_lose$d1[3,2] <- req(input$str_RC_lose)
    forces_lose$d1[4,1] <- req(input$avi_AC_lose) ; forces_lose$d1[4,2] <- req(input$avi_RC_lose)
    forces_lose$d1[5,1] <- req(input$art_AC_lose) ; forces_lose$d1[5,2] <- req(input$art_RC_lose)
    forces_lose$d1[6,1] <- req(input$so_AC_lose)  ; forces_lose$d1[6,2] <- req(input$so_RC_lose)
    forces_lose$d1[7,1] <- req(input$ad_AC_lose)  ; forces_lose$d1[7,2] <- req(input$ad_RC_lose)
    forces_lose$d1[8,1] <- req(input$ena_AC_lose) ; forces_lose$d1[8,2] <- req(input$ena_RC_lose)
  })
  
  observeEvent(c(input$tw_inf_AC_lose, input$tw_inf_RC_lose, input$tw_arm_AC_lose, input$tw_arm_RC_lose, input$tw_str_AC_lose, input$tw_str_RC_lose, input$tw_avi_AC_lose, input$tw_avi_RC_lose, input$tw_art_AC_lose, input$tw_art_RC_lose, input$tw_so_AC_lose, input$tw_so_RC_lose, input$tw_ad_AC_lose, input$tw_ad_RC_lose, input$tw_ena_AC_lose, input$tw_ena_RC_lose),{
    forces_lose$d2[1,1] <- req(input$tw_inf_AC_lose) ; forces_lose$d2[1,2] <- req(input$tw_inf_RC_lose)
    forces_lose$d2[2,1] <- req(input$tw_arm_AC_lose) ; forces_lose$d2[2,2] <- req(input$tw_arm_RC_lose)
    forces_lose$d2[3,1] <- req(input$tw_str_AC_lose) ; forces_lose$d2[3,2] <- req(input$tw_str_RC_lose)
    forces_lose$d2[4,1] <- req(input$tw_avi_AC_lose) ; forces_lose$d2[4,2] <- req(input$tw_avi_RC_lose)
    forces_lose$d2[5,1] <- req(input$tw_art_AC_lose) ; forces_lose$d2[5,2] <- req(input$tw_art_RC_lose)
    forces_lose$d2[6,1] <- req(input$tw_so_AC_lose)  ; forces_lose$d2[6,2] <- req(input$tw_so_RC_lose)
    forces_lose$d2[7,1] <- req(input$tw_ad_AC_lose)  ; forces_lose$d2[7,2] <- req(input$tw_ad_RC_lose)
    forces_lose$d2[8,1] <- req(input$tw_ena_AC_lose) ; forces_lose$d2[8,2] <- req(input$tw_ena_RC_lose)
  })
  
  # forces into next round
  
  forces_selected2 <- reactiveValues(
    d1 = matrix(1:16, nrow=8, dimnames = list(c("Infantry","Armor","Stryker","Aviation","Artillery","Special Operations","Air Defense","Enablers"),c("AC","RC"))),
    d2 = matrix(1:16, nrow=8, dimnames = list(c("Infantry","Armor","Stryker","Aviation","Artillery","Special Operations","Air Defense","Enablers"),c("AC","RC")))
  )
  
  round2_start <- reactive ({ new_round_forces(forces_selected$d1, forces_selected2$d1, req(forces_lose$d1)) })
  output$forces_selected2 <- renderTable({
    format(round2_start(), digits=1)}, rownames=T, striped = T)  
  
  round3_start <- reactive ({ new_round_forces(forces_selected$d2, forces_selected2$d2, req(forces_lose$d2)) })
  output$tw_forces_selected_rd2 <- renderTable({
    format(round3_start(), digits=1)}, rownames=T, striped = T)  
  
  new_round_forces <- function(current, new, lose){
    for(i in 1:8){
      for(j in 1:2){
        new[i,j] <- current[i,j] - lose[i,j]
      }
    }
    new
  }
  
  # forces selected to lose
  
  units_lost2 <- reactive({ units_lost2_calc(req(forces_lose$d1))})
  output$force_losses2 <- renderText({ paste("So far, you have lost ", sum(req(forces_lose$d1)), " force(s) with a total value of  ", units_lost2()) })
  
  tw_units_lost2 <- reactive({ units_lost2_calc(req(forces_lose$d2))})
  output$tw_force_losses2 <- renderText({ paste("So far, you have lost ", sum(req(forces_lose$d2)), " force(s) with a total value of ", tw_units_lost2()) })
  
  units_lost2_calc  <- function(lose){
    temp2 <- t(lose) %*% forces_cost_table[,1]
    temp2[1,] + temp2[2,]
  }
  
  # error checks
  
  error1 <- reactiveVal(0)
  error2 <- reactiveVal(0)
  error3 <- reactiveVal(0)
  error4 <- reactiveVal(0)
  error5 <- reactiveVal(0)
  
  output$no_error <- renderText({
    if ((error1()+error2()+error3()+error4() == 0) & any(is.na(NA_rd1()))==F){ "No errors detected"
    } else {NULL}
  })
  
  output$tw_no_error <- renderText({
    if ((error1()+error2()+error3()+error4()+error5() == 0) & any(is.na(NA_rd2()))==F){ "No errors detected"
    } else {NULL}
  })
  
  output$th_no_error <- renderText({
    if ((error1()+error2()+error3()+error5() == 0) & any(is.na(NA_rd3()))==F){ "No errors detected"
    } else {NULL}
  })
  
  
  #Trying to make the error box change colors for better UI 
  output$errorBox <- renderUI({
    # Determine if any errors exist
    has_error <- (
      (error1() + error2() + error3() + error4() + error5()) > 0 ||
        any(is.na(NA_rd1())) ||
        any(is.na(NA_rd2())) ||
        any(is.na(NA_rd3()))
    )
    
    div(
      class = paste("error-box", if (has_error) "red" else "green"),
      tags$strong("Error Alerts"),
      
      # Optional: show your existing outputs if you still use them
      textOutput("error1"),
      textOutput("error2"),
      textOutput("error3"),
      textOutput("error4"),
      textOutput("no_error"),
      textOutput("tw_no_error"),
      textOutput("th_no_error")
    )
  })
  
  # error1: remaining funding must be non-negative
  
  output$error1 <- renderText({error1_check(remaining())})
  
  output$tw_error1 <- renderText({error1_check(tw_remaining())})
  
  output$th_error1 <- renderText({error1_check(th_remaining())})
  
  error1_check <- function(remaining){
    if (remaining < 0){
      error1(1)
      "Error: your funding remaining must be 0 or greater"
    } else { 
      error1(0)
      NULL
    }
  }
  
  # error2: forces heading into next round cannot be negative
  
  output$error2 <- renderText({error2_check(round2_start() )})
  
  output$tw_error2 <- renderText({error2_check(round3_start() )})
  
  output$th_error2 <- renderText({error2_check(forces_selected$d3 )})
  
  error2_check <- function(new){
    if (any(new < 0)){
      error2(1)
      "Error: you have a negative amount of forces heading into the next round; adjust to 0 or greater to proceed"
    } else {
      error2(0)
      NULL
    }
  }
  
  # error3: forces not met for political event 1 (congressional interests) or political event 5 (white house priority crisis)
  
  output$error3 <- renderText({error3_check(input$political_event_1) })
  
  output$tw_error3 <- renderText({error3_check(input$tw_political_event_1) })  
  
  output$th_error3 <- renderText({error3_check(input$th_political_event_1) })  
  
  error3_check <- function(event){
    if (event == "pol1" & (pol1_error2() > pol1_error1())){
      error3(1)
      "Error: you have not yet procured the forces required by your political event; review requirements"
    } else if (event == "pol5" & (pol5_error1()+pol5_error2() < pol5_error3())){
      error3(1)
      "Error: you have not yet procured the forces required by your political event; review requirements"
    } else {
      error3(0)
      NULL
    }
  }
  
  # error4: end of round force reductions
  
  output$error4 <- renderText({error4_check(units_lost(), sum(forces_lose$d1), units_lost2(), input$crisis) })
  
  output$tw_error4 <- renderText({error4_check(tw_units_lost(), sum(forces_lose$d2), tw_units_lost2(), input$tw_crisis) })
  
  error4_check <- function(req, units, costs, crisis){
    if (!(crisis == "None") & req <= 8){
      if (!(crisis == "steady") & units < 1){
        error4(1)
        "Error: you have not sufficiently reduced your forces to finish the round"
      } else {
        error4(0)
        NULL
      }
    } else if (!(crisis == "None")){
      if (req > costs){
        error4(1)
        "Error: you have not sufficiently reduced your forces to finish the round"
      } else {
        error4(0)
        NULL
      } 
    } else {
      error4(0)
      NULL
    }
  }

  # error 5 for rounds 2 and 3: disbanding more units than one starts with
  
  output$tw_error5 <- renderText({error5_check(round2_start(), disband_matrix$d2) })
  
  output$th_error5 <- renderText({error5_check(round3_start(), disband_matrix$d3) })
  
  error5_check <- function(start, disband){
    temp <- start-disband
    if (any(temp<0)){
      error5(1)
      "Error: you disbanded more units than you started with"
    } else{
      error5(0)
      NULL
    }
  }
  
  # NA errors
  
  NA_rd1 <- reactive({ c(input$tech_die_roll1, input$tech_die_roll2, input$milcon_roll, input$milpers_roll, input$om_roll, input$proc_roll, input$RC_factor_die_roll, input$upgrades_die_roll, input$crisis_outcome_die_roll, input$inf_AC, input$inf_RC, input$arm_AC, input$arm_RC, input$str_AC, input$str_RC, input$avi_AC, input$avi_RC, input$art_AC, input$art_RC, input$so_AC, input$so_RC, input$ad_AC, input$ad_RC, input$ena_AC, input$ena_RC)
  })
  
  NA_rd2 <- reactive({ c(input$tw_tech_die_roll1, input$tw_tech_die_roll2, input$tw_milcon_roll, input$tw_milpers_roll, input$tw_om_roll, input$tw_proc_roll, input$tw_RC_factor_die_roll, input$tw_upgrades_die_roll, input$tw_crisis_outcome_die_roll, input$tw_inf_AC, input$tw_inf_RC, input$tw_arm_AC, input$tw_arm_RC, input$tw_str_AC, input$tw_str_RC, input$tw_avi_AC, input$tw_avi_RC, input$tw_art_AC, input$tw_art_RC, input$tw_so_AC, input$tw_so_RC, input$tw_ad_AC, input$tw_ad_RC, input$tw_ena_AC, input$tw_ena_RC, input$inf_AC_disband, input$inf_RC_disband, input$arm_AC_disband, input$arm_RC_disband, input$str_AC_disband, input$str_RC_disband, input$avi_AC_disband, input$avi_RC_disband, input$art_AC_disband, input$art_RC_disband, input$so_AC_disband, input$so_RC_disband, input$ad_AC_disband, input$ad_RC_disband, input$ena_AC_disband, input$ena_RC_disband)
  })
  
  NA_rd3 <- reactive({ c(input$th_tech_die_roll1, input$th_tech_die_roll2, input$th_milcon_roll, input$th_milpers_roll, input$th_om_roll, input$th_proc_roll, input$th_RC_factor_die_roll, input$th_upgrades_die_roll, input$th_crisis_outcome_die_roll, input$th_inf_AC, input$th_inf_RC, input$th_arm_AC, input$th_arm_RC, input$th_str_AC, input$th_str_RC, input$th_avi_AC, input$th_avi_RC, input$th_art_AC, input$th_art_RC, input$th_so_AC, input$th_so_RC, input$th_ad_AC, input$th_ad_RC, input$th_ena_AC_disband, input$th_ena_RC_disband, input$th_inf_AC_disband, input$th_inf_RC_disband, input$th_arm_AC_disband, input$th_arm_RC_disband, input$th_str_AC_disband, input$th_str_RC_disband, input$th_avi_AC_disband, input$th_avi_RC_disband, input$th_art_AC_disband, input$th_art_RC_disband, input$th_so_AC_disband, input$th_so_RC_disband, input$th_ad_AC_disband, input$th_ad_RC_disband, input$th_ena_AC_disband, input$th_ena_RC_disband)
  })

  output$NA_error <- renderText({
    if (any(is.na(NA_rd1()))){
        "Error: input box is blank somewhere; use 0 or enter positive number"  
      } else {NULL}
  })
  
  output$tw_NA_error <- renderText({
    if (any(is.na(NA_rd2()))){
      "Error: input box is blank somewhere; use 0 or enter positive number"  
    } else {NULL}
  })
  
  output$th_NA_error <- renderText({
    if (any(is.na(NA_rd3()))){
      "Error: input box is blank somewhere; use 0 or enter positive number"  
    } else {NULL}
  })
      
  # end of round: save data and move on to next round
  
  output$end_round1 <- renderUI({
    if ((error1()+error2()+error3()+error4() == 0) & any(is.na(NA_rd1()))==F){
      div(
        actionButton("end_round1", "Click here to finish round 1 and save round 1 data"),
        br(), br(),
      )    
    } else {
      div(class = "notice",
        "If you see this message,  you must fix outstanding errors before completing round 1"
      )
    }
  })

  observeEvent(input$end_round1,{
    output$end_round1_2 <- renderUI({
      div(class = "notice",
        "Data saved; move to round 2"
      )
    })
  })

  output$pre_rd2_reminder <- renderUI({
    if (req(input$end_round1 == 0)){
      div(
        br(),
        div(class = "notice", "Please return to the end of round 1 and save game data before proceeding")
      )
    }
  })
  
  output$tw_end_round1 <- renderUI({
    if ((error1()+error2()+error3()+error4()+error5() == 0) & any(is.na(NA_rd2()))==F){
      div(
        actionButton("tw_end_round1", "Click here to finish round 2 and save round 2 data"),
        br(), br()
      )
    } else {
      div(class = "notice", 
        "If you see this message,  you must fix outstanding errors before completing round 2"
      )
    }
  })
  
  observeEvent(input$tw_end_round1,{
    output$tw_end_round1_2 <- renderUI({
      div(class = "notice",
        "Data saved; move to round 3"
      )
    })
  })
  
  output$pre_rd3_reminder <- renderUI({
    if (req(input$tw_end_round1 == 0)){
      div(
        br(),
        div(class = "notice", "Please return to the end of round 2 and save game data before proceeding")
        
      )
    }
  })
  
  output$th_end_round1 <- renderUI({
    if ((error1()+error2()+error3()+error5() == 0) & any(is.na(NA_rd3()))==F){
      actionButton("th_end_round1", "Click here to finish the game and show results below")
    } else {
      div(class = "notice",
        "If you see this message, please fix outstanding errors before completing the game"
      )
    }
  })
  
  # final tables: failure points for entire game, tie breakers
  
  output$failure_table <- renderTable({
    temp <- matrix(rep(0,8), nrow=2, dimnames = list(c("Readiness Failure Points","Crisis Failure Points"),c("Round 1", "Round 2", "Round 3", "Total")))    
    temp[1,1] <- req(game_data$r1[1,43])
    temp[2,1] <- req(game_data$r1[1,42])
    temp[1,2] <- req(game_data$r1[2,43])
    temp[2,2] <- req(game_data$r1[2,42])
    temp[1,3] <- req(game_data$r1[3,43])
    temp[2,3] <- req(game_data$r1[3,42])
    temp[1,4] <- sum(temp[1,1:3])
    temp[2,4] <- sum(temp[2,1:3])
    format(temp, digits=1)}, rownames=T
  )
  
  output$tie_breakers <- renderTable({
    temp <- matrix(c(
      req(game_data$r1[1,44]) + req(game_data$r1[2,44]) + req(game_data$r1[3,44]),
      mean(c(game_data$r1[1,38]+game_data$r1[1,39], game_data$r1[2,38]+game_data$r1[2,39], game_data$r1[3,38]+game_data$r1[3,39]))
      ),
      nrow=2,
      dimnames = list(c("Total game losses", "Average net power"),NULL)
    )
    format(temp, digits=1)}, rownames=T, colnames=F
  )

  # data to save
  
  upgrade_data <- function(upgrade){
    temp <- 0
    if (1 %in% upgrade){
      temp <- temp + 1 
    }
    if (2 %in% upgrade){
      temp <- temp + 5
    }
    if (3 %in% upgrade){
      temp <- temp + 10
    }
    temp
  }
  
  game_data <- reactiveValues(r1 = data.frame(
    name = NA, dice = NA, political_event_1 = NA, readiness_event1 = NA, readiness_event2 = NA,
    milcon_fund = NA, milpers_fund = NA, om_fund = NA, proc_fund = NA,
    inf_AC = NA, inf_RC = NA, inf_upgrades = NA,
    arm_AC = NA, arm_RC = NA, arm_upgrades = NA,
    str_AC = NA, str_RC = NA, str_upgrades = NA,
    avi_AC = NA, avi_RC = NA, avi_upgrades = NA,
    art_AC = NA, art_RC = NA, art_upgrades = NA,
    so_AC = NA, so_RC = NA, so_upgrades = NA,
    ad_AC = NA, ad_RC = NA, ad_upgrades = NA,
    ena_AC = NA, ena_RC = NA,
    RC_factor_roll = NA, remaining = NA, tech = NA, overall_read = NA, sustainment = NA,
    offense = NA, defense = NA, crisis = NA, meet_priority = NA, crisis_failure = NA,
    read_failure = NA, losses = NA, round = NA, total_failure = NA,
    class = NA
    )
  )

  observeEvent(input$end_round1,{
    game_data$r1[1,1] <- input$name
    game_data$r1[1,2] <- input$dice
    game_data$r1[1,3] <- input$political_event_1
    game_data$r1[1,4] <- input$readiness_event
    game_data$r1[1,5] <- input$readiness_event2
    game_data$r1[1,6] <- input$milcon_fund
    game_data$r1[1,7] <- input$milpers_fund
    game_data$r1[1,8] <- input$om_fund
    game_data$r1[1,9] <- input$proc_fund
    game_data$r1[1,10] <- input$inf_AC
    game_data$r1[1,11] <- input$inf_RC
    game_data$r1[1,12] <- upgrade_data(input$inf_upgrade)
    game_data$r1[1,13] <- input$arm_AC
    game_data$r1[1,14] <- input$arm_RC
    game_data$r1[1,15] <- upgrade_data(input$arm_upgrade)
    game_data$r1[1,16] <- input$str_AC
    game_data$r1[1,17] <- input$str_RC
    game_data$r1[1,18] <- upgrade_data(input$str_upgrade)
    game_data$r1[1,19] <- input$avi_AC
    game_data$r1[1,20] <- input$avi_RC
    game_data$r1[1,21] <- upgrade_data(input$avi_upgrade)
    game_data$r1[1,22] <- input$art_AC
    game_data$r1[1,23] <- input$art_RC
    game_data$r1[1,24] <- upgrade_data(input$art_upgrade)
    game_data$r1[1,25] <- input$so_AC
    game_data$r1[1,26] <- input$so_RC
    game_data$r1[1,27] <- upgrade_data(input$so_upgrade)
    game_data$r1[1,28] <- input$ad_AC
    game_data$r1[1,29] <- input$ad_RC
    game_data$r1[1,30] <- upgrade_data(input$ad_upgrade)
    game_data$r1[1,31] <- input$ena_AC
    game_data$r1[1,32] <- input$ena_RC
    game_data$r1[1,33] <- input$RC_factor_die_roll
    game_data$r1[1,34] <- remaining()
    game_data$r1[1,35] <- tech_level()
    game_data$r1[1,36] <- overall_readiness2()
    game_data$r1[1,37] <- sustainment_factor()
    game_data$r1[1,38] <- offense()
    game_data$r1[1,39] <- defense()
    game_data$r1[1,40] <- input$crisis
    game_data$r1[1,41] <- priority_forces()
    game_data$r1[1,42] <- crisis_failure()
    game_data$r1[1,43] <- read_failure()
    game_data$r1[1,44] <- units_lost()
    game_data$r1[1,45] <- 1
  })
  
  observeEvent(input$tw_end_round1,{
    game_data$r1[2,1] <- input$name
    game_data$r1[2,2] <- input$dice
    game_data$r1[2,3] <- input$tw_political_event_1
    game_data$r1[2,4] <- input$tw_readiness_event
    game_data$r1[2,5] <- input$tw_readiness_event2
    game_data$r1[2,6] <- input$tw_milcon_fund
    game_data$r1[2,7] <- input$tw_milpers_fund
    game_data$r1[2,8] <- input$tw_om_fund
    game_data$r1[2,9] <- input$tw_proc_fund
    game_data$r1[2,10] <- forces_selected$d2[1,1]
    game_data$r1[2,11] <- forces_selected$d2[1,2]
    game_data$r1[2,12] <- upgrade_data(input$tw_inf_upgrade)
    game_data$r1[2,13] <- forces_selected$d2[2,1]
    game_data$r1[2,14] <- forces_selected$d2[2,2]
    game_data$r1[2,15] <- upgrade_data(input$tw_arm_upgrade)
    game_data$r1[2,16] <- forces_selected$d2[3,1]
    game_data$r1[2,17] <- forces_selected$d2[3,2]
    game_data$r1[2,18] <- upgrade_data(input$tw_str_upgrade)
    game_data$r1[2,19] <- forces_selected$d2[4,1]
    game_data$r1[2,20] <- forces_selected$d2[4,2]
    game_data$r1[2,21] <- upgrade_data(input$tw_avi_upgrade)
    game_data$r1[2,22] <- forces_selected$d2[5,1]
    game_data$r1[2,23] <- forces_selected$d2[5,2]
    game_data$r1[2,24] <- upgrade_data(input$tw_art_upgrade)
    game_data$r1[2,25] <- forces_selected$d2[6,1]
    game_data$r1[2,26] <- forces_selected$d2[6,2]
    game_data$r1[2,27] <- upgrade_data(input$tw_so_upgrade)
    game_data$r1[2,28] <- forces_selected$d2[7,1]
    game_data$r1[2,29] <- forces_selected$d2[7,2]
    game_data$r1[2,30] <- upgrade_data(input$tw_ad_upgrade)
    game_data$r1[2,31] <- forces_selected$d2[8,1]
    game_data$r1[2,32] <- forces_selected$d2[8,2]
    game_data$r1[2,33] <- input$tw_RC_factor_die_roll
    game_data$r1[2,34] <- tw_remaining()
    game_data$r1[2,35] <- tw_tech_level()
    game_data$r1[2,36] <- tw_overall_readiness2()
    game_data$r1[2,37] <- tw_sustainment_factor()
    game_data$r1[2,38] <- tw_offense()
    game_data$r1[2,39] <- tw_defense()
    game_data$r1[2,40] <- input$tw_crisis
    game_data$r1[2,41] <- tw_priority_forces()
    game_data$r1[2,42] <- tw_crisis_failure()
    game_data$r1[2,43] <- tw_read_failure()
    game_data$r1[2,44] <- tw_units_lost()
    game_data$r1[2,45] <- 2
  })

  observeEvent(input$th_end_round1,{
    game_data$r1[3,1] <- input$name
    game_data$r1[3,2] <- input$dice
    game_data$r1[3,3] <- input$th_political_event_1
    game_data$r1[3,4] <- input$th_readiness_event
    game_data$r1[3,5] <- input$th_readiness_event2
    game_data$r1[3,6] <- input$th_milcon_fund
    game_data$r1[3,7] <- input$th_milpers_fund
    game_data$r1[3,8] <- input$th_om_fund
    game_data$r1[3,9] <- input$th_proc_fund
    game_data$r1[3,10] <- forces_selected$d3[1,1]
    game_data$r1[3,11] <- forces_selected$d3[1,2]
    game_data$r1[3,12] <- upgrade_data(input$th_inf_upgrade)
    game_data$r1[3,13] <- forces_selected$d3[2,1]
    game_data$r1[3,14] <- forces_selected$d3[2,2]
    game_data$r1[3,15] <- upgrade_data(input$th_arm_upgrade)
    game_data$r1[3,16] <- forces_selected$d3[3,1]
    game_data$r1[3,17] <- forces_selected$d3[3,2]
    game_data$r1[3,18] <- upgrade_data(input$th_str_upgrade)
    game_data$r1[3,19] <- forces_selected$d3[4,1]
    game_data$r1[3,20] <- forces_selected$d3[4,2]
    game_data$r1[3,21] <- upgrade_data(input$th_avi_upgrade)
    game_data$r1[3,22] <- forces_selected$d3[5,1]
    game_data$r1[3,23] <- forces_selected$d3[5,2]
    game_data$r1[3,24] <- upgrade_data(input$th_art_upgrade)
    game_data$r1[3,25] <- forces_selected$d3[6,1]
    game_data$r1[3,26] <- forces_selected$d3[6,2]
    game_data$r1[3,27] <- upgrade_data(input$th_so_upgrade)
    game_data$r1[3,28] <- forces_selected$d3[7,1]
    game_data$r1[3,29] <- forces_selected$d3[7,2]
    game_data$r1[3,30] <- upgrade_data(input$th_ad_upgrade)
    game_data$r1[3,31] <- forces_selected$d3[8,1]
    game_data$r1[3,32] <- forces_selected$d3[8,2]
    game_data$r1[3,33] <- input$th_RC_factor_die_roll
    game_data$r1[3,34] <- th_remaining()
    game_data$r1[3,35] <- th_tech_level()
    game_data$r1[3,36] <- th_overall_readiness2()
    game_data$r1[3,37] <- th_sustainment_factor()
    game_data$r1[3,38] <- th_offense()
    game_data$r1[3,39] <- th_defense()
    game_data$r1[3,40] <- input$th_crisis
    game_data$r1[3,41] <- th_priority_forces()
    game_data$r1[3,42] <- th_crisis_failure()
    game_data$r1[3,43] <- th_read_failure()
    game_data$r1[3,44] <- th_units_lost()
    game_data$r1[3,45] <- 3
    game_data$r1[3,46] <- sum(game_data$r1[1:3,42:43])
    game_data$r1[1:3,47] <- paste(input$class, Sys.Date(), sep=" ")
    
    # code to save game data in database at the end of round 3
    
    db_connect <- function(){
      tryCatch(
        exp = {
          # mydb <- dbConnect(RSQLite::SQLite(), "btf_db.sqlite")
          btf_db <- dbConnect(MySQL(), user = db_user, password = db_password, dbname = db_name, host = db_host)
          dbWriteTable(btf_db, name="class_data1", value = game_data$r1, row.names = F, append = T)
          dbDisconnect(btf_db)
        },
        error = function(e){ 
          return("Database connection not established")
        }
      )
    }
    
    db <- db_connect()
    
    output$th_end_round1_2 <- renderUI({
      if (is.character(db)){
        div(class = "notice", 
          "Data saved; game over; final individual results appear below"
        )
      } else {
        div(
          div(class = "notice",
            "Data saved; game over; individual results appear below; class highlights appear in 'Class Data' tab"
          ),
          output$db_button <- renderUI({
            actionButton("db_retrieve", "Load Class Data (wait for everyone in class to save their data)", icon("truck-ramp-box"))
          })
        )
      }
    })
    
    output$failure_table_with_text <- renderUI({
      div(
        style="background-color:#ff8080; border-style: solid; border-radius: 8px; font-size: 130%;",
        div(style = "text-align: center;", tags$strong("End of Game Failure Points Table")),
        tableOutput("failure_table")
      )  
    })
    
    output$tie_breakers_table_with_text <- renderUI({
      div(
        style="background-color:#ff8080; border-style: solid; border-radius: 8px; font-size: 130%;", 
        div(style = "text-align: center;", tags$strong("Tie Breakers")),
        tableOutput("tie_breakers")  
      )
    })
    
  })
    
  # crisis webpages
  
  output$political <- renderUI({
    tags$iframe(src="https://apps.armywarcollege.edu/csl/BFWC/#/2", height = "1200", width = "100%")
  })
  
  output$political2 <- renderUI({
    tags$iframe(src="https://apps.armywarcollege.edu/csl/BFWC/#/2", height = "1200", width = "100%")
  })
  
  output$readiness <- renderUI({
    tags$iframe(src="https://apps.armywarcollege.edu/csl/BFWC/#/3", height = "1200", width = "100%")
  })
  
  output$crisis <- renderUI({
    tags$iframe(src="https://apps.armywarcollege.edu/csl/BFWC/#/", height = "1200", width = "100%")
  })
  
  # game data analytics
  
  output$data_download <- downloadHandler(
    filename = function() {paste(input$name,"-", Sys.Date(),"-", input$om_fund + input$inf_AC, "-", ".csv", sep="")},
    content = function(file) {write.csv(game_data$r1, file)}
  )
  
  # send game data to db

  observeEvent(input$db_retrieve,{
    
    #mydb <- dbConnect(RSQLite::SQLite(), "btf_db.sqlite")
    btf_db <- dbConnect(MySQL(), user = db_user, password = db_password, dbname = db_name, host = db_host)
    class_db <- dbGetQuery(btf_db, "SELECT * FROM class_data1")
    class_db <- class_db %>% filter(name != "NULL")
    dbDisconnect(btf_db)
    
    # read in each unique class as a list

    classes <- reactive({
      list("Class" = (as.list(unique(class_db["class"])))$class)
    })
    
    # select class for which to use data
    
    output$class_filter <- renderUI({
      tagList(
        selectInput("class2", "Select class", choices = c("Choose one" = "", "All", classes()))
      )
    })
    
    # filter entire database data frame by class selected
  
    class_filtered <- reactive({
      req(input$class2)
      if (input$class2 == "All"){ 
        class_db
      } else {
        class_db %>% filter(class == input$class2) }
    })
  
    # read in each unique player call sign as a list
    
    names <- reactive({
      req(class_filtered())
      list("Name" = (as.list(unique(class_filtered()["name"])))$name)
    })
      
    # graphs
    
    output$graph_filters <- renderUI({
      req(input$class2)
      tagList(
        selectInput("columns", "Filter graph by metric", choices = c("Choose one" = "", colnames(class_filtered())), selected = "sustainment")
      )
    })
    
    output$class_graphs <- renderPlotly({
      req(input$columns)
      ggplot(class_filtered(), aes(x=round, group=name, y=!!as.symbol(input$columns), fill=name)) + geom_bar(position="dodge", stat = "identity")
    })
  
    # scatterplots
    
    output$scatter_filters <- renderUI({
      req(input$class2)
      tagList(
        selectInput("scatter", "Filter scatterplot failure points by metric", choices = c("Choose one" = "", colnames(class_filtered())), selected = "sustainment")
      )
    })
    
    output$scatter_graphs <- renderPlotly({
      req(input$scatter)
      temp <- class_filtered() %>% mutate(round_failure_points=crisis_failure+read_failure)
      temp$round <- as.factor(temp$round)
      ggplot(temp, aes(x=!!as.symbol(input$scatter), y=round_failure_points, color=name, shape=round)) + geom_point()
    })  
    
    # tables
    
    output$data_filters <- renderUI({
      tagList(
        selectInput("class_names", "Filter data table by name", choices = c("Choose one" = "", "All", names())),
        selectInput("rounds", "Filter data table by round", choices = c("Choose one" = "", "All", "Round 1" = 1, "Round 2" = 2, "Round 3" = 3)),
       )
    })
    
    class_data_filtered <- reactive({
      req(input$class_names)
      if (input$class_names == "All") { temp <- class_filtered()
      } else { temp <- class_filtered() %>% filter(name == input$class_names) }
      if (input$rounds == "All") { temp
      } else { temp %>% filter(round == input$rounds)}
    })
    output$class_data <- renderDataTable({ class_data_filtered() })
  
  })
  

})
