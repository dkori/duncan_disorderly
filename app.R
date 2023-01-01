print(enc2utf8('\U0001F4A9'))
library(shiny)
library(googledrive)
library(googlesheets4)
library(shinyTime)
library(tidyr)
library(dplyr)
library(lubridate)
library(icons)
#library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(viridis)
#todo: add javascript to display times in client timezone following method described here https://stackoverflow.com/questions/24842229/how-to-retrieve-the-clients-current-time-and-time-zone-when-using-shiny

########################################## CURRENT TIME ZONE LOGIC##################
# Google sheet only records time in UTC, but I want to be able to look at the raw sheet and see eastern time, so for now: 
## 1. all inputs should be anchored to current time in eastern time
## 2. input times are then forced to UTC before writing to google sheets
## 3.) times in sheets that are read in are then forced back to eastern time
#todo: add links to the following at bottom of page
#<a href="https://www.flaticon.com/free-icons/mother" title="mother icons">Mother icons created by Freepik - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/infant" title="infant icons">Infant icons created by Freepik - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/baby-bottle" title="baby bottle icons">Baby bottle icons created by Freepik - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/breastfeeding" title="breastfeeding icons">Breastfeeding icons created by Freepik - Flaticon</a>
#<a href="https://www.flaticon.com/free-icons/infant" title="infant icons">Infant icons created by Freepik - Flaticon</a>
library(gargle)
library(here)
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = here::here(".secrets"),
  gargle_verbosity = "debug"
)
#print("does gargle_oauth_cache match secrets?")
#print(gargle::gargle_oauth_cache()==here::here(".secrets"))
# Load packages
drive_auth(
  email=gargle::gargle_oauth_email(),
  scopes="https://www.googleapis.com/auth/drive",
  )
gs4_auth(scopes="https://www.googleapis.com/auth/spreadsheets")
valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color,
                                                      ";opacity:.5"),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 56px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}
flaticon<-icons::icon_set("flaticon/")
# read in function/dictionary to modify UI
source('UI_element_changes.R')
source('at_a_glance_stats.R')
source('trends_tab_functions.R')
source('retrieve_current_records.R')
# define action button style globally since I'm still playing with it
action_button_style = "font-size:40px;"


# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
workbook_id <- drive_get("baby-log-test6")$id

#define medicine dropdown options
medicines<-c("Lobetolol","Procardia")

# Define shiny UI
ui <- fluidPage(
  theme=shinytheme('superhero'),
#  uiOutput('myUI'),
  tabsetPanel(id='nav',#title='Duncan',#title=NULL,width=12,id='tabs',
             selected='New Record',#collapsible=F,
             tabPanel("New Record",class="active",
                  actionButton("baby","baby",icon=NULL,width=NULL,
                               img(flaticon$"baby"),
                               style=action_button_style),
                  actionButton("mom","mom",icon=NULL,width=NULL,
                               img(flaticon$mother),
                               style=action_button_style),
                  actionButton("refresh", "Start Over")),
             tabPanel(title="At a Glance",
                  htmlOutput("latest"),
                  selectInput('total_range','Totals over:',
                              choices = c("Last 24 Hrs" = "last_day",
                                          "Last 7 days" = "last_week"),
                              selected = "last_day"),
                  valueBox("amount_fed_num",
                           subtitle=textOutput("fed_caption"),
                           icon='person-breastfeeding',
                           color="blue"),
                  valueBox("amount_pumped_num",
                           subtitle="Fl. oz. breast-milk pumped",
                           icon='pump-medical',
                           color="green"),
                  valueBox("poop_diapers_num",
                           subtitle="Poop diapers",
                           icon='poop',
                           color="orange")                  
                  ),
             tabPanel(title="Trends",
                      tabBox(tabPanel(title= 'Cumulative Pump vs Feed', class='active',
                                      selectInput('baseline_pump','Choose baseline pump',
                                                  choices=c()),
                                      checkboxInput("feed_trend_curve",
                                                    "Show feeding trend curve",
                                                    value=FALSE),
                                      plotlyOutput(outputId = "cumu_milk_plot")),
                             tabPanel(title='Diaper Changes and Feeding',
                                      plotOutput("feed_diaper_plot"))),
                      
             ),
             tabPanel(title='Raw Records',
                  selectInput('select_raw','Raw records for:',
                              choices = c("Diaper Records" = "diaper_records",
                                          "Bottle Feed Records" = "feed_records",
                                          "Pump Records" = "pump_records")),
                  #textOutput("Diaper Records:"),
                  dataTableOutput('chosen_raw')
                  )
         )
  
)

# Define shiny server
server <- function(input, output, session) {
  # allow user to modify theme based on url param
  # output$myUI<-renderUI({
  #   query <- parseQueryString(session$clientData$url_search)
  #   if (!is.null(query[['theme']])) {
  #     chosen_theme<-tags$head(tags$link(rel = "stylesheet", type = "text/css", 
  #                         href = shinytheme(query[['theme']])))
  #     
  #   }else{
  #     chosen_theme<-tags$head(tags$link(rel = "stylesheet", type = "text/css", 
  #                         href = shinytheme('superhero')))
  #   }
  #   return(chosen_theme)
  # })

  # add refresh button
  observeEvent(input$refresh,{
    session$reload()
  })
  # load current records from sheet
  current_records<-retrieve_current(workbook_id)
  ########################### ########################### ######################
  ##################################TRENDS TAB #################################
  ########################### ########################### ######################
    # update the select input for baseline pump in trends tab
  observeEvent(input$nav,{
    pump_choice_vec<-gen_pump_options(current_records)
    updateSelectInput(inputId = 'baseline_pump',
                      label = "Select baseline pump for chart",
                      choices = pump_choice_vec,
                      selected = pump_choice_vec[[7]]
                      )
  })
  # use pump records to create choices for 
  ########################### CREATE PLOTS########################### 
  # cumulative plot feed vs pump
  output$cumu_milk_plot<-renderPlotly({
    return(gen_feed_pump_step(current_records,input$baseline_pump,input$feed_trend_curve))
  })
  # plot showing diaper changes in relation to feeds
  output$feed_diaper_plot<-renderPlot({
    return(gen_diaper_feed(current_records))
  })
  ########################### ########################### ######################
  ##################################Raw Records TAB #################################
  ########################### ########################### ######################
  # assign chosen current record to raw_records display
  output$chosen_raw<-renderDataTable({current_records[[input$select_raw]]})
  ########################### ########################### ######################
  ##################################At a Glance Tab #################################
  ########################### ########################### ######################
  # call function to generate at_a_glance values
  # dictionary for finding start date based on input$total_range
  start_dates<-list("last_day"=with_tz(Sys.time(),'America/New_York')-days(1),
                    "last_week"=with_tz(Sys.time(),'America/New_York')-days(7))
  observeEvent(input$total_range,
               {
                 at_a_glance_data<-gen_at_a_glance(current_records,input$total_range,start_dates)
                 # bulleted list of latest data
                 output$latest<-renderText({at_a_glance_data$latest})
                 # create value boxes with recent stats
                 output$amount_fed_num<-reactive({at_a_glance_data$fed_num})
                 output$amount_pumped_num<-reactive({at_a_glance_data$pump_num})
                 output$poop_diapers_num<-reactive({at_a_glance_data$poop_diapers_num})
                 output$fed_caption<-renderText({at_a_glance_data$feed_subtitle})
                 }
               )
  ## LEVEL 1 OBSERVERS: baby or mom
  observeEvent(input$baby,{
    modify_ui(button_id = "baby",
              #mod_dict=ui_mods,
              submit=FALSE)
  })
  observeEvent(input$mom,{
    modify_ui(button_id = "mom",
              #mod_dict=ui_mods,
              submit=FALSE)
  })
  ## LEVEL 2a OBSERVERS: diaper or feed

  observeEvent(input$diaper, {
    # add UI elements for diaper choices
    modify_ui(button_id="diaper",
              #mod_dict=ui_mods
              )

  })
  # observer to submit diaper content
  observeEvent(input$submit_diaper,{
    submit_info("diaper",
                #ui_mods,
                input,workbook_id)
  })
  observeEvent(input$feed,{
    modify_ui(button_id="feed",
              #mod_dict=ui_mods,
              submit=FALSE)
  })
  ## LEVEL 2B OBSERVERS: pump or medication
  observeEvent(input$mom,{
    modify_ui(button_id="mom",
              #mod_dict=ui_mods,
              submit=FALSE)
  })
  ## LEVEL 3A OBSERVERS: feed options
  observeEvent(input$breastfeed,{
    modify_ui(button_id="breastfeed"#,mod_dict=ui_mods
              )
  })
  # logic when user selects to start a bottle
  observeEvent(input$bottle_start,{
    modify_ui(button_id="bottle_start"#,mod_dict=ui_mods
              )
  })
  observeEvent(input$formula,{
    modify_ui(button_id='formula'#,mod_dict=ui_mods
              )
  })
  # add an input for finish_time if finished_bottle is checked
  observeEvent(input$finished_bottle,{
    if(input$finished_bottle!=TRUE){
      removeUI('#finish_start_time-label')
      removeUI('.shiny-input-container:has(>#finish_start_time)')
    }else{
      insertUI('#finished_bottle','beforeBegin',
               #timeInput("finish_time", "Time Finished?", value=with_tz(Sys.time(),tzone="America/New_York"))
               timeslide("finish_start_time","Time Finished?"))
    }
  })
  observeEvent(input$bottle_finish,{
    # need to retrieve previously unfinished bottles from bottle sheet
    logged_bottles<-googlesheets4::read_sheet(ss=workbook_id, sheet="bottle_start")
    unfinished_bottles = logged_bottles[logged_bottles$finished_bottle==FALSE,]
    if(nrow(unfinished_bottles)!=0){
      
      # treating start time as unique identifier for bottle (can't see scenario where we pull two bottles at the same time)
      # choice names = start time and volume of bottle
      choice_names = unfinished_bottles%>%
        mutate(descriptive = paste0(start_volume,'fl. oz.\n','Started at ',
                                    #as.POSIXlt.character(start_time_utc,format='%d %b %H:%M'),
                                    force_tz(as.POSIXct(start_time),'America/New_York')%>%as.character(format='%d %b %H:%M',tz='America/New_York'),'\n'))%>%
        select(descriptive)
      choice_values = unfinished_bottles$start_time
      names(choice_values)<-choice_names$descriptive
      modify_ui(button_id="bottle_finish"#,mod_dict=ui_mods
                )
      # add radio buttons to select which unfinished bottle should be updated to finished
      insertUI('#finish_time', 'beforeBegin',
               selectInput('previous_bottle','select unfinished bottle',
                           # need to define choices based on previously logged bottles that weren't finished
                           choice_values,
                           selected=choice_values[[1]]))

    }else{
      insertUI(selector="#bottle_finish",
               where="afterEnd",
               ui=renderText("No Unfinished Bottles"))
    }
  })
  # submit button observers for each of the above
  observeEvent(input$submit_breastfeed,{
    submit_info("breastfeed",
                #ui_mods,input,
                workbook_id)
  })
  observeEvent(input$submit_bottle_start,{
    submit_info("bottle_start",
                #ui_mods,
                input,
                workbook_id)
  })
  observeEvent(input$submit_formula,{
    submit_info("formula",
                #ui_mods,
                input,workbook_id)
  }
               )
  observeEvent(input$submit_bottle_finish,{
    logged_bottles<-googlesheets4::read_sheet(ss=workbook_id, sheet="bottle_start")%>%
      as.data.frame()
    # reformat input for bool
    prev_bottle_reformatted = as.character(as.POSIXct(input$previous_bottle,tz='UTC'))
    # already read in bottle log in observer for bottle_finish, correct selected record to finish and re-write sheet
    # remove the update row from logged_bottles
    stay_same = logged_bottles%>%
      filter(as.character(start_time)!=prev_bottle_reformatted)%>%
      mutate(finish_time = as.POSIXlt(finish_time))
    to_update = logged_bottles%>%
      filter(as.character(start_time)==prev_bottle_reformatted)
    updated<-data.frame("start_time"=to_update$start_time,
                        start_volume = to_update$start_volume,
                        delayed_feed = to_update$delayed_feed,
                        vitamin_d_drop = to_update$vitamin_d_drop,
                        finished_bottle = TRUE,
                        # forcing timezone to UTC since sheet will always assume UTC, even though actual recorded time will be EDT
                        finish_time = force_tz(input$finish_time,'UTC'))
    export_subset<-bind_rows(stay_same,updated)
    print(export_subset)
    write_sheet(data = export_subset,
                ss=workbook_id,sheet='bottle_start')
    # indicate submission was recorded
    insertUI(selector="#submit_bottle_finish",
             where="afterEnd",
             ui=renderText("Entry Submitted"))
  })
  #LEVEL 3B Obervers: pump or medication
  observeEvent(input$pump,{
    modify_ui(button_id= "pump", 
              #mod_dict = ui_mods
              )
  })
  observeEvent(input$medicine,{
    modify_ui(button_id = "medicine",
              #mod_dict = ui_mods
              )
  })
  observeEvent(input$submit_pump,{
    submit_info("pump", 
                #mod_dict = ui_mods,
                input,workbook_id)
  })
  observeEvent(input$submit_medicine,{
    submit_info("medicine",
                #mod_dict = ui_mods,
                input,workbook_id)
  })
}
# Run the shiny application
shinyApp(ui, server)
