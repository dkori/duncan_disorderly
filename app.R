#todo: add javascript to display times in client timezone following method described here https://stackoverflow.com/questions/24842229/how-to-retrieve-the-clients-current-time-and-time-zone-when-using-shiny

#todo: add links to the following at bottom of page
#<a href="https://www.flaticon.com/free-icons/mother" title="mother icons">Mother icons created by Freepik - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/infant" title="infant icons">Infant icons created by Freepik - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/baby-bottle" title="baby bottle icons">Baby bottle icons created by Freepik - Flaticon</a>
# <a href="https://www.flaticon.com/free-icons/breastfeeding" title="breastfeeding icons">Breastfeeding icons created by Freepik - Flaticon</a>
#<a href="https://www.flaticon.com/free-icons/infant" title="infant icons">Infant icons created by Freepik - Flaticon</a>

# Load packages
library(shiny)
library(shinysurveys)
library(googledrive)
library(googlesheets4)
library(shinyTime)
library(tidyr)
library(dplyr)
library(lubridate)
library(icons)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
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
# define action button style globally since I'm still playing with it
action_button_style = "font-size:40px;"
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "duncan_disorderly/.secrets"
)

# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
workbook_id <- drive_get("baby-log-test5")$id

#define medicine dropdown options
medicines<-c("Lobetolol","Procardia")

# Define shiny UI
ui <- fluidPage(
  uiOutput('myUI'),
  tabBox(title=NULL,width=12,id='tabs',
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
                                          "Last 7 days" = "last_week")),
                  valueBox("amount_fed",
                           subtitle="Fl. oz. breast-milk consumed",
                           icon='person-breastfeeding',
                           color="blue"),
                  valueBox("amount_pumped",
                           subtitle="Fl. oz. breast-milk pumped",
                           icon='pump-medical',
                           color="green"),
                  valueBox("poop_diapers",
                           subtitle="Poop diapers",
                           icon='poop',
                           color="orange")
                  ),
         tabPanel(title='Raw Records',
                  selectInput('select_raw','Raw records for:',
                              choices = c("Diaper Records" = "diaper_records",
                                          "Bottle Feed Records" = "feed_records",
                                          "Pump Records" = "pump_records")),
                  #textOutput("Diaper Records:"),
                  dataTableOutput('chosen_raw'),
                  ))
  
)

# Define shiny server
server <- function(input, output, session) {
  # allow user to modify theme based on url param
  output$myUI<-renderUI({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['theme']])) {
      chosen_theme<-tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                          href = shinytheme(query[['theme']])))
      
    }else{
      chosen_theme<-tags$head(tags$link(rel = "stylesheet", type = "text/css", 
                          href = shinytheme('cerulean')))
    }
    return(chosen_theme)
  })

  # add refresh button
  observeEvent(input$refresh,{
    session$reload()
  })
  # read in the current logged data
  # read in diaper and feed records
  current_records<-list()
  current_records[['diaper_records']]<-read_sheet(workbook_id,"diaper")%>%
    mutate(start_time = as.POSIXct(`Time (UTC)`,tz='America/New_York'))%>%
    arrange(desc(start_time))%>%
    select(start_time,Contents,`Diaper Rash`,`Butt Paste`,`Uric Crystals`)
  current_records[['feed_records']]<-read_sheet(workbook_id,"bottle_start")%>%
    # clear finish time if bottle is unfinished
    mutate(finish_time = case_when(finished_bottle==TRUE~finish_time,
                                   TRUE~as.POSIXct(NA)))%>%
    #mutate(start_time = as.POSIXct(start_time_utc,tz='EDT'))%>%
    arrange(desc(start_time))%>%
    select(start_time,start_volume,delayed_feed,vitamin_d_drop, finished_bottle,finish_time)
  current_records[['pump_records']]<-read_sheet(workbook_id,"pump")%>%
    arrange(desc(start_time))
  # assign chosen current record to raw_records display
  output$chosen_raw<-renderDataTable({current_records[[input$select_raw]]})
  # call function to generate at_a_glance data
  at_a_glance_data<-gen_at_a_glance(current_records)
  output$latest<-renderText({at_a_glance_data$latest})
  # insert unfinished bottle list
  output$diaper_records<-renderDataTable({current_records$diaper_records})
  output$feed_records<-renderDataTable({current_records$feed_records})
  
  # dictionary for finding start date based on input$total_range
  start_dates<-list("last_day"=with_tz(Sys.time(),'America/New_York')-days(1),
                    "last_week"=with_tz(Sys.time(),'America/New_York')-days(7))
  # create value boxes with recent stats
  output$amount_fed<-reactive({
    # select start date based on user range selection
    start_date<-start_dates[[input$total_range]]
    num<-current_records$feed_records%>%
      filter(start_time>start_date)%>%
      summarise(sum(start_volume))%>%
      unlist()
    as.numeric(num)
    })
                              
  output$amount_pumped<-reactive({
    # select start date based on user range selection
    start_date<-start_dates[[input$total_range]]
    num<-current_records$pump_records%>%
      filter(start_time>start_date)%>%
      summarise(sum(volume))%>%
      unlist()
    as.numeric(num)
    })
  
  output$poop_diapers<-reactive({
    # select start date based on user range selection
    start_date<-start_dates[[input$total_range]]
    num<-current_records$diaper_records%>%
      filter(start_time>start_date)%>%
      filter(grepl('both|poop',Contents,ignore.case=TRUE))%>%
      nrow()
    num
    })

  ## LEVEL 1 OBSERVERS: baby or mom
  observeEvent(input$baby,{
    modify_ui(button_id = "baby",mod_dict=ui_mods,submit=FALSE)
  })
  observeEvent(input$mom,{
    modify_ui(button_id = "mom", mod_dict=ui_mods,submit=FALSE)
  })
  ## LEVEL 2a OBSERVERS: diaper or feed

  observeEvent(input$diaper, {
    # add UI elements for diaper choices
    modify_ui(button_id="diaper",mod_dict=ui_mods)

  })
  # observer to submit diaper content
  observeEvent(input$submit_diaper,{
    submit_info("diaper",ui_mods,input,workbook_id)
  })
  observeEvent(input$feed,{
    modify_ui(button_id="feed",mod_dict=ui_mods,submit=FALSE)
  })
  ## LEVEL 2B OBSERVERS: pump or medication
  observeEvent(input$mom,{
    modify_ui(button_id="mom",mod_dict=ui_mods,submit=FALSE)
  })
  ## LEVEL 3A OBSERVERS: feed options
  observeEvent(input$breastfeed,{
    modify_ui(button_id="breastfeed",mod_dict=ui_mods)
  })
  # logic when user selects to start a bottle
  observeEvent(input$bottle_start,{
    modify_ui(button_id="bottle_start",mod_dict=ui_mods)
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
                                    as.POSIXct(start_time,
                                               tz="UTC")%>%as.character(format='%d %b %H:%M',tz='America/New_York'),'\n'))%>%
        select(descriptive)
      choice_values = unfinished_bottles$start_time
      names(choice_values)<-choice_names$descriptive
      modify_ui(button_id="bottle_finish",mod_dict=ui_mods)
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
    submit_info("breastfeed",ui_mods,input,workbook_id)
  })
  observeEvent(input$submit_bottle_start,{
    submit_info("bottle_start",ui_mods,input,workbook_id)
  })
  observeEvent(input$submit_bottle_finish,{
    finish_time_reformatted<-with_tz(input$finish_time,'America/New_York')
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
    modify_ui(button_id= "pump", mod_dict = ui_mods)
  })
  observeEvent(input$medicine,{
    modify_ui(button_id = "medicine", mod_dict = ui_mods)
  })
  observeEvent(input$submit_pump,{
    submit_info("pump", mod_dict = ui_mods,input,workbook_id)
  })
  observeEvent(input$submit_medicine,{
    submit_info("medicine",mod_dict = ui_mods,input,workbook_id)
  })
}
# Run the shiny application
shinyApp(ui, server)