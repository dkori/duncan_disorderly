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
library(shinydashboard)
library(lubridate)
# read in function/dictionary to modify UI
source('UI_element_changes.R')

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "duncan_disorderly/.secrets"
)

# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
workbook_id <- drive_get("baby-log-test2")$id



# Define shiny UI
ui <- fluidPage(
  tabBox(title=NULL,width=12,id='tabs',
         tabPanel("Record info",class="active",
                  actionButton("baby","baby",icon=icon('baby')),
                  actionButton("mom","mom",icon=icon('person-pregnant'))),
         tabPanel("Recent entries",
                  #textOutput("Diaper Records:"),
                  dataTableOutput('diaper_records'),
                  #textOutput("Feed Records:"),
                  dataTableOutput('feed_records')
                  ))
)

# Define shiny server
server <- function(input, output, session) {
  diaper_records = read_sheet(workbook_id,"diaper")%>%
    mutate(start_time = as.POSIXct(`Time (UTC)`,tz='EDT'))%>%
    arrange(desc(start_time))%>%
    select(start_time,Contents,`Diaper Rash`,`Butt Paste`,`Pee On Clothes?`,`Blowout`)
  feed_records = read_sheet(workbook_id,"bottle_start")%>%
    mutate(start_time = as.POSIXct(start_time_utc,tz='EDT'))%>%
    arrange(desc(start_time))%>%
    select(start_time,start_volume,delayed_feed,finished_bottle,finish_time_utc)
  output$diaper_records<-renderDataTable({diaper_records})
  output$feed_records<-renderDataTable({feed_records})
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
  ## LEVEL 3A OBSERVERS: feed options
  observeEvent(input$breastfeed,{
    modify_ui(button_id="breastfeed",mod_dict=ui_mods)
  })
  # logic when user selects to start a bottle
  observeEvent(input$bottle_start,{
    modify_ui(button_id="bottle_start",mod_dict=ui_mods)
    # add an input for finish_time if finished_bottle is checked
    observeEvent(input$finished_bottle==TRUE,{
      insertUI('#finished_bottle','afterEnd',
               timeInput("finish_time", "Time Finished?", value=Sys.time()))
    })
    # remove finish time input if unchecked
    observeEvent(input$finished_bottle==FALSE,{
      removeUI('#finish_time')
    })
  })
  observeEvent(input$bottle_finish,{
    # need to retrieve previously unfinished bottles from bottle sheet
    logged_bottles<-googlesheets4::read_sheet(ss=workbook_id, sheet="bottle_start")
    print(logged_bottles)
    unfinished_bottles = logged_bottles[logged_bottles$finished_bottle==FALSE,]
    # treating start time as unique identifier for bottle (can't see scenario where we pull two bottles at the same time)
    # choice names = start time and volume of bottle
    choice_names = unfinished_bottles%>%
      mutate(descriptive = paste0(start_volume,'fl. oz.\n','Started at ',
                                  #as.POSIXlt.character(start_time_utc,format='%d %b %H:%M'),
                                  as.POSIXct(start_time_utc,
                                             tz="UTC")%>%as.character(format='%d %b %H:%M',tz='EDT'),'\n'))%>%
      select(descriptive)
    choice_values = unfinished_bottles$start_time_utc
    names(choice_values)<-choice_names$descriptive
    modify_ui(button_id="bottle_finish",mod_dict=ui_mods)
    # add radio buttons to select which unfinished bottle should be updated to finished
    insertUI('#finish_time', 'beforeBegin',
             selectInput('previous_bottle','select unfinished bottle',
                          # need to define choices based on previously logged bottles that weren't finished
                          choice_values,
                          selected=choice_values[[1]]))
  })
  # submit button observers for each of the above
  observeEvent(input$submit_breastfeed,{
    submit_info("breastfeed",ui_mods,input,workbook_id)
  })
  observeEvent(input$submit_bottle_start,{
    submit_info("bottle_start",ui_mods,input,workbook_id)
  })
  observeEvent(input$submit_bottle_finish,{
    print('finish_time:')
    print(input$finish_time)
    print(class(input$finish_time))
    finish_time_reformatted<-input$finish_time
    logged_bottles<-googlesheets4::read_sheet(ss=workbook_id, sheet="bottle_start")%>%
      as.data.frame()
    # reformat input for bool
    prev_bottle_reformatted = as.character(as.POSIXct(input$previous_bottle,tz='UTC'))
    # already read in bottle log in observer for bottle_finish, correct selected record to finish and re-write sheet
    # remove the update row from logged_bottles
    stay_same = logged_bottles%>%
      filter(as.character(start_time_utc)!=prev_bottle_reformatted)%>%
      mutate(finish_time_utc = as.POSIXlt(finish_time_utc))
    to_update = logged_bottles%>%
      filter(as.character(start_time_utc)==prev_bottle_reformatted)
    updated<-data.frame("start_time_utc"=to_update$start_time_utc,
                        start_volume = to_update$start_volume,
                        delayed_feed = to_update$delayed_feed,
                        finished_bottle = TRUE,
                        finish_time_utc = input$finish_time)
    export_subset<-bind_rows(stay_same,updated)
    write_sheet(data = export_subset,
                ss=workbook_id,sheet='bottle_start')
    # indicate submission was recorded
    insertUI(selector=paste0("#submit_",button_id),
             where="afterEnd",
             ui=renderText("Entry Submitted"))
    
  })
}
# Run the shiny application
shinyApp(ui, server)