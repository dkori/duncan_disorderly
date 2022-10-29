# Load packages
library(shiny)
library(shinysurveys)
library(googledrive)
library(googlesheets4)
library(shinyTime)

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "duncan_disorderly/.secrets"
)

# Get the ID of the sheet for writing programmatically
# This should be placed at the top of your shiny app
sheet_id <- drive_get("baby-log-test2")$id


# Define shiny UI
ui <- fluidPage(
  actionButton("diaper","diaper"),
  actionButton("feeding","feeding"),
  actionButton("sleep","sleep")
)

# Define shiny server
server <- function(input, output, session) {

  observeEvent(input$diaper, {
    # add input fields for diaper questions
    insertUI(selector="#diaper",
             where="beforeBegin",
             ui = timeInput("diaper_time","Time:",value=Sys.time()))
    insertUI(selector="#diaper",
             where="beforeBegin",
             ui = radioButtons(inputId="contents",
                              label="contents",
                              choices=c("pee","poop","both"),
                              selected="both"
                              ))
    insertUI(selector="#diaper",
             where="beforeBegin",
             ui=checkboxInput("rash","Diaper Rash present?"))
    insertUI(selector="#diaper",
             where="beforeBegin",
             ui=checkboxInput("butt_paste","Butt Paste applied?"))
    insertUI(selector="#diaper",
             where="beforeBegin",
             ui=checkboxInput("clothes_pee","Peed on clothes?"))
    # add a submit button
    insertUI(selector="#diaper",
             where="beforeBegin",
             ui=actionButton("submit_diaper","submit"))
    # remove action buttons
    removeUI('#diaper')
    removeUI('#feeding')
    removeUI('#sleep')
  })
  # observer to submit diaper content
  observeEvent(input$submit_diaper,{
    # store diaper responses in a dataframe
    diaper_responses = data.frame(
      time=input$diaper_time,
      contents=input$contents,
      rash=input$rash,
      butt_paste=input$butt_paste,
      clothes_pee=input$clothes_pee
    )
    # append results to google sheet
    sheet_append(data=diaper_responses,
                 ss = sheet_id,
                 sheet="diaper")
    insertUI(selector="#submit_diaper",
             where="afterEnd",
             ui=renderText("Entry Submitted"))
  })
  
  
}
# Run the shiny application
shinyApp(ui, server)