# create a list of lists indicating UI elements to add/remove, will be consumed by a function called in each observer function
# create an empty list storing "choice_names" and "choice_values" for bottle_finish radio_buttons
library(icons)
# define action button style globally since I'm still playing with it
action_button_style = "font-size:40px;"

flaticon<-icons::icon_set("flaticon/")
finish_choice_dict<-list("choice_names"=list(),
                         "choice_values"=list())
#define medicine dropdown options
medicines<-c("Lobetolol","Procardia")

ui_mods<-list(
  ## LEVEL 1: selecting if input is for baby or mom
  "baby"=list(
    'remove' = c("#baby","#mom"),
    'add' = list(
      "diaper" = actionButton("diaper","diaper", icon=NULL,width=NULL,
                              img(flaticon$"baby-diaper"),
                              style=action_button_style),
      "feed" = actionButton("feed","feed",icon=NULL,width=NULL,
                            img(flaticon$"feed"),
                            style=action_button_style)
    ),
    'add_location' = "beforeBegin"
  ),
  "mom"=list(
    'remove' = c("#baby","#mom"),
    'add' = list(
      "pump" = actionButton("pump","pump",icon=NULL,width=NULL,
                            img(flaticon$"breast-pump"),
                            style=action_button_style),
      "medication" = actionButton("medicine","medicine",icon=NULL,width=NULL,
                                  img(flaticon$"medicine",
                                      style="height:1em;position:relative;display:inline-block;top:.1em;"),
                                  style=action_button_style)
    ),
    'add_location' = "beforeBegin"
  ),
  ## LEVEL 2A: Options for Baby
  "diaper"=list(
    'remove' = c("#diaper","#feed"),
    'add' = list(
      # diaper change time
      "diaper_time" = timeInput("diaper_time","Time:",value=with_tz(Sys.time(),"America/New_York"),seconds=FALSE),
      # diaper contents
      "contents" = radioButtons(inputId="contents",
                   label="contents",
                   choices=c("pee","poop","both"),
                   selected="both"),
      # bools for diaper rash, pee on clothes, butt paste, blowout
      "rash" = checkboxInput("rash","Diaper Rash present?"),
      "butt_paste" = checkboxInput("butt_paste","Butt Paste applied?"),
      "clothes_pee" = checkboxInput("clothes_pee","Peed on clothes?"),
      "blowout" = checkboxInput("blowout","blowout")
               ),
    'add_location' = "beforeBegin"
  ),
  "feed"=list(
    'remove' = c("#diaper","#feed"),
    'add' = list(
      'breastfeed' = actionButton('breastfeed','breastfeed',icon=NULL,width=NULL,
                                  img(flaticon$"breastfeeding"),
                                  style=action_button_style),
      'bottle_start' = actionButton('bottle_start','started bottle',icon=NULL,width=NULL,
                                    img(flaticon$"bottle-full"),
                                    style=action_button_style),
      'bottle_finish' = actionButton('bottle_finish',
                                     'finished bottle',icon=NULL,width=NULL,
                                     img(flaticon$"bottle-empty",
                                         style="height:1em;position:relative;display:inline-block;top:.1em;"),
                                     style=action_button_style)
    ),
    'add_location' = "beforeBegin"
  ),
  ## LEVEL 3A: Feed Options
  "breastfeed"=list(
    'remove' = c("#breastfeed","#bottle_start","#bottle_finish"),
    'add' = list(
      # diaper change time
      "breast_start" = timeInput("breast_start","Breastfeed start time:",value=with_tz(Sys.time(),"America/New_York"),seconds=FALSE),
      "breast_end" = timeInput("breast_end","Breastfeed end time:",value = with_tz(Sys.time(),"America/New_York"),seconds=FALSE),
      #todo: if useful later, add functionality to log each breast separately
      # bools for diaper rash, pee on clothes, butt paste, blowout
      "nipple_shield" = checkboxInput("nipple_shield","Only fed through nipple shield?"),
      "left_breast" = checkboxInput("left_breast","Fed from left breast?"),
      "right_breast" = checkboxInput("right_breast", "Fed from right breast?")
    ),
    'add_location' = "beforeBegin"
  ),
  "bottle_start"=list(
    'remove' = c("#breastfeed","#bottle_start","#bottle_finish"),
    'add' = list(
      # diaper change time
      "bottle_start_time" = timeInput("bottle_start_time","Time bottle removed from fridge:",
                                      value=with_tz(Sys.time(),tz='America/New_York'),seconds=FALSE),
      "start_volume" = sliderInput("start_volume","Bottle Volume (fl. oz)",
                                   min=0.0,max=4.0,value=2.5,step=.5),
      "delayed_feed" = checkboxInput("delayed_feed","Delayed start (fell asleep before bottle ready etc.)"),
      "finished_bottle" = checkboxInput("finished_bottle", "Finished bottle?") # going to add timer input for finish time in app directly since its just a one-off
    ),
    'add_location' = "beforeBegin"
  ),
  "bottle_finish"=list(
    'remove' = c("#breastfeed","#bottle_start","#bottle_finish"),
    'add' = list(
      #note: option to select previous feed defined in server function
      "finish_time" = timeInput("finish_time","Time bottle finished:",value=with_tz(Sys.time(),"America/New_York"),seconds=FALSE)
    ),
    'add_location' = "beforeBegin"
  ),
  "pump"=list(
    'remove' = c("#pump","#medicine"),
    'add' = list(
      "pump_time" = timeInput("pump_time","Time started:",value=with_tz(Sys.time(),"America/New_York"),seconds=FALSE),
      "pump_volume" = sliderInput("pump_volume","Volume pumped (fl. oz)",
                                  min=0.0,max=8.0,value=4,step=.5)
    ),
    'add_location' = "beforeBegin"
  ),
  "medicine"=list(
    'remove' = c("#pump","#medicine"),
    'add' = list(
      "medicine_time" = timeInput("medicine_time","Time started:",value=with_tz(Sys.time(),"America/New_York"),seconds=FALSE),
      "medicine_type" = selectInput("medicine_type","medicine_type",choices=medicines)
    ),
    'add_location' = "beforeBegin"
  )
  )
# define function that gets called on an item in ui_mods within the appropriate observer
modify_ui<-function(button_id,mod_dict,submit=TRUE){
  # try initializing choice_names and choice_values as empty
  choice_names = list()
  choice_values = list()
  to_remove = mod_dict[[button_id]][['remove']]
  to_add = mod_dict[[button_id]][['add']]
  add_location = mod_dict[[button_id]][['add_location']]
  # iterate through to_add, add
  for(item in to_add){
    insertUI(selector=paste0('#',button_id),
             where=add_location,
             ui=item)
  }
  # if submit==True, add a submit button
  if(submit==TRUE){
    insertUI(selector=paste0('#',button_id),
             where=add_location,
             ui=actionButton(paste0('submit_',button_id),"submit"))
  }
  # iterate through to_remove, remove
  for(item in to_remove){
    removeUI(item)
  }
  return(TRUE)
}

# define a function that gets called when the submit button is clicked
submit_info<-function(button_id, mod_dict,input,workbook_id){
  # get fields to be added from mod_dict
  to_add = mod_dict[[button_id]][['add']]
  # create data frame from added input elements
  inputs_list = lapply(names(to_add),function(x) input[[x]])
  responses = data.frame(inputs_list)
  names(responses)<-names(to_add)
  sheet_append(data=tibble(responses),
               ss = workbook_id,
               sheet=button_id)
  # indicate submission was recorded
  insertUI(selector=paste0("#submit_",button_id),
           where="afterEnd",
           ui=renderText("Entry Submitted"))
}