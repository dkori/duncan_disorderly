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
# create a timeslide function since all timer slides will have same parameters
timeslide<-function(id,label,step=5){
  # function to generate values
  timeslide_values<-function(step=step){
    val = force_tz(with_tz(Sys.time(),'America/New_York'),'UTC')
    min=val-hours(4)
    max=val+minutes(5)
    return(list("min"=min,"max"=max,"val"=val))
  }
  return(sliderInput(id,label,min=timeslide_values()$min,
                     max=timeslide_values()$max,
                     value=timeslide_values()$val,
                     step=minutes(5),
                     timeFormat='%d %b %H:%M',
                     timezone='America/New_York'))
  
}
# create a function to generate time input slide range

gen_ui_mods<-function(){
  list(
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
        #"diaper_time" = timeInput("diaper_time","Time:",value=with_tz(Sys.time(),"America/New_York"),seconds=FALSE),
        "diaper_time" = timeslide("diaper_time", "Time:"),
        # diaper contents
        "contents" = radioButtons(inputId="contents",
                                  label="contents",
                                  choices=c("pee","poop","both"),
                                  selected="both"),
        # bools for diaper rash, pee on clothes, butt paste, blowout
        "rash" = selectInput("rash","Diaper Rash",
                             choices=c('None','General Redness', "Spots"),
                             selected='None',
                             multiple=T),
        "rash_avoidance" = selectInput("rash_avoidance","Treatments?",
                                       choices=c("Red Butt Paste", "Yellow Butt Paste", "Lotrimin", "Bath"),
                                       selected="Red Butt Paste",
                                       multiple=T)
        #"clothes_pee" = checkboxInput("clothes_pee","Peed on clothes?"),
        #"blowout" = checkboxInput("blowout","blowout")
      ),
      'add_location' = "beforeBegin"
    ),
    "feed"=list(
      'remove' = c("#diaper","#feed"),
      'add' = list(
        # 'breastfeed' = actionButton('breastfeed','breastfeed',icon=NULL,width=NULL,
        #                             img(flaticon$"breastfeeding"),
        #                             style=action_button_style),
        'bottle_start' = actionButton('bottle_start','start bottle',icon=NULL,width=NULL,
                                      img(flaticon$"bottle-full"),
                                      style=action_button_style),
        'bottle_finish' = actionButton('bottle_finish',
                                       'finish bottle',icon=NULL,width=NULL,
                                       img(flaticon$"bottle-empty",
                                           style=action_button_style),
                                       style=action_button_style)
        # 'formula' = actionButton('formula',
        #                          'formula',icon=NULL,width=NULL,
        #                          img(flaticon$'formula'),
        #                          style=action_button_style)
      ),
      'add_location' = "beforeBegin"
    ),
    ## LEVEL 3A: Feed Options
    # "breastfeed"=list(
    #   'remove' = c("#breastfeed","#bottle_start","#bottle_finish","#formula"),
    #   'add' = list(
    #     # diaper change time
    #     #"breast_start" = timeInput("breast_start","Breastfeed start time:",value=with_tz(Sys.time(),"America/New_York"),seconds=FALSE),
    #     "breast_start" = timeslide("breast_start", "Breastfeed start time:",step=1),
    #     #"breast_end" = timeInput("breast_end","Breastfeed end time:",value = with_tz(Sys.time(),"America/New_York"),seconds=FALSE),
    #     "breast_end" = timeslide("breast_end","Breastfeed end time:",step=1),
    #     #todo: if useful later, add functionality to log each breast separately
    #     # bools for diaper rash, pee on clothes, butt paste, blowout
    #     "nipple_shield" = checkboxInput("nipple_shield","Only fed through nipple shield?"),
    #     "left_breast" = checkboxInput("left_breast","Fed from left breast?"),
    #     "right_breast" = checkboxInput("right_breast", "Fed from right breast?")
    #   ),
    #   'add_location' = "beforeBegin"
    # ),
    "bottle_start"=list(
      'remove' = c("#breastfeed","#bottle_start","#bottle_finish","#formula"),
      'add' = list(
        # diaper change time
        #"bottle_start_time" = timeInput("bottle_start_time","Time bottle removed from fridge:", value=with_tz(Sys.time(),tz='America/New_York'),seconds=FALSE),
        "bottle_start_time" = timeslide("bottle_start_time","Time bottle removed from fridge:"),
        "start_volume" = sliderInput("start_volume","Bottle Volume (fl. oz)",
                                     min=0.0,max=5,value=3,step=.5),
        #"delayed_feed" = checkboxInput("delayed_feed","Delayed start (fell asleep before bottle ready etc.)",value=F), # this is deprecated but need to keep it to keep number of columns correct, so will just remove it in server
        "vitamin_d" = checkboxInput("vitamin_d","Added Vitamin D Drop"),
        "finished_bottle" = checkboxInput("finished_bottle", "Finished bottle?"),
        "finish_start_time" = timeslide("finish_start_time","Time Finished?"),
        "source" = selectInput("source", "Source Pump?",
                                choices=c("Fridge","Freshly Pumped","Thawed from Freezer")),
        "start_fuss" = selectInput('start_fuss', 'Fussiness to start bottle?',
                                    choices=c("Don't Know", 'Easy/No Fuss', 'Some Fuss or Avoidance', 'Very Fussy'), 
                                    selected="Don't Know"),
        "start_sleep" = selectInput('start_sleep', 'Sleep Since Last Bottle Finished?',
                                     choices=c("Don't Know", 'Almost no sleep', 'Some sleep', 'Mostly asleep'), 
                                     selected="Don't Know")
      ),
      'add_location' = "beforeBegin"
    ),
    "bottle_finish"=list(
      'remove' = c("#breastfeed","#bottle_start","#bottle_finish","#formula"),
      'add' = list(
        #note: option to select previous feed defined in server function
        #"finish_time" = timeInput("finish_time","Time bottle finished:",value=with_tz(Sys.time(),"America/New_York"),seconds=FALSE)
        "finish_time" = timeslide("finish_time","Time bottle finished:"),
        "finish_fuss" = selectInput('finish_fuss', 'Fussiness since initial bottle start?',
                              choices=c("Don't Know", 'Easy/No Fuss', 'Some Fuss or Avoidance', 'Very Fussy'), 
                              selected="Don't Know"),
        "finish_sleep" = selectInput('finish_sleep', 'Sleep Since Bottle Started?',
                                     choices=c("Don't Know", 'Almost no sleep', 'Some sleep', 'Mostly asleep'), 
                                     selected="Don't Know"),
        # initially creating it with a max of 10, but max will be updated to bottle amount in server
        "discarded"=sliderInput('discarded','Amount Discarded (if any)',value=0.0,min=0,max=10,step=.25)
      ),
      'add_location' = "beforeBegin"
    ),
    'formula' = list(
      'remove' = c("#breastfeed","#bottle_start","#bottle_finish","#formula"),
      'add' = list(
        'formula_time' = timeslide("formula_time","Time formual given:"),
        'formula_consumed'=sliderInput('formula_consumed','Formula drank (fl. oz)',min=0.0,max=4.0,value=2,step=.5)
      ),
      'add_location' = 'beforeBegin'
    ),
    "pump"=list(
      'remove' = c("#pump","#medicine"),
      'add' = list(
        #"pump_time" = timeInput("pump_time","Time started:",value=with_tz(Sys.time(),"America/New_York"),seconds=FALSE),
        "pump_time" = timeslide("pump_time","Time started:"),
        "pump_volume" = sliderInput("pump_volume","Volume pumped (fl. oz)",
                                    min=4.0,max=15,value=8,step=.5),
        "notes" = textInput("notes", "Notes (diet, storage differences, etc)")
      ),
      'add_location' = "beforeBegin"
    ),
    "medicine"=list(
      'remove' = c("#pump","#medicine"),
      'add' = list(
        #"medicine_time" = timeInput("medicine_time","Time started:",value=with_tz(Sys.time(),"America/New_York"),seconds=FALSE),
        "medicine_time" = timeslide("medicine_time", "Time taken:"),
        "medicine_type" = selectInput("medicine_type","medicine_type",choices=medicines)
      ),
      'add_location' = "beforeBegin"
    )
  )
}

# define function that gets called on an item in ui_mods within the appropriate observer
modify_ui<-function(button_id,submit=TRUE){
  mod_dict<-gen_ui_mods()
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
submit_info<-function(button_id, input,workbook_id){
  mod_dict<-gen_ui_mods()
  # get fields to be added from mod_dict
  to_add = mod_dict[[button_id]][['add']]
  extract_function<-function(x){
    val = input[[x]]
    if(class(val)=="character"&length(val)>1){
      print(val)
      print('character vector collapsed')
      return(paste(val,collapse=";"))
    }else{
      print(val)
      print(class(val))
      print(length(val))
      return(val)
    }
  }
  inputs_list = lapply(names(to_add),extract_function)
  # force timezone to UTC for any date elements
  # for(n in names(to_add)){
  #   print(n)
  #   entry=inputs_list[[n]]
  #   print(entry)
  #   print(class(entry))
  #   print(length(entry))
  #   if(any(class(entry)=="POSIXct")){
  #     inputs_list[[n]]<-force_tz(entry,'UTC')
  #   }
  #   if(length(entry)>1 & class(entry)=="character"){
  #     inputs_list[[n]]<-paste0(entry,collapse=";")
  #   }
  # }
  responses = data.frame(inputs_list)
  print(responses)
  names(responses)<-names(to_add)
  sheet_append(data=tibble(responses),
               ss = workbook_id,
               sheet=button_id)
  # indicate submission was recorded
  insertUI(selector=paste0("#submit_",button_id),
           where="afterEnd",
           ui=renderText("Entry Submitted"))
}