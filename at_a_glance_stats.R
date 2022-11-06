# create a function that generates elements for at a glance page

gen_at_a_glance<-function(current_records){
  # function used to create bulleted list from vector
  vectorBulletList <- function(vector) {
    if(length(vector > 1)) {
      paste0("<ul><li>", 
             paste0(
               paste0(vector, collpase = ""), collapse = "</li><li>"),
             "</li></ul>")   
    }
  }

  # create a vector of latest values that will be rendered as text
  latest_full_feed <- current_records$feed_records%>%
    mutate(descriptive = paste0('<span style="font-weight:bold">Latest full feed: </span>', 
                                start_volume,'fl. oz., ','Started on ',
                                #as.POSIXlt.character(start_time_utc,format='%d %b %H:%M'),
                                start_time%>%as.character(format='%d %b at %H:%M'),
                                '. Finished on ',
                                finish_time%>%as.character(format='%d %b at %H:%M',tz='America/New_York')))%>%
    filter(finished_bottle==TRUE)%>%
    select(descriptive)%>%
    slice(1)%>%
    unlist()
  # find the last vitamin d drop
  last_d<- current_records$feed_records%>%
    mutate(descriptive = paste0('<span style="font-weight:bold">Last Vitamin D drop in: </span>',
                                start_volume, 'fl. oz. bottle started on ',
                                start_time%>%as.character(format='%d %b at %H:%M')))%>%
    filter(vitamin_d_drop==TRUE)%>%
    select(descriptive)%>%
    slice(1)%>%
    unlist()
    
  latest_diaper_change<- current_records$diaper_records%>%
    # make strings of all bool variables
    mutate(contents_str = case_when(Contents=='both'~'pee and poop, ',
                                    TRUE~paste0(Contents,', ')),
           rash_str = case_when(`Diaper Rash`~'diaper rash, ',
                                TRUE~''),
           paste_str = case_when(`Butt Paste`~'butt paste applied, ',
                                 TRUE~''),
           uric_str = case_when(`Uric Crystals`~'uric crystals (dehydrated), ',
                                       TRUE~''))%>%
    mutate(descriptive = paste0('<span style="font-weight:bold">Last diaper change: </span>',
                                as.character(start_time,format='%d %b at %H:%M',tz='America/New_York'),
                                ', ',contents_str, rash_str, paste_str, uric_str))%>%
    slice(1)%>%
    select(descriptive)%>%
    unlist()
  unfinished_bottle_frame<-current_records$feed_records%>%
    # add a string indicating if vitamin d drop was finished
    mutate(descriptive = paste0('<span style="font-weight:bold">Latest unfinished bottle:</span>', 
                                start_volume,'fl. oz., ','Started on ',
                                #as.POSIXlt.character(start_time_utc,format='%d %b %H:%M'),
                                as.POSIXct(start_time,
                                           tz="UTC")%>%as.character(format='%d %b at %H:%M')))%>%
    filter(finished_bottle==FALSE)
  if(nrow(unfinished_bottle_frame)>=1){
    unfinished_str = unfinished_bottle_frame[1,]$descriptive
  }else{
    unfinished_str = '<span style="font-weight:bold">No unfinished bottles</span>'
  }
  # add latest pump
  latest_pump = current_records$pump_records%>%
    mutate(descriptive=paste0('<span style="font-weight:bold">Last Pump: </span>',
                              volume,'fl. oz. pumped on ',
                              as.character(start_time,format='%d %b at %H:%M'))
           )%>%
    slice(1)%>%
    select(descriptive)%>%
    unlist()
  # create a string with latest info that will be rendered as text
  latest_str = paste0('<span style="font-weight:bold">Latest entries:</span><br>',
                      vectorBulletList(c(latest_full_feed,unfinished_str, latest_diaper_change,latest_pump,last_d)))
  result_list = list("latest"=latest_str)
  return(result_list)
}

# create a function to create value box values
gen_VB<-function(current_records,total_range){
  # generate the starting date ranges for each select input
  start_dates<-list("last_day"=with_tz(Sys.time(),'America/New_York')-days(1),
                    "last_week"=with_tz(Sys.time(),'America/New_York')-days(7))
  # select start date based on user range selection
  start_date<-start_dates[[total_range]]
  totals_for_vb<-list("amount_fed" = current_records$feed_records%>%
                        filter(start_time_utc>start_date)%>%
                        summarise(sum(start_volume))%>%
                        unlist(),
                      "amount_pumped" = current_records$pump_records%>%
                        summarise(sum(volume))%>%
                        unlist(),
                      "poop_diapers" = current_records$diaper_records%>%
                        filter(grepl('both|poop',Contents,ignore.case=TRUE))%>%
                        nrow())
  return(totals_for_vb)
}
