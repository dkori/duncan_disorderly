# function to retrieve current records
retrieve_current<-function(workbook_id){
  # read in the current logged data
  # read in diaper and feed records
  current_records<-list()
  current_records[['diaper_records']]<-read_sheet(workbook_id,"diaper")%>%
    # google sheet should indicate eastern time even though it is stored in UTC, so force tz
    mutate(start_time = force_tz(`Time`,tz='America/New_York'))%>%
    arrange(desc(start_time))%>%
    select(start_time,Contents,`Diaper Rash`,`Rash Avoidance`)
  
  current_records[['feed_records']]<-read_sheet(workbook_id,"bottle_start")%>%
    mutate(finish_time = force_tz(finish_time,'America/New_York'),
           start_time = force_tz(start_time, 'America/New_York'))%>%
    # clear finish time if bottle is unfinished
    mutate(finish_time = case_when(finished_bottle==TRUE~finish_time,
                                   TRUE~as.POSIXct(NA)))%>%
    #mutate(start_time = as.POSIXct(start_time_utc,tz='EDT'))%>%
    arrange(desc(start_time))%>%
    select(start_time,start_volume,vitamin_d_drop, finished_bottle,finish_time,start_fuss,start_sleep,finish_fuss,finish_sleep,discarded)
  current_records[['pump_records']]<-read_sheet(workbook_id,"pump")%>%
    mutate(start_time = force_tz(start_time, 'America/New_York'))%>%
    arrange(desc(start_time))
  current_records[['formula_records']]<-read_sheet(workbook_id,"formula")%>%
    mutate(start_time=force_tz(start_time,'America/New_York'),
           finish_time=start_time+hours(1))%>%
    arrange(desc(start_time))%>%
    select(start_time,finish_time,start_volume=volume)
  return(current_records)
}