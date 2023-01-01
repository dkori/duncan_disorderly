# function to generate select inputs for baseline_pump
gen_pump_options<-function(current_records){
  pump_labels = current_records[['pump_records']]%>%
    mutate(descriptive=paste0(volume,'fl. oz. pumped on ',
                              as.character(start_time,format='%d %b at %H:%M')))%>%
    select(descriptive)%>%
    unlist()
  pump_choices = current_records[['pump_records']]%>%
    mutate(start_time_num = as.POSIXlt(start_time))%>%
    select(start_time)
  pump_choice_vec<-pump_choices$start_time
  names(pump_choice_vec)<-pump_labels
  return(pump_choice_vec)
}
# functions to generate trend plots
gen_feed_pump_step<-function(current_records,baseline_pump,trend_curve){
  # calculate cumualtive feed records
  cumu_feed<-current_records[['feed_records']]%>%
    filter(finished_bottle==T & !is.na(finish_time))%>%
    # assume unnoted finishes occurred 2 hrs after start
    # mutate(finish_time = ifelse(is.na(finish_time),start_time+hours(2),
    #                    finish_time))%>%
    mutate(series='Fl oz fed')%>%
    bind_rows(current_records[['formula_records']])%>%
    mutate(series=replace_na(series,"Formula supplemented"))%>%
    filter(start_time>=baseline_pump)%>%
    arrange(start_time)%>%
    mutate(total_volume=cumsum(start_volume),
           ylag = lag(total_volume,default=0)
    )%>%
    select(start_time,finish_time,series,total_volume,volume=start_volume,ylag)
  cumu_pump<-current_records[['pump_records']]%>%
    arrange(start_time)%>%
    filter(start_time>=baseline_pump)%>%
    mutate(total_volume=cumsum(volume),
           series='Fl oz pumped',
           finish_time = start_time+minutes(20),
           ylag = lag(total_volume,default=0))%>%
    select(start_time,finish_time,series,total_volume,volume,ylag)
  # create horizontal lines connecting the segments for feed and pump
  feed_horizontal<-cumu_feed%>%
    mutate(start2=finish_time,
           finish2 = lead(start_time),
           ylag = total_volume)%>%
    select(start_time=start2, finish_time=finish2,series,total_volume,volume,ylag)
  pump_horizontal<-cumu_pump%>%
    mutate(start2=finish_time,
           finish2 = lead(start_time),
           ylag = total_volume,default=0)%>%
    select(start_time=start2, finish_time=finish2,series,total_volume,volume,ylag)
  p<-bind_rows(cumu_feed,cumu_pump)%>%
    #TODO: fix finish time display
    #mutate(finish_time=as.POSIXct(finish_time,tz='UTC'))%>%
    ggplot()+
    geom_segment(data=bind_rows(feed_horizontal,pump_horizontal),
                 aes(x=start_time,y=ylag,
                     xend=finish_time,
                     yend=total_volume,color=series),
                 linetype='dotted')+
    geom_segment(aes(x=start_time,y=ylag,
                     xend=finish_time,
                     yend=total_volume,color=series,
                     text=paste(paste0('started: ',start_time),
                                paste0('finished: ', finish_time),
                                paste0('volume: ', volume,' fl. oz'),
                                sep='<br>'
                     )),lineend="round")+
    labs(x='',
         y='Cumulative Fluid Ounces',
         title= 'Cumulative fluid ounces pumped and fed',
         color='')+
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%b %d\n%H:%M")#+
  #theme(legend.position="bottom")+
  #theme_minimal()
  # create tables of horizontal segments for 
  # add a trend curve to p if checkbox is selected
  if(trend_curve){
    p<-p+
      geom_smooth(data=cumu_feed,
                  aes(x=finish_time,
                      y=total_volume),
                  method="glm",
                  formula=y~poly(x,2),
                  alpha=.2)
    
  }
  return(
    p%>%
      ggplotly(tooltip="text")%>%
      layout(legend=list(orientation='h',y=-.15))
  )
  
}
# for diaper vs feed chart
gen_diaper_feed<-function(current_records){
  # shape data for segments
  feed_segments<-current_records[['feed_records']]%>%
    #filter(start_time>(Sys.time()-days(7)))%>%
    # add time of day variables
    mutate(start_tod = update(start_time,year=2000,month=1,day=1),
           finish_tod = update(finish_time,year=2000,month=1,day=1),
           # get dates from datetimes
           start_day = as.Date(start_time,tz="UTC"),
           finish_day = as.Date(finish_time,tz="UTC"))%>%
    # remove edge cases of bottles started and finished on different days
    filter(start_day==finish_day & finished_bottle==TRUE)%>%
    select(start_tod,finish_tod,start_day,finish_day,start_volume)
  
  formula_segments<-current_records[['formula_records']]%>%
    mutate(finish_time=start_time+hours(1),
           start_tod=update(start_time,year=2000,month=1,day=1),
           finish_tod = update(finish_time,year=2000,month=1,day=1),
           # get dates from datetimes
           start_day = as.Date(start_time,tz="UTC"),
           finish_day = as.Date(finish_time,tz="UTC"))
  # shape feed data for points/shapes
  feed_points<-bind_rows(
    feed_segments%>%select(day=start_day,tod=start_tod,start_volume),
    feed_segments%>%select(day=start_day,tod=finish_tod,start_volume)
  )%>%
    mutate(shape=factor('bottle start/finish',
                        levels=c('bottle start/finish',
                                 'diaper change',
                                 'formula start/finish')))
  formula_points<-bind_rows(
    formula_segments%>%select(day=start_day,tod=start_tod,start_volume),
    formula_segments%>%select(day=start_day,tod=finish_tod,start_volume)
  )%>%
    mutate(shape=factor('formula start/finish',
                              levels=c('bottle start/finish',
                                       'diaper change',
                                       'formula start/finish')))
  diaper_data<-current_records[['diaper_records']]%>%
    #filter(start_time>(Sys.time()-days(7)))%>%
    mutate(tod=update(start_time,year=2000,month=1,day=1),
           day=as.Date(start_time,tz="UTC"),
           shape=factor('diaper change',
                              levels=c('bottle start/finish',
                                       'diaper change',
                                       'formula start/finish')))
  return(
    ggplot()+
           geom_point(data = bind_rows(feed_points,formula_points,
                                       diaper_data
                                       ),
                      aes(x=day,y=tod,shape=shape))+
           geom_segment(data=bind_rows(feed_segments,formula_segments),
                        aes(x=start_day,y=start_tod,
                             xend=finish_day,yend=finish_tod,color=start_volume),
                        #linetype='dotted',
                        alpha=.2)+
           scale_shape_manual(labels=c("bottle start/finish",
                                       "diaper change",
                                       "formula start/finish"),
                              values=enc2utf8(c('\U0001F37C','\U0001F4A9','\U0001F9EA'))
                              #values =c('\uD83D\uDC18','\U1F4A9','\U1F9EA')
                              )+
           geom_point(data = bind_rows(feed_points,formula_points),
                      aes(x=day,y=tod,color=start_volume),
                      alpha=.5,stroke=F)+
             scale_color_viridis_c()+
             #geom_point(data=diaper_data,aes(x=day,y=tod),shape='\ud83d\udca9')+
             scale_y_datetime(date_breaks="3 hours",
                              date_labels='%H:%M')+
             labs(title="diaper changes and bottle starts",
                  y="time of day",
                  x="date",
                  shape="",
                  color="bottle\nvolume")+
             theme(legend.position="bottom")+
           guides(shape=guide_legend(nrow=3),byrow=T)
           )
}