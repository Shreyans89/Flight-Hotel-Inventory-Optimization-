library(ggplot2)
library(gganimate)
library(reshape2)




create_pricing_anim<-function(sim,start_datetime=Sys.time()){
  
   start_datetime<-as.POSIXct(start_datetime)
   pricedates<-seq(as.POSIXct(start_datetime),by = "day",length.out = (sim$sim_horizon-1))
   sim_datetime<-lapply(pricedates,function(x){do.call("c",(lapply(seq(x,by = "day",
                                length.out = (sim$nFlights/sim$Period)),
                   function(x){(seq(x,to = x+9*60*60,length.out = sim$Period))})))})
    priceInv<-mapply(pricedates,sim_datetime,as.list.data.frame(sim$Run_inv),
                         as.list.data.frame(sim$Run_prices),cumsum(sim$Run_cum_revenue),
                          FUN=function(pdate,dtm,inv,pcs,cumrev){data.frame(pricing_date=as.Date(pdate),
                                                                flight_datetime=dtm,
                                                                Remaining_Inventory=inv,
                                                                Optimal_prices=pcs,
                                                                 Cumalative_Revenue=cumrev)},SIMPLIFY = F)
    
    priceInv_dat<-do.call("rbind",priceInv) 
    priceInv_dat<-cbind(rbind(priceInv_dat,priceInv_dat),data.frame(grp=c(rep("Optimal Price (USD)",
                                                                      times=nrow(priceInv_dat)),
                                                                       rep("Inventory Level",
                                                                      times=nrow(priceInv_dat)))))
  
    theme_update(text = element_text(size=80))
      p<-ggplot(priceInv_dat,aes(x = flight_datetime))+ 
      facet_wrap(~grp,nrow = 2,scale = "free_y") + 
      geom_line(data = subset(priceInv_dat,grp == 'Optimal Price (USD)'),
                 aes(y = Optimal_prices),size=0.8) + 
       
      geom_bar(data = subset(priceInv_dat,grp == 'Inventory Level'),
               aes(y = Remaining_Inventory),stat = "identity") + 
      labs(x = "Flight Date/Time",y = NULL)
    
    anim<-p + transition_time(pricing_date) +
        labs(title = "Pricing Date: {frame_time}",
        x = "Total Revenue (USD): ${round(min(priceInv_dat$Cumalative_Revenue[priceInv_dat$pricing_date == frame_time]))}") 
    
    animate(anim,nframes = length(pricedates),fps = 0.8,height = 1500, width = 3000)
    
  }



sim_revenue_path<-function(sims,start_datetime=Sys.time()){
  
  start_datetime<-as.POSIXct(start_datetime)
  pricedates<-seq(as.POSIXct(start_datetime),by = "day",length.out = (sims[[1]]$sim_horizon))

df<-cbind(as.data.frame(lapply(sims,function(sim){c(0,cumsum(sim$Run_cum_revenue))})),dates=as.Date(pricedates))
d_melt <- melt(df, id.vars="dates")
df_anim<-do.call("rbind",lapply(d_melt$dates,function(d){cbind(d_melt[d_melt$dates<=d,],Rev_Date=d)}))

# Everything on the same plot
theme_update(text = element_text(size=20))
p<-ggplot(df_anim, aes(dates,value, col=variable)) + 
  geom_point(size=0.8) + 
  stat_smooth() +
  labs(x = "Simulation Date",y = "Cumalative Revenue (USD)")

anim<-p + transition_time(Rev_Date) +
  labs(title = "Revenue As Of: {frame_time}"
       ) 

animate(anim,nframes = length(pricedates),fps = 0.8,height = 1500, width = 1500)
}
