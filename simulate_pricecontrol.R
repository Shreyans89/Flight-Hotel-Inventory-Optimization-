
Run_sim_pricecontrol<-function(X_init,sim_horizon=21,nFlights=60,Period=3,nPeriods=20,Umin=3000,
                               Umax=9000,Blist=BL_1,Dlist=DL_1,s=100){
                               Run_inv<-data.frame(X_init)
                               Run_prices<-matrix(nrow = nFlights)
                               Run_revenue<-numeric(1)
                               Run_status<-""
                               Run_iter<-numeric(0)
                              Run_time<-numeric(0)
                              Run_sales<-numeric(0)
                              Run_del_u<-numeric(0)
                              Run_cum_revenue<-numeric(0)
                                
             for (i in 1:(sim_horizon-1)){opt_horizon<-min(nPeriods,(sim_horizon-(i-1)))
                                         sol<-optimal_pricer_1(X_init,nFlights,Period,
                                              nPeriods = opt_horizon
                                            ,Umin,Umax,Blist = Blist[1:opt_horizon],Dlist = Dlist[1:opt_horizon],s = s)
             
                                        opt_prices<-sol$u[,1]
                                        del_u<-max(opt_prices)-min(opt_prices)
                                        demands<-rpois(nFlights,sol$demands[,1])
                                        A<-sol$A
                                        New_inv<-sol$New_inv
                                        sales<-pmin(X_init,demands)
                                        X_init<-(A%*%(X_init-sales)+New_inv[,1])
                                      Run_inv<-cbind(Run_inv,X_init)
                                      Run_prices<-cbind(Run_prices,opt_prices)
                                      Run_revenue<-Run_revenue+t(opt_prices)%*%sales
                                      Run_cum_revenue<-c(Run_cum_revenue,t(opt_prices)%*%sales)
                                      Run_status<-c(Run_status,sol$status)
                                      Run_iter<-c(Run_iter,sol$num_iters)
                                      Run_time<-c(Run_time,sol$solve_time)
                                      Run_sales<-c(Run_sales,sales)
                                      Run_del_u<-c(Run_del_u,del_u)
                                      
                                      print(i)
                                }
                    list(Run_inv=Run_inv[,-ncol(Run_inv)],Run_prices=as.data.frame(Run_prices[,-1]),Run_revenue=Run_revenue,
                           Run_status=Run_status,Run_iter=Run_iter,Run_time=Run_time,Run_cum_revenue=Run_cum_revenue,
                           Run_sales=Run_sales,nFlights=nFlights,nPeriods=nPeriods,Period=Period,sim_horizon=sim_horizon)}
  
  







