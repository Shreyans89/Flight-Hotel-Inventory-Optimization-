library("parallel")
library("pbapply")


source("Inventory_Graphing.R")
source("Optimal_pricer1.R")
source("simulate_pricecontrol.R")
source("create_problem_data.R")

nFlights= 60
s=100
nPeriods= 20
Period= 3
Umin= 3000
Umax = 9000
BL_1<-create_problem_data(nFlights,nPeriods,s,Umax,Period)$BL_1
DL_1<-create_problem_data(nFlights,nPeriods,s,Umax,Period)$DL_1

X_init<-sample(1:s,size = nFlights,replace = T)
sim<-Run_sim_pricecontrol(X_init,sim_horizon = 21,nFlights,Period,nPeriods,Umin,Umax,Blist = BL_1,Dlist = DL_1,s = s)
sim_M<-Run_sim_pricecontrol(X_init,sim_horizon = 21,nFlights,Period,nPeriods=10,Umin,Umax,Blist = BL_1[1:10],Dlist = DL_1[1:10],s)
sim_G<-Run_sim_pricecontrol(X_init,sim_horizon = 21,nFlights,Period,nPeriods=4,Umin,Umax,Blist = BL_1[1:4],Dlist = DL_1[1:4],s)

## create the pricing animation
create_pricing_anim(sim)


## code to run a 100 simulations in parallel
# Detect the number of available cores and create cluster
cl <- parallel::makeCluster(detectCores()-2)
# Run parallel computation
clusterExport(cl, c("optimal_pricer_1","Run_sim_pricecontrol","s"))
clusterEvalQ(cl, {library("CVXR"); library("Matrix")})
clusterExport(cl=cl, c("BL_1", "DL_1"))

nsim<-100
simList<-lapply(1:nsim,FUN = function(x){X_init})



compare_pricers<-pblapply(simList,function(x){
    list(Full_Horizon=Run_sim_pricecontrol(X_init = x,nPeriods = length(BL_1)),
    Greedy_Horizon=Run_sim_pricecontrol(X_init = x,nPeriods = 2,Blist = BL_1[1:2],Dlist = DL_1[1:2]),
    Medium_Horizon=Run_sim_pricecontrol(X_init = x,nPeriods = 10,Blist = BL_1[1:10],Dlist =DL_1[1:10]))},
    cl = cl)


saveRDS(object = compare_pricers,file = "compare_pricers.rds")

revs<-lapply(compare_pricers,function(x){data.frame(Full_Horizon=x$Full_Horizon$Run_revenue,
                                              Medium_Horizon=x$Medium_Horizon$Run_revenue,
                                              Greedy_Horizon=x$Greedy_Horizon$Run_revenue)})

revs<-do.call("rbind",revs)


