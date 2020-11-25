library("CVXR")
library("Matrix")


create_problem_data<-function(nFlights,nPeriods,s,Umax,Period){
# mean Demands at equilibrium price (at price deviation 0), n demands for m periods each, matrix
# of nXm/list of m n-vectors umax=9000,umin=3000 3/3 demand to fill new_inventory
D<-4.0*((1:nFlights)*2/(nFlights*(nFlights+1)))*Period*s ## Linearly reducing demands

Dmat<-matrix(rep(D,times=nPeriods),nrow = nFlights,ncol =nPeriods)
DL_1<-as.list(data.frame(Dmat))

# Price Elasticity Of Demand (as list) list of matrices(nXn) B_i i:1:m (one n by n matrix for each of
#the m periods ) (assumed diag,values adjusted to make 0 demand at Umax)
BL_1<-lapply(DL_1,FUN = function(x){diag(x)/(Umax)})
list(DL_1=DL_1,BL_1=BL_1)}






