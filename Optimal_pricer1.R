library("CVXR")
library("Matrix")

optimal_pricer_1<-function(X_init,nFlights=60,Period=3,nPeriods=20,Umin=3000,Umax=9000,
                         Blist=BL_1,Dlist=DL_1,s=100){
  Bmat<-bdiag(Blist)
  Bmat<-as(object = Bmat,Class = "dgCMatrix")
  Dmat<-data.frame(Dlist)
  
  ## Setting up objective and constraints matrices (in block diag form)
  quadlist<-lapply(Blist,FUN = function(x){chol(symmpart(x))})
  quadmat<-bdiag(quadlist)
  quadmat<-as(object = quadmat,Class = "dgCMatrix")
  
  
  #Constant part (K) to c0mplete the  squares
  Kmat<-mapply(Dlist,Blist,FUN = function(D,B){solve.default(a = 2*chol(symmpart(B)),b = D)},SIMPLIFY = T)
  
  # creating matrix for equality (dynamics) constraints x_k+1=A(x_k-(D-BU_k))
  A<-rbind(cbind(matrix(data = 0,nrow = nFlights-Period,ncol =Period),diag(nFlights-Period))
           ,matrix(data = 0,nrow = Period,ncol =nFlights))
  
  ## Calling optimizer CVX begin
  
  #creating inventory trajectory and optimal price variables,X_inv==state of inventory after mth period
  X_inv<-Variable(nFlights,nPeriods,nonneg=T)
  u<-Variable(nFlights,nPeriods)
  
  
  # creating the (negative) revenue-loss objective (without the price independent term,thus a positive Qty)
  revenue_loss<-cvxr_norm(quadmat%*%vec(u)-vec(Kmat))
  
  # creating contraints
  Demands<-reshape_expr(vec(Dmat)-Bmat%*%vec(u),new_dim = c(nFlights,nPeriods))
  X_begin<-cbind(matrix(X_init,nrow = nFlights,ncol = 1),X_inv[,min(1,nPeriods-1):(nPeriods-1)])
  X_end<-X_inv
  New_inv<-matrix(rep(c(numeric(nFlights-Period),rep(s,Period)),nPeriods),nrow = nFlights,ncol = nPeriods)
  Dynamics<-X_end-(A%*%(X_begin-Demands)+New_inv)
  
  # maximize revenue s.t constraints
  objective <- Minimize(revenue_loss)
  constraints <- list(Demands >= 0,X_begin-Demands >= 0, Dynamics==0,Umax>=u,
                      u>=Umin)
  
  prob<- Problem(objective, constraints)
  sol<-solve(prob,solver="ECOS_BB",num_iter=100)
  solution<-list(u=sol$getValue(u),demands=sol$getValue(Demands),status=sol$status,
                 num_iters=sol$num_iters,solve_time=sol$solve_time,A=A,New_inv=New_inv)
  solution
  
}






