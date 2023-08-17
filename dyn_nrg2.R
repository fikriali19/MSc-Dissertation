library(readxl)
library(lpSolve)
library(rJava)
library(WriteXLS)
setwd("/Users/fikriali/Desktop/Dissertation/Data")
rm(list = ls())

# model D1*

# dynamic data frame
df_energy = data.frame(read_excel(path="data.xlsx", sheet = "dyn_nrg_df2"))
df2 <- df_energy[,-1]
rownames(df2) <- df_energy[,1]
df_energy <- df2
rm(df2)

m = 3 # inputs
n = 1 # outputs
B = 1 # (bad) carry-overs
k = 21 # DMUs
T = 5 # years
vars = k*T + m*T + n*T + T # variables
cons = (T-1) + m*T + n*T + T + T # constraints
w = rep(1,n) # output weights

for (h in 1:k) {
  
  # objective function coefficients
  f.obj = c(rep(0,k*T), # lambdas
            rep(0,m*T), # input excesses
            # output shortfalls
            w[1:n]/as.numeric(df_energy[h,(m+1):(m+n)]),
            w[1:n]/as.numeric(df_energy[h,((m+n+1)+(m+1)):((m+n+1)+(m+n))]),
            w[1:n]/as.numeric(df_energy[h,(2*(m+n+1)+(m+1)):(2*(m+n+1)+(m+n))]),
            w[1:n]/as.numeric(df_energy[h,(3*(m+n+1)+(m+1)):(3*(m+n+1)+(m+n))]),
            w[1:n]/as.numeric(df_energy[h,(4*(m+n+1)+(m+1)):(4*(m+n+1)+(m+n))]),
            rep(0,T)) # carry-over excesses
  
  # RHS values for constraints
  f.rhs = c(rep(0,T-1), # continuity
            # input excesses
            as.numeric(df_energy[h,1:m]),
            as.numeric(df_energy[h,((m+n+1)+1):((m+n+1)+m)]),
            as.numeric(df_energy[h,(2*(m+n+1)+1):(2*(m+n+1)+m)]),
            as.numeric(df_energy[h,(3*(m+n+1)+1):(3*(m+n+1)+m)]),
            as.numeric(df_energy[h,(4*(m+n+1)+1):(4*(m+n+1)+m)]),
            # output shortfalls
            as.numeric(df_energy[h,(m+1):(m+n)]),
            as.numeric(df_energy[h,((m+n+1)+(m+1)):((m+n+1)+(m+n))]),
            as.numeric(df_energy[h,(2*(m+n+1)+(m+1)):(2*(m+n+1)+(m+n))]),
            as.numeric(df_energy[h,(3*(m+n+1)+(m+1)):(3*(m+n+1)+(m+n))]),
            as.numeric(df_energy[h,(4*(m+n+1)+(m+1)):(4*(m+n+1)+(m+n))]),
            # carry-over excesses
            as.numeric(df_energy[h,(m+n+1)]),
            as.numeric(df_energy[h,2*(m+n+1)]),
            as.numeric(df_energy[h,3*(m+n+1)]),
            as.numeric(df_energy[h,4*(m+n+1)]),
            as.numeric(df_energy[h,5*(m+n+1)]),
            rep(1,T)) # lambda sum = 1
  
  # all equality constraints
  f.dir = c(rep("=",(T-1)),rep("=",m*T),rep("=",n*T),rep("=",T),rep("=",T))
  
  # continuity constraints (B x T-1)
  con_a = c()
  for (t in 1:(T-1)){
    con = c(# lambdas
      rep(0,k*(t-1)),
      df_energy[,t*(m+n+1)],-df_energy[,(t+1)*(m+n+1)],
      rep(0,k*(T-t-1)),
      # excesses and shortfalls
      rep(0,m*(T)),
      rep(0,n*(T)),
      rep(0,T))
    con_a = rbind(con_a,con)
  }
  
  # input excess constraints (m x T)
  con_b = c()
  for (t in 1:(T)){
    con_bt = c()
    for (i in 1:m){
      con = c(# lambdas
        rep(0,k*(t-1)),
        df_energy[,(t-1)*(m+n+1)+i],
        rep(0,k*(T-t)),
        # input excesses
        rep(0,m*(t-1)),
        rep(0,(i-1)),1,rep(0,(m-i)),
        rep(0,m*(T-t)),
        # output shortfall and carry-over excess
        rep(0,n*(T)),
        rep(0,T))
      con_bt = rbind(con_bt,con)
    }
    con_b = rbind(con_b,con_bt)
  }
  
  # output shortfall constraints (n x T)
  con_c = c()
  for (t in 1:(T)){
    con_ct = c()
    for (j in 1:n){
      con = c(# lambdas
        rep(0,k*(t-1)),
        df_energy[,(t-1)*(m+n+1)+(m+j)],
        rep(0,k*(T-t)),
        # input excess
        rep(0,m*(T)),
        # output shortfall
        rep(0,n*(t-1)),
        rep(0,(j-1)),-1,rep(0,(n-j)),
        rep(0,n*(T-t)),
        #carry-over excess
        rep(0,T))
      con_ct = rbind(con_ct,con)
    }
    con_c = rbind(con_c,con_ct)
  }
  
  # carry-over excess constraints (B x T)
  con_d = c()
  for (t in 1:(T)){
    con = c(# lambdas
      rep(0,k*(t-1)),
      df_energy[,t*(m+n+1)],
      rep(0,k*(T-t)),
      # input excess, output shortfall
      rep(0,m*(T)),
      rep(0,n*(T)),
      # carry-over excess
      rep(0,(t-1)),1,rep(0,T-t))
    con_d = rbind(con_d, con)
  }
  
  # lambda sum = 1
  con_e = c()
  for (t in 1:T){
    con = c(# lambdas
      rep(0,k*(t-1)),
      rep(1,k),
      rep(0,k*(T-t)),
      # excess and shortfall
      rep(0,m*(T)),
      rep(0,n*(T)),
      rep(0,T))
    con_e = rbind(con_e, con)
  }
  
  # combine constraints
  f.con = rbind(con_a,con_b,con_c,con_d,con_e)
  
  results = lp("max", f.obj, f.con, f.dir, f.rhs, scale = 0, 
               compute.sens = TRUE) # run LP
  
  # combining results for all DMUs
  if (h==1) { 
    weights_slacks = results$solution
    effsbm = results$objval
  } else {
    weights_slacks = rbind(weights_slacks,results$solution)
    effsbm = rbind(effsbm, results$objval)
  }
}

# efficiency scores
scores_sbm_energy = 1/((1 + (1/(T*n))*effsbm))
rownames(scores_sbm_energy) <- rownames(df_energy)

# term efficiencies
term_eff_energy = matrix(, nrow = k, ncol = T)
for (h in 1:k) {
  for (t in 1:T) {
    term_eff_energy[h,t] = 1 / 
      (1+(1/n)*
         (sum (w[1:n]/as.numeric(df_energy[h,((t-1)*(m+n+1)+(m+1)):
                                             ((t-1)*(m+n+1)+(m+n))])
                       *weights_slacks[h,(k*T+m*T+n*(t-1)+1):
                                         (k*T+m*T+n*(t-1)+n)])))
  }
}
rownames(term_eff_energy) <- rownames(df_energy)
colnames(term_eff_energy) <- c("2016","2017","2018","2019","2020")