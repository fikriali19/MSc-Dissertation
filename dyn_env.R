library(readxl)
library(lpSolve)
library(rJava)
library(WriteXLS)
setwd("/Users/fikriali/Desktop/Dissertation/Data")

# dynamic environmental efficiency

# model D2

# dynamic data frame
df_enviro = data.frame(read_excel(path="data.xlsx", sheet = "dynamic_3"))
df2 <- df_enviro[,-1]
rownames(df2) <- df_enviro[,1]
df_enviro <- df2
rm(df2)

m = 1 # variable inputs
n = 2 # outputs
B = 1 # carry-overs
k = 27 # DMUs
T = 5 # years
vars = k*T + m*T + n*T + T # variables 
cons = (T-1) + m*T + n*T + T + T + T # constraints
w = c(rep(1,n)) # output weights

for (h in 1:k) {
  
  # objective function
  f.obj = c(# lambdas
            rep(0,k*T),
            # input excess
            rep(0,m*T),
            # output shortfall
            w[1:n]/as.numeric(df_enviro[h,(m+1):(m+n)]),
            w[1:n]/as.numeric(df_enviro[h,((m+n+1)+(m+1)):((m+n+1)+(m+n))]),
            w[1:n]/as.numeric(df_enviro[h,(2*(m+n+1)+(m+1)):(2*(m+n+1)+(m+n))]),
            w[1:n]/as.numeric(df_enviro[h,(3*(m+n+1)+(m+1)):(3*(m+n+1)+(m+n))]),
            w[1:n]/as.numeric(df_enviro[h,(4*(m+n+1)+(m+1)):(4*(m+n+1)+(m+n))]),
            # carry-over excess
            rep(0,T))
  
  # RHS values for constraints
  f.rhs = c(rep(0,T-1), # continuity
            #input excess (xi0t)
            as.numeric(df_enviro[h,1:m]),
            as.numeric(df_enviro[h,((m+n+1)+1):((m+n+1)+m)]),
            as.numeric(df_enviro[h,(2*(m+n+1)+1):(2*(m+n+1)+m)]),
            as.numeric(df_enviro[h,(3*(m+n+1)+1):(3*(m+n+1)+m)]),
            as.numeric(df_enviro[h,(4*(m+n+1)+1):(4*(m+n+1)+m)]),
            # output shortfall (yj0t)
            as.numeric(df_enviro[h,(m+1):(m+n)]),
            as.numeric(df_enviro[h,((m+n+1)+(m+1)):((m+n+1)+(m+n))]),
            as.numeric(df_enviro[h,(2*(m+n+1)+(m+1)):(2*(m+n+1)+(m+n))]),
            as.numeric(df_enviro[h,(3*(m+n+1)+(m+1)):(3*(m+n+1)+(m+n))]),
            as.numeric(df_enviro[h,(4*(m+n+1)+(m+1)):(4*(m+n+1)+(m+n))]),
            # carry-over excess (zb0t)
            as.numeric(df_enviro[h,(m+n+1)]),
            as.numeric(df_enviro[h,2*(m+n+1)]),
            as.numeric(df_enviro[h,3*(m+n+1)]),
            as.numeric(df_enviro[h,4*(m+n+1)]),
            as.numeric(df_enviro[h,5*(m+n+1)]),
            # lambda sum = 1
            rep(1,T),
            # fixed inputs (x_fix_i0t)
            as.numeric(df_enviro[h,(m+n+1)*T+1]),
            as.numeric(df_enviro[h,(m+n+1)*T+2]),
            as.numeric(df_enviro[h,(m+n+1)*T+3]),
            as.numeric(df_enviro[h,(m+n+1)*T+4]),
            as.numeric(df_enviro[h,(m+n+1)*T+5]))
  
  # all equality constraints
  f.dir = c(rep("=",(T-1)),rep("=",m*T),rep("=",n*T),rep("=",T),rep("=",T),
            rep("=",T))
  
  # continuity constraints
  con_a = c()
  for (t in 1:(T-1)){
    con = c(rep(0,k*(t-1)),
            df_enviro[,t*(m+n+1)],-df_enviro[,(t+1)*(m+n+1)],
            rep(0,k*(T-t-1)), 
            
            rep(0,m*(T)), 
            
            rep(0,n*(T)),
            
            rep(0,T))
    con_a = rbind(con_a,con)
  }
  
  # input excess constraints
  con_b = c()
  for (t in 1:(T)){
    con_bt = c()
    for (i in 1:m){
      con = c(rep(0,k*(t-1)),
              df_enviro[,(t-1)*(m+n+1)+i],
              rep(0,k*(T-t)),
              
              rep(0,m*(t-1)),
              rep(0,(i-1)),1,rep(0,(m-i)),
              rep(0,m*(T-t)),
              
              rep(0,n*(T)),
              
              rep(0,(T)))
      con_bt = rbind(con_bt,con)
    }
    con_b = rbind(con_b,con_bt)
  }
  
  # output shortfall constraints
  con_c = c()
  for (t in 1:(T)){
    con_ct = c()
    for (j in 1:n){
      con = c(rep(0,k*(t-1)),
              df_enviro[,(t-1)*(m+n+1)+(m+j)],
              rep(0,k*(T-t)),
              
              rep(0,m*(T)),
              
              rep(0,n*(t-1)),
              rep(0,(j-1)),-1,rep(0,(n-j)),
              rep(0,n*(T-t)),
              
              rep(0,(T)))
      con_ct = rbind(con_ct,con)
    }
    con_c = rbind(con_c,con_ct)
  }
  
  # carry-over excess constraints
  con_d = c()
  for (t in 1:(T)){
    con = c(rep(0,k*(t-1)),
            df_enviro[,t*(m+n+1)],
            rep(0,k*(T-t)),
            
            rep(0,m*(T)),
            
            rep(0,n*(T)),
            
            rep(0,(t-1)),1,rep(0,T-t))
    con_d = rbind(con_d, con)
  }
  
  # lambda sum = 1
  con_e = c()
  for (t in 1:T){
    con = c(rep(0,k*(t-1)),
            rep(1,k),
            rep(0,k*(T-t)),
            
            rep(0,m*(T)),
            
            rep(0,n*(T)),
            
            rep(0,T))
    con_e = rbind(con_e, con)
  }
  
  # fixed input constraints
  con_f = c()
  for (t in 1:(T)){
    con = c(rep(0,k*(t-1)),
            df_enviro[,(m+n+1)*T+t],
            rep(0,k*(T-t)),
            
            rep(0,m*(T)),
            
            rep(0,n*(T)),
            
            rep(0,T))
    con_f = rbind(con_f,con)
  }
  
  # combine constraints
  f.con = rbind(con_a,con_b,con_c,con_d,con_e,con_f)
  
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
scores_sbm_enviro = 1/((1 + (1/(T*n))*effsbm)) 
rownames(scores_sbm_enviro) <- rownames(df_enviro)

# term efficiencies
term_eff_enviro = matrix(, nrow = k, ncol = T)
for (h in 1:k) {
  for (t in 1:T) {
    term_eff_enviro[h,t] = 1 / 
      (1+(1/n)*
         (sum((w[1:n]/
                 as.numeric(df_enviro[h,((t-1)*(m+n+1)+(m+1)):
                                        ((t-1)*(m+n+1)+(m+n))]))
                *weights_slacks[h,(k*T+m*T+n*(t-1)+1):(k*T+m*T+n*(t-1)+n)])))
  }
}
rownames(term_eff_enviro) <- rownames(df_enviro)
colnames(term_eff_enviro) <- c("2016","2017","2018","2019","2020")

# weight and slack variable optimal solutions
rownames(weights_slacks) <- rownames(df_enviro)
colnames(weights_slacks) <- c(rep("lambda",k*T),rep("s_i",m*T),rep("s_j",n*T),
                              rep("s_co",T))
weights_env = weights_slacks[,1:(k*T)]
slacks_env = weights_slacks[,(k*T+1):vars]

# projections
proj_data_env = matrix(,nrow = nrow(df_enviro), ncol = ncol(df_enviro))
rownames(proj_data_env) = rownames(df_enviro)
colnames(proj_data_env) = colnames(df_enviro)

for (h in 1:k){
  for (t in 1:T){
    for(i in 1:m){
      proj_data_env[h,((t-1)*(m+n+B))+i] = 
        as.numeric(df_enviro[h,((t-1)*(m+n+B))+i]) - slacks_env[h,((t-1)*m)+i]
    }
    for(j in 1:n){
      proj_data_env[h,((t-1)*(m+n+B))+(m+j)] = 
        as.numeric(df_enviro[h,((t-1)*(m+n+B))+(m+j)]) 
      + slacks_env[h,(T*m)+((t-1)*n)+j]
    }
    for(b in 1:B){
      proj_data_env[h,((t-1)*(m+n+B))+(m+n+b)] = 
        as.numeric(df_enviro[h,((t-1)*(m+n+B))+(m+n+b)]) - 
        slacks_env[h,(T*m)+(T*n)+((t-1)*B)+b]
    }
  }
}

for (h in 1:k){
  for (i in 21:25){
    proj_data_env[h,i] = as.numeric(df_enviro[h,i])
  }
}

# FEI values
FEI_env = matrix(,nrow = nrow(df_enviro), ncol = ncol(df_enviro))
rownames(FEI_env) = rownames(df_enviro)
colnames(FEI_env) = colnames(df_enviro)
for(h in 1:k){
  for(i in 1:(T*(m+n+B))){
    FEI_env[h,i] = as.numeric(df_enviro[h,i])/as.numeric(proj_data_env[h,i]) - 1
  }
}
