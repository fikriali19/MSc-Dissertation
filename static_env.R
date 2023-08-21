library(readxl)
library(lpSolve)
library(rJava)
library(WriteXLS)
setwd("/Users/fikriali/Desktop/Dissertation/Data")

# model S2
static_env_df = data.frame(read_excel(path="data.xlsx",sheet="s2_df"))
df2 <- static_env_df[,-1]
rownames(df2) <- static_env_df[,1]
static_env_df <- df2
rm(df2)

m = 2 # inputs
n = 3 # outputs
k = 27 # DMUs
T = 5 # years

for (t in 1:T){
  
  f.rhs = c(rep(0,k),1) # right hand side values
  f.dir = c(rep(">=",k),"=") # inequality constraint directions
  
  for (j in 1:k) { # run LP for each DMU
    
    f.obj = c(as.numeric(static_env_df[j,((t-1)*(m+n)+1):((t-1)*(m+n)+m)]), 
              rep(0,n), 1, -1) # objective function
    con1 = cbind(static_env_df[,((t-1)*(m+n)+1):((t-1)*(m+n)+m)],
                 -static_env_df[,((t-1)*(m+n)+(m+1)):((t-1)*(m+n)+(m+n))], 
                 matrix(1,k,1), matrix((-1),k,1)) # inequality constraints
    con2=c(rep(0,m), 
           as.numeric(static_env_df[j,((t-1)*(m+n)+(m+1)):((t-1)*(m+n)+(m+n))]),
           0,0) # equal to 1
    f.con = rbind(con1, con2)
    
    results = lp("min", f.obj, f.con, f.dir, f.rhs, scale = 0, 
                 compute.sens = TRUE) # run LP
    
    multipliers = results$solution # weights and slacks
    u0 = multipliers[n+m+1] - multipliers[n+m+2] # slack variable
    
    # combining results for all DMUs
    if (j==1) { 
      weights = c(multipliers[seq(1,n+m)],u0)
      effvrs = results$objval
      lambdas = results$duals[seq(1,k+1)]
    } else {
      weights = rbind(weights,c(multipliers[seq(1,n+m)],u0))
      effvrs = rbind(effvrs, results$objval)
      lambdas = rbind(lambdas, results$duals[seq(1,k+1)])
    }
  }
  
  # combining scores for each year
  if (t==1) {
    scores_env = c(1/effvrs)
    weights_env = weights
    lambdas_env = lambdas
  } else {
    scores_env = cbind(scores_env, 1/effvrs)
    weights_env = cbind(weights_env, weights)
    lambdas_env = cbind(lambdas_env, lambdas)
  }
}

rownames(scores_env) = rownames(static_env_df)
colnames(scores_env) = c("2016","2017","2018","2019","2020")
