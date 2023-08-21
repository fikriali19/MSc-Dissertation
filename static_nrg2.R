library(readxl)
library(lpSolve)
library(rJava)
library(WriteXLS)
setwd("/Users/fikriali/Desktop/Dissertation/Data")

# model S1*

# data frame
static_nrg_df = data.frame(read_excel(path="data.xlsx", sheet="s1_df"))
df2 <- static_nrg_df[,-1]
rownames(df2) <- static_nrg_df[,1]
static_nrg_df <- df2
rm(df2)

static_nrg_df = static_nrg_df[-c(10,11,18,19),] # remove France, germany, luxembourg and malta

m = 3 # inputs
n = 2 # outputs
k = 23 # DMUs
T = 5 # years

for (t in 1:T){ # run for each year
  
  f.rhs = c(rep(0,k),1) # right hand side values
  f.dir = c(rep(">=",k),"=") # inequality constraint directions
  
  for (j in 1:k) { # run LP for each DMU
    
    f.obj = c(as.numeric(static_nrg_df[j,((t-1)*(m+n)+1):((t-1)*(m+n)+m)]), 
              rep(0,n), 1, -1) # objective function
    con1 = cbind(static_nrg_df[,((t-1)*(m+n)+1):((t-1)*(m+n)+m)],
                 -static_nrg_df[,((t-1)*(m+n)+(m+1)):((t-1)*(m+n)+(m+n))], 
                 matrix(1,k,1), matrix((-1),k,1)) # inequality constraints
    con2=c(rep(0,m), 
           as.numeric(static_nrg_df[j,((t-1)*(m+n)+(m+1)):((t-1)*(m+n)+(m+n))]),
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
    scores_nrg2 = c(1/effvrs)
  } else {
    scores_nrg2 = cbind(scores_nrg2, 1/effvrs)
  }
  
}

rownames(scores_nrg2) = rownames(static_nrg_df)
colnames(scores_nrg2) = c("2016","2017","2018","2019","2020")

