# model S1
static_nrg_df = data.frame(read_excel(path="data.xlsx", sheet="s2_df"))
df2 <- static_nrg_df[,-1]
rownames(df2) <- static_nrg_df[,1]
static_nrg_df <- df2
rm(df2)

m = 3 # inputs
n = 2 # outputs
k = 27 # DMUs
T = 5 # years

f.rhs = c(rep(0,k),1) # right hand side values
f.dir = c(rep(">=",k),"=") # inequality constraint directions

for (t in 1:T){ # run for each year
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
    scores_nrg = c(1/effvrs)
  } else {
    scores_nrg = cbind(scores_nrg, 1/effvrs)
  }
  
}

rownames(scores_nrg) = rownames(static_nrg_df)
colnames(scores_nrg) = c("2016","2017","2018","2019","2020")

