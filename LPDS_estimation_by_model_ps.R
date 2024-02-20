################################## LPDS Script ######################################

#Load Packages
library("shrinkTVP")
library("dplyr")
library("gplots")
library("bvarsv")
# Load Data:

load("BRinf.rda")

# Setting the Dataframe

BRinf = data.frame(BRinf)


library("RColorBrewer")
# Multicore LPDS
library("doParallel")
library("foreach")
# For manipulating dates
library("zoo")
# Load library for controlling number of BLAS threads
library("RhpcBLASctl")

# Define how many periods to calculate LPDS for
Tmax =  nrow(BRinf) - 1
T0   =  Tmax - 11


# Determine number of cores to be used and register parallel backend

# We will redife this everytime, a possible solution for memory consumption

ncores = 6
cl = makeCluster(ncores)
registerDoParallel(cl)
start.time = Sys.time()

# First Model 
lpds_lasso =  foreach(t = T0:Tmax, .combine = "cbind",
                .packages = c("RhpcBLASctl", "shrinkTVP")) %dopar% {
                  
                  niter = 100000
                  nburn = 50000
                  nthin = 10
                  # Set number of BLAS threads, so they don't interfere with each other
                  blas_set_num_threads(1)
                  
                  # Create data_t from all data up to time t and
                  # y_test and x_test from data at time t+1
                  data_test = BRinf[t+1,]
                  data_t    = BRinf[1:t,]
                  
                  # Run MCMC to calculate all LPDS
                  
                  res_lasso = shrinkTVP(Brazil.CPI.IPCA.MoM ~ . , data = data_t, niter = niter, nburn = nburn, nthin = nthin,
                                        learn_a_xi = FALSE, learn_a_tau = FALSE, a_xi = 1, a_tau = 1, 
                                        learn_kappa2 = TRUE, learn_lambda2 = TRUE,
                                        hyperprior_param = list(nu_xi = 1 , nu_tau = 1),display_progress = FALSE, sv = FALSE)
                  
                                                
                  
                  lpds_res  =   LPDS(res_lasso, data_test)
                  
                  rm("res_lasso")
                 
                  return(lpds_res)
                }
end.time   =  Sys.time()
time.taken = end.time - start.time
time.taken
stopCluster(cl)

############################# Memory Overload Solution #################################################

ncores = 6
cl = makeCluster(ncores)
registerDoParallel(cl)
start.time = Sys.time()
# Second Model 
lpds_ng =  foreach(t = T0:Tmax, .combine = "cbind",
                .packages = c("RhpcBLASctl", "shrinkTVP")) %dopar% {
                  
                  niter = 100000
                  nburn = 50000
                  nthin = 10
                  # Set number of BLAS threads, so they don't interfere with each other
                  blas_set_num_threads(1)
                  
                  # Create data_t from all data up to time t and
                  # y_test and x_test from data at time t+1
                  data_test = BRinf[t+1,]
                  data_t    = BRinf[1:t,]
                  
                  # Run MCMC to calculate all LPDS
                                
                  res_ng    = shrinkTVP(Brazil.CPI.IPCA.MoM ~ ., data =data_t,niter = niter, nburn = nburn, nthin = nthin,
                                        learn_a_xi = FALSE, learn_a_tau = FALSE, a_xi = 10,
                                        a_tau = 10, learn_kappa2 = TRUE, learn_lambda2 = TRUE,
                                        hyperprior_param = list(nu_xi = 1, nu_tau =1),
                                        display_progress = FALSE, sv = FALSE)
                  
                                                
                  
                  lpds_res  =   LPDS(res_ng, data_test)
                  
                  rm("res_ng")
                 
                  return(lpds_res)
                }

end.time   =  Sys.time()
time.taken = end.time - start.time
time.taken
stopCluster(cl)


########################### Memory Overload Solution ##########################
ncores = 6
cl = makeCluster(ncores)
registerDoParallel(cl)
start.time = Sys.time()

# Third Model 
lpds_lasso_sv =  foreach(t = T0:Tmax, .combine = "cbind",
                .packages = c("RhpcBLASctl", "shrinkTVP")) %dopar% {
                  
                  niter = 100000
                  nburn = 50000
                  nthin = 10
                  # Set number of BLAS threads, so they don't interfere with each other
                  blas_set_num_threads(1)
                  
                  # Create data_t from all data up to time t and
                  # y_test and x_test from data at time t+1
                  data_test = BRinf[t+1,]
                  data_t    = BRinf[1:t,]
                  
                  # Run MCMC to calculate all LPDS
                                
                  res_lasso_sv = shrinkTVP(Brazil.CPI.IPCA.MoM ~ ., data = data_t, niter = niter, nburn = nburn, nthin = nthin,
                                           learn_a_xi = FALSE, learn_a_tau = FALSE, a_xi = 1,
                                           a_tau = 1, learn_kappa2 = TRUE, learn_lambda2 = TRUE,
                                           hyperprior_param = list(nu_xi = 1, nu_tau = 1),display_progress = FALSE, sv = TRUE)
                  
                                                
                  
                  lpds_res  =   LPDS(res_lasso_sv , data_test)
                  
                  rm("res_lasso_sv")
                 
                  return(lpds_res)
                }
end.time   =  Sys.time()
time.taken = end.time - start.time
time.taken
stopCluster(cl)
############################## Memory Overload Solution  ###########################################
ncores = 6
cl = makeCluster(ncores)
registerDoParallel(cl)
start.time = Sys.time()
# Fourth Model 

lpds_ng_sv =  foreach(t = T0:Tmax, .combine = "cbind",
                .packages = c("RhpcBLASctl", "shrinkTVP")) %dopar% {
                  
                  niter = 100000
                  nburn = 50000
                  nthin = 10
                  # Set number of BLAS threads, so they don't interfere with each other
                  blas_set_num_threads(1)
                  
                  # Create data_t from all data up to time t and
                  # y_test and x_test from data at time t+1
                  data_test = BRinf[t+1,]
                  data_t    = BRinf[1:t,]
                  
                  # Run MCMC to calculate all LPDS
 	          res_ng_sv    = shrinkTVP(Brazil.CPI.IPCA.MoM ~ ., data = data_t,niter = niter, nburn = nburn, nthin = nthin,
                                           learn_a_xi = FALSE, learn_a_tau = FALSE, a_xi = 10,
                                           a_tau = 10, learn_kappa2 = TRUE, learn_lambda2 = TRUE,
                                           hyperprior_param = list(nu_xi = 1, nu_tau =1),
                                           display_progress = FALSE,
                                           sv = TRUE,sv_param = list(a0_sv = 20,Bmu =100))
      
                  
                                                
                  
                  lpds_res  =   LPDS(res_ng_sv , data_test)
                  
                  rm("res_ng_sv")
                 
                  return(lpds_res)
                }
end.time   =  Sys.time()
time.taken = end.time - start.time
time.taken
stopCluster(cl)


lpds = rbind(lpds_lasso,lpds_ng,lpds_lasso_sv,lpds_ng_sv)
cumu_lpds = apply(lpds, 1, cumsum)
color = brewer.pal(4 ,"Set2")


# Plot results
par(mfrow=c(1,1))
colnames(cumu_lpds) = c("Hierachical Lasso","Hierachical Normal-Gamma","SV-LASSO","SV-NG")

matplot(cumu_lpds, type = "l", ylab = "Cumulative LPDS",
        xaxt = "n", xlab = "", col = color, lty=1, lwd=2 )

# Extract labels from the data frame

labs = as.yearmon(rownames(BRinf))[T0:Tmax + 1][c(FALSE, TRUE)]

# Create custom axis labels
axis(1, at = (1:length(T0:Tmax))[c(FALSE, TRUE)], labels = FALSE)
text(x=(1:length(T0:Tmax))[c(FALSE, TRUE)],
     y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), srt=45, adj=1,labels= labs, xpd=TRUE)
# Add legend
legend("topright", colnames(cumu_lpds), col = color,
       lty = 1,lwd = 2, bty = "n", cex = 0.8)

