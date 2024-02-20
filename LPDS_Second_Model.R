################################## LPDS Script ######################################

#Load Packages
library("shrinkTVP")
library("dplyr")
library("gplots")
library("bvarsv")
# Load Data:
setwd("D:/Material - Mestrado FEA-RP/Disciplinas/3º Semestre/Econometria Bayesiana/Pesquisa")
load("BRinf.rda")

# Setting the Dataframe

BRinf = data.frame(BRinf)

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
