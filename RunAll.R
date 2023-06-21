############################################################# Set your directory
setwd("C:\\Users\\cody_ross\\Dropbox\\Open Papers\\Parochial altruism\\Colombia_Workflow\\Code")

################################################################# Load Libraries
library(igraph)
library(reshape2)
library(plyr)
library(GGally)
library(kinship2)
library(network)
library(geosphere)
library(ggplot2)
library(rethinking)
library(colorspace)
library(parallel)
library(qgraph)
options(mc.cores = parallel::detectCores())

################################################################# Load Database
load("Code/ColombianDataWithImputations_Site1.RData")       # Loads anonymized and rescaled
load("Code/ColombianDataWithImputations_Site2.RData")       # data with hard-coded median or mean
                                                            # imputations of sparse missings

################################################################# Descriptive things 
source("Code/Descriptive_Statistics.R") # Run this line by line to print
source("Code/Scatter.R")
source("Code/Network_Plots.R")

################################################################# Fit Models    
# Expected to run a server, otherwise the multicore stuff wont work                                                         
Iter = 1000
Warmup = 1000   
Thin = 1
Chains = 2
Refresh = 10
Seed = 123
MTD = 13
AD = 0.96

model_dat = vector("list",4)

#################################### Include all predictors
model_dat_Coast$Q = c(1,1)
model_dat_Inland$Q = c(1,1)
model_dat[[1]] = model_dat_Coast 
model_dat[[2]] = model_dat_Inland

#################################### Drop all predictors except ethnicity
model_dat_Coast$Q = c(0,1)
model_dat_Inland$Q = c(0,1)
model_dat[[3]] = model_dat_Coast
model_dat[[4]] = model_dat_Inland

StanModel = cmdstan_model(stan_file="Code/SRM.stan")

fit = mclapply(1:4,function(z){
        fit_mcmc = StanModel$sample(data=model_dat[[z]], thin=Thin, iter_sampling=Iter, iter_warmup=Warmup, chains=Chains, refresh=Refresh, max_treedepth=MTD, adapt_delta=AD)
        fit_stan = rstan::read_stan_csv(fit_mcmc$output_files())
        return(fit_stan)
             },
 mc.cores = 4)    

 source("Code/Plots.R")

######################################### Check fit
 source("Code/Check_Traceplots.R")






