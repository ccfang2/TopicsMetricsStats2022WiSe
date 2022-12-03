# ---------------------------------------
# Simulation - Rambachan and Roth (2022)
# ---------------------------------------

# Install HonestDiD package from here: https://github.com/asheshrambachan/HonestDiD

# ---------------------------------------
# Preamble
# ---------------------------------------
library(here)
library(corrplot)
library(MASS)
library(dplyr)
library(did)
library(haven)
library(ggplot2)
library(reshape2)
library(plyr)
library(fixest)
library(HonestDiD)

# ---------------------------------------
# Data and summary plot
# ---------------------------------------
# Insert your own directory and read data. Data can be downloaded from here
# https://www.openicpsr.org/openicpsr/project/151982/version/V1/view
data <- readRDS("/Users/jimmyfang/Desktop/SimHonestDiD/data/ResultsObjectList.rds")

# Bailey AER 2015.
bailey <- data[[1]]
betahat <- bailey$beta
sigma <- bailey$sigma

# beta plot
plot(betahat)
abline(v=5.5)

# sigma plot
corrplot(cov2cor(sigma))

# ---------------------------------------
# Sensitivity analysis
# ---------------------------------------
# Shiny app: https://ccfang2.shinyapps.io/HonestDiDSenAnlys/

# relative magnitude
delta_rm_results <- createSensitivityResults_relativeMagnitudes(
    betahat = betahat, #coefficients
    sigma = sigma, #covariance matrix
    numPrePeriods = 5, #num. of pre-treatment coefs
    numPostPeriods = 15, #num. of post-treatment coefs
    bound = "deviation from parallel trends", #the base choice of Delta
    method = "C-LF", #conditional least-favorable hybrid
    Mbarvec = NULL, #values of Mbar
    l_vec = basisVector(index = 1, size = 15), #l_vec
    biasDirection = NULL, #sign restriction
    monotonicityDirection = NULL, #monotonicity restriction
    alpha = 0.05 #desired size of robust confidence set
  )

originalResults <- constructOriginalCS(
    betahat = betahat, #coefficients
    sigma = sigma, #covariance matrix
    numPrePeriods = 5, #num of pre-treatment coefs
    numPostPeriods = 15, # num of post-treatment coefs
    l_vec = basisVector(index = 1, size = 15), #l_vec
    alpha = 0.05
    )

createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults)

# smoothness restriction
delta_sd_results <- createSensitivityResults(
    betahat = betahat, #coefficients
    sigma = sigma, #covariance matrix
    numPrePeriods = 5, #num of pre-treatment coefs
    numPostPeriods = 15, # num of post-treatment coefs
    method = "C-LF", #conditional least-favorable hybrid
    Mvec = NULL, # values of M
    l_vec = basisVector(index = 1, size = 15), #l_vec
    monotonicityDirection = NULL, #monotonicity restriction
    biasDirection = NULL, #sign restriction
    alpha = 0.05 #desired size of robust confidence set
    )

createSensitivityPlot(delta_sd_results, originalResults)

# ---------------------------------------
# Simulation
# ---------------------------------------

# all possible restriction choices
restriction_choices <- c("RM","SD","PB","NB","MI","MD")

# testing if a CI covers true value
fct_cvrg<-function(lb, ub, betahat) {
  case_when(lb<=betahat & ub>=betahat ~ 1,
            TRUE ~ 0)
}

# defining a function of inference using moment inequalities
sim_moment <- function(betahat_s, sigma, numPrePeriods, numPostPeriods, l_vec,
                Mbar, M, alpha, method, restriction) {
  # checking arguments
  if(!is.null(Mbar) & !is.null(M)) stop("Mbar and M cannot be defined simultaneously.")
  if(sum(l_vec)!=1) stop("Sum of l_vec must be equal to 1.")
  if(any(l_vec<0)) stop("Weight must be positive.")
  if(length(l_vec)!=numPostPeriods) stop("Length of l_vec must be equal to numPostPeriods.")
  if (sum(restriction[1:2])==0) stop("RM and/or SD must be chosen as basic Delta.")
  # bias direction and monotonicity direction
  bias <- if(restriction[3]==1) "positive" else {if(restriction[4]==1) "negative"}
  monotonicity <- if(restriction[5]==1) "increasing" else {if(restriction[6]==1) "decreasing"}
  # smoothness restriction
  if (restriction[1]==0 & restriction[2]==1) {
    result <-
      sapply(betahat_s,function(x) unlist(createSensitivityResults(betahat = x, 
                                                           sigma = sigma,
                                                           numPrePeriods = numPrePeriods, 
                                                           numPostPeriods = numPostPeriods, 
                                                           method = method, 
                                                           Mvec = M,
                                                           l_vec = l_vec, 
                                                           monotonicityDirection = monotonicity, 
                                                           biasDirection = bias, 
                                                           alpha = alpha
                                                           ))
          )
  } else {
    if (restriction[1]==1 & restriction[2]==1) {
      result <- sapply(betahat_s,function(x) unlist(createSensitivityResults_relativeMagnitudes(betahat = x, 
                                                                                          sigma = sigma, 
                                                                                          numPrePeriods = numPrePeriods,
                                                                                          numPostPeriods = numPostPeriods, 
                                                                                          bound = "deviation from linear trend",  
                                                                                          method = method, 
                                                                                          Mbarvec = Mbar, 
                                                                                          l_vec = l_vec, 
                                                                                          biasDirection = bias, 
                                                                                          monotonicityDirection = monotonicity, 
                                                                                          alpha = alpha
                                                                                          ))
                      )
    } else {
      result <- sapply(betahat_s,function(x) unlist(createSensitivityResults_relativeMagnitudes(betahat = x, 
                                                                                          sigma = sigma, 
                                                                                          numPrePeriods = numPrePeriods,
                                                                                          numPostPeriods = numPostPeriods, 
                                                                                          bound = "deviation from parallel trends",  
                                                                                          method = method, 
                                                                                          Mbarvec = Mbar, 
                                                                                          l_vec = l_vec, 
                                                                                          biasDirection = bias, 
                                                                                          monotonicityDirection = monotonicity, 
                                                                                          alpha = alpha))
                      )
    }
  }
  # finalizing output
  result<-t(result)
  result<-data.frame(apply(result[,1:2],2,function(x) as.numeric(x)),result[,3:4])
  row.names(result) <- NULL
  return(result)
}


# defining a function of inference using Fixed Length Confidence Intervals
sim_flci <- function(betahat_s, sigma, numPrePeriods, numPostPeriods, l_vec, M, alpha=0.05){
  result<-sapply(betahat_s, function(x) unlist(findOptimalFLCI(betahat = x,
                                                               sigma = sigma,
                                                               numPrePeriods = numPrePeriods, 
                                                               numPostPeriods = numPostPeriods, 
                                                               l_vec = l_vec,
                                                               M=M,
                                                               alpha = alpha)))
  # finalizing output
  result<-t(result)
  result<- data.frame(lb=as.numeric(result[,1]),
                      ub=as.numeric(result[,2]),
                      method="FLCI",
                      Delta=NA)
  return(result)
}


# defining a function of simulation
sim <- function(nsim, betahat, sigma, numPrePeriods, numPostPeriods, l_vec,
                Mbar, M, alpha, method, restriction) {
  betahat_s <- as.data.frame(t(mvrnorm(n=nsim, mu=betahat, Sigma=sigma)))
  result.moment <- sim_moment(betahat_s, sigma, numPrePeriods, numPostPeriods, l_vec,
                              Mbar, M, alpha, method, restriction)
  result.flci <- if (restriction[1]==0 & restriction[2]==1) {
    sim_flci(betahat_s, sigma, numPrePeriods, numPostPeriods, l_vec, 
             M, alpha)
    }
  result <- rbind(result.moment, result.flci)
  result$coverage <- fct_cvrg(result$lb, result$ub, 
                              betahat = as.vector(betahat[(numPrePeriods+1):(numPrePeriods+numPostPeriods)]%*%l_vec))
  result<- result %>% 
    group_by(method) %>%
    summarise(mean(coverage)) %>%
    left_join(result, by="method")
  
  return(result)
}

# run simulations: coverage probability
options(warn = -1)

Sys.time()
# DeltaSD; M=1
sd.sim.m1 <- list()
for (i in c(10,20,30,50,100,200)) {
  sd1<-sim(nsim=i, betahat=betahat, sigma=sigma, numPrePeriods=5, numPostPeriods=15, l_vec=basisVector(1,15),
           Mbar=NULL, M=1, alpha=0.05, method="C-LF", restriction=c(0,1,0,0,0,0))
  sd.sim.m1[[length(sd.sim.m1)+1]]<- sd1
}
names(sd.sim.m1)<- paste("sd.sim",c(10,20,30,50,100,200),".m1",sep="")

# DeltaSD; M=2
sd.sim.m2 <- list()
for (i in c(10,20,30,50,100,200)) {
  sd2<-sim(nsim=i, betahat=betahat, sigma=sigma, numPrePeriods=5, numPostPeriods=15, l_vec=basisVector(1,15),
           Mbar=NULL, M=1, alpha=0.05, method="C-LF", restriction=c(0,1,0,0,0,0))
  sd.sim.m2[[length(sd.sim.m2)+1]]<- sd2
}
names(sd.sim.m2)<- paste("sd.sim",c(10,20,30,50,100,200),".m2",sep="")

# DeltaRM; Mbar=0.1
rm.sim <- list()
for (i in seq(10,30,10)) {
   rm<-sim(nsim=i, betahat=betahat, sigma=sigma, numPrePeriods=5, numPostPeriods=15, l_vec=basisVector(1,15),
                 Mbar=0.1, M=NULL, alpha=0.05, method="C-LF", restriction=c(1,0,0,0,0,0))
   rm.sim[[length(rm.sim)+1]]<- rm
}
names(rm.sim)<- paste("rm.sim",seq(10,30,10),sep="")

Sys.time()
rm(sd1,sd2,rm)
options(warn = 0)


# graph
fct <- function(x) {
  data.frame(unique(x[,1:2]))
}

# sd.sim<- list(sd.sim10.m1,
#                  sd.sim20.m1,
#                  sd.sim30.m1,
#                  sd.sim50.m1,
#                  sd.sim100.m1,
#                  sd.sim200.m1,
#                  sd.sim10.m2,
#                  sd.sim20.m2,
#                  sd.sim30.m2,
#                  sd.sim50.m2,
#                  sd.sim100.m2,
#                  sd.sim200.m2)

sd.sim <-c(sd.sim.m1, sd.sim.m2)
names(sd.sim) <- c(paste("sd.sim",c(10,20,30,50,100,200),".m1", sep=""),
                   paste("sd.sim",c(10,20,30,50,100,200),".m2", sep=""))

rm.sim <- list(rm.sim10,
               rm.sim20,
               rm.sim30)

# Delta_SD: Compare C-LF and FLCI when M=1
sd.sim%>%
  ldply(fct) %>%
  dplyr::rename("id" =".id", "prob" = "mean.coverage.") %>%
  mutate(M = as.numeric(substr(id,nchar(id),nchar(id))),
         nsim = c(rep(c(10,20,30,50,100,200),each=2),
                  rep(c(10,20,30,50,100,200),each=2))) %>% 
  filter(M==1)%>%
  ggplot(aes(x=nsim,y=prob, group=method,color=method))+
    geom_line()+
  labs(x="# of Simulations", y="Coverage Probability",
       title = "Comparison between Hybrid Approach and FLCI when SD (M=1)")+
  ylim(0.5,1)
  
# Delta_SD: Compare C-LF and FLCI when M=2
sd.sim%>%
  ldply(fct) %>%
  dplyr::rename("id" =".id", "prob" = "mean.coverage.") %>%
  mutate(M = as.numeric(substr(id,nchar(id),nchar(id))),
         nsim = c(rep(c(10,20,30,50,100,200),each=2),
                  rep(c(10,20,30,50,100,200),each=2))) %>% 
  filter(M==2)%>%
  ggplot(aes(x=nsim,y=prob, group=method,color=method))+
  geom_line()+
  labs(x="# of Simulations", y="Coverage Probability",
       title = "Comparison between Hybrid Approach and FLCI when SD (M=2)")+
  ylim(0.5,1)

# Delta_SD: Compare M=1 and M=2 when C-LF
sd.sim%>%
  ldply(fct) %>%
  dplyr::rename("id" =".id", "prob" = "mean.coverage.") %>%
  mutate(M = as.numeric(substr(id,nchar(id),nchar(id))),
         nsim = c(rep(c(10,20,30,50,100,200),each=2),
                  rep(c(10,20,30,50,100,200),each=2))) %>% 
  filter(method=="C-LF")%>%
  ggplot(aes(x=nsim,y=prob, group=factor(M),color=factor(M)))+
  geom_line()+
  labs(x="# of Simulations", y="Coverage Probability",
       title = "Comparison between M=1 and M=2 when using C-LF")+
  ylim(0.5,1)

# Delta_RM
rm.sim%>%
  ldply(fct) %>%
  dplyr::rename("id" =".id", "prob" = "mean.coverage.") %>%
  mutate(nsim = c(10,20,30)) %>% 
  ggplot(aes(x=nsim,y=prob))+
  geom_line()+
  labs(x="# of Simulations", y="Coverage Probability",
       title = "RM(Mbar=0.1)")+
  ylim(0.5,1)

# run simulation: power function
# defining a function of simulating power function
sim.power <- function(nsim, b, sigma, numPrePeriods, numPostPeriods, l_vec,
                Mbar, M, alpha, method, restriction) {
  beta <- betahat
  beta[6] <- b
  beta_s <- as.data.frame(t(mvrnorm(n=nsim, mu=beta, Sigma=sigma)))
  result <- sim_moment(beta_s, sigma, numPrePeriods, numPostPeriods, l_vec,
                              Mbar, M, alpha, method, restriction)
  result$coverage <- fct_cvrg(result$lb, result$ub, 
                              betahat = as.vector(betahat[(numPrePeriods+1):(numPrePeriods+numPostPeriods)]%*%l_vec))
  return(result$coverage)
}

sd.sim.power <- matrix(0, nrow = 100, ncol = 21)
for (i in 1:21) {
  b <- seq(-10,0,0.5)[i]
  sd<-sim.power(nsim=100, b=b, sigma=sigma, numPrePeriods=5, numPostPeriods=15, l_vec=basisVector(1,15),
           Mbar=NULL, M=1, alpha=0.05, method="C-LF", restriction=c(0,1,0,0,0,0))
  sd.sim.power[,i]<- sd
}

# plotting power function
powers <- apply(sd.sim.power, 2, mean)
plot(seq(-10,0,0.5), powers)


