library(RTMB)
library(tidyverse)

# selectivity_obs <- c(0.006692857, 0.006978178, 0.008719404, 0.016956430, 0.046996470, 0.130714741, 0.306281975, 0.574998875,
#                      0.853344133, 0.997330405, 0.999999713, 0.999776305, 0.955024962, 0.811711036, 0.618923069, 0.431338309,
#                      0.285235256, 0.190624450, 0.138743981, 0.114404853, 0.104574035, 0.101141078, 0.100101640, 0.099828181,
#                      0.099765571, 0.099753077, 0.099750900, 0.099750565, 0.099750518, 0.099750510,0.099750508, 0.099750506,
#                      0.099750504)

selectivity_obs <- c(0.1,0.3,0.9, 0.7,0.5)
sel_lens <- c(1,7,16,25,33)

# Bin info ####
B_min <- 15
B_max <- 47
Bin_width <- 1
MidBin <- 0.5

# "Some intermediate quantities" 
int_min <- B_min + Bin_width * MidBin
int_max <- B_max + Bin_width * MidBin

Lbins <- seq(from=B_min, to=B_max,by=Bin_width)
MidLbins <- Lbins + Bin_width * MidBin

data <- list(Selex=selectivity_obs,lens=sel_lens)

# Define the model
f2 <- function(params, NLL=TRUE) {
  getAll(data, params, warn=FALSE)

  # Convert log parameters to their original scale
  real_p1 <- init_p1
  real_p2 <- real_p1 + Bin_width + (0.99 * int_max - real_p1 - Bin_width) / (1 + exp(-logit_p2))
  real_p3 <- exp(log_p3)
  real_p4 <- exp(log_p4)
  real_p5 <- 1 / (1 + exp(-logit_p5))
  real_p6 <- 1 / (1 + exp(-logit_p6))
  
  
  # "Some intermediate quantities continued"
  int_min_col2 <- exp(-((int_min - real_p1)^2)/real_p3)
  Peak <- real_p1
  Peak_col2 <- 1
  Peak2 <- real_p2
  
  # Ascending width curve ####
  asc <- exp(-((MidLbins - real_p1)^2)/real_p3)

  asc_scaled <- real_p5 + (1 - real_p5) * (asc - int_min_col2) / (1 - int_min_col2)

  
  # Descending width curve ####
  desc <- exp(-((MidLbins - real_p2)^2 / real_p4))
  
  desc_maxL <- exp(-((int_max - real_p2)^2 / real_p4))
  
  desc_scaled <- (1 + (real_p6 - 1) * (desc - 1) / (desc_maxL - 1))

  
  # joiner 1 curve ####
  join1 <- 1 / (1 + exp(-(20 * (MidLbins - real_p1) / (1 + abs(MidLbins - real_p1)))))
  
  # joiner 2 curve ####
  join2 <- 1 / (1 + exp(-(20 * (MidLbins - real_p2) / (1 + abs(MidLbins - real_p1)))))
  
  # selex curve ####
  selex <- asc_scaled * (1 - join1) + join1 * (1 * (1 - join2) + desc_scaled * join2)
  
  # putting it all together #### 
  selDF <- bind_cols(Lbins, MidLbins, asc, asc_scaled, desc, desc_scaled, join1, join2, selex)
  colnames(selDF) <- c("Bins", "MidBins", "asc", "asc_scaled", "desc", "desc_scaled", "join1", "join2", "selex")
  
  # Negative log-likelihood
  nll <- -sum(dnorm(selectivity_obs, selex[sel_lens], sd = 0.05, log = TRUE))
  
  if (NLL) {
    out <- nll 
  } else {
    out <- selDF
  }
  return(out)
}

params <- list(init_p1 = 24, 
               logit_p2 = -7.7, 
               log_p3 = 5.6,
               log_p4 = 4.1, 
               logit_p5 = -10.3, 
               logit_p6 = 0.4)

model <- MakeADFun(f2, parameters=params, control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15), silent=T)

fit <- nlminb(model$par, model$fn, model$gr) 

print(fit$par)

selDF <- f2(fit$par, NLL=FALSE)
write.csv(x=selDF,file="domeselex_val.csv")
write.csv(x=fit$par,file="domeselex_par.csv")

par(mfrow=c(3,1),mar=c(5,4,2,1),oma=c(2,2,2,2))

# estimated selex
plot(x=MidLbins,
     y=selDF$selex,
     type="l",
     pch=20,
     cex=2,
     lwd=2,
     col = "red",
     xlim=c(0,int_max),
     ylab="Selectivity (fits)",
     xlab="Midpoint Length Bins")

points(MidLbins[sel_lens],selectivity_obs,pch=16,col="blue",cex=2)

# estimated selex
plot(x=MidLbins,
     y=selDF$selex,
     type="b",
     pch=20,
     cex=2,
     col = "red",
     xlim=c(0,int_max),
     ylab="Selectivity (parts)",
     xlab="Midpoint Length Bins")

asc  <- selDF$asc_scaled*(1-selDF$join1)
dsc  <- selDF$join1 * selDF$desc_scaled * selDF$join2
flat <- selDF$join1 * (1 - selDF$join2)

# asc
lines(x=MidLbins,
      y=asc,
      col="green",
      lwd=3, lty=1)

# desc 
lines(x=MidLbins,
      y=dsc,
      col="blue",
      lwd=3,
      lty=1)

# flar
lines(x=MidLbins,
      y=flat,
      col="hotpink",
      lwd=3,
      lty=1)


# legend
legend(x="topleft",
       legend=c("Selex", "asc_scaled", "desc_scaled", "flat1"),
       col=c("red", "green", "blue", "hotpink"),
       lwd=1,
       lty=c(1, 1, 2, 4, 4),
       pch=c(20, NA, NA, NA, NA),
       pt.cex=2,
       bty="n")

# estimated selex
plot(x=MidLbins,
     y=asc+flat+dsc,
     type="l",
     pch=20,
     cex=2,
     lwd=2,
     col = "red",
     xlim=c(0,int_max),
     ylab="Selectivity (summed)",
     xlab="Midpoint Length Bins")


# Convert log parameters to their original scale
(real_p1 <- fit$par[1])
(real_p2 <- real_p1 + Bin_width + (0.99 * int_max - real_p1 - Bin_width) / (1 + exp(-fit$par[2])))
(real_p3 <- exp(fit$par[3]))
(real_p4 <- exp(fit$par[4]))
(real_p5 <- 1 / (1 + exp(-fit$par[5])))
(real_p6 <- 1 / (1 + exp(-fit$par[6])))

