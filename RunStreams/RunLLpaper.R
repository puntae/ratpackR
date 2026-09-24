rm(list=ls())
Path <- "C:/Research/NewRat/NewRat_spat/"
setwd(Path)

library(r4ss)
library(RTMB)
library(RcppArmadillo)
library(Rcpp)

source("RatPackR.r")

# =========================================================================================================================
# =========================================================================================================================

# Is this a test run
TestCase <- F
IsJitter <- F
Deter.Rec.devs <- F
Deter.Index.data <- F
Deter.Discard.data <- F
Deter.Length.data <- F
Deter.Age.data <- F
Huge.sample.sizes <- F
#UsePredators <- F

# 99 is actual
FullOutput <- F
Estimation.test <- 99                    #(0: Development; 1: Deterministic and no estimation ;2: Deterministic; 
                                        #3: Deterministic (incl rec_devs); 5: Huge sample sizes and no estimation; 
                                        #99: Real )
if (Estimation.test %in% c(1,2)) { Deter.Index.data <- T; Deter.Discard.data <- T; Deter.Length.data <- T; Deter.Age.data <- T }
if (Estimation.test %in% c(3)) { Deter.Rec.devs <- T; Deter.Index.data <- T; Deter.Discard.data <- T; Deter.Length.data <- T; Deter.Age.data <- T }
# 4: Huge sample sizes and estimation
if (Estimation.test %in% c(4)) { Huge.sample.sizes <- T }
# 5: Huge sample sizes and no estimation
if (Estimation.test %in% c(5)) { Huge.sample.sizes <- T }
if (Estimation.test %in% c(6)) { Huge.sample.sizes <- T; Deter.Index.data <- T; Deter.Discard.data <- T; Deter.Length.data <- T; Deter.Age.data <- T }

Input.Folder <- "Inputs LLpaper/"

Doall <- T
#if (Doall) DoRun(GeneralFile="General_Tiger.flathead.OM",RunNo="Tiger.flathead") # OK
#DoRun(GeneralFile="General_Silver.warehou_A0.OM",RunNo="Silver.warehou_A0") # OK
#DoRun(GeneralFile="General_Silver.warehou_A1.OM",RunNo="Silver.warehou_A1") # OK
#DoRun(GeneralFile="General_Silver.warehou_A2.OM",RunNo="Silver.warehou_A2") # OK
#DoRun(GeneralFile="General_Silver.warehou_A3.OM",RunNo="Silver.warehou_A3") # OK
#DoRun(GeneralFile="General_Silver.warehou_B0.OM",RunNo="Silver.warehou_B0") # OK
#DoRun(GeneralFile="General_Silver.warehou_B1.OM",RunNo="Silver.warehou_B1") # OK
#DoRun(GeneralFile="General_Silver.warehou_B2.OM",RunNo="Silver.warehou_B2") # OK
#DoRun(GeneralFile="General_Silver.warehou_B3.OM",RunNo="Silver.warehou_B3") # OK
#DoRun(GeneralFile="General_Silver.warehou_C0.OM",RunNo="Silver.warehou_C0") # OK
#DoRun(GeneralFile="General_Silver.warehou_C1.OM",RunNo="Silver.warehou_C1") # OK
#DoRun(GeneralFile="General_Silver.warehou_C2.OM",RunNo="Silver.warehou_C2") # OK
#DoRun(GeneralFile="General_Silver.warehou_C3.OM",RunNo="Silver.warehou_C3") # OK
DoRun(GeneralFile="General_Silver.warehou_D0.OM",RunNo="Silver.warehou_D0") # OK
DoRun(GeneralFile="General_Silver.warehou_D1.OM",RunNo="Silver.warehou_D1") # OK
DoRun(GeneralFile="General_Silver.warehou_D2.OM",RunNo="Silver.warehou_D2") # OK
DoRun(GeneralFile="General_Silver.warehou_D3.OM",RunNo="Silver.warehou_D3") # OK

