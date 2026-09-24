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
Estimation.test <- 99                   #(0: Development; 1: Deterministic and no estimation ;2: Deterministic; 
                                        #3: Deterministic (incl rec_devs); 5: Huge sample sizes and no estimation; 
                                        #99: Real )
if (Estimation.test %in% c(1,2)) { Deter.Index.data <- T; Deter.Discard.data <- T; Deter.Length.data <- T; Deter.Age.data <- T }
if (Estimation.test %in% c(3)) { Deter.Rec.devs <- T; Deter.Index.data <- T; Deter.Discard.data <- T; Deter.Length.data <- T; Deter.Age.data <- T }
# 4: Huge sample sizes and estimation
if (Estimation.test %in% c(4)) { Huge.sample.sizes <- T }
# 5: Huge sample sizes and no estimation
if (Estimation.test %in% c(5)) { Huge.sample.sizes <- T }
if (Estimation.test %in% c(6)) { Huge.sample.sizes <- T; Deter.Index.data <- T; Deter.Discard.data <- T; Deter.Length.data <- T; Deter.Age.data <- T }

Input.Folder <- "Inputs OMF5/"

#DoRun(GeneralFile="General_P.cod_A0.OM",RunNo="P_cod")
#DoRun(GeneralFile="General_P.cod_A1.OM",RunNo="P_cod")
#DoRun(GeneralFile="General_P.cod_A2.OM",RunNo="P_cod")
#DoRun(GeneralFile="General_P.cod_A3.OM",RunNo="P_cod")
#DoRun(GeneralFile="General_P.cod_A4.OM",RunNo="P_cod")

#DoRun(GeneralFile="General_P.cod_B0.OM",RunNo="P_cod")

#DoRun(GeneralFile="General_P.cod_C0.OM",RunNo="P_cod")

#DoRun(GeneralFile="General_P.cod_D0.OM",RunNo="P_cod")

#DoRun(GeneralFile="General_P.cod_E0.OM",RunNo="P_cod")

DoRun(GeneralFile="General_P.cod_F0.OM",RunNo="P_cod")

