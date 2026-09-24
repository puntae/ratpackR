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
FullOutput <- T
Estimation.test <- 0                   #(0: Development; 1: Deterministic and no estimation ;2: Deterministic; 
                                       #3: Deterministic (incl rec_devs); 5: Huge sample sizes and no estimation; 
                                       #99: Real )

if (Estimation.test %in% c(1,2)) { Deter.Index.data <- T; Deter.Discard.data <- T; Deter.Length.data <- T; Deter.Age.data <- T }
if (Estimation.test %in% c(3)) { Deter.Rec.devs <- T; Deter.Index.data <- T; Deter.Discard.data <- T; Deter.Length.data <- T; Deter.Age.data <- T }
# 4: Huge sample sizes and estimation
if (Estimation.test %in% c(4)) { Huge.sample.sizes <- T }
# 5: Huge sample sizes and no estimation
if (Estimation.test %in% c(5)) { Huge.sample.sizes <- T }
if (Estimation.test %in% c(6)) { Huge.sample.sizes <- T; Deter.Index.data <- T; Deter.Discard.data <- T; Deter.Length.data <- T; Deter.Age.data <- T }

Input.Folder <- "Inputs CSIRO/"

Doall <- T
#if (Doall) DoRun(GeneralFile="General_Bight.redfish.OM",RunNo="Bight.redfish") # OK
#if (Doall) DoRun(GeneralFile="General_Blue.grenadier.OM",RunNo="Blue.grenadier") # NOK
#if (Doall) DoRun(GeneralFile="General_Deepwater.flathead.OM",RunNo="Deepwater.flathead") # OK
#if (Doall) DoRun(GeneralFile="General_Morwong.OM",RunNo="Morwong") # OK
#if (Doall) DoRun(GeneralFile="General_orange.roughy.east.OM",RunNo="Orange.roughy.east") # OK
#if (Doall) DoRun(GeneralFile="General_Pink.ling.OM",RunNo="Pink.ling") # OK
#if (Doall) DoRun(GeneralFile="General_Redfish.OM",RunNo="Redfish") #OK
#if (Doall) DoRun(GeneralFile="General_School.whiting.OM",RunNo="School.whiting") # OK
if (Doall) DoRun(GeneralFile="General_Silver.warehou.OM",RunNo="Silver.warehou") # OK
#if (Doall) DoRun(GeneralFile="General_Tiger.flathead.OM",RunNo="Tiger.flathead") # OK
#if (Doall) DoRun(GeneralFile="General_Mirror.dory.OM",RunNo="Mirror.dory") # OK

