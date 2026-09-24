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

Input.Folder <- "Inputs Other/"

Doall <- T
#if (Doall) DoRun(GeneralFile="General_Milk.shark.OM",RunNo="Milk.shark") # OK
#if (Doall) DoRun(GeneralFile="General_Perch.OM",RunNo="Perch") # OK
#if (Doall) DoRun(GeneralFile="General_Herring.OM",RunNo="Herring") # NOK
#if (Doall) DoRun(GeneralFile="General_SSageselemod2.OM",RunNo="SSageselemod2") # OK
#if (Doall) DoRun(GeneralFile="General_Runze.OM",RunNo="Runze") # OK
if (Doall) DoRun(GeneralFile="General_P.cod.OM",RunNo="P.cod") # OK
#if (Doall) DoRun(GeneralFile="General_Bluespot_Pilbara.OM",RunNo="Bluespot_Pilbara") # OK
#if (Doall) DoRun(GeneralFile="General_Red_Emperor_Kimberley.OM",RunNo="Red_Emperor_Kimberley") # NOK
#if (Doall) DoRun(GeneralFile="General_Red_Emperor_Pilbara.OM",RunNo="Red_Emperor_Pilbara") # OK
#if (Doall) DoRun(GeneralFile="General_SandySprat.OM",RunNo="SandySprat") # OK
#if (Doall) DoRun(GeneralFile="General_WA_Dhufish.OM",RunNo="WA_Dhufish") # OK
#if (Doall) DoRun(GeneralFile="General_Sandbar_Shark.OM",RunNo="Sandbar_Shark") # NOK
#if (Doall) DoRun(GeneralFile="General_Sandbar_Shark2.OM",RunNo="Sandbar_Shark2") # NOK
#if (Doall) DoRun(GeneralFile="General_Sardine.OM",RunNo="Sardine") # NOK
#if (Doall) DoRun(GeneralFile="General_Mackerel.OM",RunNo="Mackerel") # NOK
#if (Doall) DoRun(GeneralFile="General_Anchovy.OM",RunNo="Anchovy") # NOK
#if (Doall) DoRun(GeneralFile="General_Squid.OM",RunNo="Squid") # NOK

#if (Doall) DoRun(GeneralFile="General_Gummy.OM",RunNo="Gummy") # OK
#if (Doall) DoRun(GeneralFile="General_Quillback.OM",RunNo="Quillback") # OK
#if (Doall) DoRun(GeneralFile="General_Dusky.OM",RunNo="Dusky") # OK
#if (Doall) DoRun(GeneralFile="General_Goldband.OM",RunNo="Goldband") # OK
#if (Doall) DoRun(GeneralFile="General_Snapper_Gascoyne.OM",RunNo="Snapper_Gascoyne") # OK
#if (Doall) DoRun(GeneralFile="General_Snapper_North.OM",RunNo="Snapper_North") # OK
#if (Doall) DoRun(GeneralFile="General_WCDSC.OM",RunNo="WCDSC") # OK
#if (Doall) DoRun(GeneralFile="General_LM_CL3.OM",RunNo="LM_CL3") # OK
#if (Doall) DoRun(GeneralFile="General_Rankin.OM",RunNo="Rankin") # OK
#if (Doall) DoRun(GeneralFile="General_Whiskery_Shark.OM",RunNo="Whiskery_Shark") # OK
#if (Doall) DoRun(GeneralFile="General_Red_Emperor_Pilbara_2A.OM",RunNo="Red_Emperor_Pilbara_2A") # OK
#if (Doall) DoRun(GeneralFile="General_Red_Emperor_Pilbara_2A_nodevs.OM",RunNo="Red_Emperor_Pilbara_2A_nodevs") # OK
#if (Doall) DoRun(GeneralFile="General_Red_Emperor_Pilbara_2A_nodevs_CVfix.OM",RunNo="Red_Emperor_Pilbara_2A_nodevs_CVfix") # OK
#if (Doall) DoRun(GeneralFile="General_Gummy_Shark.OM",RunNo="Gummy_Shark") # OK
