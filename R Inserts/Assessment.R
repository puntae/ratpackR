
# =======================================================================================================================
Write_starter <- function(Folder,Istock)
 {
  file.name <- paste0(Folder,"starter.ss")
  #print(file.name)
  write(paste0("# starter.ss for",Folder),file=file.name)
  
  write("data.dat",file=file.name,append=T)
  write("data.ctl",file=file.name,append=T)
  if (Estimation.test %in% c(0,1,2,3,4,5,6) || Stock[[Istock]]$Ass$Use.par=="Yes")
   write("1 # 0=use init values in control file; 1=use ss.par",file=file.name,append=T)
  else
   write("0 # 0=use init values in control file; 1=use ss.par",file=file.name,append=T)
  write("0 # run display detail (0,1,2)",file=file.name,append=T)
  write("1 # detailed output (0=minimal for data-limited, 1=high (w/ wtatage.ss_new), 2=brief)",file=file.name,append=T)
  write("0 # # write 1st iteration details to echoinput.sso file (0,1)",file=file.name,append=T) 
  write("0 # write parm values to ParmTrace.sso (0=no,1=good,active; 2=good,all; 3=every_iter,all_parms; 4=every,active)",file=file.name,append=T)
  write("0 # write to cumreport.sso (0=no,1=like&timeseries; 2=add survey fits)",file=file.name,append=T)
  write("1 # Include prior_like for non-estimated parameters (0,1)",file=file.name,append=T)
  write("1 # Use Soft Boundaries to aid convergence (0,1) (recommended)",file=file.name,append=T)
  write("0 # Number of bootstrap datafiles to produce",file=file.name,append=T)
  if (Estimation.test %in% c(0,1,5,6)|| Stock[[Istock]]$Ass$Estimate=="No")
   write("0 # Turn off estimation for parameters entering after this phase",file=file.name,append=T)
  else
    write("10 # Turn off estimation for parameters entering after this phase",file=file.name,append=T)
  write("0 # MCeval burn interval",file=file.name,append=T)
  write("1 # MCeval thin interval",file=file.name,append=T)
  write("0 # jitter initial parm value by this fraction",file=file.name,append=T)
  write("-1 # min yr for sdreport outputs (-1 for styr)",file=file.name,append=T)
  write("-2 # max yr for sdreport outputs (-1 for endyr; -2 for endyr+Nforecastyrs",file=file.name,append=T)
  write("0 # N individual STD years",file=file.name,append=T)
  write("#vector of year values ",file=file.name,append=T)
  write("0.0001 # final convergence criteria (e.g. 1.0e-04)",file=file.name,append=T)
  write("0 # retrospective year relative to end year (e.g. -4)",file=file.name,append=T)
  write("1 # min age for calc of summary biomass",file=file.name,append=T)
  write("1 # Depletion basis:  denom is: 0=skip; 1=rel X*B0; 2=rel X*Bmsy; 3=rel X*B_styr",file=file.name,append=T)
  write("1.0 # Fraction (X) for Depletion denominator (e.g. 0.4)",file=file.name,append=T)
  write("4 # (1-SPR)_reporting:  0=skip; 1=rel(1-SPR); 2=rel(1-SPR_MSY); 3=rel(1-SPR_Btarget); 4=notrel",file=file.name,append=T)
  write("1 # # Annual_F_units: 0=skip; 1=exploitation(Bio); 2=exploitation(Num); 3=sum(Apical_F's); 4=true F for range of ages; 5=unweighted avg. F for range of ages",file=file.name,append=T)
  write("#COND 10 15 #_min and max age over which average F will be calculated with F_reporting=4 or 5",file=file.name,append=T)
  write("3 # F_std_basis: 0=raw_annual_F; 1=F/Fspr; 2=F/Fmsy ; 3=F/Fbtgt; where F means annual_F",file=file.name,append=T)
  write("0 # MCMC output detail: integer part (0=default; 1=adds obj func components); and decimal part (added to SR_LN(R0) on first call to mcmc)",file=file.name,append=T)
  write("0 # ALK tolerance (example 0.0001)",file=file.name,append=T)
  write("-1 # random number seed for bootstrap data (-1 to use long(time) as seed): # 1585083947",file=file.name,append=T)
  write("3.30 # check value for end of file and for version control",file=file.name,append=T)
 }

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

Write_data <- function(Folder,Istock,Iarea,Year)
 {
  
  CVCatch <- 0.01
  
  Nsex <- Stock[[Istock]]$Nsex
  MaxAge <- Stock[[Istock]]$MaxAge
  Nlen <- Stock[[Istock]]$Nlen
  YrOffset <- Stock[[Istock]]$YrOffset
  
  file.name <- paste0(Folder,"data.dat")
  #print(file.name)
  write(paste0("# data.dat for",Folder),file=file.name)
  write("# Model dimensions",file=file.name,append=T)
  
  cat(1+YrOffset," # StartYr\n",file=file.name,append=T)
  write(paste0(Year+YrOffset," # EndYr"),file=file.name,append=T)
  write("1 #_Nseas",file=file.name,append=T)
  write("12 #_months/season (currently fixed at 12)",file=file.name,append=T)
  write("2 #_Nsubseasons (even number, minimum is 2)",file=file.name,append=T)
  write("1 #_spawn_month",file=file.name,append=T)
  
  write(paste0(Stock[[Istock]]$NsexUse," #_Ngenders: 1, 2, -1  (use -1 for 1 sex setup with SSB multiplied by female_frac parameter)"),file=file.name,append=T)
  write(paste0(MaxAge-1," #_Nages=accumulator age, first age is always age 0"),file=file.name,append=T)
  write("1 #_Nareas",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt=="None") write(paste0(General$Nfleet," #_General$Nfleets (including surveys),DatFile,append=T)"),file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt!="None") write(paste0(General$Nfleet+Stock[[Istock]]$Num.Pred," #_General$Nfleets (including surveys & predators),DatFile,append=T)"),file=file.name,append=T)
  write("#_fleet_type: 1=catch fleet; 2=bycatch only fleet; 3=survey; 4=ignore",file=file.name,append=T)
  write("#_sample_timing: -1 for fishing fleet to use season-long catch-at-age for observations, or 1 to use observation month;  (always 1 for surveys)",file=file.name,append=T)
  write("#_fleet_area:  area the fleet/survey operates in ",file=file.name,append=T)
  write("#_units of catch:  1=bio; 2=num (ignored for surveys; their units read later)",file=file.name,append=T)
  write("#_catch_mult: 0=no; 1=yes",file=file.name,append=T)
  write("#_rows are fleets",file=file.name,append=T)
  write("#_fleet_type timing area units need_catch_mult fleetname",file=file.name,append=T)
  for (Ifleet in 1:General$Nfleet)
   {
    if (Ifleet <= General$Ncat_fleet) Vec <- c(1,-1,1,1,0) else Vec <- c(3,1,1,1,0)
    cat(format(Vec,width=2),General$Fleet.Names[Ifleet]," #\n",file=file.name,append=T)
   }
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    {
     Vec <- c(4,1,1,1,0)
     cat(format(Vec,width=2),paste0("Predator_",Ipred)," #\n",file=file.name,append=T)
    }
  
  # Catch data
  # ==========
  write("#Bycatch_fleet_input_goes_next",file=file.name,append=T)
  write("#a:  fleet index",file=file.name,append=T)
  write("#b:  1=include dead bycatch in total dead catch for F0.1 and MSY optimizations and forecast ABC; 2=omit from total catch for these purposes (but still include the mortality)",file=file.name,append=T)
  write("#c:  1=Fmult scales with other fleets; 2=bycatch F constant at input value; 3=bycatch F from range of years",file=file.name,append=T)
  write("#d:  F or first year of range",file=file.name,append=T)
  write("#e:  last year of range",file=file.name,append=T)
  write("#f:  not usedwrite(",file=file.name,append=T)
  write("# a   b   c   d   e   f",file=file.name,append=T)
  write("#_Catch data: yr, seas, fleet, catch, catch_se",file=file.name,append=T)
  write("#_catch_se:  standard error of log(catch)",file=file.name,append=T)
  write("#_NOTE:  catch data is ignored for survey fleets",file=file.name,append=T)
  for (Ifleet in 1:General$Ncat_fleet)
   {
    cat(-999,1,Ifleet,Stock[[Istock]]$Equilbrium.catch[Ifleet],CVCatch,"\n",file=file.name,append=T)
    for (Iyear in 1:Year)
      if (Stock[[Istock]]$Data$Catches[Iarea,Ifleet,Iyear]>1.0e-10)
        cat(Iyear+YrOffset,1,Ifleet,Stock[[Istock]]$Data$Catches[Iarea,Ifleet,Iyear],CVCatch,"\n",file=file.name,append=T)
   }
  write("-9999 0 0 0 0",file=file.name,append=T)
  
  # Index data
  # ==========
  write("#",file=file.name,append=T)
  write("#_CPUE_and_surveyabundance_observations",file=file.name,append=T)
  write("#_Units:  0=numbers; 1=biomass; 2=F; 30=spawnbio; 31=recdev; 32=spawnbio*recdev; 33=recruitment; 34=depletion(&see Qsetup); 35=parm_dev(&see Qsetup)",file=file.name,append=T)
  write("#_Errtype:  -1=normal; 0=lognormal; >0=T",file=file.name,append=T)
  write("#_SD_Report: 0=no sdreport; 1=enable sdreport",file=file.name,append=T)
  write("#_Fleet Units Errtype SD_Report",file=file.name,append=T)
  for (Ifleet in 1:General$Nfleet)
    cat(Ifleet,1,0,0,"#",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)  
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
      cat(General$Nfleet+Ipred,2,0,0,"#",paste0("Predator_",Ipred),"\n",file=file.name,append=T)  
  
  write("#_yr month fleet obs stderr",file=file.name,append=T)
  for (Ifleet in 1:General$Nfleet)
   for (Iyear in 1:Year)
    {
     Index <- which(Stock[[Istock]]$Data$IndexData.Used[Iarea,,1]==Iyear+YrOffset & Stock[[Istock]]$Data$IndexData.Used[Iarea,,3]==Ifleet) 
     if (length(Index)>0)
      cat(Stock[[Istock]]$Data$IndexData.Used[Iarea,Index,1:5],"# ",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)
   }
  if (Stock[[Istock]]$Ass$Pred_opt=="Opt_5"|| Stock[[Istock]]$Ass$Pred_opt=="Opt_3")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
     if (Stock[[Istock]]$Data$Is_pred.no[Ipred])
    for (Iyear in 1:Year) 
     {
      Index <- which(Stock[[Istock]]$Data$Pred.no.Data[,1]==Iyear+YrOffset & Stock[[Istock]]$Data$Pred.no.Data[,3]==General$Nfleet+Ipred) 
       if (length(Index)>0)
        cat(Stock[[Istock]]$Data$Pred.no.Data[Index,1:5],"# ",paste0("Predator_",Ipred),"\n",file=file.name,append=T) 
     }   
      
  write("-9999 1 1 1 1 # terminator for CPUE and survey observations\n",file=file.name,append=T)
  
  # Discard data
  # ============
  N.discard.fleet <-sum(Stock[[Istock]]$Data$Is_discard=="Yes")
  if (Stock[[Istock]]$Ass$Pred_opt!="None") N.discard.fleet <- N.discard.fleet + Stock[[Istock]]$Num.Pred
  cat(N.discard.fleet ," #_N_fleets_with_discard",file=file.name,append=T)
  write("#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)",file=file.name,append=T)
  write("#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal; -3 for trunc normal with CV",file=file.name,append=T)
  write(" # note, only have units and errtype for fleets with discard",file=file.name,append=T)
  write("#Fleet Units Err_type",file=file.name,append=T)
  for (Ifleet in 1:General$Ncat_fleet)
   if (Stock[[Istock]]$Data$Is_discard[Ifleet]=="Yes")
     cat(Ifleet,Stock[[Istock]]$Data$Discard.type[Ifleet],-2," # ",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
     cat(General$Nfleet+Ipred,1,-2," # ",paste0("Predator_",Ipred),"\n",file=file.name,append=T)
  
  for (Ifleet in 1:General$Ncat_fleet)
   for (Iyear in 1:Year)
    {
     Index <- which(Stock[[Istock]]$Data$DiscardData.Used[,1]==Iyear+YrOffset & Stock[[Istock]]$Data$DiscardData.Used[,3]==Ifleet) 
     if (length(Index)>0)
      cat(Stock[[Istock]]$Data$DiscardData.Used[Index,1:5],"# ",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)
   }
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    {
     for (Iyear in 1:Year)
      {
       Index <- which(Stock[[Istock]]$Data$Consump.Data[,1]==Iyear+YrOffset & Stock[[Istock]]$Data$Consump.Data[,3]==General$Nfleet+Ipred) 
       if (length(Index)>0)
        cat(Stock[[Istock]]$Data$Consump.Data[Index,1:5],"# ",paste0("Predator_",Ipred),"\n",file=file.name,append=T) 
       }
     
    } 
    
  if (sum(Stock[[Istock]]$Data$Is_discard=="Yes")==0) write("#  -9999 0 0 0.0 0.0 # terminator for discard data\n",file=file.name,append=T)
  if (sum(Stock[[Istock]]$Data$Is_discard=="Yes")>0 || Stock[[Istock]]$Ass$Pred_opt!="None") write("  -9999 0 0 0.0 0.0 # terminator for discard data\n",file=file.name,append=T)
  
  # Mean body weight data
  # =====================
  write("0 #_use meanbodysize_data (0/1)",file=file.name,append=T)
  write("#_COND_0 #_DF_for_meanbodysize_T-distribution_like",file=file.name,append=T)
  write("# note:  use positive partition value for mean body wt, negative partition for mean body length",file=file.name,append=T)
  write("#_yr month fleet part obs stderr",file=file.name,append=T)
  write("#  -9999 0 0 0 0 0 # terminator for mean body size data",file=file.name,append=T)
  
  # Length data
  # =============
  write("# set up population length bin structure (note - irrelevant if not using size data and using empirical wtatage",file=file.name,append=T)
  write("1 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector",file=file.name,append=T)
  write("# binwidth for population size comp (no additional input for option 1)",file=file.name,append=T)
  write("# minimum size in the population (lower edge of first bin and size at age 0.00)",file=file.name,append=T)
  write("# # maximum size in the population (lower edge of last bin)\n",file=file.name,append=T)
  write("1 # use length composition data (0/1)",file=file.name,append=T)
  write("#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.",file=file.name,append=T)
  write("#_addtocomp:  after accumulation of tails; this value added to all bins",file=file.name,append=T)
  write("#_males and females treated as combined gender below this bin number",file=file.name,append=T)
  write("#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation",file=file.name,append=T)
  write("#_Comp_Error:  0=multinomial, 1=dirichlet",file=file.name,append=T)
  write("#_Comp_Error2:  parm number  for dirichlet",file=file.name,append=T)
  write("#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001",file=file.name,append=T)
  write("#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize",file=file.name,append=T)
  for (Ifleet in 1:General$Nfleet)
   cat("-1 1e-07 0 0 0 0 0.01 # ",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    cat("-1 1e-07 0 0 0 0 0.01 # ",paste0("Predator_",Ipred),"\n",file=file.name,append=T)

  write("# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution",file=file.name,append=T)
  write("# partition codes:  (0=combined; 1=discard; 2=retained",file=file.name,append=T)
  cat(Nlen," #_N_LengthBins; then enter lower edge of each length bin\n",file=file.name,append=T)
  cat(Stock[[Istock]]$LoLenBin[1:Nlen],"\n",file=file.name,append=T)
  #1e-04 1.0001 2.0001 3.0001 4.0001 5.0001 6.0001 7.0001 8.0001 9.0001 10.0001 11.0001 12.0001 13.0001 14.0001 15.0001 16.0001 17.0001 18.0001 19.0001 20.0001 21.0001 22.0001 23.0001 24.0001 25.0001 26.0001 27.0001 28.0001 29.0001 30.0001 31.0001 32.0001 33.0001 34.0001 35.0001 36.0001 37.0001 38.0001 39.0001 40.0001 41.0001 42.0001 43.0001 44.0001 45.0001 46.0001 47.0001 48.0001 49.0001 50.0001 51.0001 52.0001 53.0001 54.0001 55.0001 56.0001 57.0001 58.0001 59.0001 60.0001 61.0001 62.0001 63.0001 64.0001 65.0001 66.0001 67.0001 68.0001 69.0001 70.0001 71.0001 72.0001 73.0001 74.0001 75.0001 76.0001 77.0001 78.0001 79.0001 80.0001 81.0001 82.0001 83.0001 84.0001 85.0001 86.0001 87.0001 88.0001 89.0001 90.0001 91.0001 92.0001 93.0001 94.0001 95.0001 96.0001 97.0001 98.0001 99.0001 100.0001 101.0001 102.0001 103.0001 104.0001 105.0001 106.0001 107.0001 108.0001 109.0001 110.0001 111.0001 112.0001 113.0001 114.0001 115.0001 116.0001 117.0001 118.0001 119.0001 120.0001
  write("#_yr month fleet sex part Nsamp datavector(female-male)",file=file.name,append=T)
  Stock[[Istock]]$Data$LengthData.Used <<- matrix(Stock[[Istock]]$Data$LengthData.Used,ncol=7+Nlen*Nsex)
  if (length(Stock[[Istock]]$Data$LengthData.Used)>0)
   for (Itype in 0:2)
    for (Ifleet in 1:General$Nfleet)
     for (Iyear in 1:Year)
      {
       Index <- which(Stock[[Istock]]$Data$LengthData.Used[,1]==Iarea  & Stock[[Istock]]$Data$LengthData.Used[,2]==Iyear+YrOffset & Stock[[Istock]]$Data$LengthData.Used[,4]==Ifleet & Stock[[Istock]]$Data$LengthData.Used[,6]==Itype) 
       if (length(Index)>0)
        if (sum(Stock[[Istock]]$Data$LengthData.Used[Index,-c(1:7)])>0)
         cat(Stock[[Istock]]$Data$LengthData.Used[Index,-1],"# ",General$Fleet.Names[Ifleet],Iyear,"\n",file=file.name,append=T)
      }
  cat(-9999,rep(0,5+Nsex*Nlen),"\n\n",file=file.name,append=T)
  
  # age data
  # =============
  cat(MaxAge,"#_N_age_bins\n",file=file.name,append=T)
  cat(seq(from=0,to=MaxAge-1),"\n",file=file.name,append=T)

  #0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
  cat(1," #_N_ageerror_definitions\n",file=file.name,append=T)
  cat(Stock[[Istock]]$Ass$AgeErrorMeans,"\n",file=file.name,append=T)
  cat(Stock[[Istock]]$Ass$AgeErrorSds,"\n",file=file.name,append=T)
  write("#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.",file=file.name,append=T)
  write("#_addtocomp:  after accumulation of tails; this value added to all bins",file=file.name,append=T)
  write("#_males and females treated as combined gender below this bin number",file=file.name,append=T)
  write("#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation",file=file.name,append=T)
  write("#_Comp_Error:  0=multinomial, 1=dirichlet",file=file.name,append=T)
  write("#_Comp_Error2:  parm number  for dirichlet",file=file.name,append=T)
  write("#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001",file=file.name,append=T)
  write("#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize",file=file.name,append=T)
  for (Ifleet in 1:General$Nfleet)
    cat("-1 1e-07 0 0 0 0 0.01 # ",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
      cat("-1 1e-07 0 0 0 0 0.01 # ",paste0("Predator_",Ipred),"\n",file=file.name,append=T)
  write("2 #_Lbin_method_for_Age_Data: 1=poplenbins; 2=datalenbins; 3=lengths",file=file.name,append=T)
  write("# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sex length distribution",file=file.name,append=T)
  write("# partition codes:  (0=combined; 1=discard; 2=retained)",file=file.name,append=T)
  write("#_yr month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)",file=file.name,append=T)
  for (Itype in 0:2)
   for (Ifleet in 1:General$Nfleet)
    if (Stock[[Istock]]$Data$Is_age[Ifleet]=="Yes") 
     for (Iyear in 1:Year)
      {
       Index <- which(Stock[[Istock]]$Data$AgeData.Used[,1]==Iarea & Stock[[Istock]]$Data$AgeData.Used[,2]==Iyear+YrOffset & Stock[[Istock]]$Data$AgeData.Used[,4]==Ifleet & Stock[[Istock]]$Data$AgeData.Used[,6]==Itype) 
       if (length(Index)>0)
        cat(Stock[[Istock]]$Data$AgeData.Used[Index,-1],"# ",General$Fleet.Names[Ifleet],Iyear,"\n",file=file.name,append=T)
      }
 for (Itype in 0:2)
  for (Ifleet in 1:General$Nfleet)
   if (Stock[[Istock]]$Data$Is_CAA[Ifleet]=="Yes") 
    for (Iyear in 1:Year)
     {
      Index <- which(Stock[[Istock]]$Data$CAAData.Used[,1]==Iarea & Stock[[Istock]]$Data$CAAData.Used[,2]==Iyear+YrOffset & Stock[[Istock]]$Data$CAAData.Used[,4]==Ifleet & Stock[[Istock]]$Data$CAAData.Used[,6]==Itype) 
      if (length(Index)>0)
       for (Index2 in 1:length(Index))
        cat(Stock[[Istock]]$Data$CAAData.Used[Index[Index2],-1],"# ",General$Fleet.Names[Ifleet],Iyear,"\n",file=file.name,append=T)
    }
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    for (Iyear in 1:Year)
    {
     Index <- which(Stock[[Istock]]$Data$PredAgeData[,1]==Iyear+YrOffset & Stock[[Istock]]$Data$PredAgeData[,3]==General$Nfleet+Ipred) 
     if (length(Index)>0)
       cat(Stock[[Istock]]$Data$PredAgeData[Index,],"# ",paste0("Predator_",Ipred),Iyear,"\n",file=file.name,append=T)  
    }
  
  cat(-9999,rep(1,8+Nsex*MaxAge),"\n\n",file=file.name,append=T)
  
  # Other data lines
  write("0 #_Use_MeanSize-at-Age_obs (0/1)",file=file.name,append=T)
  write("# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution",file=file.name,append=T)
  write("# partition codes:  (0=combined; 1=discard; 2=retained",file=file.name,append=T)
  write("# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age",file=file.name,append=T)
  write("#_yr month fleet sex part ageerr ignore datavector(female-male)",file=file.name,append=T)
  write("#                                          samplesize(female-male)",file=file.name,append=T)
  write("#",file=file.name,append=T)
  Num.Ind <- Stock[[Istock]]$N.env.ind + Stock[[Istock]]$Num.Pred
  cat(Num.Ind,"# number of environmental variables: year, variable, value\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
   {
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    for (Iyear in 1:(Stock[[Istock]]$Nhist+General$Nproj+1))  
      cat(Iyear+YrOffset,Ipred,log(Stock[[Istock]]$Pred.No[Iyear,Ipred]),"# ",paste0("Predator_",Ipred),Iyear,"\n",file=file.name,append=T)  
    
   }
  if (Stock[[Istock]]$N.env.ind>0)
  for (Ind in 1:Stock[[Istock]]$N.env.ind)
   {
    for (Iyear in 1:(Year))  
     {
      if (Ind != Stock[[Istock]]$Num.Pred+Ind)
       cat(Iyear+YrOffset,Stock[[Istock]]$Num.Pred+Ind,Stock[[Istock]]$EnvData[Iyear,Ind],"# ",paste0("Indicator_",Ind),Iyear,"\n",file=file.name,append=T)  
      if (Ind==Stock[[Istock]]$Num.Pred+Ind)
       {
        Env.Val <- Stock[[Istock]]$EnvData[Iyear,Ind]
        if (Stock[[Istock]]$Mcat.Index[Iyear]==1) Env.Val <- 1
        cat(Iyear+YrOffset,Stock[[Istock]]$Num.Pred+Ind,Env.Val,"# ",paste0("Indicator_",Ind),Iyear,"\n",file=file.name,append=T)  
       }
    }
    for (Iyear in (Year+1):(Stock[[Istock]]$Nhist+General$Nproj+1))  
      cat(Iyear+YrOffset,Stock[[Istock]]$Num.Pred+Ind,0,"# ",paste0("Indicator_",Ind),Iyear,"\n",file=file.name,append=T)  
  }
  if (Num.Ind>0) cat(-9999,0,0,"# ",file=file.name,append=T)
    
  write("#",file=file.name,append=T)
  write("0 # N sizefreq methods to read",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("0 # do tags (0/1)",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("0 # morphcomp data (0/1)",file=file.name,append=T)
  write("#Nobs, Nmorphs, mincomp",file=file.name,append=T)
  write("# yr, seas, type, partition, Nsamp, datavector_by_Nmorphs",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("0 # Do dataread for selectivity priors(0/1)",file=file.name,append=T)
  write("# Yr, Seas, Fleet,  Age/Size,  Bin,  selex_prior,  prior_sd",file=file.name,append=T)
  write("# feature not yet implemented",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("999",file=file.name,append=T)
  }

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

Do_Jitter <- function(Istock,Isex,Value,Phase,Bound_mult,Jitter_SD,parameter_offset_approach,TV=rep(0,9),CV=F)
 {
  YrOffset <- Stock[[Istock]]$YrOffset
  Rand <- rnorm(1,0,Jitter_SD)
  Low <- Value/Bound_mult; Hi <- Value*Bound_mult
  if (Value < 0) { Hi <- Value/Bound_mult; Low <-Value*Bound_mult }
  if (Low==Hi & Value==0) { Low <- -10; Hi <- 10; }
  Bounds <- c(Low,Hi)
  Return.V <- c(Bounds,Value,Value) 
  if (Phase != -2)
   {
    if (Rand >= 0) Est <- Low+(Value-Low)/(1.0+exp(Rand))*2
    if (Rand <= 0) Est <- Value+(Hi-Value)*(1.0/(1.0+exp(Rand))-0.5)*2
    Return.V <- c(Bounds,Est,Est) 
   }
  if (Phase == -2 || IsJitter==F) Return.V <- c(Bounds,Value,Value) 
  if (Isex != 1 & parameter_offset_approach==2)  Return.V <- c(-3,3,Value,Value) 
  if (CV==T & parameter_offset_approach==3)  Return.V <- c(-3,3,Value,Value) 
  Vec2 <- rep(0,7)
  if (TV[1]!=0) Vec2[1] <- TV[1]  
  if (TV[4]!=0) Vec2[2:5] <- TV[4:7]
  if (TV[8]!=0) Vec2[6:7] <- TV[8:9] 
  if (parameter_offset_approach == -1) Vec2 <- NULL
  Return.V <- c(Return.V,10,0,Phase,Vec2)
  return(Return.V)
 }


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------
Write_ctl <- function(Folder,Istock,Iarea,Year)
 {
  Nsex <- Stock[[Istock]]$Nsex
  MaxAge <- Stock[[Istock]]$MaxAge
  Nlen <- Stock[[Istock]]$Nlen
  YrOffset <- Stock[[Istock]]$YrOffset
  
  # General outputs
  Ass <- Stock[[Istock]]$Ass
  Bound_mult <- Stock[[Istock]]$Ass_Gen$Bound_Mult
  Jitter_SD <- Stock[[Istock]]$Ass_Gen$Parameter_jitter_SD

  FleetsToAreas <- Stock[[Istock]]$FleetsToAreas
  Use <- which(FleetsToAreas==Iarea)

  # AEP set.seed
   
  file.name <- paste0(Folder,"data.ctl")
  #print(file.name)
  write(paste0("# data.ctl for",Folder,"\n"),file=file.name)
  
  write("#_data_and_control_files: data.dat // data.ctl",file=file.name,append=T)
  write("0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters",file=file.name,append=T)
  write("1  #_N_Growth_Patterns (Growth Patterns, Morphs, Bio Patterns, GP are terms used interchangeably in SS3)",file=file.name,append=T)
  write("1 #_N_platoons_Within_GrowthPattern ",file=file.name,append=T)
  write("#_Cond 1 #_Platoon_within/between_stdev_ratio (no read if N_platoons=1)",file=file.name,append=T)
  write("#_Cond sd_ratio_rd < 0: platoon_sd_ratio parameter required after movement params.",file=file.name,append=T)
  write("#_Cond  1 #vector_platoon_dist_(-1_in_first_val_gives_normal_approx)",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("3 # recr_dist_method for parameters:  2=main effects for GP, Area, Settle timing; 3=each Settle entity; 4=none (only when N_GP*Nsettle*pop==1)",file=file.name,append=T)
  write("1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area",file=file.name,append=T)
  write("1 #  number of recruitment settlement assignments ",file=file.name,append=T)
  write("0 # unused option",file=file.name,append=T)
  write("#GPattern month  area  age (for each settlement assignment)",file=file.name,append=T)
  write("1 1 1 0",file=file.name,append=T)
  write("#",file=file.name,append=T)
  
  
  write("#_Cond 0 # N_movement_definitions goes here if Nareas > 1",file=file.name,append=T)
  write("#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0",file=file.name,append=T)
  write("#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10",file=file.name,append=T)
  
  write("#",file=file.name,append=T)
  cat(Ass$SS_Nblock.patterns," #_Nblock_Patterns\n",file=file.name,append=T)
  cat(Ass$SS_blocks_per_pattern," #_blocks_per_pattern\n",file=file.name,append=T) 
  write("# begin and end years of blocks",file=file.name,append=T)
  for (Iblock.year in 1:Ass$SS_Nblock.patterns)
    {
     Block.years.update <-Ass$block.years[Iblock.year,]
     for (Iblk in 1:Ass$SS_blocks_per_pattern[Iblock.year]) if (Block.years.update[Iblk*2]==Ass$SS_Original_yr1) Block.years.update[Iblk*2] <- YrOffset+Year+100
     cat(Block.years.update[1:(Ass$SS_blocks_per_pattern[Iblock.year]*2)],"\n",file=file.name,append=T) 
  }
  
  write("#",file=file.name,append=T)
  write("# controls for all timevary parameters ",file=file.name,append=T)
  write("1 #_time-vary parm bound check (1=warn relative to base parm bounds; 3=no bound check); Also see env (3) and dev (5) options to constrain with base bounds",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("# AUTOGEN",file=file.name,append=T)
  write("1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex",file=file.name,append=T)
  write("# where: 0 = autogen time-varying parms of this category; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("#_Available timevary codes",file=file.name,append=T)
  write("#_Block types: 0: P_block=P_base*exp(TVP); 1: P_block=P_base+TVP; 2: P_block=TVP; 3: P_block=P_block(-1) + TVP",file=file.name,append=T)
  write("#_Block_trends: -1: trend bounded by base parm min-max and parms in transformed units (beware); -2: endtrend and infl_year direct values; -3: end and infl as fraction of base range",file=file.name,append=T)
  write("#_EnvLinks:  1: P(y)=P_base*exp(TVP*env(y));  2: P(y)=P_base+TVP*env(y);  3: P(y)=f(TVP,env_Zscore) w/ logit to stay in min-max;  4: P(y)=2.0/(1.0+exp(-TVP1*env(y) - TVP2))",file=file.name,append=T)
  write("#_DevLinks:  1: P(y)*=exp(dev(y)*dev_se;  2: P(y)+=dev(y)*dev_se;  3: random walk;  4: zero-reverting random walk with rho;  5: like 4 with logit transform to stay in base min-max",file=file.name,append=T)
  write("#_DevLinks(more):  21-25 keep last dev for rest of years",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("#_Prior_codes:  0=none; 6=normal; 1=symmetric beta; 2=CASAL's beta; 3=lognormal; 4=lognormal with biascorr; 5=gamma",file=file.name,append=T)
  cat(Ass$M.type," #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=BETA:_Maunder_link_to_maturity;_6=Lorenzen_range\n",file=file.name,append=T)
  if (Ass$M.type==0)
   write("#_no additional input for selected M option; read 1P per morph",file=file.name,append=T)
  if (Ass$M.type==3)
   {
    write("#_ #_Age_natmort_by sex x growthpattern",file=file.name,append=T)
    for (Isex in 1:Nsex)
     write(Stock[[Istock]]$Mbase[Isex,],ncol=MaxAge,file=file.name,append=T)   
   }
  write("#",file=file.name,append=T)
  
  cat(Stock[[Istock]]$Growth.Model," # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation\n",file=file.name,append=T)
  cat(Ass$SS_Age1," #_Age(post-settlement) for L1 (aka Amin); first growth parameter is size at this age; linear growth below this\n",file=file.name,append=T)
  cat(Ass$SS_Age2," #_Age(post-settlement) for L2 (aka Amax); 999 to treat as Linf\n",file=file.name,append=T)
  write("-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)",file=file.name,append=T)
  write("0  #_placeholder for future growth feature",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)",file=file.name,append=T)
  cat(Stock[[Istock]]$CVoption," #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)\n",file=file.name,append=T)
  write("#",file=file.name,append=T)
  cat(Stock[[Istock]]$Mat.option," #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity\n",file=file.name,append=T)
  if (Stock[[Istock]]$Mat.option==4)
   {
    write("# read Age_Maturity(3) or Age_Fecundity(4)\n",file=file.name,append=T)
    cat(Stock[[Istock]]$age.mat,"\n",file=file.name,append=T)
   }
  cat(Ass$SS_First_mat_age," #_First_Mature_Age\n",file=file.name,append=T)
  cat(Ass$SS_fec_option,"#_fecundity_at_length option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W\n",file=file.name,append=T)
  write("0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn",file=file.name,append=T)
  cat(Ass$SS_parameter_offset_approach,"#_parameter_offset_approach for M, G, CV_G:  1- direct, no offset**; 2- male=fem_parm*exp(male_parm); 3: male=female*exp(parm) then old=young*exp(parm)\n",file=file.name,append=T)
  write("#_** in option 1, any male parameter with value = 0.0 and phase <0 is set equal to female parameter",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("#_growth_parms",file=file.name,append=T)
  write("#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn",file=file.name,append=T)
  
  if (Ass$M.type==0)
   {
    write("# Sex: 1  BioPattern: 1  NatMort",file=file.name,append=T)
    TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[1];  TV[8] <- Ass$MG_blocks[1]; TV[9] <- Ass$MG_block_fns[1]
    Initial.val <- Ass$SS_Mbase[1,1]*Ass$SS_M_Bias; if (Ass$SS_M_Bias < 0)  Initial.val <- -1*Ass$SS_M_Bias;
    cat(Do_Jitter(Istock,1,Initial.val,Ass$SS_M_Phase[1],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# NatM_uniform_Fem_GP_1\n",file=file.name,append=T)
   }

  write("# Sex: 1  BioPattern: 1  Growth",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[2];  TV[8] <- Ass$MG_blocks[2]; TV[9] <- Ass$MG_block_fns[2]
  cat(Do_Jitter(Istock,1,Ass$SS_LenA1[1]*Ass$SS_LenA1_Bias,Ass$SS_LenA1_Phase[1],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# L_at_Amin_Fem_GP_1\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[3];  TV[8] <- Ass$MG_blocks[3]; TV[9] <- Ass$MG_block_fns[3]
  cat(Do_Jitter(Istock,1,Ass$SS_LenA2[1]*Ass$SS_LenA2_Bias,Ass$SS_LenA2_Phase[1],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# L_at_Amax_Fem_GP_1\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[4];  TV[8] <- Ass$MG_blocks[4]; TV[9] <- Ass$MG_block_fns[4]
  cat(Do_Jitter(Istock,1,Ass$SS_Kappa[1]*Ass$SS_Kappa_Bias,Ass$SS_Kappa_Phase[1],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# VonBert_K_Fem_GP_1\n",file=file.name,append=T)
  if (Stock[[Istock]]$Growth.Model==2)
   {
    TV <- rep(0,9); 
    cat(Do_Jitter(Istock,1,Ass$SS_Richards[1]*Ass$SS_Richards_Bias,    Ass$SS_Richards_Phase[1],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# Richards_Fem_GP_1\n",file=file.name,append=T)
   }
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[5];  TV[8] <- Ass$MG_blocks[5]; TV[9] <- Ass$MG_block_fns[5]
  cat(Do_Jitter(Istock,1,Ass$SS_CV1[1]*Ass$SS_CV1_Bias,    Ass$SS_CV1_Phase[1],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# CV_young_Fem_GP_1\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[6];  TV[8] <- Ass$MG_blocks[6]; TV[9] <- Ass$MG_block_fns[6]
  if (Ass$SS_parameter_offset_approach %in% c(1,2))
   cat(Do_Jitter(Istock,1,Ass$SS_CV2[1]*Ass$SS_CV2_Bias,    Ass$SS_CV2_Phase[1],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# CV_old_Fem_GP_1\n",file=file.name,append=T)
  if (Ass$SS_parameter_offset_approach %in% c(3))
  cat(Do_Jitter(Istock,1,log(Ass$SS_CV2[1]*Ass$SS_CV2_Bias/(Ass$SS_CV1[1]*Ass$SS_CV1_Bias)),    Ass$SS_CV2_Phase[1],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# CV_old_Fem_GP_1\n",file=file.name,append=T)
  
  write("# Sex: 1  BioPattern: 1  WtLen",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[7];  TV[8] <- Ass$MG_blocks[7]; TV[9] <- Ass$MG_block_fns[7]
  cat(Do_Jitter(Istock,1,Ass$SS_WtLen_a[1],-2,Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# Wtlen_1_Fem_GP_1\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[8];  TV[8] <- Ass$MG_blocks[8]; TV[9] <- Ass$MG_block_fns[8]
  cat(Do_Jitter(Istock,1,Ass$SS_WtLen_b[1],-2,Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# Wtlen_2_Fem_GP_1\n",file=file.name,append=T)
  write("# Sex: 1  BioPattern: 1  Maturity&Fecundity",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[9];  TV[8] <- Ass$MG_blocks[9]; TV[9] <- Ass$MG_block_fns[9]
  cat(Do_Jitter(Istock,1,Ass$SS_Mat50,-2,Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# Mat50%_Fem_GP_1\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[10];  TV[8] <- Ass$MG_blocks[10]; TV[9] <- Ass$MG_block_fns[10]
  cat(Do_Jitter(Istock,1,Ass$SS_SlopeMat,-2,Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# Mat_slope_Fem_GP_1\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[11];  TV[8] <- Ass$MG_blocks[11]; TV[9] <- Ass$MG_block_fns[11]
  cat(Do_Jitter(Istock,1,Ass$SS_Egg_1,-2,Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# Eggs/kg_inter_Fem_GP_1\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[12];  TV[8] <- Ass$MG_blocks[12]; TV[9] <- Ass$MG_block_fns[12]
  cat(Do_Jitter(Istock,1,Ass$SS_Egg_2,-2,Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# Eggs/kg_slope_wt_Fem_GP_1\n",file=file.name,append=T)

  if (Nsex==2)
   {
    if (Ass$M.type==0)
     {
      write("# Sex: 2  BioPattern: 1  NatMort",file=file.name,append=T)
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[13];  TV[8] <- Ass$MG_blocks[13]; TV[9] <- Ass$MG_block_fns[13]
      if (Ass$SS_parameter_offset_approach==1)
       {
        Initial.val <- Ass$SS_Mbase[2,1]*Ass$SS_M_Bias; if (Ass$SS_M_Bias < 0)  Initial.val <- -1*Ass$SS_M_Bias;
        cat(Do_Jitter(Istock,2,Initial.val,Ass$SS_M_Phase[1],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# NatM_uniform_Mal_GP_1\n",file=file.name,append=T)
       }
      if (Ass$SS_parameter_offset_approach %in% c(2,3) )
       cat(Do_Jitter(Istock,2,0.0,Ass$SS_M_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# NatM_uniform_Mal_GP_1\n",file=file.name,append=T)
      }
    write("# Sex: 2  BioPattern: 1  Growth",file=file.name,append=T)
    if (Ass$SS_parameter_offset_approach==1)
     {
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[14];  TV[8] <- Ass$MG_blocks[14]; TV[9] <- Ass$MG_block_fns[14]
      cat(Do_Jitter(Istock,2,Ass$SS_LenA1[2]*Ass$SS_LenA1_Bias,Ass$SS_LenA1_Phase[2],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# L_at_Amin_Mal_GP_1\n",file=file.name,append=T)
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[15];  TV[8] <- Ass$MG_blocks[15]; TV[9] <- Ass$MG_block_fns[15]
      cat(Do_Jitter(Istock,2,Ass$SS_LenA2[2]*Ass$SS_LenA2_Bias,Ass$SS_LenA2_Phase[2],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# L_at_Amax_Mal_GP_1\n",file=file.name,append=T)
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[16];  TV[8] <- Ass$MG_blocks[16]; TV[9] <- Ass$MG_block_fns[16]
      cat(Do_Jitter(Istock,2,Ass$SS_Kappa[2]*Ass$SS_Kappa_Bias,Ass$SS_Kappa_Phase[2],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# VonBert_K_Mal_GP_1\n",file=file.name,append=T)
      if (Stock[[Istock]]$Growth.Model==2)
       {
        TV <- rep(0,9); 
        cat(Do_Jitter(Istock,1,Ass$SS_Richards[2]*Ass$SS_Richards_Bias,    Ass$SS_Richards_Phase[2],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# Richards_Mal_GP_1\n",file=file.name,append=T)
       }
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[17];  TV[8] <- Ass$MG_blocks[17]; TV[9] <- Ass$MG_block_fns[17]
      cat(Do_Jitter(Istock,2,Ass$SS_CV1[2]*Ass$SS_CV1_Bias  ,  Ass$SS_CV1_Phase[2],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# CV_young_Mal_GP_1\n",file=file.name,append=T)
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[18];  TV[8] <- Ass$MG_blocks[18]; TV[9] <- Ass$MG_block_fns[18]
      cat(Do_Jitter(Istock,2,Ass$SS_CV2[2]*Ass$SS_CV2_Bias  ,  Ass$SS_CV2_Phase[2],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# CV_old_Mal_GP_1\n",file=file.name,append=T)
    }
    if (Ass$SS_parameter_offset_approach==2)
     {
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[14];  TV[8] <- Ass$MG_blocks[14]; TV[9] <- Ass$MG_block_fns[14]
      cat(Do_Jitter(Istock,2,log(Ass$SS_LenA1[2]/Ass$SS_LenA1[1]),Ass$SS_LenA1_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# L_at_Amin_Mal_GP_1\n",file=file.name,append=T)
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[15];  TV[8] <- Ass$MG_blocks[15]; TV[9] <- Ass$MG_block_fns[15]
      cat(Do_Jitter(Istock,2,log(Ass$SS_LenA2[2]/Ass$SS_LenA2[1]),Ass$SS_LenA2_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# L_at_Amax_Mal_GP_1\n",file=file.name,append=T)
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[16];  TV[8] <- Ass$MG_blocks[16]; TV[9] <- Ass$MG_block_fns[16]
      cat(Do_Jitter(Istock,2,log(Ass$SS_Kappa[2]/Ass$SS_Kappa[1]),Ass$SS_Kappa_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# VonBert_K_Mal_GP_1\n",file=file.name,append=T)
      if (Stock[[Istock]]$Growth.Model==2)
       {
        TV <- rep(0,9); 
        cat(Do_Jitter(Istock,1,log(Ass$SS_Richards[2]/Ass$SS_Richards[1]),    Ass$SS_Richards_Phase[2],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# Richards_Mal_GP_1\n",file=file.name,append=T)
       }
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[17];  TV[8] <- Ass$MG_blocks[17]; TV[9] <- Ass$MG_block_fns[17]
      cat(Do_Jitter(Istock,2,log(Ass$SS_CV1[2]/Ass$SS_CV1[1])  ,  Ass$SS_CV1_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# CV_young_Mal_GP_1\n",file=file.name,append=T)
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[18];  TV[8] <- Ass$MG_blocks[18]; TV[9] <- Ass$MG_block_fns[18]
      cat(Do_Jitter(Istock,2,log(Ass$SS_CV2[2]/Ass$SS_CV2[1])  ,  Ass$SS_CV2_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# CV_old_Mal_GP_1\n",file=file.name,append=T)
    }
    if (Ass$SS_parameter_offset_approach==3)
    {
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[14];  TV[8] <- Ass$MG_blocks[14]; TV[9] <- Ass$MG_block_fns[14]
      cat(Do_Jitter(Istock,2,log(Ass$SS_LenA1[2]/Ass$SS_LenA1[1]),Ass$SS_LenA1_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# L_at_Amin_Mal_GP_1\n",file=file.name,append=T)
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[15];  TV[8] <- Ass$MG_blocks[15]; TV[9] <- Ass$MG_block_fns[15]
      cat(Do_Jitter(Istock,2,log(Ass$SS_LenA2[2]/Ass$SS_LenA2[1]),Ass$SS_LenA2_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# L_at_Amax_Mal_GP_1\n",file=file.name,append=T)
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[16];  TV[8] <- Ass$MG_blocks[16]; TV[9] <- Ass$MG_block_fns[16]
      cat(Do_Jitter(Istock,2,log(Ass$SS_Kappa[2]/Ass$SS_Kappa[1]),Ass$SS_Kappa_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# VonBert_K_Mal_GP_1\n",file=file.name,append=T)
      if (Stock[[Istock]]$Growth.Model==2)
       {
        TV <- rep(0,9); 
        cat(Do_Jitter(Istock,1,log(Ass$SS_Richards[2]/Ass$SS_Richards[1]),    Ass$SS_Richards_Phase[2],Bound_mult,Jitter_SD,Ass$SS_parameter_offset_approach,TV=TV),"# Richards_Mal_GP_1\n",file=file.name,append=T)
       }
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[17];  TV[8] <- Ass$MG_blocks[17]; TV[9] <- Ass$MG_block_fns[17]
      cat(Do_Jitter(Istock,2,log(Ass$SS_CV1[2]/Ass$SS_CV1[1])  ,  Ass$SS_CV1_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# CV_young_Mal_GP_1\n",file=file.name,append=T)
      TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[18];  TV[8] <- Ass$MG_blocks[18]; TV[9] <- Ass$MG_block_fns[18]
      cat(Do_Jitter(Istock,2,log(Ass$SS_CV2[2]/Ass$SS_CV2[1])  ,  Ass$SS_CV2_Phase[2],Bound_mult,Jitter_SD,2,TV=TV),"# CV_old_Mal_GP_1\n",file=file.name,append=T)
    }
    
    write("# Sex: 2  BioPattern: 1  WtLen",file=file.name,append=T)
    TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[19];  TV[8] <- Ass$MG_blocks[19]; TV[9] <- Ass$MG_block_fns[19]
    cat(Do_Jitter(Istock,2,Ass$SS_WtLen_a[2],-2,Bound_mult,Jitter_SD,0,TV=TV),"# Wtlen_1_Mal_GP_1\n",file=file.name,append=T)
    TV <- rep(0,9); TV[1] <- Ass$MG_env_devs[20];  TV[8] <- Ass$MG_blocks[20]; TV[9] <- Ass$MG_block_fns[20]
    cat(Do_Jitter(Istock,2,Ass$SS_WtLen_b[2],-2,Bound_mult,Jitter_SD,0,TV=TV),"# Wtlen_2_Mal_GP_1\n",file=file.name,append=T)
   }
  write("# Hermaphroditism",file=file.name,append=T)
  write("#  Recruitment Distribution ",file=file.name,append=T)
  write("-4 4 0 0 10 0 -1 0 0 0 0 0 0 0 # RecrDist_GP_1_area_1_month_1",file=file.name,append=T)
  write("#  Cohort growth dev base",file=file.name,append=T)
  write("0.1 10 1 1 1 0 -1 0 0 0 0 0 0 0 # CohortGrowDevv",file=file.name,append=T)
  write("#  Movement",file=file.name,append=T)
  write("#  Platoon StDev Ratio",file=file.name,append=T) 
  write("#  Age Error from parameters",file=file.name,append=T)
  write("#  catch multiplier",file=file.name,append=T)
  write("#  fraction female, by GP",file=file.name,append=T)
  write("0 1 0.5 0.5 10 0 -1 0 0 0 0 0 0 0 # FracFemale_GP_1",file=file.name,append=T)
  write("#  M2 parameter for each predator fleet",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt=="Opt_5")
   {
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
      cat(0,4,Stock[[Istock]]$Pred.BaseM[Ipred],Stock[[Istock]]$Pred.BaseM[Ipred],0.8,0,-3,0,5,1+YrOffset, Year+YrOffset,2,1,2,"#", paste0("Predator_",Ipred),"\n",file=file.name,append=T)
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
     {
      cat(1e-05,2.4,1,0.2,0.8,0,3,"#",paste0("M2_pred1_BLK1repl_",paste0("Predator_",Ipred)),"\n",file=file.name,append=T)
      cat(0.0001,2,1,0.5,0.5,-6,-5,"#",paste0("M2_pred1_dev_se_",paste0("Predator_",Ipred)),"\n",file=file.name,append=T)
      cat(-0.99,0.99,0,0,0.5,-6,-6,"#",paste0("M2_pred1_dev_autocorr_",paste0("Predator_",Ipred)),"\n",file=file.name,append=T)
     } 
    }
  if (Stock[[Istock]]$Ass$Pred_opt=="Opt_3")
   {
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
      cat(0,4,Stock[[Istock]]$Pred.BaseM[Ipred],Stock[[Istock]]$Pred.BaseM[Ipred],0.8,0,-3,0,3,1+YrOffset, Year+YrOffset,2,0,0,"#", paste0("Predator_",Ipred),"\n",file=file.name,append=T)
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
     {
      #cat(1e-05,2.4,1,0.2,0.8,0,3,"#",paste0("M2_pred1_BLK1repl_",paste0("Predator_",Ipred)),"\n",file=file.name,append=T)
      cat(0.0001,2,1,0.5,0.5,-6,-5,"#",paste0("M2_pred1_dev_se_",paste0("Predator_",Ipred)),"\n",file=file.name,append=T)
      cat(-0.99,1.03,1,1,0.5,-6,-6,"#",paste0("M2_pred1_dev_autocorr_",paste0("Predator_",Ipred)),"\n",file=file.name,append=T)
     } 
    }
  if (Stock[[Istock]]$Ass$Pred_opt=="Env_predators")
   {
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
      cat(0,4,Stock[[Istock]]$Pred.BaseM[Ipred],Stock[[Istock]]$Pred.BaseM[Ipred],0.8,0,3,100+Ipred,0,0,0,0,0,0,"#", paste0("Predator_",Ipred),"\n",file=file.name,append=T)
   }
  
    
  write("#",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt!="Env_predators" & Ass$N_dev_MG==0) write("#_no timevary MG parameters",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt=="Env_predators" | Ass$N_dev_MG!=0) write("#Timevary MG parameters",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$TVM[4]>0)
    for (Isex in 1:Nsex)
      if (Isex==1 || Ass$SS_parameter_offset_approach==1) 
       {
        cat(0,2,1,1,0.8,0,-1,"#",paste0("TV_M_SD",Isex),"\n",file=file.name,append=T)
        cat(-99,99,0,0,0.8,0,-1,"#",paste0("TV_M_Prow",Isex),"\n",file=file.name,append=T)
       }  
        
  if (Stock[[Istock]]$Ass$TVM[1]>0)
    for (Isex in 1:Nsex)
      if (Isex==1 || Ass$SS_parameter_offset_approach==1) cat(-99,99,Stock[[Istock]]$Ass$TVM[2],Stock[[Istock]]$Ass$TVM[2],0.8,0,Stock[[Istock]]$Ass$TVM[3],"#",paste0("TV_M_",Isex),"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$TVLenA1[1]>0)
   for (Isex in 1:Nsex)
     if (Isex==1 || Ass$SS_parameter_offset_approach==1) cat(-99,99,Stock[[Istock]]$Ass$TVLenA1[2],Stock[[Istock]]$Ass$TVLenA1[2],0.8,0,Stock[[Istock]]$Ass$TVLenA1[3],"#",paste0("TV_LenA_",Isex),"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$TVLenA2[1]>0)
   for (Isex in 1:Nsex)
    if (Isex==1 || Ass$SS_parameter_offset_approach==1) cat(-99,99,Stock[[Istock]]$Ass$TVLenA2[2],Stock[[Istock]]$Ass$TVLenA2[2],0.8,0,Stock[[Istock]]$Ass$TVLenA2[3],"#",paste0("TV_LenB_",Isex),"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$TVKappa[1]>0)
   for (Isex in 1:Nsex)
     if (Isex==1 || Ass$SS_parameter_offset_approach==1) cat(-99,99,Stock[[Istock]]$Ass$TVKappa[2],Stock[[Istock]]$Ass$TVKappa[2],0.8,0,Stock[[Istock]]$Ass$TVKappa[3],"#",paste0("TV_Kappa_",Isex),"\n",file=file.name,append=T)
  
  if (Ass$N_dev_MG>0)
    for (Ipar in 1:Ass$N_dev_MG)
      cat(-99,99,Ass$MG_devs[Ipar],Ass$MG_devs[Ipar],0.8,0,Ass$MG_devs_phs[Ipar],"#",paste0("MG_dev_par_",Ipar),"\n",file=file.name,append=T)
  
  if (Stock[[Istock]]$Ass$Pred_opt=="Env_predators")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    cat(-99,99,1,1,0.8,0,-1,"#",paste0("Predator_",Ipred),"\n",file=file.name,append=T)

  write("#",file=file.name,append=T)
  write("#_seasonal_effects_on_biology_parms",file=file.name,append=T)
  write("0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K",file=file.name,append=T)
  write("#_ LO HI INIT PRIOR PR_SD PR_type PHASE",file=file.name,append=T)
  write("#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters",file=file.name,append=T)
  write("#",file=file.name,append=T)

  # Stock and recruitment (SR_parms)
  write("#",file=file.name,append=T)
  write("3 #_Spawner-Recruitment; Options: 1=NA; 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm",file=file.name,append=T)
  cat(Ass$Use_Steep_in_equ," # 0/1 to use steepness in initial equ recruitment calculation\n",file=file.name,append=T)
  write("0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature",file=file.name,append=T)
  write("#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name",file=file.name,append=T)
  
  # Base SR parameters
  TV <- rep(0,9); TV[1] <- Ass$SR_env_devs[1];  TV[8] <- Ass$SR_blocks[1]; TV[9] <- Ass$SR_block_fns[1]
  cat(Do_Jitter(Istock,1,log(sum(Stock[[Istock]]$R00.orig[Use]*Ass$SR_R0_mult)), Ass$SR_R0_Phase,    Bound_mult,Jitter_SD,0,TV=TV),"# SR_LN(R0)\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$SR_env_devs[2];  TV[8] <- Ass$SR_blocks[2]; TV[9] <- Ass$SR_block_fns[2]
  Initial.val <- Ass$SR_Steep; if (Ass$SS_Steep_Bias < 0)  Initial.val <- -1*Ass$SS_Steep_Bias;
  cat(Do_Jitter(Istock,1,Initial.val,                  Ass$SR_Steep_Phase, Bound_mult,Jitter_SD,0,TV=TV),"# SR_BH_steep\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$SR_env_devs[3];  TV[8] <- Ass$SR_blocks[3]; TV[9] <- Ass$SR_block_fns[3]
  cat(Do_Jitter(Istock,1,Ass$SR_SigmaR,                 Ass$SR_SigmaR_Phase,Bound_mult,Jitter_SD,0,TV=TV),"# SR_sigmaR\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$SR_env_devs[4];  TV[8] <- Ass$SR_blocks[4]; TV[9] <- Ass$SR_block_fns[4]
  cat(Do_Jitter(Istock,1,0,                             Ass$SR_regime_Phase,Bound_mult,Jitter_SD,0,TV=TV),"# R0_regime\n",file=file.name,append=T)
  TV <- rep(0,9); TV[1] <- Ass$SR_env_devs[5];  TV[8] <- Ass$SR_blocks[5]; TV[9] <- Ass$SR_block_fns[5]
  cat(Do_Jitter(Istock,1,0,                                              -1,Bound_mult,Jitter_SD,0,TV=TV),"# SR_autocorr\n",file=file.name,append=T)
  
  write("#_no timevary SR parameters",file=file.name,append=T)
  if (Ass$N_dev_SR>0)
  for (Ipar in 1:Ass$N_dev_SR)
    cat(-99,99,Ass$SR_devs[Ipar],Ass$SR_devs[Ipar],0.8,0,Ass$SR_devs_phs[Ipar],"#",paste0("SR_dev_par_",Ipar),"\n",file=file.name,append=T)
  
  if (Stock[[Istock]]$Ass$TVRecr[1]>0)
    cat(-99,99,Stock[[Istock]]$Ass$TVRecr[2],Stock[[Istock]]$Ass$TVRecr[2],0.8,0,Stock[[Istock]]$Ass$TVRecr[3],"#",paste0("TV_Recr"),"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$TVR0Off[1]>0)
    cat(-99,99,Stock[[Istock]]$Ass$TVR0Off[2],Stock[[Istock]]$Ass$TVR0Off[2],0.8,0,Stock[[Istock]]$Ass$TVR0Off[3],"#",paste0("TV_R0_offset"),"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$TVR0Off[1]==-1)
    cat(-99,99,Stock[[Istock]]$Ass$TVR0Off[2],Stock[[Istock]]$Ass$TVR0Off[2],0.8,0,Stock[[Istock]]$Ass$TVR0Off[3],"#",paste0("TV_R0_offset"),"\n",file=file.name,append=T)
  
  write("2 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty",file=file.name,append=T)
  cat(Ass$SS_rec_dev_start+YrOffset+1,"# first year of main recr_devs; early devs can precede this era\n",file=file.name,append=T)
  cat(Year-Ass$SS_rec_dev_end+YrOffset,"# last year of main recr_devs; forecast devs start in following year\n",file=file.name,append=T)
  Nrec_est <- (Year-Ass$SS_rec_dev_end) - Ass$SS_rec_dev_start + 1

  write("2 #_recdev phase ",file=file.name,append=T)
  write("1 # (0/1) to read 13 advanced options",file=file.name,append=T)
  cat(Ass$SS_early_dev_yr1," #_recdev_early_start (0=none; neg value makes relative to recdev_start)\n",file=file.name,append=T)
  cat(Ass$SS_early_dev_phase," #_recdev_early_phase\n",file=file.name,append=T)
  write("-1 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)",file=file.name,append=T)
  write("1 #_lambda for Fcast_recr_like occurring before endyr+1",file=file.name,append=T)
  # AEP Adjusted for this case
  if (TestCase==T) 
   {
    cat(Ass$SS_last_yr_nobias_adj+YrOffset,"#_last_yr_nobias_adj_in_MPD; begin of ramp\n",file=file.name,append=T)
    cat(Ass$SS_first_yr_fullbias_adj+YrOffset,"#_first_yr_fullbias_adj_in_MPD; begin of plateau\n",file=file.name,append=T)
    cat(Year+Ass$SS_last_yr_fullbias_adj+YrOffset,"#_last_yr_fullbias_adj_in_MPD\n",file=file.name,append=T)
    cat(Year+Ass$SS_end_yr_for_ramp+YrOffset,"#_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS3 sets bias_adj to 0.0 for fcast yrs)\n",file=file.name,append=T)
   }
  if (TestCase==F) 
   {
    cat(Ass$SS_last_yr_nobias_adj+YrOffset,"#_last_yr_nobias_adj_in_MPD; begin of ramp\n",file=file.name,append=T)
    cat(Ass$SS_first_yr_fullbias_adj+YrOffset,"#_first_yr_fullbias_adj_in_MPD; begin of plateau\n",file=file.name,append=T)
    cat(Year+Ass$SS_last_yr_fullbias_adj+YrOffset,"#_last_yr_fullbias_adj_in_MPD\n",file=file.name,append=T)
    cat(Year+Ass$SS_end_yr_for_ramp+YrOffset,"#_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS3 sets bias_adj to 0.0 for fcast yrs)\n",file=file.name,append=T)
   }
  cat(Ass$SS_max_bias_adjust," #_max_bias_adj_in_MPD (typical ~0.8; -3 sets all years to 0.0; -2 sets all non-forecast yrs w/ estimated recdevs to 1.0; -1 sets biasadj=1.0 for all yrs w/ recdevs)\n",file=file.name,append=T)
  write("0 #_period of cycles in recruitment (N parms read below)",file=file.name,append=T)
  write("-5 #min rec_dev",file=file.name,append=T)
  write("5 #max rec_dev",file=file.name,append=T)
  write("0 #_read_recdevs",file=file.name,append=T)
  write("#_end of advanced SR options",file=file.name,append=T)
  write("#",file=file.name,append=T)

  # Fishing mortality
  write("#Fishing Mortality info",file=file.name,append=T) 
  write("0.2 # F ballpark value in units of annual_F",file=file.name,append=T)
  write("-75 # F ballpark year (neg value to disable)",file=file.name,append=T)
  write("3 # F_Method:  1=Pope midseason rate; 2=F as parameter; 3=F as hybrid; 4=fleet-specific parm/hybrid (#4 is superset of #2 and #3 and is recommended)",file=file.name,append=T)
  write("4 # max F (methods 2-4) or harvest fraction (method 1)",file=file.name,append=T)
  write("5  # N iterations for tuning in hybrid mode; recommend 3 (faster) to 5 (more precise if many fleets)",file=file.name,append=T)
  write("#",file=file.name,append=T)
  
 
  write("#",file=file.name,append=T) 
  write("#_initial_F_parms",file=file.name,append=T) 
  for (Ifleet in 1:General$Ncat_fleet)
   if (Stock[[Istock]]$Finitial[Ifleet]>0)
     cat(Do_Jitter(Istock,1,Stock[[Istock]]$Finitial[Ifleet],-3,Bound_mult,Jitter_SD,-1),paste0("# F_initr_",Ifleet),"\n",file=file.name,append=T)
  #     cat(Do_Jitter(Istock,1,Stock[[Istock]]$Finitial[Ifleet],Ass$Retain_par_phase[Ifleet,Ipar],Bound_mult,Jitter_SD,0,TV=TV),paste0("# Retain_par_",Ipar,"_",General$Fleet.Names[Ifleet]),"\n",file=file.name,append=T)
  
  # Q setup
  write("#_Q_setup for fleets with cpue or survey data",file=file.name,append=T)
  write("#_1:  fleet number",file=file.name,append=T)
  write("#_2:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm; 4=mirror with offset, 2 parm)",file=file.name,append=T)
  write("#_3:  extra input for link, i.e. mirror fleet# or dev index number",file=file.name,append=T)
  write("#_4:  0/1 to select extra sd parameter",file=file.name,append=T)
  write("#_5:  0/1 for biasadj or not",file=file.name,append=T)
  write("#_6:  0/1 to float",file=file.name,append=T)
  write("#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname",file=file.name,append=T)
  for (Ifleet in 1:General$Nfleet)
   if (Stock[[Istock]]$Data$Is_index[Ifleet]=="Yes") cat(Ifleet,1,0,0,0,0,"#",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt=="Opt_5" || Stock[[Istock]]$Ass$Pred_opt=="Opt_3")
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
     if (Stock[[Istock]]$Data$Is_pred.no[Ipred])
      cat(Ipred+General$Nfleet,1,0,0,0,0,"#",paste0("Predator_",Ipred),"\n",file=file.name,append=T)
  write("-9999 0 0 0 0 0",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("#_Q_parms(if_any);Qunits_are_ln(q)",file=file.name,append=T)
  write("#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name",file=file.name,append=T)
  Qinit <- rep(0,General$Nfleet+Stock[[Istock]]$Num.Pred)
  if (General$Nfleet+Stock[[Istock]]$Num.Pred>0)
   for (Index in 1:(General$Nfleet+Stock[[Istock]]$Num.Pred))
    if (Stock[[Istock]]$Ass$Qphase[Index])  Qinit[Index] <- log(Stock[[Istock]]$Ass$Qinit[Index])
  for (Ifleet in 1:General$Nfleet)
    if (Stock[[Istock]]$Data$Is_index[Ifleet]=="Yes") cat(-15,15,Qinit[Ifleet],Qinit[Ifleet],10,0,Stock[[Istock]]$Ass$Qphase[Ifleet],rep(0,7),"#",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt=="Opt_5" || Stock[[Istock]]$Ass$Pred_opt=="Opt_3")
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
     if (Stock[[Istock]]$Data$Is_pred.no[Ipred])
      cat(-15,15,Qinit[General$Nfleet+Ipred],Qinit[General$Nfleet+Ipred],10,0,Stock[[Istock]]$Ass$Qphase[General$Nfleet+Ipred],rep(0,7),"#",paste0("Predator_",Ipred),"\n",file=file.name,append=T)
    
  #Size_Selectivity
  write("#_no timevary Q parameters",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("#_size_selex_patterns",file=file.name,append=T)
  write("#Pattern:_0;  parm=0; selex=1.0 for all sizes",file=file.name,append=T)
  write("#Pattern:_1;  parm=2; logistic; with 95% width specification",file=file.name,append=T)
  write("#Pattern:_5;  parm=2; mirror another size selex; PARMS pick the min-max bin to mirror",file=file.name,append=T)
  write("#Pattern:_11; parm=2; selex=1.0  for specified min-max population length bin range",file=file.name,append=T)
  write("#Pattern:_15; parm=0; mirror another age or length selex",file=file.name,append=T)
  write("#Pattern:_6;  parm=2+special; non-parm len selex",file=file.name,append=T)
  write("#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (mean over bin range)",file=file.name,append=T)
  write("#Pattern:_8;  parm=8; double_logistic with smooth transitions and constant above Linf option",file=file.name,append=T)
  write("#Pattern:_9;  parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset",file=file.name,append=T)
  write("#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex",file=file.name,append=T)
  write("#Pattern:_22; parm=4; double_normal as in CASAL",file=file.name,append=T)
  write("#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0",file=file.name,append=T)
  write("#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners",file=file.name,append=T)
  write("#Pattern:_2;  parm=6; double_normal with sel(minL) and sel(maxL), using joiners, back compatibile version of 24 with 3.30.18 and older",file=file.name,append=T)
  write("#Pattern:_25; parm=3; exponential-logistic in length",file=file.name,append=T)
  write("#Pattern:_27; parm=special+3; cubic spline in length; parm1==1 resets knots; parm1==2 resets all ",file=file.name,append=T)
  write("#Pattern:_42; parm=special+3+2; cubic spline; like 27, with 2 additional param for scaling (mean over bin range)",file=file.name,append=T)
  write("#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention",file=file.name,append=T)
  write("#_Pattern Discard Male Special",file=file.name,append=T)
  for (Ifleet in 1:General$Nfleet)
   {
    Discard <- 0; Special <- 0
    if (Ifleet <= General$Ncat_fleet) Discard <-  Ass$Retain_size_pattern[Ifleet]
    if (Ass$Selex_size_mirror_fleet[Ifleet]!=0) Special <- Ass$Selex_size_mirror_fleet[Ifleet]
    Male <- Ass$Selex_size_pattern_male[Ifleet]
    cat(Ass$Selex_size_pattern[Ifleet],Discard,Male,Special,"#",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)
   }
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    cat(0,0,0,0,"#",paste0("Predator_",Ipred),"\n",file=file.name,append=T)
  
  # Age_Selectivity
  write("#",file=file.name,append=T)
  write("#_age_selex_patterns",file=file.name,append=T)
  write("#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage",file=file.name,append=T)
  write("#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage",file=file.name,append=T)
  write("#Pattern:_11; parm=2; selex=1.0  for specified min-max age",file=file.name,append=T)
  write("#Pattern:_12; parm=2; age logistic",file=file.name,append=T)
  write("#Pattern:_13; parm=8; age double logistic. Recommend using pattern 18 instead.",file=file.name,append=T)
  write("#Pattern:_14; parm=nages+1; age empirical",file=file.name,append=T)
  write("#Pattern:_15; parm=0; mirror another age or length selex",file=file.name,append=T)
  write("#Pattern:_16; parm=2; Coleraine - Gaussian",file=file.name,append=T)
  write("#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero",file=file.name,append=T)
  write("#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (mean over bin range)",file=file.name,append=T)
  write("#Pattern:_18; parm=8; double logistic - smooth transition",file=file.name,append=T)
  write("#Pattern:_19; parm=6; simple 4-parm double logistic with starting age",file=file.name,append=T)
  write("#Pattern:_20; parm=6; double_normal,using joiners",file=file.name,append=T)
  write("#Pattern:_26; parm=3; exponential-logistic in age",file=file.name,append=T)
  write("#Pattern:_27; parm=3+special; cubic spline in age; parm1==1 resets knots; parm1==2 resets all ",file=file.name,append=T)
  write("#Pattern:_42; parm=2+special+3; // cubic spline; with 2 additional param for scaling (mean over bin range)",file=file.name,append=T)
  write("#Age patterns entered with value >100 create Min_selage from first digit and pattern from remainder",file=file.name,append=T)
  write("#_Pattern Discard Male Special",file=file.name,append=T)
  for (Ifleet in 1:General$Nfleet)
  {
    Discard <- 0; Special <- 0
    if (Ifleet <= General$Ncat_fleet) Discard <-  Ass$Retain_age_pattern[Ifleet]
    Male <- Ass$Selex_age_pattern_male[Ifleet]
    if (Ass$Selex_age_mirror_fleet[Ifleet]!=0) Special <- Ass$Selex_age_mirror_fleet[Ifleet]
    cat(Ass$Selex_age_pattern[Ifleet],Discard,Male,Special,"#",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)
  }
  
  #for (Ifleet in 1:General$Nfleet)
  #  cat("10 0 0 0 #",General$Fleet.Names[Ifleet],"\n",file=file.name,append=T)
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    cat(12,3,0,0,"#",paste0("Predator_",Ipred),"\n",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name",file=file.name,append=T)
  for (Ifleet in 1:General$Nfleet)
   {
    cat("#",Ifleet,General$Fleet.Names[Ifleet],"LENSELEX\n",file=file.name,append=T) 
    if (Ass$N.size.selexPars[Ifleet] >0)
     for (Ipar in 1:Ass$N.size.selexPars[Ifleet])
      {
       TV <- rep(0,9)
       TV[4] <- Ass$Selex_size_ann_devs_use[Ifleet,Ipar]; TV[5] <- Ass$Selex_size_ann_devs_miny[Ifleet,Ipar]; TV[6] <- Ass$Selex_size_ann_devs_maxy[Ifleet,Ipar];TV[7] <- Ass$Selex_size_ann_devs_phs[Ifleet,Ipar];
       TV[8] <- Ass$Selex_size_par_blks[Ifleet,Ipar]; TV[9] <- Ass$Selex_size_par_fns[Ifleet,Ipar]
       cat(Do_Jitter(Istock,1,Ass$Selex_size_par_init[Ifleet,Ipar],Ass$Selex_size_par_phase[Ifleet,Ipar],Bound_mult,Jitter_SD,0,TV=TV),paste0("# Size_par_",Ipar,"_",General$Fleet.Names[Ifleet]),"\n",file=file.name,append=T)
      }
     if (Ifleet <= General$Ncat_fleet)
      {
       if (Ass$Retain_size_pattern[Ifleet] %in% c(1,2))
        {
         for (Ipar in 1:4)
          {
           TV <- rep(0,9)
           TV[4] <- Ass$Retain_size_ann_devs_use[Ifleet,Ipar]; TV[5] <- Ass$Retain_size_ann_devs_miny[Ifleet,Ipar]; TV[6] <- Ass$Retain_size_ann_devs_maxy[Ifleet,Ipar];TV[7] <- Ass$Retain_size_ann_devs_phs[Ifleet,Ipar];
           TV[8] <- Ass$Retain_size_par_blks[Ifleet,Ipar]; TV[9] <- Ass$Retain_size_par_fns[Ifleet,Ipar]
           cat(Do_Jitter(Istock,1,Ass$Retain_size_par_init[Ifleet,Ipar],Ass$Retain_size_par_phase[Ifleet,Ipar],Bound_mult,Jitter_SD,0,TV=TV),paste0("# Retain_par_",Ipar,"_",General$Fleet.Names[Ifleet]),"\n",file=file.name,append=T)
          }
         }
       if (Ass$Retain_size_pattern[Ifleet] %in% c(4))
        {
         for (Ipar in 1:(5+Nsex))
         {
           TV <- rep(0,9)
           TV[4] <- Ass$Retain_size_ann_devs_use[Ifleet,Ipar]; TV[5] <- Ass$Retain_size_ann_devs_miny[Ifleet,Ipar]; TV[6] <- Ass$Retain_size_ann_devs_maxy[Ifleet,Ipar];TV[7] <- Ass$Retain_size_ann_devs_phs[Ifleet,Ipar];
           TV[8] <- Ass$Retain_size_par_blks[Ifleet,Ipar]; TV[9] <- Ass$Retain_size_par_fns[Ifleet,Ipar]
           cat(Do_Jitter(Istock,1,Ass$Retain_size_par_init[Ifleet,Ipar],Ass$Retain_size_par_phase[Ifleet,Ipar],Bound_mult,Jitter_SD,0,TV=TV),paste0("# Retain_par_",Ipar,"_",General$Fleet.Names[Ifleet]),"\n",file=file.name,append=T)
         }
       }
     }
    if (Ifleet <= General$Ncat_fleet)
      if (Ass$Retain_size_pattern[Ifleet] %in% c(2,4))
      {
        for (Ipar in 1:4)
        {
          TV <- rep(0,9)
          cat(Do_Jitter(Istock,1,Ass$Mort_size_par_init[Ifleet,Ipar],Ass$Mort_size_par_phase[Ifleet,Ipar],Bound_mult,Jitter_SD,0,TV=TV),paste0("# Mort_par_",Ipar,"_",General$Fleet.Names[Ifleet]),"\n",file=file.name,append=T)
        }
      }
    
   } # length selex pars
  if (Ass$Pred_op!="None")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    cat("#",Ifleet,paste0("Predator_",Ipred),"LENSELEX\n",file=file.name,append=T) 
   
  write("#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name",file=file.name,append=T)
  for (Ifleet in 1:General$Nfleet)
   {
    cat("#",Ifleet,General$Fleet.Names[Ifleet],"AGESELEX\n",file=file.name,append=T) 
    if (Ass$N.age.selexPars[Ifleet] >0)
      for (Ipar in 1:Ass$N.age.selexPars[Ifleet])
      {
        TV <- rep(0,9)
        TV[4] <- Ass$Selex_age_ann_devs_use[Ifleet,Ipar]; TV[5] <- Ass$Selex_age_ann_devs_miny[Ifleet,Ipar]; TV[6] <- Ass$Selex_age_ann_devs_maxy[Ifleet,Ipar];TV[7] <- Ass$Selex_age_ann_devs_phs[Ifleet,Ipar];
        TV[8] <- Ass$Selex_age_par_blks[Ifleet,Ipar]; TV[9] <- Ass$Selex_age_par_fns[Ifleet,Ipar]
        cat(Do_Jitter(Istock,1,Ass$Selex_age_par_init[Ifleet,Ipar],Ass$Selex_age_par_phase[Ifleet,Ipar],Bound_mult,Jitter_SD,0,TV=TV),paste0("# age_par_",Ipar,"_",General$Fleet.Names[Ifleet]),"\n",file=file.name,append=T)
      }
    if (Ifleet <= General$Ncat_fleet)
     {
      if (Ass$Retain_age_pattern[Ifleet] %in% c(1,2))
       {
        for (Ipar in 1:4)
        {
          TV <- rep(0,9)
          TV[4] <- Ass$Retain_age_ann_devs_use[Ifleet,Ipar]; TV[5] <- Ass$Retain_age_ann_devs_miny[Ifleet,Ipar]; TV[6] <- Ass$Retain_age_ann_devs_maxy[Ifleet,Ipar];TV[7] <- Ass$Retain_age_ann_devs_phs[Ifleet,Ipar];
          TV[8] <- Ass$Retain_age_par_blks[Ifleet,Ipar]; TV[9] <- Ass$Retain_age_par_fns[Ifleet,Ipar]
          cat(Do_Jitter(Istock,1,Ass$Retain_age_par_init[Ifleet,Ipar],Ass$Retain_age_par_phase[Ifleet,Ipar],Bound_mult,Jitter_SD,0,TV=TV),paste0("# Retain_par_",Ipar,"_",General$Fleet.Names[Ifleet]),"\n",file=file.name,append=T)
        }
       }
      if (Ass$Retain_age_pattern[Ifleet] %in% c(4))
      {
        for (Ipar in 1:(5+Nsex))
        {
          TV <- rep(0,9)
          TV[4] <- Ass$Retain_age_ann_devs_use[Ifleet,Ipar]; TV[5] <- Ass$Retain_age_ann_devs_miny[Ifleet,Ipar]; TV[6] <- Ass$Retain_age_ann_devs_maxy[Ifleet,Ipar];TV[7] <- Ass$Retain_age_ann_devs_phs[Ifleet,Ipar];
          TV[8] <- Ass$Retain_age_par_blks[Ifleet,Ipar]; TV[9] <- Ass$Retain_age_par_fns[Ifleet,Ipar]
          cat(Do_Jitter(Istock,1,Ass$Retain_age_par_init[Ifleet,Ipar],Ass$Retain_age_par_phase[Ifleet,Ipar],Bound_mult,Jitter_SD,0,TV=TV),paste0("# Retain_par_",Ipar,"_",General$Fleet.Names[Ifleet]),"\n",file=file.name,append=T)
        }
      }
    } 
    if (Ifleet <= General$Ncat_fleet)
      if (Ass$Retain_age_pattern[Ifleet] %in% c(2,4))
      {
        for (Ipar in 1:4)
        {
          TV <- rep(0,9)
          cat(Do_Jitter(Istock,1,Ass$Mort_age_par_init[Ifleet,Ipar],Ass$Mort_age_par_phase[Ifleet,Ipar],Bound_mult,Jitter_SD,0,TV=TV),paste0("# Mort_par_",Ipar,"_",General$Fleet.Names[Ifleet]),"\n",file=file.name,append=T)
        }
      }
    
   } # age selex pars
  
  
  for (Ifleet in 1:General$Nfleet)
    cat("#",Ifleet,General$Fleet.Names[Ifleet],"AGESELEX\n",file=file.name,append=T) 
  if (Stock[[Istock]]$Ass$Pred_opt!="None")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    {
     cat("#",Ifleet,paste0("Predator_",Ipred),"AGESELEX\n",file=file.name,append=T) 
     for (Ipar in 1:2)
      cat(Do_Jitter(Istock,1,Ass$Pred_Selex_par_init[Ipred,Ipar],Ass$Pred_Selex_par_phase[Ipred,Ipar],Bound_mult,Jitter_SD,0),paste0("# Age_par_",Ipar,"_",paste0("Predator_",Ipred)),"\n",file=file.name,append=T)
    }
  
  write("#_No_Dirichlet parameters",file=file.name,append=T)
  write("#_no timevary selex parameters",file=file.name,append=T)
  if (Ass$N.time.varying.selex.pars.size > 0)
   {
    write("#",file=file.name,append=T)
    write("#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE",file=file.name,append=T)
    for (Iseldevs in 1: Ass$N.non.dev.time.varying.selex.pars.size)
     {
      cat(Do_Jitter(Istock,1,Ass$Time.varying.selex.size.par.vals[Iseldevs],Ass$Time.varying.selex.size.par.phs[Iseldevs],Bound_mult,Jitter_SD,-1),paste0("# Age_par_",Ipar,"_",paste0("Sel/ret_dev_size_",Iseldevs)),"\n",file=file.name,append=T)
     }
   } # Sel/ret devs
  if (Ass$N.time.varying.selex.pars.age > 0)
  {
    write("#",file=file.name,append=T)
    write("#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE",file=file.name,append=T)
    for (Iseldevs in 1: Ass$N.non.dev.time.varying.selex.pars.age)
    {
      cat(Do_Jitter(Istock,1,Ass$Time.varying.selex.age.par.vals[Iseldevs],Ass$Time.varying.selex.age.par.phs[Iseldevs],Bound_mult,Jitter_SD,-1),paste0("# Age_par_",Ipar,"_",paste0("Sel/ret_dev_age_",Iseldevs)),"\n",file=file.name,append=T)
    }
  } # Sel/ret devs
  write("#",file=file.name,append=T)
  write("0   #  use 2D_AR1 selectivity? (0/1)",file=file.name,append=T)
  write("#_no 2D_AR1 selex offset used",file=file.name,append=T)
  write("#_specs:  fleet, ymin, ymax, amin, amax, sigma_amax, use_rho, len1/age2, devphase, before_range, after_range",file=file.name,append=T)
  write("#_sigma_amax>amin means create sigma parm for each bin from min to sigma_amax; sigma_amax<0 means just one sigma parm is read and used for all bins",file=file.name,append=T)
  write("#_needed parameters follow each fleet's specifications",file=file.name,append=T)
  write("# -9999  0 0 0 0 0 0 0 0 0 0 # terminator",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("# Tag loss and Tag reporting parameters go next",file=file.name,append=T)
  write("0  # TG_custom:  0=no read and autogen if tag data exist; 1=read",file=file.name,append=T)
  write("#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("# no timevary parameters",file=file.name,append=T)

  write("#",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("# Input variance adjustments factors:",file=file.name,append=T) 
  write("#_1=add_to_survey_CV",file=file.name,append=T)
  write("#_2=add_to_discard_stddev",file=file.name,append=T)
  write("#_3=add_to_bodywt_CV",file=file.name,append=T)
  write("#_4=mult_by_lencomp_N",file=file.name,append=T)
  write("#_5=mult_by_agecomp_N",file=file.name,append=T)
  write("#_6=mult_by_size-at-age_N",file=file.name,append=T)
  write("#_7=mult_by_generalized_sizecomp",file=file.name,append=T)
  write("#_Factor  Fleet  Value",file=file.name,append=T)
  write("-9999   1    0  # terminator",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("1 #_maxlambdaphase",file=file.name,append=T)
  write("1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter",file=file.name,append=T)
  write("# read 1 changes to default Lambdas (default value is 1.0)",file=file.name,append=T)
  write("# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; ",file=file.name,append=T)
  write("# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime",file=file.name,append=T)
  write("#like_comp fleet  phase  value  sizefreq_method",file=file.name,append=T)
  write("18 1 1 0 1",file=file.name,append=T)
  write("-9999  1  1  1  1  #  terminator",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("# lambdas (for info only; columns are phases)",file=file.name,append=T)
  write("0 # (0/1/2) read specs for more stddev reporting: 0 = skip, 1 = read specs for reporting stdev for selectivity, size, and numbers, 2 = add options for M,Dyn. Bzero, SmryBio",file=file.name,append=T)
  write("999",file=file.name,append=T)

  file.name <- paste0(Folder,"ss3.par")
  #print(file.name)
  write("# Number of parameters = 91 Objective function value = -52.6765980883531  Maximum gradient component = 2.45487067979909e-05",file=file.name)
  write("# dummy_parm:",file=file.name,append=T)
  write("1.00000000000",file=file.name,append=T)
  MG.par <- 0
  if (Ass$M.type==0)
   {
    MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
    write(Stock[[Istock]]$Mbase[1,1],file=file.name,append=T)
   }
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(Stock[[Istock]]$LenA1[1],file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(Stock[[Istock]]$LenA2[1],file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(Stock[[Istock]]$Kappa[1],file=file.name,append=T)
  if (Stock[[Istock]]$Growth.Model==2) 
   {
    MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
    write(Stock[[Istock]]$Richards[1],file=file.name,append=T)
   }  
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(Stock[[Istock]]$CV1[1],file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  if (Ass$SS_parameter_offset_approach == 1)
   write(Stock[[Istock]]$CV2[1],file=file.name,append=T)
  if (Ass$SS_parameter_offset_approach %in% c(2,3))
    write(log(Stock[[Istock]]$CV2[1]/Stock[[Istock]]$CV1[1]),file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(Stock[[Istock]]$WtLen_a[1],file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(Stock[[Istock]]$WtLen_b[1],file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(Stock[[Istock]]$Mat50,file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(Stock[[Istock]]$SlopeMat,file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(Stock[[Istock]]$Eggs.1,file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(Stock[[Istock]]$Eggs.2,file=file.name,append=T)
  if (Nsex == 2)
   {
    if (Ass$SS_parameter_offset_approach==1)
     {
      if (Ass$M.type==0)
       {
        MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
        write(Stock[[Istock]]$Mbase[2,1],file=file.name,append=T)
       }
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(Stock[[Istock]]$LenA1[2],file=file.name,append=T)
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(Stock[[Istock]]$LenA2[2],file=file.name,append=T)
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(Stock[[Istock]]$Kappa[2],file=file.name,append=T)
      if (Stock[[Istock]]$Growth.Model==2) 
       {
        MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
        write(Stock[[Istock]]$Richards[2],file=file.name,append=T)
       }  
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(Stock[[Istock]]$CV1[2],file=file.name,append=T)
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(Stock[[Istock]]$CV2[2],file=file.name,append=T)
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(Stock[[Istock]]$WtLen_a[2],file=file.name,append=T)
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(Stock[[Istock]]$WtLen_b[2],file=file.name,append=T)
     }
    if (Ass$SS_parameter_offset_approach==2)
     {
       if (Ass$M.type==0)
        {
         MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
         write(0,file=file.name,append=T)
        }
       MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
       write(log(Stock[[Istock]]$LenA1[2]/Stock[[Istock]]$LenA1[1]),file=file.name,append=T)
       MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
       write(log(Stock[[Istock]]$LenA2[2]/Stock[[Istock]]$LenA2[1]),file=file.name,append=T)
       MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
       write(log(Stock[[Istock]]$Kappa[2]/Stock[[Istock]]$Kappa[1]),file=file.name,append=T)
       if (Stock[[Istock]]$Growth.Model==2) 
        {
         MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
         write(log(Stock[[Istock]]$Richards[2]/Stock[[Istock]]$Richards[1]),file=file.name,append=T)
        }  
       MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
       write(log(Stock[[Istock]]$CV1[2]/Stock[[Istock]]$CV1[1]),file=file.name,append=T)
       MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
       write(log(Stock[[Istock]]$CV2[2]/Stock[[Istock]]$CV2[1]),file=file.name,append=T)
       MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
       write(Stock[[Istock]]$WtLen_a[2],file=file.name,append=T)
       MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
       write(Stock[[Istock]]$WtLen_b[2],file=file.name,append=T)
     }
    #if (Ass$SS_parameter_offset_approach==2)
    # {
    #  if (Ass$M.type==0)
    #   {
    #    MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
    #    write(0,file=file.name,append=T)
    #   }
    #  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
    #  write(log(Stock[[Istock]]$LenA1[2]/Stock[[Istock]]$LenA1[1]),file=file.name,append=T)
    ##  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
    #  write(log(Stock[[Istock]]$LenA2[2]/Stock[[Istock]]$LenA2[1]),file=file.name,append=T)
    #  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
    #  write(log(Stock[[Istock]]$Kappa[2]/Stock[[Istock]]$Kappa[1]),file=file.name,append=T)
    #  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
    ##  write(log(Stock[[Istock]]$CV1[2]/Stock[[Istock]]$CV1[1]),file=file.name,append=T)
    #  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
    #  write(log(Stock[[Istock]]$CV2[2]/Stock[[Istock]]$CV2[1]),file=file.name,append=T)
    #  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
    #  write(Stock[[Istock]]$WtLen_a[2],file=file.name,append=T)
    #  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
    #  write(Stock[[Istock]]$WtLen_b[2],file=file.name,append=T)
    #}
    if (Ass$SS_parameter_offset_approach==3)
     {
      if (Ass$M.type==0)
       {
        MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
        write(0,file=file.name,append=T)
       }
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(log(Stock[[Istock]]$LenA1[2]/Stock[[Istock]]$LenA1[1]),file=file.name,append=T)
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(log(Stock[[Istock]]$LenA2[2]/Stock[[Istock]]$LenA2[1]),file=file.name,append=T)
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(log(Stock[[Istock]]$Kappa[2]/Stock[[Istock]]$Kappa[1]),file=file.name,append=T)
      if (Stock[[Istock]]$Growth.Model==2) 
       {
        MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
        write(log(Stock[[Istock]]$Richards[2]/Stock[[Istock]]$Richards[1]),file=file.name,append=T)
       }  
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(log(Stock[[Istock]]$CV1[2]/Stock[[Istock]]$CV1[1]),file=file.name,append=T)
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(log(Stock[[Istock]]$CV2[2]/Stock[[Istock]]$CV2[1]),file=file.name,append=T)
      MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
      write(Stock[[Istock]]$WtLen_a[2],file=file.name,append=T)
      MG.par <- MG.par + 1; write(paste0("#MG par ",MG.par),file=file.name,append=T)
      write(Stock[[Istock]]$WtLen_b[2],file=file.name,append=T)
    }
    
   }
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(1.0,file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(1.0,file=file.name,append=T)
  MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
  write(0.5,file=file.name,append=T)
  if (Ass$N_dev_MG>0)
   for (Ipar in 1:Ass$N_dev_MG)
    {
     MG.par <- MG.par + 1; write(paste0("#MG_par ",MG.par),file=file.name,append=T)
     write(Stock[[Istock]]$Ass$MG_devs[Ipar],file=file.name,append=T)
    }
  
  write("#SR_par 1",file=file.name,append=T)
  write(log(sum(Stock[[Istock]]$R00.orig[Use])*Ass$SR_R0_mult),file=file.name,append=T)
  write("#SR_par 2",file=file.name,append=T)
  write(Stock[[Istock]]$Steep,file=file.name,append=T)
  write("#SR_par 3",file=file.name,append=T)
  write(Stock[[Istock]]$SigmaR,file=file.name,append=T)
  write("#SR_par 4",file=file.name,append=T)
  write(0,file=file.name,append=T)
  write("#SR_par 5",file=file.name,append=T)
  write(0,file=file.name,append=T)
  if (Stock[[Istock]]$Ass$N_dev_SR>0)
    for (Ipar in 1:Stock[[Istock]]$Ass$N_dev_SR)
    {
      write(paste0("#SR_par ",Ipar+5),file=file.name,append=T)
      write(Stock[[Istock]]$Ass$SR_devs[Ipar],file=file.name,append=T)
    }
  write("# recdev2:",file=file.name,append=T)
  Recs.out <- NULL
  Recs.out <- c(Stock[[Istock]]$Ass$EarlyDevsEst,Stock[[Istock]]$Ass$LateDevsEst)
  if (Year>=Stock[[Istock]]$Ass$Late.range.est[2]+1)
  for (Iyr in max(1,Stock[[Istock]]$Ass$Late.range.est[2]+1):Year)
   {
     Iyear <- Iyr+YrOffset
     if (Iyear < Ass$SS_last_yr_nobias_adj+YrOffset) 
       Bias <- 0
     else
      if (Iyear < Ass$SS_first_yr_fullbias_adj+YrOffset)
        Bias <- (Iyear-(Ass$SS_last_yr_nobias_adj+YrOffset))/(Ass$SS_first_yr_fullbias_adj-Ass$SS_last_yr_nobias_adj)
      else
        if (Iyear < Year+Ass$SS_last_yr_fullbias_adj+YrOffset)
        Bias <- 1
        else 
          if (Iyear < Year+Ass$SS_end_yr_for_ramp+YrOffset)
            Bias <- (Year+Ass$SS_last_yr_fullbias_adj+YrOffset- Iyr)/(Ass$SS_end_yr_for_ramp-Ass$SS_last_yr_fullbias_adj+YrOffset)
         else
            Bias <- 0
          #Bias <- Bias*Ass$SS_max_bias_adjust
         if (Iyear < Year-Ass$SS_rec_dev_end+YrOffset)
           Rec.dev.yr <- Stock[[Istock]]$Rec_devs[Iyr]+Bias*Stock[[Istock]]$SigmaR^2/2.0
         else
          Rec.dev.yr <- Stock[[Istock]]$Rec_devs[Iyr]
         #cat(Iyr,Iyear,Bias,Bias*Stock[[Istock]]$SigmaR,Stock[[Istock]]$Rec_devs[Iyr],Rec.dev.yr,"\n")   
         Recs.out <- c(Recs.out,Rec.dev.yr)
     }
  cat(Recs.out,"\n",file=file.name,append=T)

  write("# Fcast_recruitments:",file=file.name,append=T)
  cat(rep(0,50),"\n",file=file.name,append=T)
  
  for (Ifleet in 1:General$Ncat_fleet)
   if (Stock[[Istock]]$Finitial[Ifleet]>0)
     {
      write(paste0("# F_initr_",Ifleet),file=file.name,append=T)
      write(Stock[[Istock]]$Finitial[Ifleet],file=file.name,append=T)
    
     }
 
  for (Ifleet in 1:General$Nfleet)
    if (Stock[[Istock]]$Data$Is_index[Ifleet]=="Yes")
     {
      write(paste0("# Q_parm[",Ifleet,"]:"),file=file.name,append=T)
      write("0.00000000000",file=file.name,append=T)
     }
  if (Stock[[Istock]]$Ass$Pred_opt=="Opt_5" || Stock[[Istock]]$Ass$Pred_opt=="Opt_3")
   for (Ipred in 1:Stock[[Istock]]$Num.Pred)
    if (Stock[[Istock]]$Data$Is_pred.no[Ipred])
     {
      write(paste0("# Q_parm[",Ipred+General$Nfleet,"]:"),file=file.name,append=T)
      write("0.00000000000",file=file.name,append=T)
     }
  
  Kpar <- 0
  for (Ifleet in 1:General$Nfleet)
   {
    for (Jpar in 1:15)
     if (Ass$Selex_size_par_phase[Ifleet,Jpar] != 0)
      {
       Kpar <- Kpar + 1
       cat("# selparm[",Kpar,"]:\n",file=file.name,append=T) 
       cat(Ass$Selex_size_par_init[Ifleet,Jpar],"\n",file=file.name,append=T) 
      } 
    for (Jpar in 1:15)
      if (Ass$Retain_size_par_phase[Ifleet,Jpar] != 0)
      {
        Kpar <- Kpar + 1
        cat("# retparm[",Kpar,"]:\n",file=file.name,append=T) 
        cat(Ass$Retain_size_par_init[Ifleet,Jpar],"\n",file=file.name,append=T) 
      } 
    for (Jpar in 1:15)
      if (Ass$Mort_size_par_phase[Ifleet,Jpar] != 0)
      {
        Kpar <- Kpar + 1
        cat("# mortparm[",Kpar,"]:\n",file=file.name,append=T) 
        cat(Ass$Mort_size_par_init[Ifleet,Jpar],"\n",file=file.name,append=T) 
      } 
  } # Ifleet
  for (Ifleet in 1:General$Nfleet)
  {
    for (Jpar in 1:15)
      if (Ass$Selex_age_par_phase[Ifleet,Jpar] != 0)
      {
        Kpar <- Kpar + 1
        cat("# selparm[",Kpar,"]:\n",file=file.name,append=T) 
        cat(Ass$Selex_age_par_init[Ifleet,Jpar],"\n",file=file.name,append=T) 
      } 
    for (Jpar in 1:15)
      if (Ass$Retain_age_par_phase[Ifleet,Jpar] != 0)
      {
        Kpar <- Kpar + 1
        cat("# retparm[",Kpar,"]:\n",file=file.name,append=T) 
        cat(Ass$Retain_age_par_init[Ifleet,Jpar],"\n",file=file.name,append=T) 
      } 
    for (Jpar in 1:15)
      if (Ass$Mort_age_par_phase[Ifleet,Jpar] != 0)
      {
        Kpar <- Kpar + 1
        cat("# Mortparm[",Kpar,"]:\n",file=file.name,append=T) 
        cat(Ass$Mort_age_par_init[Ifleet,Jpar],"\n",file=file.name,append=T) 
      } 
  } # Ifleet
  if (Ass$N.time.varying.selex.pars.size>0)
   for (Iselex in 1:Ass$N.time.varying.selex.pars.size)
    {
     Kpar <- Kpar + 1 
     cat("# sel.dev.parm[",Kpar,"]:\n",file=file.name,append=T) 
     cat(Ass$Time.varying.selex.size.par.vals[Iselex],"\n",file=file.name,append=T) 
    }  
  if (Ass$N.time.varying.selex.pars.age>0)
    for (Iselex in 1:Ass$N.time.varying.selex.pars.age)
    {
      Kpar <- Kpar + 1 
      cat("# sel.dev.parm[",Kpar,"]:\n",file=file.name,append=T) 
      cat(Ass$Time.varying.selex.age.par.vals[Iselex],"\n",file=file.name,append=T) 
    }  
  
  write("# checksum999:",file=file.name,append=T)
  write("999",file=file.name,append=T)
  file.copy(from=paste0(Folder,"ss3.par"),to=paste0(Folder,"ss3a.par"))
 }

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

Write_forecast <- function(Folder,Istock,Year)
 {
  Nsex <- Stock[[Istock]]$Nsex
  MaxAge <- Stock[[Istock]]$MaxAge
  Nlen <- Stock[[Istock]]$Nlen
  
  file.name <- paste0(Folder,"forecast.ss")
  #print(file.name)
  write(paste0("# forecast.ss for",Folder,"\n"),file=file.name,append=T)
  
  write("# for all year entries except rebuilder; enter either: actual year, -999 for styr, 0 for endyr, neg number for rel. endyr",file=file.name,append=T)
  write("1 # Benchmarks: 0=skip; 1=calc F_spr,F_btgt,F_msy; 2=calc F_spr,F0.1,F_msy; 3=add F_Blimit; ",file=file.name,append=T)
  cat(Stock[[Istock]]$Ass$MSY," # Do_MSY: 1= set to F(SPR); 2=calc F(MSY); 3=set to F(Btgt) or F0.1; 4=set to F(endyr); 5=calc F(MEY) with MSY_unit options\n",file=file.name,append=T)  
  write("# if Do_MSY=5, enter MSY_Units; then list fleet_ID, cost/F, price/mt, include_in_Fmey_scaling; # -fleet_ID to fill; -9999 to terminate",file=file.name,append=T)
  cat(Stock[[Istock]]$Ass$SPR_target,"# SPR target (e.g. 0.40)\n",file=file.name,append=T)
  cat(Stock[[Istock]]$Ass$Biomass_target,"# Biomass target (e.g. 0.40)\n",file=file.name,append=T)
  write("#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF, beg_recr_dist, end_recr_dist, beg_SRparm, end_SRparm (enter actual year, or values of 0 or -integer to be rel. endyr)",file=file.name,append=T)
  write(" -999 -999    0    0    0    0    0    0    0    0",file=file.name,append=T)
  write("#-999 -999 -999 -999 -999 -999 -999 -999 -999 -999",file=file.name,append=T)
  write("#  75 75 75 75 75 75 75 75 75 75",file=file.name,append=T)
  write("# value <0 convert to endyr-value; except -999 converts to start_yr; must be >=start_yr and <=endyr",file=file.name,append=T)
  write("1 #Bmark_relF_Basis: 1 = use year range; 2 = set relF same as forecast below",file=file.name,append=T)
  write("#",file=file.name,append=T)
  cat(Stock[[Istock]]$Ass$Forecast_type," # Forecast: -1=none; 0=simple_1yr; 1=F(SPR); 2=F(MSY) 3=F(Btgt) or F0.1; 4=Ave F (uses first-last relF yrs); 5=input annual F scalar\n",file=file.name,append=T)
  write("# where none and simple require no input after this line; simple sets forecast F same as end year F",file=file.name,append=T)
  write("50 # N forecast years ",file=file.name,append=T)
  write("0 # Fmult (only used for Do_Forecast==5) such that apical_F(f)=Fmult*relF(f)",file=file.name,append=T)
  write("#_Fcast_years for averaging:  beg_selex, end_selex, beg_relF, end_relF, beg_mean recruits, end_recruits  (enter actual year, or values of 0 or -integer to be rel. endyr)",file=file.name,append=T)
  write("0 0 0 0 -999 0",file=file.name,append=T)
  write("#  75 75 75 75 1 75",file=file.name,append=T)
  write("0 # Forecast selectivity (0=fcast selex is mean from year range; 1=fcast selectivity from time-vary parms). NOTE: logic reverses in new format",file=file.name,append=T)
  write("# A revised protocol for the Fcast_yr specification is available and recommended. Template is below.",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("#-12345  # code to invoke new format for expanded fcast year controls",file=file.name,append=T)
  write("# biology and selectivity vectors are updated annually in the forecast according to timevary parameters, so check end year of blocks and dev vectors",file=file.name,append=T)
  write("# input in this section directs creation of means over historical years to override any time_vary changes",file=file.name,append=T)
  write("# Factors implemented so far: 1=M, 4=recr_dist, 5=migration, 10=selectivity, 11=rel_F, 12=recruitment",file=file.name,append=T)
  write("# rel_F and Recruitment also have additional controls later in forecast.ss",file=file.name,append=T)
  write("# input as list: Factor, method (0, 1), st_yr, end_yr",file=file.name,append=T)
  write("# Terminate with -9999 for Factor",file=file.name,append=T)
  write("# st_yr and end_yr input can be actual year; <=0 sets rel. to timeseries endyr; Except -999 for st_yr sets to first year if time series",file=file.name,append=T)
  write("# Method = 0 (or omitted) continue using time_vary parms; 1  use mean of derived factor over specified year range",file=file.name,append=T)
  write("# Factor method st_yr end_yr ",file=file.name,append=T)
  write("# 10 1 0 0 # selectivity; use:  10 1 0 0",file=file.name,append=T)
  write("# 11 1 0 0 # rel_F; use:  11 1 0 0",file=file.name,append=T)
  write("# 12 1 -999 0 # recruitment; use:  12 1 -999 0",file=file.name,append=T)
  write("#-9999 0 0 0",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("2 # Control rule method (0: none; 1: ramp does catch=f(SSB), buffer on F; 2: ramp does F=f(SSB), buffer on F; 3: ramp does catch=f(SSB), buffer on catch; 4: ramp does F=f(SSB), buffer on catch) ",file=file.name,append=T)
  write("# values for top, bottom and buffer exist, but not used when Policy=0",file=file.name,append=T)
  cat(Stock[[Istock]]$Ass$C_inflect," # Control rule inflection for constant F (as frac of Bzero, e.g. 0.40); must be > control rule cutoff, or set to -1 to use Bmsy/SSB_unf\n ",file=file.name,append=T)
  cat(Stock[[Istock]]$Ass$C_NoF," # Control rule cutoff for no F (as frac of Bzero, e.g. 0.10)\n ",file=file.name,append=T)  
  if (Stock[[Istock]]$Ass$C_NoF < 0)
    cat(Stock[[Istock]]$Ass$C_Prot," # Protection level (as frac of Bzero, e.g. 0.20)\n ",file=file.name,append=T)  
  cat(Stock[[Istock]]$Ass$C_Buffer," # Buffer:  enter Control rule target as fraction of Flimit (e.g. 0.75), negative value invokes list of [year, scalar] with filling from year to YrMax\n ",file=file.name,append=T)
  write("#",file=file.name,append=T) 
  write("3 #_N forecast loops (1=OFL only; 2=ABC; 3=get F from forecast ABC catch with allocations applied",file=file.name,append=T)
  write("3 # First forecast loop with stochastic recruitment",file=file.name,append=T)
  write("0 # Forecast base recruitment:  0= spawn_recr; 1=mult*spawn_recr_fxn; 2=mult*VirginRecr; 3=deprecated; 4=mult*mean_over_yr_range",file=file.name,append=T)
  write("# for option 4, set phase for fore_recr_devs to -1 in control to get constant mean in MCMC, else devs will be applied",file=file.name,append=T)
  write("1 # Value multiplier is ignored",file=file.name,append=T)
  write("0 # not used",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("2019  # FirstYear for caps and allocations (should be after years with fixed inputs) ",file=file.name,append=T)
  write("0 # stddev of log(realized catch/target catch) in forecast (set value>0.0 to cause active impl_error)",file=file.name,append=T)
  write("0 # Do West Coast gfish rebuilder output: 0=no; 1=yes ",file=file.name,append=T)
  write("0 # Rebuilder:  first year catch could have been set to zero (Ydecl)(-1 to set to 1999)",file=file.name,append=T)
  write("0 # Rebuilder:  year for current age structure (Yinit) (-1 to set to endyear+1)",file=file.name,append=T)
  write("1 # fleet relative F:  1=use mean over year range; 2=read seas, fleet, alloc list below",file=file.name,append=T)
  write("# Note that fleet allocation values is used directly as F if Do_Forecast=4 ",file=file.name,append=T)
  write("3 # basis for fcast catch tuning and for fcast catch caps and allocation  (2=deadbio; 3=retainbio; 5=deadnum; 6=retainnum); NOTE: same units for all fleets",file=file.name,append=T)
  write("# Conditional input if relative F choice = 2",file=file.name,append=T)
  write("# enter list of:  season,  fleet, relF; if used, terminate with season=-9999",file=file.name,append=T)
  write("# 1 1 0.250744",file=file.name,append=T)
  write("# 1 2 0.323565",file=file.name,append=T)
  write("# 1 3 0.42569",file=file.name,append=T)
  write("# -9999 0 0  # terminator for list of relF",file=file.name,append=T)
  write("# enter list of: fleet number, max annual catch for fleets with a max; terminate with fleet=-9999",file=file.name,append=T)
  write("-9999 -1",file=file.name,append=T)
  write("# enter list of area ID and max annual catch; terminate with area=-9999",file=file.name,append=T)
  write("-9999 -1",file=file.name,append=T)
  write("# enter list of fleet number and allocation group assignment, if any; terminate with fleet=-9999",file=file.name,append=T)
  write("-9999 -1",file=file.name,append=T)
  write("#_if N allocation groups >0, list year, allocation fraction for each group",file=file.name,append=T) 
  write("# list sequentially because read values fill to end of N forecastv",file=file.name,append=T) 
  write("# terminate with -9999 in year field ",file=file.name,append=T)
  write("# no allocation groups",file=file.name,append=T)
  write("#",file=file.name,append=T)
  write("3 # basis for input Fcast catch: -1=read basis with each obs; 2=dead catch; 3=retained catch; 99=input apical_F; NOTE: bio vs num based on fleet's catchunits",file=file.name,append=T)
  write("#enter list of Fcast catches or Fa; terminate with line having year=-9999",file=file.name,append=T)
  write("#_Yr Seas Fleet Catch(or_F)",file=file.name,append=T)
  write("-9999 1 1 0",file=file.name,append=T) 
  write("#",file=file.name,append=T)
  write("999 # verify end of input",file=file.name,append=T) 
 }

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

Do.Insurance <- function(Isim,Istock,Year)
 {
  
  # Note: Year is the last year with data (not the quota year)
  
  # Extract indices from the SS output
  nsex <- modeloutput$nsexes
  endyr <- modeloutput$endyr
  nagebins <- modeloutput$nagebins
  Fleets <- which(modeloutput$fleet_type == 1)
  nfleet <- length(Fleets)
  
  TACs <- Stock[[Istock]]$TheTACs

  # Input for the projection code
  Proj <- NULL
  Proj$nsex <- nsex
  Proj$nagebins <- nagebins
  Proj$endyr <- endyr
  Proj$nfleet <- nfleet
  Proj$baseM <- modeloutput$Natural_Mortality$'0'[1]
  
  Index <- which(modeloutput$ageselex$Label==paste0(endyr,"_Fecund"))
  Fecund <- rep(0,nagebins); for (Iage in 1:nagebins) Fecund[Iage] <-as.numeric(modeloutput$ageselex[Index,7+Iage])
  SelAge <- matrix(0,nrow=nfleet,ncol=nagebins)
  SelAgeRetWght <- matrix(0,nrow=nfleet,ncol=nagebins)
  for (Ifleet in 1:nfleet)
   {
    Index <- which(modeloutput$ageselex$Label==paste0(endyr+1,"_",Ifleet,"_Asel2"))
    # Need to fix for multiple sexes
    print("Need to fix for multiple sexes")
    for (Iage in 1:nagebins) SelAge[Ifleet,Iage] <- as.numeric(modeloutput$ageselex[Index,7+Iage])
    Index <- which(modeloutput$ageselex$Label==paste0(endyr+1,"_",Ifleet,"_sel*ret*wt"))
    for (Iage in 1:nagebins) SelAgeRetWght[Ifleet,Iage] <-as.numeric(modeloutput$ageselex[Index,7+Iage])
   }
  # Relative exploitation rate by fleet
  Index <- which(modeloutput$exploitation$Yr==endyr+1)
  Exploit <- rep(0,nfleet); for (Ifleet in 1:nfleet) Exploit[Ifleet] <- as.numeric(modeloutput$exploitation[Index,6+Ifleet])
  # Numbers-at-age projection year 1
  Index <- which(modeloutput$natage$Yr==endyr+1 & modeloutput$natage$'Beg/Mid'=="B")
  NatAge <- rep(0,nagebins); for (Iage in 1:nagebins) NatAge[Iage] <-as.numeric(modeloutput$natage[Index,12+Iage])
  # Virgin recruitment
  Index <- which(modeloutput$derived_quants$Label =="Recr_Virgin"); R0 <- modeloutput$derived_quants$Value[Index]
  
  # Now save the results
  Proj$Exploit <- Exploit/sum(Exploit)
  Proj$Fecund <- Fecund
  Proj$SelAge <- SelAge
  Proj$SelAgeRetWght <- SelAgeRetWght
  Proj$NatAge <- NatAge
  Proj$R0 <- R0
  Proj$Nyear <- 10
  Proj$Nsim <- 50
  Proj$TAC_Year1 <- TACs[1]
  
  # Find the recruit and M devs (last 10 years)
  Years.for.sampling <- 10
  Index <- max(which(abs(modeloutput$recruit$raw_dev) > 1.0e-8))
  Recdevs <- modeloutput$recruit$raw_dev[Index+c(-1*(Years.for.sampling-1):0)]
  Index <- which(modeloutput$Natural_Mortality$Y==endyr)
  Mdevs <- log(modeloutput$Natural_Mortality$'0'[Index+c(-1*(Years.for.sampling-1):0)]/Proj$baseM)

  # Spawning biomass-per-recruit
  sprf0 <- SPR(Proj,0.0)
  Ftarget <- Get_SPR(Proj,0.41)
  Proj$sprf0 <- sprf0
  Proj$SSB0 <- R0* sprf0
  Proj$Ftarget <- Ftarget
  Proj$Inflect <- Stock[[Istock]]$Ass$C_inflect
  Proj$CutOff <- Stock[[Istock]]$Ass$C_NoF

  # Economics parameters
  Proj$Price <- Stock[[Istock]]$Price
  Proj$CostPerDay <- Stock[[Istock]]$CostPerDay
  Proj$DaysPerF1 <- Stock[[Istock]]$DaysPerF1
  Proj$DaysPerF2 <- Stock[[Istock]]$DaysPerF2
  Proj$FixedCosts <- Stock[[Istock]]$FixedCosts
  #print(str(Proj))

  # Base insurance on projected years
  NyearUse <- Stock[[Istock]]$Ass$Insurance_risk_ave 

  # Projection matrices (and sample rec_devs)
  Proj$RecDevs <- matrix(0,nrow=Proj$Nyear+1,ncol=Proj$Nsim)
  Proj$MDevs <- matrix(0,nrow=Proj$Nyear+1,ncol=Proj$Nsim)
  for (Isim in 1:Proj$Nsim) Proj$RecDevs[,Isim] <- sample(Recdevs,Proj$Nyear+1,replace=T)
  
  # Now project (Random rec devs and no Mdevs)
  for (Isim in 1:Proj$Nsim) Proj$MDevs[,Isim] <- 0
  ProjOut0 <- Project(Proj)
  TotalLoss0 <- sum(ProjOut0$Profit[,1:NyearUse])/1000
  TotalLoss0a <- ProjOut0$Profit[,1]/1000
  Revenue0a <- ProjOut0$Revenue[,1]/1000

  # Now project (Random rec devs and Mdevs)
  for (Isim in 1:Proj$Nsim) Proj$MDevs[,Isim] <- sample(Mdevs,Proj$Nyear+1,replace=T)
  ProjOut1 <- Project(Proj)
  TotalLoss1 <- sum(ProjOut1$Profit[,1:NyearUse])/1000
  TotalLoss1a <- ProjOut1$Profit[,1]/1000
  Revenue1a <- ProjOut1$Revenue[,1]/1000
  
  # Now project (Random rec devs and pessimistic Mdevs)
  Ms <- Mdevs[which(Mdevs>0.2)]
  for (Isim in 1:Proj$Nsim)
   if (length(Ms)>0) Proj$MDevs[,Isim] <- sample(Ms,Proj$Nyear+1,replace=T) else Proj$MDevs[,Isim] <- 0
  ProjOut2 <- Project(Proj)
  TotalLoss2 <- sum(ProjOut2$Profit[,1:NyearUse])/1000
  TotalLoss2a <- ProjOut2$Profit[,1]/1000
  Revenue2a <- ProjOut2$Revenue[,1]/1000
  
  Insurance.type <- Stock[[Istock]]$Ass$Insurance_type 
  Insurer_profit_margin <- Stock[[Istock]]$Ass$Insurer_profit_margin
  ThreshProfit <- 0

  # Is there really an M-event next year
  MdevFut <- Stock[[Istock]]$Mcat.Index[Year+1]

  # Now make the call
  ProbCME <- length(Ms)/length(Mdevs)
  CostOfInsurance <- ProbCME*(1.0+Insurer_profit_margin)
  
  # Random no for detecting heatwaves
  RandM <- runif(1,0,1)
  
  # Extract delta for use when deciding on insurance
  Delta <- Stock[[Istock]]$Ass$Delta

  for (Insurance.type in 1:11)
   {
  
    # Was a heatwave detected?
    Prob.correct <- 1
    if (Insurance.type %in% c(8)) Prob.correct <- 0.8
    if (Insurance.type %in% c(9)) Prob.correct <- 0.5
    HeatWave <- F
    # There is a heatwave - was it detected?
    if (MdevFut == 1) 
     if (RandM <= Prob.correct) HeatWave <- T    
    # There is no heatwave - was it detected?
    if (MdevFut != 1) 
      if (RandM <= 1-Prob.correct) HeatWave <- T    
    cat("Insure",Year,Insurance.type,ProbCME,Delta,TotalLoss0,TotalLoss1,TotalLoss2,MdevFut,HeatWave,"\n")
  
    #cat("Amount of insurance",TotalLoss,"\n")
    Stock[[Istock]]$Prob.Collapse.Ins[Year+1] <<- CostOfInsurance
    for (Ifleet in 1:nfleet)
     { 
      # Default
      Insurance.Bought <- 0
      Stock[[Istock]]$PayOut[Insurance.type,Ifleet,Year+1] <<- 0
      Stock[[Istock]]$Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- 0
      Stock[[Istock]]$Cost.Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- 0
    
      # Save the expected profit 
      Stock[[Istock]]$ExpectProfit.Fish[1,Ifleet,Year+1] <<- TotalLoss0a[Ifleet]
      Stock[[Istock]]$ExpectProfit.Fish[2,Ifleet,Year+1] <<- TotalLoss1a[Ifleet]
      Stock[[Istock]]$ExpectProfit.Fish[3,Ifleet,Year+1] <<- TotalLoss2a[Ifleet]
    
      # Status-quo
      if (Insurance.type==1)
       {
       }
      # Base insurance
      if (Insurance.type==2)
       {
        NetLoss <- ThreshProfit-TotalLoss1a[Ifleet]
        if (NetLoss > 0 & ProbCME <= Delta) 
         {
          Insurance.Bought <- max(0,NetLoss/(1-CostOfInsurance))
          Stock[[Istock]]$Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<-  Insurance.Bought
          Stock[[Istock]]$Cost.Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- Insurance.Bought*CostOfInsurance
         }
       }
      # Pessimistic
      if (Insurance.type==3)
       {
        NetLoss <- ThreshProfit-TotalLoss2a[Ifleet]
        if (NetLoss > 0 & ProbCME <= Delta) 
         {
          Insurance.Bought <- max(0,NetLoss/(1-CostOfInsurance))
          Stock[[Istock]]$Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- Insurance.Bought
          Stock[[Istock]]$Cost.Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- Insurance.Bought*CostOfInsurance
         }
       }
      # Loss with a CME compared to no CMEs (Jack)
      if (Insurance.type==4)
       {
        NetLoss <- ThreshProfit-TotalLoss2a[Ifleet]
        if (NetLoss > 0 & ProbCME <= Delta) 
         {
          Insurance.Bought <- max(0,NetLoss/(1-CostOfInsurance))
          Stock[[Istock]]$Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- Insurance.Bought
          Stock[[Istock]]$Cost.Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- Insurance.Bought*CostOfInsurance
         }
       }
      # With prediction of CME
      if (Insurance.type %in% c(5,8,9))
       {
        if (HeatWave==F) NetLoss <- ThreshProfit-TotalLoss1a[Ifleet]
        if (HeatWave==T) NetLoss <- ThreshProfit-TotalLoss2a[Ifleet]
        if (NetLoss > 0 & ProbCME <= Delta) 
         {
          Insurance.Bought <- max(0,NetLoss/(1-CostOfInsurance))
          Stock[[Istock]]$Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- Insurance.Bought
          Stock[[Istock]]$Cost.Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- Insurance.Bought*CostOfInsurance
        }
       } 
      # State supported insurance
      if (Insurance.type==6)
       {
        NetLoss <- 0
       }
      # Anticipatory insurance
      if (Insurance.type==7)
       {
        NetLoss <- 0
        TotalRevenue <- 0
        Insurance.Bought <- 0
        for (Iyear in (Year-4):(Year))
         TotalRevenue <- TotalRevenue + sum(Stock[[Istock]]$Revenue[,Ifleet,Iyear])/5
        PayOut <- 0
        if (Revenue1a[Ifleet] < 0.5*TotalRevenue) PayOut <- 0.55*(TotalRevenue-Revenue1a[Ifleet]) 
        Stock[[Istock]]$PayOut[Insurance.type,Ifleet,Year+1] <<- PayOut
       }
      #cat(Insurance.type,Year,Ifleet,NetLoss,NetLoss*CostOfInsurance,Insurance.Bought,Insurance.Bought*(1-CostOfInsurance),"\n")
      # Base insurance
      if (Insurance.type==10)
      {
        NetLoss <- ThreshProfit-TotalLoss1a[Ifleet]
        if (NetLoss > 0) 
        {
          Insurance.Bought <- max(0,NetLoss/(1-CostOfInsurance))
          Stock[[Istock]]$Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<-  Insurance.Bought
          Stock[[Istock]]$Cost.Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- Insurance.Bought*CostOfInsurance
        }
      }
      # Pessimistic
      if (Insurance.type==11)
      {
        NetLoss <- ThreshProfit-TotalLoss2a[Ifleet]
        if (NetLoss > 0) 
        {
          Insurance.Bought <- max(0,NetLoss/(1-CostOfInsurance))
          Stock[[Istock]]$Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- Insurance.Bought
          Stock[[Istock]]$Cost.Insurance.Bought.Fish[Insurance.type,Ifleet,Year+1] <<- Insurance.Bought*CostOfInsurance
        }
      }
    } #Insurance.type
    
  } # Ifleet
 }

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------

Run_Tier1a <- function(Isim,Istock,Iarea,Year)
 {
  #Check whether the target folder exists
  #print(General$RunFolder)
  Folder <- paste0(General$RunFolder,"SS_Sim_",Isim,"_",General$Stock.Names[Istock],"_",Year,"_",Iarea,"/")
  print(Folder)
  if (!dir.exists(Folder)) dir.create((Folder))
  
  Nhist <- Stock[[Istock]]$Nhist

  # Copy the SS fie
  File.from <- paste0(Path,"CodeBase/SS.exe")
  File.to <- paste0(Folder,"SS.exe")
  file.copy(File.from,File.to)
  
  # Start with the starter file
  Write_starter(Folder,Istock)
  
  # now save the data file
  Write_data(Folder,Istock,Iarea,Year)
  
  # now save the ctl file
  Write_ctl(Folder,Istock,Iarea,Year)
  
  # now save the forecast file
  Write_forecast(Folder,Istock,Year)
  #AAA
  #AAAA
  
  setwd(Folder)
  file.copy("ss3.par","ss3par.par",overwrite=T)
  cat("Running Stock Synthesis",Isim,Istock,Year,Stock[[Istock]]$Ass$SS_est_opt,Stock[[Istock]]$Ass$Clean.up,"\n")
  if (Stock[[Istock]]$Ass$SS_est_opt=="Full") vv <- system("SS",intern=T)
  if (Stock[[Istock]]$Ass$SS_est_opt=="First.Full.Only" & Year == Stock[[Istock]]$Nhist) vv <- system("SS",intern=T)
  if (Stock[[Istock]]$Ass$SS_est_opt=="First.Full.Only" & Year != Stock[[Istock]]$Nhist) vv <- system("SS -est",intern=T)
  if (Stock[[Istock]]$Ass$SS_est_opt=="EstOnly") vv <- system("SS -est",intern=T)
  if (file.exists("ss3par.par")) file.remove("ss3par.par",showWarnings=F)
  modeloutput <<- SS_output(dir=Folder,covar=F,printstats=FALSE,verbose=FALSE,warn=FALSE)
  ParsOut <- c(modeloutput$Growth_Parameters$M_age0[1],
               modeloutput$Growth_Parameters$L_a_A1[1],modeloutput$Growth_Parameters$L_a_A2[1],modeloutput$Growth_Parameters$K[1],
               modeloutput$Growth_Parameters$CVmin[1], modeloutput$Growth_Parameters$CVmax[1])
  Index <- which(modeloutput$parameters[,2]=="SR_LN(R0)"); Est <- modeloutput$parameters[Index,3]; ParsOut <- c(ParsOut,Est)
  ParsOut2 <- c(modeloutput$parameters[,3],0)

  #print(modeloutput$recruit$dev[1:(Nhist+General$Nproj+1)])
  Offset <- Stock[[Istock]]$Ass$SS_early_dev_yr1+1
  if (Stock[[Istock]]$Ass$SS_early_dev_yr1==0) Offset <- 0
  SSB0 <- modeloutput$derived_quants[1,2]
  Index1 <- which(modeloutput$derived_quants$Label == paste0("SSB_",modeloutput$startyr))
  N.out.years <- modeloutput$endyr-modeloutput$startyr+1+modeloutput$N_forecast_yrs
  if (N.out.years > Nhist+General$Nproj+1) N.out.years <- Nhist+General$Nproj+1
  Stock[[Istock]]$Ass$SSB.estimates[Isim,Year-Nhist+1,Iarea,1:N.out.years] <<- modeloutput$derived_quants$Value[Index1+1:N.out.years-1]
  Stock[[Istock]]$Ass$Depl.estimates[Isim,Year-Nhist+1,Iarea,1:N.out.years] <<- modeloutput$derived_quants$Value[Index1+1:N.out.years-1]/SSB0*100
  Index1 <- which(modeloutput$recruit$Yr == modeloutput$startyr)
  Stock[[Istock]]$Ass$Recr.estimates[Isim,Year-Nhist+1,Iarea,1:N.out.years] <<- modeloutput$recruit$dev[Index1+1:N.out.years-1]

  Stock[[Istock]]$Ass$Par.estimates[Isim,Year-Nhist+1,Iarea,1:length(ParsOut)] <<- ParsOut
  Stock[[Istock]]$Ass$M.estimates[Isim,Year-Nhist+1,Iarea,1:(Nhist+General$Nproj+1)] <<- modeloutput$Natural_Mortality$`0`[4:(3+Nhist+General$Nproj+1)]
  Stock[[Istock]]$Ass$AllPar.estimates[Isim,Year-Nhist+1,Iarea,1:length(ParsOut2)] <<- ParsOut2
  Stock[[Istock]]$Ass$MaxGrads[Isim,Year-Nhist+1,Iarea] <<- modeloutput$maximum_gradient_component
  
  Quants <- modeloutput$derived_quants
  Index <- which("ForeCatch_" == substr(rownames(Quants),1,10))
  #Index <- which("ForeCatchret_" == substr(rownames(Quants),1,13))
  TACs <- as.numeric(Quants[Index,2])
  Stock[[Istock]]$TheTACs <<- TACs
  #print(TACs)

  if (Stock[[Istock]]$Ass$Has.insurance) Do.Insurance(Isim,Istock,Year)

  # Set the TAC to zero if there was a MHW
  if (Stock[[Istock]]$Ass$Close.in.MHW == "Yes" & Stock[[Istock]]$Mcat.Index[Year+1]==1) Stock[[Istock]]$TheTACs[1] <- 0
  #print(Stock[[Istock]]$TheTACs)
  
  # Final output
  TACs <- Stock[[Istock]]$TheTACs
  #print(TACs)
  
  if (Estimation.test < 99) { setwd(Path); return(TACs) }
  
  # Tidy up
  file.remove("wtatage.ss_new")
  file.remove("derived_posteriors.sso")
  file.remove("Forecast-report.sso")
  file.remove("posterior_vectors.sso")
  file.remove("rebuild.sso")
  file.remove("SIS_table.sso")
  file.remove("ss_summary.sso")
  file.remove("fmin.log")
  file.remove("ss.log")
  file.remove("posterior_obj_func.sso")
  file.remove("posteriors.sso")
  file.remove("ParmTrace.sso")
  file.remove("runnumber.ss")
  file.remove("ss3.bar")
  if (file.exists("ss3.b01")) file.remove("ss3.b01",showWarnings=F)
  if (file.exists("ss3.b02")) file.remove("ss3.b02",showWarnings=F)
  if (file.exists("ss3.b03")) file.remove("ss3.b03",showWarnings=F)

  if (!Estimation.test %in% c(99) & (Stock[[Istock]]$Ass$Clean.up=="Default" || Stock[[Istock]]$Ass$Clean.up=="Full"))
   {
    file.remove("SS.exe")
    file.remove("echoinput.sso")
    file.remove("warning.sso")
    file.remove("compreport.sso")
    file.remove("cumreport.sso")
    file.remove("covar.sso")
    file.remove("ss3.par")
    file.remove("ss3.rep")
    if (file.exists("ss3.p01")) file.remove("ss3.p01",showWarnings=F)
    if (file.exists("ss3.r01")) file.remove("ss3.r01",showWarnings=F)
    if (file.exists("ss3.p02")) file.remove("ss3.p02",showWarnings=F)
    if (file.exists("ss3.r02")) file.remove("ss3.r02",showWarnings=F)
    if (file.exists("ss3.p03")) file.remove("ss3.p03",showWarnings=F)
    if (file.exists("ss3.r03")) file.remove("ss3.r03",showWarnings=F)
   }
  if (Stock[[Istock]]$Ass$Clean.up=="Full")
   {
    file.remove("Report.SSO")
    file.remove("starter.ss")
    file.remove("data.dat")
    file.remove("data.ctl")
    file.remove("forecast.ss")
   }
  setwd(Path)
  #print(Quants)
  return(TACs)
 }
# =======================================================================================================================

# ===============================================================================================================================

f2 <- function(parms,data) {
  getAll(data, parms, warn=FALSE)
  
  n <- length(C);

  # Extract the parameters
  k <- exp(logK);
  z <- exp(logz);
  q <- exp(logQ1);
  sigma <- exp(logSigma);
  SigmaR <- exp(logSigmaR);
  if (ProcessError==0) SigmaR = 0;

  # Is MSYL (BMSY/B0) calculated from z (1) or pre-specified (0)
  if (EstZ=="Yes") MSYLOut <- (1.0/(z+1.0))^(1.0/z) else MSYLOut <- MSYL;
  
  # Is R estimated (1) or computed from MSYINPUT (0)
  BMSY <- MSYLOut*k;
  if (EstR=="Yes") {  r <- exp(logR); } else {  r <- MSYINPUT / (BMSY*(1.0-MSYLOut^z));   }
  MSY <- r*BMSY*(1.0-MSYLOut^z);

  # End of years
  n1 <- n + 1;
  f <- 0

  # Declare variables
  B <- rep(0,n1);
  Dep <- rep(0,n1);
  Ihat <- matrix(0,nrow=n,ncol=Ncpue)
  Chat <- rep(0,n);
  ExpOut <-rep (0,n);
  B0<- rep(0,n1+50);

  # project model forward
  B[1] <- k;
  B0[1] <- k;
  for (t in 1:n)
  {
    Expl <- 1.0/(1.0+exp(-FF[t]));
    B[t+1] <- B[t] + r*B[t]*(1-(B[t]/k)^z) - Expl*B[t];
    B[t+1] <- B[t+1]*exp(Rec_dev[t]*SigmaR-SigmaR*SigmaR/2.0);
    if (IsDymB0==1)
    {
      B0[t+1] <- B0(t) + r*B0[t]*(1-(B0[t]/k)^z);
      B0[t+1] <- B0[t+1]*exp(Rec_dev[t]*SigmaR-SigmaR*SigmaR/2.0);
    }
    else
      B0[t+1] <- k;
    Chat[t] <- Expl*B[t];
    ExpOut[t] <- Expl;
    # Predicted CPUE
    for (Icpue in 1:Ncpue) Ihat[t,Icpue] <- q[Icpue]*(B[t]+B[t+1])/2.0;
  }
  Blast <- B[n+1]
  #print(B)

  f <- f - sum(dnorm(log(C), log(Chat), 0.05, log=T));
  
  # project model under B0
  for(t in (n+1):(n+50)) B0[t+1] <- B0[t] + r*B0[t]*(1-(B0[t]/k)^z);

  # Negative log-likelihood for the cpue indices
  for (Icpue in 1:Ncpue)
    for (t in 1:n)  
      if (I[t,Icpue] > 0)
        f <- f - dnorm(log(I[t,Icpue]), log(Ihat[t,Icpue]), sigma[Icpue], log=T);
  # Only compute a likelihood contribution when the index is positive
  #for (Ifleet in 1:Nfleet)
  # for (t in 1:n)
  #  if (I[t,Ifleet]>0)
  #    f <- f - dnorm(log(I[t,Ifleet]), log(Ihat1[t,Ifleet]), SigmaI[t,Ifleet]*SigmaIMult[Ifleet], log=T);
  
  # Can fit to mean biomass relative to MSYL
  BioAve = 0;
  for(t in (MSYRY1):(MSYRY2)) BioAve <- BioAve + B[t];
  BioAve <- BioAve / (MSYRY2-MSYRY1+1);

  # Penalty on that mean biomass matches MSYL
  CompPrior1 <- 1.0/(CVMSYL*CVMSYL)*(log(BioAve) - log(BMSY))*(log(BioAve) - log(BMSY));
  
  # Prior on r
  CompPrior2 <- dnorm(r,PriorMeanR,PriorSDr,log=T);

  # Prior on rec_devs
  CompPrior3 <- 0;
  for (t in 1:n) CompPrior3 <- CompPrior3 + dnorm(Rec_dev[t],0.0,1.0,log=T);

  # Calc dep and report
  Dep <- B/B[1];

  # Add penalties
  f <- f + CompPrior1;  f <- f - CompPrior2; f <- f - CompPrior3;

  # calculate depletion
  Depletion2 <-  Blast/k

  # Calculate the TAC - note the "trick" to get the OFL to be differentiable
  
  Ftargt1 <- r*(1-DT4.target^z)
  Ftargt2 <- r*(1-PGMSY.limit^z)

  ABC2 <- rep(0,50); FABC2 <- rep(0,50)
  for (t in (n+1):(n+50))
   {
    Depletion <- B[t]/k
    t1 <- 1.0/(1+exp(-50.0*(Depletion-alpha)))
    t2 <- 1.0/(1+exp( 50.0*(Depletion-beta)))
    tmult <- t1*t2
    t3 <- 1.0/(1+exp(-50.0*(Depletion-beta)))
    FABC2[t-n] <- Ftargt2
    ABC2[t-n] <- B[t]*FABC2[t-n]
    B[t+1] <- (B[t] + r*B[t]*(1-(B[t]/k)^z) - ABC2[t-n]);
  }

  ABC1 <- rep(0,50); FABC1 <- rep(0,50)
  for (t in (n+1):(n+50))
  {
    Depletion <- B[t]/k
    t1 <- 1.0/(1+exp(-50.0*(Depletion-alpha)))
    t2 <- 1.0/(1+exp( 50.0*(Depletion-beta)))
    tmult <- t1*t2
    t3 <- 1.0/(1+exp(-50.0*(Depletion-beta)))
    FABC1[t-n] <- Ftargt1*(tmult*(Depletion-alpha)/(beta-alpha)+t3)
    ABC1[t-n] <- B[t]*FABC1[t-n]
    B[t+1] <- (B[t] + r*B[t]*(1-(B[t]/k)^z) - ABC1[t-n]);
  }
  Depl <- B/k

  ADREPORT(r);
  ADREPORT(k);
  ADREPORT(z);
  ADREPORT(q);
  ADREPORT(MSY);
  ADREPORT(MSYLOut);
  ADREPORT(MSYLOut*k);
  ADREPORT(BioAve);
  ADREPORT(sigma);
  ADREPORT(B);          # uncertainty
  ADREPORT(Dep);        # stock status
  REPORT(f);            # plot
  REPORT(B);            # plot
  REPORT(B0);           # plot
  REPORT(Chat);         # plot
  REPORT(ExpOut);       # plot
  REPORT(Ihat);         # plot

  # Output stuff for TACs
  REPORT(r);
  REPORT(k);
  REPORT(z);
  REPORT(Blast)
  REPORT(Depl)
  REPORT(tmult)
  REPORT(t3)
  REPORT((Depletion2-alpha)/(beta-alpha))
  REPORT(ABC1)
  REPORT(FABC1)
  REPORT(ABC2)
  REPORT(FABC2)
  REPORT(Depletion)
  
  ADREPORT(log(Blast))
  ADREPORT(Depletion)
  ADREPORT(FABC1)
  ADREPORT(ABC1)

  return(f);
}

# Allow for explicit data argument
cmb <- function(f, d) function(p) f(p, d)

# ===============================================================================================================================

Run_Tier4a<- function(Isim,Istock,Year,Plot=T)
{
  
  FindZ <- function(z,msyl)
   {
    val <- 1-(z+1)*msyl^z
    return(val)
   }  
  
  cat("Running Dynamic Tier 4a:",Isim,Istock,Year,Stock[[Istock]]$Ass$Tier4a$alpha,Stock[[Istock]]$Ass$Tier4a$beta,"\n")
  Catch <- rep(0,nrow=Year)
  for (Iyear in 1:Year)  
   Catch[Iyear] <- sum(Stock[[Istock]]$Data$Catches[,,Iyear])

  FleetCnt <- 0
  IndexVl <- matrix(-1,nrow=Year,ncol=1000)
  IndexCV <- matrix(-1,nrow=Year,ncol=1000)
  for (Ifleet in 1:General$Nfleet)
   for (Iarea in 1:Stock[[Istock]]$NassArea)
    {
    Index <- which(Stock[[Istock]]$Data$IndexData.Used[Iarea,,3]==Ifleet)
    if (length(Index)>0)
     {
      FleetCnt <- FleetCnt + 1
      for (Index2 in 1:length(Index))
       {
        Index3 <- Index[Index2]
        TheYear <- Stock[[Istock]]$Data$IndexData.Used[Iarea,Index3,1]-Stock[[Istock]]$YrOffset
        Val <- Stock[[Istock]]$Data$IndexData.Used[Iarea,Index3,4]
        CV <- Stock[[Istock]]$Data$IndexData.Used[Iarea,Index3,5]
        IndexVl[TheYear,FleetCnt]<- Val 
        IndexCV[TheYear,FleetCnt]<- CV 
       }
    } # Length(Index) > 0
   }

  # Compute median catch as a proxy for MSY
  Yr1 <- Stock[[Istock]]$Ass$Tier4a$MSYPriorY1-Stock[[Istock]]$YrOffset
  Yr2 <- Stock[[Istock]]$Ass$Tier4a$MSYPriorY2-Stock[[Istock]]$YrOffset
  MSYINPUT= median(Catch[Yr1:Yr2])

  # find the  z corresponding to BMSY/B0
  fitz <- uniroot(FindZ,lower=0.0001,upper=10,msyl=Stock[[Istock]]$Ass$Tier4a$MSYL)
  z <- fitz$root
  MSYL <- (1.0/(z+1.0))^(1.0/z)
  
  ProcessError=Stock[[Istock]]$Ass$Tier4a$ProcessError
  EstR=Stock[[Istock]]$Ass$Tier4a$EstR
  EstZ=Stock[[Istock]]$Ass$Tier4a$EstZ
  MSYRY1=Stock[[Istock]]$Ass$Tier4a$MSYRY1-Stock[[Istock]]$YrOffset
  MSYRY2=Stock[[Istock]]$Ass$Tier4a$MSYRY2-Stock[[Istock]]$YrOffset
  alpha=Stock[[Istock]]$Ass$Tier4a$alpha
  beta=Stock[[Istock]]$Ass$Tier4a$beta
  DT4.target <- Stock[[Istock]]$Ass$Tier4a$DT4.target
  if (is.null(General$PGMSY.limit)) PGMSY.limit <- 0.25 else PGMSY.limit <- General$PGMSY.limit
  
  data <- list(C=Catch,I=IndexVl,Nyear=Year,
               MSYL=MSYL,EstR=EstR,EstZ=EstZ,
               MSYINPUT=MSYINPUT,MSYRY1=MSYRY1,MSYRY2=MSYRY2,
               Ncpue=FleetCnt,CVMSYL=Stock[[Istock]]$Ass$Tier4a$CVMSYL,MSYPriorY1=Yr1,MSYPriorY2=Yr2,
               alpha=alpha,beta=beta, DT4.target=DT4.target,PGMSY.limit=PGMSY.limit,
               PriorMeanR=Stock[[Istock]]$Ass$Tier4a$PriorMeanR,PriorSDr=Stock[[Istock]]$Ass$Tier4a$PriorSDr,
               ProcessError=ProcessError,IsDymB0=0)
  #print(str(data))

  # Fit the observation error estimator 
  map <- list(Eps=rep(factor(NA),Year),logSigmaR=factor(NA))
  TrueK <- sum(Stock[[Istock]]$SSB0)*Stock[[Istock]]$Ass$Tier4a$K_init_mult
  parameters <- list(logR=Stock[[Istock]]$Ass$Tier4a$logr_init, logK=log(TrueK),logQ1=rep(log(1),FleetCnt), 
                     logSigma=rep(-2.3,FleetCnt),
                     FF=rep(-2,Year),logz=log(z),logSigmaR=log(0.1),Rec_dev=rep(0,Year))
  #print(str(parameters))
  
  if (EstR=="No"  & EstZ=="No" ) map <- list(logz=factor(NA),logR=factor(NA))
  if (EstR=="Yes" & EstZ=="No" ) map <- list(logz=factor(NA))
  if (EstR=="No"  & EstZ=="Yes") map <- list(logr=factor(NA))
  if (EstR=="Yes" & EstZ=="Yes") map <- NULL
  if (ProcessError==0) map <- c(map,list(logSigmaR=factor(NA),Rec_dev=rep(factor(NA),length(parameters$Rec_dev))))
  if (ProcessError==1) map <- c(map,list(logSigmaR=factor(NA)))
  #print(map)
  #print(str(parameters))
  
  ################################################################################
  
  # fit the model
  if (ProcessError %in% c(0,1))
    model <- MakeADFun(cmb(f2,data), parameters,map=map,control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)
  if (ProcessError %in% c(2))
    model <- MakeADFun(cmb(f2,data), parameters,map=map,random=c("Rec_dev"),control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)
  fit <- nlminb(model$par, model$fn, model$gr)
  if (ProcessError != 2)
    for (i in 1:8) 
    {
      BestP <- model$env$last.par.best
      if (ProcessError==2) 
      { 
        Index <- which(names(BestP)=="Rec_dev"); 
        BestP <- BestP[-Index]; 
      }
      fit <- nlminb(BestP, model$fn, model$gr)
    }

  rep <- sdreport(model)
  rep2 <- summary(rep)
  #print(rep2)
  Report <- model$report(model$env$last.par.best)
  #print(str(Report))
 
  Nhist <- Stock[[Istock]]$Nhist
  Stock[[Istock]]$Ass$SSB.estimates[Isim,Year-Nhist+1,1,1:Year] <<- Report$B[1:Year]
  Stock[[Istock]]$Ass$Depl.estimates[Isim,Year-Nhist+1,1,1:Year] <<- Report$B[1:Year]/Report$k*100
  
  # Do we need to plot the fits
  Plot <- F
  if (Plot==T)
  {
    par(mfrow=c(2,2))
    for (Ifleet in 1:FleetCnt)
     {
      plot(1:Year,IndexVl[,Ifleet],xlab="Year",ylab="Index",pch=16)
      lines(1:Year,Report$Ihat[,Ifleet],lwd=2,col="red")
    }
    plot(1:(Year+51),Report$Depl,xlab="Year",ylab="Depletion",pch=16,type="l",ylim=c(0,1.2))
    abline(h=0.5,lwd=2,col="green")
    lines(1:(Year+51),Report$Depl,lwd=3)
    plot(1:Year,Catch,xlab="Year",ylab="Index",pch=16)
    lines(1:Year,Report$Chat,lwd=2)
    #AA
  }
  
  # Compute the TAC from the ABC (which one we reports depend on whether we have a key commericial species or not)
  if (General$PGMSY.Primary.Species[Istock] == 0) ABC <- Report$ABC1
  if (General$PGMSY.Primary.Species[Istock]!= 0) ABC <- Report$ABC2
  ABC[which(ABC<0)] <- 0
  ABC[which(is.nan(ABC))] <- 0
  RBC <- ABC 
  #AA
  
  #cat(Isim,Nyear,Pstar,round(Report$r,3),round(Report$k),round(Report$Blast),round(Depletion,3),round(OFL,2),round(FOFL,3),round(SDlog,3),round(TAC,2),"\n")
  return(RBC)
}

# ===============================================================================================================================

Run_Tier4b<- function(Isim,Istock,Year)
 {

  cat("Running Dynamic Tier 4b:",Isim,Istock,Year,"\n")
  
  # Target CPUE
  Yr1 <- Stock[[Istock]]$Ass$Tier4b$Cpue_Yr1; Yr2 <- Stock[[Istock]]$Ass$Tier4b$Cpue_Yr2; 
  Cpue.area <-Stock[[Istock]]$Ass$Tier4b$Cpue_Area
  Cpue.Index <- Stock[[Istock]]$Ass$Tier4b$Cpue_Index
  Index <- which(Stock[[Istock]]$Data$IndexData.Used[Cpue.area,,3]==Cpue.Index)
  Cpue1 <- Stock[[Istock]]$Data$IndexData.Used[Cpue.area,Index,]
  Index <- which(Cpue1[,1] %in% Yr1:Yr2)
  Cpue1 <- Cpue1[Index,]
  Cpue.target <- Stock[[Istock]]$Ass$Tier4b$Cpue_target_mult * mean(Cpue1[,4])
   
  # Recent CpUE
  Cpue.Yrs <- Stock[[Istock]]$Ass$Tier4b$Cpue_avg_yr-1
  Cpue.area <-Stock[[Istock]]$Ass$Tier4b$Cpue_Area
  Cpue.Index <- Stock[[Istock]]$Ass$Tier4b$Cpue_Index
  Index <- which(Stock[[Istock]]$Data$IndexData.Used[Cpue.area,,3]==Cpue.Index)
  Cpue1 <- Stock[[Istock]]$Data$IndexData.Used[Cpue.area,Index,]
  Yrs <- Stock[[Istock]]$YrOffset+Year+c(-1*Cpue.Yrs:0)
  Index <- which(Cpue1[,1] %in% Yrs)
  Cpue1 <- Cpue1[Index,]
  #print(Cpue1)
  CpueAvg <-  mean(Cpue1[,4])

  # Catch for the last year
  current_catch <- sum(Stock[[Istock]]$CatchTotal[,,Year])
  
  # HCR parameters
  Cpue.limit <- Stock[[Istock]]$Ass$Tier4b$Cpue_limit_mult * Cpue.target
  Cpue.HCR.target <- Stock[[Istock]]$Ass$Tier4b$HCR_target
  Max.catch <- Stock[[Istock]]$Ass$Tier4b$Max.catch
  
  # Calculate the RBC
  if (CpueAvg  <= Stock[[Istock]]$Ass$Tier4b$Cpue_lim)
    mult <- 0.0
  else
    mult <- (CpueAvg-Cpue.limit)/(Cpue.target-Cpue.limit);
  RBC <- current_catch * mult * Cpue.HCR.target/0.48;
  print(c(Istock,Year,current_catch,CpueAvg,Cpue.limit,Cpue.target,Cpue.HCR.target,Max.catch,mult,RBC))
  
  # Check if above maximum catch and then store
  if (RBC > Max.catch) RBC <- Max.catch
  RBC <- rep(RBC,50)
  return(RBC)
 }  
# =======================================================================================================================

Run_Tier5<- function(Isim,Istock,Year,Plot=T)
{
  Option <- Stock[[Istock]]$Ass$Tier5.Option
  #cat("Running Tier 4",Isim,Istock,Year,Option,"\n")
  
  Narea <- General$Narea
  Ncat_fleet <- General$Ncat_fleet
  
  Fout <- matrix(0,nrow=Narea,ncol=Ncat_fleet)

  # Find the mean F (and normalize it)
  MeanF <- array(0,dim=c(Narea,Ncat_fleet)) 
  for (Iarea in 1:Narea)
    for (Ifleet in 1:Ncat_fleet)
    {
      Icnt <- 0
      for (Iyear in Stock[[Istock]]$Ass$Tier5.Years[1]:Stock[[Istock]]$Ass$Tier5.Years[2]-Stock[[Istock]]$YrOffset)  
      { MeanF[Iarea,Ifleet] <- MeanF[Iarea,Ifleet] + Stock[[Istock]]$FullF[Iarea,Ifleet,Iyear]; Icnt <- Icnt + 1 }
      MeanF[Iarea,Ifleet] <- MeanF[Iarea,Ifleet]  / Icnt
    }
  
    
  # Catch is zero
  if (Option==0) TAC <- Fout
  
  # mean F
  if (Option==1) Fout <- MeanF

  # ABC+HCR 1: Tier 3 HCR 
  if (Option==21)
   {
    # Mean F
    MeanF <- MeanF/sum(MeanF)
    
    # Find depletion
    YearProj <- Year+1
    SSB_Current <- sum(Stock[[Istock]]$SSB[,YearProj])
    B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,YearProj]/Stock[[Istock]]$R00)
    Depl <- SSB_Current/B0
    
    BMSY_prox <- 0.4; FMSY_prox <- 0.4
    Alpha <- 0.05*BMSY_prox
    Close.Thresh <- 0.2

    # Apply the HCR to get the Fmultipler
    if (Depl < Close.Thresh)
      Fmult <- 0
    else
      if (Depl > BMSY_prox)  
        Fmult <- 1
    else
      Fmult <- (Depl-Alpha)/(BMSY_prox-Alpha) 
    if (FullOutput==T) cat(YearProj,SSB_Current/B0,Fmult,"\n")
    Outs <- GetRefs(Istock,MeanF,YearProj,FMSY_prox)
    # Set the SPR
    Fout <- MeanF*Fmult*Outs$F40_SPR
  }

  # ABC+HCR 2: Lagged recovery to estimate emergency relief financing needs
  if (Option==22)
  {
    # Mean F
    MeanF <- MeanF/sum(MeanF)
    
    # Find depletion
    YearProj <- Year+1
    SSB_Current <- sum(Stock[[Istock]]$SSB[,YearProj])
    B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,YearProj]/Stock[[Istock]]$R00)
    Depl <- SSB_Current/B0
    
    BMSY_prox <- 0.4; FMSY_prox <- 0.4
    Alpha <- 0.05*BMSY_prox
    Close.Thresh <- 0.25
    if (Depl < Close.Thresh) Stock[[Istock]]$Ass$Tier5.Rebuilding <<- T
    if (Depl > BMSY_prox) Stock[[Istock]]$Ass$Tier5.Rebuilding <<- F
    if (Stock[[Istock]]$Ass$Tier5.Rebuilding == F) Alpha <- 0.3*BMSY_prox
    
    # Apply the HCR to get the Fmultipler
    if (Depl < Close.Thresh)
      Fmult <- 0
    else
      if (Depl > BMSY_prox)  
        Fmult <- 1
    else
      Fmult <- (Depl-Alpha)/(BMSY_prox-Alpha) 
    if (FullOutput==T) cat(YearProj,SSB_Current/B0,Fmult,"\n")
    Outs <- GetRefs(Istock,MeanF,YearProj,FMSY_pro)
    # Set the SPR
    Fout <- MeanF*Fmult*Outs$F40_SPR
  }

  # ABC+HCR 3: Long-term resilience (stronger reserve) B_target 
  if (Option==23)
  {
    # Mean F
    MeanF <- MeanF/sum(MeanF)
    
    # Find depletion
    YearProj <- Year+1
    SSB_Current <- sum(Stock[[Istock]]$SSB[,YearProj])
    B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,YearProj]/Stock[[Istock]]$R00)
    Depl <- SSB_Current/B0
    
    BMSY_prox <- 0.5; FMSY_prox <- 0.4
    Alpha <- 0.05*BMSY_prox
    Close.Thresh <- 0.2
    
    # Apply the HCR to get the Fmultipler
    if (Depl < Close.Thresh)
      Fmult <- 0
    else
      if (Depl > BMSY_prox)  
        Fmult <- 1
    else
      Fmult <- (Depl-Alpha)/(BMSY_prox-Alpha) 
    if (FullOutput==T) cat(YearProj,SSB_Current/B0,Fmult,"\n")
    Outs <- GetRefs(Istock,MeanF,YearProj,FMSY_pro)
    # Set the SPR
    Fout <- MeanF*Fmult*Outs$F40_SPR
  }
  
  # ABC+HCR 4: Environmental index informed sloping rate
  if (Option==24)
   {
    print("Not available")
    AA
   }
  
  # ABC+HCR 5: Maximize productivity/ increased reserve 
  if (Option==25)
  {
    print("Not available")
    # Mean F
    MeanF <- MeanF/sum(MeanF)
    
    # Find depletion
    YearProj <- Year+1
    SSB_Current <- sum(Stock[[Istock]]$SSB[,YearProj])
    B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,YearProj]/Stock[[Istock]]$R00)
    Depl <- SSB_Current/B0
    
    BMSY_prox <- 0.5; FMSY_prox <- 0.4
    Alpha <- 0.05*BMSY_prox
    Close.Thresh <- 0.2
    Gamma <- 0.1
    Depl.BMSY <- SSB_Current/(B0*BMSY_prox)
    
    # Apply the HCR to get the Fmultipler
    if (Depl < Close.Thresh)
      Fmult <- 0
    else
      if (Depl.BMSY > 1)  
        Fmult <- 1*exp(-Gamma*(Depl-1))
    else
      Fmult <- (Depl-Alpha)/(BMSY_prox-Alpha) 
    if (FullOutput==T) cat(YearProj,SSB_Current/B0,Fmult,"\n")
    Outs <- GetRefs(Istock,MeanF,YearProj,FMSY_pro)
    # Set the SPR
    Fout <- MeanF*Fmult*Outs$F40_SPR
   }
  
  # ABC+HCR 6: Combination of MHW (HCR4) + Maximize productivity (HCR5)
  if (Option==26)
  {
    print("Not available")
    AA
   }
  # ABC+HCR 7: Risk Table Bridging: R/S variability covariate adjusted HCR
  if (Option==27)
   {
    # Mean F
    MeanF <- MeanF/sum(MeanF)
    
    # Find depletion
    YearProj <- Year+1
    SSB_Current <- sum(Stock[[Istock]]$SSB[,YearProj])
    B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,YearProj]/Stock[[Istock]]$R00)
    Depl <- SSB_Current/B0
    omega1 <- 0; omega2 <- 0
    
    BMSY_prox <- 0.4*exp(-1*omega2*cov); FMSY_prox <- 0.4
    Alpha <- 0.05*BMSY_prox
    Close.Thresh <- 0.2
    
    # Apply the HCR to get the Fmultipler
    if (Depl < Close.Thresh)
      Fmult <- 0
    else
      if (Depl > BMSY_prox)  
        Fmult <- 1
    else
      Fmult <- (Depl-Alpha)/(BMSY_prox-Alpha) 
    Fmult <- Fmult * exp(omega1*cov)
    if (FullOutput==T) cat(YearProj,SSB_Current/B0,Fmult,"\n")
    Outs <- GetRefs(Istock,MeanF,YearProj,FMSY_prox)
    # Set the SPR
    Fout <- MeanF*Fmult*Outs$F40_SPR
   }
  # ABC+HCR 8: Adjust effective spawning biomass (simulate adjusted B_target)
  if (Option==28)
   {
    Theta <- 1; Theta <- 0.8
    
    # Mean F
    MeanF <- MeanF/sum(MeanF)
    
    # Find depletion
    YearProj <- Year+1
    SSB_Current <- sum(Stock[[Istock]]$SSB[,YearProj])
    B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,YearProj]/Stock[[Istock]]$R00)
    Depl <- Theta*SSB_Current/B0
    
    BMSY_prox <- 0.4; FMSY_prox <- 0.4
    Alpha <- 0.05*BMSY_prox
    Close.Thresh <- 0.2
    
    # Apply the HCR to get the Fmultipler
    if (Depl < Close.Thresh)
      Fmult <- 0
    else
      if (Depl > BMSY_prox)  
        Fmult <- 1
    else
      Fmult <- (Depl-Alpha)/(BMSY_prox-Alpha) 
    if (FullOutput==T) cat(YearProj,SSB_Current/B0,Fmult,"\n")
    Outs <- GetRefs(Istock,MeanF,YearProj,FMSY_prox)
    # Set the SPR
    Fout <- MeanF*Fmult*Outs$F40_SPR
  }
  # ABC+HCR 9: Forecast informed version of HCR 5
  if (Option==29)
  {
    # Mean F
    MeanF <- MeanF/sum(MeanF)
    
    # Find depletion
    YearProj <- Year+1
    SSB_Current <- sum(Stock[[Istock]]$SSB[,YearProj])
    B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,YearProj]/Stock[[Istock]]$R00)
    Depl <- SSB_Current/B0
    
    BMSY_prox <- 0.4; FMSY_prox <- 0.4
    Alpha <- 0.05*BMSY_prox
    Close.Thresh <- 0.25
    Depl.BMSY = SSB_Current/(BMSY_prox*B0)
    Gamma <- 2.6
    
    # Apply the HCR to get the Fmultipler
    if (Depl < Close.Thresh)
      Fmult <- 0
    else
      if (Depl.BMSY > 1)  
        Fmult <- exp(-Gamma*(inv.logit(-1*cov)/0.2))*(Depl.BMSY-1)
    else
      Fmult <- (Depl-Alpha)/(BMSY_prox-Alpha) 
    if (FullOutput==T) cat(YearProj,SSB_Current/B0,Fmult,"\n")
    Outs <- GetRefs(Istock,MeanF,YearProj,FMSY_pro)
    # Set the SPR
    Fout <- MeanF*Fmult*Outs$F40_SPR
   }
  # ABC+HCR 10: Maximize productivity/increased reserve (HCR5), linear version (1/ B_target) with offset
  if (Option==210)
   {
    # Mean F
    MeanF <- MeanF/sum(MeanF)
    
    # Find depletion
    YearProj <- Year+1
    SSB_Current <- sum(Stock[[Istock]]$SSB[,YearProj])
    B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,YearProj]/Stock[[Istock]]$R00)
    Depl <- SSB_Current/B0
    
    BMSY_prox <- 0.4; FMSY_prox <- 0.4
    Alpha <- 0.05*BMSY_prox
    Close.Thresh <- 0.2
    Gamma <- 0.5; Gamma <- 1.5; Gamma <- 3
    
    # Apply the HCR to get the Fmultipler
    if (Depl < Close.Thresh)
      Fmult <- 0
    else
      if (Depl > BMSY_prox)  
        {
         Fmult <- 1
         if (Depl > (1+Gamma)) Fmult <- 1/(Depl/(1+Gamma))
        }
    else
      Fmult <- (Depl-Alpha)/(BMSY_prox-Alpha) 
    if (FullOutput==T) cat(10,YearProj,SSB_Current/B0,Fmult,"\n")
    Outs <- GetRefs(Istock,MeanF,YearProj,FMSY_prox)
    # Set the SPR
    Fout <- MeanF*Fmult*Outs$F40_SPR
   }
  
  
  # ===============================
  Stock[[Istock]]$Ass$F.fix <<- Fout

  return()
}


# =======================================================================================================================

Run_Tier6a<- function(Isim,Istock,Year,Plot=T)
{
 if (Istock==1) 
  {
   Survey.est <- Stock[[Istock]]$Data$   
   TACs <- rep(2000,50)
   Index <- which(Stock[[1]]$Data$IndexData[1,,2]==Stock[[1]]$YrOffset+Year-1)
   Est <- Stock[[1]]$Data$IndexData[1,Index,4]
   if (Est < 150000)
     OFL <- 0
   else
    OFL <- max(0,(Est-15000)*0.18)
   ABC <- OFL * 0.89191
   TACs <- rep(ABC,50)
   
  }
 if (Istock==2) TACs <- rep(200,50)
 if (Istock==3) TACs <- rep(20000,50)
 if (Istock==4) TACs <- rep(1000,50)
 return(TACs)
}

# =======================================================================================================================
Run_Assessment <- function(Isim,Istock,Iarea,Year)
 {
  # Remove data that are not available
  # Index data
  Stock[[Istock]]$Data$IndexData.Used <<- Stock[[Istock]]$Data$IndexData
  if (sum(Stock[[Istock]]$Data$Is_index=="Yes")>0)
   {
    Index <- which(Stock[[Istock]]$Data$IndexData[Iarea,,1] > Year+Stock[[Istock]]$YrOffset-General$Delay.Index)
    if (length(Index)>0)
     Stock[[Istock]]$Data$IndexData.Used[Iarea,Index,1] <<- -1*Stock[[Istock]]$Data$IndexData.Used[Iarea,Index,1]
   }
  # Discard data
  Stock[[Istock]]$Data$DiscardData.Used <<- Stock[[Istock]]$Data$DiscardData
  if (sum(Stock[[Istock]]$Data$Is_discard=="Yes")>0)
   {
    Index <- which(Stock[[Istock]]$Data$DiscardData[,1] > Year+Stock[[Istock]]$YrOffset-General$Delay.Discard)
    if (length(Index)>0) Stock[[Istock]]$Data$DiscardData.Used[Index,1] <<- -1*Stock[[Istock]]$Data$DiscardData.Used[Index,1]
   }
  # Length data
  Stock[[Istock]]$Data$LengthData.Used <<- Stock[[Istock]]$Data$LengthData
  if (sum(Stock[[Istock]]$Data$Is_length=="Yes")>0)
   {
    Index <- which(Stock[[Istock]]$Data$LengthData.Used[,2] > Year+Stock[[Istock]]$YrOffset-General$Delay.M.Length)
    if (length(Index)>0)Stock[[Istock]]$Data$LengthData.Used[Index,2] <<- -1*Stock[[Istock]]$Data$LengthData.Used[Index,2]
   }
  # Age data
  Stock[[Istock]]$Data$AgeData.Used <<- Stock[[Istock]]$Data$AgeData
  if (sum(Stock[[Istock]]$Data$Is_age=="Yes")>0)
   {
    Index <- which(Stock[[Istock]]$Data$AgeData.Used[,2] > Year+Stock[[Istock]]$YrOffset-General$Delay.M.Age)
    if (length(Index)>0)Stock[[Istock]]$Data$AgeData.Used[Index,2] <<- -1*Stock[[Istock]]$Data$AgeData.Used[Index,2]
   }
  # CAA data
  Stock[[Istock]]$Data$CAAData.Used <<- Stock[[Istock]]$Data$CAAData
  if (sum(Stock[[Istock]]$Data$Is_CAA=="Yes")>0)
   {
    Index <- which(Stock[[Istock]]$Data$CAAData.Used[,2] > Year+Stock[[Istock]]$YrOffset-General$Delay.CAA)
    if (length(Index)>0)Stock[[Istock]]$Data$CAAData.Used[Index,2] <<- -1*Stock[[Istock]]$Data$CAAData.Used[Index,2]
   }

  if (Stock[[Istock]]$Ass$Type=="Tier_1a") TACs <- Run_Tier1a(Isim,Istock,Iarea,Year)
  if (Stock[[Istock]]$Ass$Type=="Tier_4a") TACs <- Run_Tier4a(Isim,Istock,Year)
  if (Stock[[Istock]]$Ass$Type=="Tier_4b") TACs <- Run_Tier4b(Isim,Istock,Year)
  
  # Tier 0: Pre-specified catch
  if (Stock[[Istock]]$Ass$Type=="Tier_0")  
   {
    TACs <- rep(Stock[[Istock]]$Ass$Prespecified.TAC,50)
  }
  
  # Perfect information tier
  if (Stock[[Istock]]$Ass$Type=="Tier_5")  
   {
    Run_Tier5(Isim,Istock,Year)
    TACs <- rep(-1,50)
   }
  
  # Link for Caitlin's multispecies HCR
  if (Stock[[Istock]]$Ass$Type=="Tier_6a")  
   {
    TACs <- Run_Tier6a(Isim,Istock,Year)
   }

  # Placeholder for WA control rules
  if (Stock[[Istock]]$Ass$Type=="Tier_7")  
   {
    TACs <- rep(0,50)
   }
  
  #print(TACs)
  return(TACs)
}

