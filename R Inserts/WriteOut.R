# ===================================================================================================================================

WriteBiol <- function(Istock,FileName,FolderResults)
 {
  MaxAge <- Stock[[Istock]]$MaxAge
  Nsex   <- Stock[[Istock]]$Nsex
  Nhist   <- Stock[[Istock]]$Nhist
  Nlen   <- Stock[[Istock]]$Nlen
  YrOffset <- Stock[[Istock]]$YrOffset
  
  ObjSave <- NULL
  
  # Brett C - OPEN THE FILE ONCE
  fc <- file(FileName, open = "a")
  
  write("\n#Environmental variables",file=FileName,append=T)
  for (Year in 1:(Nhist+General$Nproj+1))
   if (Stock[[Istock]]$N.env.ind >0)  
    {
     Out <- Year+YrOffset; for (Ind in 1:Stock[[Istock]]$N.env.ind) Out <- paste0(Out, " ",Stock[[Istock]]$EnvData[Year,Ind])     
      write(Out,file=FileName,append=T)
    }   
    
  write("\n#Natural mortality",file=FileName,append=T)
  write(c("#_Stock Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Year in 1:(Nhist+General$Nproj))
    for (Isex in 1:Nsex)
      write(c(Istock,Isex,Year+YrOffset,Stock[[Istock]]$M[Isex,,Year]),ncol=MaxAge+3,file=FileName,append=T)
  
  if (Stock[[Istock]]$Num.Pred > 0)
   {
    write("\n#Natural mortality (M0)",file=FileName,append=T)
    write(c("#_Stock Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
    for (Year in 1:(Nhist+General$Nproj))
     for (Isex in 1:Nsex)
       write(c(Istock,Isex,Year+YrOffset,Stock[[Istock]]$M0[Isex,,Year]),ncol=MaxAge+3,file=FileName,append=T)
    write("\n#Natural mortality (M2)",file=FileName,append=T)
    write(c("#_Stock Pred Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
    for (Ipred in 1:Stock[[Istock]]$Num.Pred)
     for (Year in 1:(Nhist+General$Nproj))
      for (Isex in 1:Nsex)
        write(c(Istock,Ipred,Isex,Year+YrOffset,Stock[[Istock]]$M2[Ipred,Isex,,Year]),ncol=MaxAge+4,file=FileName,append=T)
  }
 
  write("\n#Time-varying parameters",file=FileName,append=T)
  # for (Sex in 1:Nsex)
  for (Isex in 1:Nsex) # BrettC - Sex defined as wrong thing
  for (Year in 1:(Nhist+General$Nproj))
    write(c(Isex,Year+YrOffset,
            format(round(Stock[[Istock]]$LenA1TV[Year,Isex],6)),format(round(Stock[[Istock]]$LenA2TV[Year,Isex],6)),
            format(round(Stock[[Istock]]$KappaTV[Year,Isex],6)),format(round(Stock[[Istock]]$LinfTV[Year,Isex],6))),ncol=6,file=FileName,append=T)
  
  
  write("\n#Mean_Length_At_Age",file=FileName,append=T)
  write(c("#_Stock Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Isex in 1:Nsex)
    for (Year in 1:(Nhist+General$Nproj))
     {
      write(c(Isex,Year+YrOffset,Istock,1,Stock[[Istock]]$MeanLenAgeS[Isex,,Year]),ncol=MaxAge+4,file=FileName,append=T)
      write(c(Isex,Year+YrOffset,Istock,2,Stock[[Istock]]$MeanLenAgeM[Isex,,Year]),ncol=MaxAge+4,file=FileName,append=T)
     }

  write("\n#Mean_Length_At_Age_Start_Year",file=FileName,append=T)
  write(c("#_Stock Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Isex in 1:Nsex)
   for (Year in 1:(Nhist+General$Nproj))
    write(c(Istock,Isex,Year+YrOffset,Stock[[Istock]]$MeanLenAgeS[Isex,,Year]),ncol=MaxAge+3,file=FileName,append=T)
  write("\n#Mean_Length_At_Age_Mid_Year",file=FileName,append=T)
  write(c("#_Stock Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Isex in 1:Nsex)
   for (Year in 1:(Nhist+General$Nproj))
    write(c(Istock,Isex,Year+YrOffset,Stock[[Istock]]$MeanLenAgeM[Isex,,Year]),ncol=MaxAge+3,file=FileName,append=T)
 
    write("\n#SD_Length_At_Age_Start_Year",file=FileName,append=T)
  write(c("#_Stock Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Year in 1:2)
    for (Isex in 1:Nsex)
      write(c(Istock,Isex,Year+YrOffset,Stock[[Istock]]$SigmaLenAgeS[Isex,,Year]),ncol=MaxAge+3,file=FileName,append=T)
  write("\n#SD_Length_At_Age_Mid_Year",file=FileName,append=T)
  write(c("#_Stock Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Year in 1:2)
    for (Isex in 1:Nsex)
      write(c(Istock,Isex,Year+YrOffset,Stock[[Istock]]$SigmaLenAgeM[Isex,,Year]),ncol=MaxAge+3,file=FileName,append=T)
  
  write("\n#_ALK_Start_Year (yr1)",file=FileName,append=T)
  write(c("#_Stock Sex Length ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+2,file=FileName,append=T)
  for (Isex in 1:Nsex)
   for (Ilen in 1:Nlen)  
     write(c(Istock,Isex,format(Ilen,width=2),format(round(Stock[[Istock]]$FracLenS[Isex,,1,Ilen],6),6)),ncol=MaxAge+3,file=FileName,append=T)
  write("\n#_ALK_Mid_Year (yr1)",file=FileName,append=T)
  write(c("#_Stock Sex Length ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+2,file=FileName,append=T)
  for (Isex in 1:Nsex)
    for (Ilen in 1:Nlen)  
      write(c(Istock,Isex,format(Ilen,width=2),format(round(Stock[[Istock]]$FracLenM[Isex,,1,Ilen],6),6)),ncol=MaxAge+3,file=FileName,append=T)
  
  write("\n_Mean_Weight_At_Age_Start_Year",file=FileName,append=T)
  write(c("#_Stock Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Year in 1:Stock[[Istock]]$Nhist)
    for (Isex in 1:Nsex)
      write(c(Istock,Isex,Year+YrOffset,format(round(Stock[[Istock]]$MeanWtAtAgeS[Isex,,Year],6),width=6)),ncol=MaxAge+3,file=FileName,append=T)
  write("\n_Mean_Weight_At_Age_Mid_Year",file=FileName,append=T)
  write(c("#_Stock Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Year in 1:Stock[[Istock]]$Nhist)
    for (Isex in 1:Nsex)
      write(c(Istock,Isex,Year+YrOffset,format(round(Stock[[Istock]]$MeanWtAtAgeM[Isex,,Year],6),width=6)),ncol=MaxAge+3,file=FileName,append=T)
  write("\n_Fecundity",file=FileName,append=T)
  write(c("#_Stock Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Year in 1:Stock[[Istock]]$Nhist)
    for (Isex in 1:Nsex)
      write(c(Istock,Year+YrOffset,format(round(Stock[[Istock]]$Fecundity[,Year],6),width=6)),ncol=MaxAge+3,file=FileName,append=T)
  
  write("\n_Length-specific selectivity",file=FileName,append=T)
  write(c("#_Fleet Sex Year ",paste0(seq(from=1,to=Nlen))),ncol=Nlen+3,file=FileName,append=T)
  for (Ifleet in 1:General$Nfleet)
    for (Year in 1:(Stock[[Istock]]$Nhist+General$Nproj))
      for (Isex in 1:Nsex)
        write(c(Ifleet,Isex,Year+YrOffset,format(round(Stock[[Istock]]$SelSelLen[Isex,Ifleet,,Year],6),width=6)),ncol=Nlen+3,file=FileName,append=T)
  write("\n_Length-specific retention",file=FileName,append=T)
  write(c("#_Fleet Sex Year ",paste0(seq(from=1,to=Nlen))),ncol=Nlen+3,file=FileName,append=T)
  for (Ifleet in 1:General$Nfleet)
    for (Year in 1:(Stock[[Istock]]$Nhist+General$Nproj))
      for (Isex in 1:Nsex)
        write(c(Ifleet,Isex,Year+YrOffset,format(round(Stock[[Istock]]$RetSelLen[Isex,Ifleet,,Year],6),width=6)),ncol=Nlen+3,file=FileName,append=T)
  
  write("\n_Age-specific selectivity",file=FileName,append=T)
  write(c("#_Fleet Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Ifleet in 1:General$Nfleet)
   for (Year in 1:(Stock[[Istock]]$Nhist+General$Nproj))
    for (Isex in 1:Nsex)
      write(c(Ifleet,Isex,Year+YrOffset,format(round(Stock[[Istock]]$SelAge[Ifleet,Isex,,Year],6),width=6)),ncol=MaxAge+3,file=FileName,append=T)
  write("\n_Age-specific weight-at-age (total catch) ",file=FileName,append=T)
  write(c("#_Fleet Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Ifleet in 1:General$Nfleet)
    for (Year in 1:(Stock[[Istock]]$Nhist+General$Nproj))
      for (Isex in 1:Nsex)
        write(c(Ifleet,Isex,Year+YrOffset,format(round(Stock[[Istock]]$SelWghtAge[Ifleet,Isex,,Year],6),width=6)),ncol=MaxAge+3,file=FileName,append=T)
  write("\n_Age-specific weight-at-age (retained catch) ",file=FileName,append=T)
  write(c("#_Fleet Sex Year ",paste0(seq(from=0,to=MaxAge-1))),ncol=MaxAge+3,file=FileName,append=T)
  for (Ifleet in 1:General$Nfleet)
    for (Year in 1:(Stock[[Istock]]$Nhist+General$Nproj))
         for (Isex in 1:Nsex)
        write(c(Ifleet,Isex,Year+YrOffset,format(round(Stock[[Istock]]$SelRetWghtAge[Ifleet,Isex,,Year],6),width=6)),ncol=MaxAge+3,file=FileName,append=T)
 
  ObjSave$SelAge <- Stock[[Istock]]$SelAge
  ObjSave$SelWghtAge <- Stock[[Istock]]$SelWghtAge
  ObjSave$SelRetWghtAge <- Stock[[Istock]]$SelRetWghtAge
  
  write("\n_Recruitment deviations ",file=FileName,append=T)
  write(c("#_Year rec_dev"),ncol=MaxAge+3,file=FileName,append=T)
  for (Year in 1:(Nhist+General$Nproj))
   write(paste0(Year+YrOffset," ",format(round(Stock[[Istock]]$Rec_devs[Year],6),width=8)),file=FileName,append=T)  
  
  print(FileName)
  Species <- substr(FileName,18,nchar(FileName))
  print(Species)
  print(FolderResults)
  FileName <- paste0(FolderResults,"/Object.",Species,".sav")
  print(FileName)
  #save(ObjSave,file=FileName)
  
  # Brett C ADD THIS LINE TO CLOSE THE FILE
  close(fc)
  
}

# ===================================================================================================================================
# ===================================================================================================================================

WriteLog <- function(Isim,Istock,FileName)
 {
  #print(FileName)
  MaxAge <- Stock[[Istock]]$MaxAge
  Nsex   <- Stock[[Istock]]$Nsex
  Nhist   <- Stock[[Istock]]$Nhist
  Nlen   <- Stock[[Istock]]$Nlen
  YrOffset <- Stock[[Istock]]$YrOffset
  
  if (Isim==1 & Istock==1)
   {
    cat("#Nspecies ",General$Nspec,"\n",file=FileName)
    cat("#Nstocks ",General$Nstocks,"\n",file=FileName,append=T)
    cat(General$Stock.Names,file=FileName,append=T)
    cat("\n",file=FileName,append=T)
    cat("#Nfleets ",General$Nfleet,"\n",file=FileName,append=T)
    cat("#Ncat_fleets ",General$Ncat_fleet,"\n",file=FileName,append=T)
    cat(General$Fleet.Names,file=FileName,append=T)
    cat("\n",file=FileName,append=T)
    cat("#Projection_period ",General$Nproj,"years \n",file=FileName,append=T)
    cat("\n",file=FileName,append=T)
    cat("#Will PGMSY be used: ",General$PGMSY.Use,"\n",file=FileName,append=T)
    cat("#Will Indicator species be used: ",General$Indicator.Species.Use,"\n",file=FileName,append=T)
    cat("\n",file=FileName,append=T)
    cat(paste0("Number of simulations: ",General$Nsim),file=FileName,append=T)
  }
  
  if (Isim==1)
   {
    cat(paste0("\n#Stock_",Istock," ",General$Stock.Names[Istock]," details\n"),file=FileName,append=T)
    cat(paste0("#Nsex_",Istock),Stock[[Istock]]$Nsex,"\n",file=FileName,append=T)
    cat(paste0("#Year_Offset_",Istock),Stock[[Istock]]$YrOffset,"\n",file=FileName,append=T)
    cat(paste0("#NhistY_",Istock," ",Stock[[Istock]]$Nhist),"\n",file=FileName,append=T)
    cat(paste0("#Historical_years_",Istock," Start_Hist: ",Stock[[Istock]]$YrOffset+1,"; End_Hist: ",Stock[[Istock]]$YrOffset+Stock[[Istock]]$Nhist,"; End_Proj: ",Stock[[Istock]]$YrOffset+Stock[[Istock]]$Nhist+General$Nproj+1),"\n",file=FileName,append=T)
    cat(paste0("#Type_of_assessment: ",Stock[[Istock]]$Ass$Type,"\n"),file=FileName,append=T)
    cat(paste0("#Assessment_frequecy: ",General$AssFreq[Istock]," years\n"),file=FileName,append=T)
    if (Stock[[Istock]]$Ass$Type=="Tier_1a")
     {
      if (Stock[[Istock]]$Ass$MSY==3) Out <- "SSB target" else Out <- "SPR target"
      cat(paste0(Out,"; HCR = ",Stock[[Istock]]$Ass$C_Prot," ; ",abs(Stock[[Istock]]$Ass$C_NoF)," ; ",Stock[[Istock]]$Ass$C_inflect," ; ",Stock[[Istock]]$Ass$Biomass_target,"\n"),file=FileName,append=T)  
    }
    if (Stock[[Istock]]$Ass$Type=="Tier_4a")
     {
      cat(paste0("HCR = ",Stock[[Istock]]$Ass$BDM_alpha," ; ",Stock[[Istock]]$Ass$BDM_beta," ; ",Stock[[Istock]]$Ass$BDM_DT4.target,"\n"),file=FileName,append=T)  
    }
    cat("#Variability_constaints\n",file=FileName,append=T)
    cat(paste0("    #Minimum_TAC: ",Stock[[Istock]]$Ass$Min.TAC,"\n"),file=FileName,append=T)
    cat(paste0("    #Min_and_Max_change: ",Stock[[Istock]]$Ass$Beta1," ; ",Stock[[Istock]]$Ass$Beta1,"\n"),file=FileName,append=T)
    cat(paste0("    #Last_TAC: ",Stock[[Istock]]$Ass$Last.TAC,"\n"),file=FileName,append=T)
    if (General$PGMSY.Primary.Species[Istock]>0) cat(paste0("#Byproduct species for PGMSY; linked to ",General$Stock.Names[General$PGMSY.Primary.Species[Istock]],"\n"),file=FileName,append=T)
    if (General$Indicator.Species[Istock]>0) cat(paste0("#Indicated species; linked to ",General$Stock.Names[General$Indicator.Species[Istock]],"\n"),file=FileName,append=T)
  
    if (Istock==General$Nstocks) cat("\n\n\n",file=FileName,append=T)
   }

  if (Istock==General$Nstocks) 
   {
    VecOut <- c("Doing simulation",Isim)
    cat(VecOut,"\n",file=FileName,append=T)
   }

 }

# ===================================================================================================================================
# ===================================================================================================================================

WriteProjFiles <- function(Isim,Istock,ProjSumSaveName,ProjSaveName,ProjSumAreaSaveName)
{
  Nhist   <- Stock[[Istock]]$Nhist
  YrOffset <- Stock[[Istock]]$YrOffset
  
  # Headers
  if (Isim==1 & Istock==1)
  {
   
   VecOut <- c("#Stock Sim Year Period ")
   VecOut <- c(VecOut,"Ret_catch","Discard_catch","Tot_catch")
   VecOut <- c(VecOut,"Total_SSB","Total_Depletion","Total_Ref_Biomass","Expected.recruitment","Recruitment","Revenue","Cost","Profit")
   VecOut <- c(VecOut,"")
   cat(VecOut,"\n",file=ProjSumSaveName,append=T)
   
   VecOut <- c("#Stock Sim Year Period ")
   VecOut <- c(VecOut,"True_RBC","RBC","TAC-adjust_(Discard)","TAC-adjust_(Buffer)","TAC-adjust_(Constrain)","TAC-adjust_(PGMSY)","TAC-adjust_(Indicator)","Target_catch",
                      "Ret_catch","Discard_catch","Tot_catch",
                      "Total_SSB","Total_Depletion","Ref_Biomass",
                      "Total_Recruitment")
   for (Ifleet in c(1:General$Ncat_fleet,0)) VecOut <- c(VecOut,c(paste0("Ret_catch_",Ifleet),paste0("Discard_catch_",Ifleet),paste0("Tot_catch_",Ifleet)),c(paste0("Revenue_",Ifleet),paste0("Cost_",Ifleet),paste0("Profit_",Ifleet)))
   VecOut <- c(VecOut,"")
   cat(VecOut,"\n",file=ProjSaveName,append=T)
    
   VecOut <- c("#Stock Area Sim Year Period ")
   VecOut <- c(VecOut,"Ret_catch_area","Discard_catch_Area","Tot_catch_Area",
                      "Area_SSB","Area_Depletion","Area_Recruitment","Area_Recr_Dev")
   for (Ifleet in c(1:General$Ncat_fleet,0)) VecOut <- c(VecOut,c(paste0("Full_F_",Ifleet),paste0("Ret_catch_",Ifleet),paste0("Discard_catch_",Ifleet),paste0("Tot_catch_",Ifleet)),c(paste0("Revenue_",Ifleet),paste0("Cost_",Ifleet),paste0("Profit_",Ifleet)))
   VecOut <- c(VecOut,"")
   cat(VecOut,"\n",file=ProjSumAreaSaveName,append=T)
  }

  # Other outputs
  for (Year in 1:(Nhist+General$Nproj+1))
   {
    if (Year <= Nhist) Period <- "Hist" else Period <- "Sim"
    
    # Highest level summary
    VecOut <- c(Istock,Isim,Year+YrOffset,Period)
    VecOut <- c(VecOut,sum(Stock[[Istock]]$CatchRetained[,,Year]),sum(Stock[[Istock]]$CatchDiscard[,,Year]),sum(Stock[[Istock]]$CatchTotal[,,Year]))
    B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,Year]/Stock[[Istock]]$R00)
    VecOut <- c(VecOut,sum(Stock[[Istock]]$SSB[,Year]),sum(Stock[[Istock]]$SSB[,Year])/B0*100,
                sum(Stock[[Istock]]$BREF[,Year]),sum(Stock[[Istock]]$Exp.Recr[,Year]),sum(Stock[[Istock]]$Recr[,Year]))    
    Revenue <- sum(Stock[[Istock]]$Revenue[,,Year])
    Cost <- sum(Stock[[Istock]]$Cost[,,Year])
    Profit <- Revenue - Cost
    VecOut <- c(VecOut,Revenue,Cost,Profit)
    write(VecOut,ncol=length(VecOut),file=ProjSumSaveName,append=T) 
    
    # Next level summary including fleets
    VecOut0 <- c(Istock,Isim,Year+YrOffset,Period)
    VecOut0 <- c(VecOut0,Stock[[Istock]]$TrueRBC[Year],sum(Stock[[Istock]]$RBC[Year]),
                 Stock[[Istock]]$TACsStep[1:6,Year])
    VecOut0 <- c(VecOut0,sum(Stock[[Istock]]$CatchRetained[,,Year]),sum(Stock[[Istock]]$CatchDiscard[,,Year]),sum(Stock[[Istock]]$CatchTotal[,,Year]))
    VecOut0 <- c(VecOut0,sum(Stock[[Istock]]$SSB[,Year]),sum(Stock[[Istock]]$SSB[,Year])/B0*100,
                sum(Stock[[Istock]]$BREF[,Year]),sum(Stock[[Istock]]$Recr[,Year]))    
    Totals <- rep(0,6)
    for (Ifleet in 1:General$Ncat_fleet)
     {
      RetCatch <- sum(Stock[[Istock]]$CatchRetained[,Ifleet,Year])
      Discard <- sum(Stock[[Istock]]$CatchDiscard[,Ifleet,Year])
      TotCatch <- sum(Stock[[Istock]]$CatchTotal[,Ifleet,Year])
      Revenue <- sum(Stock[[Istock]]$Revenue[,Ifleet,Year])
      Cost <- sum(Stock[[Istock]]$Cost[,Ifleet,Year])
      Profit <- Revenue - Cost
      Outputs <- c(RetCatch,Discard,TotCatch,Revenue,Cost,Profit)
      Totals <- Totals + Outputs
      VecOut0 <- c(VecOut0,Outputs)
    }
    VecOut0 <- c(VecOut0,Totals)
    write(VecOut0,ncol=length(VecOut0),file=ProjSaveName,append=T) 
 
    # Results by area
    for (Iarea in 1:General$Narea)  
     {
      VecOut0 <- c(Istock,Iarea,Isim,Year+YrOffset,Period)
      VecOut0 <- c(VecOut0,sum(Stock[[Istock]]$CatchRetained[Iarea,,Year]),sum(Stock[[Istock]]$CatchDiscardl[Iarea,,Year]),sum(Stock[[Istock]]$CatchTotal[Iarea,,Year]))
      VecOut0 <- c(VecOut0,Stock[[Istock]]$SSB[Iarea,Year],Stock[[Istock]]$Depletion[Iarea,Year]*100,
                   Stock[[Istock]]$Recr[Iarea,Year],Stock[[Istock]]$Rec_devs[Iarea,Year])    
      Totals <- rep(0,7)
      for (Ifleet in 1:General$Ncat_fleet)
       {
        RetCatch   <- Stock[[Istock]]$CatchRetained[Iarea,Ifleet,Year]
        Discard    <- Stock[[Istock]]$CatchDiscard[Iarea,Ifleet,Year]
        TotCatch   <- Stock[[Istock]]$CatchTotal[Iarea,Ifleet,Year]
        Revenue    <- Stock[[Istock]]$Revenue[Iarea,Ifleet,Year]
        Cost <- Stock[[Istock]]$Cost[Iarea,Ifleet,Year]
        Profit <- Revenue - Cost
        Outputs <- c(Stock[[Istock]]$Full[Iarea,Ifleet,Year],RetCatch,Discard,TotCatch,Revenue,Cost,Profit)
        Totals <- Totals + Outputs
        VecOut0 <- c(VecOut0,Outputs)
       }
      VecOut0 <- c(VecOut0,Totals)
      write(VecOut0,ncol=length(VecOut0),file=ProjSumAreaSaveName,append=T) 
    }
    
  }
   
}

# ===================================================================================================================================

WriteWA <- function(Isim,Istock,FileName)
 {
  MaxAge <- Stock[[Istock]]$MaxAge
  Nsex   <- Stock[[Istock]]$Nsex
  Nhist   <- Stock[[Istock]]$Nhist
  Nlen   <- Stock[[Istock]]$Nlen
  YrOffset <- Stock[[Istock]]$YrOffset

  if (Isim==1)
   {
    VecOut <- c("#Stock Area Sim Year Period ") 
    VecOut <- c(VecOut,"SSB-F","SSB-M","SSB-T","Mat-F")
    VecOut <- c(VecOut,"")
    cat(VecOut,"\n",file=FileName,append=T)
   }

  # Other outputs
  for (Year in 1:(Nhist+General$Nproj+1))
   {
    if (Year <= Nhist) Period <- "Hist" else Period <- "Sim"
    
    VecOut <- c(Istock,0,Isim,Year+YrOffset,Period)
    VecOut <- c(VecOut,sum(Stock[[Istock]]$SSBSex[1,,Year]))
    if (Nsex==1) VecOut <- c(VecOut,-1,sum(Stock[[Istock]]$SSBSex[1,,Year]))    
    if (Nsex==2) VecOut <- c(VecOut,sum(Stock[[Istock]]$SSBSex[2,,Year]),
                           sum(Stock[[Istock]]$SSBSex[1,,Year])+sum(Stock[[Istock]]$SSBSex[2,,Year]))    
    VecOut <- c(VecOut,Stock[[Istock]]$MatF[Year])
    write(VecOut,ncol=length(VecOut),file=FileName,append=T) 
   }
  
 }

# ===================================================================================================================================

WriteEst <- function(Isim,Istock,FileName1,FileName2,FileName3)
 {
  # Assessment output  
  for (Jyr in 1:General$Nproj)
   for (IassArea in 1:Stock[[Istock]]$NassArea)  
    {
    VecOut <- c(Istock,Isim,Jyr,IassArea,1,Stock[[Istock]]$Ass$SSB.estimates[Isim,Jyr,IassArea,])
    write(VecOut,ncol=length(VecOut),file=FileName1,append=T) 
    VecOut <- c(Istock,Isim,Jyr,IassArea,2,Stock[[Istock]]$Ass$Depl.estimates[Isim,Jyr,IassArea,])
    write(VecOut,ncol=length(VecOut),file=FileName1,append=T) 
    VecOut <- c(Istock,Isim,Jyr,IassArea,3,Stock[[Istock]]$Ass$Recr.estimates[Isim,Jyr,IassArea,])
    write(VecOut,ncol=length(VecOut),file=FileName1,append=T) 
   }
  # Parameter estimates
  for (Jyr in 1:General$Nproj)  
   for (IassArea in 1:Stock[[Istock]]$NassArea)  
   {
    VecOut <- c(Istock,Isim,Jyr,IassArea,1,Stock[[Istock]]$Ass$Par.estimates[Isim,Jyr,IassArea,])
    write(VecOut,ncol=length(VecOut),file=FileName2,append=T) 
   }
  for (Jyr in 1:General$Nproj)
   for (IassArea in 1:Stock[[Istock]]$NassArea)  
   {
    VecOut <- c(Istock,Isim,Jyr,IassArea,1,Stock[[Istock]]$Ass$AllPar.estimates[Isim,Jyr,IassArea,])
    write(VecOut,ncol=length(VecOut),file=FileName3,append=T) 
   }
  
 }

# ===================================================================================================================================

WriteMulti <- function(Isim,FileName)
 {

  Narea <- General$Narea
  if (Isim==1)
   {
    VecOut <- c("#Sim Year ") 
    VecOut <- c(VecOut,"Obj1","Obj2","Obj3")
    for (Jstock in 1:General$Nstocks) VecOut <- c(VecOut,paste0("TargetTAC_",Jstock))
    for (Jstock in 1:General$Nstocks) VecOut <- c(VecOut,paste0("EstTAC_",Jstock))
    for (Ifleet in 1:(Narea*sum(General$TACs.fleets=="Yes"))) VecOut <- c(VecOut,paste0("Multi_",Ifleet))
    for (Jstock in 1:General$Nstocks) VecOut <- c(VecOut,paste0("Link_Species_",Jstock))
    for (Jstock in 1:General$Nstocks) VecOut <- c(VecOut,paste0("PGMSY_Adjust_",Jstock))
    for (Jstock in 1:General$Nstocks) VecOut <- c(VecOut,paste0("Indicator_Adjust_",Jstock))
    cat(VecOut,"\n",file=FileName,append=T)
   }

   for (Jyr in 1:General$Nproj)
    {
     VecOut <- c(Isim,Jyr)
     VecOut <- c(VecOut,General$Multi$Obj0[Jyr],General$Multi$Obj1[Jyr],General$Multi$Obj2[Jyr])
     VecOut <- c(VecOut,General$Multi$TargetTACs[Jyr,])
     VecOut <- c(VecOut,General$Multi$EstTACs[Jyr,])
     VecOut <- c(VecOut,General$Multi$Multipliers[Jyr,])
     VecOut <- c(VecOut,General$Multi$Link.Species[Jyr,])
     VecOut <- c(VecOut,General$Multi$PGMSY.Adjustment[Jyr,])
     VecOut <- c(VecOut,General$Multi$Indicator.Adjustment[Jyr,])
     write(VecOut,ncol=length(VecOut),file=FileName,append=T) 
    }
 }

# ===================================================================================================================================

WriteInsurance <- function(Isim,Istock,FileName)
 {
  
  # Set variables
  Nhist    <- Stock[[Istock]]$Nhist
  YrOffset <- Stock[[Istock]]$YrOffset
  
  if (Isim==1)
   {
    VecOut <- c("#Stock Area Sim Year Period ")
    VecOut <- c(VecOut,"RBC","TAC-final","TAC","Ret_catch","SSB","Depletion","Ref_Biomass",
                "Catastrophic.M","Insurance.prob")
    for (Ifleet in c(1:General$Ncat_fleet,0)) 
     {
      VecOut <- c(VecOut,c(paste0("Catch_",Ifleet),paste0("Revenue_",Ifleet),paste0("Days_",Ifleet),paste0("Cost_",Ifleet),paste0("Profit_",Ifleet)))
      for (Itype in 1:3) VecOut <- c(VecOut,c(paste0("Expected.Loss_",Ifleet,"_",Itype)))
      for (Ins.type in 1:11)
        VecOut <- c(VecOut,c(paste0("Ins.Cost_",Ifleet,"_",Ins.type),paste0("Payout_",Ifleet,"_",Ins.type),paste0("Net_Profit_",Ifleet,"_",Ins.type)))
     }
    VecOut <- c(VecOut,"")
    cat(VecOut,"\n",file=FileName,append=T)
   }
  
  for (Year in 1:(Nhist+General$Nproj+1))
   {
    if (Year <= Nhist) Period <- "Hist" else Period <- "Sim"
    
    # By area
    for (Iarea in 1:General$Narea)  
     {
      # Insurance summary
      VecOutI <- c(Istock,Iarea,Isim,Year+YrOffset,Period)
      VecOutI <- c(VecOutI,sum(Stock[[Istock]]$RBC[Year]),Stock[[Istock]]$TACsStep[5,Year],Stock[[Istock]]$TACs[Year],sum(Stock[[Istock]]$CatchRetained[,,Year]))
      VecOutI <- c(VecOutI,Stock[[Istock]]$SSB[Iarea,Year],Stock[[Istock]]$Depletion[Iarea,Year]*100,sum(Stock[[Istock]]$BREF[,Year]))    
      VecOutI <- c(VecOutI,Stock[[Istock]]$Mcat.Index[Year], Stock[[Istock]]$Prob.Collapse.Ins[Year])
      Totals <- rep(0,35)
      for (Ifleet in 1:General$Ncat_fleet)
       {
        Catch <- Stock[[Istock]]$CatchRetained[Iarea,Ifleet,Year]
        Revenue <- Stock[[Istock]]$Revenue[Iarea,Ifleet,Year]
        Cost <- Stock[[Istock]]$Cost[Iarea,Ifleet,Year]
        Days <- Stock[[Istock]]$Days[Iarea,Ifleet,Year]
        Profit1 <- Revenue - Cost
        Outputs <- c(Catch,Revenue,Days,Cost,Profit1,Stock[[Istock]]$ExpectProfit.Fish[,Ifleet,Year])
        for (Ins.type in 1:11)
         {
          Payout <- Stock[[Istock]]$PayOut[Ins.type,Ifleet,Year]
          Ins.Cost <- Stock[[Istock]]$Cost.Insurance.Bought.Fish[Ins.type,Ifleet,Year]
          Profit2 <- Profit1 + Payout - Ins.Cost
          Outputs <- c(Outputs,c(Ins.Cost,Payout,Profit2))
         }
        Totals <- Totals + Outputs
        VecOutI <- c(VecOutI,Outputs)
       }
      VecOutI <- c(VecOutI,Totals)
      write(VecOutI,ncol=length(VecOutI),file=FileName,append=T) 
    }
  } # Year

 }

# ===================================================================================================================================

WriteMisc <- function(Isim,Istock,FileName)
{
  
  # Set variables
  Nhist    <- Stock[[Istock]]$Nhist
  YrOffset <- Stock[[Istock]]$YrOffset
  
  if (Isim==1)
   {
    VecOut <- c("#Stock Area Sim Year Period ")
    for (IassArea in 1:Stock[[Istock]]$NassArea) VecOut <- c(VecOut,paste0("Tier1_converage_area_",IassArea))
    cat(VecOut,"\n",file=FileName,append=T)
   }
  
  for (Jyr in 1:General$Nproj)  
   for (IassArea in 1:Stock[[Istock]]$NassArea)  
    {
    Period <- "Sim"
    VecOutI <- c(Istock,IassArea,Isim,Jyr,Period)
    for (IassArea in 1:Stock[[Istock]]$NassArea) 
      VecOutI <- c(VecOutI,Stock[[Istock]]$Ass$MaxGrads[Isim,Jyr,IassArea])
    write(VecOutI,ncol=length(VecOutI),file=FileName,append=T) 
   }
                                                             
}

# ===================================================================================================================================
