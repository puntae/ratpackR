 
source("R inserts/Readin.R")
source("R inserts/WriteOut.R")
source("R inserts/Assessment.R")
source("R inserts/DataGen.R")
Current_folder <- getwd()

DebugFile <- NULL

# Compile the model code
print("compiling")
sourceCpp("R inserts/Model.cpp")
print("compiled")


# ===================================================================================================================================

SetBuffer <- function(Istock,TACs) 
 {
  
  for (Iyear in 1:length(TACs))
   {
    # Default buffer
    Buffer <- Stock[[Istock]]$Ass$Buffer
    # Time buffer
    Time <- (Iyear-1) 
    T.Buffer <- 1.0-Time*Stock[[Istock]]$Ass$Stale.rate
    # Net buffer  
    Buffer <- max(Stock[[Istock]]$Ass$Min.Buffer,T.Buffer)
    TACs[Iyear] <- TACs[Iyear]*Buffer
   }
 
 return(TACs)
}

# ===================================================================================================================================

PGMSY <- function(TACs,Iyear,Years=c(1,2))
 {
  Diags <- T
  YYEAR <- 1 

  if (Diags==T) print(c("in PGMSY ",Iyear))  
  write(paste0("in PGMSY ",Iyear),file=DebugFile,append=T)  
  write("Original TACs (total and then by fleet)",file=DebugFile,append=T)
  for (Istock in 1:General$Nstocks) cat(round(sum(TACs[Istock,1,,1]),0)," ",file=DebugFile,append=T); cat("\n",file=DebugFile,append=T)
  for (Istock in 1:General$Nstocks) cat(Istock,round(TACs[Istock,1,,1],0),"\n",file=DebugFile,append=T)
  
  TAC.muliplier <- rep(1,General$Nstocks)                        # Multiplier is nothing to start with (1)

  # Compute the average catch matrix (based on the most recent 5 years)
  Catch.Comp <- matrix(0,nrow=General$Ncat_fleet,ncol=General$Nstocks)
  for (Istock in 1:General$Nstocks)
   for (Ifleet in 1:General$Ncat_fleet) 
     # Update
     for (Jyear in Stock[[Istock]]$Nhist+Iyear-1-c(0:4))  
      Catch.Comp[Ifleet,Istock] <- Catch.Comp[Ifleet,Istock] + sum(Stock[[Istock]]$CatchRetained[,Ifleet,Jyear])/5

  cat("Catch (stock rows, fleet column)\n",file=DebugFile,append=T)
  for (Istock in 1:General$Nstocks) cat(Istock,round(Catch.Comp[,Istock],0),"\n",file=DebugFile,append=T) 
  
  # Predict the catch of a byproduct species for one year (YYEAR=1)
  Revised.catch <- rep(0,General$Nstocks)
  Source.Revised.catch <- matrix(0,nrow=General$Nstocks,ncol=General$Nstocks)
  
  for (Istock in 1:General$Nstocks)
   {
    # Only continue for byproduct species
    if (General$PGMSY.Primary.Species[Istock]!=0)
     {
      write(paste0("PGMSY stock: ",Istock," ",General$PGMSY.Primary.Species[Istock]),file=DebugFile,append=T)
      # Compute the catch of the byproduct species based on the TACs for the key commercial species
      for (Ifleet in 1:General$Ncat_fleet)
       {
        # Which stock do we use to link to this fleet
        Jstock <- General$PGMSY.Primary.Species.by.fleet[Ifleet]
        # There is no species linked to this species - just carry over the TAC
        if (Jstock == 0) Revised.catch[Istock] <- Revised.catch[Istock] + sum(TACs[Istock,,Ifleet,YYEAR])
        if (Jstock > 0)
         {
          # Ratio of the TAC to the average catch
          Proportion <- sum(TACs[Jstock,,Ifleet,YYEAR])/Catch.Comp[Ifleet,Jstock]
          cat(Istock,Ifleet,Jstock,sum(TACs[Jstock,,Ifleet,YYEAR]),Catch.Comp[Ifleet,Jstock],Proportion,Catch.Comp[Ifleet,Istock],Proportion*Catch.Comp[Ifleet,Istock],"\n",file=DebugFile,append=T)
          # Now scale to the catch of the byproduct species
          Source.Revised.catch[Istock,Jstock] <- Source.Revised.catch[Istock,Jstock]+ Proportion*Catch.Comp[Ifleet,Istock]
          Revised.catch[Istock] <- Revised.catch[Istock] + Proportion*Catch.Comp[Ifleet,Istock]
          #cat(Proportion*Catch.Comp[Ifleet,Istock],"\n")
         }
       } # Ifleet
      
      # now check for any multiplier on the TAC
      # =======================================
      write("Source.Revised.catch",file=DebugFile,append=T)
      write(c(Istock,Source.Revised.catch[Istock,]),ncol=General$Nstocks+1,file=DebugFile,append=T)  
      write(c("Check",Istock,General$PGMSY.Primary.Species[Istock],Revised.catch[Istock],sum(TACs[Istock,,,YYEAR])),ncol=10,file=DebugFile,append=T)
      
      # Is the expected catch larger than than TAC (MFT)  
      if (Revised.catch[Istock] > sum(TACs[Istock,,,YYEAR]))  
       {
        cat("Revising: ",General$PGMSY.Primary.Species[Istock]," because of ",Istock,"\n",file=DebugFile,append=T)
        # Work out how much to reduce the TAC for key commercial species by
        TAC.reduction <- sum(TACs[Istock,,,YYEAR])/Revised.catch[Istock]
        #print(TAC.reduction)      
        if (TAC.reduction < TAC.muliplier[General$PGMSY.Primary.Species[Istock]])
         {
          General$Multi$Link.Species[Iyear,General$PGMSY.Primary.Species[Istock]] <<- Istock
          General$Multi$PGMSY.Adjustment[Iyear,General$PGMSY.Primary.Species[Istock]] <<-TAC.reduction
          TAC.muliplier[General$PGMSY.Primary.Species[Istock]] <- TAC.reduction
         } 
        cat("Adjust",Istock,General$PGMSY.Primary.Species[Istock],TAC.reduction,TAC.muliplier[General$PGMSY.Primary.Species[Istock]],"\n",file=DebugFile,append=T)
       } # Was there a need for a multiplier

    } # If this is a byproduct species
   } # Istock loop

  # Now adjust the TACs (if necessary) and also impose the minimum TAC (again)
  for (Istock in 1:General$Nstocks) 
   {
    TACs[Istock,,,YYEAR] <-  TACs[Istock,,,YYEAR] *  TAC.muliplier[Istock]
    if (sum(TACs[Istock,,,YYEAR]) < Stock[[Istock]]$Ass$Min.TAC) TACs[Istock,,,YYEAR] <- TACs[Istock,,,YYEAR]/sum(TACs[Istock,,,YYEAR])*Stock[[Istock]]$Ass$Min.TAC
   }
  write("TAC.muliplier",file=DebugFile,append=T)
  write(TAC.muliplier,file=DebugFile,append=T)
  write("Updated TACs (total and then by fleet)",file=DebugFile,append=T)
  for (Istock in 1:General$Nstocks) cat(round(sum(TACs[Istock,1,,1]),0)," ",file=DebugFile,append=T); cat("\n",file=DebugFile,append=T)
  for (Istock in 1:General$Nstocks) cat(Istock," ",round(TACs[Istock,1,,1],0),"\n",file=DebugFile,append=T)
  write("Done PGMSY",file=DebugFile,append=T)
  #if (Diags==1)  AAAA
  if (Diags==T) print("Done PGMSY")
  return(TACs)
 } # PGMSY

# ===================================================================================================================================

Indicator.species.TACS <- function(TACs,Iyear,Years=c(1,2))
{
  Diags <- T
  YYEAR <- 1
  
  if (Diags==T) print(c("In Indicator ",Iyear))  
  write(paste0("In Indicator ",Iyear),file=DebugFile,append=T)  
  write("Original TACs (total and then by fleet)",file=DebugFile,append=T)
  for (Istock in 1:General$Nstocks) cat(round(sum(TACs[Istock,1,,1]),0)," ",file=DebugFile,append=T); cat("\n",file=DebugFile,append=T)
  for (Istock in 1:General$Nstocks) cat(Istock," ",round(TACs[Istock,1,,1],0),"\n",file=DebugFile,append=T)
  
  # First find the TACs for last year and this year
  Last.TAC <- rep(0,General$Nstocks)
  This.TAC <- rep(0,General$Nstocks)
  for (Istock in 1:General$Nstocks)
   {
    # Special case for year 1
    if (Iyear==1)
     Last.TAC[Istock] <- General$Indicator.Last.TAC[Istock]  
    else
     Last.TAC[Istock] <- Stock[[Istock]]$Final.TAC[Stock[[Istock]]$Nhist+Iyear-1] 
    This.TAC[Istock] <- sum(TACs[Istock,,,YYEAR])      
   } # Istock
  write("Last.TAC",file=DebugFile,append=T) 
  write(Last.TAC,ncol=length(Last.TAC),file=DebugFile,append=T) 
  write("This.TAC",file=DebugFile,append=T) 
  write(This.TAC,length(This.TAC),file=DebugFile,append=T) 
  
  # Now adjust the TACs for the stocks that are indicated (does not change the indicator species)
  for (Istock in 1:General$Nstocks) 
   if (General$Indicator.Species[Istock] != 0)
    {
     Ratio <- This.TAC[General$Indicator.Species[Istock]]/(1.0e-10+Last.TAC[General$Indicator.Species[Istock]])
     This.TAC[Istock] <- Last.TAC[Istock] * Ratio
     General$Multi$Link.Species[Iyear,Istock] <<- General$Indicator.Species[Istock]
     General$Multi$Indicator.Adjustment[Iyear,Istock] <<- Ratio
     #print(c(Ratio,This.TAC[Istock]))
     TACs[Istock,,,YYEAR] <- This.TAC[Istock]*TACs[Istock,,,YYEAR]/sum(TACs[Istock,,,YYEAR])
     write(c("Adjusted: ",Istock,General$Indicator.Species[Istock],Ratio,Last.TAC[Istock],sum(TACs[Istock,,,YYEAR])),ncol=10,file=DebugFile,append=T)
   } 

  write("Updated TACs (total and then by fleet)",file=DebugFile,append=T)
  for (Istock in 1:General$Nstocks) cat(round(sum(TACs[Istock,1,,1]),0)," ",file=DebugFile,append=T); cat("\n",file=DebugFile,append=T)
  for (Istock in 1:General$Nstocks) cat(Istock," ",round(TACs[Istock,1,,1],0),"\n",file=DebugFile,append=T)
  write("Done Indicator species",file=DebugFile,append=T)
  if (Diags==T) print("Done Indicator species")
  return(TACs)
} # Indicator species

# ===================================================================================================================================
HybridA <- function(Istock,Year,Cat.H,Initial.Eqn=F,Use.Profit.constraint="No")
{
  Narea <- General$Narea
  Nsex <- Stock[[Istock]]$Nsex
  MaxAge <- Stock[[Istock]]$MaxAge
  Nfleet <- General$Nfleet
  Ncat_fleet <- General$Ncat_fleet
  
  # Pull out things we will need
  N.H <- array(Stock[[Istock]]$N[,,,Year],dim=c(Narea,Nsex,MaxAge))
  SelAge.H <- array(Stock[[Istock]]$SelAge[,,,Year],dim=c(General$Nfleet,Nsex,MaxAge))
  SelRetWght.H <- array(Stock[[Istock]]$SelRetWghtAge[,,,Year],dim=c(General$Nfleet,Nsex,MaxAge))
  # Weird SS thing about initial conditions
  if (Initial.Eqn==T) SelAge.H <- array(Stock[[Istock]]$SelAge[,,,Stock[[1]]$Nhist],dim=c(Nfleet,Nsex,MaxAge))
  if (Initial.Eqn==T) SelRetWght.H <- array(Stock[[Istock]]$SelRetWghtAge[,,,Stock[[1]]$Nhist],dim=c(Nfleet,Nsex,MaxAge))

  # Output of the routine
  Fout <- matrix(0,nrow=Narea,ncol=Ncat_fleet)
    
  # Allocate catch by fleet to catch by fleet and area
  Cat.H2 <- matrix(0,nrow=Narea,ncol=Ncat_fleet)
  for (Ifleet in 1:Ncat_fleet)
   if (Cat.H[Ifleet]>0)
    {
     # AEP - need to update for closed areas here
     Bvul <- rep(0,Narea)
     for (Iarea in 1:Narea) Bvul[Iarea] <- sum(N.H[Iarea,,]*Stock[[Istock]]$UseFleet[Iarea,Ifleet,Year]*SelRetWght.H[Ifleet,,])*Stock[[Istock]]$ClosedAreas[Iarea,Year]
     Bvul <- Bvul/sum(Bvul)
     Cat.H2[,Ifleet] <- Cat.H[Ifleet]*Bvul
    }

  # Solve for F by area  
  for (Iarea in 1:Narea)
   {
    CatMult <- 1
    Fout2 <- Hybrid(Istock,Year,Iarea,Cat.H2*CatMult,Initial.Eqn=F)
 
    Profit <- 0
    for (Ifleet in 1:General$Ncat_fleet) 
     {
      Revenue <- CatchPass[Ifleet]*Stock[[Istock]]$Price[Ifleet]/1000
      logFishF <- log(Fout2[Ifleet]+1.0e-20)
      Days <- exp(Stock[[Istock]]$DaysPerF1[Ifleet]+Stock[[Istock]]$DaysPerF2[Ifleet]*logFishF)
      Cost <- (Days*Stock[[Istock]]$CostPerDay[Ifleet])/1000
      Profit <- Profit + (Revenue-Cost)
     }
  
    if (Use.Profit.constraint=="Yes" & Profit < 0)
     {
      MultMin <- 0; MultMax <- 1   
      for (Itune in 1:50)
       {
        CatMult <- (MultMin + MultMax)/2
        Fout2 <- Hybrid(Istock,Year,Iarea,Cat.H2*CatMult,Initial.Eqn=F)
     
        Profit <- 0
        for (Ifleet in 1:General$Ncat_fleet)
         {
          Revenue <- CatchPass[Ifleet]*Stock[[Istock]]$Price[Ifleet]/1000
          logFishF <- log(Fout2[Ifleet]+1.0e-20)
          Days <- exp(Stock[[Istock]]$DaysPerF1[Ifleet]+Stock[[Istock]]$DaysPerF2[Ifleet]*logFishF)
          Cost <- (Days*Stock[[Istock]]$CostPerDay[Ifleet])/1000
          Profit <- Profit + (Revenue-Cost)
         }
        if (Profit > 0) MultMin <- CatMult else MultMax <- CatMult
       }
     } # Profit = 0
    Fout[Iarea,] <- Fout2
     
   } # Iarea 

  return(Fout)
}

# ===================================================================================================================================

Hybrid <- function(Istock,Year,Iarea,Cat.H2,Initial.Eqn=F)
{
 # Set some parameters
 Ntune <- 12
 Ntune <- 20
 max_harvest_rate <- Stock[[Istock]]$max_F
 Narea <- General$Narea
 Nsex <- Stock[[Istock]]$Nsex
 MaxAge <- Stock[[Istock]]$MaxAge
 Nfleet <- General$Nfleet
 Ncat_fleet <- General$Ncat_fleet

 # Pull out things we will need
 M.H <- matrix(Stock[[Istock]]$M[,,Year],nrow=Nsex,ncol=MaxAge)
 N.H <- array(Stock[[Istock]]$N[,,,Year],dim=c(Narea,Nsex,MaxAge))
 SelAge.H <- array(Stock[[Istock]]$SelAge[,,,Year],dim=c(General$Nfleet,Nsex,MaxAge))
 SelRetWght.H <- array(Stock[[Istock]]$SelRetWghtAge[,,,Year],dim=c(General$Nfleet,Nsex,MaxAge))
 # Weird SS thing about initial conditions
 if (Initial.Eqn==T) SelAge.H <- array(Stock[[Istock]]$SelAge[,,,Stock[[1]]$Nhist],dim=c(Nfleet,Nsex,MaxAge))
 if (Initial.Eqn==T) SelRetWght.H <- array(Stock[[Istock]]$SelRetWghtAge[,,,Stock[[1]]$Nhist],dim=c(Nfleet,Nsex,MaxAge))

 # Set fully-selected fishing mortality
 Fout <- rep(0,Ncat_fleet)
 
 # First find the initial F   
 TotalCatch <- sum(Cat.H2[Iarea,])
 for (Ifleet in 1:Ncat_fleet)
  if (Cat.H2[Iarea,Ifleet]>0)
   {
    Bvul <- sum(N.H[Iarea,,]*SelRetWght.H[Ifleet,,])
    temp <- Cat.H2[Iarea,Ifleet] / (Bvul + Cat.H2[Iarea,Ifleet]);
    join1 <- 1.0/(1.0+exp(30.0*(temp-0.95)));
    temp1 <- join1*temp + (1.0-join1)*0.95;
    Fout[Ifleet] = -log(1.0-temp1);
   }   
  else
   Fout[Ifleet] <- 0
 
  # Now do the tuning
  for (TuneF in 1:Ntune)
   {  
    # Compute the Z
    Zrate <- M.H
    for (Ifleet in 1:Ncat_fleet) if (Cat.H2[Iarea,Ifleet]>0) Zrate <- Zrate + Fout[Ifleet]*SelAge.H[Ifleet,,]
    Zrate2 <- N.H[Iarea,,]*(1-exp(-Zrate))/Zrate
  
    # Now so the tuning
    if (TuneF < Ntune)
     {
      # How close are we to the total catch
      Zadjuster2 <- 0
      for (Ifleet in 1:Ncat_fleet)
       if (Cat.H2[Iarea,Ifleet]>0) Zadjuster2 <- Zadjuster2 + sum(SelRetWght.H[Ifleet,,]*Fout[Ifleet]*Zrate2)
      Zadjuster <- TotalCatch/(Zadjuster2+0.00001)

      # Now adjust total Z
      Zrate <-  M.H + Zadjuster*(Zrate-M.H)
      Zrate2 <- N.H[Iarea,,]*(1-exp(-Zrate))/Zrate

      # Now adjust Z by fleet 
      for (Ifleet in 1:Ncat_fleet)
      if (Cat.H2[Iarea,Ifleet]>0)
       {
        Zadjuster2 <- sum(SelRetWght.H[Ifleet,,]*Zrate2)
        temp <- Cat.H2[Iarea,Ifleet]/(Zadjuster2 + 0.00001);
        join1 <- 1.0/(1.0+exp(30.0*(temp-0.95*max_harvest_rate)));
        Fout[Ifleet] <- join1*temp + (1.0-join1)*max_harvest_rate;
       }
     
      } # if (TuneF < Ntune)
  
   } # TuneF

 # How close are we to the total catch
 Zadjuster2 <- rep(0,General$Ncat_fleet)
 for (Ifleet in 1:General$Ncat_fleet)
   if (Cat.H2[Iarea,Ifleet]>0) Zadjuster2[Ifleet] <- Zadjuster2[Ifleet] + sum(SelRetWght.H[Ifleet,,]*Fout[Ifleet]*Zrate2)
 #cat(Cat.H2,Zadjuster2,Fout,"\n")
 CatchPass <<- Zadjuster2

 return(Fout)
}

# ===================================================================================================================================

Project.One.Ahead <- function(Istock,Iyear)
 {
  
  # Project a stock one year ahead
  
  MaxAge <- Stock[[Istock]]$MaxAge
  Nsex <- Stock[[Istock]]$Nsex
  Nlen <- Stock[[Istock]]$Nlen
  Narea <- General$Narea
  Nfleet <- General$Nfleet
  Ncat_fleet <- General$Ncat_fleet

  # Compute the true RBC
  if (Iyear != 1)
   {
    # Find the mean F (and normalize it) 
    MeanF <- array(0,dim=c(Narea,Ncat_fleet)) 
    for (Iarea in 1:Narea)
     for (Ifleet in 1:Ncat_fleet)
      MeanF[Iarea,Ifleet] <- 1.0E-20+Stock[[Istock]]$FullF[Iarea,Ifleet,Iyear-1];
    MeanF <- MeanF/sum(MeanF)
  
    # Find depletion
    YearProj <- Iyear
    SSB_Current <- sum(Stock[[Istock]]$SSB[,YearProj])
    B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,YearProj]/Stock[[Istock]]$R00)
    Depl <- SSB_Current/B0
  
    # Apply the HCR to get the Fmultipler
    if (Depl < Stock[[Istock]]$C_Prot || Depl < Stock[[Istock]]$C_NoF)
      Fmult <- 0
    else
      if (Depl > Stock[[Istock]]$C_inflect)  
       Fmult <- 1
      else
       Fmult <- (Depl-Stock[[Istock]]$C_NoF)/(Stock[[Istock]]$C_inflect-Stock[[Istock]]$C_NoF) 
    Outs <- GetRefs(Istock,MeanF,YearProj,Stock[[Istock]]$Biomass_target)
    # Set the SPR
    if (Stock[[Istock]]$MSY==2) Fout <- MeanF*Fmult*Outs$Ftar_SPR
    if (Stock[[Istock]]$MSY==3) Fout <- MeanF*Fmult*Outs$Ftar_SSB
    
    # Storage for catches
    CatchTotalRBC <- 0
    
    # Calculate Z and project catches
    NtempZ <- array(0,dim=c(Narea,Nsex,MaxAge))
    for (Iarea in 1:General$Narea)
     {
      # Fleets for this area
      Z <- matrix(Stock[[Istock]]$M[,,Iyear],nrow=Nsex,ncol=MaxAge)
      for (Ifleet in 1:Ncat_fleet) Z <- Z + Fout[Iarea,Ifleet]*Stock[[Istock]]$SelAge[Ifleet,,,Iyear]

      # Survivors from total mortality
      for (Isex in 1:Nsex)
        for (Iage in 1:MaxAge)
          NtempZ[Iarea,Isex,Iage] <- Stock[[Istock]]$N[Iarea,Isex,Iage,Iyear] * exp(-Z[Isex,Iage]) 
      
      # fishery catch-at-age
      for (Ifleet in 1:Ncat_fleet)
       {
        # Temporary variables for within-a
        for (Isex in 1:Nsex)
          for (Iage in 1:MaxAge)
          {
            # Mortality from fishing (without selex)
            Temp <- Stock[[Istock]]$N[Iarea,Isex,Iage,Iyear]*(Fout[Iarea,Ifleet]+1.0e-20)/Z[Isex,Iage]*(1.0-exp(-Z[Isex,Iage]))
            # Total catch in numbers
            RawTotalCatchN <- Temp*Stock[[Istock]]$SelexAge[Isex,Ifleet,Iage,Iyear]*Stock[[Istock]]$FracLenM[Isex,Iage,Iyear,]*Stock[[Istock]]$SelSelLen[Isex,Ifleet,,Iyear]
            # Catch in  total
            CatchTotalRBC <- CatchTotalRBC + sum(RawTotalCatchN*Stock[[Istock]]$WtLen[Isex,])
          } #Iage x Iage 
       } # Ifleet
     } # Iarea
    Stock[[Istock]]$TrueRBC[Iyear] <<- CatchTotalRBC
    #cat("Project.One.Ahead",Iyear,Stock[[Istock]]$MSY,Fmult,Fout,Stock[[Istock]]$RBC[Iyear],"\n")
   } # Iyear != 1
    
  # Set fully-selected fishing mortality
  Fout <- matrix(Stock[[Istock]]$FullF[,,Iyear],nrow=Narea,ncol=Ncat_fleet)

  # Storage for catches
  CatchRetained <- matrix(0,nrow=Narea,Ncat_fleet); CatchTotal <- matrix(0,nrow=Narea,Ncat_fleet)
  
  # Loop over areas
  BioPreds <- matrix(0,nrow=Narea,ncol=Nfleet)
  
  # Calculate Z, project catches and compute the mature F peformance statistic
  NtempZ <- array(0,dim=c(Narea,Nsex,MaxAge))
  MatF.num <- MatF.den <- 0
  for (Iarea in 1:General$Narea)
   {
  
    # Total mortality by sex and age
    Z <- matrix(Stock[[Istock]]$M[,,Iyear],nrow=Nsex,ncol=MaxAge)
    for (Ifleet in 1:Ncat_fleet) Z <- Z + Fout[Iarea,Ifleet]*Stock[[Istock]]$SelAge[Ifleet,,,Iyear]

    # Average F (for females: sex1)
    Isex <- 1
    F.at.age <- rep(0,MaxAge)
    for (Ifleet in 1:Ncat_fleet) F.at.age <- F.at.age + Fout[Iarea,Ifleet]*Stock[[Istock]]$SelAge[Ifleet,Isex,,Iyear]
    MatF.num <- MatF.num + sum(F.at.age*Stock[[Istock]]$N[Iarea,Isex,,Iyear]*Stock[[Istock]]$Fecundity[,Iyear]) 
    MatF.den <- MatF.den + sum(Stock[[Istock]]$N[Iarea,Isex,,Iyear]*Stock[[Istock]]$Fecundity[,Iyear]) 

    # Survivors from total mortality
    for (Isex in 1:Nsex)
      for (Iage in 1:MaxAge)
        NtempZ[Iarea,Isex,Iage] <- Stock[[Istock]]$N[Iarea,Isex,Iage,Iyear] * exp(-Z[Isex,Iage]) 
    
    # fishery catch-at-age
    for (Ifleet in 1:Ncat_fleet)
     {
      # Temporary variables for within-a
      RawRetCatch <- array(0,dim=c(Nsex,MaxAge,Nlen)) 
      RawTotCatch <- array(0,dim=c(Nsex,MaxAge,Nlen)) 
      Fval <- Fout[Iarea,Ifleet]+1.0e-20
      for (Isex in 1:Nsex)
       for (Iage in 1:MaxAge)
        {
         # Comps don't need to worry about F
         # Mortality from fishing (without selex)
         Temp <- Stock[[Istock]]$N[Iarea,Isex,Iage,Iyear]/Z[Isex,Iage]*(1.0-exp(-Z[Isex,Iage]))
         # Total catch in numbers
         RawTotalCatchN <- Temp*Stock[[Istock]]$SelexAge[Isex,Ifleet,Iage,Iyear]*Stock[[Istock]]$FracLenM[Isex,Iage,Iyear,]*Stock[[Istock]]$SelSelLen[Isex,Ifleet,,Iyear]
         # Catch in numbers by retained and total
         RawTotalCatchN.Rel <- RawTotalCatchN*Stock[[Istock]]$RetSelAge[Isex,Ifleet,Iage,Iyear]*Stock[[Istock]]$RetSelLen[Isex,Ifleet,,Iyear]
         RawRetCatch[Isex,Iage,] <- Fval*RawTotalCatchN.Rel
         RawTotCatch[Isex,Iage,] <- Fval*RawTotalCatchN
         CatchRetained[Iarea,Ifleet] <- CatchRetained[Iarea,Ifleet] + Fval*Stock[[Istock]]$RetSelAge[Isex,Ifleet,Iage,Iyear]*sum(RawTotalCatchN*Stock[[Istock]]$RetSelLen[Isex,Ifleet,,Iyear]*Stock[[Istock]]$WtLen[Isex,])
         CatchTotal[Iarea,Ifleet] <- CatchTotal[Iarea,Ifleet] + Fval*sum(RawTotalCatchN*Stock[[Istock]]$WtLen[Isex,])
         Stock[[Istock]]$CatchAtAgeRet[Iarea,Ifleet,Isex,Iage,Iyear] <<- sum(RawTotalCatchN.Rel)
         Stock[[Istock]]$CatchAtAgeTot[Iarea,Ifleet,Isex,Iage,Iyear] <<- sum(RawTotalCatchN)
         Stock[[Istock]]$CatchAtCAARet[Iarea,Ifleet,Isex,Iage,,Iyear] <<- RawTotalCatchN.Rel
         Stock[[Istock]]$CatchAtCAATot[Iarea,Ifleet,Isex,Iage,,Iyear] <<- RawTotalCatchN
        } #Iaex x Iage 
      # fishery catch-at-length
      for (Isex in 1:Nsex)
       for (Ilen in 1:Nlen)
        {
         #Stock[[Istock]]$CatchAtLenRet[Iarea,Ifleet,Isex,Ilen,Iyear] <<- sum(RawRetCatch[Isex,,Ilen])
         #Stock[[Istock]]$CatchAtLenTot[Iarea,Ifleet,Isex,Ilen,Iyear] <<- sum(RawTotCatch[Isex,,Ilen])
         Stock[[Istock]]$CatchAtLenRet[Iarea,Ifleet,Isex,Ilen,Iyear] <<- sum(Stock[[Istock]]$CatchAtCAARet[Iarea,Ifleet,Isex,,Ilen,Iyear])
         Stock[[Istock]]$CatchAtLenTot[Iarea,Ifleet,Isex,Ilen,Iyear] <<- sum(Stock[[Istock]]$CatchAtCAATot[Iarea,Ifleet,Isex,,Ilen,Iyear])
       } # Isex x Ilen
     } # Ifleet
    # Save the retained catch
    Stock[[Istock]]$CatchRetained[Iarea,,Iyear] <<- CatchRetained[Iarea,]
    Stock[[Istock]]$CatchTotal[Iarea,,Iyear] <<- CatchTotal[Iarea,]
    Stock[[Istock]]$CatchDiscard[Iarea,,Iyear] <<- CatchTotal[Iarea,] - CatchRetained[Iarea,]

    # Now handle surveys
    if (Nfleet > Ncat_fleet)
     for (Ifleet in (Ncat_fleet+1):Nfleet)
      {
       # survey catch-at-age
       RawRetCatch <- array(0,dim=c(Nsex,MaxAge,Nlen)) 
       RawTotCatch <- array(0,dim=c(Nsex,MaxAge,Nlen)) 
       for (Isex in 1:Nsex)
        for (Iage in 1:MaxAge)
         {
          # Mortality from fishing (without selex)
          Temp <- Stock[[Istock]]$N[Iarea,Isex,Iage,Iyear]*exp(-Z[Isex,Iage]/2)
          # Total catch in numbers
          RawTotalCatchN <- Temp*Stock[[Istock]]$SelexAge[Isex,Ifleet,Iage,Iyear]*Stock[[Istock]]$FracLenM[Isex,Iage,Iyear,]*Stock[[Istock]]$SelSelLen[Isex,Ifleet,,Iyear]
          # Catch in numbers by retained and total
          RawRetCatch[Isex,Iage,] <- RawTotalCatchN*Stock[[Istock]]$RetSelLen[Isex,Ifleet,,Iyear]
          RawTotCatch[Isex,Iage,] <- RawTotalCatchN
          Stock[[Istock]]$CatchAtAgeRet[Iarea,Ifleet,Isex,Iage,Iyear] <<- sum(RawRetCatch[Isex,Iage,])
          Stock[[Istock]]$CatchAtAgeTot[Iarea,Ifleet,Isex,Iage,Iyear] <<- sum(RawTotCatch[Isex,Iage,])
          # Catch by age and length
          Stock[[Istock]]$CatchAtCAARet[Iarea,Ifleet,Isex,Iage,,Iyear] <<- RawRetCatch[Isex,Iage,]
          Stock[[Istock]]$CatchAtCAATot[Iarea,Ifleet,Isex,Iage,,Iyear] <<- RawTotCatch[Isex,Iage,]
         } # Isex x Iage
       # survey catch at length
       for (Isex in 1:Nsex)
        for (Ilen in 1:Nlen)
         {
          Stock[[Istock]]$CatchAtLenRet[Iarea,Ifleet,Isex,Ilen,Iyear] <<- sum(RawRetCatch[Isex,,Ilen])
          Stock[[Istock]]$CatchAtLenTot[Iarea,Ifleet,Isex,Ilen,Iyear] <<- sum(RawTotCatch[Isex,,Ilen])
         } # Isex x Ilen
      } # Ifleet
   
    # Predator consumption
    if (Stock[[Istock]]$Num.Pred>0)
     for (Ipred in 1:Stock[[Istock]]$Num.Pred)
      {
       print("Andre.check")
       AAAA
       TotalConsump <- 0
       for (Isex in 1:Nsex)
        for (Iage in 1:MaxAge)
         {
          # Consumption by age
          Temp <- Stock[[Istock]]$N[Iarea,sex,Iage,Iyear]*Stock[[Istock]]$M2[Ipred,Isex,Iage,Iyear]/Z[Isex,Iage]*(1.0-exp(-Z[Isex,Iage]))
          MeanW <- sum(Stock[[Istock]]$FracLenM[Isex,Iage,Iyear,]*Stock[[Istock]]$WtLen[Isex,])
          TotalConsump <- TotalConsump + Temp*MeanW
          Stock[[Istock]]$CompumpAtAge[Iarea,Ipred,Isex,Iage,Iyear] <<- Temp
         }  
       Stock[[Istock]]$ConsumpTotal[Iarea,Ipred,Iyear] <<- TotalConsump 
     }  # Ipred
  
    # Predicted index
    for (Ifleet in 1:Nfleet)
     {
      for (Isex in 1:Nsex)
       for (Iage in 1:MaxAge)
        if (Ifleet <= General$Ncat_fleet)    
         BioPreds[Iarea,Ifleet] <- BioPreds[Iarea,Ifleet] +  Stock[[Istock]]$SelexAge[Isex,Ifleet,Iage,Iyear]*Stock[[Istock]]$SelRetWghtAge[Ifleet,Isex,Iage,Iyear]*Stock[[Istock]]$N[Iarea,Isex,Iage,Iyear]*(1.0-exp(-Z[Isex,Iage]))/Z[Isex,Iage]
        else
         BioPreds[Iarea,Ifleet] <- BioPreds[Iarea,Ifleet] +  Stock[[Istock]]$SelexAge[Isex,Ifleet,Iage,Iyear]*Stock[[Istock]]$SelWghtAge[Ifleet,Isex,Iage,Iyear]*Stock[[Istock]]$N[Iarea,Isex,Iage,Iyear]*exp(-Z[Isex,Iage]/2)
      }
  
    } # Iarea (project catches, removals, etc)
  
  # Compute a special performance metric that aggregates over area
  Stock[[Istock]]$MatF[Iyear] <<- MatF.num/MatF.den

  # Initial update update 
  Stock[[Istock]]$N[,,,Iyear+1] <<- 0
  for (Isex in 1:Nsex)
    for (Iage in 2:MaxAge)
      for (Iarea in 1:Narea)
      {
        Ntemp <- NtempZ[Iarea,Isex,Iage-1]
        for (Jarea in 1:Narea)
          Stock[[Istock]]$N[Jarea,Isex,Iage,Iyear+1] <<- Stock[[Istock]]$N[Jarea,Isex,Iage,Iyear+1] + Stock[[Istock]]$Move[Iarea,Jarea,Iage]*Ntemp
      } # Isex, Iage x Iarea
  
  # project forward (including a plus-group)
  Iage <- MaxAge
  for (Isex in 1:Nsex)
   for (Iarea in 1:Narea) 
    {
      # Plus-group
      Ntemp <- NtempZ[Iarea,Isex,Iage]
      for (Jarea in 1:Narea)
        Stock[[Istock]]$N[Jarea,Isex,Iage,Iyear+1] <<- Stock[[Istock]]$N[Jarea,Isex,Iage,Iyear+1] + Stock[[Istock]]$Move[Iarea,Jarea,Iage]*Ntemp
    }

  # Calculate SSB by area
  for (Iarea in 1:General$Narea)
   {
    SSB.y <- sum(Stock[[Istock]]$N[Iarea,1,-1,Iyear+1]*Stock[[Istock]]$Fecundity[-1,Iyear+1])
    Stock[[Istock]]$SSB[Iarea,Iyear+1] <<- SSB.y
    Stock[[Istock]]$SSBSex[1,Iarea,Iyear+1] <<- SSB.y
    if (Nsex > 1) Stock[[Istock]]$SSBSex[Nsex,Iarea,Iyear+1] <<- sum(Stock[[Istock]]$N[Iarea,Nsex,-1,Iyear+1]*Stock[[Istock]]$Fecundity[-1,Iyear+1])
    SSB0Yr <- Stock[[Istock]]$SSB0[Iarea]*Stock[[Istock]]$R0[Iarea,Iyear+1]/Stock[[Istock]]$R00[Iarea]
    Depl <- SSB.y/SSB0Yr
    Stock[[Istock]]$Depletion[Iarea,Iyear+1] <<- Depl
    BREF.y <- sum(Stock[[Istock]]$N[Iarea,,-1,Iyear+1]*Stock[[Istock]]$MeanWtAtAgeS[,-1,Iyear+1])
    Stock[[Istock]]$BREF[Iarea,Iyear+1] <<- BREF.y
   }
  
  # Apply the density-dependence function (locally / globally
  for (Iarea in 1:General$Narea)
   {
    if (Stock[[Istock]]$Global.Density.Dep==T)
     {
      SSB.y <- 0
      for (Jarea in 1:General$Narea) SSB.y <- SSB.y + sum(Stock[[Istock]]$N[Jarea,1,-1,Iyear+1]*Stock[[Istock]]$Fecundity[-1,Iyear+1])
      SSB0Yr <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,Iyear+1]/Stock[[Istock]]$R00)
      Depl <- SSB.y/SSB0Yr
      Recr <- 4.0 *sum(Stock[[Istock]]$R0[,Iyear+1])*Stock[[Istock]]$Steep*Depl / ( (1-Stock[[Istock]]$Steep) + (5*Stock[[Istock]]$Steep-1)*Depl)
      Recr <- Recr *Stock[[Istock]]$Recr.split[Iyear+1,Iarea]
    }
    else
     {
      SSB.y <- sum(Stock[[Istock]]$N[Iarea,1,-1,Iyear+1]*Stock[[Istock]]$Fecundity[-1,Iyear+1])
      SSB0Yr <- Stock[[Istock]]$SSB0[Iarea]*Stock[[Istock]]$R0[Iarea,Iyear+1]/Stock[[Istock]]$R00[Iarea]
      Depl <- SSB.y/SSB0Yr
      Recr <- 4.0 *Stock[[Istock]]$R0[Iarea,Iyear+1]*Stock[[Istock]]$Steep*Depl / ( (1-Stock[[Istock]]$Steep) + (5*Stock[[Istock]]$Steep-1)*Depl)
     }
    Recr <- Recr*Stock[[Istock]]$R0.Env.Mult[Iyear+1]
    for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,1,Iyear+1] <<- Recr/Nsex*exp(Stock[[Istock]]$Rec_devs[Iarea,Iyear+1])
    Stock[[Istock]]$Recr[Iarea,Iyear+1] <<- Recr*exp(Stock[[Istock]]$Rec_devs[Iarea,Iyear+1])
    Stock[[Istock]]$Exp.Recr[Iarea,Iyear+1] <<- Recr
   }
  
  # Economic outputs
  for (Iarea in 1:General$Narea)
   for (Ifleet in 1:Ncat_fleet)
     {
      Stock[[Istock]]$Revenue[Iarea,Ifleet,Iyear] <<- Stock[[Istock]]$CatchRetained[Iarea,Ifleet,Iyear]*Stock[[Istock]]$Price[Ifleet]/1000
      logFishF <- log(Stock[[Istock]]$FullF[Iarea,Ifleet,Iyear]+1.0e-20)
      Days <- exp(Stock[[Istock]]$DaysPerF1[Ifleet]+Stock[[Istock]]$DaysPerF2[Ifleet]*logFishF)
      Stock[[Istock]]$Days[Iarea,Ifleet,Iyear] <<- Days
      Stock[[Istock]]$Cost[Iarea,Ifleet,Iyear] <<- (Days*Stock[[Istock]]$CostPerDay[Ifleet]+Stock[[Istock]]$FixedCosts[Ifleet])/1000
      if (Stock[[Istock]]$Ass$Has.insurance)
       for (Insurance_type in 1:11)
        {
         if (Insurance_type %in% c(2,3,4,5,8,9,10,11))
          {
           if (Stock[[Istock]]$Mcat.Index[Iyear]==1)
            Stock[[Istock]]$PayOut[Insurance_type,Ifleet,Iyear] <<- Stock[[Istock]]$Insurance.Bought.Fish[Insurance_type,Ifleet,Iyear]  
          } # Parametric
         if (Insurance_type %in% c(6) & Iyear >=Stock[[Istock]]$Nhist)
          {
           Ave.Revenue <- 0
           for (Kyear in (Iyear-5):(Iyear-1)) Ave.Revenue <- Ave.Revenue + Stock[[Istock]]$Revenue[Iarea,Ifleet,Kyear]/5.0
           Last.Revenue <- Stock[[Istock]]$Revenue[Iarea,Ifleet,Iyear]
           #cat(Ave.Revenue,Last.Revenue,"\n")
           if (Last.Revenue < 0.5*Ave.Revenue) Stock[[Istock]]$PayOut[Insurance_type,Ifleet,Iyear] <<- 0.55*(Ave.Revenue-Last.Revenue)
          } # State-sponsored
         if (Insurance_type %in% c(7))
          {
          } # Anticipatory
       }
     } # Ifleet and Iarea

  # Call the data generator
  if (General$Do.any.projections==T) DataGen(Istock,Iyear,BioPreds)

} # Project.One.Ahead

# ===================================================================================================================================

Solve.for.multispecies.F <- function(Jyear,TACs)
 {
  
  # Select Effort multipliers to best match the TACS by species (not by fleet!)
  
  print("Solve.for.multispecies.F")
  write("Solve.for.multispecies.F",file=DebugFile,append=T)
  Nstocks <- General$Nstocks
  Narea <- General$Narea

  TargetTACs <- rep(0,Nstocks); EstTACs <- rep(0,Nstocks)
  
  Minfun <- function(Multipliers)
   {
    Narea <-General$Narea
    Nstocks <- General$Nstocks
    Ncat_fleet <- General$Ncat_fleet
    Multipliers <- exp(Multipliers)
    #print(Multipliers)
 
    # Fill in the F matrix
    FullF <- array(0,dim=c(Narea,Nstocks,Ncat_fleet))
    for (Istock in 1:General$Nstocks)
     {
      Ipnt <- 0
      for (Ifleet in 1:Ncat_fleet)
       for (Iarea in 1:Narea)
        if (General$TACs.fleets[Ifleet]=="Yes")
         { Ipnt <- Ipnt + 1; FullF[Iarea,Istock,Ifleet] <- LastEffort[Iarea,Istock,Ifleet]*Multipliers[Ipnt] }
        else
         FullF[Iarea,Istock,Ifleet] <- Stock[[Istock]]$AveF[Iarea,Ifleet]
      } # Istock
     FullFPass <<- FullF
 
    # Now project the catch
    Obj0 <- 0; Obj1 <- 0
    for (Istock in 1:Nstocks)
     {
      # Set the maximum age, number of sexes, number of length-classes, and the year (given each stock starts in different year)
      Iyear <- Stock[[Istock]]$Nhist+Jyear
      MaxAge <- Stock[[Istock]]$MaxAge
      Nsex <- Stock[[Istock]]$Nsex
      Nlen <- Stock[[Istock]]$Nlen

      ProjectTAC <- 0
      TargetTAC <- sum(TACs[Istock,])
      for (Iarea in 1:General$Narea)
       {
      
        # Set total mortality
        Fout <- FullF[Iarea,Istock,]
        Z <- matrix(Stock[[Istock]]$M[,,Iyear],nrow=Nsex,ncol=MaxAge)
        for (Ifleet in 1:General$Ncat_fleet) Z <- Z + Fout[Ifleet]*Stock[[Istock]]$SelAge[Ifleet,,,Iyear]
         
        # Compute catches
        CatchRetained <- rep(0,Ncat_fleet)
        for (Ifleet in 1:Ncat_fleet)
         {
          RawRetCatch <- array(0,dim=c(Nsex,MaxAge,Nlen)) 
          for (Isex in 1:Nsex)
           for (Iage in 1:MaxAge)
            {
             # Mortality from fishing (without selex)
             Temp <- Stock[[Istock]]$N[Iarea,Isex,Iage,Iyear]*(Fout[Ifleet]+1.0e-20)/Z[Isex,Iage]*(1.0-exp(-Z[Isex,Iage]))
             # Total catch in numbers
             RawTotalCatchN <- Temp*Stock[[Istock]]$SelexAge[Isex,Ifleet,Iage,Iyear]*Stock[[Istock]]$FracLenM[Isex,Iage,Iyear,]*Stock[[Istock]]$SelSelLen[Isex,Ifleet,,Iyear]
             # Catch in numbers by retained and total
             RawRetCatch[Isex,Iage,] <- RawTotalCatchN*Stock[[Istock]]$RetSelAge[Isex,Ifleet,Iage,Iyear]*Stock[[Istock]]$RetSelLen[Isex,Ifleet,,Iyear]
             CatchRetained[Ifleet] <- CatchRetained[Ifleet] + sum(RawRetCatch[Isex,Iage,]*Stock[[Istock]]$WtLen[Isex,])
            } # Isex x Iarea
          } # Ifleet
        ProjectTAC <- ProjectTAC + sum(CatchRetained)
        #cat(Istock,Ifleet,CatchRetained,sum(CatchRetained),ProjectTAC,"\n")
      } # Iarea
      #cat(Istock,TargetTAC,ProjectTAC,"\n")
      TargetTACs[Istock] <<- TargetTAC
      EstTACs[Istock] <<- ProjectTAC
      
      # Can't exceed the TargestYAC
      if (ProjectTAC > TargetTAC) Obj1 <- Obj1 + General$Multispecies.Wght2*(ProjectTAC-TargetTAC)^4
      
      # Match the TAC (only key commericial species)
      if (Nstocks==1 || General$PGMSY.Primary.Species[Istock]==0)
       {
        Obj0 <- Obj0 + General$Multispecies.Wght1*(TargetTAC-ProjectTAC)^2
       }
      else
        TargetTACs[Istock] <<- -999
     } # Istock
    #print(TargetTACs)
    #print(General$PGMSY.Primary.Species)
    #AA
    
    # Change in effort penalty
    Obj2 <- 0
    for (Imult in 1:length(Multipliers)) Obj2 <- Obj2 + General$Multispecies.Wght3*(Multipliers[Imult]-1.0)^2
    
    # Save diagnostics
    General$Multi$Obj0[Jyear] <<- Obj0
    General$Multi$Obj1[Jyear] <<- Obj1
    General$Multi$Obj2[Jyear] <<- Obj2
    General$Multi$Multipliers[Jyear,] <<- Multipliers
    General$Multi$TargetTACs[Jyear,] <<- TargetTACs
    General$Multi$EstTACs[Jyear,] <<- EstTACs
    
    # Total objective function
    Obj <- Obj0 + Obj1 + Obj2
    #cat(Obj,Obj0,Obj1,Obj2,"\n")
    #AAAA
    return(Obj)
   } # Minimum

  # Extract the last effort
  LastEffort <- array(0,dim=c(General$Narea,General$Nstocks,General$Nfleet))
  for (Iarea in 1:General$Narea)
   for (Istock in 1:Nstocks)
    for (Ifleet in 1:General$Ncat_fleet)
    LastEffort[Iarea,Istock,Ifleet] <- Stock[[Istock]]$FullF[Iarea,Ifleet,Stock[[Istock]]$Nhist+Jyear-1]
 
  # Minimize
  Neffort <- sum(General$TACs.fleets=="Yes")
  Pars <- rep(-1,Narea*Neffort)

  ss <- nlminb(Pars,Minfun,control=list(eval.max=100))
  
  write("TargetTACs",file=DebugFile,append=T)
  write(TargetTACs,ncol=length(TargetTACs),file=DebugFile,append=T)
  write("EstTACs",file=DebugFile,append=T)
  write(EstTACs,ncol=length(EstTACs),file=DebugFile,append=T)
  write(c("Done multi Year: ",Jyear),ncol=2,file=DebugFile,append=T)
  cat("Done multi",Jyear,"\n")

  return(FullFPass)
 } # Solve.for.multispecies.F

# ===================================================================================================================================

SPR_ref <- function(Istock,Fmult=0,AveF,Iyear)
{

 Narea <- General$Narea
 Nfleet <- General$Nfleet
 Ncat_fleet <- General$Ncat_fleet
 MaxAge <- Stock[[Istock]]$MaxAge
 Nsex <- Stock[[Istock]]$Nsex

 
 # Initial conditions
 Neqn <- array(0,dim=c(Narea,Nsex,MaxAge)); SSBR <- rep(0,Narea); Catch <- rep(0,Narea)
 FF <- array(0,dim=c(Narea,Ncat_fleet)); Z <- array(0,dim=c(Narea,Nsex,MaxAge));

 for (Isex in 1:Nsex)
  {
   for (Iarea in 1:Narea)
    {
     # First age-class 
     Neqn[Iarea,,1] <- 1.0/Nsex*Stock[[Istock]]$Relative.Density[Iarea]

     # Compute F by age, sex and fleet and then Z by sex and age
     for (Iage in 1:MaxAge)
      {
       Z[Iarea,Isex,Iage] <- Stock[[Istock]]$M[Isex,Iage,1]; 
       for (Ifleet in 1:Ncat_fleet){  
        FF[Iarea,Ifleet] <- AveF[Iarea,Ifleet]*Fmult
        Z[Iarea,Isex,Iage] <- Z[Iarea,Isex,Iage] + FF[Iarea,Ifleet]*Stock[[Istock]]$SelAge[Ifleet,Isex,Iage,Iyear];
       } # Ifleet 
      } # Iage
    } # Iarea
   
   # Ages 2 to maxage (no plus group)
   for (Iage in 2:MaxAge)
     for (Iarea in 1:Narea)
     {
       Ntemp <- Neqn[Iarea,Isex,Iage-1] * exp(-Z[Iarea,Isex,Iage-1])
       for (Jarea in 1:Narea)
         Neqn[Iarea,Isex,Iage] <- Neqn[Iarea,Isex,Iage] + Stock[[Istock]]$Move[Iarea,Jarea,Iage-1]*Ntemp
     } # IAge x Iarea
   
   # Now for the plus-group
   VecIn <- Neqn[,Isex,MaxAge]
   Amove <- matrix(0,nrow=Narea,ncol=Narea)
   for (Iarea in 1:Narea)
    for (Jarea in 1:Narea) 
      Amove[Iarea,Jarea] <- Stock[[Istock]]$Move[Iarea,Jarea,MaxAge]* exp(-Z[Iarea,Isex,MaxAge])
   Amove2 <- -1*Amove
   for (Iarea in 1:Narea) Amove2[Iarea,Iarea] <- 1 - Amove[Iarea,Iarea]
   
   # Compute equilbrium
   Neqn[,Isex,MaxAge] <- as.vector(solve(Amove2) %*% VecIn)
   
   # Catch in weight
   for (Iage in 1:MaxAge)
    { 
     NN <- Neqn[Iarea,Isex,Iage]*(1.0-exp(-Z[Iarea,Isex,Iage]))/Z[Iarea,Isex,Iage]
     for (Ifleet in 1:Ncat_fleet)
      {
       # Total catch in numbers
       CatchN <- FF[Iarea,Ifleet]*Stock[[Istock]]$SelexAge[Isex,Ifleet,Iage,Iyear]*Stock[[Istock]]$FracLenM[Isex,Iage,Iyear,]*Stock[[Istock]]$SelSelLen[Isex,Ifleet,,Iyear]*NN 
       # Catch in numbers by retained and total
       CatchNF <- CatchN*Stock[[Istock]]$RetSelAge[Isex,Ifleet,Iage,Iyear]*Stock[[Istock]]$RetSelLen[Isex,Ifleet,,Iyear]
       Catch[Iarea] <- Catch[Iarea] + sum(CatchNF*Stock[[Istock]]$WtLen[Isex,])
      } # Ifleet
    } # Iage
   } # Isex
 
  # Initial Spawning biomass-per-recruit
  for (Iarea in 1:Narea) SSBR[Iarea] <- sum(Neqn[Iarea,1,]*Stock[[Istock]]$Fecundity[,1])

  Return.V <- NULL
  Return.V$Ninit <- Neqn
  Return.V$SSBR <- sum(SSBR)
  Return.V$CatR <- sum(Catch)
  Return.V$Z <- Z
  return(Return.V)
} # Get_initial

# -------------------------------------------------------------------------------------------------------------------------

 GetRec <- function(Istock,SPR,SPRF0) 
  {
   Steepness <- Stock[[Istock]]$Steep
   SR_type <- 2

   # Ricker
   if (SR_type==1)
    {
     Temp1 <- 0.8*log(SPRF0/SPR)/log(5.0*Steepness);
     Temp2 <- 1.0 - Temp1;
     Recr <- SSB0/SPR*Temp2;
    }
  
   # Beverton-Holt
   if (SR_type==2)
   {
    Temp1 <- 4*Steepness*SPR + (Steepness-1)*SPRF0;
    Temp2 <- (5*Steepness-1.0)*SPR;
    Recr <- Temp1/Temp2;
   }
  
  return(Recr);
}

# -------------------------------------------------------------------------------------------------------------------------

GetRefs <- function(Istock,AveF,Iyear,Target)
{
  SPRF0 <- SPR_ref(Istock,Fmult=0,AveF,Iyear)$SSBR

  # F such that SSB/R(F) = 0.4SSB/R(F=0)
  Fmult.min <- 0; Fmult.max <- 10
  for (II in 1:20)
   {
    Fmult <- (Fmult.min+Fmult.max)/2
    SPRF <- SPR_ref(Istock,Fmult=Fmult,AveF,Iyear)$SSBR
    if (SPRF < Target*SPRF0) Fmult.max <- Fmult else Fmult.min <- Fmult
  }
  Ftar_SPR <- Fmult
  #print(c("F40%",Fmult,SPRF/SPRF0,Fmult.min,Fmult.max))

  # F such that SSB(F) = 0.4SSB(F=0)
  Fmult.min <- 0; Fmult.max <- 10
  for (II in 1:20)
   {
    Fmult <- (Fmult.min+Fmult.max)/2
    SPRF <- SPR_ref(Istock,Fmult=Fmult,AveF,Iyear)$SSBR
    Recr <- GetRec(Istock,SPRF,SPRF0)
    SPRF <- SPRF*Recr
    if (SPRF < Target*SPRF0) Fmult.max <- Fmult else Fmult.min <- Fmult
  }
  Ftar_SSB <- Fmult
  #print(c("F40",Fmult,SPRF/SPRF0,Fmult.min,Fmult.max))
  
  # FMSY
  Fmult.min <- 0; Fmult.max <- 100
  for (II in 1:20)
   {
    Fmult <- (Fmult.min+Fmult.max)/2
    Call1 <- SPR_ref(Istock,Fmult=Fmult+0.0001,AveF,Iyear)
    SPRF <- Call1$SSBR
    CatF <- Call1$CatR
    Recr <- GetRec(Istock,SPRF,SPRF0)
    Catch1 <- (CatF*Recr)
    Call2 <- SPR_ref(Istock,Fmult=Fmult-0.0001,AveF,Iyear)
    SPRF <- Call2$SSBR
    CatF <- Call2$CatR
    Recr <- GetRec(Istock,SPRF,SPRF0)
    Catch2 <- (CatF*Recr)
    Deriv <- (Catch1-Catch2)
    if (Deriv < 0) Fmult.max <- Fmult else Fmult.min <- Fmult
  } 
  FMSY <- Fmult
  #print(c("FMSY",Fmult,SPRF/SPRF0,Deriv,Fmult.min,Fmult.max))
  
  for (II in 0:200)
  {
   #Fmult <- (II/100)
   #Call2 <- SPR_ref(Istock,Fmult=Fmult,AveF,Iyear)
   #SPRF <- Call2$SSBR
   #Recr <- GetRec(Istock,SPRF,SPRF0)
   #SPRF <- Call2$SSBR*Recr
   #CatF <- Call2$CatR*Recr
   #cat(Fmult,SPRF/SPRF0,CatF,"\n")
   }
  
  Outs <- NULL
  Outs$Ftar_SPR <- Ftar_SPR
  Outs$Ftar_SSB <- Ftar_SSB
  Outs$FMSY <- FMSY
  return(Outs)
}

# ===================================================================================================================================
# ===================================================================================================================================

Do.Project <- function(Isim)
 {
  Narea <- General$Narea
  Nstocks <- General$Nstocks
  Nfleet <- General$Nfleet
  Ncat_fleet <- General$Ncat_fleet

  # Get initial stuff
  Get.Initial <- function(Istock)
   {
    MaxAge <- Stock[[Istock]]$MaxAge
    Nsex <- Stock[[Istock]]$Nsex
    Narea <- General$Narea
    
    # Initial conditions
    Ninit <- array(0,dim=c(Narea,Nsex,MaxAge)); SSB0 <- rep(0,Narea)
    for (Isex in 1:Nsex)
     {
      # First age-class
      for (Iarea in 1:Narea)
       Ninit[Iarea,Isex,1] <- Stock[[Istock]]$R00[Iarea]/Nsex
      
      # Ages 2 to maxage (no plus group)
      for (Iage in 2:MaxAge)
        for (Iarea in 1:Narea)
         {
          Ntemp <- Ninit[Iarea,Isex,Iage-1] * exp(-Stock[[Istock]]$M[Isex,Iage-1,1])
          for (Jarea in 1:Narea)
           Ninit[Jarea,Isex,Iage] <- Ninit[Jarea,Isex,Iage] + Stock[[Istock]]$Move[Iarea,Jarea,Iage-1]*Ntemp
        } # IAge x Iarea
      
      # Now for the plus-group
      VecIn <- Ninit[,Isex,MaxAge]
      Amove <- t(matrix(Stock[[Istock]]$Move[,,MaxAge]* exp(-Stock[[Istock]]$M[Isex,MaxAge,1]),nrow=Narea,ncol=Narea))
      Amove2 <- -1*Amove
      for (Iarea in 1:Narea) Amove2[Iarea,Iarea] <- 1 - Amove[Iarea,Iarea]

      # Compute equilbrium
      Ninit[,Isex,MaxAge] <- as.vector(solve(Amove2) %*% VecIn)
      Nout <- Ninit[,Isex,MaxAge]

      # Check that the equilibrium is corrct
      Ntst <-  as.vector(VecIn + Amove%*%Nout)
     } # Isex

    # Unfished SSB/R
    for (Iarea in 1:Narea) SSB0[Iarea] <- sum(Ninit[Iarea,1,]*Stock[[Istock]]$Fecundity[,1])
    
#    print(Ninit[,1,])
#    print(Ninit[,2,])
#    AA
    
    Return.V <- NULL
    Return.V$Ninit <- Ninit
    Return.V$SSB0 <- SSB0
    return(Return.V)
   } # Get_initial

# -------------------------------------------------------------------------------------------------------------------------
  
  # Set up equilibrium age-structure
  for (Istock in 1:Nstocks)
   {
    MaxAge <- Stock[[Istock]]$MaxAge; Nsex <- Stock[[Istock]]$Nsex;  Initial <- Get.Initial(Istock)
    for (Iarea in 1:Narea)
     {
      Stock[[Istock]]$N[Iarea,,,1] <<- Initial$Ninit[Iarea,,]
      Stock[[Istock]]$SSB0[Iarea] <<- Initial$SSB0[Iarea]
      # Adjust by early devs
      for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,1:MaxAge,1] <<- Stock[[Istock]]$N[Iarea,Isex,1:MaxAge,1]*exp(Stock[[Istock]]$EarlyDevs[Iarea,1:MaxAge])
      for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,1,1] <<- Stock[[Istock]]$R0[Iarea,1]*exp(Stock[[Istock]]$EarlyDevs[Iarea,1])/Nsex
      Stock[[Istock]]$Exp.Recr[Iarea,1] <<- Nsex*Stock[[Istock]]$R0[Iarea,1]
      # SSB does not include age=0
      Stock[[Istock]]$SSB[Iarea,1] <<- sum(Stock[[Istock]]$N[Iarea,1,-1,1]*Stock[[Istock]]$Fecundity[-1,1])
      Stock[[Istock]]$SSBSex[1,Iarea,1] <<- sum(Stock[[Istock]]$N[Iarea,1,-1,1]*Stock[[Istock]]$Fecundity[-1,1])
      if (Nsex > 1) Stock[[Istock]]$SSBSex[Nsex,Iarea,1] <<- sum(Stock[[Istock]]$N[Iarea,Nsex,-1,1]*Stock[[Istock]]$Fecundity[-1,1])
      Stock[[Istock]]$Depletion[Iarea,1] <<- Stock[[Istock]]$SSB[Iarea,1]/(Stock[[Istock]]$SSB0[Iarea]*Stock[[Istock]]$R0[Iarea,1]/Stock[[Istock]]$R00[Iarea])
      Depl <- Stock[[Istock]]$Depletion[Iarea,1]
      
      # Recruitment for year 1
      Recr <- 4.0 *Stock[[Istock]]$R0[Iarea,1]*Stock[[Istock]]$Steep*Depl / ( (1-Stock[[Istock]]$Steep) + (5*Stock[[Istock]]$Steep-1)*Depl)
      Stock[[Istock]]$Exp.Recr[Iarea,1] <<- Recr
      Recr <- Recr*exp(Stock[[Istock]]$EarlyDevs[Iarea,1])
      for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,1,1] <<- Recr/Nsex

      #for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,1,1] <<- Stock[[Istock]]$R0[Iarea,1]*exp(Stock[[Istock]]$EarlyDevs[Iarea,1])/Nsex
      Stock[[Istock]]$BREF[Iarea,1] <<- sum(Stock[[Istock]]$N[Iarea,,-1,1]*Stock[[Istock]]$MeanWtAtAgeS[,-1,1])
      Stock[[Istock]]$Recr[Iarea,1] <<- sum(Stock[[Istock]]$N[Iarea,,1,1])
    } # Iarea
    Stock[[Istock]]$Ninitial <<- Initial$Ninit
  } # Istock
  
  # Set up equilibrium age-structure
  Stock[[Istock]]$Finitial <<- matrix(0,nrow=Narea,ncol=General$Ncat_fleet)
  for (Istock in 1:Nstocks)
   if (sum(Stock[[Istock]]$Equilbrium.catch) > 0)
    {
     cat("Doing initial catch\n")
     MaxAge <- Stock[[Istock]]$MaxAge; Nsex <- Stock[[Istock]]$Nsex
     Initial <- Get.Initial(Istock)
     for (Iarea in 1:Narea) Stock[[Istock]]$N[Iarea,,,1] <<- Initial$Ninit[Iarea,,]
     TACsOrig <- matrix(Stock[[Istock]]$Equilbrium.catch,General$Ncat_fleet,ncol=50)
     
     Iyear <- 99999999
     for (Jyear in 1:150)
      {
       # Solve for F
       if (Stock[[Istock]]$Use_init_F=="F")
        {
         print("Not checked yet!!!!"); exit
         if (Stock[[Istock]]$CatchType==1) Fout <- HybridA(Istock,1,Stock[[Istock]]$Equilbrium.catch,Initial.Eqn=F)
         if (Stock[[Istock]]$CatchType==2) Fout <- Stock[[Istock]]$Equilbrium.catch
       }
       if (Stock[[Istock]]$Use_init_F=="T") Fout <- matrix(Stock[[Istock]]$Set_initial_F[1:Ncat_fleet],nrow=Narea,ncol=Ncat_fleet)
       
       # Calculate Z an project catches
       NtempZ <- array(0,dim=c(Narea,Nsex,MaxAge))
       for (Iarea in 1:Narea)
        {
         # Set total mortality
         Z <- matrix(Stock[[Istock]]$M[,,1],nrow=Nsex,ncol=MaxAge)
         for (Ifleet in 1:General$Ncat_fleet) Z <- Z + Fout[Iarea,Ifleet]*Stock[[Istock]]$SelAge[Ifleet,,,1]

         # Survivors from total mortality
         for (Isex in 1:Nsex)
          for (Iage in 1:MaxAge)
           NtempZ[Iarea,Isex,Iage] <-Stock[[Istock]]$N[Iarea,Isex,Iage,1] * exp(-Z[Isex,Iage]) 
        } # Iarea
       
       # Initial update 
       Stock[[Istock]]$N[,,,2] <<- 0
       for (Isex in 1:Nsex)
        for (Iage in 2:MaxAge)
         for (Iarea in 1:Narea)
          {
           Ntemp <- NtempZ[Iarea,Isex,Iage-1]
           for (Jarea in 1:Narea)
            Stock[[Istock]]$N[Jarea,Isex,Iage,2] <<- Stock[[Istock]]$N[Jarea,Isex,Iage,2] + Stock[[Istock]]$Move[Iarea,Jarea,Iage-1]*Ntemp
          } # Isx, Iage x Iarea
         
       # project forward (including a plus-group)
       Iage <- MaxAge
       for (Isex in 1:Nsex)
        for (Iarea in 1:Narea) 
         {
          # Plus-group
          Ntemp <- NtempZ[Iarea,Isex,Iage]
          for (Jarea in 1:Narea)
           Stock[[Istock]]$N[Jarea,Isex,Iage,2] <<- Stock[[Istock]]$N[Jarea,Isex,Iage,2] + Stock[[Istock]]$Move[Iarea,Jarea,Iage]*Ntemp
         }

       # Calculate SSB
       for (Iarea in 1:Narea)
        {
         SSB.y <- sum(Stock[[Istock]]$N[Iarea,1,-1,2]*Stock[[Istock]]$Fecundity[-1,1])
         Depl <- SSB.y/Stock[[Istock]]$SSB0[Iarea]
         Stock[[Istock]]$Depletion[Iarea,1] <<- Depl
         if (Stock[[Istock]]$Use_Steep_in_equ==0) Recr <- Stock[[Istock]]$R00[Iarea]
         if (Stock[[Istock]]$Use_Steep_in_equ==1) Recr <- 4.0 *Stock[[Istock]]$R00[Iarea]*Stock[[Istock]]$Steep*Depl / ( (1-Stock[[Istock]]$Steep) + (5*Stock[[Istock]]$Steep-1)*Depl)
         for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,1,2] <<- Recr/Nsex
         for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,,1] <<- Stock[[Istock]]$N[Iarea,Isex,,2]
        } # Iarea
      } # Jyear     
     
     # Final adjust
     for (Iarea in 1:Narea)
      {
       #print(Stock[[Istock]]$N[Iarea,,,1] )
       Depl <- Stock[[Istock]]$Depletion[Iarea,1]
       Recr <- 4.0 *Stock[[Istock]]$R00[Iarea]*Stock[[Istock]]$Steep*Depl / ( (1-Stock[[Istock]]$Steep) + (5*Stock[[Istock]]$Steep-1)*Depl)
       # Adjust by early devs
       if (Stock[[Istock]]$Early.R0.mult_end_yrs[1]!=0) 
        for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,1:MaxAge,1] <<- Stock[[Istock]]$N[Iarea,Isex,1:MaxAge,1]*exp(Stock[[Istock]]$Early.R0.mult+Stock[[Istock]]$EarlyDevs[1:MaxAge])
       if (Stock[[Istock]]$Early.R0.mult_end_yrs[1]==0) 
         for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,1:MaxAge,1] <<- Stock[[Istock]]$N[Iarea,Isex,1:MaxAge,1]*exp(Stock[[Istock]]$EarlyDevs[1:MaxAge])
       if (Stock[[Istock]]$Use_Steep_in_equ==0)
        {
         for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,1,1] <<- Stock[[Istock]]$R0[1]*exp(Stock[[Istock]]$EarlyDevs[1])/Nsex
         Stock[[Istock]]$Exp.Recr[Iarea,1] <<- Stock[[Istock]]$R0[1]
        }
       if (Stock[[Istock]]$Use_Steep_in_equ==1)
        {
         for (Isex in 1:Nsex) Stock[[Istock]]$N[Iarea,Isex,1,1] <<- Recr/Nsex*exp(Stock[[Istock]]$EarlyDevs[1])
         Stock[[Istock]]$Exp.Recr[Iarea,1] <<- Recr
        }
       # SSB does not include age=0
       Stock[[Istock]]$SSB[Iarea,1] <<- sum(Stock[[Istock]]$N[Iarea,1,-1,1]*Stock[[Istock]]$Fecundity[-1,1])
       Stock[[Istock]]$SSBSex[1,Iarea,1] <<- sum(Stock[[Istock]]$N[Iarea,1,-1,1]*Stock[[Istock]]$Fecundity[-1,1])
       if (Nsex > 1) Stock[[Istock]]$SSBSex[Nsex,Iarea,1] <<- sum(Stock[[Istock]]$N[Iarea,Nsex,-1,1]*Stock[[Istock]]$Fecundity[-1,1])
       Stock[[Istock]]$BREF[Iarea,1] <<- sum(Stock[[Istock]]$N[Iarea,,-1,1]*Stock[[Istock]]$MeanWtAtAgeS[,-1,1])
       Stock[[Istock]]$Recr[Iarea,1] <<- sum(Stock[[Istock]]$N[Iarea,,1,1])
       Stock[[Istock]]$Finitial[Iarea,] <<- Fout[Iarea,]
       #print(Stock[[Istock]]$N[Iarea,,,1] )
     } # Iarea
   } # Set up equilibrium age-structure
 
  
  # Project from year 1 to Nhist+1
  for (Istock in 1:Nstocks)
   {
    for (Iyear in 1:Stock[[Istock]]$Nhist) 
     {
      if (FullOutput==T) print(c("Year",Istock,Iyear,Stock[[Istock]]$Nhist))
      # Apply Hybrid (or not)
      if (Stock[[Istock]]$CatchType==1) Fout <- HybridA(Istock,Iyear,Stock[[Istock]]$CatchInp[Iyear,])
      if (Stock[[Istock]]$CatchType==2) Fout <- Stock[[Istock]]$CatchInp[Iyear,]
      if (Stock[[Istock]]$CatchType==3) Fout <- Stock[[Istock]]$F.Selex.Inp[,,Iyear]
      Fout <- matrix(Fout,nrow=Narea,ncol=Ncat_fleet)
      for (Iarea in 1:Narea) for (Ifleet in 1:Ncat_fleet) Stock[[Istock]]$FullF[Iarea,Ifleet,Iyear] <<- Fout[Iarea,Ifleet]
      Project.One.Ahead(Istock,Iyear)
     }
   }

  # Find the average F for the last 5 years (AEP)
  # if (General$MultiStock.Version=="Yes")
   for (Istock in 1:Nstocks)
    {
     AveF <- matrix(0,nrow=Narea,ncol=Ncat_fleet)
     for (Iarea in 1:Narea)
      for (Ifleet in 1:Ncat_fleet)
       for (Jyear in (Stock[[Istock]]$Nhist-4):(Stock[[Istock]]$Nhist))
        AveF[Iarea,Ifleet] <- AveF[Iarea,Ifleet] + Stock[[Istock]]$UseFleet[Iarea,Ifleet,Jyear]*Stock[[Istock]]$FullF[Iarea,Ifleet,Jyear]/5.0  
     Stock[[Istock]]$AveF <<- AveF
    } # Istock
    
  #for (Istock in 1:Nstocks)
  #  GetRefs(Istock,AveF,Iyear=Stock[[Istock]]$Nhist)
  #AA
  
  #print(Stock[[Istock]]$SSB)
  print("starting projection")

  # Now project
  
  MaxAssArea <- 0; for (Istock in 1:General$Nstocks) if (Stock[[Istock]]$NassArea >MaxAssArea ) MaxAssArea <-Stock[[Istock]]$NassArea
  RBCsOrig <- array(0,dim=c(Nstocks,MaxAssArea,50))
  TACsAdjust1 <- array(0,dim=c(Nstocks,MaxAssArea,Ncat_fleet,50))
  TACs <- array(0,dim=c(Nstocks,MaxAssArea,Ncat_fleet,50)); TAC.length <- 50
  if (General$Do.any.projections==T)
  for (Jyear in 1:General$Nproj)
   {
    
    cat("\n running year",Jyear,"\n",file=DebugFile,append=T)
 
    # Run the assessment and apply the control rules
    for (Istock in 1:Nstocks)
     {
      Is.do.assessment <- Should.do.assessment(Jyear,General$AssFreq,Istock)
      if (FullOutput==T) cat("Do assessment",Istock," ",Jyear," ",General$AssFreq[Istock]," ",Is.do.assessment,"\n")
      write(paste0("Do assessment",Istock," ",Jyear," ",General$AssFreq[Istock]," ",Is.do.assessment),file=DebugFile,append=T)
      # Create a TAC for the future
      if (Is.do.assessment==T) 
       {
        # Run an assessment (based on data to the end of the previous year)
        TACs[Istock,,,] <- 0
        for (IassArea in 1:Stock[[Istock]]$NassArea)
         {
          # Base RBCs from the assessment
          RBCsOrig[Istock,IassArea,] <- Run_Assessment(Isim,Istock,IassArea,Stock[[Istock]]$Nhist+Jyear-1)
          # Adjust the RBC to compute a TAC
          if(Stock[[Istock]]$Ass$TAC.management=="Yes")
           {
            #Adjust the RBCs to compute a retained only RBC
            write(RBCsOrig[Istock,IassArea,],ncol=length(RBCsOrig[Istock,IassArea,]),file=DebugFile,append=T)
            Outs <- AdjustTACS(Isim,RBCsOrig[Istock,IassArea,],IassArea,Istock,Stock[[Istock]]$Nhist+Jyear-1)
            write(Outs$TACs.ret,ncol=length(Outs$TACs.ret),file=DebugFile,append=T)
            write(Outs$TACs.post.Buffer,ncol=length(Outs$TACs.post.Buffer),file=DebugFile,append=T)
            write(Outs$TACs.constain,ncol=length(Outs$TACs.constain),file=DebugFile,append=T)
            for (Kyear in 1:50)
             if (Jyear+Kyear-1 <= General$Nproj)  
              {
               Stock[[Istock]]$TACsStep[1,Stock[[Istock]]$Nhist+Jyear+Kyear-1] <<- Outs$TACs.ret[Kyear]
               Stock[[Istock]]$TACsStep[2,Stock[[Istock]]$Nhist+Jyear+Kyear-1] <<- Outs$TACs.post.Buffer[Kyear]
               Stock[[Istock]]$TACsStep[3:6,Stock[[Istock]]$Nhist+Jyear+Kyear-1] <<- Outs$TACs.constain[Kyear]
             } 
            if (General$Indicator.Last.TAC.Code[Istock]==3) General$Indicator.Last.TAC[Istock] <<- Outs$TACs.constain[1]
            TACsAdjust1[Istock,IassArea,,] <- Outs$TACs.final
            Stock[[Istock]]$RBC[Stock[[Istock]]$Nhist+Jyear] <<- sum(RBCsOrig[Istock,,1])
           }
          else
           TACsAdjust1[Istock,IassArea,,] <- TACsOrig[Istock,IassArea,]
          TACs[Istock,IassArea,,] <- TACsAdjust1[Istock,IassArea,,]
         } # IassArea  
        Stock[[Istock]]$Last.Assessmnent <<- Stock[[Istock]]$Nhist+Jyear
        } # Did assessment an assessment
      else
       {
        # Roll over the TAC 
        for (IassArea in 1:Stock[[Istock]]$NassArea)
         for (Ifleet in 1:General$Ncat_fleet)
          for (Kyear in 1:49)
           TACs[Istock,IassArea,Ifleet,Kyear] <- TACs[Istock,IassArea,Ifleet,Kyear+1]
        } # else
     } # Istock
    

    # Apply PGMSY adjustment (If appropriate)
    if (General$MultiStock.Version=="Yes" & General$PGMSY.Use == "Yes")
     {
      # Apply PGMSY to adjust the TACs
      TACs <- PGMSY(TACs,Jyear)  
      for (Istock in 1:General$Nstocks)
      {
        Iyear <- Stock[[Istock]]$Nhist+Jyear
        Stock[[Istock]]$TACsStep[4:6,Iyear] <<- sum(TACs[Istock,,,1])
      }
     } #Apply PGMSY adjustment 

    # Apply Indicator species adjustment (If appropriate)
    if (General$MultiStock.Version=="Yes")
     {
      # Apply Indicator species approach to adjust the TACs
      TACs <- Indicator.species.TACS(TACs,Jyear)  
      for (Istock in 1:General$Nstocks)
      {
        Iyear <- Stock[[Istock]]$Nhist+Jyear
        Stock[[Istock]]$TACsStep[5:6,Iyear] <<- sum(TACs[Istock,,,1])
      }
     } # Apply Indicator species adjustment

    # Apply Caitlin's strategies
    if (General$MultiStock.Version=="Yes" & General$Caitlin == "Yes")
     {
      # Apply PGMSY to adjust the TACs
      #print(round(TACs[,,,1],0))
     }

    # Add implementation error
    if(Stock[[Istock]]$Ass$TAC.management=="Yes")
     for (Istock in 1:Nstocks)
      if (Stock[[Istock]]$Use.Implementation.Error!=0)
       {
        if (Stock[[Istock]]$Implementation.Error.Specs[1]==1)
         for (IassArea in 1:Stock[[Istock]]$NassArea)
          TACs[Istock,IassArea,,1] <- TACs[Istock,IassArea,,1]*rbeta(1,Stock[[Istock]]$Implementation.Error.Specs[2],Stock[[Istock]]$Implementation.Error.Specs[3])
        Iyear <- Stock[[Istock]]$Nhist+Jyear
        Stock[[Istock]]$TACsStep[6,Iyear] <<- Stock[[Istock]]$TACs[Iyear]
       } # If
    
    for(Istocks in 1:Nstocks)
     for (IassArea in 1:Stock[[Istock]]$NassArea)
       {
        Alloc.vals <- Stock[[Istock]]$Ass$Relallocation.vals+1.0e-10
        # Use the pre-specified allocation
        if (Stock[[Istock]]$Ass$Relallocation==1)
         {
          TACs[Istock,IassArea,,1] <- sum(TACs[Istock,IassArea,,1])*Alloc.vals/sum(Alloc.vals)
         }
        # Allocate based on weighting and allocated TACs (can be used to close fisheries)
        if (Stock[[Istock]]$Ass$Relallocation==2)
         {
          TACs[Istock,IassArea,,1] <- 1.0e-10+sum(TACs[Istock,IassArea,,1]) * Alloc.vals*TACs[Istock,IassArea,,1]/sum(Alloc.vals*TACs[Istock,IassArea,,1]+1.0e-10)
         }
     }
    

    # Save the final TAC
    for (Istock in 1:Nstocks) Stock[[Istock]]$Final.TAC[Stock[[Istock]]$Nhist+Jyear] <<- sum(TACs[Istock,,,1])
      
    # Solve for F by fleet and stock (depends on version of the code)
    if (General$MultiStock.Version=="No")
     {
      for (Istock in 1:General$Nstocks)
       {
        Iyear <- Stock[[Istock]]$Nhist+Jyear
        if (Stock[[Istock]]$Ass$TAC.management=="Yes")
         {
          print(TACs[Istock,,,1])
          TAC.mat <- matrix(TACs[Istock,,,1],nrow=Stock[[Istock]]$NassArea,ncol=Ncat_fleet,byrow=T)
          TheCat <- apply(TAC.mat,2,sum)
          Fout <- HybridA(Istock,Iyear,TheCat,Use.Profit.constraint=General$Use.Profit.constraint)
         }
        else
          Fout <- Stock[[Istock]]$Ass$F.fix 
         for (Iarea in 1:Narea) for (Ifleet in 1:Ncat_fleet) Stock[[Istock]]$FullF[Iarea,Ifleet,Iyear] <<- Fout[Iarea,Ifleet]
       }
     } # Solve for F by fleet and stock (depends on version of the code)
    
        
    # Solve for F by fleet and stock (depends on version of the code)
    if (General$MultiStock.Version=="Yes")
     {
      if(Stock[[Istock]]$Ass$TAC.management=="No") {print("Not coded"); stopping}
      TAC_mult <- matrix(0,nrow=General$Nstocks,ncol=General$Ncat_fleet)
      for (Istock in 1:General$Nstocks)
       for (Ifleet in 1:General$Ncat_fleet)
        TAC_mult[Istock,Ifleet]<- sum(TACs[Istock,,Ifleet,1])
      print("next step in multispecies")
      FullFpass <- Solve.for.multispecies.F(Jyear,TAC_mult)
      for (Istock in 1:General$Nstocks) 
       for (Iarea in 1:Narea) for (Ifleet in 1:Ncat_fleet) 
        {
         Iyear <- Stock[[Istock]]$Nhist+Jyear
         Stock[[Istock]]$FullF[Iarea,Ifleet,Iyear] <<- FullFpass[Iarea,Istock,Ifleet]
        }
     } # Solve for F by fleet and stock (depends on version of the code)
    
    # Save TACS
    for (Istock in 1:General$Nstocks)
     {
      Iyear <- Stock[[Istock]]$Nhist+Jyear
      Stock[[Istock]]$TACs[Iyear] <<- sum(TACs[Istock,,,1])
     }
    
    # update the population dynamics
    for (Istock in 1:General$Nstocks) Project.One.Ahead(Istock,Stock[[Istock]]$Nhist+Jyear)
  }
  #print(Stock[[1]]$SSB)
  #print(Stock[[1]]$SSB/Stock[[1]]$SSB0*100)
  #if (General$Nstocks==2) print(Stock[[2]]$SSB)
  if (General$Nstocks==2) print(Stock[[2]]$SSB/Stock[[2]]$SSB0*100)
 } # Do.project

# =======================================================================================================================
Should.do.assessment <- function(Jyear,AssFreq,Istock)
 {
  
  # This function checks whether an assessment is due
  
  Is.Assessment <- F  
  if ((Jyear-1) %% AssFreq[Istock]==0 & General$Indicator.Species[Istock] == 0) Is.Assessment <- T
  if (Jyear==1 & General$Indicator.Species[Istock] != 0) Is.Assessment <- T
  return(Is.Assessment)
 }

# ======================================================================================================================

AdjustTACS <- function(Isim,RBCs,IassArea,Istock,Iyear)
 {
  
  # This routine has several steps
  # 1. Adjust the catch by the discard rates 
  # 2. Check on year-to-year changes in TAC
  # 3. Impose the minimum TAC
  # 4. Split to fleets
  
  # Extract pinters
  Nhist <- Stock[[Istock]]$Nhist

  # Compute retained to total catch for the last five years by assessment area
  OFL.to.TAC.byfleet <- rep(0,General$Ncat_fleet)
  FleetsToAreas <- Stock[[Istock]]$FleetsToAreas
  RelFleetCat <- rep(0,General$Ncat_fleet)
  for (Ifleet in 1:General$Ncat_fleet)
   {
    TotalRetain <-0; TotalTotal <- 0 
    for (Jyear in (Iyear-4):(Iyear))
     {
      Use <- which(FleetsToAreas==IassArea)
      for (Iarea in Use)
       {
        TotalRetain <- TotalRetain + Stock[[Istock]]$CatchRetained[Iarea,Ifleet,Jyear]
        RelFleetCat[Ifleet] <- RelFleetCat[Ifleet] + Stock[[Istock]]$CatchRetained[Iarea,Ifleet,Jyear]
        TotalTotal <- TotalTotal + Stock[[Istock]]$CatchTotal[Iarea,Ifleet,Jyear]   
       }
     }
    if (TotalTotal>0) OFL.to.TAC.byfleet[Ifleet] <- TotalRetain/TotalTotal
   }
  if (sum(RelFleetCat)>0) RelFleetCat <- RelFleetCat/sum(RelFleetCat)   # Split of TAC to fleet
  
  # Compute the TAC by fleet (which is in retained catch) by spliting the TAC to fleet then adjusting for retained to total catch
  TACs.ret <- array(0,dim=c(General$Ncat_fleet,length(RBCs)))
  for (Jyear in 1:length(RBCs))
   for (Ifleet in 1:General$Ncat_fleet) TACs.ret[Ifleet,Jyear] <- RBCs[Jyear]*RelFleetCat[Ifleet]*OFL.to.TAC.byfleet[Ifleet]

  # Check on the year-to-year in RBC
  TACs.orig <- apply(TACs.ret,2,sum)

  TACs.post.Buffer <- SetBuffer(Istock,TACs.orig) 

  TACs.constrain <- TACs.post.Buffer
  for (Jyear in 1:length(TACs.orig))
   {  
    if (Isim==19) print(TACs.constrain[Jyear])
    if (Iyear==Nhist & Jyear==1) 
     # This is the first TAC  
     Last.TAC <- Stock[[Istock]]$Ass$Last.TAC
    else
     {
      if (Jyear==1)
       Last.TAC <- Stock[[Istock]]$TACs[Iyear]  
      else
       Last.TAC <- TACs.constrain[Jyear-1]  
     }
    # Only continue if the last TAC was not zero or negative
    if (Last.TAC > 0)
     {
      if (Stock[[Istock]]$Ass$Beta1 >0 )
       if (TACs.constrain[Jyear] < (1-Stock[[Istock]]$Ass$Beta1)*Last.TAC) TACs.constrain[Jyear] <- (1-Stock[[Istock]]$Ass$Beta1)*Last.TAC
      if (Stock[[Istock]]$Ass$Beta2 >0 )
        if (TACs.constrain[Jyear] > (1+Stock[[Istock]]$Ass$Beta2)*Last.TAC) TACs.constrain[Jyear] <- (1+Stock[[Istock]]$Ass$Beta2)*Last.TAC
      if (TACs.constrain[Jyear] < Stock[[Istock]]$Ass$Min.TAC) TACs.constrain[Jyear] <- Stock[[Istock]]$Ass$Min.TAC
     }
  }
  
  # Now convert back into fleets
  TACs.final <-  array(0,dim=c(General$Ncat_fleet,length(RBCs)))
  for (Jyear in 1:length(TACs.orig))
   TACs.final[,Jyear] <- TACs.constrain[Jyear]*(TACs.ret[,Jyear]+1.0e-10)/sum(TACs.ret[,Jyear]+1.0e-10)
  
  # Now return
  Outs <- NULL
  Outs$RBCs <- RBCs                                      # What came in
  Outs$TACs.ret <- TACs.orig                             # Post retention correction
  Outs$TACs.post.Buffer <- TACs.post.Buffer              # Post Buffer adjust
  Outs$TACs.constain <- TACs.constrain                   # Post catch constraints
  Outs$TACs.final <- TACs.final                          # Final TACs by fleet and year
  return(Outs)    
 }

# =======================================================================================================================
# =======================================================================================================================

Read.General.File <- function(GeneralFile,RunNo)
 {
  # Clean up the folder structure
  cat("Deleting temp folders\n")
  Folder <- paste0(Path,paste0("Temp/"))
  if (!dir.exists(Folder)) dir.create(Folder)  
  RunFolder <- paste0(Path,paste0("Temp/Assess_temp_",RunNo,"/"))
  if (dir.exists(RunFolder)) unlink(RunFolder,recursive=T)
  dir.create(RunFolder)  
  
  # General specifications
  FileName <- paste0(Path,Input.Folder,GeneralFile)   
  print(FileName)
  
  GenInput <- read.table(FileName,header=F,col.names=paste0("C",1:100),fill=T,comment="?")

  # Run designator
  Index <- which(GenInput[,2]=="Run" & GenInput[,3]=="designator");CheckError(Index,"Run designator") 
  Run_designator <- GenInput[Index+1,1]

  # Spcies and stocks
  Index <- which(GenInput[,2]=="Number" & GenInput[,4]=="species")
  Nspec <- as.numeric(GenInput[Index+1,1])
  Index <- which(GenInput[,2]=="Stocks-per-species")
  Stocks <- as.numeric(GenInput[Index+1,1:Nspec])
  Nstocks <- sum(Stocks)
  # Stock names
  Index <- which(GenInput[,2]=="Species/stock" & GenInput[,3]=="names")
  Stock.Names <- rep(NA,Nstocks)
  for (Istock in 1:Nstocks) Stock.Names[Istock] <- GenInput[Index+Istock,1]
  
  # Areas
  Index <- which(GenInput[,2]=="Number" & GenInput[,4]=="areas")
  Narea <- as.numeric(GenInput[Index+1,1])
  
  # Fleets
  Index <- which(GenInput[,1]=="#Number_of_catch_fleets")
  Ncat_fleet <- as.numeric(GenInput[Index+1,1])
  Index <- which(GenInput[,1]=="#Number_of_survey_fleets")
  Nsuv_fleet <- as.numeric(GenInput[Index+1,1])
  Nfleet <- Ncat_fleet + Nsuv_fleet
  Index <- which(GenInput[,1]=="#Fleet.names")
  Fleet.Names <- rep(NA,Nfleet)
  for (Ifleet in 1:Nfleet) Fleet.Names[Ifleet] <- GenInput[Index+1,Ifleet]

  # Other general items
  FirstProjYr <- 1
  Index <- which(GenInput[,1]=="#First_projection_year"); FirstProjYr <- as.numeric(GenInput[Index,2])
  Index <- which(GenInput[,1]=="#Number_of_simulations"); Nsim <- as.numeric(GenInput[Index,2])
  Index <- which(GenInput[,1]=="#Number_of_projection_years"); Nproj <- as.numeric(GenInput[Index,2])
  Index <- which(GenInput[,1]=="#Assessment_frequency"); AssFreq <- rep(1,Nstocks)
  for (Istock in 1:Nstocks) AssFreq[Istock]  <- as.numeric(GenInput[Index+Istock,1]) 
  Index <- which(GenInput[,1]=="#TestCase"); TestCase  <- GenInput[Index,2]
  if (Estimation.test==0) TestCase <- TRUE
  if (Estimation.test==0) TestCase <<- TRUE
  Index <- which(GenInput[,1]=="#TACs_apply_to_fleets"); TACs.fleets  <- GenInput[Index+1,1:Nfleet]
  Index <- which(GenInput[,1]=="#Multispecies"); MultiStock.Version  <- GenInput[Index+1,1]
  Multispecies.Wght1 <- 0; Multispecies.Wght2 <- 0; Multispecies.Wght3 <-0
  if (MultiStock.Version=="Yes")
   {
    Index <- which(GenInput[,1]=="#Multispecies.Wght1"); Multispecies.Wght1  <- as.numeric(GenInput[Index,2]) 
    Index <- which(GenInput[,1]=="#Multispecies.Wght2"); Multispecies.Wght2  <- as.numeric(GenInput[Index,2]) 
    Index <- which(GenInput[,1]=="#Multispecies.Wght3"); Multispecies.Wght3  <- as.numeric(GenInput[Index,2]) 
   }

  Uncertain.Level <- 0 
  Index <- which(GenInput[,1]=="#Reduced_uncertainty"); Uncertain.Level <- as.numeric(GenInput[Index,2])
  if (Uncertain.Level == 1) print("RUNNING WITH REDUCED UNCERTAINTY")
  Use.Profit.constraint <- "No"
  Index <- which(GenInput[,1]=="#Impose_profit_constraint_on_F"); 
  if (length(Index)>0) Use.Profit.constraint <- GenInput[Index,2]
  Do.any.projections <- T
  
  # -------------------------------------------------------------------------------------------------------------------------------

  Delay.Index <- 0; Delay.Discard <- 0; Delay.M.Length <- 0; Delay.M.Age <- 0; Delay.CAA <- 0
  Index <- which(GenInput[,1]=="#Delay-for-dats-sets:")
  if (length(Index>0))
  { Delay.Index <- as.numeric(GenInput[Index+1,1]); Delay.Discard <- as.numeric(GenInput[Index+1,2]);
    Delay.M.Length <- as.numeric(GenInput[Index+1,3]); Delay.M.Age <- as.numeric(GenInput[Index+1,4])
    Delay.CAA <- as.numeric(GenInput[Index+1,5]) }
  
  
  # -------------------------------------------------------------------------------------------------------------------------------
  if (Nstocks > 1)
   {
    Index <- which(GenInput[,1]=="#PGMSY_Primary_Species"); CheckError(Index,"PGMSY - Missing species designations") 
    PGMSY.Primary.Species <- as.numeric(GenInput[Index+1,1:Nstocks])
    Index <- which(GenInput[,1]=="#PGMSY.Limit"); CheckError(Index,"PGMSY - Missing Limit") 
    PGMSY.limit <- as.numeric(GenInput[Index,2])
    Index <- which(GenInput[,1]=="#PGMSY.Linked_Species_by_fleet"); CheckError(Index,"PGMSY -link between species and fleet") 
    PGMSY.Primary.Species.by.fleet <- as.numeric(GenInput[Index+1,1:Ncat_fleet])                 # Which species are best linked to which fleet
  
    Index <- which(GenInput[,1]=="#Indicator_Primary_Species"); CheckError(Index,"Indicator - Missing species designations") 
    Indicator.Species <- as.numeric(GenInput[Index+1,1:Nstocks])

    Index <- which(GenInput[,1]=="#Indicator_Final_TACs"); CheckError(Index,"Indicator - Final TAC specs")
    Indicator.Last.TAC.Code <- rep(0,Nstocks); Indicator.Last.TAC <- rep(0,Nstocks);  Indicator.Last.TAC.specs <- matrix(0,nrow=Nstocks,ncol=2)
    for (Istock in 1:Nstocks)
     {
      Indicator.Last.TAC.Code[Istock] <- as.numeric(GenInput[Index+Istock,1])  
      Indicator.Last.TAC.specs[Istock,] <- as.numeric(GenInput[Index+Istock,2:3]) 
     }
  } # Nstocks > 0
  
  # -------------------------------------------------------------------------------------------------------------------------------
  print(paste0(RunNo))
  if(!dir.exists(paste0(Path, "Results/"))) dir.create(paste0(Path, "Results/"))
  FolderResults <- paste0("Results/",RunNo)
  if (dir.exists(FolderResults)) unlink(FolderResults,recursive=T)
  dir.create(FolderResults)  

  BiolSaveName <- paste0(FolderResults,"/HistBiol_",RunNo,"_",Run_designator,".Out")
  write("# biological, selectivty, etc",file=BiolSaveName)

  Return.V <- NULL
  Return.V$Run_designator <- Run_designator
  Return.V$Nspec  <- Nspec 
  Return.V$Stocks <- Stocks
  Return.V$Nstocks <- Nstocks
  Return.V$Ncat_fleet <- Ncat_fleet
  Return.V$Nsuv_fleet <- Nsuv_fleet
  Return.V$Nfleet <- Nfleet
  Return.V$Narea <- Narea
  Return.V$Fleet.area <- rep(1,Nfleet)
  Return.V$Fleet.Names <- Fleet.Names
  Return.V$Stock.Names <- Stock.Names
  Return.V$TACs.fleets <- TACs.fleets
  Return.V$MultiStock.Version <- MultiStock.Version
  Return.V$Multispecies.Wght1 <- Multispecies.Wght1
  Return.V$Multispecies.Wght2 <- Multispecies.Wght2
  Return.V$Multispecies.Wght3 <- Multispecies.Wght3
  
  Return.V$BiolSaveName <- BiolSaveName
  
  Return.V$ProjAllSaveName <- paste0(FolderResults,"/ProjectAll_",RunNo,"_",Run_designator,".Out")
  Return.V$ProjFleetSaveName <- paste0(FolderResults,"/ProjectFleet_",RunNo,"_",Run_designator,".Out")
  Return.V$ProjFleetAreaSaveName <- paste0(FolderResults,"/ProjectFleetArea_",RunNo,"_",Run_designator,".Out")
  
  Return.V$EstSaveName <- paste0(FolderResults,"/Estimates_",RunNo,"_",Run_designator,".Out")
  Return.V$ParSaveName <- paste0(FolderResults,"/Parameters_",RunNo,"_",Run_designator,".Out")
  Return.V$AllParSaveName <- paste0(FolderResults,"/AllParameters_",RunNo,"_",Run_designator,".Out")
  Return.V$WASaveName <- paste0(FolderResults,"/ProjectWA_",RunNo,"_",Run_designator,".Out")
  Return.V$LogName <- paste0(FolderResults,"/LogFile_",RunNo,"_",Run_designator,".Out")
  
  Return.V$InsuranceSaveName <- paste0(FolderResults,"/ProjectIns_",RunNo,"_",Run_designator,".Out")
  Return.V$MultiSaveName <- paste0(FolderResults,"/ProjectMulti_",RunNo,"_",Run_designator,".Out")
  Return.V$MiscSaveName <- paste0(FolderResults,"/ProjectMisc_",RunNo,"_",Run_designator,".Out")
  DebugFile <<- paste0(FolderResults,"/Debug_",RunNo,"_",Run_designator,".Out")
  
  Return.V$Nsim  <- Nsim 
  Return.V$FirstProjYr  <- FirstProjYr
  Return.V$AssFreq  <- AssFreq 
  Return.V$Nproj <- Nproj 
  Return.V$RunFolder <- RunFolder
  Return.V$TestCase <- TestCase
  Return.V$Uncertain.Level <- Uncertain.Level
  Return.V$Use.Profit.constraint <- Use.Profit.constraint
  Return.V$FolderResults <- FolderResults
  Return.V$Do.any.projections <- Do.any.projections
  
  Return.V$Multi <- NULL
  Return.V$Multi$Obj0 <- rep(0,Nproj)
  Return.V$Multi$Obj1 <- rep(0,Nproj)
  Return.V$Multi$Obj2 <- rep(0,Nproj)
  Return.V$Multi$Multipliers <- matrix(0,nrow=Nproj,ncol=Narea*sum(TACs.fleets=="Yes"))
  Return.V$Multi$TargetTACs <- matrix(0,nrow=Nproj,ncol=Nstocks)
  Return.V$Multi$EstTACs <- matrix(0,nrow=Nproj,ncol=Nstocks)
  Return.V$Multi$Link.Species <- matrix(0,nrow=Nproj,ncol=Nstocks)
  Return.V$Multi$PGMSY.Adjustment <- matrix(1,nrow=Nproj,ncol=Nstocks)
  Return.V$Multi$Indicator.Adjustment <- matrix(1,nrow=Nproj,ncol=Nstocks)
  
  if (Nstocks > 1)
   {
    Return.V$PGMSY.Primary.Species <- PGMSY.Primary.Species
    Return.V$PGMSY.limit <- PGMSY.limit
    Return.V$PGMSY.Primary.Species.by.fleet <- PGMSY.Primary.Species.by.fleet
    Return.V$Indicator.Species <- Indicator.Species
    if (max(PGMSY.Primary.Species) > 0) Return.V$PGMSY.Use <- "Yes" else Return.V$PGMSY.Use <- "No"
    if (max(Indicator.Species) > 0) Return.V$Indicator.Species.Use <- "Yes" else Return.V$Indicator.Species.Use <- "No"
    Return.V$Indicator.Last.TAC.Code <- Indicator.Last.TAC.Code
    Return.V$Indicator.Last.TAC <- Indicator.Last.TAC
    Return.V$Indicator.Last.TAC.specs <- Indicator.Last.TAC.specs 
   }
  else
   {
    Return.V$PGMSY.Primary.Species[1] <- 0
    Return.V$Indicator.Last.TAC.Code[1] <- 3
    Return.V$Indicator.Species[1] <- 0
    Return.V$PGMSY.Use <- "No"
    Return.V$Indicator.Species.Use <- "No"
   }

  Return.V$Delay.Index <- Delay.Index
  Return.V$Delay.Discard <- Delay.Discard
  Return.V$Delay.M.Length <- Delay.M.Length
  Return.V$Delay.M.Age <- Delay.M.Age
  Return.V$Delay.CAA <- Delay.CAA
  
  Return.V$Caitlin <- "No"
  
  return(Return.V)
 }

# ======================================================================================================================================
# ======================================================================================================================================

DoRun <- function(GeneralFile,RunNo)
{
  # Read the general file
  General <<- Read.General.File(GeneralFile,RunNo)
  if (TestCase==T) General$Nsim <- 1
  write("",file=DebugFile)

  # General the seeds
  TheSeeds <- 1234; set.seed(TheSeeds)
  Seeds <- matrix(nrow=General$Nsim,ncol=General$Nstocks)
  for (Isim in 1:General$Nsim) Seeds[Isim,] <- floor(runif(General$Nstocks,1,1000000))
  
  # Create key global variables
  Stock <<- vector(mode="list",length=General$Nstocks)
  R0Save1 <- vector(mode="list",length=General$Nstocks)
  # R0 multiplier
  R0mult <- rep(1,General$Nstocks)
  # Now project
  for (Isim in 1:General$Nsim)
   {
    cat("Doing simulation",Isim,"of",General$Nsim,"\n")
    # Specify initial conditions and store R0-related outputs
    for (Istock in 1:General$Nstocks) 
     {
      cat("reading: ",General$Stock.Names[Istock],"\n")
      Stock[[Istock]] <<- SetSpec(Isim,Istock,General$Stock.Names[Istock],Seed=Seeds[Isim,Istock])
      R0Save1[[Istock]]$Save1 <- Stock[[Istock]]$R00
      R0Save1[[Istock]]$Save2 <- Stock[[Istock]]$R00.orig
      R0Save1[[Istock]]$Save3 <- Stock[[Istock]]$R0
     }
    print("Done read")
    for (Istock in 1:General$Nstocks) WriteBiol(Istock,General$BiolSaveName,General$FolderResults)
    for (Istock in 1:General$Nstocks) WriteLog(Isim,Istock,General$LogName)

    print("Solve for initial depletion")
    for (Istock in 1:General$Nstocks)
     if (!is.na(Stock[[Istock]]$Target.Depletion))
      {
       # AEP: Might want to do this all sims (be careful of the initial R0)
       if (Isim == 1)
        {
         # No projections (to save time)
         General$Do.any.projections <<- F
          
         R0mult.min <- 0 ; R0mult.max <- 10
         for (Iloop in 1:10)
          {
           R0mult[Istock] <-  (R0mult.min+R0mult.max)/2.0
           
           # Do evaluation
           Stock[[Istock]]$R00 <<- R0Save1[[Istock]]$Save1*R0mult[Istock]
           Stock[[Istock]]$R00.orig <<- R0Save1[[Istock]]$Save2*R0mult[Istock]
           Stock[[Istock]]$R0 <<- R0Save1[[Istock]]$Save3*R0mult[Istock]
           Stock[[Istock]]$Data$NindexData <<- rep(0,Stock[[Istock]]$NassArea)
           Stock[[Istock]]$Data$NdiscardData <<- 0
           # No projection call
           Do.Project(Isim);
          
           # Check how close we got
           YearProj <- Stock[[Istock]]$Nhist+1
           Bcurrent <- sum( sum(Stock[[Istock]]$SSB[,YearProj]))
           B0 <- sum(Stock[[Istock]]$SSB0*Stock[[Istock]]$R0[,YearProj]/Stock[[Istock]]$R00)
           Depletion <- Bcurrent/B0
           cat(R0mult[Istock],Depletion,R0mult.min,R0mult.max,"\n")
           
           # Update range of R0 multipliers
           if (Depletion > Stock[[Istock]]$Target.Depletion)
             R0mult.max <- R0mult[Istock]
           else
             R0mult.min <- R0mult[Istock]
          } # Iloop
         General$Do.any.projections <<- T
        } # Isim == 1
       # Copy (for all sims)
       Stock[[Istock]]$R00 <<- R0Save1[[Istock]]$Save1*R0mult[Istock]
       Stock[[Istock]]$R00.orig <<- R0Save1[[Istock]]$Save2*R0mult[Istock]
       Stock[[Istock]]$R0 <<- R0Save1[[Istock]]$Save3*R0mult[Istock]
     } # Istock
    
    print("projecting")
    Do.Project(Isim);
   
    # Now write out stuff
    for (Istock in 1:General$Nstocks) WriteProjFiles(Isim,Istock,General$ProjAllSaveName,General$ProjFleetSaveName,General$ProjFleetAreaSaveName)
    for (Istock in 1:General$Nstocks) WriteEst(Isim,Istock,General$EstSaveName,General$ParSaveName,General$AllParSaveName)
    for (Istock in 1:General$Nstocks) WriteInsurance(Isim,Istock,General$InsuranceSaveName)
    WriteMulti(Isim,General$MultiSaveName)
    for (Istock in 1:General$Nstocks) WriteWA(Isim,Istock,General$WASaveName)
    for (Istock in 1:General$Nstocks) WriteMisc(Isim,Istock,General$MiscSaveName)
    
    print("done projecting")
    if (Estimation.test < 90) return()
   } # Isim loop

}

# ======================================================================================================================================
# ======================================================================================================================================
