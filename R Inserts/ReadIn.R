
# ===================================================================================================================================

CheckError <- function(Value,Name)
 {
  if (length(Value)!=1) 
  { cat("Parameter ",Name," is not found - stopping"); stop(); }
 }

# ===================================================================================================================================

Fill.time.var <- function(Icol,I.time,AllInput,Index,TimePar,Nsex,LastYr)
 {
  Yr1 <- as.numeric(AllInput[Index+1+I.time,1])
  for (Isex in 1:Nsex)
   for (t in Yr1:LastYr) 
    TimePar[t,Isex] <- as.numeric(AllInput[Index+1+I.time,Icol[Isex]])
  return(TimePar)
}

Fill.time.var2 <- function(Icol,I.time,AllInput,Index,TimePar,Nsex,LastYr)
{
  Yr1 <- as.numeric(AllInput[Index+1+I.time,1])
  for (Isex in 1:Nsex)
   for (t in Yr1:LastYr) 
    if (as.numeric(AllInput[Index+1+I.time,Icol[Isex]]) != 0)
     TimePar[Isex,,t] <- as.numeric(AllInput[Index+1+I.time,Icol[Isex]])
  return(TimePar)
}

# ===================================================================================================================================

SetSpec <- function(Isim,Istock,StockName,Seed)
 {

  # Constants
  Narea <- General$Narea
 
  # Set the seed for this trial
  set.seed(Seed); NextSeed <- floor(runif(1,1,1000000))

  # Read the files
  FileName1 <- paste0(Input.Folder,StockName,".OM")
  AllInput <- read.table(FileName1,header=F,col.names=paste0("C",1:220),fill=T,comment="?")
  FileName2 <- paste0(Input.Folder,StockName,".EM")
  AssInput <- read.table(FileName2,header=F,col.names=paste0("C",1:220),fill=T,comment="?")
  FileName3 <- paste0(Input.Folder,StockName,".OM.SAV")
  load(FileName3)

  StockDetails <- NULL
  Ass <- NULL
  Ass$TAC.management <- "Yes"
  
  # Assessment areas (if needed)
  StockDetails$NassArea <- 1
  StockDetails$FleetsToAreas <- rep(0,General$Narea)
  StockDetails$FleetsToAreas[1] <- 1
  Index <- which(AssInput[,1]=="#Ass_Basic.NassArea")
  if (length(Index) >0 )
   {
    StockDetails$NassArea <- as.numeric(AssInput[Index,2])
    if (StockDetails$NassArea > 10) {cat("RatpackR was not designed for more than 10 assessment areas"); AAA}
    Index <- which(AssInput[,1]=="#Ass_Basic.AssAreas"); CheckError(Index,"Assessment Areas")
    StockDetails$FleetsToAreas <- as.numeric(AssInput[Index+1,1:General$Narea])
   }
  NassArea <-  StockDetails$NassArea
  FleetsToAreas <- StockDetails$FleetsToAreas

  # Initial depletion (optional)
  StockDetails$Target.Depletion <- NA
  Index <- which(AllInput[,2]=="Initial" & AllInput[,3]=="depletion");
  if (length(Index)>0) StockDetails$Target.Depletion <-as.numeric(AllInput[Index+1,1])
  
  # Spatial density
  StockDetails$Relative.Density <- 1
  if (Narea > 1)
   {
    Index <- which(AllInput[,2]=="Relative" & AllInput[,3]=="density" & AllInput[,4]=="spatially"); CheckError(Index,"Relative density spatial")
    StockDetails$Relative.Density <- as.numeric(AllInput[Index+1,1:General$Narea])
    StockDetails$Relative.Density <- StockDetails$Relative.Density/sum(StockDetails$Relative.Density)
   }

  # Basic biological details
  Index <- which(AllInput[,2]=="First" & AllInput[,3]=="projection"); CheckError(Index,"First projection year")
  FirstProjYr <- as.numeric(AllInput[Index+1,1]); StockDetails$FirstProjYr <- FirstProjYr
  StockDetails$FirstProjYr <- FirstProjYr
  Index <- which(AllInput[,2]=="Maximum" & AllInput[,3]=="age"); CheckError(Index,"Maximum age")
  MaxAge <- as.numeric(AllInput[Index+1,1]); StockDetails$MaxAge <- MaxAge
  Index <- which(AllInput[,2]=="Number" & AllInput[,4]=="sexes"); CheckError(Index,"Number of sexes")
  Nsex <- as.numeric(AllInput[Index+1,1]); StockDetails$Nsex <- Nsex
  Index <- which(AllInput[,2]=="Number" & AllInput[,4]=="sexes(V2)"); CheckError(Index,"Number of sexes (perhaps -1)")
  NsexUse <- as.numeric(AllInput[Index+1,1]); StockDetails$NsexUse <- NsexUse
  Index <- which(AllInput[,2]=="Number" & AllInput[,4]=="historical"); CheckError(Index,"Number of number of pre-management years")
  Nhist <- as.numeric(AllInput[Index+1,1]); StockDetails$Nhist <- Nhist
  Index <- which(AllInput[,2]=="Number" & AllInput[,4]=="length"); CheckError(Index,"Number of length-classes")
  Nlen <- as.numeric(AllInput[Index+1,1]); StockDetails$Nlen <- Nlen
  Index <- which(AllInput[,2]=="lower" & AllInput[,3]=="length"); CheckError(Index,"Lower bounds for length bins")
  LoLenBin <- as.numeric(AllInput[Index+1,1:(Nlen+1)])
  HiLenBin <- LoLenBin[2:(Nlen+1)]
  ages <- 0:(MaxAge-1)
  lengths <- rep(NA,Nlen)
  for (Ilen in 1:Nlen) lengths[Ilen] <- LoLenBin[Ilen] + (HiLenBin[Ilen] -LoLenBin[Ilen])/2.0;
  StockDetails$LoLenBin <- LoLenBin
  
  # Get the offset (one year before the first historical year)
  StockDetails$YrOffset <- StockDetails$FirstProjYr-(Nhist+1)

  # Initial F
  StockDetails$Finitial <- matrix(0,nrow=Narea,ncol=General$Ncat_fleet)
  
  # ----------------------------------------------------------------------------------------------------------------------------------------------------------  
  # Environmental data
  set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
  Index <- which(AllInput[,1]=="#EnvIndex:Num"); CheckError(Index,"Number of environmental indicators"); N.env.ind <- as.numeric(AllInput[Index,2])
  EnvData <- matrix(0,nrow=Nhist+General$Nproj+10,ncol=N.env.ind)
  if (N.env.ind>0)
  for (Ind in 1:N.env.ind)
   {
    Index <- which(AllInput[,1]==paste0("#EnvIndex:",Ind));
    for (Iyear in 1:(Nhist+General$Nproj+1))
     {
      EnvData[Iyear,Ind] <- as.numeric(AllInput[Index+Iyear,3]); YearInd <- as.numeric(AllInput[Index+Iyear,1])
      if (YearInd != StockDetails$YrOffset+Iyear) { cat("Error for evironmental index ",Ind,"and year",Iyear+StockDetails$YrOffset,"\n"); exit()}
     }
    Index <- which(AllInput[,1]=="#EnvIndexOverRide:" & AllInput[,2]==Ind)
    if (length(Index)>0)
      {
       N.index.overide <- as.numeric(AllInput[Index+1,1])
       Blocks <- as.numeric(AllInput[Index+1,2])
       Ind.overide <- matrix(NA,nrow=N.index.overide,ncol=Blocks+1)
       for (II in 1:N.index.overide) Ind.overide[II,] <- as.numeric(AllInput[Index+1+II,1:(Blocks+1)])
       for (Iyr in seq(from=Nhist+1,to=Nhist+General$Nproj+1,by=Blocks))
        {
         Prob <- 1; OK <- F
         for (Jyr in 1:Blocks)  EnvData[Iyr+Jyr-1,Ind] <- 0
         for (II in 1:N.index.overide)
          {
           if (runif(1,0,1)<Ind.overide[II,1]/Prob)
            {
             if (OK==F) cat("found",II+StockDetails$YrOffset+Iyr,"\n")
             if (OK==F)
              for (Jyr in 1:Blocks)  EnvData[Iyr+Jyr-1,Ind] <- Ind.overide[II,1+Jyr]
              OK <- T   
            } # Random number check (and then adjust)
           Prob <- Prob - Ind.overide[II,1]
          } # Possible overides
        } # All blocks
      } # Is there an override
                    
   }
  StockDetails$N.env.ind <- N.env.ind
  StockDetails$EnvData <- EnvData
  if (FullOutput==T) cat("Done:Read the environmental indices\n")

  StockDetails$N.env.links <- 0
  Index <- which(AllInput[,1]=="#Env_links"); if (length(Index)>0)  StockDetails$N.env.links <- as.numeric(AllInput[Index,2])
  if (StockDetails$N.env.links > 0)
   {
    StockDetails$LinkVarsType <- AllInput[Index+1:StockDetails$N.env.links,1]
    StockDetails$LinkVars <- as.numeric(AllInput[Index+1:StockDetails$N.env.links,2])
    StockDetails$LinkVals <- as.numeric(AllInput[Index+1:StockDetails$N.env.links,3])
   }

  StockDetails$N.env.vars <- 0
  Index <- which(AllInput[,1]=="#Env_vars"); if (length(Index)>0)  StockDetails$N.env.vars <- as.numeric(AllInput[Index,2])
  if (StockDetails$N.env.vars > 0)
   {
    StockDetails$VarVarType <- AllInput[Index+1:StockDetails$N.env.vars,1]
    StockDetails$VarVals <- as.numeric(AllInput[Index+1:StockDetails$N.env.vars,2])
   }
  
  set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
  Index <- which(AllInput[,1]=="#Catastrophic_mortality"); 
  StockDetails$Cata.mort <-matrix(0,nrow=Nhist+General$Nproj+1,ncol=3)
  M.dev.cat <- rep(0,Nhist+General$Nproj+1)
  for (Iyear in 1:(Nhist+General$Nproj+1))
   {
    if (length(Index)>0) StockDetails$Cata.mort[Iyear,] <- as.numeric(AllInput[Index+Iyear,1:3]); YearInd <- as.numeric(AllInput[Index+Iyear,1])
    Rand <- runif(1,0,1)
    if (Rand < StockDetails$Cata.mort[Iyear,2]) M.dev.cat[Iyear]<- StockDetails$Cata.mort[Iyear,3]
   }
  #print(M.dev.cat)

  if (FullOutput==T) cat("Done: environmental links and environmental variables\n")
  
  # ----------------------------------------------------------------------------------------------------------------------------------------------------------  
  # Specify M
  M.type <- 0; SigmaM <- 0; Prob_Collapse <- 0.1; Size_collapse <- 0
  Index <- which(AllInput[,1]=="#M.type");          if (length(Index)>0) M.type <- as.numeric(AllInput[Index+1,1])
  Index <- which(AllInput[,1]=="#M_var");           if (length(Index)>0) SigmaM <- as.numeric(AllInput[Index,2])
  Index <- which(AllInput[,1]=="#M_prob_collapse"); if (length(Index)>0) Prob_Collapse <- as.numeric(AllInput[Index,2])
  Index <- which(AllInput[,1]=="#M_size_collapse"); if (length(Index)>0) Size_collapse <- as.numeric(AllInput[Index,2])
  #cat("M pars",SigmaM, Prob_Collapse, Size_collapse,"\n")
  
  Index <- which(AllInput[,2]=="Base" & AllInput[3]=="M"); CheckError(Index,"Base value for N")
  Mbase <- matrix(0,nrow=Nsex,ncol=MaxAge)
  for (Isex in 1:Nsex) Mbase[Isex,] <-as.numeric(AllInput[Index+Isex,1:MaxAge]); 
  StockDetails$Mbase <- Mbase
  StockDetails$M0 <- array(0,dim=c(Nsex,MaxAge,Nhist+General$Nproj+1))
  for (Iyear in 1:(Nhist+General$Nproj+1))StockDetails$M0[,,Iyear] <- Mbase
  set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))

  Index <- which(AllInput[,1]=="#Growth:Time-varying.N"); CheckError(Index,"N time-varying growth")
  N.time.varying <-  as.numeric(AllInput[Index+1,1]);
  if (N.time.varying>0)
   {
     Index <- which(AllInput[,1]=="#Growth:Time-varying"); CheckError(Index,"Time-varying growth blocks")
    Icol <- which(substr(AllInput[Index+1,],1,12)=="NatM_uniform")
    if (length(Icol)>0)
      for (I.time in 1:N.time.varying)
        StockDetails$M0 <- Fill.time.var2(Icol,I.time,AllInput,Index,StockDetails$M0,Nsex,Nhist+General$Nproj+1)
   } # Time-varying
 
  StockDetails$Mcat.Index <- rep(0,Nhist+General$Nproj+1)
  for (Iyear in 2:(Nhist+General$Nproj))
   {
    # Probability of collapse
    if (runif(1,0,1) < Prob_Collapse) StockDetails$M0[,,Iyear] <- StockDetails$M0[,,Iyear] + Size_collapse
    if (Iyear > Nhist)
    if (StockDetails$N.env.links>0)
     for (Ilink in 1:StockDetails$N.env.links)
      {
       # Account for an environmental link on M
       if (StockDetails$LinkVarsType[Ilink]=="M")  
        {
         StockDetails$Mdiff <- StockDetails$LinkVals[Ilink]
         StockDetails$M0[,,Iyear] <- StockDetails$M0[,,Iyear]*exp(StockDetails$LinkVals[Ilink]*EnvData[Iyear,StockDetails$LinkVars[Ilink]])
         StockDetails$Mcat.Index[Iyear] <- EnvData[Iyear,StockDetails$LinkVars[Ilink]]
        }
      }
    # Random variability in M
    if (Iyear > Nhist) Mdev <- exp(rnorm(1,0,SigmaM)-SigmaM^2/2.0) else Mdev <- 1
    StockDetails$M0[,,Iyear] <- StockDetails$M0[,,Iyear]*Mdev*exp(M.dev.cat[Iyear])
    if (abs(M.dev.cat[Iyear]) >0.1) StockDetails$Mcat.Index[Iyear] <- 1
  }
  StockDetails$M <- StockDetails$M0
  StockDetails$M.dev.cat <- M.dev.cat
  
  print(StockDetails$M0[1,1,])
  if (FullOutput==T) cat("Done: Natural mortality\n")

  # ----------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  # Growth stuff
  Index <- which(AllInput[,1]=="#Growth_model"); CheckError(Index,"Growth model")
  Growth.Model <-  as.numeric(AllInput[Index+1,1]); Age2 <-  as.numeric(AllInput[Index+1,2]);
  
  Index <- which(AllInput[,1]=="#Growth:min/max"); CheckError(Index,"Amin and Amax for growth")
  Age1 <-  as.numeric(AllInput[Index+1,1]); Age2 <-  as.numeric(AllInput[Index+1,2]);
  Index <- which(AllInput[,1]=="#Growth:L_at_Amin"); CheckError(Index,"L_at_Amin")
  LenA1 <- as.numeric(AllInput[Index+1,1:Nsex]);
  Index <- which(AllInput[,1]=="#Growth:L_at_Amax"); CheckError(Index,"Growth L_at_AMAX")
  LenA2 <- as.numeric(AllInput[Index+1,1:Nsex]);
  Index <- which(AllInput[,1]=="#Growth:k"); CheckError(Index,"Growth kappa")
  Kappa <- as.numeric(AllInput[Index+1,1:Nsex]);
  if (Growth.Model==2)
   {
    Index <- which(AllInput[,1]=="#Growth:Richards"); CheckError(Index,"Growth Richards")
    Richards <- as.numeric(AllInput[Index+1,1:Nsex]);
   }
  Index <- which(AllInput[,1]=="#Growth:CV1"); CheckError(Index,"CV 1")
  CV1 <- as.numeric(AllInput[Index+1,1:Nsex]);
  Index <- which(AllInput[,1]=="#Growth:CV2"); CheckError(Index,"CV 2")
  CV2 <- as.numeric(AllInput[Index+1,1:Nsex]);
  CVoption <- 1
  Index <- which(AllInput[,1]=="#Growth:CVoption"); CheckError(Index,"Option for the CV")
  if (length(Index)>0) CVoption <- as.numeric(AllInput[Index+1,1]);
  StockDetails$Growth.Model <- Growth.Model
  StockDetails$CVoption <- CVoption
  StockDetails$LenA1 <- LenA1
  StockDetails$LenA2 <- LenA2
  StockDetails$Kappa <- Kappa
  if (Growth.Model==2) StockDetails$Richards <- Richards
  StockDetails$CV1 <- CV1
  StockDetails$CV2 <- CV2
  
  Index <- which(AllInput[,1]=="#Growth:weight-len_a"); CheckError(Index,"length-weight-a")
  WtLen_a <- as.numeric(AllInput[Index+1,1:Nsex]);
  Index <- which(AllInput[,1]=="#Growth:weight-len_b"); CheckError(Index,"length-weight-b")
  WtLen_b <- as.numeric(AllInput[Index+1,1:Nsex]);
  StockDetails$WtLen_a <- WtLen_a
  StockDetails$WtLen_b <- WtLen_b
  
  # Weight at length
  WtLen <- matrix(NA,Nsex,Nlen)
  for (Isex in 1:Nsex)
   for (Ilen in 1:Nlen)  
     WtLen[Isex,Ilen] <- WtLen_a[Isex] * exp(WtLen_b[Isex]*log(lengths[Ilen]));
  StockDetails$WtLen <- WtLen
    
  Index <- which(AllInput[,1]=="#Growth:maturity_option"); CheckError(Index,"maturityoption")
  Mat.option <- as.numeric(AllInput[Index+1,1]);
  age.mat <- NULL
  if (Mat.option==4) 
   {
    Index <- which(AllInput[,1]=="#_Age_Fecundity"); CheckError(Index,"maturity by age")
    age.mat <- as.numeric(AllInput[Index+1,1:MaxAge]);
   }
  StockDetails$age.mat <- age.mat
  Index <- which(AllInput[,1]=="#Growth:maturity_a50"); CheckError(Index,"length-maturity-a")
  Mat50 <- as.numeric(AllInput[Index+1,1]);
  Index <- which(AllInput[,1]=="#Growth:maturity_slope"); CheckError(Index,"length-maturity-b")
  SlopeMat <- as.numeric(AllInput[Index+1,1]);
  Index <- which(AllInput[,1]=="#Growth:first_mature_age"); CheckError(Index,"first_mature_age")
  First.Mat.Age <- as.numeric(AllInput[Index+1,1]);
  StockDetails$Mat.option <- Mat.option
  StockDetails$Mat50    <- Mat50
  StockDetails$SlopeMat <- SlopeMat
  StockDetails$First.Mat.Age <- First.Mat.Age+1
  
  Index <- which(AllInput[,1]=="#Growth:Fecundity.option"); CheckError(Index,"Fecundity optio")
  Fec.opt <- as.numeric(AllInput[Index+1,1]);
  
  Index <- which(AllInput[,1]=="#Growth:Egg_1"); CheckError(Index,"eggs-1")
  Eggs.1 <- as.numeric(AllInput[Index+1,1]);
  Index <- which(AllInput[,1]=="#Growth:Egg_2"); CheckError(Index,"eggs-2")
  Eggs.2 <- as.numeric(AllInput[Index+1,1]);
  StockDetails$Eggs.1 <- Eggs.1
  StockDetails$Eggs.2 <- Eggs.2
  
  # Various temp variables
  FracLenS <- array(NA,dim=c(Nsex,MaxAge,Nhist+General$Nproj+1,Nlen))
  FracLenM <- array(NA,dim=c(Nsex,MaxAge,Nhist+General$Nproj+1,Nlen))
  MeanLenAgeS <- array(0,dim=c(Nsex,MaxAge,Nhist+General$Nproj+1))
  SigmaLenAgeS <- array(0,dim=c(Nsex,MaxAge,Nhist+General$Nproj+1))
  MeanLenAgeM <- array(0,dim=c(Nsex,MaxAge,Nhist+General$Nproj+1))
  SigmaLenAgeM <- array(0,dim=c(Nsex,MaxAge,Nhist+General$Nproj+1))

  #Set Linf and time-varying pars by year
  KappaTV <- matrix(0,nrow=(Nhist+General$Nproj+1),ncol=Nsex)
  RichardsTV <- matrix(0,nrow=(Nhist+General$Nproj+1),ncol=Nsex)
  LenA1TV <- matrix(0,nrow=(Nhist+General$Nproj+1),ncol=Nsex)
  LenA2TV <- matrix(0,nrow=(Nhist+General$Nproj+1),ncol=Nsex)
  LinfTV  <- matrix(0,nrow=(Nhist+General$Nproj+1),ncol=Nsex)
  for (Isex in 1:Nsex)
    for (t in 1:(Nhist+General$Nproj+1))
     {
      KappaTV[t,Isex] <- Kappa[Isex]
      LenA1TV[t,Isex] <- LenA1[Isex]
      LenA2TV[t,Isex] <- LenA2[Isex]
      if (Growth.Model==2) RichardsTV[t,Isex] <- Richards[Isex]
    } 
    
  Index <- which(AllInput[,1]=="#Growth:Time-varying.N"); CheckError(Index,"N time-varying growth")
  N.time.varying <-  as.numeric(AllInput[Index+1,1]);
  
  if (N.time.varying>0)
   {
    Index <- which(AllInput[,1]=="#Growth:Time-varying"); CheckError(Index,"Time-varying growth blocks")
    Icol <- which(substr(AllInput[Index+1,],1,9)=="L_at_Amin")
    if (length(Icol)>0)
    for (I.time in 1:N.time.varying)
      LenA1TV <- Fill.time.var(Icol,I.time,AllInput,Index,LenA1TV,Nsex,Nhist+General$Nproj+1)
    Icol <- which(substr(AllInput[Index+1,],1,9)=="L_at_Amax")
    if (length(Icol)>0)
      for (I.time in 1:N.time.varying)
        LenA2TV <- Fill.time.var(Icol,I.time,AllInput,Index,LenA2TV,Nsex,Nhist+General$Nproj+1)
    Icol <- which(substr(AllInput[Index+1,],1,9)=="VonBert_K")
    if (length(Icol)>0)
      for (I.time in 1:N.time.varying)
        KappaTV <- Fill.time.var(Icol,I.time,AllInput,Index,KappaTV,Nsex,Nhist+General$Nproj+1)
    if (Growth.Model==2)
    {
     Icol <- which(substr(AllInput[Index+1,],1,8)=="Richards")
     if (length(Icol)>0)
      for (I.time in 1:N.time.varying)
        RichardsTV <- Fill.time.var(Icol,I.time,AllInput,Index,RichardsTV,Nsex,Nhist+General$Nproj+1)
    }
  } # Time-varying
 
  for (Isex in 1:Nsex)
    for (t in 1:(Nhist+General$Nproj+1))
     {
      if (StockDetails$N.env.links>0)
       for (Ilink in 1:StockDetails$N.env.links)
        {
         if (StockDetails$LinkVarsType[Ilink]=="LenA1") LenA1TV[t,Isex] <- LenA1TV[t,Isex]*exp(StockDetails$LinkVals[Ilink]*EnvData[t,StockDetails$LinkVars[Ilink]])
         if (StockDetails$LinkVarsType[Ilink]=="LenA2") LenA2TV[t,Isex] <- LenA2TV[t,Isex]*exp(StockDetails$LinkVals[Ilink]*EnvData[t,StockDetails$LinkVars[Ilink]])
         if (StockDetails$LinkVarsType[Ilink]=="Kappa") KappaTV[t,Isex] <- KappaTV[t,Isex]*exp(StockDetails$LinkVals[Ilink]*EnvData[t,StockDetails$LinkVars[Ilink]])
       }
      # Random variability in growth parameters
      if (StockDetails$N.env.vars>0)
       for (Ivar in 1:StockDetails$N.env.vars)
        {
         if (StockDetails$VarVarType[Ivar]=="LenA1") LenA1TV[t,Isex] <- LenA1TV[t,Isex]*exp(rnorm(1,0,StockDetails$VarVals[Ivar]))
         if (StockDetails$VarVarType[Ivar]=="LenA2") LenA2TV[t,Isex] <- LenA2TV[t,Isex]*exp(rnorm(1,0,StockDetails$VarVals[Ivar]))
         if (StockDetails$VarVarType[Ivar]=="Kappa") KappaTV[t,Isex] <- KappaTV[t,Isex]*exp(rnorm(1,0,StockDetails$VarVals[Ivar]))
        }
      # Set L(infinity)
      LminR <- LenA1TV[t,Isex]
      LmaxR <- LenA2TV[t,Isex]
      if (Growth.Model==1)
        {
         if (Age2==999) 
          LinfTV[t,Isex] <-	LenA2TV[t,Isex]
         else 
          LinfTV[t,Isex] <-	LminR + (LmaxR-LminR)/(1-exp(-KappaTV[t,Isex]*(Age2-Age1))); 
         }
      if (Growth.Model==2)
        { 
         inv_Richards <- 1.0/RichardsTV[t,Isex]
         LenA1TV[t,Isex] <- LenA1TV[t,Isex]^RichardsTV[t,Isex]
         LenA2TV[t,Isex] <- LenA2TV[t,Isex]^RichardsTV[t,Isex]
         LminR <- LenA1TV[t,Isex]
         LmaxR <- LenA2TV[t,Isex]
         if (Age2==999) 
           LinfTV[t,Isex] <-	LmaxR
         else 
           LinfTV[t,Isex] <-	(LminR + (LmaxR-LminR)/(1-exp(-KappaTV[t,Isex]*(Age2-Age1))))
        }
    }  
  StockDetails$LinfTV <- LinfTV
  StockDetails$LenA1TV <- LenA1TV
  StockDetails$LenA2TV <- LenA2TV
  StockDetails$KappaTV <- KappaTV
  if (Growth.Model==2) StockDetails$RichardsTV <- RichardsTV
 
  # Length-at-age (VB)
  if (Growth.Model==1)
  {
   # Start-year length-at-age
   for (Isex in 1:Nsex)
     {
      # First year
      t <- 1
      MeanLenAgeS[Isex,1,t] <- LenA1TV[t,Isex]
      for (Iage in 1:MaxAge)
       {
        Age = Iage - 1;
        if (Age < Age1)
         MeanLenAgeS[Isex,Iage,t] <- LoLenBin[1] + (LenA1TV[t,Isex]-LoLenBin[1])/(Age1)*Age
        else
         if (Age == Age1)
           MeanLenAgeS[Isex,Iage,t] <- LenA1TV[t,Isex]
         else
           MeanLenAgeS[Isex,Iage,t] <- LenA1TV[t,Isex]+(LinfTV[t,Isex]-LenA1TV[t,Isex])*(1-exp(-KappaTV[t,Isex]*(Age-Age1)));
       } 
    
      # All subsequent years
      for (t in 2:(Nhist+General$Nproj+1))
       {
        MeanLenAgeS[Isex,1,t] <- LenA1TV[t,Isex]
        for (Iage in 2:MaxAge)
         {
          Age = Iage - 1;
          if (Age < Age1+1)
            MeanLenAgeS[Isex,Iage,t] <- LenA1TV[t,Isex]+(LinfTV[t,Isex]-LenA1TV[t,Isex])*(1-exp(-KappaTV[t,Isex]*(Age-Age1)))
          else
           MeanLenAgeS[Isex,Iage,t] <-  MeanLenAgeS[Isex,Iage-1,t-1]+max((exp(-KappaTV[t-1,Isex])-1)*(MeanLenAgeS[Isex,Iage-1,t-1]-LinfTV[t-1,Isex]),0)
        }
        #cat(t+StockDetails$YrOffset,round(MeanLenAgeS[Isex,1:8,t],4),"\n")
        for (Iage in 1:(Age1+1))
         {
          Age = Iage - 1;
          IyrEnd <- max(1,t-Iage+1)
          if (IyrEnd > Nhist+General$Nproj+1) IyrEnd <- Nhist+General$Nproj+1
          if (Age1 >0) MeanLenAgeS[Isex,Iage,t] <- LoLenBin[1] +(Age/Age1)*(LenA1TV[IyrEnd,Isex]-LoLenBin[1])
         }
      }
     } # Isex

   # Mid-year length-at-age
   for (Isex in 1:Nsex)
    for (t in 1:(Nhist+General$Nproj+1))
     {
      for (Iage in 1:MaxAge)
        MeanLenAgeM[Isex,Iage,t] <-  MeanLenAgeS[Isex,Iage,t]+max((MeanLenAgeS[Isex,Iage,t]-LinfTV[t,Isex])*(exp(-KappaTV[t,Isex]*0.5)-1),0)
      for (Iage in 1:floor(Age1+0.9999))
       {
        Age = Iage - 1+0.5;
        IyrEnd <- max(1,t-Iage+1)
        if (IyrEnd > Nhist+General$Nproj+1) IyrEnd <- Nhist+General$Nproj+1
        if (Age1 >0)  MeanLenAgeM[Isex,Iage,t] <- LoLenBin[1] +(Age/Age1)*(LenA1TV[IyrEnd,Isex]-LoLenBin[1])
       }
     }  
  } # Growth model = 1
    
  # Length-at-age (Richards)
  if (Growth.Model==2)
   {
    # Start-year length-at-age
    for (Isex in 1:Nsex)
     {
      # First year
      t <- 1
      MeanLenAgeS[Isex,1,t] <- LenA1TV[t,Isex]
      for (Iage in 1:MaxAge)
       {
        Age = Iage - 1;
        if (Age < Age1)
          MeanLenAgeS[Isex,Iage,t] <- LoLenBin[1] + (LenA1TV[t,Isex]-LoLenBin[1])/(Age1)*Age
        else
          if (Age == Age1)
            MeanLenAgeS[Isex,Iage,t] <- LenA1TV[t,Isex]^(1/RichardsTV[t,Isex])
          else
            MeanLenAgeS[Isex,Iage,t] <- (LenA1TV[t,Isex]+(LinfTV[t,Isex]-LenA1TV[t,Isex])*(1-exp(-KappaTV[t,Isex]*(Age-Age1))))^(1/RichardsTV[t,Isex])
       } 
 
      # All subsequent years
      for (t in 2:(Nhist+General$Nproj+1))
       {
        MeanLenAgeS[Isex,1,t] <- LenA1TV[t,Isex]^(1/RichardsTV[t-1,Isex])
        for (Iage in 2:MaxAge)
         {
          Age = Iage - 1;
          if (Age < Age1+1)
            MeanLenAgeS[Isex,Iage,t] <- (LenA1TV[t,Isex]+(LinfTV[t,Isex]-LenA1TV[t,Isex])*(1-exp(-KappaTV[t,Isex]*(Age-Age1))))^(1/RichardsTV[t-1,Isex])
          else
           {
            Temp <- MeanLenAgeS[Isex,Iage-1,t-1]^RichardsTV[t-1,Isex]
            t2 <- Temp - LinfTV[t,Isex];
            MeanLenAgeS[Isex,Iage,t] <- max(0,(Temp+(exp(-KappaTV[t-1,Isex])-1)*t2)^(1/RichardsTV[t-1,Isex]))
           }   
         }   
        #cat(t+StockDetails$YrOffset,round(MeanLenAgeS[Isex,1:8,t],4),"\n")
        for (Iage in 1:(Age1+1))
         {
          Age = Iage - 1;
          IyrEnd <- max(1,t-Iage+1)
          if (IyrEnd > Nhist+General$Nproj+1) IyrEnd <- Nhist+General$Nproj+1
          if (Age1 >0) MeanLenAgeS[Isex,Iage,t] <- LoLenBin[1] +(Age/Age1)*(LenA1TV[IyrEnd,Isex]^(1/RichardsTV[t-1,Isex])-LoLenBin[1])
         }
      }
     } # Isex
    # Mid-year length-at-age
    for (Isex in 1:Nsex)
      for (t in 1:(Nhist+General$Nproj+1))
       {
        for (Iage in 1:MaxAge)
          {
           Temp <- MeanLenAgeS[Isex,Iage,t]^RichardsTV[t,Isex]
           t2 <- Temp - LinfTV[t,Isex];
           Temp2 <- max(0,Temp + (exp(-KappaTV[t,Isex]*0.5)-1)*t2)
           MeanLenAgeM[Isex,Iage,t] <- Temp2^(1/RichardsTV[t,Isex])
          }
        for (Iage in 1:floor(Age1+0.9999))
         {
          Age = Iage - 1+0.5;
          IyrEnd <- max(1,t-Iage+1)
          if (IyrEnd > Nhist+General$Nproj+1) IyrEnd <- Nhist+General$Nproj+1
          if (Age1 >0)  MeanLenAgeM[Isex,Iage,t] <- LoLenBin[1] +(Age/Age1)*(LenA1TV[IyrEnd,Isex]^(1/RichardsTV[t,Isex])-LoLenBin[1])
        }
      }
   } # Growth model =2
         
  # Standard deviations of length-at-age
  if ( CVoption==0 ) # CV = F(A)
    for (t in 1:(Nhist+General$Nproj+1))
      for (Isex in 1:Nsex)
        for (Iage in 1:MaxAge)
        {
          Age <- Iage - 1
          if (Age < Age1)
          {
            SigmaLenAgeS[Isex,Iage,t] <- CV1[Isex]  * MeanLenAgeS[Isex,Iage,t]
            SigmaLenAgeM[Isex,Iage,t] <- CV1[Isex]  * MeanLenAgeM[Isex,Iage,t]
          }
          else if (Age >= MaxAge-1)
          {
            SigmaLenAgeS[Isex,Iage,t] <- CV2[Isex]  * MeanLenAgeS[Isex,Iage,t]
            SigmaLenAgeM[Isex,Iage,t] <- CV2[Isex]  * MeanLenAgeM[Isex,Iage,t]
          }
          else
          {
            SigmaLenAgeS[Isex,Iage,t] <- (CV1[Isex] + (MeanLenAgeS[Isex,Iage,t]-LenA1TV[t,Isex])/(LenA2TV[t,Isex]-LenA1TV[t,Isex])*(CV2[Isex]-CV1[Isex])) * MeanLenAgeS[Isex,Iage,t]
            SigmaLenAgeM[Isex,Iage,t] <- (CV1[Isex] + (MeanLenAgeM[Isex,Iage,t]-LenA1TV[t,Isex])/(LenA2TV[t,Isex]-LenA1TV[t,Isex])*(CV2[Isex]-CV1[Isex])) * MeanLenAgeM[Isex,Iage,t]
          }
        } # Isex and Iage
  if (CVoption==1) # CV = F(LAA)
   for (t in 1:(Nhist+General$Nproj+1))
    for (Isex in 1:Nsex)
     for (Iage in 1:MaxAge)
      {
       Age <- Iage - 1
       if (Age <= Age1)
        {
         SigmaLenAgeS[Isex,Iage,t] <- CV1[Isex]  * MeanLenAgeS[Isex,Iage,t]
         SigmaLenAgeM[Isex,Iage,t] <- CV1[Isex]  * MeanLenAgeM[Isex,Iage,t]
        }
       else
        if (Age <= Age2) 
         {
          SigmaLenAgeS[Isex,Iage,t] <- (CV1[Isex] + (Age-Age1)/(Age2-Age1)*(CV2[Isex]-CV1[Isex])) * MeanLenAgeS[Isex,Iage,t]
          SigmaLenAgeM[Isex,Iage,t] <- (CV1[Isex] + (Age-Age1)/(Age2-Age1)*(CV2[Isex]-CV1[Isex])) * MeanLenAgeM[Isex,Iage,t]
         } 
        else
         {
          SigmaLenAgeS[Isex,Iage,t] <- CV2[Isex]  * MeanLenAgeS[Isex,Iage,t]
          SigmaLenAgeM[Isex,Iage,t] <- CV2[Isex]  * MeanLenAgeM[Isex,Iage,t]
         }
      } # Isex and Iage
  
  if (CVoption==2) # SD = F(LAA)
    for (t in 1:(Nhist+General$Nproj+1))
      for (Isex in 1:Nsex)
        for (Iage in 1:MaxAge)
        {
          Age <- Iage - 1
          if (Age < Age1)
          {
            SigmaLenAgeS[Isex,Iage,t] <- CV1[Isex]  
            SigmaLenAgeM[Isex,Iage,t] <- CV1[Isex]  
          }
          else if (Age >= MaxAge-1)
          {
            SigmaLenAgeS[Isex,Iage,t] <- CV2[Isex]  
            SigmaLenAgeM[Isex,Iage,t] <- CV2[Isex]  
          }
          else
          {
            SigmaLenAgeS[Isex,Iage,t] <- (CV1[Isex] + (MeanLenAgeS[Isex,Iage,t]-LenA1TV[t,Isex])/(LenA2TV[t,Isex]-LenA1TV[t,Isex])*(CV2[Isex]-CV1[Isex]))
            SigmaLenAgeM[Isex,Iage,t] <- (CV1[Isex] + (MeanLenAgeM[Isex,Iage,t]-LenA1TV[t,Isex])/(LenA2TV[t,Isex]-LenA1TV[t,Isex])*(CV2[Isex]-CV1[Isex]))
          }
        } # Isex and Iage
  # Compute the transition matrices
   for (Isex in 1:Nsex)
    {
     for (t in 1:(Nhist+General$Nproj+1))
      for (Iage in 1:MaxAge)
       {
        # Start-year ALK
        len <- MeanLenAgeS[Isex,Iage,t]
        sig <- SigmaLenAgeS[Isex,Iage,t]
        xlim <- (LoLenBin[2] - len)/sig;
        accum = pnorm(xlim);
        FracLenS[Isex,Iage,t,1] <-  accum;
        for (l in 2:(Nlen-1))
         { xlim <- (LoLenBin[l+1] - len)/sig; integral <- pnorm(xlim); FracLenS[Isex,Iage,t,l] <-  integral - accum; accum <- integral; }
        FracLenS[Isex,Iage,t,Nlen] <- 1.0 - accum

        # Mid-year ALK
        len <- MeanLenAgeM[Isex,Iage,t]
        sig <- SigmaLenAgeM[Isex,Iage,t]
        xlim <- (LoLenBin[2] - len)/sig;
        accum = pnorm(xlim);
        FracLenM[Isex,Iage,t,1] <-  accum;
        for (l in 2:(Nlen-1))
         { xlim <- (LoLenBin[l+1] - len)/sig; integral <- pnorm(xlim); FracLenM[Isex,Iage,t,l] <-  integral -accum; accum <- integral; }
        FracLenM[Isex,Iage,t,Nlen] <- 1.0 - accum
       } # Iage / time
    } # Isex

  # Compute population vector for weight-at-age
  MeanWtAtAgeS <- array(NA,dim=c(Nsex,MaxAge,Nhist+General$Nproj+1))
  MeanWtAtAgeM <- array(NA,dim=c(Nsex,MaxAge,Nhist+General$Nproj+1))
  for (Year in 1:(Nhist+General$Nproj+1))
   for (Isex in 1:Nsex)
    for (Iage in 1:MaxAge)  
     {
      MeanWtAtAgeS[Isex,Iage,Year] <- sum(FracLenS[Isex,Iage,Year,]*WtLen[Isex,])   
      MeanWtAtAgeM[Isex,Iage,Year] <- sum(FracLenM[Isex,Iage,Year,]*WtLen[Isex,])   
     }

  # Maturity-at-length
  MatureL <- matrix(1,nrow=Nlen,ncol=Nhist+General$Nproj+1)
  
  if (!(Mat.option %in% c(1,2,4))) { cat("Mat option not coded\n"); AAA }
  if (Mat.option==1)
   for (Year in 1:(Nhist+General$Nproj+1))
    MatureL[,Year] <- 1.0/(1.0+exp(SlopeMat*(lengths-Mat50)))
  MatureA <- matrix(1,nrow=MaxAge,ncol=Nhist+General$Nproj+1)
  if (Mat.option==2)
    for (Year in 1:(Nhist+General$Nproj+1))
      MatureA[,Year] <- 1.0/(1.0+exp(SlopeMat*(ages-Mat50)))
  if (Mat.option==4)
    for (Year in 1:(Nhist+General$Nproj+1))
      MatureA[,Year] <- age.mat

  # Compute fecundity
  if (Fec.opt==1) Eggs <- WtLen[1,]*(Eggs.1 + Eggs.2*WtLen[1,])
  if (Fec.opt==2) Eggs <- (Eggs.1 * lengths^Eggs.2)
  if (Fec.opt==3) Eggs <- (Eggs.1 * WtLen[1,]^Eggs.2)
  if (Fec.opt==4) Eggs <- Eggs.1 + Eggs.2*lengths
  if (Mat.option %in% c(4,5)) Eggs <- rep(1,Nlen)
  Fecundity <- matrix(NA,nrow=MaxAge,ncol=Nhist+General$Nproj+1)
  for (Year in 1:(Nhist+General$Nproj+1))
   for (Iage in 1:MaxAge) 
     {
      Fecundity[Iage,Year] <- MatureA[Iage,Year]*sum(FracLenS[1,Iage,Year,]*Eggs*MatureL[,Year]) 
      if (NsexUse == -1) Fecundity[Iage,Year]  <- Fecundity[Iage,Year] * 0.5
     }
  if (StockDetails$First.Mat.Age>1)
   Fecundity[1:(StockDetails$First.Mat.Age-1),] <- 0

  #logical stuff
  StockDetails$Fecundity <- Fecundity
  StockDetails$MeanWtAtAgeS <- MeanWtAtAgeS
  StockDetails$MeanWtAtAgeM <- MeanWtAtAgeM
  StockDetails$MeanLenAgeS <- MeanLenAgeS
  StockDetails$MeanLenAgeM <- MeanLenAgeM 
  StockDetails$SigmaLenAgeS <- SigmaLenAgeS
  StockDetails$SigmaLenAgeM <- SigmaLenAgeM 
  StockDetails$FracLenM <-FracLenM
  StockDetails$FracLenS <-FracLenS
  if (FullOutput==T) cat("Done: Biology (growth and fecundity)\n")

# ----------------------------------------------------------------------------------------------------------------------------------------------------------  

  StockDetails$Global.Density.Dep <- T
  StockDetails$Move <- array(0,dim=c(Narea,Narea,MaxAge))
  if (Narea > 1)
   {
    Index <- which(AllInput[,1]=="#Spatial:Movement_rates"); CheckError(Index,"Movement rates for a spatial model")
    Ipnt <- Index + 2
    for (Iarea in 1:Narea)
     {
      for (Jarea in 1:Narea)
       if (Jarea != Iarea)
        { StockDetails$Move[Iarea,Jarea,1:MaxAge] <- as.numeric(AllInput[Ipnt,1:MaxAge]); Ipnt <- Ipnt + 1 }
      Total <- apply(StockDetails$Move[Iarea,,],2,sum)
      for (Iage in 1:MaxAge)StockDetails$Move[Iarea,Iarea,Iage] <- 1-Total[Iage] 
     }
    cat("Done: Biology (Movement)\n")
   }
  else
   {
    # No movement
    StockDetails$Move <- array(1,dim=c(Narea,Narea,MaxAge))  
   }
  
  StockDetails$ClosedAreas <- matrix(1,nrow=Narea,ncol=Nhist+General$Nproj+1)
  Index <- which(AllInput[,1]=="#Spatial:Closed_areas")
  if (length(Index) >0)
   {
    Index <- which(AllInput[,1]=="#Spatial:N_Closed_areas"); CheckError(Index,"Number of closed areas")
    N.closed.Areas <- as.numeric(AllInput[Index,2])
    if (N.closed.Areas > 0)
     for (Iclosed in 1:N.closed.Areas)
      {
       Iarea <- as.numeric(AllInput[Index+1+Iclosed,1])
       Years <- as.numeric(AllInput[Index+1+Iclosed,c(2,3)])-StockDetails$YrOffset
       for (Iyear in Years[1]:Years[2])
         StockDetails$ClosedAreas[Iarea,Iyear] <- 0
      }
   }
  
  # Which areas are closed to which fleets (for now all areas are open)
  StockDetails$UseFleet <- array(1,dim=c(Narea,General$Nfleet,Nhist+General$Nproj+1))
  Open.fleets <- rep(1,General$Nfleet)
  if (Narea > 1)
   {
    StockDetails$UseFleet <- array(0,dim=c(Narea,General$Nfleet,Nhist+General$Nproj+1))
    Index <- which(AllInput[,1]=="#Spatial:Open_fleets"); CheckError(Index,"Details on open fleets")
    for (Iarea in 1:Narea)
     {
      Open.fleets <- as.numeric(AllInput[Index+Iarea,1:General$Nfleet])
      for (Ifleet in 1:General$Nfleet) StockDetails$UseFleet[Iarea,Ifleet,] <- Open.fleets[Ifleet]  
     }
   } # Narea
  
  print("Done: Area-stuff")
  
  StockDetails$Recr.split <- matrix(1,nrow=Nhist+General$Nproj+1,ncol=Narea)
  for (Iarea in 1:Narea) StockDetails$Recr.split[,Iarea] <- StockDetails$Relative.Density[Iarea]

  # Time-varying rec_devs only apply for spatial models
  Index <- which(AllInput[,1]=="#Growth:Time-varying"); CheckError(Index,"N time-varying parameters")
  if (N.time.varying> 0 & Narea > 1)
   {
    Icol <- which(substr(AllInput[Index+1,],1,8)=="RecrDist")
    
    if (length(Icol)>0)
     {
      Icol <- Icol[1:Narea]
      if (length(Icol)>0)
       for (I.time in 1:N.time.varying)
        {
         Iyear <- as.numeric(AllInput[Index+1+I.time,1])
         Rec.Ratio <- exp(as.numeric(AllInput[Index+1+I.time,Icol]))
         Rec.Ratio <- Rec.Ratio/sum(Rec.Ratio)
         StockDetails$Recr.split[Iyear,] <- Rec.Ratio
       }
     } # length(Icol)
   } # Time-varying

# ----------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  Index <- which(AllInput[,1]=="#Maximum_F"); CheckError(Index,"Maximum F")
  StockDetails$max_F  <- as.numeric(AllInput[Index,2]);    
  Index <- which(AllInput[,1]=="#Use_Initial_F"); CheckError(Index,"Use initial F")
  StockDetails$Use_init_F  <- AllInput[Index,2]
  Index <- which(AllInput[,1]=="#Initial_F"); CheckError(Index,"Initial F")
  StockDetails$Set_initial_F  <- as.numeric(AllInput[Index+1,1:General$Nfleet]);    
  
# ----------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  # Read in selectivity (size)
  Selex.Len.Initial <- array(NA,dim=c(Nsex,General$Nfleet,Nlen))
  SelLen <- array(NA,dim=c(Nsex,General$Nfleet,Nlen,Nhist+General$Nproj+1))
  for (Isex in 1:Nsex)
   {
    if (Isex == 1) addv <- "fem" else addv <- "mal"
    
    Index <- which(AllInput[,1]==paste0("#Selex:Size_Initial_",addv)); CheckError(Index,"Size selectivity (initial)")
    for (Ifleet in 1:General$Nfleet) Selex.Len.Initial[Isex,Ifleet,] <- as.numeric(AllInput[Index+Ifleet,1:Nlen]);
    SelLen[Isex,,,1] <- Selex.Len.Initial[Isex,,]
    for (Ifleet in 1:General$Nfleet)
     for (Year in 2:(Nhist+General$Nproj+1))
     { SelLen[Isex,Ifleet,,Year] <- SelLen[Isex,Ifleet,,1] }      
  
    Index <- which(AllInput[,1]==paste0("#Selex:Size_Nblocks_",addv)); CheckError(Index,"Size selectivity (nblocks)")
    N.selex.blocks <- as.numeric(AllInput[Index+1,1:General$Nfleet])
    Index <- which(AllInput[,1]==paste0("#Selex:Size_Blocks_",addv)); CheckError(Index,"Size selectivity (blocks)")
    Index2 <- which(AllInput[,1]==paste0("#Selex:Size_Block.selex_",addv)); CheckError(Index2,"Size selectivity (blocks selex)")
    Ipnt <- 0
    for (Ifleet in 1:General$Nfleet)
     if (N.selex.blocks[Ifleet]>0)
      for (Iblk in 1:N.selex.blocks[Ifleet])  
       {
        Ipnt <- Ipnt + 1
        Sel.yr1 <- as.numeric(AllInput[Index+Ipnt,1]); Sel.yr2 <- as.numeric(AllInput[Index+Ipnt,2]);
        if (Sel.yr2==Nhist) Sel.yr2 <- Nhist+General$Nproj+1
        Sels <-  as.numeric(AllInput[Index2+Ipnt,1:Nlen]);
        for (Iyr in Sel.yr1:Sel.yr2) SelLen[Isex,Ifleet,,Iyr] <- Sels
       }
   } # Isex
  
  
  # Read in retention (size)
  RetLen <- array(1,dim=c(Nsex,General$Nfleet,Nlen,Nhist+General$Nproj+1))
  Retain.Len.Initial <- array(NA,dim=c(Nsex,General$Nfleet,Nlen))
  for (Isex in 1:Nsex)
   {
    if (Isex == 1) addv <- "fem" else addv <- "mal"
    
    Index <- which(AllInput[,1]==paste0("#Retain:Size_Initial_",addv)); CheckError(Index,"Size retension (initial)")
    for (Ifleet in 1:General$Nfleet) Retain.Len.Initial[Isex,Ifleet,] <- as.numeric(AllInput[Index+Ifleet,1:Nlen]);
    RetLen[Isex,,,1] <- Retain.Len.Initial[Isex,,]
    for (Ifleet in 1:General$Ncat_fleet)
     for (Year in 2:(Nhist+General$Nproj+1))
      { RetLen[Isex,Ifleet,,Year] <- RetLen[Isex,Ifleet,,1] }      
  
    Index <- which(AllInput[,1]==paste0("#Retain:Size_Nblocks_",addv)); CheckError(Index,"Size retension (nblocks)")
    N.retain.blocks <- as.numeric(AllInput[Index+1,1:General$Ncat_fleet]); 
    Index <- which(AllInput[,1]==paste0("#Retain:Size_Blocks_",addv)); CheckError(Index,"Size retension (blocks)")
    Index2 <- which(AllInput[,1]==paste0("#Retain:Size_Block.selex_",addv)); CheckError(Index2,"Size retention (blocks selex)")
    Ipnt <- 0
    for (Ifleet in 1:General$Ncat_fleet)
     if (N.retain.blocks[Ifleet]>0)
      for (Iblk in 1:N.retain.blocks[Ifleet])  
       {
        Ipnt <- Ipnt + 1
        Sel.yr1 <- as.numeric(AllInput[Index+Ipnt,1]); Sel.yr2 <- as.numeric(AllInput[Index+Ipnt,2]);
        if (Sel.yr2==Nhist) Sel.yr2 <- Nhist+General$Nproj+1
        Sels <-  as.numeric(AllInput[Index2+Ipnt,1:Nlen]);
        for (Iyr in Sel.yr1:Sel.yr2) RetLen[Isex,Ifleet,,Iyr] <- Sels
       }
   }
    
  # Read in discard mortality (size)
  Mort.Len.Initial <- array(NA,dim=c(Nsex,General$Nfleet,Nlen))
  MortLen <- array(1,dim=c(Nsex,General$Nfleet,Nlen,Nhist+General$Nproj+1))
  for (Isex in 1:Nsex)
   {
    if (Isex == 1) addv <- "fem" else addv <- "mal"
    
    Index <- which(AllInput[,1]==paste0("#Mort:Size_Initial_",addv)); CheckError(Index,"Size mortality (initial)")
    for (Ifleet in 1:General$Nfleet) Mort.Len.Initial[Isex,Ifleet,] <- as.numeric(AllInput[Index+Ifleet,1:Nlen]);
    MortLen[Isex,,,1] <- Mort.Len.Initial[Isex,,]
    for (Ifleet in 1:General$Ncat_fleet)
     for (Year in 2:(Nhist+General$Nproj+1))
      { MortLen[Isex,Ifleet,,Year] <- MortLen[Isex,Ifleet,,1] }      
   }
    
  StockDetails$SelSelLen <- SelLen 
  StockDetails$RetSelLen <- RetLen 
  StockDetails$MortSelLen <- MortLen 

  # Read in selectivity (age)
  Selex.Age.Initial <- array(NA,dim=c(Nsex,General$Nfleet,MaxAge))
  SelAge <- array(NA,dim=c(Nsex,General$Nfleet,MaxAge,Nhist+General$Nproj+1))
  for (Isex in 1:Nsex)
   {
    if (Isex == 1) addv <- "fem" else addv <- "mal"
    
    Index <- which(AllInput[,1]==paste0("#Selex:Age_Initial_",addv)); CheckError(Index,"Age selectivity (initial)")
    for (Ifleet in 1:General$Nfleet) Selex.Age.Initial[Isex,Ifleet,] <- as.numeric(AllInput[Index+Ifleet,1:MaxAge]);
    SelAge[Isex,,,1] <- Selex.Age.Initial[Isex,,]
    for (Ifleet in 1:General$Nfleet)
     for (Year in 2:(Nhist+General$Nproj+1))
      { SelAge[Isex,Ifleet,,Year] <- SelAge[Isex,Ifleet,,1] }      

    Index <- which(AllInput[,1]==paste0("#Selex:Age_Nblocks_",addv)); CheckError(Index,"Age selectivity (nblocks)")
    N.selex.blocks <- as.numeric(AllInput[Index+1,1:General$Nfleet])
    Index <- which(AllInput[,1]==paste0("#Selex:Age_Blocks_",addv)); CheckError(Index,"Age selectivity (blocks)")
    Index2 <- which(AllInput[,1]==paste0("#Selex:Age_Block.selex_",addv)); CheckError(Index2,"Age selectivity (blocks selex)")
    Ipnt <- 0
    for (Ifleet in 1:General$Nfleet)
     if (N.selex.blocks[Ifleet]>0)
      for (Iblk in 1:N.selex.blocks[Ifleet])  
       {
        Ipnt <- Ipnt + 1
        Sel.yr1 <- as.numeric(AllInput[Index+Ipnt,1]); Sel.yr2 <- as.numeric(AllInput[Index+Ipnt,2]);
        if (Sel.yr2==Nhist) Sel.yr2 <- Nhist+General$Nproj+1
        Sels <-  as.numeric(AllInput[Index2+Ipnt,1:MaxAge]);
        for (Iyr in Sel.yr1:Sel.yr2) SelAge[Ifleet,,Iyr] <- Sels
      }
   }
  
  # Read in retention (age)
  Retain.Age.Initial <-  array(NA,dim=c(Nsex,General$Nfleet,MaxAge))
  RetAge <- array(1,dim=c(Nsex,General$Nfleet,MaxAge,Nhist+General$Nproj+1))
  for (Isex in 1:Nsex)
   {
    if (Isex == 1) addv <- "fem" else addv <- "mal"

    Index <- which(AllInput[,1]==paste0("#Retain:Age_Initial_",addv)); CheckError(Index,"Age retension (initial)")
    for (Ifleet in 1:General$Nfleet) Retain.Age.Initial[Isex,Ifleet,] <- as.numeric(AllInput[Index+Ifleet,1:MaxAge]);
    RetAge[Isex,,,1] <- Retain.Age.Initial[Isex,,]
    for (Ifleet in 1:General$Ncat_fleet)
     for (Year in 2:(Nhist+General$Nproj+1))
      { RetAge[Isex,Ifleet,,Year] <- RetAge[Isex,Ifleet,,1] }      
  
    Index <- which(AllInput[,1]==paste0("#Retain:Age_Nblocks_",addv)); CheckError(Index,"Age retension (nblocks)")
    N.retain.blocks <- as.numeric(AllInput[Index+1,1:General$Ncat_fleet])
    Index <- which(AllInput[,1]==paste0("#Retain:Age_Blocks_",addv)); CheckError(Index,"Age retension (blocks)")
    Index2 <- which(AllInput[,1]==paste0("#Retain:Age_Block.selex_",addv)); CheckError(Index2,"Age retension (blocks selex)")
    Ipnt <- 0
    for (Ifleet in 1:General$Ncat_fleet)
     if (N.retain.blocks[Ifleet]>0)
      for (Iblk in 1:N.retain.blocks[Ifleet])  
       {
        Ipnt <- Ipnt + 1
        Sel.yr1 <- as.numeric(AllInput[Index+Ipnt,1]); Sel.yr2 <- as.numeric(AllInput[Index+Ipnt,2]);
        if (Sel.yr2==Nhist) Sel.yr2 <- Nhist+General$Nproj+1
        Sels <-  as.numeric(AllInput[Index2+Ipnt,1:MaxAge]);
        for (Iyr in Sel.yr1:Sel.yr2) RetAge[Isex,Ifleet,,Iyr] <- Sels
       }
   } # isex
    
  # Read in discard mortality (size)
  Mort.Age.Initial <- array(NA,dim=c(Nsex,General$Nfleet,MaxAge))
  MortAge <- array(1,dim=c(Nsex,General$Nfleet,MaxAge,Nhist+General$Nproj+1))
  for (Isex in 1:Nsex)
   {
    if (Isex == 1) addv <- "fem" else addv <- "mal"
    
    Index <- which(AllInput[,1]==paste0("#Mort:Age_Initial_",addv)); CheckError(Index,"Age mortality (initial)")
    for (Ifleet in 1:General$Nfleet) Mort.Age.Initial[Isex,Ifleet,] <- as.numeric(AllInput[Index+Ifleet,1:MaxAge]);
    # Fill in retention
    MortAge[Isex,,,1] <- Mort.Age.Initial[Isex,,]
    for (Ifleet in 1:General$Ncat_fleet)
      for (Year in 2:(Nhist+General$Nproj+1))
      { MortAge[Isex,Ifleet,,Year] <- MortAge[Isex,Ifleet,,1] }      
   } # isex
 
  StockDetails$SelSelAge <- SelAge
  StockDetails$RetSelAge <- RetAge 
  StockDetails$MortSelAge <- MortAge 

  # Selectivity as a function of age
  SelexAge <- SelAge

  # Convert from selectivity as a function of length to selectivity as a function of age
  SelAge <- array(0,dim=c(General$Nfleet,Nsex,MaxAge,Nhist+General$Nproj+1))
  SelWghtAge <- array(0,dim=c(General$Nfleet,Nsex,MaxAge,Nhist+General$Nproj+1))
  SelRetWghtAge <- array(0,dim=c(General$Nfleet,Nsex,MaxAge,Nhist+General$Nproj+1))
  for (Ifleet in 1:General$Nfleet) 
   for (Isex in 1:Nsex)
    for (Iage in 1:MaxAge)
     for (Year in 1:(Nhist+General$Nproj+1))
      {
       if (Ifleet <=General$Ncat_fleet)
        SelAge[Ifleet,Isex,Iage,Year] <- SelexAge[Isex,Ifleet,Iage,Year]*(RetAge[Isex,Ifleet,Iage,Year]+(1-RetAge[Isex,Ifleet,Iage,Year])*MortAge[Isex,Ifleet,Iage,Year])*sum(SelLen[Isex,Ifleet,,Year]*(RetLen[Isex,Ifleet,,Year]+(1-RetLen[Isex,Ifleet,,Year])*MortLen[Isex,Ifleet,,Year])*FracLenM[Isex,Iage,Year,])    
       else
        SelAge[Ifleet,Isex,Iage,Year] <- SelexAge[Isex,Ifleet,Iage,Year]*sum(SelLen[Isex,Ifleet,,Year]*FracLenM[Isex,Iage,Year,])    
       SelWghtAge[Ifleet,Isex,Iage,Year] <- sum(SelLen[Isex,Ifleet,,Year]*WtLen[Isex,]*FracLenM[Isex,Iage,Year,])
       SelRetWghtAge[Ifleet,Isex,Iage,Year] <- SelexAge[Isex,Ifleet,Iage,Year]*RetAge[Isex,Ifleet,Iage,Year]*sum(SelLen[Isex,Ifleet,,Year]*WtLen[Isex,]*RetLen[Isex,Ifleet,,Year]*FracLenM[Isex,Iage,Year,])
      }
  StockDetails$SelAge <- SelAge
  StockDetails$SelWghtAge <- SelWghtAge
  StockDetails$SelRetWghtAge <- SelRetWghtAge
  StockDetails$SelexAge <- SelexAge
  if (FullOutput==T) cat("Done: Selectivity\n")
  #if (FullOutput==T) str(StockDetails)
  
  # ----------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  # Specify R0, Steepness and recruitment deviations
  Index <- which(AllInput[,1]=="#SR:R0"); CheckError(Index,"Unfished recruitment")
  StockDetails$R00 <- rep(as.numeric(AllInput[Index,2]),General$Narea)*StockDetails$Relative.Density;
  StockDetails$R00.orig <- StockDetails$R00
  Index <- which(AllInput[,1]=="#SR:Steepness"); CheckError(Index,"Steepness")
  StockDetails$Steep <- as.numeric(AllInput[Index,2]);
  Index <- which(AllInput[,1]=="#SR:SigmaR"); CheckError(Index,"SigmaR")
  StockDetails$SigmaR <- as.numeric(AllInput[Index,2]);
  StockDetails$ProwR <- 0
  Index <- which(AllInput[,1]=="#SR:ProwR")
  if (length(Index) > 0) StockDetails$ProwR <- as.numeric(AllInput[Index,2]);
  Index <- which(AllInput[,1]=="#SR:Use_Steepness_Equi"); CheckError(Index,"Steepness in unfished")
  StockDetails$Use_Steep_in_equ <- as.numeric(AllInput[Index+1,1]);
  if (FullOutput==T) cat("Done: Recruitment (primary)\n")

  # Read in deviations in recruitment (early - pre-first year) and main period (note there is no bias-correction here)
  StockDetails$EarlyDevs <- matrix(0,nrow=General$Narea,MaxAge); Use.Early.Devs <- 0
  Index <- which(AllInput[,1]=="#Early_devs")
  if (length(Index) >0) 
   for (Iarea in 1:General$Narea)  
    StockDetails$EarlyDevs[Iarea,1:MaxAge] <- as.numeric(AllInput[Index+1,1:MaxAge]);
  StockDetails$Early.R0.mult <- 0
  Index <- which(AllInput[,1]=="#Early_R0Mult"); StockDetails$Use.Early.R0.mult <- length(Index)
  if (length(Index)>0) StockDetails$Early.R0.mult <- as.numeric(AllInput[Index,2]);
  StockDetails$Early.R0.mult_end_yrs <- c(0,0)
  Index <- which(AllInput[,1]=="#Early_R0Mult_Block_end")
  if (length(Index)>0) StockDetails$Early.R0.mult.end.yrs <- as.numeric(AllInput[Index,2:3]);
  Index <- which(AllInput[,1]=="#Late_Devs")
  StockDetails$Late.range <- c(-1,-1)
  if (length(Index)>0) StockDetails$Late.range <- as.numeric(AllInput[Index,2:3]);
  if (StockDetails$Late.range[1] >0)
   {
     StockDetails$LateDevs[1:(StockDetails$Late.range[2]-StockDetails$Late.range[1]+1)] <- as.numeric(AllInput[Index+1,1:(StockDetails$Late.range[2]-StockDetails$Late.range[1]+1)]);  
     if (StockDetails$LateDevs[StockDetails$Late.range[2]-StockDetails$Late.range[1]+1] == 0) cat("WARNING: Last rec dev is zero\n")
   }
  if (FullOutput==T) cat("done: Devs\n")

  # Set up a R0 vector
  R0 <- matrix(StockDetails$R00,nrow=General$Narea,ncol=Nhist+General$Nproj+1)
  if (StockDetails$Early.R0.mult!=0)
    if (StockDetails$Early.R0.mult.end.yrs[2] >0)
     for (Iyr in StockDetails$Early.R0.mult.end.yrs[1]:StockDetails$Early.R0.mult.end.yrs[2])  
      for (Iarea in 1:General$Narea)
       R0[Iarea,Iyr] <- R0[Iarea,Iyr]*exp(StockDetails$Early.R0.mult)
  if (StockDetails$Early.R0.mult!=0) 
    if (StockDetails$Early.R0.mult.end.yrs[1]<1) StockDetails$R00 <- StockDetails$R00*exp(StockDetails$Early.R0.mult)   
  StockDetails$R0 <- R0
  
  # Generate recruitment deviations
  set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
  
  # Set up devs before the first year post pre-specified devs
  TestDevs <- matrix(0,nrow=General$Narea,ncol=Nhist+General$Nproj+1)
  StockDetails$Rec_devs <- matrix(0,nrow=General$Narea,ncol=Nhist+General$Nproj+1)
    
  for (Iarea in 1:General$Narea)
   { 
    if (StockDetails$Late.range[1] == -1) 
     { StartProw <- 2; TestDevs[Iarea,1] <- rnorm(1,0, StockDetails$SigmaR); if (TestCase==T || Deter.Rec.devs==T) TestDevs[Iarea,1] <- 0; } 
    else 
     { StartProw <- StockDetails$Late.range[2]+1; TestDevs[Iarea,StockDetails$Late.range[1]:StockDetails$Late.range[2]] <- StockDetails$LateDevs; }
    # Now generate the devs
    for (Iyr in StartProw:(Nhist+General$Nproj+1))
     {
      TestDevs[Iarea,Iyr] <- StockDetails$ProwR*TestDevs[Iarea,Iyr-1] + sqrt(1 - StockDetails$ProwR^2)*rnorm(1, 0, StockDetails$SigmaR)
      if (TestCase==T || Deter.Rec.devs==T) TestDevs[Iarea,Iyr] <- 0
     }

    # Now clean up with bias-correction
   if (StockDetails$Late.range[1] == -1) 
     {
      TestDevs[Iarea,1] <- TestDevs[Iarea,1] - StockDetails$SigmaR^2.0/2.0
      if (TestCase==T || Deter.Rec.devs==T) TestDevs[Iarea,1] <- 0
     } 
    for (Iyr in StartProw:(Nhist+General$Nproj+1))
     {
      TestDevs[Iarea,Iyr] <- TestDevs[Iarea,Iyr] - StockDetails$SigmaR^2.0/2.0
      if (TestCase==T || Deter.Rec.devs==T) TestDevs[Iarea,Iyr] <- 0
     }
    StockDetails$Rec_devs[Iarea,] <- TestDevs[Iarea,]
   } # Iarea

  # Account for recruitment deviations linked to an environmental variable
  StockDetails$R0.Env.Mult <- rep(1,Nhist+General$Nproj+1)
  if (StockDetails$N.env.links>0)
   {
    Ilink <- which(StockDetails$LinkVarsType=="Regime") 
    if (length(Ilink)==1)
     for (t in 1:(Nhist+General$Nproj+1))
       StockDetails$R0.Env.Mult[t] <- exp(StockDetails$LinkVals[Ilink]*EnvData[t,StockDetails$LinkVars[Ilink]])
   }
  if (FullOutput==T) cat("Done: Recruitment (other)\n")
 
  # ----------------------------------------------------------------------------------------------------------------------------------------------------------  

  Index <- which(AllInput[,1]=="#Catches:Project_type"); CheckError(Index,"Catch data type")
  CatchType <- as.numeric(AllInput[Index+1,1])
  Index <- which(AllInput[,1]=="#Catches:Eqilibrium"); CheckError(Index,"Equilibrium catch data")
  Equilbrium.catch <- as.numeric(AllInput[Index+1,1:(General$Ncat_fleet)]);
  Index <- which(AllInput[,1]=="#Catches:Historical"); CheckError(Index,"Annual catch data")
  CatchInp <- matrix(NA,nrow=Nhist,ncol=General$Ncat_fleet)
  for (Year in 1:Nhist) CatchInp[Year,] <- as.numeric(AllInput[Index+Year,2:(General$Ncat_fleet+1)]);
  StockDetails$CatchType <- CatchType
  StockDetails$CatchInp <- CatchInp
  StockDetails$Equilbrium.catch <- Equilbrium.catch
  F.Selex.Inp <- array(0,dim=c(Narea,General$Ncat_fleet,Nhist))
  ## AEP FIX
  #for (Ifleet in 1:General$Ncat_fleet)
  # for (Iyear in 1:Nhist)
  #  F.Selex.Inp[Open.fleets[Ifleet],Ifleet,Iyear] <- OM.obj$Fleet.F.data[Iyear,Ifleet] 
  StockDetails$F.Selex.Inp <- F.Selex.Inp
  if (FullOutput==T) cat("Done: Catch\n")

  # ==========================================================================================================================================
  # ==========================================================================================================================================
  # predation-related stuff
  
  Num.Pred <- 0
  Index <- which(AllInput[,1]=="#Pred:Number_of_predators")
  if (length(Index)!=0) Num.Pred <- as.numeric(AllInput[Index+1,1])
  if (FullOutput==T) cat("Number of predators:",Num.Pred,"\n") 
  if (Num.Pred > 0)
   {
    Index <- which(AllInput[,1]=="#Pred:BaseM")
    Pred.BaseM <- as.numeric(AllInput[Index+1,1:Num.Pred])
    # Predator trends
    Pred.No <- matrix(NA,nrow=Nhist+General$Nproj+1,ncol=Num.Pred)
    Index <- which(AllInput[,1]=="#Pred_abundance") 
    for (Iyear in 1:(Nhist+General$Nproj+1))
     for (Ipred in 1:Num.Pred) Pred.No[Iyear,Ipred] <-  as.numeric(AllInput[Index+Iyear,1+Ipred])  
    # Predator select
    Pred.Selex <- matrix(NA,nrow=Num.Pred,ncol=MaxAge)
    Index <- which(AllInput[,1]=="#Pred:Selex") 
    for (Ipred in 1:Num.Pred) Pred.Selex[Ipred,] <-  as.numeric(AllInput[Index+Ipred,1:MaxAge])
    # compute M2
    M2 <- array(Mbase,dim=c(Num.Pred,Nsex,MaxAge,Nhist+General$Nproj))
    for (Iyear in 1:(Nhist+General$Nproj))
     for (Ipred in 1:Num.Pred) 
      for (Isex in 1:Nsex)
       for (Iage in 1:MaxAge)
        {
         M2[Ipred,Isex,Iage,Iyear] <- Pred.BaseM[Ipred]*Pred.Selex[Ipred,Iage]*Pred.No[Iyear,Ipred]
         StockDetails$M[Isex,Iage,Iyear] <-  StockDetails$M[Isex,Iage,Iyear] + M2[Ipred,Isex,Iage,Iyear]
        }
    StockDetails$M2 <- M2
    StockDetails$Pred.BaseM <- Pred.BaseM
    StockDetails$Pred.No <- Pred.No
    StockDetails$ConsumpTotal <- matrix(0,nrow=Num.Pred,ncol=Nhist+General$Nproj)
    StockDetails$CompumpAtAge <- array(NA,dim=c(Num.Pred,Nsex,MaxAge,Nhist+General$Nproj))
    
   }
  StockDetails$Num.Pred <- Num.Pred
  if (FullOutput==T) cat("Done: Predators\n")

  # ==========================================================================================================================================
  # ==========================================================================================================================================
  # Data-related stuff
  StockDetails$Data <- NULL
  
  Projected.Data.Spec <- 0
  Index <- which(AllInput[,1]=="#Extended_Data"); 
  if (length(Index) > 0)
   {
    Index <- which(AllInput[,1]=="#Extended_data_length"); CheckError(Index,"No extended data length")
    Projected.Data.Spec <- as.numeric(AllInput[Index,2])
    if (Projected.Data.Spec > General$Nproj) Projected.Data.Spec <- General$Nproj
   }

  # Catch data
  # ==========
  StockDetails$Data$Catches <- array(NA,dim=c(NassArea,General$Ncat_fleet,Nhist+General$Nproj+1))

  # Index data
  # ==========
  Index <- which(AllInput[,1]=="#Index:Is_index"); CheckError(Index,"Is there index data")
  StockDetails$Data$Is_index <- AllInput[Index+1,1:General$Nfleet]
  Index <- which(AllInput[,1]=="#Index:Base_CVs_past"); CheckError(Index,"Historical index CVs")
  StockDetails$Data$Index.cv.past <- as.numeric(AllInput[Index+1,1:General$Nfleet])
  if (Huge.sample.sizes==T) { Index <- which(StockDetails$Data$Index.cv.past>0); StockDetails$Data$Index.cv.past[Index] <- 0.001 }
  Index <- which(AllInput[,1]=="#Index:Base_CVs_future"); CheckError(Index,"Future index CVs")
  StockDetails$Data$Index.cv.future <- as.numeric(AllInput[Index+1,1:General$Nfleet])
  if (Huge.sample.sizes==T) { Index <- which(StockDetails$Data$Index.cv.future>0); StockDetails$Data$Index.cv.future[Index] <- 0.001 }
  StockDetails$Data$Index.Beta <- rep(1,General$Nfleet)
  Index <- which(AllInput[,1]=="#Index:Beta"); if (length(Index>0)) StockDetails$Data$Index.Beta <- as.numeric(AllInput[Index+1,1:General$Nfleet])
  StockDetails$Data$Index.Start.Qinc <- rep(1,General$Nfleet)
  Index <- which(AllInput[,1]=="#Index:Qinc.Yr1"); if (length(Index>0)) StockDetails$Data$Index.Start.Qinc <- as.numeric(AllInput[Index+1,1:General$Nfleet]) - StockDetails$YrOffset
  StockDetails$Data$Index.rate.Qinc <- rep(0,General$Nfleet)
  Index <- which(AllInput[,1]=="#Index:Qinc.Rate"); if (length(Index>0)) StockDetails$Data$Index.rate.Qinc <- as.numeric(AllInput[Index+1,1:General$Nfleet])
  Index <- which(AllInput[,1]=="#Index:Frequency_future"); CheckError(Index,"Frequency of index data")
  StockDetails$Data$Index.freq.future <- as.numeric(AllInput[Index+1,1:General$Nfleet])
  Index <- which(AllInput[,1]=="#Index:q"); CheckError(Index,"Index catchability")
  StockDetails$Data$Index.q <- as.numeric(AllInput[Index+1,1:General$Nfleet])
  StockDetails$Data$Index.CV.bias <- rep(1,General$Nfleet)
  Index <- which(AllInput[,1]=="#Index:CV.bias"); if (length(Index>0)) StockDetails$Data$Index.CV.bias <- as.numeric(AllInput[Index+1,1:General$Nfleet])
  Index <- which(AllInput[,1]=="#Index:Frequency_past"); CheckError(Index,"Historical index frequency")
  Index.spec <- array(-1,dim=c(NassArea,Nhist+General$Nproj+1,General$Nfleet))

  # Fill in the index matrix
  for (IassArea in 1:NassArea)
   {
    CVMult <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:(Nhist+Projected.Data.Spec))
     {
      Index.spec[IassArea,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Nfleet+1)])
      for (Ifleet in 1:General$Nfleet)
       if (Index.spec[IassArea,Year,Ifleet] >= 0) Index.spec[IassArea,Year,Ifleet] <- Index.spec[IassArea,Year,Ifleet]*StockDetails$Data$Index.cv.past[Ifleet]*CVMult 
     } # Year
    for (Ifleet in 1:General$Nfleet)
     if ( StockDetails$Data$Index.freq.future[Ifleet]>0)
      for (Year in (Projected.Data.Spec+1):(General$Nproj+1))
       if ((Year-1) %% StockDetails$Data$Index.freq.future[Ifleet] == 0) Index.spec[IassArea,Year+Nhist,Ifleet] <- StockDetails$Data$Index.cv.future[Ifleet]*CVMult 
   } # IassArea
    
  # Generate the devs for indexes
  set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
  Index.devs <- array(-1,dim=c(NassArea,Nhist+General$Nproj+1,General$Nfleet))
  for (IassArea in 1:NassArea)
   for (Year in 1:(Nhist+General$Nproj+1))
    for (Ifleet in 1:General$Nfleet)
     Index.devs[IassArea,Year,Ifleet] <- rnorm(1,0,1)*Index.spec[IassArea,Year,Ifleet]- Index.spec[IassArea,Year,Ifleet]^2/2.0
  StockDetails$Data$Index.spec <- Index.spec
  StockDetails$Data$Index.devs <- Index.devs
  StockDetails$Data$IndexData <- array(NA,dim=c(NassArea,General$Nfleet*(Nhist+General$Nproj+1),6))
  StockDetails$Data$NindexData <- rep(0,NassArea)
  if (FullOutput==T) cat("Done: Index readin\n")

  # Discard data
  # ============
  Index <- which(AllInput[,1]=="#Discard:Is_data"); CheckError(Index,"Is there discard data")
  StockDetails$Data$Is_discard <- AllInput[Index+1,1:General$Ncat_fleet]
  Index <- which(AllInput[,1]=="#Discard:type"); CheckError(Index,"Discard type")
  StockDetails$Data$Discard.type <- as.numeric(AllInput[Index+1,1:General$Ncat_fleet])
  Index <- which(AllInput[,1]=="#Discard:Base_CVs_past"); CheckError(Index,"Historical discard CVs")
  StockDetails$Data$Discard.cv.past <- as.numeric(AllInput[Index+1,1:General$Ncat_fleet])
  if (Huge.sample.sizes==T) { Index <- which(StockDetails$Data$Discard.cv.past>0); StockDetails$Data$Discard.cv.past[Index] <- 0.001 }
  Index <- which(AllInput[,1]=="#Discard:Base_CVs_future"); CheckError(Index,"Future discard CVs")
  StockDetails$Data$Discard.cv.future <- as.numeric(AllInput[Index+1,1:General$Ncat_fleet])
  if (Huge.sample.sizes==T) { Index <- which(StockDetails$Data$Discard.cv.future>0); StockDetails$Data$Discard.cv.future[Index] <- 0.001 }
  Index <- which(AllInput[,1]=="#Discard:Frequency_future"); CheckError(Index,"Frequency of discard data")
  StockDetails$Data$Discard.freq.future <- as.numeric(AllInput[Index+1,1:General$Ncat_fleet])
  Index <- which(AllInput[,1]=="#Discard:Frequency_past"); CheckError(Index,"Historical discard frequency")
  Discard.spec <- array(-1,dim=c(NassArea,Nhist+General$Nproj+1,General$Ncat_fleet))

  # Fill in the discard matrix
  for (IassArea in 1:NassArea)
   {
    CVMult <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:(Nhist+Projected.Data.Spec))
     { 
      Discard.spec[IassArea,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Ncat_fleet+1)])
      for (Ifleet in 1:General$Ncat_fleet) 
       {
        if (Year <= Nhist) { if (CatchInp[Year,Ifleet] <= 0 || StockDetails$Data$Is_discard[Ifleet]=="No") Discard.spec[IassArea,Year,Ifleet] <- -1 }
        if (Year > Nhist) { if (StockDetails$Data$Is_discard[Ifleet]=="No") Discard.spec[IassArea,Year,Ifleet] <- -1 }
       }
      for (Ifleet in 1:General$Ncat_fleet)
       if (Discard.spec[IassArea,Year,Ifleet] >= 0) Discard.spec[IassArea,Year,Ifleet] <- Discard.spec[IassArea,Year,Ifleet]*StockDetails$Data$Discard.cv.past[Ifleet]*CVMult 
     } # Year
    for (Ifleet in 1:General$Ncat_fleet)
     if ( StockDetails$Data$Discard.freq.future[Ifleet]>0)
      for (Year in (Projected.Data.Spec+1):(General$Nproj+1))
        if ((Year-1) %% StockDetails$Data$Discard.freq.future[Ifleet] == 0) Discard.spec[IassArea,Year+Nhist,Ifleet] <- StockDetails$Data$Discard.cv.future[Ifleet]*CVMult 
   } # IassArea
  
  # Generate the devs for discard discard
  set.seed(Seed); NextSeed <- floor(runif(1,1,1000000))
  Discard.devs <- array(-1,dim=c(NassArea,Nhist+General$Nproj+1,General$Nfleet))
  for (IassArea in 1:NassArea)
   for (Year in 1:(Nhist+General$Nproj+1))
    for (Ifleet in 1:General$Ncat_fleet)
      Discard.devs[IassArea,Year,Ifleet] <- rnorm(1,0,1)*Discard.spec[IassArea,Year,Ifleet]- Discard.spec[IassArea,Year,Ifleet]^2/2.0
  StockDetails$Data$Discard.spec <- Discard.spec
  StockDetails$Data$Discard.devs <- Discard.devs
  StockDetails$Data$DiscardData <- matrix(NA,nrow=General$Nfleet*(Nhist+General$Nproj+1),ncol=5)
  StockDetails$Data$NdiscardData <- 0
  if (FullOutput==T) cat("Done: Discard readin\n")
  
  # Length data
  # ===========
  Index <- which(AllInput[,1]=="#Length:Is_length"); CheckError(Index,"Is there length data")
  StockDetails$Data$Is_length <- AllInput[Index+1,1:General$Nfleet]
  Index <- which(AllInput[,1]=="#Length:Base_EFN_past"); CheckError(Index,"Historical length EFNs")
  StockDetails$Data$length.eff.past <- matrix(0,nrow=3,ncol=General$Nfleet)
  for (Itype in 1:3) StockDetails$Data$length.eff.past[Itype,] <- as.numeric(AllInput[Index+Itype,1:General$Nfleet])
  if (Huge.sample.sizes==T) { Index <- which(StockDetails$Data$length.eff.past>0); StockDetails$Data$length.eff.past[Index] <- 10000 }
  Index <- which(AllInput[,1]=="#Length:Base_EFN_future"); CheckError(Index,"Future length EFNs")
  StockDetails$Data$length.eff.future <- matrix(0,nrow=3,ncol=General$Nfleet)
  for (Itype in 1:3) StockDetails$Data$length.eff.future[Itype,] <- as.numeric(AllInput[Index+Itype,1:General$Nfleet])
  if (Huge.sample.sizes==T) { Index <- which(StockDetails$Data$length.eff.future>0); StockDetails$Data$length.eff.future[Index] <- 10000 }
  Index <- which(AllInput[,1]=="#Length:Frequency_future"); CheckError(Index,"Frequency of length data")
  StockDetails$Data$length.freq.future <- matrix(0,nrow=3,ncol=General$Nfleet)
  for (Itype in 1:3) StockDetails$Data$length.freq.future[Itype,] <- as.numeric(AllInput[Index+Itype,1:General$Nfleet])

  Length.spec <- array(-1,dim=c(NassArea,3,Nhist+General$Nproj+1,General$Nfleet))
  Itype <- 1
  Index <- which(AllInput[,1]=="#Length:Frequency_past_retained"); Itype <- 1; CheckError(Index,"Historical retained length data frequency")
  for (IassArea in 1:NassArea)
   {
    EffN.Area <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:(Nhist+Projected.Data.Spec))
     {
      Length.spec[IassArea,Itype,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Nfleet+1)])
      for (Ifleet in 1:General$Nfleet)
      if (Length.spec[IassArea,Itype,Year,Ifleet] >= 0) Length.spec[IassArea,Itype,Year,Ifleet] <- Length.spec[IassArea,Itype,Year,Ifleet]*StockDetails$Data$length.eff.past[Itype,Ifleet]*EffN.Area
     }
    for (Ifleet in 1:General$Nfleet)
     if ( StockDetails$Data$length.freq.future[Itype,Ifleet]>0)
      for (Year in (Projected.Data.Spec+1):(General$Nproj+1))
       if ((Year-1) %% StockDetails$Data$length.freq.future[Itype,Ifleet] == 0) Length.spec[IassArea,Itype,Year+Nhist,Ifleet] <- StockDetails$Data$length.eff.future[Itype,Ifleet]*EffN.Area
   } # IassArea

  Itype <- 2
  Index <- which(AllInput[,1]=="#Length:Frequency_past_discarded"); Itype <- 2; CheckError(Index,"Historical discard length data frequency")
  for (IassArea in 1:NassArea)
   {
    EffN.Area <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:(Nhist+Projected.Data.Spec))
     {
      Length.spec[IassArea,Itype,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Nfleet+1)])
      for (Ifleet in 1:General$Nfleet)
       if (Length.spec[IassArea,Itype,Year,Ifleet] >= 0) Length.spec[IassArea,Itype,Year,Ifleet] <- Length.spec[IassArea,Itype,Year,Ifleet]*StockDetails$Data$length.eff.past[Itype,Ifleet]*EffN.Area 
     }
    for (Ifleet in 1:General$Nfleet)
      if ( StockDetails$Data$length.freq.future[Itype,Ifleet]>0)
        for (Year in (Projected.Data.Spec+1):(General$Nproj+1))
         if ((Year-1) %% StockDetails$Data$length.freq.future[Itype,Ifleet] == 0) Length.spec[IassArea,Itype,Year+Nhist,Ifleet] <- StockDetails$Data$length.eff.future[Itype,Ifleet]*EffN.Area 
   }
  
  Itype <- 3
  Index <- which(AllInput[,1]=="#Length:Frequency_past_total"); Itype <- 3; CheckError(Index,"Historical total length data frequency")
  for (IassArea in 1:NassArea)
   {
    EffN.Area <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:(Nhist+Projected.Data.Spec))
     {
      Length.spec[IassArea,Itype,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Nfleet+1)])
      for (Ifleet in 1:General$Nfleet)
       if (Length.spec[IassArea,Itype,Year,Ifleet] >= 0) Length.spec[IassArea,Itype,Year,Ifleet] <- Length.spec[IassArea,Itype,Year,Ifleet]*StockDetails$Data$length.eff.past[Itype,Ifleet]*EffN.Area 
     }
    for (Ifleet in 1:General$Nfleet)
     if ( StockDetails$Data$length.freq.future[Itype,Ifleet]>0)
       for (Year in (Projected.Data.Spec+1):(General$Nproj+1))
        if ((Year-1) %% StockDetails$Data$length.freq.future[Itype,Ifleet] == 0) Length.spec[IassArea,Itype,Year+Nhist,Ifleet] <- StockDetails$Data$length.eff.future[Itype,Ifleet]*EffN.Area 
   }
    
  # Generate the seeds
  set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
  Length.seeds <- array(-1,dim=c(NassArea,3,Nhist+General$Nproj+1,General$Nfleet))
  for (IassArea in 1:NassArea)
   for (Itype in 1:3)
    for (Year in 1:(Nhist+General$Nproj+1))
     for (Ifleet in 1:General$Nfleet)
      Length.seeds[IassArea,Itype,Year,Ifleet] <- floor(runif(1,1,1000000))
  StockDetails$Data$Length.spec <- Length.spec
  StockDetails$Data$Length.seeds <- Length.seeds
  StockDetails$Data$LengthData <- rep(0,6+Nsex*Nlen)
  StockDetails$Data$NlengthData <- 0
  if (FullOutput==T) cat("Done: Length readin\n")
  
  # Age data
  # =========
  Index <- which(AllInput[,1]=="#Age:Is_age"); CheckError(Index,"Is there age data")
  StockDetails$Data$Is_age <- AllInput[Index+1,1:General$Nfleet]
  Index <- which(AllInput[,1]=="#Age:Base_EFN_past"); CheckError(Index,"Historical age EFNs")
  StockDetails$Data$age.eff.past <- matrix(0,nrow=3,ncol=General$Nfleet)
  for (Itype in 1:3) StockDetails$Data$age.eff.past[Itype,] <- as.numeric(AllInput[Index+Itype,1:General$Nfleet])
  if (Huge.sample.sizes==T) { Index <- which(StockDetails$Data$age.eff.past>0); StockDetails$Data$age.eff.past[Index] <- 10000 }
  Index <- which(AllInput[,1]=="#Age:Base_EFN_future"); CheckError(Index,"Future length EFNs")
  StockDetails$Data$age.eff.future <- matrix(0,nrow=3,ncol=General$Nfleet)
  for (Itype in 1:3)StockDetails$Data$age.eff.future[Itype,] <- as.numeric(AllInput[Index+Itype,1:General$Nfleet])
  if (Huge.sample.sizes==T) { Index <- which(StockDetails$Data$age.eff.future>0); StockDetails$Data$age.eff.future[Index] <- 10000 }
  Index <- which(AllInput[,1]=="#Age:Frequency_future"); CheckError(Index,"Frequency of age data")
  StockDetails$Data$age.freq.future <- matrix(0,nrow=3,ncol=General$Nfleet)
  for (Itype in 1:3) StockDetails$Data$age.freq.future[Itype,] <- as.numeric(AllInput[Index+Itype,1:General$Nfleet])
  
  Age.spec <- array(-1,dim=c(NassArea,3,Nhist+General$Nproj+1,General$Nfleet))
  Itype <- 1
  Index <- which(AllInput[,1]=="#Age:Frequency_past_retained"); Itype <- 1; CheckError(Index,"Historical retained age data frequency")
  for (IassArea in 1:NassArea)
   {
    EffN.Area <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:(Nhist+Projected.Data.Spec))
     {
      Age.spec[IassArea,Itype,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Nfleet+1)])
      for (Ifleet in 1:General$Ncat_fleet) 
       if (Year <= Nhist) if (CatchInp[Year,Ifleet] <= 0) Age.spec[IassArea,Itype,Year,Ifleet] <- -1
      for (Ifleet in 1:General$Nfleet)
       if (Age.spec[IassArea,Itype,Year,Ifleet] >= 0) Age.spec[IassArea,Itype,Year,Ifleet] <- Age.spec[IassArea,Itype,Year,Ifleet]*StockDetails$Data$age.eff.past[Itype,Ifleet]*EffN.Area
     }
    for (Ifleet in 1:General$Nfleet)
     if ( StockDetails$Data$age.freq.future[Itype,Ifleet]>0)
      for (Year in (Projected.Data.Spec+1):(General$Nproj+1))
       if ((Year-1) %% StockDetails$Data$age.freq.future[Itype,Ifleet] == 0) Age.spec[IassArea,Itype,Year+Nhist,Ifleet] <- StockDetails$Data$age.eff.future[Itype,Ifleet]*EffN.Area 
   }

  Itype <- 2
  Index <- which(AllInput[,1]=="#Age:Frequency_past_discarded"); Itype <- 2; CheckError(Index,"Historical discard age data frequency")
  for (IassArea in 1:NassArea)
   {
    EffN.Area <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:(Nhist+Projected.Data.Spec))
     {
      Age.spec[IassArea,Itype,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Nfleet+1)])
      for (Ifleet in 1:General$Nfleet)
       if (Age.spec[IassArea,Itype,Year,Ifleet] >= 0) Age.spec[IassArea,Itype,Year,Ifleet] <- Age.spec[IassArea,Itype,Year,Ifleet]*StockDetails$Data$age.eff.past[Itype,Ifleet]*EffN.Area 
     }
    for (Ifleet in 1:General$Nfleet)
     if ( StockDetails$Data$age.freq.future[Itype,Ifleet]>0)
       for (Year in (Projected.Data.Spec+1):(General$Nproj+1))
        if ((Year-1) %% StockDetails$Data$age.freq.future[Itype,Ifleet] == 0) Age.spec[IassArea,Itype,Year+Nhist,Ifleet] <- StockDetails$Data$age.eff.future[Itype,Ifleet]*EffN.Area 
   }
  
  Itype <- 3
  Index <- which(AllInput[,1]=="#Age:Frequency_past_total"); Itype <- 3; CheckError(Index,"Historical total age data frequency")
  for (IassArea in 1:NassArea)
   {
    EffN.Area <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:Nhist)
     {
      Age.spec[IassArea,Itype,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Nfleet+1)])
      for (Ifleet in 1:General$Nfleet)
       if (Age.spec[IassArea,Itype,Year,Ifleet] >= 0) Age.spec[IassArea,Itype,Year,Ifleet] <- Age.spec[IassArea,Itype,Year,Ifleet]*StockDetails$Data$age.eff.past[Itype,Ifleet]*EffN.Area 
     }
    for (Ifleet in 1:General$Nfleet)
     if ( StockDetails$Data$age.freq.future[Itype,Ifleet]>0)
      for (Year in 1:(General$Nproj+1))
       if ((Year-1) %% StockDetails$Data$age.freq.future[Itype,Ifleet] == 0) Age.spec[IassArea,Itype,Year+Nhist,Ifleet] <- StockDetails$Data$age.eff.future[Itype,Ifleet]*EffN.Area 
   }
  
  # Generate the seeds
  set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
  Age.seeds <- array(-1,dim=c(NassArea,3,Nhist+General$Nproj+1,General$Nfleet))
  for (IassArea in 1:NassArea)
   for (Itype in 1:3)
    for (Year in 1:(Nhist+General$Nproj+1))
     for (Ifleet in 1:General$Nfleet)
      Age.seeds[IassArea,Itype,Year,Ifleet] <- floor(runif(1,1,1000000))
  StockDetails$Data$Age.spec <- Age.spec
  StockDetails$Data$Age.seeds <- Age.seeds
  StockDetails$Data$AgeData <- rep(0,10+Nsex*MaxAge)
  StockDetails$Data$NageData <- 0
  if (FullOutput==T) cat("Done: Age readin\n")
  
  # CAA data
  # =========
  Index <- which(AllInput[,1]=="#CAA:Is_CAA"); CheckError(Index,"Is there conditional length-at-age data?")
  StockDetails$Data$Is_CAA <- AllInput[Index+1,1:General$Nfleet]
  Index <- which(AllInput[,1]=="#CAA:Base_EFN_past"); CheckError(Index,"Historical conditional length-at-age EFNs")
  StockDetails$Data$CAA.eff.past <- matrix(0,nrow=3,ncol=General$Nfleet)
  for (Itype in 1:3) StockDetails$Data$CAA.eff.past[Itype,] <- as.numeric(AllInput[Index+Itype,1:General$Nfleet])
  if (Huge.sample.sizes==T) { Index <- which(StockDetails$Data$CAA.eff.past>0); StockDetails$Data$CAA.eff.past[Index] <- 10000 }
  Index <- which(AllInput[,1]=="#CAA:Base_EFN_future"); CheckError(Index,"Future conditional length-at-age EFNs")
  StockDetails$Data$CAA.eff.future <- matrix(0,nrow=3,ncol=General$Nfleet)
  for (Itype in 1:3)StockDetails$Data$CAA.eff.future[Itype,] <- as.numeric(AllInput[Index+Itype,1:General$Nfleet])
  if (Huge.sample.sizes==T) { Index <- which(StockDetails$Data$CAA.eff.future>0); StockDetails$Data$CAA.eff.future[Index] <- 10000 }
  Index <- which(AllInput[,1]=="#CAA:Frequency_future"); CheckError(Index,"Frequency of conditional length-at-age data")
  StockDetails$Data$CAA.freq.future <- matrix(0,nrow=3,ncol=General$Nfleet)
  for (Itype in 1:3) StockDetails$Data$CAA.freq.future[Itype,] <- as.numeric(AllInput[Index+Itype,1:General$Nfleet])
  
  CAA.spec <- array(-1,dim=c(NassArea,3,Nhist+General$Nproj+1,General$Nfleet))
  Index <- which(AllInput[,1]=="#CAA:Frequency_past_retained"); Itype <- 1; CheckError(Index,"Historical retained conditional length-at-age data frequency")
  for (IassArea in 1:NassArea)
   {
    EffN.Area <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:(Nhist+Projected.Data.Spec))
     {
      CAA.spec[IassArea,Itype,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Nfleet+1)])
      for (Ifleet in 1:General$Ncat_fleet) if (Year <= Nhist) if (CatchInp[Year,Ifleet] <= 0) CAA.spec[IassArea,Itype,Year,Ifleet] <- -1
      
      for (Ifleet in 1:General$Nfleet)
       if (CAA.spec[IassArea,Itype,Year,Ifleet] >= 0) CAA.spec[IassArea,Itype,Year,Ifleet] <- CAA.spec[IassArea,Itype,Year,Ifleet]*StockDetails$Data$CAA.eff.past[Itype,Ifleet]*EffN.Area 
     } # Year   
     for (Ifleet in 1:General$Nfleet)
     if ( StockDetails$Data$CAA.freq.future[Itype,Ifleet]>0)
      for (Year in (Projected.Data.Spec+1):(General$Nproj+1))
       if ((Year-1) %% StockDetails$Data$CAA.freq.future[Itype,Ifleet] == 0) CAA.spec[IassArea,Itype,Year+Nhist,Ifleet] <- StockDetails$Data$CAA.eff.future[Itype,Ifleet]*EffN.Area 
    }
  
  Index <- which(AllInput[,1]=="#CAA:Frequency_past_discarded"); Itype <- 2; CheckError(Index,"Historical discard conditional length-at-age data frequency")
  for (IassArea in 1:NassArea)
   {
    EffN.Area <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:(Nhist+Projected.Data.Spec))
     {
      CAA.spec[IassArea,Itype,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Nfleet+1)])
      for (Ifleet in 1:General$Nfleet)
       if (CAA.spec[IassArea,Itype,Year,Ifleet] >= 0) CAA.spec[IassArea,Itype,Year,Ifleet] <- CAA.spec[IassArea,Itype,Year,Ifleet]*StockDetails$Data$CAA.eff.past[Itype,Ifleet]*EffN.Area 
     }
    for (Ifleet in 1:General$Nfleet)
     if ( StockDetails$Data$CAA.freq.future[Itype,Ifleet]>0)
      for (Year in (Projected.Data.Spec+1):(General$Nproj+1))
        if ((Year-1) %% StockDetails$Data$CAA.freq.future[Itype,Ifleet] == 0) CAA.spec[IassArea,Itype,Year+Nhist,Ifleet] <- StockDetails$Data$CAA.eff.future[Itype,Ifleet]*EffN.Area 
   }
  
  Index <- which(AllInput[,1]=="#CAA:Frequency_past_total"); Itype <- 3; CheckError(Index,"Historical total conditional length-at-age data frequency")
  for (IassArea in 1:NassArea)
   {
    EffN.Area <- sqrt(General$Narea/length(which(FleetsToAreas==IassArea)))
    for (Year in 1:(Nhist+Projected.Data.Spec))
     {
      CAA.spec[IassArea,Itype,Year,] <- as.numeric(AllInput[Index+Year,2:(General$Nfleet+1)])
      for (Ifleet in 1:General$Nfleet)
       if (CAA.spec[IassArea,Itype,Year,Ifleet] >= 0) CAA.spec[IassArea,Itype,Year,Ifleet] <- CAA.spec[IassArea,Itype,Year,Ifleet]*StockDetails$Data$CAA.eff.past[Itype,Ifleet]*EffN.Area 
     }
    for (Ifleet in 1:General$Nfleet)
     if ( StockDetails$Data$CAA.freq.future[Itype,Ifleet]>0)
      for (Year in (Projected.Data.Spec+1):(General$Nproj+1))
       if ((Year-1) %% StockDetails$Data$CAA.freq.future[Itype,Ifleet] == 0) CAA.spec[IassArea,Itype,Year+Nhist,Ifleet] <- StockDetails$Data$CAA.eff.future[Itype,Ifleet]*EffN.Area 
   }
  # Generate the seeds
  set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
  CAA.seeds <- array(-1,dim=c(NassArea,3,Nhist+General$Nproj+1,General$Nfleet))
  for (IassArea in 1:NassArea)
   for (Itype in 1:3)
    for (Year in 1:(Nhist+General$Nproj+1))
     for (Ifleet in 1:General$Nfleet)
      CAA.seeds[IassArea,Itype,Year,Ifleet] <- floor(runif(1,1,1000000))
  StockDetails$Data$CAA.spec <- CAA.spec
  StockDetails$Data$CAA.seeds <- CAA.seeds
  StockDetails$Data$CAAData <- rep(0,10+Nsex*MaxAge)
  StockDetails$Data$NCAAData <- 0
  if (FullOutput==T) cat("Done: CAA readin\n")
  
  # Age-reading error
  # =================
  Index <- which(AllInput[,1]=="#age_reading_error"); 
  StockDetails$AgeErrorMatrix <- array(0,dim=c(1,MaxAge,MaxAge));
  Ass$AgeErrorMeans <- seq(from=1,to=MaxAge,by=1) - 0.5
  Ass$AgeErrorSds <- rep(0.0001,MaxAge)
  diag(StockDetails$AgeErrorMatrix[1,,]) <- 1
  if (length(Index)>0)
   if (AllInput[Index,2]=="Yes")
    {
     Index <- which(AllInput[,1]=="#n.age_reading_error"); CheckError(Index,"Number of age reading error matrices")
     N.age.error.mats <- as.numeric(AllInput[Index,2])
     StockDetails$AgeErrorMatrix <- array(0,dim=c(N.age.error.mats,MaxAge,MaxAge));
     AgeErrorMeans <- matrix(0,ncol=N.age.error.mats,nrow=MaxAge)
     Index <- which(AllInput[,1]=="#age_reading_error_means"); CheckError(Index,"Age reading error means")
     for (Iage in 1:MaxAge) AgeErrorMeans[Iage,] <-as.numeric(AllInput[Index+Iage,1+1:N.age.error.mats])
     AgeErrorSds <- matrix(0,ncol=N.age.error.mats,nrow=MaxAge)
     Index <- which(AllInput[,1]=="#age_reading_error_sds"); CheckError(Index,"Age reading error sds")
     for (Iage in 1:MaxAge) AgeErrorSds[Iage,] <-as.numeric(AllInput[Index+Iage,1+1:N.age.error.mats])
     for (Imat in 1:N.age.error.mats)
      {
       for (Iage in 1:MaxAge)
        {
         Age1a <- Iage-0.5
         Cum <- 0
         for (Jage in 1:(MaxAge-1))
          { 
           Age2a <- Jage
           Val <- pnorm(Age2a,AgeErrorMeans[Iage,Imat],AgeErrorSds[Iage,Imat])
           StockDetails$AgeErrorMatrix[Imat,Jage,Iage] <- Val-Cum
           Cum <- Val
                        
         }
         StockDetails$AgeErrorMatrix[Imat,MaxAge,Iage] <- 1- Cum
        }
      }
     Ass$AgeErrorSds <- AgeErrorSds[,1]
     Ass$AgeErrorMeans <- AgeErrorMeans[,1]
    }
  if (FullOutput==T) cat("Done: Aging error matrices readin\n")

  # ----------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  if (Num.Pred > 0)
   {
    # Predator number data
    # ====================
    Index <- which(AllInput[,1]=="#PredNo:Is_PredNo"); CheckError(Index,"Is there predator number data")
    StockDetails$Data$Is_pred.no <- as.logical(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredNo::Base_CVs_past"); CheckError(Index,"Predator number past CVs")
    StockDetails$Data$Pred.no.cv.past <- as.numeric(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredNo::Base_CVs_future"); CheckError(Index,"Predator number future CVs")
    StockDetails$Data$Pred.no.cv.future <- as.numeric(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredNo:Frequency_future"); CheckError(Index,"Predator number future frequency")
    StockDetails$Data$Pred.no.freq.future <- as.numeric(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredNo:Frequency_past"); CheckError(Index,"Predator number past frequency")
    Pred.no.spec <- matrix(-1,nrow=Nhist+General$Nproj+1,ncol=Num.Pred)

    # Fill in the predator number matrix
    for (Year in 1:(Nhist))
     {
      Pred.no.spec[Year,] <- as.numeric(AllInput[Index+Year,2:(Num.Pred+1)])
      for (Ipred in 1:Num.Pred)
       if ( Pred.no.spec[Year,Ipred] >= 0)  Pred.no.spec[Year,Ipred] <- Pred.no.spec[Year,Ipred]*StockDetails$Data$Pred.no.cv.past[Ipred] 
     } # Year
    for (Ipred in 1:Num.Pred)
     if ( StockDetails$Data$Pred.no.freq.future[Ipred]>0)
      for (Year in 1:(General$Nproj+1))
       if ((Year-1) %% StockDetails$Data$Pred.no.freq.future[Ipred] == 0) Pred.no.spec[Year+Nhist,Ipred] <- StockDetails$Data$Pred.no.cv.future[Ipred] 
    # Generate the devs
    set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
    Pred.no.devs <- matrix(-1,nrow=Nhist+General$Nproj+1,ncol=Num.Pred)
    for (Year in 1:(Nhist+General$Nproj+1))
      for (Ipred in 1:Num.Pred)
        Pred.no.devs[Year,Ipred] <- rnorm(1,0,1)* Pred.no.spec[Year,Ipred]-  Pred.no.spec[Year,Ipred]^2/2.0
    StockDetails$Data$Pred.no.spec <- Pred.no.spec
    StockDetails$Data$Pred.no.devs <- Pred.no.devs
    StockDetails$Data$Pred.no.Data <- rep(0,5)
    if (FullOutput==T) cat("Done: Predator number data\n")

    # Predator consumption data
    # =========================
    Index <- which(AllInput[,1]=="#PredConsump:Is_PredConsump"); CheckError(Index,"Is there predator consumption data")
    StockDetails$Data$Is_Consump <- as.logical(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredConsump::Base_CVs_past"); CheckError(Index,"Predator number past CVs")
    StockDetails$Data$Consump.cv.past <- as.numeric(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredConsump::Base_CVs_future"); CheckError(Index,"Predator consumption future CVs")
    StockDetails$Data$Consump.cv.future <- as.numeric(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredConsump:Frequency_future"); CheckError(Index,"Predator consumption future frequency")
    StockDetails$Data$Consump.freq.future <- as.numeric(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredConsump:Frequency_past"); CheckError(Index,"Predator consumption past frequency")
    Consump.spec <- matrix(-1,nrow=Nhist+General$Nproj+1,ncol=Num.Pred)
     
    # Fill in the index matrix
    for (Year in 1:(Nhist))
     {
      Consump.spec[Year,] <- as.numeric(AllInput[Index+Year,2:(Num.Pred+1)])
      for (Ipred in 1:Num.Pred)
        if ( Consump.spec[Year,Ipred] >= 0)  Consump.spec[Year,Ipred] <- Consump.spec[Year,Ipred]*StockDetails$Data$Consump.cv.past[Ipred] 
     } # Year
    for (Ipred in 1:Num.Pred)
     if ( StockDetails$Data$Consump.freq.future[Ipred]>0)
      for (Year in 1:(General$Nproj+1))
       if ((Year-1) %% StockDetails$Data$Consump.freq.future[Ipred] == 0) Consump.spec[Year+Nhist,Ipred] <- StockDetails$Data$Consump.cv.future[Ipred] 
    # Generate the devs
    set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
    Consump.devs <- matrix(-1,nrow=Nhist+General$Nproj+1,ncol=Num.Pred)
    for (Year in 1:(Nhist+General$Nproj+1))
     for (Ipred in 1:Num.Pred)
      Consump.devs[Year,Ipred] <- rnorm(1,0,1)* Consump.spec[Year,Ipred]-  Consump.spec[Year,Ipred]^2/2.0
    StockDetails$Data$Consump.spec <- Consump.spec
    StockDetails$Data$Consump.devs <- Consump.devs
    StockDetails$Data$Consump.Data <- rep(0,5)
    if (FullOutput==T) cat("Done: Predator consumption data\n")
    
    # Predator age data
    # =================
    Index <- which(AllInput[,1]=="#PredAge:Is_PredAge"); CheckError(Index,"Is there predator age data")
    StockDetails$Data$Is_comsump.age <- as.logical(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredAge:Base_EFN_past"); CheckError(Index,"Historical predator age EFNs")
    StockDetails$Data$Pred.age.eff.past <- as.numeric(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredAge:Base_EFN_future"); CheckError(Index,"Future predator age EFNs")
    StockDetails$Data$Pred.age.eff.future <- as.numeric(AllInput[Index+1,1:Num.Pred])
    Index <- which(AllInput[,1]=="#PredAge:Frequency_future"); CheckError(Index,"Predator age data future frequency")
    StockDetails$Data$Pred.age.freq.future <- as.numeric(AllInput[Index+1,1:Num.Pred])
    
    Pred.age.spec <- matrix(-1,nrow=Nhist+General$Nproj+1,ncol=Num.Pred)
    Index <- which(AllInput[,1]=="#PredAge:Frequency_past"); CheckError(Index,"Predator age data past frequency")
     for (Year in 1:Nhist)
     {
      Pred.age.spec[Year,] <- as.numeric(AllInput[Index+Year,2:(Num.Pred+1)])
      for (Ipred in 1:Num.Pred)
        if (Pred.age.spec[Year,Ipred] >= 0) Pred.age.spec[Year,Ipred] <- Pred.age.spec[Year,Ipred]*StockDetails$Data$Pred.age.eff.past[Ipred]; 
     }
    for (Ipred in 1:Num.Pred)
     if ( StockDetails$Data$Pred.age.freq.future[Ipred]>0)
      for (Year in 1:(General$Nproj+1))
       if ((Year-1) %% StockDetails$Data$Pred.age.freq.future[Ipred] == 0) Pred.age.spec[Year+Nhist,Ipred] <- StockDetails$Data$Pred.age.eff.future[Ipred] 
    
    # Generate the seeds
    set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
    Pred.age.seeds <-matrix(-1,nrow=Nhist+General$Nproj+1,Num.Pred)
    for (Year in 1:(Nhist+General$Nproj+1))
     for (Ipred in 1:Num.Pred)
      Pred.age.seeds[Year,Ipred] <- floor(runif(1,1,1000000))
    StockDetails$Data$Pred.age.spec <- Pred.age.spec
    StockDetails$Data$Pred.age.seeds <- Pred.age.seeds
    StockDetails$Data$PredAgeData <- rep(0,9+Nsex*MaxAge)
    if (FullOutput==T) cat("Done: Predator age data\n")
   }

  # Bias in values for catch
  Catch.Bias <- array(1,dim=c(NassArea,Nhist+General$Nproj+1,General$Nfleet))
  Index <- which(AllInput[,1]=="#Catch:Base_Bias")
  if (length(Index) > 0)
   for (Ifleet in 1:General$Nfleet)
    {
     Index2 <- Index+Ifleet
     Nbreak <- as.numeric(AllInput[Index+1,1])
     Yr1 <- as.numeric(AllInput[Index+1,2])-StockDetails$YrOffset
     YrN <- as.numeric(AllInput[Index+1,1+(Nbreak-1)*2+1])-StockDetails$YrOffset
     if (YrN>General$Nproj+1) YrN <- Nhist+General$Nproj+1
     for (Iyr in 1:Yr1)  Catch.Bias[,Iyr,Ifleet] <- as.numeric(AllInput[Index+1,3])
     for (Iyr in YrN:(Nhist+General$Nproj+1))  Catch.Bias[,Iyr,Ifleet] <- as.numeric(AllInput[Index+1,1+Nbreak*2])
     if (Nbreak >= 2)
      for (Ibreak in 1:(Nbreak-1))
       {
        Yr1 <- as.numeric(AllInput[Index+1,1+(Ibreak-1)*2+1])-StockDetails$YrOffset
        if (Yr1 > Nhist+General$Nproj+1) Yr1 <- Nhist+General$Nproj+1
        V1 <- as.numeric(AllInput[Index+1,1+(Ibreak-1)*2+2])
        Yr2 <- as.numeric(AllInput[Index+1,1+Ibreak*2+1])-StockDetails$YrOffset
        if (Yr1==Yr2) { print("Error in catch bias"); stop() }
        if (Yr2 > Nhist+General$Nproj+1) Yr2 <- Nhist+General$Nproj+1
        V2 <- as.numeric(AllInput[Index+1,1+Ibreak*2+2])
        for (Iyr in Yr1:Yr2) Catch.Bias[,Iyr,Ifleet] <- V1 +(V2-V1)*(Iyr-Yr1)/(Yr2-Yr1)  
       }  
    }

  # Random devs on catch
  Catch.CV <- rep(0,General$Nfleet)
  Index <- which(AllInput[,1]=="#Catch:Base_CV");
  if(length(Index)>0) Catch.CV <- as.numeric(AllInput[Index+1,1:General$Nfleet])
  set.seed(NextSeed); NextSeed <- floor(runif(1,1,1000000))
  Catch.devs <- array(0,dim=c(NassArea,Nhist+General$Nproj+1,General$Nfleet))
  for (IassArea in 1:NassArea)
    for (Year in 1:(Nhist+General$Nproj+1))
      for (Ifleet in 1:General$Nfleet)
        Catch.devs[IassArea,Year,Ifleet] <- rnorm(1,0,1)*Catch.CV[Ifleet]- Catch.CV[Ifleet]^2/2.0
  StockDetails$Data$Catch.devs <- Catch.devs
  StockDetails$Data$Catch.Bias <- Catch.Bias
  if (FullOutput==T) cat("Done: Catch error readin\n")

  # ==========================================================================================================================================
  # ==========================================================================================================================================
  # Implementation error specifications
  StockDetails$Use.Implementation.Error <- 0
  Index <- which(AllInput[,1]=="#Use.Implementation.Err"); CheckError(Index,"Is there implementation error")
  
  StockDetails$Use.Implementation.Error <- as.numeric(AllInput[Index,2]) 
  if (StockDetails$Use.Implementation.Error!=0)
   {
    Index <- which(AllInput[,1]=="#Implementation.Err.Spec");CheckError(Index,"Implementation error specifications")
    StockDetails$Implementation.Error.Specs <- as.numeric(AllInput[Index,2:4])  
   }
  if (FullOutput==T) cat("Done: Implementation error\n")
  
  # ----------------------------------------------------------------------------------------------------------------------
  # Economics inputs
  StockDetails$Price <- rep(0,General$Ncat_fleet)
  StockDetails$CostPerDay <- rep(0,General$Ncat_fleet)
  StockDetails$DaysPerF1 <- rep(0,General$Ncat_fleet)
  StockDetails$DaysPerF2 <- rep(0,General$Ncat_fleet)
  StockDetails$FixedCosts <- rep(0,General$Ncat_fleet)
  StockDetails$Prob.Collapse.Ins <- rep(0,Nhist+General$Nproj+1)
  StockDetails$ExpectProfit.Fish <- array(0,dim=c(3,General$Ncat_fleet,Nhist+General$Nproj+1))
  StockDetails$Insurance.Bought.Fish <-array(0,dim=c(20,General$Ncat_fleet,Nhist+General$Nproj+1))
  StockDetails$Cost.Insurance.Bought.Fish <- array(0,dim=c(20,General$Ncat_fleet,ncol=Nhist+General$Nproj+1))
  StockDetails$PayOut <- array(0,dim=c(20,General$Ncat_fleet,ncol=Nhist+General$Nproj+1))
  Index <- which(AllInput[,1]=="#Econ_parameters");CheckError(Index,"Economics parameters")
  if (length(Index)>0)
   {
    for (Ifleet in 1:General$Ncat_fleet)  
     {
      StockDetails$Price[Ifleet] <- as.numeric(AllInput[Index+Ifleet,1])
      StockDetails$CostPerDay[Ifleet] <- as.numeric(AllInput[Index+Ifleet,2])
      StockDetails$DaysPerF1[Ifleet] <- as.numeric(AllInput[Index+Ifleet,3])
      StockDetails$DaysPerF2[Ifleet] <- as.numeric(AllInput[Index+Ifleet,4])
      StockDetails$FixedCosts[Ifleet] <- as.numeric(AllInput[Index+Ifleet,5])
    }
  }
  StockDetails$Revenue <- array(0,dim=c(Narea,General$Ncat_fleet,Nhist+General$Nproj+1))
  StockDetails$Cost <- array(0,dim=c(Narea,General$Ncat_fleet,Nhist+General$Nproj+1))
  StockDetails$Days <- array(0,dim=c(Narea,General$Ncat_fleet,Nhist+General$Nproj+1))
  if (FullOutput==T) cat("Done: Economics\n")
  
  # ----------------------------------------------------------------------------------------------------------------------
  # True control rule
  # Tier 1 specifications
  Index <- which(AllInput[,1]=="#MSY");                                     StockDetails$MSY <- as.numeric(AllInput[Index,2])
  Index <- which(AllInput[,1]=="#SPR_target");                              StockDetails$SPR_target <- as.numeric(AllInput[Index,2])
  Index <- which(AllInput[,1]=="#Biomass_target");                          StockDetails$Biomass_target <- as.numeric(AllInput[Index,2])
  Index <- which(AllInput[,1]=="#Forecast_type");                           StockDetails$Forecast_type <- as.numeric(AllInput[Index,2])
  Index <- which(AllInput[,1]=="#Control_rule_inflection");                 StockDetails$C_inflect <- as.numeric(AllInput[Index,2])
  Index <- which(AllInput[,1]=="#Control_rule_Biomass_level_for_no_F");     StockDetails$C_NoF <- as.numeric(AllInput[Index,2])
  Index <- which(AllInput[,1]=="#Control_rule_Protection_level_for_no_F");  StockDetails$C_Prot <- as.numeric(AllInput[Index,2]);
  if (FullOutput==T) cat("Done: True Control Rule\n")

  # ==========================================================================================================================================
  # ==========================================================================================================================================
  # Assessment method bounds
  Index <- which(AllInput[,1]=="#Assessment:Bound_mult");CheckError(Index,"Parameters bound multipliers") 
  StockDetails$Ass_Gen$Bound_Mult <- as.numeric(AllInput[Index,2])
  Index <- which(AllInput[,1]=="#Assessment:Jitter_SD");CheckError(Index,"Jitter value")  
  StockDetails$Ass_Gen$Parameter_jitter_SD <- as.numeric(AllInput[Index,2])

  # ----------------------------------------------------------------------------------------------------------------------------------------------------------  
  # Key Stock-specific stuff
  StockDetails$N <- array(NA,dim=c(General$Narea,Nsex,MaxAge,Nhist+General$Nproj+1))
  StockDetails$SSB <- matrix(NA,nrow=General$Narea,ncol=Nhist+General$Nproj+1)
  StockDetails$SSBSex <- array(NA,dim=c(Nsex,Narea,Nhist+General$Nproj+1))
  StockDetails$BREF <- matrix(NA,nrow=General$Narea,ncol=Nhist+General$Nproj+1)
  StockDetails$Depletion <- matrix(NA,nrow=General$Narea,ncol=Nhist+General$Nproj+1)
  StockDetails$Exp.Recr <- matrix(NA,nrow=General$Narea,ncol=Nhist+General$Nproj+1)
  StockDetails$Recr <- matrix(NA,nrow=General$Narea,ncol=Nhist+General$Nproj+1)
  StockDetails$TrueRBC <- rep(NA,length=Nhist+General$Nproj+1)
  StockDetails$RBC <- rep(NA,length=Nhist+General$Nproj+1)
  StockDetails$TACsStep <- matrix(NA,nrow=6,ncol=Nhist+General$Nproj+1)
  StockDetails$Final.TAC <- rep(NA,length=Nhist+General$Nproj+1)
  StockDetails$TACsAdjust <- rep(NA,length=Nhist+General$Nproj+1)
  StockDetails$TACs <- rep(NA,length=Nhist+General$Nproj+1)
  StockDetails$FullF <- array(NA,dim=c(General$Narea,General$Ncat_fleet,Nhist+General$Nproj+1))
  StockDetails$MatF <- rep(NA,Nhist+General$Nproj+1)
  StockDetails$CatchRetained <- array(NA,dim=c(General$Narea,General$Ncat_fleet,Nhist+General$Nproj+1))
  StockDetails$CatchTotal <- array(NA,dim=c(General$Narea,General$Ncat_fleet,Nhist+General$Nproj+1))
  StockDetails$CatchDiscard <- array(NA,dim=c(General$Narea,General$Ncat_fleet,Nhist+General$Nproj+1))
  StockDetails$CatchAtLenRet <- array(NA,dim=c(General$Narea,General$Nfleet,Nsex,Nlen,Nhist+General$Nproj+1))
  StockDetails$CatchAtLenTot <- array(NA,dim=c(General$Narea,General$Nfleet,Nsex,Nlen,Nhist+General$Nproj+1))
  StockDetails$CatchAtAgeRet <- array(NA,dim=c(General$Narea,General$Nfleet,Nsex,MaxAge,Nhist+General$Nproj+1))
  StockDetails$CatchAtAgeTot <- array(NA,dim=c(General$Narea,General$Nfleet,Nsex,MaxAge,Nhist+General$Nproj+1))
  StockDetails$CatchAtCAARet <- array(NA,dim=c(General$Narea,General$Nfleet,Nsex,MaxAge,Nlen,Nhist+General$Nproj+1))
  StockDetails$CatchAtCAATot <- array(NA,dim=c(General$Narea,General$Nfleet,Nsex,MaxAge,Nlen,Nhist+General$Nproj+1))
 
  # ==========================================================================================================================================
  # ==========================================================================================================================================
  # Now read in the assessment file
  
  Index <- which(AssInput[,1]=="#Ass_Basic:Type"); Ass$Type <- AssInput[Index,2]; CheckError(Index,"Assessment type") 
  Ass$Clean.up <- "Default"
  Index <- which(AssInput[,1]=="#Ass_Basic:Clean.up"); if (length(Index)!=0)  Ass$Clean.up <- AssInput[Index,2]
  Ass$Estimate <- "Yes"
  Index <- which(AssInput[,1]=="#Ass_Basic.Estimate"); if (length(Index)!=0)  Ass$Estimate <- AssInput[Index,2]
  Ass$Use.par <- "No"
  Index <- which(AssInput[,1]=="#Ass_Basic.Use.SS3.par"); if (length(Index)!=0) Ass$Use.par <- AssInput[Index,2]
  if (General$TestCase=="Yes") { Ass$Estimate <- "No"; Ass$Use.par <- "Yes"; }
  # Override estimation with and environmental trigger is set 
  Ass$Use_Ass_Env <- "No"
  Ass$Use_Ass_Env_Specs <- NULL
  Index <- which(AssInput[,1]=="#Ass_Basic.Env_assess")
  if (length(Index)!=0) 
   {
    Ass$Use_Ass_Env <- AssInput[Index,2]
    if (Ass$Use_Ass_Env=="Yes") print("RUN_NOTE: DO ASSESSMENT WITH A HEATWAVE")
    Ass$Use_Ass_Env_Specs <- as.numeric(AssInput[Index,3:4])
   }
  if (FullOutput==T) cat("Done: Assessment Basics\n")

  # Tier 1 specifications
  
  # Read in specifications
  Ass$SS_parameter_offset_approach <- 1
  Index <- which(AssInput[,1]=="#SS:parameter_offset_approach");  if (length(Index)!=0) Ass$SS_parameter_offset_approach <- as.numeric(AssInput[Index,2])
  if (Ass$SS_parameter_offset_approach != 1 & Ass$SS_parameter_offset_approach != 2& Ass$SS_parameter_offset_approach != 3) { print("Ass$SS_parameter_offset_approach out of bounds"); AAA }
  Ass$SS_est_opt <- "EstOnly"
  Index <- which(AssInput[,1]=="#SS_est_opt");  if (length(Index)!=0) Ass$SS_est_opt <- AssInput[Index,2]
  
  Index <- which(AssInput[,1]=="#SS:original.last.year"); CheckError(Index,"Assessment - last year"); Ass$SS_Original_yr1 <- as.numeric(AssInput[Index,2]); 
  
  
  Index <- which(AssInput[,1]=="#SS:Nblock_Patterns"); CheckError(Index,"Assessment - block patterns"); Ass$SS_Nblock.patterns <- as.numeric(AssInput[Index,2]); 
  Index <- which(AssInput[,1]=="#SS:blocks_per_pattern"); CheckError(Index,"Assessment - blocks per pattern"); Ass$SS_blocks_per_pattern <- as.numeric(AssInput[Index,2:(1+ Ass$SS_Nblock.patterns)]);
  Ass$block.years <- matrix(0,nrow=Ass$SS_Nblock.patterns,ncol=50)
  for (Iblock.year in 1:Ass$SS_Nblock.patterns)
    Ass$block.years[Iblock.year,1:50] <-  as.numeric(AssInput[Index+Iblock.year,1:50]); 
  
  # Expected values for M growth parameters
  Ass$SS_Mbase <- Mbase
  Ass$M.type <- M.type
  
  Ass$SS_M_Phase <- rep(-1,Nsex); Index <- which(AssInput[,1]=="#SS:parameter_M_phase");if (length(Index)!=0) Ass$SS_M_Phase <- as.numeric(AssInput[Index,1+1:Nsex])
  Ass$SS_M_Bias <- 1.0; Index <- which(AssInput[,1]=="#SS:M.est.Bias");                 if (length(Index)!=0) Ass$SS_M_Bias <- as.numeric(AssInput[Index,2])
  Ass$SS_Age1 <- Age1
  Ass$SS_Age2 <- Age2
  Ass$SS_LenA1 <- LenA1
  Ass$SS_LenA1_Phase <- rep(-1,Nsex); Index <- which(AssInput[,1]=="#SS:parameter_Len1_phase");if (length(Index)!=0) Ass$SS_LenA1_Phase <- as.numeric(AssInput[Index,1+1:Nsex])
  Ass$SS_LenA1_Bias <- 1.0; Index <- which(AssInput[,1]=="#SS:LenA1.est.Bias");               if (length(Index)!=0) Ass$SS_LenA1_Bias <- as.numeric(AssInput[Index,2])
  Ass$SS_LenA2 <- LenA2
  Ass$SS_LenA2_Phase <- rep(-1,Nsex); Index <- which(AssInput[,1]=="#SS:parameter_Len2_phase");if (length(Index)!=0) Ass$SS_LenA2_Phase <- as.numeric(AssInput[Index,1+1:Nsex])
  Ass$SS_LenA2_Bias <- 1.0; Index <- which(AssInput[,1]=="#SS:LenA2.est.Bias");                if (length(Index)!=0) Ass$SS_LenA2_Bias <- as.numeric(AssInput[Index,2])
  Ass$SS_Kappa <- Kappa
  Ass$SS_Kappa_Phase <- rep(-1,Nsex); Index <- which(AssInput[,1]=="#SS:parameter_Kappa_phase");if (length(Index)!=0) Ass$SS_Kappa_Phase <- as.numeric(AssInput[Index,1+1:Nsex])
  Ass$SS_Kappa_Bias <- 1.0; Index <- which(AssInput[,1]=="#SS:Kappa.est.Bias");                 if (length(Index)!=0) Ass$SS_Kappa_Bias <- as.numeric(AssInput[Index,2])
  if (Growth.Model==2)
   {
    Ass$SS_Richards <- Richards
    Ass$SS_Richards_Phase <- rep(-1,Nsex); Index <- which(AssInput[,1]=="#SS:parameter_Richards_phase");if (length(Index)!=0) Ass$SS_Kappa_Phase <- as.numeric(AssInput[Index,1+1:Nsex])
    Ass$SS_Richards_Bias <- 1.0; Index <- which(AssInput[,1]=="#SS:Richards.est.Bias");                 if (length(Index)!=0) Ass$SS_Kappa_Bias <- as.numeric(AssInput[Index,2])
   }
  Ass$SS_CV1 <- CV1
  Ass$SS_CV1_Phase <- rep(-1,Nsex); Index <- which(AssInput[,1]=="#SS:parameter_CV1_phase");if (length(Index)!=0) Ass$SS_CV1_Phase <- as.numeric(AssInput[Index,1+1:Nsex])
  Ass$SS_CV1_Bias <- 1.0; Index <- which(AssInput[,1]=="#SS:CV1.est.Bias");                 if (length(Index)!=0) Ass$SS_CV1_Bias <- as.numeric(AssInput[Index,2])
  Ass$SS_CV2 <- CV2
  Ass$SS_CV2_Phase <- rep(-1,Nsex); Index <- which(AssInput[,1]=="#SS:parameter_CV2_phase");if (length(Index)!=0) Ass$SS_CV2_Phase <- as.numeric(AssInput[Index,1+1:Nsex])
  Ass$SS_CV2_Bias <- 1.0; Index <- which(AssInput[,1]=="#SS:CV2.est.Bias");                 if (length(Index)!=0) Ass$SS_CV2_Bias <- as.numeric(AssInput[Index,2])
  Ass$SS_WtLen_a <- WtLen_a
  Ass$SS_WtLen_b <- WtLen_b
  Ass$SS_First_mat_age <- 1; Index <- which(AssInput[,1]=="#SS:parameter_first_mat_age");if (length(Index)!=0) Ass$SS_First_mat_age <- as.numeric(AssInput[Index,2])
  Ass$SS_Mat50 <- Mat50
  Ass$SS_SlopeMat <- SlopeMat
  Ass$SS_fec_option <-  Fec.opt
  Ass$SS_Egg_1 <- Eggs.1
  Ass$SS_Egg_2 <- Eggs.2
  if (General$Uncertain.Level==1) 
   {
    Ass$SS_LenA1_Phase <- rep(-2,Nsex); Ass$SS_LenA2_Phase <- rep(-2,Nsex); Ass$SS_Kappa_Phase <- rep(-2,Nsex); 
    Ass$SS_CV1_Phase <- rep(-2,Nsex); Ass$SS_CV2_Phase <- rep(-2,Nsex)
   }
  
  Index <- which(AssInput[,1]=="#SS:MG_env.dev");   CheckError(Index,"Assessment - MG env_devs");    Ass$MG_env_devs <- as.numeric(AssInput[Index+1,1:(6+8*Nsex)])
  Index <- which(AssInput[,1]=="#SS:MG_blocks");    CheckError(Index,"Assessment - MG block specs"); Ass$MG_blocks <- as.numeric(AssInput[Index+1,1:(6+8*Nsex)])
  Index <- which(AssInput[,1]=="#SS:MG_block_fns"); CheckError(Index,"Assessment - MG block fn");    Ass$MG_block_fns <- as.numeric(AssInput[Index+1,1:(6+8*Nsex)])
  Index <- which(AssInput[,1]=="#SS:MG_N_dev_vals"); CheckError(Index,"Assessment - MG n devs");     Ass$N_dev_MG <- as.numeric(AssInput[Index+1,1])
  Ass$MG_devs <- NULL; Ass$MG_devs_phs <- NULL
  if (Ass$N_dev_MG > 0)
   {  
    Index <- which(AssInput[,1]=="#SS:MG_dev_vals"); CheckError(Index,"Assessment - MG dev vals");
    for (Ipar in 1:Ass$N_dev_MG) Ass$MG_devs <- c(Ass$MG_devs,as.numeric(AssInput[Index+1,Ipar]))
    Index <- which(AssInput[,1]=="#SS:MG_dev_phs"); CheckError(Index,"Assessment - MG dev phs");
    for (Ipar in 1:Ass$N_dev_MG) Ass$MG_devs_phs <- c(Ass$MG_devs_phs,as.numeric(AssInput[Index+1,Ipar]))
   }

  # Rcruitment stuff
  Ass$SR_R0 <- StockDetails$R00
  Ass$SR_R0_mult <- 1;       Index <- which(AssInput[,1]=="#SS:parameter_R0_mult");         if (length(Index)!=0) Ass$SR_R0_mult <- as.numeric(AssInput[Index,2])
  Ass$SR_R0_Phase <- -1;     Index <- which(AssInput[,1]=="#SS:parameter_logR0_phase");     if (length(Index)!=0) Ass$SR_R0_Phase <- as.numeric(AssInput[Index,2])
  Ass$SR_Steep <- StockDetails$Steep
  Ass$SR_Steep_Phase <- -1;  Index <- which(AssInput[,1]=="#SS:parameter_Steep_phase");     if (length(Index)!=0) Ass$SR_Steep_Phase <- as.numeric(AssInput[Index,2])
  Ass$SS_Steep_Bias <- 1.0; Index <- which(AssInput[,1]=="#SS:parameter_Steep_bias");       if (length(Index)!=0) Ass$SS_Steep_Bias <- as.numeric(AssInput[Index,2])
  Ass$SR_SigmaR <- StockDetails$SigmaR
  Ass$SR_SigmaR_Phase <- -1; Index <- which(AssInput[,1]=="#SS:parameter_SigmaR_phase");    if (length(Index)!=0) Ass$SR_SigmaR_Phase <- as.numeric(AssInput[Index,2])
  StockDetails$regime <- 0
  Ass$SR_regime_Phase <- -1; Index <- which(AssInput[,1]=="#SS:parameter_regime_phase");    if (length(Index)!=0) Ass$SR_regime_Phase <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#SS:SR_env.dev");   CheckError(Index,"Assessment - SR env_devs");    Ass$SR_env_devs <- as.numeric(AssInput[Index+1,1:5])
  Index <- which(AssInput[,1]=="#SS:SR_blocks");    CheckError(Index,"Assessment - SR block specs"); Ass$SR_blocks <- as.numeric(AssInput[Index+1,1:5])
  Index <- which(AssInput[,1]=="#SS:SR_block_fns"); CheckError(Index,"Assessment - SR block fn");    Ass$SR_block_fns <- as.numeric(AssInput[Index+1,1:5])
  N_dev_SR <- length(which(Ass$SR_blocks!=0))+length(which(Ass$SR_env_devs!=0))
  Ass$SR_devs <- NULL
  Ass$SR_devs_phs <- NULL
  if (N_dev_SR > 0)
   {  
    Index <- which(AssInput[,1]=="#SS:SR_dev_vals"); CheckError(Index,"Assessment - SR dev vals");
    for (Ipar in 1:N_dev_SR) Ass$SR_devs <- c(Ass$SR_devs,as.numeric(AssInput[Index+1,Ipar]))
    Index <- which(AssInput[,1]=="#SS:SR_dev_phs"); CheckError(Index,"Assessment - SR dev phs");
    for (Ipar in 1:N_dev_SR) Ass$SR_devs_phs <- c(Ass$SR_devs_phs,as.numeric(AssInput[Index+1,Ipar]))
  }
  Ass$N_dev_SR <- N_dev_SR
 
  # Recruitment estim$tion
  Ass$SS_rec_dev_start <- 1;         Index <- which(AssInput[,1]=="#SS:Rec_dev_main_start");   if (length(Index)!=0) Ass$SS_rec_dev_start <- as.numeric(AssInput[Index,2])
  Ass$SS_rec_dev_end <- 1;           Index <- which(AssInput[,1]=="#SS:Rec_dev_end");          if (length(Index)!=0) Ass$SS_rec_dev_end <- as.numeric(AssInput[Index,2])
  Ass$SS_last_yr_nobias_adj <- 1;    Index <- which(AssInput[,1]=="#SS_last_yr_nobias_adj");   if (length(Index)!=0) Ass$SS_last_yr_nobias_adj <- as.numeric(AssInput[Index,2])
  Ass$SS_first_yr_fullbias_adj <- 1; Index <- which(AssInput[,1]=="#SS_first_yr_fullbias_adj");if (length(Index)!=0) Ass$SS_first_yr_fullbias_adj <- as.numeric(AssInput[Index,2])
  Ass$SS_last_yr_fullbias_adj <- 1;  Index <- which(AssInput[,1]=="#SS_last_yr_fullbias_adj"); if (length(Index)!=0) Ass$SS_last_yr_fullbias_adj <- as.numeric(AssInput[Index,2])
  Ass$SS_end_yr_for_ramp <- 1;       Index <- which(AssInput[,1]=="#SS_end_yr_for_ramp");      if (length(Index)!=0) Ass$SS_end_yr_for_ramp <- as.numeric(AssInput[Index,2])
  Ass$SS_early_dev_phase <- -1;      Index <- which(AssInput[,1]=="#SS:Early_dev_phase");      if (length(Index)!=0) Ass$SS_early_dev_phase <- as.numeric(AssInput[Index,2])
  Ass$SS_early_dev_yr1 <- 0;         Index <- which(AssInput[,1]=="#SS:Rec_dev_early_start");  if (length(Index)!=0) Ass$SS_early_dev_yr1 <- as.numeric(AssInput[Index,2])
  Ass$SS_max_bias_adjust <- 1;       Index <- which(AssInput[,1]=="#SS:Max_bias_adjust");      if (length(Index)!=0) Ass$SS_max_bias_adjust <- as.numeric(AssInput[Index,2])

  Ass$EarlyDevsEst <- rep(0,MaxAge); Use.Early.DevsEst <- 0
  Index <- which(AssInput[,1]=="#Early_devs_est")
  if (length(Index) > 0) Ass$n.earlyDevsEst<- as.numeric(AssInput[Index,2]);
  if (length(Index) > 0 & Ass$n.earlyDevsEst > 0) Ass$EarlyDevsEst <- as.numeric(AssInput[Index+1,1:Ass$n.earlyDevsEst]);
  if (Ass$n.earlyDevsEst ==0) Ass$EarlyDevsEst <- NULL
  Index <- which(AssInput[,1]=="#Late_Devs_est")
  Ass$Late.range.est <- c(-1,-1)
  if (length(Index)>0) Ass$Late.range.est <- as.numeric(AssInput[Index,2:3]);
  if (Ass$Late.range.est[2]-Ass$Late.range.est[1]+1 >0)
    Ass$LateDevsEst[1:(Ass$Late.range.est[2]-Ass$Late.range.est[1]+1)] <- as.numeric(AssInput[Index+1,1:(Ass$Late.range.est[2]-Ass$Late.range.est[1]+1)]);  
  
  
  if (General$Uncertain.Level==1) 
   {
    for (Ipar in 1:N_dev_SR) Ass$SR_devs_phs[Ipar] <- -2
   }
  
  Index <- which(AssInput[,1]=="#SS:Use_Steepness_Equi"); CheckError(Index,"EM:Steepness in unfished")
  Ass$Use_Steep_in_equ <- as.numeric(AssInput[Index+1,1]);
  if (FullOutput==T) cat("Done: Assessment Biology and recruitment\n")
  
  # ----------------------------------------------------------------------------------------------------------------------------
  
  # Length selectivity and retention
  Index <- which(AssInput[,1]=="#SS:Length_selex_pattern"); CheckError(Index,"Assessment - length selectivity patterns") 
  Ass$Selex_size_pattern <- as.numeric(AssInput[Index+1,1:General$Nfleet]) 
  Index <- which(AssInput[,1]=="#SS:Length_selex_pattern_male"); CheckError(Index,"Assessment - length selectivity patterns (male)") 
  Ass$Selex_size_pattern_male <- as.numeric(AssInput[Index+1,1:General$Nfleet]) 
  Index <- which(AssInput[,1]=="#SS:Mirrored_size_selex");CheckError(Index,"Assessment - length selectivity pattern mirrors")        
  Ass$Selex_size_mirror_fleet  <- as.numeric(AssInput[Index+1,1:General$Nfleet]) 
  Index <- which(AssInput[,1]=="#SS:N.estimated.selex.size.pars");CheckError(Index,"Assessment - number of selectivity parameters per fleet")        
  Ass$Selex_size_nsel.pars <- as.numeric(AssInput[Index+1,1:General$Nfleet]) 
  Ass$Selex_size_par_init <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_size_parameters");CheckError(Index,"Assessment - length selectivity parameters")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_size_par_init[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_size_par_phase <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_size_par_phase");CheckError(Index,"Assessment - length selectivity parameter phases")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_size_par_phase[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_size_par_blks <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_size_par_blocks");CheckError(Index,"Assessment - length selectivity blocks")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_size_par_blks[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_size_par_fns <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_size_par_fns");CheckError(Index,"Assessment - length selectivity block_phases")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_size_par_fns[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_size_ann_devs_use <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_size_ann_devs_use");CheckError(Index,"Assessment - length selectivity use_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_size_ann_devs_use[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_size_ann_devs_minys <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_size_ann_devs_miny");CheckError(Index,"Assessment - length selectivity miny_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_size_ann_devs_miny[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_size_ann_devs_maxy <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_size_ann_devs_maxy");CheckError(Index,"Assessment - length selectivity maxy_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_size_ann_devs_maxy[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_size_ann_devs_phs <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_size_ann_devs_phs");CheckError(Index,"Assessment - length selectivity phs_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_size_ann_devs_phs[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  
  Index <- which(AssInput[,1]=="#SS:Length_retain_pattern");CheckError(Index,"Assessment - length retain patterns") 
  Ass$Retain_size_pattern <- as.numeric(AssInput[Index+1,1:General$Nfleet]) 
  Ass$Retain_size_par_init <- matrix(0,nrow=General$Ncat_fleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_size_parameters");CheckError(Index,"Assessment - length retain parameters")           
  for (Ifleet in 1:General$Ncat_fleet) Ass$Retain_size_par_init[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Retain_size_par_phase <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_size_par_phase");CheckError(Index,"Assessment - length retention parameter phases")          
  for (Ifleet in 1:General$Ncat_fleet) Ass$Retain_size_par_phase[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Index <- which(AssInput[,1]=="#SS:Retain_size_par_blocks");CheckError(Index,"Assessment - length retention block")          
  Ass$Retain_size_par_blks <- matrix(0,nrow=General$Nfleet,ncol=15)
  for (Ifleet in 1:General$Ncat_fleet) Ass$Retain_size_par_blks[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Index <- which(AssInput[,1]=="#SS:Retain_size_par_fns");CheckError(Index,"Assessment - length retention block_phases")          
  Ass$Retain_size_par_fns <- matrix(0,nrow=General$Nfleet,ncol=15)
  for (Ifleet in 1:General$Ncat_fleet) Ass$Retain_size_par_fns[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Retain_size_ann_devs_use <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_size_ann_devs_use");CheckError(Index,"Assessment - length retention use_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Retain_size_ann_devs_use[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Retain_size_ann_devs_miny <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_size_ann_devs_miny");CheckError(Index,"Assessment - length retention miny_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Retain_size_ann_devs_miny[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Retain_size_ann_devs_maxy <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_size_ann_devs_maxy");CheckError(Index,"Assessment - length retention maxy_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Retain_size_ann_devs_maxy[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Retain_size_ann_devs_phs <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_size_ann_devs_phs");CheckError(Index,"Assessment - length retention phs_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Retain_size_ann_devs_phs[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  
  Ass$Mort_size_par_init <- matrix(0,nrow=General$Ncat_fleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Mort_size_parameters");CheckError(Index,"Assessment - length mort parameters")           
  for (Ifleet in 1:General$Ncat_fleet) Ass$Mort_size_par_init[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Mort_size_par_phase <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Mort_size_par_phase");CheckError(Index,"Assessment - length mort parameter phases")          
  for (Ifleet in 1:General$Ncat_fleet) Ass$Mort_size_par_phase[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  
  
  Index <- which(AssInput[,1]=="#SS:Number_of_non-dev_time_varying_pars_size");CheckError(Index,"Assessment - number of size non-dev time varying parameters")
  Ass$N.non.dev.time.varying.selex.pars.size <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#SS:Number_of_time_varying_pars_size");CheckError(Index,"Assessment - number of size time varying parameters")
  Ass$N.time.varying.selex.pars.size <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#SS:Time_varying_size_pars_vals");CheckError(Index,"Assessment - selex size time varying parameters")
  Ass$Time.varying.selex.size.par.vals <- as.numeric(AssInput[Index+1,1:Ass$N.time.varying.selex.pars.size])
  Index <- which(AssInput[,1]=="#SS:Time_varying_size_pars_phs");CheckError(Index,"Assessment - selex size time varying parameters phs")
  Ass$Time.varying.selex.size.par.phs <- as.numeric(AssInput[Index+1,1:Ass$N.time.varying.selex.pars.size])
  
  Ass$N.size.selexPars <- rep(0,General$Nfleet);
  for (Ifleet in 1:General$Nfleet) 
   {
    if (Ass$Selex_size_pattern[Ifleet]==1) Ass$N.size.selexPars[Ifleet] <- 2
    if (Ass$Selex_size_pattern[Ifleet]==24) Ass$N.size.selexPars[Ifleet] <- 6
    if (Ass$Selex_size_pattern[Ifleet]==17) Ass$N.size.selexPars[Ifleet] <- Ass$Selex_size_nsel.pars[Ifleet]
    if (Ass$Selex_size_pattern[Ifleet]==27) Ass$N.size.selexPars[Ifleet] <- Ass$Selex_size_nsel.pars[Ifleet]
    if (Ass$Selex_size_pattern_male[Ifleet]==3 & Ass$Selex_size_pattern[Ifleet]==24) Ass$N.size.selexPars[Ifleet] <- Ass$N.size.selexPars[Ifleet] + 5
    if (Ass$Selex_size_pattern_male[Ifleet]==3 & Ass$Selex_size_pattern[Ifleet]==1) Ass$N.size.selexPars[Ifleet] <- Ass$N.size.selexPars[Ifleet] + 3
  }
  if (General$Uncertain.Level==1)
   {
    for (Ifleet in 1:General$Nfleet) 
     {
      for (Icol in 1:15) if (Ass$Selex_size_par_phase[Ifleet,Icol]!=0) Ass$Selex_size_par_phase[Ifleet,Icol] <- -2 
      Ass$Selex_size_ann_devs_phs[Ifleet,] <- -2
      for (Icol in 1:15) if (Ass$Retain_size_par_phase[Ifleet,Icol]!=0) Ass$Retain_size_par_phase[Ifleet,Icol] <- -2 
      Ass$Retain_size_ann_devs_phs[Ifleet,] <- -2
      for (Icol in 1:15) if (Ass$Mort_size_par_phase[Ifleet,Icol]!=0) Ass$Mort_size_par_phase[Ifleet,Icol] <- -2 
     }
    for (Icol in 1:Ass$N.time.varying.selex.pars.siz) Ass$Time.varying.selex.size.par.phs[Icol] <- -2
   }
  
  if (FullOutput==T) cat("Done: Assessment length selectivity\n")
  
  # Age selectivity and retention
  Index <- which(AssInput[,1]=="#SS:Age_selex_pattern"); CheckError(Index,"Assessment - age selectivity patterns") 
  Ass$Selex_age_pattern <- as.numeric(AssInput[Index+1,1:General$Nfleet]) 
  Index <- which(AssInput[,1]=="#SS:Age_selex_pattern_male"); CheckError(Index,"Assessment - age selectivity patterns (male)") 
  Ass$Selex_age_pattern_male <- as.numeric(AssInput[Index+1,1:General$Nfleet]) 
  Index <- which(AssInput[,1]=="#SS:Mirrored_age_selex");CheckError(Index,"Assessment - age selectivity pattern mirrors")        
  Ass$Selex_age_mirror_fleet  <- as.numeric(AssInput[Index+1,1:General$Nfleet]) 
  Index <- which(AssInput[,1]=="#SS:N.estimated.selex.age.pars");CheckError(Index,"Assessment - number of selectivity parameters per fleet")        
  Ass$Selex_age_nsel.pars <- as.numeric(AssInput[Index+1,1:General$Nfleet]) 
  Ass$Selex_age_par_init <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_age_parameters");CheckError(Index,"Assessment - age selectivity parameters")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_age_par_init[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_age_par_phase <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_age_par_phase");CheckError(Index,"Assessment - age selectivity parameter phases")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_age_par_phase[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_age_par_blks <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_age_par_blocks");CheckError(Index,"Assessment - age selectivity blocks")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_age_par_blks[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_age_par_fns <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_age_par_fns");CheckError(Index,"Assessment - age selectivity block_phases")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_age_par_fns[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_age_ann_devs_use <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_age_ann_devs_use");CheckError(Index,"Assessment - age selectivity use_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_age_ann_devs_use[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_age_ann_devs_minys <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_age_ann_devs_miny");CheckError(Index,"Assessment - age selectivity miny_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_age_ann_devs_miny[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_age_ann_devs_maxy <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_age_ann_devs_maxy");CheckError(Index,"Assessment - age selectivity maxy_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_age_ann_devs_maxy[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Selex_age_ann_devs_phs <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Selex_age_ann_devs_phs");CheckError(Index,"Assessment - age selectivity phs_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Selex_age_ann_devs_phs[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  
  Index <- which(AssInput[,1]=="#SS:Age_retain_pattern");CheckError(Index,"Assessment - age retain patterns") 
  Ass$Retain_age_pattern <- as.numeric(AssInput[Index+1,1:General$Nfleet]) 
  Ass$Retain_age_par_init <- matrix(0,nrow=General$Ncat_fleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_age_parameters");CheckError(Index,"Assessment - age retain parameters")           
  for (Ifleet in 1:General$Ncat_fleet) Ass$Retain_age_par_init[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Retain_age_par_phase <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_age_par_phase");CheckError(Index,"Assessment - age retention parameter phases")          
  for (Ifleet in 1:General$Ncat_fleet) Ass$Retain_age_par_phase[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Index <- which(AssInput[,1]=="#SS:Retain_age_par_blocks");CheckError(Index,"Assessment - age retention block")          
  Ass$Retain_age_par_blks <- matrix(0,nrow=General$Nfleet,ncol=15)
  for (Ifleet in 1:General$Ncat_fleet) Ass$Retain_age_par_blks[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Index <- which(AssInput[,1]=="#SS:Retain_age_par_fns");CheckError(Index,"Assessment - age retention block_phases")          
  Ass$Retain_age_par_fns <- matrix(0,nrow=General$Nfleet,ncol=15)
  for (Ifleet in 1:General$Ncat_fleet) Ass$Retain_age_par_fns[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Retain_age_ann_devs_use <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_age_ann_devs_use");CheckError(Index,"Assessment - age retention use_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Retain_age_ann_devs_use[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Retain_age_ann_devs_miny <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_age_ann_devs_miny");CheckError(Index,"Assessment - age retention miny_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Retain_age_ann_devs_miny[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Retain_age_ann_devs_maxy <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_age_ann_devs_maxy");CheckError(Index,"Assessment - age retention maxy_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Retain_age_ann_devs_maxy[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Retain_age_ann_devs_phs <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Retain_age_ann_devs_phs");CheckError(Index,"Assessment - age retention phs_devs")      
  for (Ifleet in 1:General$Nfleet) Ass$Retain_age_ann_devs_phs[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  
  Ass$Mort_age_par_init <- matrix(0,nrow=General$Ncat_fleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Mort_age_parameters");CheckError(Index,"Assessment - age mort parameters")           
  for (Ifleet in 1:General$Ncat_fleet) Ass$Mort_age_par_init[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  Ass$Mort_age_par_phase <- matrix(0,nrow=General$Nfleet,ncol=15)
  Index <- which(AssInput[,1]=="#SS:Mort_age_par_phase");CheckError(Index,"Assessment - age mort parameter phases")          
  for (Ifleet in 1:General$Ncat_fleet) Ass$Mort_age_par_phase[Ifleet,]  <- as.numeric(AssInput[Index+Ifleet,1:15])
  
  
  Index <- which(AssInput[,1]=="#SS:Number_of_non-dev_time_varying_pars_age");CheckError(Index,"Assessment - number of age non-dev time varying parameters")
  Ass$N.non.dev.time.varying.selex.pars.age <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#SS:Number_of_time_varying_pars_age");CheckError(Index,"Assessment - number of age time varying parameters")
  Ass$N.time.varying.selex.pars.age <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#SS:Time_varying_age_pars_vals");CheckError(Index,"Assessment - selex age time varying parameters")
  Ass$Time.varying.selex.age.par.vals <- as.numeric(AssInput[Index+1,1:Ass$N.time.varying.selex.pars.age])
  Index <- which(AssInput[,1]=="#SS:Time_varying_age_pars_phs");CheckError(Index,"Assessment - selex age time varying parameters phs")
  Ass$Time.varying.selex.age.par.phs <- as.numeric(AssInput[Index+1,1:Ass$N.time.varying.selex.pars.age])
  
  Ass$N.age.selexPars <- rep(0,General$Nfleet);
  for (Ifleet in 1:General$Nfleet) 
  {
    if (Ass$Selex_age_pattern[Ifleet]==12) Ass$N.age.selexPars[Ifleet] <- 2
    if (Ass$Selex_age_pattern[Ifleet]==20) Ass$N.age.selexPars[Ifleet] <- 6
    if (Ass$Selex_age_pattern[Ifleet]==14) Ass$N.age.selexPars[Ifleet] <- Ass$Selex_age_nsel.pars[Ifleet]
    if (Ass$Selex_age_pattern[Ifleet]==17) Ass$N.age.selexPars[Ifleet] <- Ass$Selex_age_nsel.pars[Ifleet]
    if (Ass$Selex_age_pattern[Ifleet]==27) Ass$N.age.selexPars[Ifleet] <- Ass$Selex_age_nsel.pars[Ifleet]
    if (Ass$Selex_age_pattern_male[Ifleet]==3 & Ass$Selex_age_pattern[Ifleet]==20) Ass$N.age.selexPars[Ifleet] <- Ass$N.age.selexPars[Ifleet] + 5
    if (Ass$Selex_age_pattern_male[Ifleet]==3 & Ass$Selex_age_pattern[Ifleet]==12) Ass$N.age.selexPars[Ifleet] <- Ass$N.age.selexPars[Ifleet] + 3
  }
  if (FullOutput==T) cat("Done: Assessment age selectivity\n")
  
  # --------------------------------------------------------------------------------------------------------------
  
  # --------------------------------------------------------------------------------------------------------------
  
  #Time-varying stuff
  Ass$TVKappa <- rep(0,9)
  Ass$TVLenA1 <- rep(0,9)
  Ass$TVLenA2 <- rep(0,9)
  Ass$TVRecr <- rep(0,9)
  Ass$TVM <- rep(0,9)
  Ass$TVR0Off <- rep(0,9)
  Index <- which(AssInput[,1]=="#SS:TimeVarM");      if (length(Index) >0) Ass$TVM[1:7]      <- as.numeric(AssInput[Index,2:8])    
  Index <- which(AssInput[,1]=="#SS:TimeVarLenA1");  if (length(Index) >0) Ass$TVLenA1[1:7]  <- as.numeric(AssInput[Index,2:8])    
  Index <- which(AssInput[,1]=="#SS:TimeVarLenA2");  if (length(Index) >0) Ass$TVLenA2[1:7]  <- as.numeric(AssInput[Index,2:8])    
  Index <- which(AssInput[,1]=="#SS:TimeVarKappa");  if (length(Index) >0) Ass$TVKappa[1:7]  <- as.numeric(AssInput[Index,2:8])    
  Index <- which(AssInput[,1]=="#SS:TimeVarRecr");   if (length(Index) >0) Ass$TVRecr[1:7]   <- as.numeric(AssInput[Index,2:8])    
  Index <- which(AssInput[,1]=="#SS:TimeVarR0Off");  if (length(Index) >0) Ass$TVR0Off[1:7]  <- as.numeric(AssInput[Index,2:8])    
  if (Ass$TVM[1]     != 0) Ass$TVM[1]     <- Ass$TVM[1]     + Num.Pred
  if (Ass$TVLenA1[1] != 0) Ass$TVLenA1[1] <- Ass$TVLenA1[1] + Num.Pred
  if (Ass$TVLenA2[1] != 0) Ass$TVLenA2[1] <- Ass$TVLenA2[1] + Num.Pred
  if (Ass$TVKappa[1] != 0) Ass$TVKappa[1] <- Ass$TVKappa[1] + Num.Pred
  if (Ass$TVRecr[1]  != 0) Ass$TVRecr[1]  <- Ass$TVRecr[1]  + Num.Pred
  if (Ass$TVR0Off[1]  > 0) Ass$TVR0Off[1]  <- Ass$TVR0Off[1]  + Num.Pred
  
  
  # Catchability
  Ass$UseQinit <- rep(T,General$Nfleet+Num.Pred)
  Ass$Qinit <- rep(1,General$Nfleet+Num.Pred)
  Ass$Qphase <- rep(1,General$Nfleet+Num.Pred)
  Index <- which(AssInput[,1]=="#SS:UseQinit"); if (length(Index) >0) Ass$UseQinit = AssInput[Index,2:(General$Nfleet+Num.Pred+1)]
  Index <- which(AssInput[,1]=="#SS:Qinit"); if (length(Index) >0) Ass$Qinit = as.numeric(AssInput[Index,2:(General$Nfleet+Num.Pred+1)])
  Index <- which(AssInput[,1]=="#SS:Qphase"); if (length(Index) >0) Ass$Qphase = as.numeric(AssInput[Index,2:(General$Nfleet+Num.Pred+1)])
  if (FullOutput==T) cat("Done: Assessment catchabiity\n")
  
  # -----------------------------------------------------------------------------------------------------------------------------------
  
  # Predator prey option
  Ass$Pred_opt <- "None"
  Index <- which(AssInput[,1]=="#SS_pred_opt"); if (length(Index) >0) Ass$Pred_opt = AssInput[Index,2]
  if (Num.Pred <= 0) Ass$Pred_opt <- "None"
  if (FullOutput==T) cat("Assessment : Predator option:",Ass$Pred_opt,"\n")

  if (Num.Pred > 0)
   {
    Ass$Pred_Selex_par_init <- matrix(NA,nrow=Num.Pred,ncol=8)
    Index <- which(AssInput[,1]=="#SS:Pred_selex_parameters");CheckError(Index,"Assessment - predator selex parameters")               
    for (Ipred in 1:Num.Pred) Ass$Pred_Selex_par_init[Ipred,]  <- as.numeric(AssInput[Index+Ipred,1:8])
    Ass$Pred_Selex_par_phase <- matrix(NA,nrow=Num.Pred,ncol=8)
    Index <- which(AssInput[,1]=="#SS:Pred_selex_phase");CheckError(Index,"Assessment - predator selex parameters phase ")                    
    for (Ipred in 1:Num.Pred) Ass$Pred_Selex_par_phase[Ipred,]  <- as.numeric(AssInput[Index+Ipred,1:8])
   }
  
  # Tier 1 specifications
  Index <- which(AssInput[,1]=="#Tier_1:MSY");                                     Ass$MSY <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_1:SPR_target");                              Ass$SPR_target <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_1:Biomass_target");                          Ass$Biomass_target <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_1:Forecast_type");                           Ass$Forecast_type <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_1:Control_rule_inflection");                 Ass$C_inflect <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_1:Control_rule_Biomass_level_for_no_F");     Ass$C_NoF <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_1:Control_rule_Protection_level_for_no_F")
  if (length(Index))
   {
    Ass$C_Prot <- as.numeric(AssInput[Index,2]);
    Ass$C_NoF <- -1*Ass$C_NoF;
   }
  else
    Ass$C_Prot <- 0
  Index <- which(AssInput[,1]=="#Tier_1:Buffer");                               Ass$C_Buffer <- as.numeric(AssInput[Index,2])
  if (FullOutput==T) cat("Done: Assessment Tier 1\n")

  # Tier 4a specifications
  Index <- which(AssInput[,1]=="#Tier_4a:logr_init");      if (length(Index)!=0) Ass$Tier4a$logr_init <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:K_init_mult");    if (length(Index)!=0) Ass$Tier4a$K_init_mult <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:alpha");          if (length(Index)!=0) Ass$Tier4a$alpha <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:beta");           if (length(Index)!=0) Ass$Tier4a$beta <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:DT4.target");     if (length(Index)!=0) Ass$Tier4a$DT4.target <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:MSYPriorY1");     if (length(Index)!=0) Ass$Tier4a$MSYPriorY1 <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:MSYPriorY2");     if (length(Index)!=0) Ass$Tier4a$MSYPriorY2 <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:MSYL");           if (length(Index)!=0) Ass$Tier4a$MSYL <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:MSYRY1");         if (length(Index)!=0) Ass$Tier4a$MSYRY1 <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:MSYRY2");         if (length(Index)!=0) Ass$Tier4a$MSYRY2 <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:EstR");           if (length(Index)!=0) Ass$Tier4a$EstR <- AssInput[Index,2]
  Index <- which(AssInput[,1]=="#Tier_4a:EstZ");           if (length(Index)!=0) Ass$Tier4a$EstZ <- AssInput[Index,2]
  Index <- which(AssInput[,1]=="#Tier_4a:ProcessError");   if (length(Index)!=0) Ass$Tier4a$ProcessError <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:CVMSYL");         if (length(Index)!=0) Ass$Tier4a$CVMSYL <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:PriorMeanR");     if (length(Index)!=0) Ass$Tier4a$PriorMeanR <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4a:PriorSDr");       if (length(Index)!=0) Ass$Tier4a$PriorSDr <- as.numeric(AssInput[Index,2])
  if (FullOutput==T) cat("Done: Assessment Tier 4a (DT4)\n")
  

  # Tier 4b specifications (Tier 4)
  Ass$Tier4b <- NULL
  Index <- which(AssInput[,1]=="#Tier_4b:Cpue_Area");         if (length(Index)!=0) Ass$Tier4b$Cpue_Area <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4b:Cpue_Index");        if (length(Index)!=0) Ass$Tier4b$Cpue_Index <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4b:Cpue_avg_yr");       if (length(Index)!=0) Ass$Tier4b$Cpue_avg_yr <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4b:Cpue_target_mult");  if (length(Index)!=0) Ass$Tier4b$Cpue_target_mult <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4b:Cpue_limit_mult");   if (length(Index)!=0) Ass$Tier4b$Cpue_limit_mult <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4b:HCR_target");        if (length(Index)!=0) Ass$Tier4b$HCR_target <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4b:Max.catch");         if (length(Index)!=0) Ass$Tier4b$Max.catch <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4b:Cpue_Yr1");          if (length(Index)!=0) Ass$Tier4b$Cpue_Yr1 <- as.numeric(AssInput[Index,2])
  Index <- which(AssInput[,1]=="#Tier_4b:Cpue_Yr2");          if (length(Index)!=0) Ass$Tier4b$Cpue_Yr2 <- as.numeric(AssInput[Index,2])
  
  if (FullOutput==T) cat("Done: Assessment Tier 4b (Traditional Tier 4)\n")
  
  #Tier 0 specifications
  Index <- which(AssInput[,1]=="#Tier_0:Fixed");                                     Ass$Prespecified.TAC <- as.numeric(AssInput[Index,2])
  if (FullOutput==T) cat("Done: Assessment Tier 0\n")
  
  #Tier 5 specifications
  Index <- which(AssInput[,1]=="#Tier_5:Option");      
  if (Ass$Type == "Tier_5" & length(Index)>0)
   {
    Ass$Tier5.Option <- as.numeric(AssInput[Index,2])
    Ass$TAC.management <- "No"
    if (Ass$Tier5.Option %in% c(1,21)) 
     {
      Index <- which(AssInput[,1]=="#Tier_5:Years");
      Ass$Tier5.Years <-as.numeric(AssInput[Index,c(2,3)])
    }
    Ass$Tier5.Rebuilding <- F
   }
  
  # Tier 6 specifications
  if (Ass$Type == "Tier_6")
   {
    print("Caitlin is here") 
    
   }
  
  
  # Assessment results
  Ass$SSB.estimates  <- array(NA,dim=c(General$Nsim,General$Nproj,10,ncol=Nhist+General$Nproj+1))
  Ass$Depl.estimates <- array(NA,dim=c(General$Nsim,General$Nproj,10,ncol=Nhist+General$Nproj+1))
  Ass$Recr.estimates <- array(NA,dim=c(General$Nsim,General$Nproj,10,ncol=Nhist+General$Nproj+1))
  Ass$Par.estimates <- array(NA,dim=c(General$Nsim,General$Nproj,10,ncol=6*Nsex+1))
  Ass$M.estimates <- array(NA,dim=c(General$Nsim,General$Nproj,10,ncol=Nhist+General$Nproj+1))
  Ass$AllPar.estimates <- array(NA,dim=c(General$Nsim,General$Nproj,10,ncol=1000))
  Ass$MaxGrads <- array(NA,dim=c(General$Nsim,General$Nproj,10))
  
  #Insurance options
  Ass$Has.insurance <- F
  Index <- which(AssInput[,1]=="#Use.insurance");
  if (length(Index)>0)
   {
    Ass$Has.insurance <- T
    Index <- which(AssInput[,1]=="#Insurance_risk_ave");CheckError(Index,"Insurance: Years to compute loss")               
    Ass$Insurance_risk_ave  <- as.numeric(AssInput[Index,2])
    Index <- which(AssInput[,1]=="#Insurance_profit_margin");CheckError(Index,"Insurance: Profit margin")               
    Ass$Insurer_profit_margin  <- as.numeric(AssInput[Index,2])
    Index <- which(AssInput[,1]=="#Insurance_delta");CheckError(Index,"Insurance: Delta")               
    Ass$Delta  <- as.numeric(AssInput[Index,2])
  }

  
  # Buffers
  Ass$Buffer <- 1
  Index <- which(AssInput[,1]=="#Buffer_specifications:Default"); if (length(Index)>0) Ass$Buffer <- as.numeric(AssInput[Index,2])  
  Ass$Min.Buffer <- 0
  Index <- which(AssInput[,1]=="#Buffer_specifications:Minimum"); if (length(Index)>0) Ass$Min.Buffer <- as.numeric(AssInput[Index,2])  
  Ass$Stale.rate <- 0
  Index <- which(AssInput[,1]=="#Buffer_specifications:Stale_rate"); if (length(Index)>0) Ass$Stale.rate <- as.numeric(AssInput[Index,2])  
  
  # Constaints on TACs
  Ass$Min.TAC <- 0
  Index <- which(AssInput[,1]=="#Constraints:Min_TAC"); if (length(Index)>0) Ass$Min.TAC <- as.numeric(AssInput[Index,2])  
  Ass$Beta1 <- -100
  Index <- which(AssInput[,1]=="#Constraints:Min_change"); if (length(Index)>0) Ass$Beta1 <- as.numeric(AssInput[Index,2])  
  Ass$Beta2 <- -100
  Index <- which(AssInput[,1]=="#Constraints:Max_change"); if (length(Index)>0) Ass$Beta2 <- as.numeric(AssInput[Index,2])  
  Ass$Last.TAC <- -100
  Index <- which(AssInput[,1]=="#Constraints:Last_TAC"); if (length(Index)>0) Ass$Last.TAC <- as.numeric(AssInput[Index,2])  
  
  # Relallocation of TACS
  Ass$Relallocation <- 0
  Index <- which(AssInput[,1]=="#Reallocation_specifications:Option"); if (length(Index)>0) Ass$Relallocation <- as.numeric(AssInput[Index,2])  
  Ass$Relallocation.vals <- rep(1,General$Ncat_fleet)
  Index <- which(AssInput[,1]=="#Reallocation_specifications:Values"); 
  if (length(Index)>0) Ass$Relallocation.vals <- as.numeric(AssInput[Index+1,1:General$Ncat_fleet])  
  
  #Close fishery in catastrophes
  Ass$Close.in.MHW <- "No"
  Index <- which(AssInput[,1]=="#Close.if.MHW"); if (length(Index) > 0) Ass$Close.in.MHW <- AssInput[Index,2]
  
  StockDetails$Ass <- Ass
  cat("Done: Read input for stock",Istock,"\n\n")
  
  # Special when there multiple stocks
  if (General$Nstock >0)
   {
    if (General$Indicator.Last.TAC.Code[Istock]==1) General$Indicator.Last.TAC[Istock] <<- General$Indicator.Last.TAC.specs[Istock,1]
    if (General$Indicator.Last.TAC.Code[Istock]==2)
     {
      if (General$Indicator.Last.TAC.specs[Istock,1] > Nhist) { cat("Year for averaging catches out of range"); stop()}
      AveCat <- 0; Num.Year <- 0
      for (Iyear in 0:General$Indicator.Last.TAC.specs[Istock,1])
       { AveCat <- AveCat + sum(StockDetails$CatchInp[Nhist-Iyear,]); Num.Year <- Num.Year + 1; }
      General$Indicator.Last.TAC[Istock] <<- AveCat/Num.Year
     } # Option 2
    
   } # Only do this for multistock models
  
  # ----------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  # Save stuff to the parfile
  if (Isim==1)
   {
    ParsOut <- c(rep(Mbase,Nsex),LenA1,LenA2,Kappa,CV1,CV2,log(StockDetails$R0))
    VecOut <- c(Istock,0,0,1,ParsOut)
    write(VecOut,ncol=length(VecOut),file=General$ParSaveName,append=T) 
   }
  
  # ----------------------------------------------------------------------------------------------------------------------------------------------------------  
  
  return(StockDetails)
  }

