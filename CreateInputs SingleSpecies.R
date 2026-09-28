rm(list=ls())

Path <- "C:/MSE/ratpackR/"
setwd(Path)

library(r4ss)
library(readxl)
source(paste0(Path,"R inserts/SaveFiles.R"))

#
# =====================================================================================================================
#
ExtractMG <- function(parameters,Name,offset,nsex)
 {
  Index <- which(substr(parameters$Label,1,nchar(Name))==Name)
  Values <- parameters$Value[Index[1:nsex]]
  if (nsex == 2 & length(Index)>1 & offset==1 & Values[2]==0) Values[2] <- Values[1]*exp(Values[2])
  if (nsex == 2 & length(Index)>1 & offset==2) Values[2] <- Values[1]*exp(Values[2])
  if (nsex == 2 & length(Index)>1 & offset==3) Values[2] <- Values[1]*exp(Values[2])
  return(Values)
 }

#
# =====================================================================================================================

do.extract <- function(SS_folder,Species2,YearAdjust=0,AgeAdjust=F,Reduce.Uncertain=1,
                       Control.Rule.Type="NA",Project.Type=1,
                       NextraCat=0,CatYears=NULL,CatFuture=NULL,FakeAreas=0,FillProjection=F)
{
  Default.Proj <- 25; if (FillProjection==F) Default.Proj <- 0
  
  OM.obj <- NULL                              # The object with the OM file
  EM.obj <- NULL                              # The object with the EM file
  
  FileName1 <- paste0(Species2,".OM")
  FileName2 <- paste0(Species2,".EM")
  FileName3 <- paste0("General_",Species2,".OM")
  
  RunFolder <- paste0(AssignmentPath,SS_folder)
  cat(RunFolder,"\n")
  RunFolder2 <- paste0(BasePath,FileName1)
  cat(RunFolder2,"\n")
  RunFolder3 <- paste0(BasePath,FileName2)
  cat(RunFolder3,"\n")
  RunFolder4 <- paste0(BasePath,FileName3)
  cat(RunFolder4,"\n")
  
  RunFolder2a <- paste0(RunFolder2,".SAV")
  RunFolder3a <- paste0(RunFolder3,".SAV")

  starter <<- SS_readstarter(file.path(RunFolder, "starter.ss"),verbose=F)
  dat <<- SS_readdat(file.path(RunFolder, starter$datfile),verbose=F)
  ctl <<- SS_readctl(file.path(RunFolder, starter$ctlfile),datlist=dat)

  rep.filename <- paste0("REPORT.SSO")
  comp.filename <- paste0("COMPREPORT.SSO")
  model0 <<- SS_output(dir=RunFolder,repfile=rep.filename,covar=F,verbose=F,printstat=F)
  styr <- dat$styr
  endyrOrig <- dat$endyr
  nyearsOrig <- endyrOrig - styr + 1
  endyr <- dat$endyr+NextraCat
  nyears <- endyr - styr + 1
  nyear1 <- endyr+1 - styr + 1
  nareas <- dat$N_areas
  nareas.fake <- 0
  if (nareas > 1) print("WARNING MULTIPLE AREAS")
  if (nareas > 1 & FakeAreas != 0) { print("Fake area needs to be zero when nareas > 0"); stop() }
  nages <- dat$Nages+1
  nsexUse <- dat$Nsexes
  nsex <- dat$Nsexes; if (nsex == -1) nsex <- 1
  nlen <- length(model0$biology$Len_lo)
  nfleets <- dat$Nfleets
  catch.fleets <- which(dat$fleetinfo$type==1)
  area.fleets <- dat$fleetinfo$area
  nfleet.catch <- max(catch.fleets)
  
  OM.obj$styr <- styr
  OM.obj$endyr <- endyr
  OM.obj$nyears <- nyears
  OM.obj$nyear1 <- nyear1
  OM.obj$nareas <- nareas
  OM.obj$nages <- nages
  OM.obj$nsex <- nsex
  OM.obj$nlen <- nlen
  OM.obj$nsexUse <- nsexUse
  OM.obj$nfleets <- nfleets
  OM.obj$nfleet.catch <- nfleet.catch
  OM.obj$area.fleets <- area.fleets
  OM.obj$Species2 <- Species2
  OM.obj$fleetnames <- dat$fleetinfo$fleetname
  OM.obj$FillProjection <- FillProjection
  OM.obj$Default.Proj <- Default.Proj
  
  # Weird SESSF stuff
  #if (ctl$parameter_offset_approach==3) ctl$parameter_offset_approach <- 2

  # ===========================================================================================================================================================
  # ===========================================================================================================================================================
  
  # Area-stuff
  if (nareas>1)
  {
    print("need to find relative densities")
    
    Index <- which(substr(rownames(model0$parameters),1,18)=="RecrDist_GP_1_area")[1:nareas]
    Pars <- exp(model0$parameters$Value[Index])
    OM.obj$Rel.Dens <- Pars/sum(Pars)
    
    OM.obj$movement.rate <- matrix(0,nrow=nareas*(nareas-1),ncol=nages)
    for (Iarea in 1:nareas)
     {
      Icnt <- 0
      for (Jarea in 1:nareas)
       if (Iarea != Jarea) 
        {
         Icnt <- Icnt + 1
         Index <- which(model0$movement$Source_area == Iarea & model0$movement$Dest_area == Jarea) 
          if (length(Index))
          {
           Moves <- as.numeric(model0$movement[Index,6+1:nages])
           Index2 <- (nareas-1)*(Iarea-1)+Icnt
           OM.obj$movement.rate[Index2,] <- Moves
          }
        } # area x area
      } # Iarea
    } # nareas
  

  # Area-stuff
  if (FakeAreas>1)
   {
    
    nareas <- FakeAreas
    Pars <- rep(1,nareas)
    OM.obj$Rel.Dens <- Pars/sum(Pars)
    
    # Default movement (0.05 among all areas)
    OM.obj$movement.rate <- matrix(0,nrow=nareas*(nareas-1),ncol=nages)
    for (Iarea in 1:nareas)
     {
      Icnt <- 0
      for (Jarea in 1:nareas)
       if (Iarea != Jarea) 
        {
         Icnt <- Icnt + 1
         Index2 <- (nareas-1)*(Iarea-1)+Icnt
         OM.obj$movement.rate[Index2,] <- 0.05
        }
     } # Iarea
    
    area.fleets <- matrix(1,nrow=nareas,ncol=nfleets)
    
    # Save various things
    OM.obj$area.fleets <- area.fleets
    OM.obj$area.fleets[1,1] <- 0
    OM.obj$area.fleets[4,1] <- 0
    OM.obj$area.fleets[2,nfleets] <- 0
    OM.obj$area.fleets[3,nfleets] <- 0
    OM.obj$nareas <- nareas 
   } # FakeAreas

  # ===========================================================================================================================================================
  # ===========================================================================================================================================================
  
  lbins <- model0$biology$Len_lo
  lbins <- c(lbins,2*lbins[nlen]-lbins[nlen-1])
  OM.obj$lbins <- lbins
  
  # Natural mortality
  OM.obj$natM_type <- ctl$natM_type
  if (!(OM.obj$natM_type %in% c(0,3))) { print("M type in error; stopping"); stop()}
  OM.obj$Mbase <- -1
  if (OM.obj$natM_type==0) OM.obj$Mbase <- model0$parameters$Value[1]
  OM.obj$Mvals <- matrix(OM.obj$Mbase,nrow=nsex,ncol=nages)
  if (OM.obj$natM_type==3) for (Isex in 1:nsex) for (Iage in 1:nages) OM.obj$Mvals[Isex,Iage] <- ctl$natM[Isex,Iage]
  
  # Length-at-age
  OM.obj$Growth_model <- ctl$GrowthModel
  if (!(OM.obj$Growth_model %in% c(1,2))) { print("Growth model is out of range; stopping"); stop()}
  
  
  OM.obj$Age_for_L1 <- ctl$Growth_Age_for_L1
  OM.obj$Age_for_L2 <- ctl$Growth_Age_for_L2
  Values <- ExtractMG(model0$parameters,"L_at_Amin",ctl$parameter_offset_approach,nsex)
  OM.obj$LAmin <- Values
  Values <- ExtractMG(model0$parameters,"L_at_Amax",ctl$parameter_offset_approach,nsex)
  OM.obj$LAmax <- Values
  Values <- ExtractMG(model0$parameters,"VonBert_K",ctl$parameter_offset_approach,nsex)
  OM.obj$Kappa <- Values
  if (OM.obj$Growth_model==2)
   {
    Values <- ExtractMG(model0$parameters,"Richards",ctl$parameter_offset_approach,nsex)
    OM.obj$Richards <- Values
   }
 
  OM.obj$CV_Growth_Pattern <- ctl$CV_Growth_Pattern
  if (ctl$CV_Growth_Pattern==0 || ctl$CV_Growth_Pattern==1)
   {
    ValuesA <- ExtractMG(model0$parameters,"CV_young",ctl$parameter_offset_approach,nsex)
    OM.obj$CVyoung <- ValuesA
    ValuesB <- ExtractMG(model0$parameters,"CV_old",ctl$parameter_offset_approach,nsex)
    EM.obj$CVold <- ValuesB
    if (ctl$parameter_offset_approach==0) OM.obj$CVold <- ValuesB
    if (ctl$parameter_offset_approach==1) OM.obj$CVold <- ValuesB
    #if (ctl$parameter_offset_approach==2) OM.obj$CVold <- ValuesA*exp(ValuesB)
    if (ctl$parameter_offset_approach==2) OM.obj$CVold <- ValuesB
    if (ctl$parameter_offset_approach==3) OM.obj$CVold <- OM.obj$CVyoung*exp(ValuesB)
  }
  if (ctl$CV_Growth_Pattern==2)
   {
    ValuesA <- ExtractMG(model0$parameters,"SD_young",ctl$parameter_offset_approach,nsex)
    OM.obj$CVyoung <- ValuesA
    #if (ctl$parameter_offset_approach==3 & nsex == 2) OM.obj$CVyoung[2] <- ValuesA[1]*exp(ValuesA[2])
    ValuesB <- ExtractMG(model0$parameters,"SD_old",ctl$parameter_offset_approach,nsex)
    if (ctl$parameter_offset_approach==0) OM.obj$CVold <- ValuesB
    if (ctl$parameter_offset_approach==1) OM.obj$CVold <- ValuesB
    #if (ctl$parameter_offset_approach==2) OM.obj$CVold <- ValuesA*exp(ValuesB)
    if (ctl$parameter_offset_approach==2) OM.obj$CVold <- ValuesB
    if (ctl$parameter_offset_approach==3) OM.obj$CVold <- OM.obj$CVyoung*exp(ValuesB)
  }
  # Weight-length
  Values <- ExtractMG(model0$parameters,"Wtlen_1",0,nsex)
  OM.obj$Wtlen_1 <- Values
  Values <- ExtractMG(model0$parameters,"Wtlen_2",0,nsex)
  OM.obj$Wtlen_2 <- Values

  # Maturity
  OM.obj$maturity_option <- ctl$maturity_option
  OM.obj$age_maturity <- NULL
  Values <- ExtractMG(model0$parameters,"Mat50%",0,1)
  OM.obj$Mat50 <- Values
  Values <- ExtractMG(model0$parameters,"Mat_slope",0,1)
  OM.obj$Mat_Slope <- Values
  OM.obj$fecundity_option <- ctl$fecundity_option
  OM.obj$first_mat_age <-ctl$First_Mature_Age
  if (!(OM.obj$fecundity_option %in% c(1,2,3,4))) { print("fecundity_option out of range; stopping"); AAA}
  if (OM.obj$fecundity_option==1)
   {
    Values <- ExtractMG(model0$parameters,"Eggs/kg_inter",0,1); OM.obj$Egg_1 <- Values
    Values <- ExtractMG(model0$parameters,"Eggs/kg_slope",0,1); OM.obj$Egg_2 <- Values
   }
  if (OM.obj$fecundity_option==2)
   {
    Values <- ExtractMG(model0$parameters,"Eggs_scalar_Fem_GP_1",0,1); OM.obj$Egg_1 <- Values
    Values <- ExtractMG(model0$parameters,"Eggs_exp_len_Fem_GP_1",0,1); OM.obj$Egg_2 <- Values
   }
  if (OM.obj$fecundity_option==3)
  {
    Values <- ExtractMG(model0$parameters,"Eggs_scalar_Fem_GP_1",0,1); OM.obj$Egg_1 <- Values
    Values <- ExtractMG(model0$parameters,"Eggs_exp_wt_Fem_GP_1",0,1); OM.obj$Egg_2 <- Values
  }
  if (OM.obj$fecundity_option==4)
   {
    Values <- ExtractMG(model0$parameters,"Eggs_intercept",0,1); OM.obj$Egg_1 <- Values
    Values <- ExtractMG(model0$parameters,"Eggs_slope",0,1); OM.obj$Egg_2 <- Values
   }
  if (OM.obj$maturity_option==4) OM.obj$age_maturity <- as.numeric(ctl$Age_Maturity)
  
  # Time-varying parameters
  Current <- model0$MGparmAdj[1,]
  TimeVarying <- NULL
  for (Iyear in 2:nyearsOrig)
   if (any(Current[-c(1,2)] != model0$MGparmAdj[Iyear,-c(1,2)]))
    {
     TimeVarying <- rbind(TimeVarying,model0$MGparmAdj[Iyear,])
     Current <- model0$MGparmAdj[Iyear,]
    }
  OM.obj$TimeVarying <- TimeVarying

  # -----------------------------------------------------------------------------------------------------------------------------------
  # Stock and recruitment
  Index <- which(rownames(model0$parameters)=="SR_LN(R0)"); Values <- exp(model0$parameters$Value[Index]); OM.obj$R0 <- Values
  Index <- which(rownames(model0$parameters)=="SR_BH_steep"); Values <- model0$parameters$Value[Index]; OM.obj$Steep <- Values
  Index <- which(rownames(model0$parameters)=="SR_sigmaR"); Values <- model0$parameters$Value[Index]; OM.obj$SigmaR <- Values
  Index <- which(rownames(model0$parameters)=="SR_autocorr"); Values <- model0$parameters$Value[Index]; OM.obj$auto <- Values

  Index <- which(model0$recruit$Yr==styr)
  devs <- rev(log(model0$recruit$pred_recr[1:Index]/model0$recruit$with_regime[1:Index]))
  Early.devs <- c(devs,rep(0,nages-length(devs)))
  OM.obj$Early.devs <- Early.devs
  Index1 <- which(substr(rownames(model0$parameters),1,13) =="SR_LN(R0)_BLK"); if (length(Index1)>0) Block <- as.numeric(substr(rownames(model0$parameters)[Index1],14,14))
  Index2 <- which(substr(rownames(model0$parameters),1,13) =="SR_regime_BLK"); if (length(Index2)>0) Block <- as.numeric(substr(rownames(model0$parameters)[Index2],14,14))
  Index <- c(Index1,Index2)
  OM.Early_R0Mult <- NULL
  if (length(Index)>0)
   {
    OM.obj$OM.Early_R0Mult <- model0$parameters$Value[Index]
    OM.obj$OM.Early_Early_R0Mult_Block_end <- c(ctl$Block_Design[[Block]][1]-styr+1,ctl$Block_Design[[Block]][2]-styr+1)
   }  
  Index <- which(model0$recruit$Yr==styr)-1
  Late.devs <- log(model0$recruit$pred_recr[Index+1:nyearsOrig]/model0$recruit$with_regime[Index+1:nyearsOrig])
  if (endyrOrig-ctl$MainRdevYrLast>0) Late.devs <- Late.devs[1:(length(Late.devs)-(endyrOrig-ctl$MainRdevYrLast))]
  OM.obj$Late.devs <- Late.devs
  OM.obj$Use_steep_init_equi <- ctl$Use_steep_init_equi
 
  # -----------------------------------------------------------------------------------------------------------------------------------
  # Maximum F
  OM.obj$maxF <- ctl$maxF
  
  # -----------------------------------------------------------------------------------------------------------------------------------
  # Selectivity and retention (size)
  OM.obj$Size_Initial.sel_fem <- NULL
  for (Ifleet in 1:nfleets)
   {
    Index <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Yr==styr & model0$sizeselex$Factor=="Lsel" & model0$sizeselex$Sex %in% c(0,1))
    SelVec <- rep(0,nlen); for (Ilen in 1:nlen) SelVec[Ilen] <- as.numeric(model0$sizeselex[Index,5+Ilen])
    OM.obj$Size_Initial.sel_fem <- rbind(OM.obj$Size_Initial.sel_fem,SelVec)
   }

  Nblocks <- rep(0,nfleets); Block.years <- matrix(0,nrow=1000,ncol=2);Iblk <- 0; BlockSels <- NULL
  for (Ifleet in 1:nfleets)
   {
    Index1 <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Factor=="Lsel" & model0$sizeselex$Sex %in% c(0,1) & model0$sizeselex$Yr==styr)
    Index2 <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Factor=="Lsel" & model0$sizeselex$Sex %in% c(0,1) & model0$sizeselex$Yr>styr)
    Vec1 <- as.numeric(model0$sizeselex[Index1,-c(1:5)])
    WithBlock <- F
    for (Ind in 1:length(Index2))
     {
      Vec2 <- as.numeric(model0$sizeselex[Index2[Ind],-c(1:5)])
      if (sum(abs(Vec1-Vec2))>0)
       {
        Iblk <- Iblk + 1
        Nblocks[Ifleet] <- Nblocks[Ifleet] + 1
        if (WithBlock==T) Block.years[Iblk-1,2] <- model0$sizeselex[Index2[Ind],3]-1
        if (WithBlock==T) if (Block.years[Iblk-1,2]==endyrOrig) Block.years[Iblk-1,2] <- endyr
        Block.years[Iblk,1] <- model0$sizeselex[Index2[Ind],3]
        if (Block.years[Iblk,1]==endyrOrig) Block.years[Iblk,1] <- endyr
        BlockSels <- rbind(BlockSels,Vec2)
        Vec1 <- Vec2
        WithBlock <- T
       }
       if (WithBlock==T) Block.years[Iblk,2] <- endyr
      } # Ind
   } # Fleet
  OM.obj$Size_Nblocks.selex_fem <- Nblocks
  OM.obj$Size_Blocks.selex_fem <- NULL
  OM.obj$Size_BlockSels.selex_fem <- NULL
  if (sum(Nblocks)>0)
  { OM.obj$Size_Blocks.selex_fem <- matrix(Block.years[1:sum(Nblocks),],ncol=2); OM.obj$Size_BlockSels.selex_fem <- BlockSels}

  if (nsex > 1)
   {
    OM.obj$Size_Initial.sel_mal <- NULL
    for (Ifleet in 1:nfleets)
     {
      Index <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Yr==styr & model0$sizeselex$Factor=="Lsel" & model0$sizeselex$Sex %in% c(2))
      SelVec <- rep(0,nlen); for (Ilen in 1:nlen) SelVec[Ilen] <- as.numeric(model0$sizeselex[Index,5+Ilen])
      OM.obj$Size_Initial.sel_mal <- rbind(OM.obj$Size_Initial.sel_mal,SelVec)
     }
    Nblocks <- rep(0,nfleets); Block.years <- matrix(0,nrow=1000,ncol=2);Iblk <- 0; BlockSels <- NULL
    for (Ifleet in 1:nfleets)
     {
      Index1 <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Factor=="Lsel" & model0$sizeselex$Sex %in% c(2) & model0$sizeselex$Yr==styr)
      Index2 <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Factor=="Lsel" & model0$sizeselex$Sex %in% c(2) & model0$sizeselex$Yr>styr)
      Vec1 <- as.numeric(model0$sizeselex[Index1,-c(1:5)])
      WithBlock <- F
      for (Ind in 1:length(Index2))
       {
        Vec2 <- as.numeric(model0$sizeselex[Index2[Ind],-c(1:5)])
        if (sum(abs(Vec1-Vec2))>0)
         {
          Iblk <- Iblk + 1
          Nblocks[Ifleet] <- Nblocks[Ifleet] + 1
          if (WithBlock==T) Block.years[Iblk-1,2] <- model0$sizeselex[Index2[Ind],3]-1
          if (WithBlock==T) if (Block.years[Iblk-1,2]==endyrOrig) Block.years[Iblk-1,2] <- endyr
          Block.years[Iblk,1] <- model0$sizeselex[Index2[Ind],3]
          if (Block.years[Iblk,1]==endyrOrig) Block.years[Iblk,1] <- endyr
          BlockSels <- rbind(BlockSels,Vec2)
          Vec1 <- Vec2
          WithBlock <- T
         }
        if (WithBlock==T) Block.years[Iblk,2] <- endyr
       } # Ind
     } # Fleet
    OM.obj$Size_Nblocks.selex_mal <- Nblocks
    OM.obj$Size_Blocks.selex_mal <- NULL
    OM.obj$Size_BlockSels.selex_mal <- NULL
    if (sum(Nblocks)>0)
     { OM.obj$Size_Blocks.selex_mal <- matrix(Block.years[1:sum(Nblocks),],ncol=2); OM.obj$Size_BlockSels.selex_mal <- BlockSels}
   }

  OM.obj$Size_Initial.ret_fem <- NULL
  Retain.Vec <- NULL
  for (Ifleet in 1:nfleets)
   {
    Index <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Yr==styr & model0$sizeselex$Factor=="Ret" & model0$sizeselex$Sex %in% c(0,1))
    if (length(Index) >=1) { SelVec <- rep(0,nlen); for (Ilen in 1:nlen) SelVec[Ilen] <- as.numeric(model0$sizeselex[Index[1],5+Ilen]) }
    if (length(Index) ==0) { SelVec <- rep(1,nlen);  }
    Retain.Vec <- rbind(Retain.Vec,SelVec)
    OM.obj$Size_Initial.ret_fem <- rbind(OM.obj$Size_Initial.ret_fem,SelVec)
   }
  
  Nblocks <- rep(0,nfleets); Block.years <- matrix(0,nrow=1000,ncol=2);Iblk <- 0; BlockSels <- NULL
  for (Ifleet in 1:nfleets)
  {
    Index2 <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Factor=="Ret" & model0$sizeselex$Sex %in% c(0,1) & model0$sizeselex$Yr>styr )
    Vec1 <- Retain.Vec[Ifleet,]
    WithBlock <- F
    if (length(Index2)>0)
      for (Ind in 1:length(Index2))
      {
        Vec2 <- as.numeric(model0$sizeselex[Index2[Ind],-c(1:5)])
        if (sum(abs(Vec1-Vec2))>0)
        {
          Iblk <- Iblk + 1
          Nblocks[Ifleet] <- Nblocks[Ifleet] + 1
          if (WithBlock==T) Block.years[Iblk-1,2] <- model0$sizeselex[Index2[Ind],3]-1
          Block.years[Iblk,1] <- model0$sizeselex[Index2[Ind],3]
          BlockSels <- rbind(BlockSels,Vec2)
          Vec1 <- Vec2
          WithBlock <- T
        }
        if (WithBlock==T) Block.years[Iblk,2] <- endyr
      } # Ind
  } # Fleet
  OM.obj$Size_Nblocks.retain_fem <- Nblocks
  OM.obj$Size_Blocks.retain_fem <- NULL
  OM.obj$Seiz_BlockSels.retain_fem <- NULL
  if (sum(Nblocks)>0)
  { OM.obj$Size_Blocks.retain_fem <- matrix(Block.years[1:sum(Nblocks),],ncol=2); OM.obj$Size_BlockSels.retain_fem <- as.matrix(BlockSels)}
  
  OM.obj$Size_Initial.mort_fem <- NULL
  Mort.Vec <- NULL
  for (Ifleet in 1:nfleets)
   {
    Index <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Yr==styr & model0$sizeselex$Factor=="Mort" & model0$sizeselex$Sex %in% c(0,1))
    if (length(Index) >=1) { SelVec <- rep(0,nlen); for (Ilen in 1:nlen) SelVec[Ilen] <- as.numeric(model0$sizeselex[Index[1],5+Ilen]) }
    if (length(Index) ==0) { SelVec <- rep(1,nlen);  }
    Mort.Vec <- rbind(Mort.Vec,SelVec)
    OM.obj$Size_Initial.mort_fem <- rbind(OM.obj$Size_Initial.mort_fem,SelVec)
   }
  
  
  if (nsex > 1)
   {
    OM.obj$Size_Initial.ret_mal <- NULL
    Retain.Vec <- NULL
    for (Ifleet in 1:nfleets)
     {
      Index <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Yr==styr & model0$sizeselex$Factor=="Ret" & model0$sizeselex$Sex %in% c(2))
      if (length(Index) >=1) { SelVec <- rep(0,nlen); for (Ilen in 1:nlen) SelVec[Ilen] <- as.numeric(model0$sizeselex[Index[1],5+Ilen]) }
      if (length(Index) ==0) { SelVec <- rep(1,nlen);  }
      Retain.Vec <- rbind(Retain.Vec,SelVec)
      OM.obj$Size_Initial.ret_mal <- rbind(OM.obj$Size_Initial.ret_mal,SelVec)
     }
  
    Nblocks <- rep(0,nfleets); Block.years <- matrix(0,nrow=1000,ncol=2);Iblk <- 0; BlockSels <- NULL
    for (Ifleet in 1:nfleets)
     {
      Index2 <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Factor=="Ret" & model0$sizeselex$Sex %in% c(2) & model0$sizeselex$Yr>styr)
      Vec1 <- Retain.Vec[Ifleet,]
      WithBlock <- F
      if (length(Index2)>0)
       for (Ind in 1:length(Index2))
        {
         Vec2 <- as.numeric(model0$sizeselex[Index2[Ind],-c(1:5)])
         if (sum(abs(Vec1-Vec2))>0)
          {
           Iblk <- Iblk + 1
           Nblocks[Ifleet] <- Nblocks[Ifleet] + 1
           if (WithBlock==T) Block.years[Iblk-1,2] <- model0$sizeselex[Index2[Ind],3]-1
           Block.years[Iblk,1] <- model0$sizeselex[Index2[Ind],3]
           BlockSels <- rbind(BlockSels,Vec2)
           Vec1 <- Vec2
           WithBlock <- T
          }
         if (WithBlock==T) Block.years[Iblk,2] <- endyr
        } # Ind
     } # Fleet
    OM.obj$Size_Nblocks.retain_mal <- Nblocks
    OM.obj$Size_Blocks.retain_mal <- NULL
    OM.obj$Seiz_BlockSels.retain_mal <- NULL
    if (sum(Nblocks)>0)
     { OM.obj$Size_Blocks.retain_mal <- matrix(Block.years[1:sum(Nblocks),],ncol=2); OM.obj$Size_BlockSels.retain_mal <- as.matrix(BlockSels)}

    OM.obj$Size_Initial.mort_mal <- NULL
    Mort.Vec <- NULL
    for (Ifleet in 1:nfleets)
     {
      Index <- which(model0$sizeselex$Fleet==Ifleet & model0$sizeselex$Yr==styr & model0$sizeselex$Factor=="Mort" & model0$sizeselex$Sex %in% c(2))
      if (length(Index) >=1) { SelVec <- rep(0,nlen); for (Ilen in 1:nlen) SelVec[Ilen] <- as.numeric(model0$sizeselex[Index[1],5+Ilen]) }
      if (length(Index) ==0) { SelVec <- rep(1,nlen);  }
      Mort.Vec <- rbind(Mort.Vec,SelVec)
      OM.obj$Size_Initial.mort_mal <- rbind(OM.obj$Size_Initial.mort_mal,SelVec)
     }
  } # nsex > 1
    
  # -----------------------------------------------------------------------------------------------------------------
  
  # Selectivity and retention (Age)
  OM.obj$Age_Initial.sel_fem <- NULL
  for (Ifleet in 1:nfleets)
   {
    Index <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Yr==styr & model0$ageselex$Factor=="Asel" & model0$ageselex$Sex %in% c(0,1))
    SelVec <- rep(0,nages); for (Iage in 1:nages) SelVec[Iage] <- as.numeric(model0$ageselex[Index,7+Iage])
    OM.obj$Age_Initial.sel_fem <- rbind(OM.obj$Age_Initial.sel_fem,SelVec)
   }
  
  Nblocks <- rep(0,nfleets); Block.years <- matrix(0,nrow=1000,ncol=2);Iblk <- 0; BlockSels <- NULL
  for (Ifleet in 1:nfleets)
   {
    Index1 <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Factor=="Asel" & model0$ageselex$Sex %in% c(0,1) & model0$ageselex$Yr==styr)
    Index2 <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Factor=="Asel" & model0$ageselex$Sex %in% c(0,1) & model0$ageselex$Yr>styr)
    Vec1 <- as.numeric(model0$ageselex[Index1,-c(1:7)])
    WithBlock <- F
    for (Ind in 1:length(Index2))
    {
      Vec2 <- as.numeric(model0$ageselex[Index2[Ind],-c(1:7)])
      if (sum(abs(Vec1-Vec2))>0)
      {
        Iblk <- Iblk + 1
        Nblocks[Ifleet] <- Nblocks[Ifleet] + 1
        if (WithBlock==T) Block.years[Iblk-1,2] <- model0$ageselex[Index2[Ind],3]-1
        Block.years[Iblk,1] <- model0$ageselex[Index2[Ind],3]
        BlockSels <- rbind(BlockSels,Vec2)
        Vec1 <- Vec2
        WithBlock <- T
      }
      if (WithBlock==T) Block.years[Iblk,2] <- endyr
    } # Ind
  } # Fleet
  OM.obj$Age_Nblocks.selex_fem <- Nblocks
  OM.obj$Age_Blocks.selex_fem <- NULL
  OM.obj$Age_BlockSels.selex_fem <- NULL
  if (sum(Nblocks)>0)
  { OM.obj$Age_Blocks.selex_fem <- matrix(Block.years[1:sum(Nblocks),],ncol=2); OM.obj$Age_BlockSels.selex_fem <- BlockSels}
  
  if (nsex > 1)
   {
    OM.obj$Age_Initial.sel_mal <- NULL
    for (Ifleet in 1:nfleets)  
     {
      Index <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Yr==styr & model0$ageselex$Factor=="Asel" & model0$ageselex$Sex %in% c(0,1))
      SelVec <- rep(0,nages); for (Iage in 1:nages) SelVec[Iage] <- as.numeric(model0$ageselex[Index,7+Iage])
      OM.obj$Age_Initial.sel_mal <- rbind(OM.obj$Age_Initial.sel_mal,SelVec)
     }
  
    Nblocks <- rep(0,nfleets); Block.years <- matrix(0,nrow=1000,ncol=2);Iblk <- 0; BlockSels <- NULL
    for (Ifleet in 1:nfleets)
     {
      Index1 <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Factor=="Asel" & model0$ageselex$Sex %in% c(2) & model0$ageselex$Yr==styr)
      Index2 <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Factor=="Asel" & model0$ageselex$Sex %in% c(2) & model0$ageselex$Yr>styr)
      Vec1 <- as.numeric(model0$ageselex[Index1,-c(1:7)])
      WithBlock <- F
      for (Ind in 1:length(Index2))
       {
        Vec2 <- as.numeric(model0$ageselex[Index2[Ind],-c(1:7)])
        if (sum(abs(Vec1-Vec2))>0)
         {
          Iblk <- Iblk + 1
          Nblocks[Ifleet] <- Nblocks[Ifleet] + 1
          if (WithBlock==T) Block.years[Iblk-1,2] <- model0$ageselex[Index2[Ind],3]-1
          Block.years[Iblk,1] <- model0$ageselex[Index2[Ind],3]
          BlockSels <- rbind(BlockSels,Vec2)
          Vec1 <- Vec2
          WithBlock <- T
         }
        if (WithBlock==T) Block.years[Iblk,2] <- endyr
       } # Ind
     } # Fleet
    OM.obj$Age_Nblocks.selex_mal <- Nblocks
    OM.obj$Age_Blocks.selex_mal <- NULL
    OM.obj$Age_BlockSels.selex_mal <- NULL
    if (sum(Nblocks)>0)
     { OM.obj$Age_Blocks.selex_mal <- matrix(Block.years[1:sum(Nblocks),],ncol=2); OM.obj$Age_BlockSels.selex_mal <- BlockSels}
   } # If nsex > 1
    
  OM.obj$Age_Initial.ret_fem <- NULL
  Retain.Vec <- NULL
  for (Ifleet in 1:nfleets)
  {
    Index <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Yr==styr & model0$ageselex$Factor=="Aret" & model0$ageselex$Sex %in% c(0,1))
    if (length(Index) >=1) { SelVec <- rep(0,nages); for (Iage in 1:nages) SelVec[Iage] <- as.numeric(model0$ageselex[Index[1],7+Iage]) }
    if (length(Index) ==0) { SelVec <- rep(1,nages);  }
    Retain.Vec <- rbind(Retain.Vec,SelVec)
    OM.obj$Age_Initial.ret_fem <- rbind(OM.obj$Age_Initial.ret_fem,SelVec)
   }
  
  Nblocks <- rep(0,nfleets); Block.years <- matrix(0,nrow=1000,ncol=2);Iblk <- 0; BlockSels <- NULL
  for (Ifleet in 1:nfleets)
  {
    Index2 <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Factor=="Aret" & model0$ageselex$Sex %in% c(0,1) & model0$ageselex$Yr>styr)
    Vec1 <- Retain.Vec[Ifleet,]
    WithBlock <- F
    if (length(Index2)>0)
      for (Ind in 1:length(Index2))
      {
        Vec2 <- as.numeric(model0$ageselex[Index2[Ind],-c(1:7)])
        if (sum(abs(Vec1-Vec2))>0)
        {
          Iblk <- Iblk + 1
          Nblocks[Ifleet] <- Nblocks[Ifleet] + 1
          if (WithBlock==T) Block.years[Iblk-1,2] <- model0$ageselex[Index2[Ind],3]-1
          Block.years[Iblk,1] <- model0$ageselex[Index2[Ind],3]
          BlockSels <- rbind(BlockSels,Vec2)
          Vec1 <- Vec2
          WithBlock <- T
        }
        if (WithBlock==T) Block.years[Iblk,2] <- endyr
      } # Ind
  } # Fleet
  OM.obj$Age_Nblocks.retain_fem <- Nblocks
  OM.obj$Age_Blocks.retain_fem <- NULL
  OM.obj$Seiz_BlockSels.retain_fem <- NULL
  if (sum(Nblocks)>0)
  { OM.obj$Age_Blocks.retain_fem <- matrix(Block.years[1:sum(Nblocks),],ncol=2); OM.obj$Age_BlockSels.retain_fem <- as.matrix(BlockSels)}
  
  OM.obj$Age_Initial.mort_fem <- NULL
  Mort.Vec <- NULL
  for (Ifleet in 1:nfleets)
   {
    Index <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Yr==styr & model0$ageselex$Factor=="Amort"& model0$ageselex$Sex %in% c(0,1))
    if (length(Index) >=1) { SelVec <- rep(0,nages); for (Iage in 1:nages) SelVec[Iage] <- as.numeric(model0$ageselex[Index[1],7+Iage]) }
    if (length(Index) ==0) { SelVec <- rep(1,nages);  }
    Mort.Vec <- rbind(Mort.Vec,SelVec)
    OM.obj$Age_Initial.mort_fem <- rbind(OM.obj$Age_Initial.mort_fem,SelVec)
   }
  
  if (nsex > 1)
   {
    OM.obj$Age_Initial.ret_mal <- NULL
    Retain.Vec <- NULL
    for (Ifleet in 1:nfleets)
     {
      Index <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Yr==styr & model0$ageselex$Factor=="Aret"& model0$ageselex$Sex %in% c(2))
      if (length(Index) >=1) { SelVec <- rep(0,nages); for (Iage in 1:nages) SelVec[Iage] <- as.numeric(model0$ageselex[Index[1],7+Iage]) }
      if (length(Index) ==0) { SelVec <- rep(1,nages);  }
      Retain.Vec <- rbind(Retain.Vec,SelVec)
      OM.obj$Age_Initial.ret_mal <- rbind(OM.obj$Age_Initial.ret_mal,SelVec)
     }

    Nblocks <- rep(0,nfleets); Block.years <- matrix(0,nrow=1000,ncol=2);Iblk <- 0; BlockSels <- NULL
    for (Ifleet in 1:nfleets)
     {
      Index2 <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Factor=="Aret" & model0$ageselex$Sex %in% c(2) & model0$ageselex$Yr>styr)
      Vec1 <- Retain.Vec[Ifleet,]
      WithBlock <- F
      if (length(Index2)>0)
       for (Ind in 1:length(Index2))
        {
         Vec2 <- as.numeric(model0$ageselex[Index2[Ind],-c(1:7)])
         if (sum(abs(Vec1-Vec2))>0)
          {
           Iblk <- Iblk + 1
           Nblocks[Ifleet] <- Nblocks[Ifleet] + 1
           if (WithBlock==T) Block.years[Iblk-1,2] <- model0$ageselex[Index2[Ind],3]-1
           Block.years[Iblk,1] <- model0$ageselex[Index2[Ind],3]
           BlockSels <- rbind(BlockSels,Vec2)
           Vec1 <- Vec2
           WithBlock <- T
          }
         if (WithBlock==T) Block.years[Iblk,2] <- endyr
        } # Ind
     } # Fleet
    OM.obj$Age_Nblocks.retain_mal <- Nblocks
    OM.obj$Age_Blocks.retain_mal <- NULL
    OM.obj$Seiz_BlockSels.retain_mal <- NULL
    if (sum(Nblocks)>0)
     { OM.obj$Age_Blocks.retain_mal <- matrix(Block.years[1:sum(Nblocks),],ncol=2); OM.obj$Age_BlockSels.retain_mal <- as.matrix(BlockSels)}
  
    OM.obj$Age_Initial.mort_mal <- NULL
    Mort.Vec <- NULL
    for (Ifleet in 1:nfleets)
     {
      Index <- which(model0$ageselex$Fleet==Ifleet & model0$ageselex$Yr==styr & model0$ageselex$Factor=="Amort"& model0$ageselex$Sex %in% c(2))
      if (length(Index) >=1) { SelVec <- rep(0,nages); for (Iage in 1:nages) SelVec[Iage] <- as.numeric(model0$ageselex[Index[1],7+Iage]) }
      if (length(Index) ==0) { SelVec <- rep(1,nages);  }
      Mort.Vec <- rbind(Mort.Vec,SelVec)
      OM.obj$Age_Initial.mort_mal <- rbind(OM.obj$Age_Initial.mort_mal,SelVec)
     }
  } # nsex > 1
    
  # -----------------------------------------------------------------------------------------------------------------------------------

  # Fishing mortality rates
  OM.obj$Age_Initial.Fs <- NULL
  Index <- which(substr(model0$parameters$Label,1,5)=="InitF")
  TheFleets <- as.numeric(substr(model0$parameters$Label[Index],12,12))
  OM.obj$Initial_F <- rep(0,nfleets)
  OM.obj$Initial_F[TheFleets] <- as.numeric(model0$parameters$Value[Index])

  # -----------------------------------------------------------------------------------------------------------------------------------

  N.env.link <- 0
  Link.vars <- NULL
  Index <- which(rownames(ctl$MG_parms)=="NatM_p_1_Fem_GP_1")
  if (length(Index)==0) print("M is not estimated or set for this model")
  if (length(Index)>0)
   if (ctl$MG_parms$'env_var&link'[Index]!=0)
    {
     N.env.link <- N.env.link + 1
     Env.var <- ctl$MG_parms$'env_var&link'[Index] %% 100
     Index <- which(model0$parameters$Label=="NatM_uniform_Fem_GP_1_ENV_mult");  M2 <- model0$parameters$Value[Index]
     Index <- which(model0$parameters$Label=="NatM_uniform_Fem_GP_1"); M1 <- model0$parameters$Value[Index]
     Par.Val <- log(M2/M1)
     Par.Val <- M2
     LinkLine <- c("M ",Env.var,Par.Val)
     Link.vars <- rbind(Link.vars,LinkLine)
    }
  
  Index <- which(rownames(ctl$SR_parms)=="SR_regime")
  if (ctl$SR_parms$'env_var&link'[Index]!=0)
   {
    N.env.link <- N.env.link + 1
    Env.var <- ctl$SR_parms$'env_var&link'[Index] %% 100
    Index <- which(model0$parameters$Label=="SR_regime_ENV_add");  P2 <- model0$parameters$Value[Index]
    Par.Val <- P2
    LinkLine <- c("Regime",Env.var,Par.Val)
    Link.vars <- rbind(Link.vars,LinkLine)
   }

  Index <- which(rownames(ctl$SR_parms)=="SR_LN(R0)")
  if (ctl$SR_parms$'env_var&link'[Index]!=0)
   {
    N.env.link <- N.env.link + 1
    Env.var <- ctl$SR_parms$'env_var&link'[Index] %% 100
    Index <- which(model0$parameters$Label=="SR_LN(R0)_ENV_add");  P2 <- model0$parameters$Value[Index]
    Par.Val <- P2
    LinkLine <- c("Ln_R0",Env.var,Par.Val)
    Link.vars <- rbind(Link.vars,LinkLine)
   }

  OM.obj$N.env.link <- N.env.link
  OM.obj$Link.vars <- Link.vars
  OM.obj$Env.vars <- 0
  OM.obj$Env.override <- 0
  
  OM.obj$project.type <- Project.Type
  Catch.data <- rep(0,nfleet.catch)
  for (Ifleet in 1:nfleet.catch)
   {
    Index <- which(dat$catch$fleet==Ifleet & dat$catch$year < 0)
    if (length(Index)>0)
    {  CC <- dat$catch$catch[Index];Catch.data[Ifleet] <- CC; }
   }
  OM.obj$eqn.catch <- Catch.data

  Catch.data <- matrix(0,ncol=nfleet.catch,nrow=nyearsOrig)
  for (Ifleet in 1:nfleet.catch)
   {
    Index <- which(dat$catch$fleet==Ifleet & dat$catch$year %in% c(styr:endyrOrig))
    CC <- dat$catch$catch[Index]
    Ind <- dat$catch$year[Index]-styr+1
    Catch.data[Ind,Ifleet] <- dat$catch$catch[Index]
   }
  OM.obj$hist.catch <- cbind(c(styr:endyrOrig),Catch.data)
  if (NextraCat>0)
   for (Iyear in 1:NextraCat)
    { Vec <- c(as.numeric(CatYears[Iyear]), as.numeric(CatFuture[Iyear,1:nfleet.catch])); OM.obj$hist.catch <- rbind(OM.obj$hist.catch,Vec)  }  

  Fleet.F.data <- matrix(0,ncol=nfleet.catch,nrow=nyears)
  for (Ifleet in 1:nfleet.catch)
   {
    Index1 <- which(colnames(model0$exploitation)==OM.obj$fleetnames[Ifleet])
    if (length(Index1)==1)
     {
      Index2 <- which(model0$exploitation$Yr %in% c(styr:endyrOrig))
      CC <- model0$exploitation[Index2,Index1]
      Ind <- model0$exploitation$Yr[Index2]-styr+1
      Fleet.F.data[Ind,Ifleet] <- CC
     }
   }
  OM.obj$Fleet.F.data <- Fleet.F.data

  # Process the CPUE data
  index.Use <- rep("No",nfleets)
  index.CV.past <- rep(0,nfleets)
  index.CV.fut <- rep(0,nfleets)
  index.q <- rep(1,nfleets)
  index.freq <- rep(0,nfleets)
  index.tab <- matrix(-1,ncol=nfleets,nrow=nyears+Default.Proj)
  
  for (Ifleet in 1:nfleets)
  {
   Index <- which(dat$CPUE$index==Ifleet)
   if (length(Index)>0)
    {
     index.Use[Ifleet] <- "Yes"
     CV <- median(dat$CPUE$se_log[Index])
     index.CV.past[Ifleet] <- CV
     index.CV.fut[Ifleet] <- CV
     #if (dat$CPUE$year[Index[length(Index)]] %in% seq(from=endyr-YearAdjust,to=endyr)) index.freq[Ifleet] <- 1
     dat.year <- dat$CPUE$year[Index] - styr+1
     dat.CV <- dat$CPUE$se_log[Index]/CV
     index.tab[dat.year,Ifleet] <- dat.CV
     if (YearAdjust>0)
      for (Kyear in 0:(YearAdjust-1))
       if (index.tab[endyrOrig-styr+1-Kyear,Ifleet]<=0)  index.tab[endyrOrig-styr+1-Kyear,Ifleet] <- index.tab[endyrOrig-styr+1-YearAdjust,Ifleet]
     TheYears <- which(dat$CPUE$year[Index] %in% seq(from=endyrOrig-YearAdjust-9,to=endyrOrig-YearAdjust))
     if (length(TheYears)>0) index.freq[Ifleet] <- round(10/length(TheYears))
     if (NextraCat >0 & index.freq[Ifleet] > 0)
      for (Iyear in (nyearsOrig+1):nyears)
       if ((Iyear-nyearsOrig-1) %% index.freq[Ifleet] == 0) index.tab[Iyear,Ifleet] <- 1
     if (index.freq[Ifleet] > 0 & Default.Proj > 0)
      for (Iyear in (nyears+1):(nyears+Default.Proj))
       if (Iyear %% index.freq[Ifleet] == 0) index.tab[Iyear,Ifleet] <- 1
   }
  }

  OM.obj$index.Use <- index.Use
  OM.obj$index.CV.past <- index.CV.past
  OM.obj$index.CV.fut <- index.CV.fut
  OM.obj$index.freq <- index.freq
  OM.obj$index.q <- index.q
  OM.obj$index.tab <- cbind(c(styr:(endyr+Default.Proj)),index.tab)

  # Process the discard data
  discard.Use <- rep("No",nfleet.catch)
  discard.type <- rep(1,nfleet.catch)
  discard.CV.past <- rep(0,nfleet.catch)
  discard.CV.fut <- rep(0,nfleet.catch)
  discard.freq <- rep(-1,nfleet.catch)
  discard.tab <- matrix(-1,ncol=nfleet.catch,nrow=nyears+Default.Proj)
  
  for (Ifleet in 1:nfleet.catch)
   {
    Index <- which(dat$discard_data$fleet==Ifleet)
    if (length(Index)>0)
     {
      Index2 <- which(dat$discard_fleet_info$fleet==Ifleet); discard.type[Ifleet] <- dat$discard_fleet_info$units[Index2]
      discard.Use[Ifleet] <- "Yes"
      CV <- median(dat$discard_data$stderr[Index])
      discard.CV.past[Ifleet] <- CV
      discard.CV.fut[Ifleet] <- CV
      #if (dat$discard_data$year[Index[length(Index)]]%in% seq(from=endyr-YearAdjust,to=endyr)) discard.freq[Ifleet] <- 1
      dat.year <- dat$discard_data$year[Index] - styr+1
      dat.CV <- dat$discard_data$stderr[Index]/CV
      discard.tab[dat.year,Ifleet] <- dat.CV
      if (YearAdjust>0)
        for (Kyear in 0:(YearAdjust-1))
          if (discard.tab[endyr-styr+1-Kyear,Ifleet]<=0)  discard.tab[endyr-styr+1-Kyear,Ifleet] <- discard.tab[endyr-styr+1-YearAdjust,Ifleet]
      TheYears <- which(dat$discard_data$year[Index] %in% seq(from=endyrOrig-YearAdjust-9,to=endyrOrig-YearAdjust))
      if (length(TheYears)>0) discard.freq[Ifleet] <- round(10/length(TheYears))
      if (NextraCat > 0 & discard.freq[Ifleet] > 0)
        for (Iyear in (nyearsOrig+1):nyears)
          if ((Iyear-nyearsOrig-1) %% discard.freq[Ifleet] == 0) discard.tab[Iyear,Ifleet] <- 1
      if (discard.freq[Ifleet] > 0 & Default.Proj > 0)
       for (Iyear in (nyears+1):(nyears+Default.Proj))
        if (Iyear %% discard.freq[Ifleet] == 0) discard.tab[Iyear,Ifleet] <- 1
    }
   }
  
  OM.obj$discard.Use <- discard.Use
  OM.obj$discard.type <- discard.type
  OM.obj$discard.CV.past <- discard.CV.past
  OM.obj$discard.CV.fut <- discard.CV.fut
  OM.obj$discard.freq <- discard.freq
  OM.obj$discard.tab <- cbind(c(styr:(endyr+Default.Proj)),discard.tab)   

  # Process the length data
  cat("Processing length data\n")
  length.Use <- rep("No",nfleets)
  length.ESS.past <- matrix(0,nrow=3,ncol=nfleets)
  length.ESS.fut <- matrix(0,nrow=3,ncol=nfleets)
  length.freq <-  matrix(0,nrow=3,ncol=nfleets)
  length.tab <- array(-1,dim=c(3,nyears+Default.Proj,nfleets))
  Index <- which(dat$lencomp$year >= styr)
  dat$lencomp <- dat$lencomp[Index,]
  parts <- c(2,1,0)
  for (Ifleet in 1:nfleets)
  { Index <- which(dat$lencomp$fleet==Ifleet); if (length(Index)>0) length.Use[Ifleet] <- "Yes"; }
  for (Ipart in 1:3)
   for (Ifleet in 1:nfleets)
    {
     Index <- which(dat$lencomp$fleet==Ifleet & dat$lencomp$part==parts[Ipart])
     if (length(Index)>0)
      {
       ESS <- median(dat$lencomp$Nsamp[Index])
       length.ESS.past[Ipart,Ifleet] <- ESS
       length.ESS.fut[Ipart,Ifleet] <- ESS
       #if (dat$lencomp$year[Index[length(Index)]]%in% seq(from=endyr-YearAdjust,to=endyr)) length.freq[Ipart,Ifleet] <- 1
       dat.year <- dat$lencomp$year[Index] - styr+1
       dat.ESS <- dat$lencomp$Nsamp[Index]/ ESS
       length.tab[Ipart,dat.year,Ifleet] <- dat.ESS
       if (YearAdjust>0)
         for (Kyear in 0:(YearAdjust-1))
           if (length.tab[Ipart,endyr-styr+1-Kyear,Ifleet]<=0)  length.tab[Ipart,endyr-styr+1-Kyear,Ifleet] <- length.tab[Ipart,endyr-styr+1-YearAdjust,Ifleet]
       TheYears <- which(dat$lencomp$year[Index] %in% seq(from=endyrOrig-YearAdjust-9,to=endyrOrig-YearAdjust))
       if (length(TheYears)>0) length.freq[Ipart,Ifleet] <- round(10/length(TheYears))
       if (NextraCat >0 & length.freq[Ipart,Ifleet] > 0)
        for (Iyear in (nyearsOrig+1):nyears)
         if ((Iyear-nyearsOrig-1) %% length.freq[Ipart,Ifleet] == 0) length.tab[Ipart,Iyear,Ifleet] <- 1
       if (length.freq[Ipart,Ifleet] > 0 & Default.Proj > 0)
        for (Iyear in (nyears+1):(nyears+Default.Proj))
         if (Iyear %% length.freq[Ipart,Ifleet] == 0) length.tab[Ipart,Iyear,Ifleet] <- 1
     }
    }
  OM.obj$length.Use <- length.Use
  OM.obj$length.ESS.past <- length.ESS.past
  OM.obj$length.ESS.fut <- length.ESS.fut
  OM.obj$length.freq <- length.freq
  OM.obj$length.tab <- length.tab
  
  # Process the age data
  cat("Processing marginal age compostion data\n")
  age.Use <- rep("No",nfleets)
  age.ESS.past <- matrix(0,nrow=3,ncol=nfleets)
  age.ESS.fut <- matrix(0,nrow=3,ncol=nfleets)
  age.freq <-  matrix(0,nrow=3,ncol=nfleets)
  age.tab <- array(-1,dim=c(3,nyears+Default.Proj,nfleets))
  
  if (AgeAdjust==T) dat$agecomp$fleet <- -1*dat$agecomp$fleet
  Index <- which(dat$agecomp$Lbin_lo==-1)
  ages.for.gen <- dat$agecomp[Index,]
 
  parts <- c(2,1,0)
  for (Ifleet in 1:nfleets)
  { Index <- which(ages.for.gen$fleet==Ifleet); if (length(Index)>0) age.Use[Ifleet] <- "Yes"; }
  for (Ipart in 1:3)
    for (Ifleet in 1:nfleets)
    {
      Index <- which(ages.for.gen$fleet==Ifleet & ages.for.gen$part==parts[Ipart])
     if (length(Index)>0)
      {
        ESS <- median(ages.for.gen$Nsamp[Index])
        age.ESS.past[Ipart,Ifleet] <- ESS
        age.ESS.fut[Ipart,Ifleet] <- ESS
        dat.year <- ages.for.gen$year[Index] - styr+1
        dat.ESS <- ages.for.gen$Nsamp[Index]/ ESS
        age.tab[Ipart,dat.year,Ifleet] <- dat.ESS
        if (YearAdjust>0)
          for (Kyear in 0:(YearAdjust-1))
            if (age.tab[Ipart,endyr-styr+1-Kyear,Ifleet]<=0)  age.tab[Ipart,endyr-styr+1-Kyear,Ifleet] <- age.tab[Ipart,endyr-styr+1-YearAdjust,Ifleet]
        TheYears <- which(ages.for.gen$year[Index] %in% seq(from=endyrOrig-YearAdjust-9,to=endyrOrig-YearAdjust))
        if (length(TheYears)>0) age.freq[Ipart,Ifleet] <- round(10/length(TheYears))
        if (NextraCat >0 & age.freq[Ipart,Ifleet] > 0)
          for (Iyear in (nyearsOrig+1):nyears)
            if ((Iyear-nyearsOrig-1) %% age.freq[Ipart,Ifleet] == 0) age.tab[Ipart,Iyear,Ifleet] <- 1
        if (age.freq[Ipart,Ifleet] > 0 & Default.Proj > 0)
          for (Iyear in (nyears+1):(nyears+Default.Proj))
            if (Iyear %% age.freq[Ipart,Ifleet] == 0) age.tab[Ipart,Iyear,Ifleet] <- 1
     }
    }
  OM.obj$age.Use <- age.Use
  OM.obj$age.ESS.past <- age.ESS.past
  OM.obj$age.ESS.fut <- age.ESS.fut
  OM.obj$age.freq <- age.freq
  OM.obj$age.tab <- age.tab
  
  # Process the conditional age-at-length data
  cat("Processing conditional age-at-length data","\n")
  caa.Use <- rep("No",nfleets)
  caa.ESS.past <- matrix(0,nrow=3,ncol=nfleets)
  caa.ESS.fut <- matrix(0,nrow=3,ncol=nfleets)
  caa.freq <-  matrix(0,nrow=3,ncol=nfleets)
  caa.tab <- array(-1,dim=c(3,nyears+Default.Proj,nfleets))
  Index <- which(dat$agecomp$Lbin_lo!=-1)
  ages.for.gen <- dat$agecomp[Index,]
 
  # Combine the actual CAA data over length-classes
  ages.combined <- NULL
  parts <- c(2,1,0)
  for (Ipart in 1:3)
    for (Ifleet in 1:nfleets)
      for (year in styr:endyr)
       {
        if (nsex==1 & Ipart==1) Ipars <- c(Ipart,0)
        Index <- which(ages.for.gen$fleet==Ifleet & ages.for.gen$part %in% parts[Ipart] & ages.for.gen$year==year) 
        if (length(Index)>0)
         {
          TotSS <- sum(ages.for.gen$Nsamp[Index])
          Age.vec <- ages.for.gen[Index[1],]
          Age.vec$Nsamp <- TotSS
          ages.combined <- rbind(ages.combined,Age.vec)
         }
       } # fleets,ages and years
  ages.for.gen <- ages.combined

  parts <- c(2,1,0)
  for (Ifleet in 1:nfleets)
  { Index <- which(ages.for.gen$fleet==Ifleet); if (length(Index)>0) caa.Use[Ifleet] <- "Yes"; }
  for (Ipart in 1:3)
    for (Ifleet in 1:nfleets)
    {
      Index <- which(ages.for.gen$fleet==Ifleet & ages.for.gen$part==parts[Ipart])
      if (length(Index)>0)
      {
        ESS <- median(ages.for.gen$Nsamp[Index])
        caa.ESS.past[Ipart,Ifleet] <- ESS
        caa.ESS.fut[Ipart,Ifleet] <- ESS
        #if (ages.for.gen$year[Index[length(Index)]]%in% seq(from=endyr-YearAdjust,to=endyr)) caa.freq[Ipart,Ifleet] <- 1
        dat.year <- ages.for.gen$year[Index] - styr+1
        dat.ESS <- ages.for.gen$Nsamp[Index]/ ESS
        caa.tab[Ipart,dat.year,Ifleet] <- dat.ESS
        if (YearAdjust>0)
          for (Kyear in 0:(YearAdjust-1))
            if (caa.tab[Ipart,endyr-styr+1-Kyear,Ifleet]<=0)  caa.tab[Ipart,endyr-styr+1-Kyear,Ifleet] <- caa.tab[Ipart,endyr-styr+1-YearAdjust,Ifleet]
        TheYears <- which(ages.for.gen$year[Index] %in% seq(from=endyrOrig-YearAdjust-9,to=endyrOrig-YearAdjust))
        if (length(TheYears)>0) caa.freq[Ipart,Ifleet] <- round(10/length(TheYears))
        if (NextraCat >0 & caa.freq[Ipart,Ifleet] > 0)
          for (Iyear in (nyearsOrig+1):nyears)
            if ((Iyear-nyearsOrig-1) %% caa.freq[Ipart,Ifleet] == 0) caa.tab[Ipart,Iyear,Ifleet] <- 1
        if (caa.freq[Ipart,Ifleet] > 0 & Default.Proj > 0)
          for (Iyear in (nyears+1):(nyears+Default.Proj))
            if (Iyear %% caa.freq[Ipart,Ifleet] == 0) caa.tab[Ipart,Iyear,Ifleet] <- 1
      }
    }
  OM.obj$caa.Use <- caa.Use
  OM.obj$caa.ESS.past <- caa.ESS.past
  OM.obj$caa.ESS.fut <- caa.ESS.fut
  OM.obj$caa.freq <- caa.freq
  OM.obj$caa.tab <- caa.tab
  
  # Process the age-reading error
  OM.obj$N_ageerror_definitions <- dat$N_ageerror_definitions
  if (is.null(OM.obj$N_ageerror_definitions)) OM.obj$N_ageerror_definitions <- 0
  OM.obj$age_error_mean <- model0$age_error_mean
  OM.obj$age_error_sd <- model0$age_error_sd
  
  # Environmental index data
  OM.obj$EnvIndex.Num <- dat$N_environ_variables
  OM.obj$env.data <- array(0,dim=c(OM.obj$EnvIndex.Num,endyr-styr+100,3))
  if (dat$N_environ_variables > 0)
  for (Ienv.index in 1:dat$N_environ_variables)
   {
    env.data <- matrix(0,nrow=endyr-styr+100,ncol=3)
    env.data[,2] <- Ienv.index
    env.data[,1] <- 1:(endyr-styr+100)+styr-1
    Index <- which(dat$envdat[,2]==Ienv.index & dat$envdat[,1]>=styr)
    if (length(Index)>0)
     {
      env.sub <- dat$envdat[Index,]
      env.yrs <- env.sub[,1]-styr+1
      env.data[env.yrs,3] <- env.sub[,3]
     }
    OM.obj$env.data[Ienv.index,,] <- env.data
   }  

  Cata.mort <- matrix(0,nrow=endyr-styr+100,3)
  Cata.mort[,1] <- seq(from=styr,to=endyr+99)
  Cata.mort[,2] <- rep(0,endyr-styr+100)
  Cata.mort[,3] <- rep(1,endyr-styr+100)
  OM.obj$Cata.mort <- Cata.mort
  
  write.out.OM(OM.obj,RunFolder2,Control.Rule.Type)
  cat("SAVING",RunFolder2a,"\n")
  save(OM.obj,file=RunFolder2a)
 
  # ===========================================================================================================================================================
  # ===========================================================================================================================================================
  
  write("# Generated EM file",RunFolder3)
  write("#Ass_Basic:Type Tier_0",RunFolder3,append=T)     # here for testing
  write("##Ass_Basic:Type Tier_1a",RunFolder3,append=T)
  write("##Ass_Basic:Type Tier_4a",RunFolder3,append=T)
  write("",RunFolder3,append=T)     
  write("#Ass_Basic:Clean.up Default                  # None Default Full",RunFolder3,append=T)
  write("#Ass_Basic.Estimate Yes                      # Conduct an assessment: Yes / No",RunFolder3,append=T)
  write("#Ass_Basic.Use.SS3.par No                    # Started with the par file: Yes / No",RunFolder3,append=T)
  write("",RunFolder3,append=T)     
  
  write("##SS_est_opt Full                             # EstOnly First.Full.Only Full",RunFolder3,append=T) 
  write("#SS_est_opt EstOnly                           # EstOnly First.Full.Only Full",RunFolder3,append=T) 
  write("#SS_pred_opt None                             # None, Opt_5 Opt_3 Env_predators",RunFolder3,append=T) 

  MG_parms.labs <- rownames(ctl$MG_parms)
  SR_parms.labs <- rownames(ctl$SR_parms)

  # -------------------------------------------------------------------------------------------------------------------
  
  EM.obj$Nblocks <- 2
  EM.obj$nblocks.per.block <- c(1,1)
  EM.obj$block.years <- c(styr,endyr,rep(0,24*2))
  EM.obj$block.years <- rbind(EM.obj$block.years,c(styr-1,styr-1,rep(0,24*2)))

  # Parameter offset
  EM.obj$param.offset <- ctl$parameter_offset_approach

  # -------------------------------------------------------------------------------------------------------------------
  
  EM.obj$M.phase <- ctl$MG_parms$PHASE[which(substr(MG_parms.labs,1,4)=="NatM")][1:nsex]
  EM.obj$Lamin.phase <-ctl$MG_parms$PHASE[which(substr(MG_parms.labs,1,9)=="L_at_Amin")][1:nsex]
  EM.obj$Lamax.phase <-ctl$MG_parms$PHASE[which(substr(MG_parms.labs,1,9)=="L_at_Amax")][1:nsex]
  EM.obj$Kappa.phase <-ctl$MG_parms$PHASE[which(substr(MG_parms.labs,1,9)=="VonBert_K")][1:nsex]
  if (OM.obj$Growth_model==2)
    EM.obj$Richards.phase <-ctl$MG_parms$PHASE[which(substr(MG_parms.labs,1,8)=="Richards")][1:nsex]
  EM.obj$CV.young.phase <-ctl$MG_parms$PHASE[which(substr(MG_parms.labs,1,8)=="CV_young")][1:nsex]
  EM.obj$CV.old.phase <-ctl$MG_parms$PHASE[which(substr(MG_parms.labs,1,6)=="CV_old")][1:nsex]
  EM.obj$first_mat_age <-ctl$First_Mature_Age
  N_MG_Pars <- length(ctl$MG_parms[,1])
  
  IndexA <- which(substr(model0$parameters$Label,1,3)=="SR_")
  EM.obj$MG_blocks <- ctl$MG_parms$Block[1:N_MG_Pars]
  EM.obj$MG_block_fn <- ctl$MG_parms$Block_Fxn[1:N_MG_Pars]
  EM.obj$MG_env.dev <- ctl$MG_parms$'env_var&link'[1:N_MG_Pars]
  MG.dev.use <- 0
  MG.dev.vals <- NULL
  MG.dev.phs <- NULL
  Ipnt <- N_MG_Pars
  for (Ipar in 1:N_MG_Pars) 
   {
    if ( EM.obj$MG_env.dev[Ipar]!=0)
     {
      Ipnt <- Ipnt + 1
      MG.dev.use <- MG.dev.use + 1; 
      MG.dev.vals <- c(MG.dev.vals,model0$parameters$Value[Ipnt])
      MG.dev.phs <- c(MG.dev.phs,model0$parameters$Phase[Ipnt])
     }  
    if ( EM.obj$MG_blocks[Ipar]!=0)
     {
      Iblock <- EM.obj$MG_blocks[Ipar]
      EM.obj$Nblocks <- EM.obj$Nblocks + 1
      EM.obj$nblocks.per.block <- c(EM.obj$nblocks.per.block,as.numeric(ctl$blocks_per_pattern[Iblock]))
      BlkYears <- ctl$Block_Design[[Iblock]]
      BlkYears <- as.numeric(c(BlkYears,rep(0,50-length(BlkYears))))
      Index <- which(BlkYears==endyrOrig); BlkYears[Index] <- endyr  
      EM.obj$block.years <- rbind(EM.obj$block.years,BlkYears)
      EM.obj$MG_blocks[Ipar] <- EM.obj$Nblocks
      for (Iblk in 1:ctl$blocks_per_pattern[Iblock])
       { 
        Ipnt <- Ipnt + 1
        MG.dev.use <- MG.dev.use + 1; 
        MG.dev.vals <- c(MG.dev.vals,model0$parameters$Value[Ipnt])
        MG.dev.phs <- c(MG.dev.phs,model0$parameters$Phase[Ipnt])
       }  
     } # if   
   } # Ipars
  EM.obj$MG.dev.use <- MG.dev.use
  EM.obj$MG.dev.vals <- MG.dev.vals
  EM.obj$MG.dev.phs <- MG.dev.phs

  # -------------------------------------------------------------------------------------------------------------------

  # Recruitment phases, ramps and time-varying stuff
  EM.obj$logR0.phase <- ctl$SR_parms$PHASE[which(SR_parms.labs=="SR_LN(R0)")]
  EM.obj$steep.phase <-ctl$SR_parms$PHASE[which(SR_parms.labs=="SR_BH_steep")]
  EM.obj$sigmaR.phase <-ctl$SR_parms$PHASE[which(SR_parms.labs=="SR_sigmaR")]
  EM.obj$regime.phase <-ctl$SR_parms$PHASE[which(SR_parms.labs=="SR_regime")]
  EM.obj$Rec_dev_main.start <- ctl$MainRdevYrFirst-styr
  EM.obj$Rec_dev_early.start <- ctl$recdev_early_start
  if (EM.obj$Rec_dev_early.start>0) EM.obj$Rec_dev_early.start <- EM.obj$Rec_dev_early.start-ctl$MainRdevYrFirst
  
  EM.obj$Rec_dev_end <- endyr-ctl$MainRdevYrLast
  EM.obj$ast_yr_nobias_adj <- ctl$last_early_yr_nobias_adj-styr+1
  EM.obj$first_yr_fullbias_adj <- ctl$first_yr_fullbias_adj-styr+1
  EM.obj$last_yr_fullbias_adj <- ctl$last_yr_fullbias_adj-styr+1-nyears
  EM.obj$end_yr_for_ramp <-  ctl$first_recent_yr_nobias_adj-styr+1-nyears

  EM.obj$SR_blocks <- ctl$SR_parms$Block[1:5]
  EM.obj$SR_block_fn <- ctl$SR_parms$Block_Fxn[1:5]
  EM.obj$SR_env.dev <- ctl$SR_parms$'env_var&link'[1:5]
  SR.dev.use <- 0
  SR.dev.vals <- NULL
  SR.dev.phs <- NULL
  IndexA <- which(substr(model0$parameters$Label,1,3)=="SR_")
  Ipnt <- 5
  for (Ipar in 1:5) 
   {
    if ( EM.obj$SR_env.dev[Ipar]!=0)
     {
      Ipnt <- Ipnt + 1
      SR.dev.use <- SR.dev.use + 1; 
      SR.dev.vals <- c(SR.dev.vals,model0$parameters$Value[IndexA[Ipnt]])
      SR.dev.phs <- c(SR.dev.phs,model0$parameters$Phase[IndexA[Ipnt]])
     }  
    if ( EM.obj$SR_blocks[Ipar]!=0)
     {
      Iblock <- EM.obj$SR_blocks[Ipar]
      EM.obj$Nblocks <- EM.obj$Nblocks + 1
      EM.obj$nblocks.per.block <- c(EM.obj$nblocks.per.block,as.numeric(ctl$blocks_per_pattern[Iblock]))
      BlkYears <- ctl$Block_Design[[Iblock]]
      BlkYears <- as.numeric(c(BlkYears,rep(0,50-length(BlkYears))))
      Index <- which(BlkYears==endyrOrig); BlkYears[Index] <- endyr  
      EM.obj$block.years <- rbind(EM.obj$block.years,BlkYears)
      EM.obj$SR_blocks[Ipar] <- EM.obj$Nblocks
      for (Iblk in 1:ctl$blocks_per_pattern[Iblock])
      { 
        Ipnt <- Ipnt + 1
        SR.dev.use <- SR.dev.use + 1; 
        SR.dev.vals <- c(SR.dev.vals,model0$parameters$Value[IndexA[Ipnt]])
        SR.dev.phs <- c(SR.dev.phs,model0$parameters$Phase[IndexA[Ipnt]])
      }  
    }   
   } # Ipars
  #AEP
  #print(EM.obj$SR_blocks)
  #if (EM.obj$SR_blocks[1]!=0)
  # {
  #  EM.obj$SR_blocks[4] <- EM.obj$SR_blocks[1]; EM.obj$SR_blocks[1] <- 0
  #  EM.obj$SR_block_fn[4] <- EM.obj$SR_block_fn[1]; EM.obj$SR_block_fn[1] <- 0
  #  
  # }
  EM.obj$SR.dev.use <- SR.dev.use
  EM.obj$SR.dev.vals <- SR.dev.vals
  EM.obj$SR.dev.phs <- SR.dev.phs
  
  FirstMainRdevYrA <- ctl$MainRdevYrFirst
  EM.obj$early.dev <- NULL
  if (-EM.obj$Rec_dev_early.start >0)
  EM.obj$early.dev <- model0$recruitpars$Value[1:(-EM.obj$Rec_dev_early.start)]

  n.est.devs <- ctl$MainRdevYrLast-ctl$MainRdevYrFirst+1
  EM.obj$late.devs.est.y1 <- ctl$MainRdevYrFirst-styr+1
  EM.obj$late.devs.est.y2 <- ctl$MainRdevYrLast-styr+1
  EM.obj$late.devs <- model0$recruitpars$Value[-EM.obj$Rec_dev_early.start+1:n.est.devs]
  
  EM.obj$recdev_early_phase <- ctl$recdev_early_phase
  EM.obj$max_bias_adj <- ctl$max_bias_adj
  
  write(paste("#Late_Devs_est",EM.obj$late.devs.est.y1,EM.obj$late.devs.est.y2),RunFolder3,append=T)
  write(model0$recruitpars$Value[1:n.est.devs],ncol=n.est.devs,RunFolder3,append=T)
  write("",RunFolder3,append=T) 

  # -------------------------------------------------------------------------------------------------------------------
  
  # Selectivity (size)
  sel.Phases <- matrix(0,nrow=nfleets,ncol=15)
  sel.pars <- matrix(0,nrow=nfleets,ncol=15)
  Nsel.pars <- rep(0,nfleets)
  sel.blocks.no <- matrix(0,nrow=nfleets,ncol=15)
  sel.blocks.fn <- matrix(0,nrow=nfleets,ncol=15)
  sel.ann.devs.use <- matrix(0,nrow=nfleets,ncol=15)
  sel.ann.devs.miny <- matrix(0,nrow=nfleets,ncol=15)
  sel.ann.devs.maxy <- matrix(0,nrow=nfleets,ncol=15)
  sel.ann.devs.phs <- matrix(0,nrow=nfleets,ncol=15)
  sel.ann.devs.no <-  matrix(0,nrow=nfleets,ncol=15)
  sel.dev.blocks.vals <- NULL
  sel.dev.blocks.phs <- NULL

  ret.Phases <- matrix(0,nrow=nfleets,ncol=15)
  ret.pars <- matrix(0,nrow=nfleets,ncol=15)
  Nret.pars <- rep(0,nfleets)
  ret.blocks.no <- matrix(0,nrow=nfleets,ncol=15)
  ret.blocks.fn <- matrix(0,nrow=nfleets,ncol=15)
  ret.ann.devs.use <- matrix(0,nrow=nfleets,ncol=15)
  ret.ann.devs.miny <- matrix(0,nrow=nfleets,ncol=15)
  ret.ann.devs.maxy <- matrix(0,nrow=nfleets,ncol=15)
  ret.ann.devs.phs <- matrix(0,nrow=nfleets,ncol=15)
  ret.ann.devs.no <-  matrix(0,nrow=nfleets,ncol=15)
  ret.dev.blocks.vals <- NULL
  ret.dev.blocks.phs <- NULL
  
  mort.Phases <- matrix(0,nrow=nfleets,ncol=15)
  mort.pars <- matrix(0,nrow=nfleets,ncol=15)
  Nmort.pars <- rep(0,nfleets)

  Selex.pars.pnt.use <- 0
  Selex.pars.pnt <- c(which(substr(model0$parameters$Label,1,5)=="Size_"),which(substr(model0$parameters$Label,1,6)=="SzSel_"),which(substr(model0$parameters$Label,1,10)=="SizeSpline"))
  Selex.pars.pnt <- sort( Selex.pars.pnt)
  Retain.pars.pnt.use <- 0
  Retain.pars.pnt <- which(substr(model0$parameters$Label,1,9)=="Retain_L_")
  Mort.pars.pnt.use <- 0
  Mort.pars.pnt <- which(substr(model0$parameters$Label,1,11)=="DiscMort_L_")

  Par.point <- 0
  for (Ifleet in 1:nfleets)
   {
    Npars.sel <- -1; Npars.ret <- -999; Npars.mort <- -999
    if (ctl$size_selex_types$Pattern[Ifleet]==5) ctl$size_selex_types$Pattern[Ifleet] <<- 15
    if (ctl$size_selex_types$Pattern[Ifleet]==0) Npars.sel <- 0
    if (ctl$size_selex_types$Pattern[Ifleet]==1) Npars.sel <- 2
    if (ctl$size_selex_types$Pattern[Ifleet]==15) Npars.sel <- 0
    if (ctl$size_selex_types$Pattern[Ifleet]==17) Npars.sel <- ctl$size_selex_types$Special[Ifleet]+1
    if (ctl$size_selex_types$Pattern[Ifleet]==24) Npars.sel <- 6
    if (ctl$size_selex_types$Pattern[Ifleet]==27) Npars.sel <- 3+ 2*ctl$size_selex_types$Special[Ifleet]
    if (ctl$size_selex_types$Discard[Ifleet]==0) { Npars.ret <- 0; Npars.mort <- 0; }
    if (ctl$size_selex_types$Discard[Ifleet]==1) { Npars.ret <- 4; Npars.mort <- 0; }
    if (ctl$size_selex_types$Discard[Ifleet]==2) { Npars.ret <- 4; Npars.mort <- 4; }
    if (ctl$size_selex_types$Discard[Ifleet]==4) { Npars.ret <- 5+nsex; Npars.mort <- 4; }
    if (ctl$size_selex_types$Discard[Ifleet] <0) { Npars.ret <- 0; Npars.mort <- 0; }
    if (Npars.sel==-1) { print(c("size-selex1 error",Ifleet,ctl$size_selex_types$Pattern[Ifleet])); stop() }
    if (Npars.ret==-999) { print(c("size-retain1 error",Ifleet,ctl$size_selex_types$Discard[Ifleet])); stop() }
    if (ctl$size_selex_types$Male[Ifleet]==3 & ctl$size_selex_types$Pattern[Ifleet]==24) { Npars.sel <- Npars.sel + 5 }
    if (ctl$size_selex_types$Male[Ifleet]==3 & ctl$size_selex_types$Pattern[Ifleet]==1) { Npars.sel <- Npars.sel + 3 }
    Nsel.pars[Ifleet] <- Npars.sel
    Nret.pars[Ifleet] <- Npars.ret
    if (Npars.sel > 0)
     {
      Index <- which(model0$SelSizeAdj$Fleet==Ifleet & model0$SelSizeAdj$Yr==styr)
      for (Ipar in 1:Npars.sel) { Selex.pars.pnt.use <- Selex.pars.pnt.use + 1; sel.pars[Ifleet,Ipar] <- model0$parameters$Value[Selex.pars.pnt[Selex.pars.pnt.use]] }
      for (Ipar in 1:Npars.sel) sel.Phases[Ifleet,Ipar] <- ctl$size_selex_parms$PHASE[Par.point+Ipar]
      Par.point <- Par.point + Npars.sel
     }
    if (Npars.ret >0)
     {
      for (Ipar in 1:Npars.ret) { Retain.pars.pnt.use <- Retain.pars.pnt.use + 1; ret.pars[Ifleet,Ipar] <- model0$parameters$Value[Retain.pars.pnt[Retain.pars.pnt.use]] }
      for (Ipar in 1:Npars.ret) ret.Phases[Ifleet,Ipar] <- ctl$size_selex_parms$PHASE[Par.point+Ipar]
      Par.point <- Par.point + Npars.ret
     }
    if (Npars.mort >0)
     {
      for (Ipar in 1:Npars.mort) { Mort.pars.pnt.use <- Mort.pars.pnt.use + 1; mort.pars[Ifleet,Ipar] <- model0$parameters$Value[Mort.pars.pnt[Mort.pars.pnt.use]] }
      for (Ipar in 1:Npars.mort) mort.Phases[Ifleet,Ipar] <- ctl$size_selex_parms$PHASE[Par.point+Ipar]
      Par.point <- Par.point + Npars.mort
     }
  } # fleet
  Par.point <- 0
  for (Ifleet in 1:nfleets)
  {
    Npars.sel <- -1; Npars.ret <- -999; Npars.mort <- -999
    if (ctl$size_selex_types$Pattern[Ifleet]==0) Npars.sel <- 0
    if (ctl$size_selex_types$Pattern[Ifleet]==1) Npars.sel <- 2
    if (ctl$size_selex_types$Pattern[Ifleet]==5) Npars.sel <- 0
    if (ctl$size_selex_types$Pattern[Ifleet]==15) Npars.sel <- 0
    if (ctl$size_selex_types$Pattern[Ifleet]==17) Npars.sel <- ctl$size_selex_types$Special[Ifleet]+1
    if (ctl$size_selex_types$Pattern[Ifleet]==24) Npars.sel <- 6
    if (ctl$size_selex_types$Pattern[Ifleet]==27) Npars.sel <- 3 + 2*ctl$size_selex_types$Special[Ifleet]
    if (ctl$size_selex_types$Discard[Ifleet]==0) { Npars.ret <- 0; Npars.mort <- 0 }
    if (ctl$size_selex_types$Discard[Ifleet]==1) { Npars.ret <- 4; Npars.mort <- 0 }
    if (ctl$size_selex_types$Discard[Ifleet]==2) { Npars.ret <- 4; Npars.mort <- 4; }
    if (ctl$size_selex_types$Discard[Ifleet]==4) { Npars.ret <- 5+nsex; Npars.mort <- 4; }
    if (ctl$size_selex_types$Discard[Ifleet] <0) { Npars.ret <- 0; Npars.mort <- 0; }
    if (Npars.sel==-1) { print(c("size-selex2 error",Ifleet,ctl$size_selex_types$Pattern[Ifleet])) }
    if (Npars.ret==-999) { print(c("size-retain2 error",Ifleet,ctl$isze_selex_types$Discard[Ifleet])) }
    if (ctl$size_selex_types$Male[Ifleet]==3 & ctl$size_selex_types$Pattern[Ifleet]==24) { Npars.sel <- Npars.sel + 5 }
    if (ctl$size_selex_types$Male[Ifleet]==3 & ctl$size_selex_types$Pattern[Ifleet]==1) { Npars.sel <- Npars.sel + 3 }
    if (Npars.sel > 1)
     {
      Index <- which(model0$SelSizeAdj$Fleet==Ifleet & model0$SelSizeAdj$Yr==styr)
      for (Ipar in 1:Npars.sel) 
       {
         if (ctl$size_selex_parms$Block[Par.point+Ipar]!=0)
         {
          Iblock <- ctl$size_selex_parms$Block[Par.point+Ipar]
          EM.obj$Nblocks <- EM.obj$Nblocks + 1
          EM.obj$nblocks.per.block <- c(EM.obj$nblocks.per.block,as.numeric(ctl$blocks_per_pattern[Iblock]))
          BlkYears <- ctl$Block_Design[[Iblock]]
          BlkYears <- as.numeric(c(BlkYears,rep(0,50-length(BlkYears))))
          Index <- which(BlkYears==endyrOrig); BlkYears[Index] <- endyr  
          EM.obj$block.years <- rbind(EM.obj$block.years,BlkYears)
          sel.blocks.no[Ifleet,Ipar] <- Iblock
          sel.blocks.no[Ifleet,Ipar] <- EM.obj$Nblocks
          sel.blocks.fn[Ifleet,Ipar] <- ctl$size_selex_parms$Block_Fxn[Par.point+Ipar]
          for (Iblk in 1:ctl$blocks_per_pattern[Iblock])
          { Selex.pars.pnt.use <- Selex.pars.pnt.use + 1; 
           sel.dev.blocks.vals <- c(sel.dev.blocks.vals,model0$parameters$Value[Selex.pars.pnt[Selex.pars.pnt.use]])
           sel.dev.blocks.phs <- c(sel.dev.blocks.phs,model0$parameters$Phase[Selex.pars.pnt[Selex.pars.pnt.use]])
          }  
         } # Blocks
        if (ctl$size_selex_parms$dev_link[Par.point+Ipar]!=0)
         {
          sel.ann.devs.use[Ifleet,Ipar] <- ctl$size_selex_parms$dev_link[Par.point+Ipar]
          sel.ann.devs.miny[Ifleet,Ipar]<- ctl$size_selex_parms$dev_minyr[Par.point+Ipar]
          sel.ann.devs.maxy[Ifleet,Ipar] <- ctl$size_selex_parms$dev_maxy[Par.point+Ipar]
          sel.ann.devs.phs[Ifleet,Ipar] <-ctl$size_selex_parms$dev_PH[Par.point+Ipar]
          sel.ann.devs.no[Ifleet,Ipar] <- sel.ann.devs.maxy[Ifleet,Ipar]-sel.ann.devs.miny[Ifleet,Ipar]+1
          for (Iblk in 1:2)                      # This is related to SE and Propr
           { Selex.pars.pnt.use <- Selex.pars.pnt.use + 1; 
             sel.dev.blocks.vals <- c(sel.dev.blocks.vals,model0$parameters$Value[Selex.pars.pnt[Selex.pars.pnt.use]])
             sel.dev.blocks.phs <- c(sel.dev.blocks.phs,model0$parameters$Phase[Selex.pars.pnt[Selex.pars.pnt.use]])
            }  
        } # Annual devs
      } # Ipar
     } # if (Npars.sel > 1)
    Par.point <- Par.point + Npars.sel
    if (Npars.ret >0)
     {
      for (Ipar in 1:Npars.ret) 
       {
        if (ctl$size_selex_parms$Block[Par.point+Ipar]!=0)
         {
          Iblock <- ctl$size_selex_parms$Block[Par.point+Ipar]
          EM.obj$Nblocks <- EM.obj$Nblocks + 1
          EM.obj$nblocks.per.block <- c(EM.obj$nblocks.per.block,as.numeric(ctl$blocks_per_pattern[Iblock]))
          BlkYears <- ctl$Block_Design[[Iblock]]
          BlkYears <- as.numeric(c(BlkYears,rep(0,50-length(BlkYears))))
          Index <- which(BlkYears==endyrOrig); BlkYears[Index] <- endyr  
          EM.obj$block.years <- rbind(EM.obj$block.years,BlkYears)
          ret.blocks.no[Ifleet,Ipar] <- Iblock
          ret.blocks.no[Ifleet,Ipar] <- EM.obj$Nblocks
          ret.blocks.fn[Ifleet,Ipar] <- ctl$size_selex_parms$Block_Fxn[Par.point+Ipar]
          for (Iblk in 1:ctl$blocks_per_pattern[Iblock])
           { Retain.pars.pnt.use <- Retain.pars.pnt.use + 1; 
             sel.dev.blocks.vals <- c(sel.dev.blocks.vals,model0$parameters$Value[Retain.pars.pnt[Retain.pars.pnt.use]])
             sel.dev.blocks.phs <- c(sel.dev.blocks.phs,model0$parameters$Phase[Retain.pars.pnt[Retain.pars.pnt.use]])
            }  
         } # Blocks  
    
        if (ctl$size_selex_parms$dev_link[Par.point+Ipar]!=0)
         {
          ret.ann.devs.use[Ifleet,Ipar] <- ctl$size_selex_parms$dev_link[Par.point+Ipar]
          ret.ann.devs.miny[Ifleet,Ipar]<- ctl$size_selex_parms$dev_minyr[Par.point+Ipar]
          ret.ann.devs.maxy[Ifleet,Ipar] <- ctl$size_selex_parms$dev_maxy[Par.point+Ipar]
          ret.ann.devs.phs[Ifleet,Ipar] <-ctl$size_selex_parms$dev_PH[Par.point+Ipar]
          ret.ann.devs.no[Ifleet,Ipar] <- ret.ann.devs.maxy[Ifleet,Ipar]-ret.ann.devs.miny[Ifleet,Ipar]+1
          for (Iblk in 1:2)                      # This is related to SE and Propr
           { Retain.pars.pnt.use <- Retain.pars.pnt.use + 1; 
             ret.dev.blocks.vals <- c(ret.dev.blocks.vals,model0$parameters$Value[Retain.pars.pnt[Retain.pars.pnt.use]])
             ret.dev.blocks.phs <- c(ret.dev.blocks.phs,model0$parameters$Phase[Retain.pars.pnt[Retain.pars.pnt.use]])
           }  
         } # Annual devs
      } # Ipar
     } #if (Npars.ret >0)
    Par.point <- Par.point + Npars.ret
   } # fleet
  EM.obj$size_selex_types.pattern <- ctl$size_selex_types$Pattern
  EM.obj$size_selex_types.special <- ctl$size_selex_types$Special
  EM.obj$size_selex_types.male <- ctl$size_selex_types$Male
  EM.obj$size_selex_types.sel.pars <- sel.pars
  EM.obj$size_selex_types.sel.phase <- sel.Phases
  EM.obj$size_selex_types.blocks.no <- sel.blocks.no
  EM.obj$size_selex_types.blocks.fn <- sel.blocks.fn
  EM.obj$size_nsel.pars <- Nsel.pars
  EM.obj$size.sel.dev.blocks.vals <- sel.dev.blocks.vals
  EM.obj$size.sel.dev.blocks.phs <- sel.dev.blocks.phs
  EM.obj$size.sel.ann.devs.use <- sel.ann.devs.use
  EM.obj$size.sel.ann.devs.miny <- sel.ann.devs.miny
  EM.obj$size.sel.ann.devs.maxy <- sel.ann.devs.maxy
  EM.obj$size.sel.ann.devs.phs <- sel.ann.devs.phs
  
  EM.obj$size_retain_types.pattern <- ctl$size_selex_types$Discard[1:nfleet.catch]
  EM.obj$size_retain_types.ret.pars <- ret.pars
  EM.obj$size_retain_types.ret.phase <- ret.Phases
  EM.obj$size_retain_types.blocks.no <- ret.blocks.no
  EM.obj$size_retain_types.blocks.fn <- ret.blocks.fn
  EM.obj$size.ret.dev.blocks.vals <- ret.dev.blocks.vals
  EM.obj$size.ret.dev.blocks.phs <- ret.dev.blocks.phs
  EM.obj$size.ret.ann.devs.use <- ret.ann.devs.use
  EM.obj$size.ret.ann.devs.miny <- ret.ann.devs.miny
  EM.obj$size.ret.ann.devs.maxy <- ret.ann.devs.maxy
  EM.obj$size.ret.ann.devs.phs <- ret.ann.devs.phs
  
  EM.obj$size_retain_types.mort.pars <- mort.pars
  EM.obj$size_retain_types.mort.phase <- mort.Phases

  size.sel.dev.vals <- NULL
  size.sel.dev.phs <- NULL
  size.ret.dev.vals <- NULL
  size.ret.dev.phs <- NULL
   for (Ifleet in 1:nfleets)
   {
    Npars.sel <- Nsel.pars[Ifleet]
    if (Npars.sel > 0)
     for (Ipar in 1:Npars.sel)  
      if (sel.ann.devs.no[Ifleet,Ipar])
       for (Iblk in 1:sel.ann.devs.no[Ifleet,Ipar])  
        {
         Selex.pars.pnt.use <- Selex.pars.pnt.use + 1;
         size.sel.dev.vals <- c(size.sel.dev.vals,model0$parameters$Value[Selex.pars.pnt[Selex.pars.pnt.use]]) 
         size.sel.dev.phs <- c(size.sel.dev.phs,model0$parameters$Phase[Selex.pars.pnt[Selex.pars.pnt.use]]) 
        }
   } # Ifleet
  for (Ifleet in 1:nfleets)
   {
    Npars.ret <- Nret.pars[Ifleet]
    if (Npars.ret > 0)
      for (Ipar in 1:Npars.ret)  
        if (ret.ann.devs.no[Ifleet,Ipar])
          for (Iblk in 1:ret.ann.devs.no[Ifleet,Ipar])  
          {
            Retain.pars.pnt.use <- Retain.pars.pnt.use + 1;
            size.ret.dev.vals <- c(size.ret.dev.vals,model0$parameters$Value[Retain.pars.pnt[Retain.pars.pnt.use]]) 
            size.ret.dev.phs <- c(size.ret.dev.phs,model0$parameters$Phase[Retain.pars.pnt[Retain.pars.pnt.use]]) 
          }
    
   } # Ifleet
  EM.obj$size.sel.dev.vals <- size.sel.dev.vals
  EM.obj$size.sel.dev.phs <-size.sel.dev.phs
  EM.obj$size.ret.dev.vals <- size.ret.dev.vals
  EM.obj$size.ret.dev.phs <- size.ret.dev.phs

  # -------------------------------------------------------------------------------------------------------------------

  # Selectivity (age)
  sel.Phases <- matrix(0,nrow=nfleets,ncol=15)
  sel.pars <- matrix(0,nrow=nfleets,ncol=15)
  Nsel.pars <- rep(0,nfleets)
  sel.blocks.no <- matrix(0,nrow=nfleets,ncol=15)
  sel.blocks.fn <- matrix(0,nrow=nfleets,ncol=15)
  sel.ann.devs.use <- matrix(0,nrow=nfleets,ncol=15)
  sel.ann.devs.miny <- matrix(0,nrow=nfleets,ncol=15)
  sel.ann.devs.maxy <- matrix(0,nrow=nfleets,ncol=15)
  sel.ann.devs.phs <- matrix(0,nrow=nfleets,ncol=15)
  sel.ann.devs.no <-  matrix(0,nrow=nfleets,ncol=15)
  sel.dev.blocks.vals <- NULL
  sel.dev.blocks.phs <- NULL
  
  ret.Phases <- matrix(0,nrow=nfleets,ncol=15)
  ret.pars <- matrix(0,nrow=nfleets,ncol=15)
  Nret.pars <- rep(0,nfleets)
  ret.blocks.no <- matrix(0,nrow=nfleets,ncol=15)
  ret.blocks.fn <- matrix(0,nrow=nfleets,ncol=15)
  ret.ann.devs.use <- matrix(0,nrow=nfleets,ncol=15)
  ret.ann.devs.miny <- matrix(0,nrow=nfleets,ncol=15)
  ret.ann.devs.maxy <- matrix(0,nrow=nfleets,ncol=15)
  ret.ann.devs.phs <- matrix(0,nrow=nfleets,ncol=15)
  ret.ann.devs.no <-  matrix(0,nrow=nfleets,ncol=15)
  ret.dev.blocks.vals <- NULL
  ret.dev.blocks.phs <- NULL
  
  mort.Phases <- matrix(0,nrow=nfleets,ncol=15)
  mort.pars <- matrix(0,nrow=nfleets,ncol=15)
  Nmort.pars <- rep(0,nfleets)

  Selex.pars.pnt.use <- 0
  Selex.pars.pnt <- c(which(substr(model0$parameters$Label,1,4)=="Age_"),which(substr(model0$parameters$Label,1,7)=="AgeSel_"),which(substr(model0$parameters$Label,1,9)=="AgeSpline"))
  Selex.pars.pnt <- sort( Selex.pars.pnt)
  Retain.pars.pnt.use <- 0
  Retain.pars.pnt <- which(substr(model0$parameters$Label,1,9)=="Retain_A_")
  Mort.pars.pnt.use <- 0
  Mort.pars.pnt <- which(substr(model0$parameters$Label,1,11)=="DiscMort_A_")

  Par.point <- 0
  for (Ifleet in 1:nfleets)
  {
    Npars.sel <- -1; Npars.ret <- -999; Npars.mort <- -999
    if (ctl$age_selex_types$Pattern[Ifleet]==11) ctl$age_selex_types$Pattern[Ifleet] <<- 0
    if (ctl$age_selex_types$Pattern[Ifleet]==0) Npars.sel <- 0
    if (ctl$age_selex_types$Pattern[Ifleet]==10) Npars.sel <- 0
    if (ctl$age_selex_types$Pattern[Ifleet]==12) Npars.sel <- 2
    if (ctl$age_selex_types$Pattern[Ifleet]==14) Npars.sel <- nages
    if (ctl$age_selex_types$Pattern[Ifleet]==15) Npars.sel <- 0
    if (ctl$age_selex_types$Pattern[Ifleet]==17) Npars.sel <- ctl$age_selex_types$Special[Ifleet]+1
    if (ctl$age_selex_types$Pattern[Ifleet]==20) Npars.sel <- 6
    if (ctl$age_selex_types$Pattern[Ifleet]==27) Npars.sel <- 3+ 2*ctl$age_selex_types$Special[Ifleet]
    if (ctl$age_selex_types$Discard[Ifleet]==0) { Npars.ret <- 0; Npars.mort <- 0 }
    if (ctl$age_selex_types$Discard[Ifleet]==1) { Npars.ret <- 4; Npars.mort <- 0 }
    if (ctl$age_selex_types$Discard[Ifleet]==2) { Npars.ret <- 4; Npars.mort <- 4 }
    if (ctl$age_selex_types$Discard[Ifleet]==4) { Npars.ret <- 5+nsex; Npars.mort <- 4 }
    if (ctl$age_selex_types$Discard[Ifleet] <0) { Npars.ret <- 0; Npars.mort <- 0; }
    if (Npars.sel==-1) { print(c("age-selex1 error",Ifleet,ctl$age_selex_types$Pattern[Ifleet])) }
    if (Npars.ret==-999) { print(c("age-retain1 error",Ifleet,ctl$age_selex_types$Discard[Ifleet])) }
    if (ctl$age_selex_types$Male[Ifleet]==3 & ctl$age_selex_types$Pattern[Ifleet]==20) { Npars.sel <- Npars.sel + 5 }
    if (ctl$age_selex_types$Male[Ifleet]==3 & ctl$age_selex_types$Pattern[Ifleet]==12) { Npars.sel <- Npars.sel + 3 }
    Nsel.pars[Ifleet] <- Npars.sel
    Nret.pars[Ifleet] <- Npars.ret
    if (Npars.sel > 0)
    {
      Index <- which(model0$SelAgeAdj$Fleet==Ifleet & model0$SelAgeAdj$Yr==styr)
      for (Ipar in 1:Npars.sel) { Selex.pars.pnt.use <- Selex.pars.pnt.use + 1; sel.pars[Ifleet,Ipar] <- model0$parameters$Value[Selex.pars.pnt[Selex.pars.pnt.use]] }
      for (Ipar in 1:Npars.sel) sel.Phases[Ifleet,Ipar] <- ctl$age_selex_parms$PHASE[Par.point+Ipar]
      Par.point <- Par.point + Npars.sel
    }
    if (Npars.ret >0)
    {
      for (Ipar in 1:Npars.ret) { Retain.pars.pnt.use <- Retain.pars.pnt.use + 1; ret.pars[Ifleet,Ipar] <- model0$parameters$Value[Retain.pars.pnt[Retain.pars.pnt.use]] }
      for (Ipar in 1:Npars.ret) ret.Phases[Ifleet,Ipar] <- ctl$age_selex_parms$PHASE[Par.point+Ipar]
      Par.point <- Par.point + Npars.ret
    }
    if (Npars.mort >0)
     {
      for (Ipar in 1:Npars.mort) { Mort.pars.pnt.use <- Mort.pars.pnt.use + 1; mort.pars[Ifleet,Ipar] <- model0$parameters$Value[Mort.pars.pnt[Mort.pars.pnt.use]] }
      for (Ipar in 1:Npars.mort) mort.Phases[Ifleet,Ipar] <- ctl$age_selex_parms$PHASE[Par.point+Ipar]
      Par.point <- Par.point + Npars.mort
     }
  } # fleet
  
  Par.point <- 0
  for (Ifleet in 1:nfleets)
  {
    Npars.sel <- -1; Npars.ret <- -999; Npars.mort <- -999
    if (ctl$age_selex_types$Pattern[Ifleet]==11) ctl$age_selex_types$Pattern[Ifleet] <<- 0
    if (ctl$age_selex_types$Pattern[Ifleet]==0) Npars.sel <- 0
    if (ctl$age_selex_types$Pattern[Ifleet]==10) Npars.sel <- 0
    if (ctl$age_selex_types$Pattern[Ifleet]==12) Npars.sel <- 2
    if (ctl$age_selex_types$Pattern[Ifleet]==14) Npars.sel <- nages
    if (ctl$age_selex_types$Pattern[Ifleet]==15) Npars.sel <- 0
    if (ctl$age_selex_types$Pattern[Ifleet]==17) Npars.sel <- ctl$age_selex_types$Special[Ifleet]+1
    if (ctl$age_selex_types$Pattern[Ifleet]==20) Npars.sel <- 6
    if (ctl$age_selex_types$Pattern[Ifleet]==27) Npars.sel <- 3+ 2*ctl$age_selex_types$Special[Ifleet]
    if (ctl$age_selex_types$Discard[Ifleet]==0) { Npars.ret <- 0; Npars.mort <- 0 }
    if (ctl$age_selex_types$Discard[Ifleet]==1) { Npars.ret <- 4; Npars.mort <- 0 }
    if (ctl$age_selex_types$Discard[Ifleet]==2) { Npars.ret <- 4; Npars.mort <- 4 }
    if (ctl$age_selex_types$Discard[Ifleet]==4) { Npars.ret <- 5+nsex; Npars.mort <- 4 }
    if (ctl$age_selex_types$Discard[Ifleet] <0) { Npars.ret <- 0; Npars.mort <- 0; }
    if (Npars.sel==-1) { print(c("age-selex2 error",Ifleet,ctl$age_selex_types$Pattern[Ifleet])) }
    if (Npars.ret==-999) { print(c("age-retain2 error",Ifleet,ctl$age_selex_types$Discard[Ifleet])) }
    if (ctl$age_selex_types$Male[Ifleet]==3 & ctl$age_selex_types$Pattern[Ifleet]==20) { Npars.sel <- Npars.sel + 5 }
    if (ctl$age_selex_types$Male[Ifleet]==3 & ctl$age_selex_types$Pattern[Ifleet]==12) { Npars.sel <- Npars.sel + 3 }
    if (Npars.sel > 1)
    {
      Index <- which(model0$SelSizeAdj$Fleet==Ifleet & model0$SelSizeAdj$Yr==styr)
      for (Ipar in 1:Npars.sel) 
      {
         if (ctl$age_selex_parms$Block[Par.point+Ipar]!=0)
        {
          Iblock <- ctl$age_selex_parms$Block[Par.point+Ipar]
          EM.obj$Nblocks <- EM.obj$Nblocks + 1
          EM.obj$nblocks.per.block <- c(EM.obj$nblocks.per.block,as.numeric(ctl$blocks_per_pattern[Iblock]))
          BlkYears <- ctl$Block_Design[[Iblock]]
          BlkYears <- as.numeric(c(BlkYears,rep(0,50-length(BlkYears))))
          Index <- which(BlkYears==endyrOrig); BlkYears[Index] <- endyr  
          EM.obj$block.years <- rbind(EM.obj$block.years,BlkYears)
          sel.blocks.no[Ifleet,Ipar] <- Iblock
          sel.blocks.no[Ifleet,Ipar] <- EM.obj$Nblocks
          sel.blocks.fn[Ifleet,Ipar] <- ctl$age_selex_parms$Block_Fxn[Par.point+Ipar]
          for (Iblk in 1:ctl$blocks_per_pattern[Iblock])
          { Selex.pars.pnt.use <- Selex.pars.pnt.use + 1; 
          sel.dev.blocks.vals <- c(sel.dev.blocks.vals,model0$parameters$Value[Selex.pars.pnt[Selex.pars.pnt.use]])
          sel.dev.blocks.phs <- c(sel.dev.blocks.phs,model0$parameters$Phase[Selex.pars.pnt[Selex.pars.pnt.use]])
          }  
        } # Blocks
        if (ctl$age_selex_parms$dev_link[Par.point+Ipar]!=0)
        {
          sel.ann.devs.use[Ifleet,Ipar] <- ctl$age_selex_parms$dev_link[Par.point+Ipar]
          sel.ann.devs.miny[Ifleet,Ipar]<- ctl$age_selex_parms$dev_minyr[Par.point+Ipar]
          sel.ann.devs.maxy[Ifleet,Ipar] <- ctl$age_selex_parms$dev_maxy[Par.point+Ipar]
          sel.ann.devs.phs[Ifleet,Ipar] <-ctl$age_selex_parms$dev_PH[Par.point+Ipar]
          sel.ann.devs.no[Ifleet,Ipar] <- sel.ann.devs.maxy[Ifleet,Ipar]-sel.ann.devs.miny[Ifleet,Ipar]+1
          for (Iblk in 1:2)                      # This is related to SE and Propr
          { Selex.pars.pnt.use <- Selex.pars.pnt.use + 1; 
          sel.dev.blocks.vals <- c(sel.dev.blocks.vals,model0$parameters$Value[Selex.pars.pnt[Selex.pars.pnt.use]])
          sel.dev.blocks.phs <- c(sel.dev.blocks.phs,model0$parameters$Phase[Selex.pars.pnt[Selex.pars.pnt.use]])
          }  
        } # Annual devs
      } # Ipar
    } # if (Npars.sel > 1)
    Par.point <- Par.point + Npars.sel
    if (Npars.ret >0)
    {
      for (Ipar in 1:Npars.ret) 
      {
        if (ctl$age_selex_parms$Block[Par.point+Ipar]!=0)
        {
          Iblock <- ctl$age_selex_parms$Block[Par.point+Ipar]
          EM.obj$Nblocks <- EM.obj$Nblocks + 1
          EM.obj$nblocks.per.block <- c(EM.obj$nblocks.per.block,as.numeric(ctl$blocks_per_pattern[Iblock]))
          BlkYears <- ctl$Block_Design[[Iblock]]
          BlkYears <- as.numeric(c(BlkYears,rep(0,50-length(BlkYears))))
          Index <- which(BlkYears==endyrOrig); BlkYears[Index] <- endyr  
          EM.obj$block.years <- rbind(EM.obj$block.years,BlkYears)
          ret.blocks.no[Ifleet,Ipar] <- Iblock
          ret.blocks.no[Ifleet,Ipar] <- EM.obj$Nblocks
          ret.blocks.fn[Ifleet,Ipar] <- ctl$age_selex_parms$Block_Fxn[Par.point+Ipar]
          for (Iblk in 1:ctl$blocks_per_pattern[Iblock])
          { Retain.pars.pnt.use <- Retain.pars.pnt.use + 1; 
          sel.dev.blocks.vals <- c(sel.dev.blocks.vals,model0$parameters$Value[Retain.pars.pnt[Retain.pars.pnt.use]])
          sel.dev.blocks.phs <- c(sel.dev.blocks.phs,model0$parameters$Phase[Retain.pars.pnt[Retain.pars.pnt.use]])
          }  
        } # Blocks  
        
        if (ctl$age_selex_parms$dev_link[Par.point+Ipar]!=0)
        {
          ret.ann.devs.use[Ifleet,Ipar] <- ctl$age_selex_parms$dev_link[Par.point+Ipar]
          ret.ann.devs.miny[Ifleet,Ipar]<- ctl$age_selex_parms$dev_minyr[Par.point+Ipar]
          ret.ann.devs.maxy[Ifleet,Ipar] <- ctl$age_selex_parms$dev_maxy[Par.point+Ipar]
          ret.ann.devs.phs[Ifleet,Ipar] <-ctl$age_selex_parms$dev_PH[Par.point+Ipar]
          ret.ann.devs.no[Ifleet,Ipar] <- ret.ann.devs.maxy[Ifleet,Ipar]-ret.ann.devs.miny[Ifleet,Ipar]+1
          for (Iblk in 1:2)                      # This is related to SE and Propr
          { Retain.pars.pnt.use <- Retain.pars.pnt.use + 1; 
          ret.dev.blocks.vals <- c(ret.dev.blocks.vals,model0$parameters$Value[Retain.pars.pnt[Retain.pars.pnt.use]])
          ret.dev.blocks.phs <- c(ret.dev.blocks.phs,model0$parameters$Phase[Retain.pars.pnt[Retain.pars.pnt.use]])
          }  
        } # Annual devs
      } # Ipar
    } #if (Npars.ret >0)
    Par.point <- Par.point + Npars.ret
  } # fleet
  EM.obj$age_selex_types.pattern <- ctl$age_selex_types$Pattern
  EM.obj$age_selex_types.special <- ctl$age_selex_types$Special
  EM.obj$age_selex_types.male <- ctl$age_selex_types$Male
  EM.obj$age_selex_types.sel.pars <- sel.pars
  EM.obj$age_selex_types.sel.phase <- sel.Phases
  EM.obj$age_selex_types.blocks.no <- sel.blocks.no
  EM.obj$age_selex_types.blocks.fn <- sel.blocks.fn
  EM.obj$age_nsel.pars <- Nsel.pars
  EM.obj$age.sel.dev.blocks.vals <- sel.dev.blocks.vals
  EM.obj$age.sel.dev.blocks.phs <- sel.dev.blocks.phs
  EM.obj$age.sel.ann.devs.use <- sel.ann.devs.use
  EM.obj$age.sel.ann.devs.miny <- sel.ann.devs.miny
  EM.obj$age.sel.ann.devs.maxy <- sel.ann.devs.maxy
  EM.obj$age.sel.ann.devs.phs <- sel.ann.devs.phs
  
  EM.obj$age_retain_types.pattern <- ctl$age_selex_types$Discard[1:nfleet.catch]
  EM.obj$age_retain_types.ret.pars <- ret.pars
  EM.obj$age_retain_types.ret.phase <- ret.Phases
  EM.obj$age_retain_types.blocks.no <- ret.blocks.no
  EM.obj$age_retain_types.blocks.fn <- ret.blocks.fn
  EM.obj$age.ret.dev.blocks.vals <- ret.dev.blocks.vals
  EM.obj$age.ret.dev.blocks.phs <- ret.dev.blocks.phs
  EM.obj$age.ret.ann.devs.use <- ret.ann.devs.use
  EM.obj$age.ret.ann.devs.miny <- ret.ann.devs.miny
  EM.obj$age.ret.ann.devs.maxy <- ret.ann.devs.maxy
  EM.obj$age.ret.ann.devs.phs <- ret.ann.devs.phs
  
  EM.obj$age_retain_types.mort.pars <- mort.pars
  EM.obj$age_retain_types.mort.phase <- mort.Phases

    age.sel.dev.vals <- NULL
  age.sel.dev.phs <- NULL
  age.ret.dev.vals <- NULL
  age.ret.dev.phs <- NULL
  for (Ifleet in 1:nfleets)
  {
    Npars.sel <- Nsel.pars[Ifleet]
    if (Npars.sel > 0)
      for (Ipar in 1:Npars.sel)  
        if (sel.ann.devs.no[Ifleet,Ipar])
          for (Iblk in 1:sel.ann.devs.no[Ifleet,Ipar])  
          {
            Selex.pars.pnt.use <- Selex.pars.pnt.use + 1;
            age.sel.dev.vals <- c(age.sel.dev.vals,model0$parameters$Value[Selex.pars.pnt[Selex.pars.pnt.use]]) 
            age.sel.dev.phs <- c(age.sel.dev.phs,model0$parameters$Phase[Selex.pars.pnt[Selex.pars.pnt.use]]) 
          }
  } # Ifleet
  for (Ifleet in 1:nfleets)
  {
    Npars.ret <- Nret.pars[Ifleet]
    if (Npars.ret > 0)
      for (Ipar in 1:Npars.ret)  
        if (ret.ann.devs.no[Ifleet,Ipar])
          for (Iblk in 1:ret.ann.devs.no[Ifleet,Ipar])  
          {
            Retain.pars.pnt.use <- Retain.pars.pnt.use + 1;
            age.ret.dev.vals <- c(age.ret.dev.vals,model0$parameters$Value[Retain.pars.pnt[Retain.pars.pnt.use]]) 
            age.ret.dev.phs <- c(age.ret.dev.phs,model0$parameters$Phase[Retain.pars.pnt[Retain.pars.pnt.use]]) 
          }
    
  } # Ifleet
  EM.obj$age.sel.dev.vals <- age.sel.dev.vals
  EM.obj$age.sel.dev.phs <- age.sel.dev.phs
  EM.obj$age.ret.dev.vals <- age.ret.dev.vals
  EM.obj$age.ret.dev.phs <- age.ret.dev.phs
  
  # -------------------------------------------------------------------------------------------------------------------
  
  cat("SAVING",RunFolder3a,"\n")
  write.out.EM(OM.obj,EM.obj,RunFolder3,Control.Rule.Type)
  cat("SAVING",RunFolder3a,"\n")
  save(EM.obj,file=RunFolder3a)
  
  write.out.Gen(OM.obj,RunFolder4,RunType=RunType,Reduced.Uncertain=Reduce.Uncertain,Narea=OM.obj$nareas)
  
  return(NULL)
}

# =====================================================================================================================

CaseAll <- 1                                               # 1 means try a run (0=mean constant catch)
RunType <- "Not Test"
RunType <- "Test"

# YearAdjust - this is designed to handle cases where there are not data for the last year with catches
# AgeAdjust - Switch off age data

ExtraCat <- read.csv("ExtraCatches.Csv",head=F)
ncol.Cat <- length(ExtraCat[1,])

#for (Ispec in c("A1","A2","B1",paste0("C",c(1:10)),paste0("D",c(1:9)) ) )
#for (Ispec in paste0("D",c(19,28)))
#for (Ispec in paste0("C",c(1:11)))
#for (Ispec in paste0("C",c(6,8,9,10,11)))
#for (Ispec in paste0("C",c(8,9)))
#for (Ispec in paste0("C",c(9)))
# for (Ispec in paste0("D",c(11)))
##for (Ispec in paste0("D",c(12,19,21,28)))
for (Ispec in c(paste0("D",c(6:12, 20, 22, 28)) ) ) # skip 13 20

     {
  Analsis.Type <- -999;
  YearAdjust <- 0; AgeAdjust <- F; Reduce.Uncertain <- 0; Nproj <- 50; Control.Rule.Type <- "CSIRO"
  Project.Type <- 1; FakeAreas <- 0; FillProjection <- F
  
  # Hint: Set: FillProjection to T to extend the input file to allow for data beyond the historical period
  # Hint: Set: FakeAreas > 0 to create a multi-area model
  # Hint: Set: Project.Type to 3 (from 1) to do projections based on historical F
  
  # Buffer paper
  #if (Ispec=="A1")  { Analsis.Type <- 1; SS_folder <- "Whiting_Tuned/"; Species <- "School whiting"  } 
  #if (Ispec=="A2")  { Analsis.Type <- 1; SS_folder <- "Flathead_Tuned/"; Species <- "Tiger flathead"  } 

  # OMF 5
  #if (Ispec=="B1")  { Analsis.Type <- 2; SS_folder <- "P cod/"; Species <- "P cod"; Reduce.Uncertain=1; Nproj=40  } 
  
  # CSIRO
  if (Ispec=="C1")  { Analsis.Type <- 3; SS_folder <- "Bight redfish"; Species <- "Bight redfish"  } 
  if (Ispec=="C2")  { Analsis.Type <- 3; SS_folder <- "Blue grenadier/"; Species <- "Blue grenadier"  } 
  if (Ispec=="C3")  { Analsis.Type <- 3; SS_folder <- "Deepwater flathead/"; Species <- "Deepwater flathead"  } 
  if (Ispec=="C4")  { Analsis.Type <- 3; SS_folder <- "Morwong/"; Species <- "Morwong"  } 
  if (Ispec=="C5")  { Analsis.Type <- 3; SS_folder <- "Orange roughy east/"; Species <- "Orange roughy east"  } 
  if (Ispec=="C6")  { Analsis.Type <- 3; SS_folder <- "Pink ling/"; Species <- "Pink ling"; FakeAreas <- 4  } 
  if (Ispec=="C7")  { Analsis.Type <- 3; SS_folder <- "Redfish/"; Species <- "Redfish"  } 
  if (Ispec=="C8")  { Analsis.Type <- 3; SS_folder <- "School whiting/"; Species <- "School whiting"; FakeAreas <- 4  } 
  if (Ispec=="C9")  { Analsis.Type <- 3; SS_folder <- "Silver warehou/"; Species <- "Silver warehou"  } 
  if (Ispec=="C10")  { Analsis.Type <- 3; SS_folder <- "Tiger flathead/"; Species <- "Tiger flathead"; FakeAreas <- 4  } 
  if (Ispec=="C11")  { Analsis.Type <- 3; SS_folder <- "Mirror dory/"; Species <- "Mirror dory"; FakeAreas <- 4  } 
  
  
  # Other
  if (Ispec=="D1")  { Analsis.Type <- 4; SS_folder <- "Milk Shark"; Species <- "Milk Shark"  } 
  if (Ispec=="D2")  { Analsis.Type <- 4; SS_folder <- "Perch"; Species <- "Perch"  } 
  if (Ispec=="D3")  { Analsis.Type <- 4; SS_folder <- "Herring"; Species <- "Herring"  } 
  if (Ispec=="D4")  { Analsis.Type <- 4; SS_folder <- "SSageselemod2"; Species <- "SSageselemod2"  } 
  if (Ispec=="D5")  { Analsis.Type <- 4; SS_folder <- "Runze"; Species <- "Runze"  } 
  if (Ispec=="D6")  { Analsis.Type <- 4; SS_folder <- "P cod"; Species <- "P cod"; Control.Rule.Type <- "NPFMC"; FakeAreas <- 4  } 
  if (Ispec=="D7")  { Analsis.Type <- 4; SS_folder <- "Bluespot_Pilbara"; Species <- "Bluespot_Pilbara"  } 
  if (Ispec=="D8")  { Analsis.Type <- 4; SS_folder <- "Red_Emperor_Kimberley"; Species <- "Red_Emperor_Kimberley"  } 
  if (Ispec=="D9")  { Analsis.Type <- 4; SS_folder <- "Red_Emperor_Pilbara"; Species <- "Red_Emperor_Pilbara"  } 
  if (Ispec=="D10")  { Analsis.Type <- 4; SS_folder <- "SandySprat"; Species <- "SandySprat"  } 
  if (Ispec=="D11")  { Analsis.Type <- 4; SS_folder <- "WA_Dhufish"; Species <- "WA_Dhufish"  } 
  if (Ispec=="D12")  { Analsis.Type <- 4; SS_folder <- "Sandbar_Shark"; Species <- "Sandbar_Shark"  } 
  if (Ispec=="D13")  { Analsis.Type <- 4; SS_folder <- "Sandbar_Shark2"; Species <- "Sandbar_Shark2"  } 
  if (Ispec=="D14")  { Analsis.Type <- 4; SS_folder <- "Sardine"; Species <- "Sardine"  } 
  if (Ispec=="D15")  { Analsis.Type <- 4; SS_folder <- "Mackerel"; Species <- "Mackerel"  } 
  if (Ispec=="D16")  { Analsis.Type <- 4; SS_folder <- "Anchovy"; Species <- "Anchovy"  } 
  if (Ispec=="D17")  { Analsis.Type <- 4; SS_folder <- "Squid"; Species <- "Squid"  } 
  if (Ispec=="D19")  { Analsis.Type <- 4; SS_folder <- "Gummy"; Species <- "Gummy"}
  if (Ispec=="D20")  { Analsis.Type <- 4; SS_folder <- "Quillback"; Species <- "Quillback"}
  if (Ispec=="D21")  { Analsis.Type <- 4; SS_folder <- "Dusky"; Species <- "Dusky"}
  if (Ispec=="D22")  { Analsis.Type <- 4; SS_folder <- "Goldband"; Species <- "Goldband"}
  if (Ispec=="D23")  { Analsis.Type <- 4; SS_folder <- "Snapper_Gascoyne"; Species <- "Snapper_Gascoyne"}
  if (Ispec=="D24")  { Analsis.Type <- 4; SS_folder <- "Snapper_North"; Species <- "Snapper_North"}
  if (Ispec=="D25")  { Analsis.Type <- 4; SS_folder <- "WCDSC"; Species <- "WCDSC"}
  if (Ispec=="D26")  { Analsis.Type <- 4; SS_folder <- "LM_CL3"; Species <- "LM_CL3"}
  if (Ispec=="D27")  { Analsis.Type <- 4; SS_folder <- "Rankin"; Species <- "Rankin"}
  if (Ispec=="D28")  { Analsis.Type <- 4; SS_folder <- "Whiskery_Shark"; Species <- "Whiskery_Shark"}
  if (Ispec=="D18")  { Analsis.Type <- 4; SS_folder <- "Red_Emperor_Pilbara_2A"; Species <- "Red_Emperor_Pilbara_2A"; Project.Type <- 3 }
  if (Ispec=="D29")  { Analsis.Type <- 4; SS_folder <- "Red_Emperor_Pilbara_2A_nodevs"; Species <- "Red_Emperor_Pilbara_2A_nodevs"; Project.Type <- 1 }
  if (Ispec=="D30")  { Analsis.Type <- 4; SS_folder <- "Red_Emperor_Pilbara_2A_nodevs_CVfix"; Species <- "Red_Emperor_Pilbara_2A_nodevs_CVfix"; Project.Type <- 1 }

  
  if (Analsis.Type==1) BasePath <- paste0(Path,"Inputs Buffer/")
  if (Analsis.Type==1) AssignmentPath <- paste0(Path,"Base Buffer files/")
  if (Analsis.Type==2) BasePath <- paste0(Path,"Inputs OMF52/")
  if (Analsis.Type==2) AssignmentPath <- paste0(Path,"Base OMF5 files/")
  if (Analsis.Type==3) BasePath <- paste0(Path,"Inputs CSIRO/")
  if (Analsis.Type==3) AssignmentPath <- paste0(Path,"Base CSIRO files/")
  if (Analsis.Type==4) BasePath <- paste0(Path,"Inputs Other/")
  if (Analsis.Type==4) AssignmentPath <- paste0(Path,"Base Other files/")
  
  Index <- which(ExtraCat[,1]==Ispec)
  if (length(Index)>0)
   {
    NextraCat <- as.numeric(ExtraCat[Index,2])
    print(c(Ispec,NextraCat,Index))
    CatYears <- as.numeric(ExtraCat[Index+1:NextraCat,1])
    CatFuture <- matrix(0,nrow=NextraCat,ncol=ncol.Cat-1)
    for (Iyear in 1:NextraCat) CatFuture[Iyear,] <- as.numeric(ExtraCat[Index+Iyear,2:ncol.Cat])
   } 
  else
  { NextraCat <- 0; CatYears <- NULL; CatFuture <- NULL }
    
  if (Analsis.Type != -999)
   {
    setwd(BasePath)
    Species2 <- gsub(" ",".",Species)
    cat(Ispec," ",Species2,"\n")
  
    Outcomes <- do.extract(SS_folder,Species2,YearAdjust=YearAdjust,AgeAdjust=AgeAdjust,
                           Reduce.Uncertain=Reduce.Uncertain,Control.Rule.Type=Control.Rule.Type,
                           Project.Type=Project.Type,
                           NextraCat=NextraCat,CatYears=CatYears,CatFuture=CatFuture,
                           FakeAreas=FakeAreas,FillProjection=FillProjection)  
   }
  
}
