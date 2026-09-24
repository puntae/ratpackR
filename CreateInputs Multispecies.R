rm(list=ls())
library(r4ss)
library(readxl)
source("C:/Research/NewRat/Newrat_spat/R inserts/SaveFiles.R")


CaseAll <- 1                                               # 1 means try a run (0=mean constant catch)

# ===========================================================================================================================================

do.process.OM <- function(OM.obj,Fleet.to.Metier,Nmetiers,FileName1,MetierNames,SelOrig,
                          DataOrig,IndexOrig,Nmetiers.catch)
{
   
  RunFolder2 <- paste0(OutputPath,FileName1)
  print(RunFolder2)
 
  # Save the orignal object
  OM.obj.org <- OM.obj
  nfleets <- OM.obj$nfleets
  nfleet.catch <- OM.obj$nfleet.catch
  nyears <- OM.obj$nyears
  nareas <- OM.obj$nareas
  nsex <- OM.obj$nsex
  
  # Update fleets
  OM.obj$nfleets <- Nmetiers
  OM.obj$nfleet.catch <- Nmetiers.catch
  OM.obj$fleetnames <- MetierNames
  
  # --------------------------------------------------------------------------------------------------------------------------------
  
  # Selectivity and retention (size)
  # Size-based selectivity (Females)
  Initial.sel <- NULL; for (Imet in 1:Nmetiers) Initial.sel <- rbind(Initial.sel,OM.obj.org$Size_Initial.sel_fem[SelOrig[Imet],])
  OM.obj$Size_Initial.sel_fem <- Initial.sel
  Size_Nblocks.selex <- NULL; for (Imet in 1:Nmetiers) Size_Nblocks.selex <- c(Size_Nblocks.selex,OM.obj.org$Size_Nblocks.selex_fem[SelOrig[Imet]])
  OM.obj$Size_Nblocks.selex_fem <- Size_Nblocks.selex
  #if (OM.obj.org$Size_Nblocks.selex[1]>0) Accum <- 1 else Accum <- 0; for (Imet in 2:nfleets)   Accum  <- c(Accum, Accum[Imet-1]+OM.obj.org$Size_Nblocks.selex[Imet]) 
  if (OM.obj.org$Size_Nblocks.selex_fem[1]>0) Accum2 <- 0 else Accum2 <- 0; for (Imet in 2:nfleets) Accum2 <- c(Accum2,Accum2[Imet-1]+OM.obj.org$Size_Nblocks.selex_fem[Imet-1]) 
  BlockSels.selex <- NULL;
  Blocks.selex <- NULL
  for (Imet in 1:Nmetiers) 
   if (OM.obj.org$Size_Nblocks.selex_fem[SelOrig[Imet]]>0)
    for (Iblk in 1:OM.obj.org$Size_Nblocks.selex_fem[SelOrig[Imet]])
     {
      #Blocks.selex <- rbind(Blocks.selex,OM.obj.org$Size_Blocks.selex[Accum[SelOrig[Imet]]+Iblk-1,]) 
      #BlockSels.selex <- rbind(BlockSels.selex,OM.obj.org$Size_BlockSels.selex[Accum[SelOrig[Imet]]+Iblk-1,]) 
      if (SelOrig[Imet]==1) RefSel <- 0 else RefSel <- Accum2[SelOrig[Imet]]
      Blocks.selex <- rbind(Blocks.selex,OM.obj.org$Size_Blocks.selex_fem[RefSel+Iblk,]) 
      BlockSels.selex <- rbind(BlockSels.selex,OM.obj.org$Size_BlockSels.selex_fem[RefSel+Iblk,]) 
     }
  OM.obj$Size_Blocks.selex_fem <- Blocks.selex
  OM.obj$Size_BlockSels.selex_fem <- BlockSels.selex

  # Size-based selectivity (Males)
  if (nsex > 1)
   {
    Initial.sel <- NULL; for (Imet in 1:Nmetiers) Initial.sel <- rbind(Initial.sel,OM.obj.org$Size_Initial.sel_mal[SelOrig[Imet],])
    OM.obj$Size_Initial.sel_mal <- Initial.sel
    print(OM.obj.org$Size_Nblocks.selex_mal)
    Size_Nblocks.selex <- NULL; for (Imet in 1:Nmetiers) Size_Nblocks.selex <- c(Size_Nblocks.selex,OM.obj.org$Size_Nblocks.selex_mal[SelOrig[Imet]])
    OM.obj$Size_Nblocks.selex_mal <- Size_Nblocks.selex
    #if (OM.obj.org$Size_Nblocks.selex[1]>0) Accum <- 1 else Accum <- 0; for (Imet in 2:nfleets)   Accum  <- c(Accum, Accum[Imet-1]+OM.obj.org$Size_Nblocks.selex[Imet]) 
    if (OM.obj.org$Size_Nblocks.selex_mal[1]>0) Accum2 <- 0 else Accum2 <- 0; for (Imet in 2:nfleets) Accum2 <- c(Accum2,Accum2[Imet-1]+OM.obj.org$Size_Nblocks.selex_mal[Imet-1]) 
    BlockSels.selex <- NULL;
    Blocks.selex <- NULL
    for (Imet in 1:Nmetiers) 
     if (OM.obj.org$Size_Nblocks.selex_mal[SelOrig[Imet]]>0)
      for (Iblk in 1:OM.obj.org$Size_Nblocks.selex_mal[SelOrig[Imet]])
       {
        #Blocks.selex <- rbind(Blocks.selex,OM.obj.org$Size_Blocks.selex[Accum[SelOrig[Imet]]+Iblk-1,]) 
        #BlockSels.selex <- rbind(BlockSels.selex,OM.obj.org$Size_BlockSels.selex[Accum[SelOrig[Imet]]+Iblk-1,]) 
        if (SelOrig[Imet]==1) RefSel <- 0 else RefSel <- Accum2[SelOrig[Imet]]
        Blocks.selex <- rbind(Blocks.selex,OM.obj.org$Size_Blocks.selex_mal[RefSel+Iblk,]) 
        BlockSels.selex <- rbind(BlockSels.selex,OM.obj.org$Size_BlockSels.selex_mal[RefSel+Iblk,]) 
       }
    OM.obj$Size_Blocks.selex_mal <- Blocks.selex
    OM.obj$Size_BlockSels.selex_mal <- BlockSels.selex
   }
    
  OM.tst <<- OM.obj
  
  # Size-based retention (females)
  Initial.ret <- NULL; for (Imet in 1:Nmetiers) Initial.ret <- rbind(Initial.ret,OM.obj.org$Size_Initial.ret_fem[SelOrig[Imet],])
  OM.obj$Size_Initial.ret_fem <- Initial.ret
  Size_Nblocks.retain <- NULL; for (Imet in 1:Nmetiers) Size_Nblocks.retain <- c(Size_Nblocks.retain,OM.obj.org$Size_Nblocks.retain_fem[SelOrig[Imet]])
  OM.obj$Size_Nblocks.retain_fem <- Size_Nblocks.retain
  #if (OM.obj.org$Size_Nblocks.retain[1]>0) Accum <- 1 else Accum <- 0; for (Imet in 2:nfleets)   Accum  <- c(Accum, Accum[Imet-1]+OM.obj.org$Size_Nblocks.retain[Imet])
  #if (OM.obj.org$Size_Nblocks.retain[1]>0) Accum2 <- 1 else Accum2 <- 0; for (Imet in 2:nfleets) Accum2 <- c(Accum2,Accum2[Imet-1]+OM.obj.org$Size_Nblocks.retain[Imet-1]-1) 
  if (OM.obj.org$Size_Nblocks.retain_fem[1]>0) Accum2 <- 0 else Accum2 <- 0; for (Imet in 2:nfleets) Accum2 <- c(Accum2,Accum2[Imet-1]+OM.obj.org$Size_Nblocks.retain_fem[Imet-1]) 
  BlockSels.retain <- NULL;
  Blocks.retain <- NULL
  for (Imet in 1:Nmetiers) 
   if (SelOrig[Imet] >0)  
    if (OM.obj.org$Size_Nblocks.retain_fem[SelOrig[Imet]]>0)
     for (Iblk in 1:OM.obj.org$Size_Nblocks.retain_fem[SelOrig[Imet]])
      {
       #Blocks.retain <- rbind(Blocks.retain,OM.obj.org$Size_Blocks.retain[Accum[SelOrig[Imet]]+Iblk-1,]) 
       #BlockSels.retain <- rbind(BlockSels.retain,OM.obj.org$Size_BlockSels.retain[Accum[SelOrig[Imet]]+Iblk-1,]) 
       if (SelOrig[Imet]==1) RefSel <- 0 else RefSel <- Accum2[SelOrig[Imet]]
       Blocks.retain <- rbind(Blocks.retain,OM.obj.org$Size_Blocks.retain_fem[RefSel+Iblk,]) 
       BlockSels.retain <- rbind(BlockSels.retain,OM.obj.org$Size_BlockSels.retain_fem[RefSel+Iblk,]) 
     }
  OM.obj$Size_Blocks.retain_fem <- Blocks.retain
  OM.obj$Size_BlockSels.retain_fem <- BlockSels.retain

  # Size-based retention (males)
  if (nsex > 1)
   {
    Initial.ret <- NULL; for (Imet in 1:Nmetiers) Initial.ret <- rbind(Initial.ret,OM.obj.org$Size_Initial.ret_mal[SelOrig[Imet],])
    OM.obj$Size_Initial.ret_mal <- Initial.ret
    Size_Nblocks.retain <- NULL; for (Imet in 1:Nmetiers) Size_Nblocks.retain <- c(Size_Nblocks.retain,OM.obj.org$Size_Nblocks.retain_mal[SelOrig[Imet]])
    OM.obj$Size_Nblocks.retain_mal <- Size_Nblocks.retain
    #if (OM.obj.org$Size_Nblocks.retain[1]>0) Accum <- 1 else Accum <- 0; for (Imet in 2:nfleets)   Accum  <- c(Accum, Accum[Imet-1]+OM.obj.org$Size_Nblocks.retain[Imet])
    #if (OM.obj.org$Size_Nblocks.retain[1]>0) Accum2 <- 1 else Accum2 <- 0; for (Imet in 2:nfleets) Accum2 <- c(Accum2,Accum2[Imet-1]+OM.obj.org$Size_Nblocks.retain[Imet-1]-1) 
    if (OM.obj.org$Size_Nblocks.retain_mal[1]>0) Accum2 <- 0 else Accum2 <- 0; for (Imet in 2:nfleets) Accum2 <- c(Accum2,Accum2[Imet-1]+OM.obj.org$Size_Nblocks.retain_mal[Imet-1]) 
    BlockSels.retain <- NULL;
    Blocks.retain <- NULL
    for (Imet in 1:Nmetiers) 
     if (SelOrig[Imet] >0)  
      if (OM.obj.org$Size_Nblocks.retain_mal[SelOrig[Imet]]>0)
        for (Iblk in 1:OM.obj.org$Size_Nblocks.retain_mal[SelOrig[Imet]])
        {
          #Blocks.retain <- rbind(Blocks.retain,OM.obj.org$Size_Blocks.retain[Accum[SelOrig[Imet]]+Iblk-1,]) 
          #BlockSels.retain <- rbind(BlockSels.retain,OM.obj.org$Size_BlockSels.retain[Accum[SelOrig[Imet]]+Iblk-1,]) 
          if (SelOrig[Imet]==1) RefSel <- 0 else RefSel <- Accum2[SelOrig[Imet]]
          Blocks.retain <- rbind(Blocks.retain,OM.obj.org$Size_Blocks.retain_mal[RefSel+Iblk,]) 
          BlockSels.retain <- rbind(BlockSels.retain,OM.obj.org$Size_BlockSels.retain_mal[RefSel+Iblk,]) 
        }
    OM.obj$Size_Blocks.retain_mal <- Blocks.retain
    OM.obj$Size_BlockSels.retain_mal <- BlockSels.retain
   }

  Initial.mort <- NULL; for (Imet in 1:Nmetiers) Initial.mort <- rbind(Initial.mort,OM.obj.org$Size_Initial.mort_fem[SelOrig[Imet],])
  OM.obj$Size_Initial.mort_fem <- Initial.mort
  if (nsex > 1)
   {
    Initial.mort <- NULL; for (Imet in 1:Nmetiers) Initial.mort <- rbind(Initial.mort,OM.obj.org$Size_Initial.mort_mal[SelOrig[Imet],])
    OM.obj$Size_Initial.mort_mal <- Initial.mort
   }
  print("size dn")
  
  # Selectivity and retention (Age)
  # Age-based selectivity (Females)
  Initial.sel <- NULL; for (Imet in 1:Nmetiers) Initial.sel <- rbind(Initial.sel,OM.obj.org$Age_Initial.sel_fem[SelOrig[Imet],])
  OM.obj$Age_Initial.sel_fem <- Initial.sel
  Age_Nblocks.selex <- NULL; for (Imet in 1:Nmetiers) Age_Nblocks.selex <- c(Age_Nblocks.selex,OM.obj.org$Age_Nblocks.selex_fem[SelOrig[Imet]])
  OM.obj$Age_Nblocks.selex_fem <- Age_Nblocks.selex
  #if (OM.obj.org$Age_Nblocks.selex[1]>0) Accum <- 1 else Accum <- 0; for (Imet in 2:nfleets) Accum <- c(Accum,Accum[Imet-1]+OM.obj.org$Age_Nblocks.selex[Imet]) 
  if (OM.obj.org$Age_Nblocks.selex_fem[1]>0) Accum2 <- 1 else Accum2 <- 0; for (Imet in 2:nfleets) Accum2 <- c(Accum2,Accum2[Imet-1]+OM.obj.org$Age_Nblocks.selex_fem[Imet-1]-1) 
  BlockSels.selex <- NULL;
  Blocks.selex <- NULL
  for (Imet in 1:Nmetiers) 
    if (SelOrig[Imet] >0 )
     if (OM.obj.org$Age_Nblocks.selex_fem[SelOrig[Imet]]>0)
      for (Iblk in 1:OM.obj.org$Age_Nblocks.selex_fem[SelOrig[Imet]])
       {
        if (SelOrig[Imet]==1) RefSel <- 0 else RefSel <- Accum2[SelOrig[Imet]]
        Blocks.selex <- rbind(Blocks.selex,OM.obj.org$Age_Blocks.selex_fem[RefSel+Iblk,]) 
        BlockSels.selex <- rbind(BlockSels.selex,OM.obj.org$Age_BlockSels.selex_fem[RefSel+Iblk,]) 
        }
  OM.obj$Age_Blocks.selex_fem <- Blocks.selex
  OM.obj$Age_BlockSels.selex_fem <- BlockSels.selex
  
  # Age-based selectivity (Males)
  if (nsex > 1)
   {
    Initial.sel <- NULL; for (Imet in 1:Nmetiers) Initial.sel <- rbind(Initial.sel,OM.obj.org$Age_Initial.sel_mal[SelOrig[Imet],])
    OM.obj$Age_Initial.sel_mal <- Initial.sel
    Age_Nblocks.selex <- NULL; for (Imet in 1:Nmetiers) Age_Nblocks.selex <- c(Age_Nblocks.selex,OM.obj.org$Age_Nblocks.selex_mal[SelOrig[Imet]])
    OM.obj$Age_Nblocks.selex_mal <- Age_Nblocks.selex
    #if (OM.obj.org$Age_Nblocks.selex[1]>0) Accum <- 1 else Accum <- 0; for (Imet in 2:nfleets) Accum <- c(Accum,Accum[Imet-1]+OM.obj.org$Age_Nblocks.selex[Imet]) 
    if (OM.obj.org$Age_Nblocks.selex_mal[1]>0) Accum2 <- 1 else Accum2 <- 0; for (Imet in 2:nfleets) Accum2 <- c(Accum2,Accum2[Imet-1]+OM.obj.org$Age_Nblocks.selex_mal[Imet-1]-1) 
    BlockSels.selex <- NULL;
    Blocks.selex <- NULL
    for (Imet in 1:Nmetiers) 
     if (SelOrig[Imet] >0 )
      if (OM.obj.org$Age_Nblocks.selex_mal[SelOrig[Imet]]>0)
        for (Iblk in 1:OM.obj.org$Age_Nblocks.selex_mal[SelOrig[Imet]])
         {
          if (SelOrig[Imet]==1) RefSel <- 0 else RefSel <- Accum2[SelOrig[Imet]]
          Blocks.selex <- rbind(Blocks.selex,OM.obj.org$Age_Blocks.selex_mal[RefSel+Iblk,]) 
          BlockSels.selex <- rbind(BlockSels.selex,OM.obj.org$Age_BlockSels.selex_mal[RefSel+Iblk,]) 
         }
    OM.obj$Age_Blocks.selex_mal <- Blocks.selex
    OM.obj$Age_BlockSels.selex_mal <- BlockSels.selex
  }
    
  # Age-based retention (females)
  Initial.ret <- NULL; for (Imet in 1:Nmetiers) Initial.ret <- rbind(Initial.ret,OM.obj.org$Age_Initial.ret_fem[SelOrig[Imet],])
  OM.obj$Age_Initial.ret_fem <- Initial.ret
  Age_Nblocks.retain <- NULL; for (Imet in 1:Nmetiers) Age_Nblocks.retain <- c(Age_Nblocks.retain,OM.obj.org$Age_Nblocks.retain_fem[SelOrig[Imet]])
  OM.obj$Age_Nblocks.retain_fem <- Age_Nblocks.retain
  #if (OM.obj.org$Age_Nblocks.retain[1]>0) Accum <- 1 else Accum <- 0; for (Imet in 2:nfleets)   Accum  <- c(Accum, Accum[Imet-1]+OM.obj.org$Age_Nblocks.retain[Imet])
  if (OM.obj.org$Age_Nblocks.retain_fem[1]>0) Accum2 <- 1 else Accum2 <- 0; for (Imet in 2:nfleets) Accum2 <- c(Accum2,Accum2[Imet-1]+OM.obj.org$Age_Nblocks.retain_fem[Imet-1]-1) 
  BlockSels.retain <- NULL;
  Blocks.retain <- NULL
  for (Imet in 1:Nmetiers) 
   if (SelOrig[Imet] >0 )
    if (OM.obj.org$Age_Nblocks.retain_fem[SelOrig[Imet]]>0)
     for (Iblk in 1:OM.obj.org$Age_Nblocks.retain_fem[SelOrig[Imet]])
      {
        #Blocks.retain <- rbind(Blocks.retain,OM.obj.org$Age_Blocks.retain[Accum[SelOrig[Imet]]+Iblk-1,]) 
        #BlockSels.retain <- rbind(BlockSels.retain,OM.obj.org$Age_BlockSels.retain[Accum[SelOrig[Imet]]+Iblk-1,]) 
        if (SelOrig[Imet]==1) RefSel <- 0 else RefSel <- Accum2[SelOrig[Imet]]
        Blocks.retain <- rbind(Blocks.retain,OM.obj.org$Age_Blocks.retain_fem[RefSel+Iblk,]) 
        BlockSels.retain <- rbind(BlockSels.retain,OM.obj.org$Age_BlockSels.retain_fem[RefSel+Iblk,]) 
      }
  OM.obj$Age_Blocks.retain_fem <- Blocks.retain
  OM.obj$Age_BlockSels.retain_fem <- BlockSels.retain
  
  # Age-based retention (males)
  if (nsex > 1)
   {
    Initial.ret <- NULL; for (Imet in 1:Nmetiers) Initial.ret <- rbind(Initial.ret,OM.obj.org$Age_Initial.ret_mal[SelOrig[Imet],])
    OM.obj$Age_Initial.ret_mal <- Initial.ret
    Age_Nblocks.retain <- NULL; for (Imet in 1:Nmetiers) Age_Nblocks.retain <- c(Age_Nblocks.retain,OM.obj.org$Age_Nblocks.retain_mal[SelOrig[Imet]])
    OM.obj$Age_Nblocks.retain_mal <- Age_Nblocks.retain
    #if (OM.obj.org$Age_Nblocks.retain[1]>0) Accum <- 1 else Accum <- 0; for (Imet in 2:nfleets)   Accum  <- c(Accum, Accum[Imet-1]+OM.obj.org$Age_Nblocks.retain[Imet])
    if (OM.obj.org$Age_Nblocks.retain_mal[1]>0) Accum2 <- 1 else Accum2 <- 0; for (Imet in 2:nfleets) Accum2 <- c(Accum2,Accum2[Imet-1]+OM.obj.org$Age_Nblocks.retain_mal[Imet-1]-1) 
    BlockSels.retain <- NULL;
    Blocks.retain <- NULL
    for (Imet in 1:Nmetiers) 
     if (SelOrig[Imet] >0 )
      if (OM.obj.org$Age_Nblocks.retain_mal[SelOrig[Imet]]>0)
        for (Iblk in 1:OM.obj.org$Age_Nblocks.retain_mal[SelOrig[Imet]])
        {
          #Blocks.retain <- rbind(Blocks.retain,OM.obj.org$Age_Blocks.retain[Accum[SelOrig[Imet]]+Iblk-1,]) 
          #BlockSels.retain <- rbind(BlockSels.retain,OM.obj.org$Age_BlockSels.retain[Accum[SelOrig[Imet]]+Iblk-1,]) 
          if (SelOrig[Imet]==1) RefSel <- 0 else RefSel <- Accum2[SelOrig[Imet]]
          Blocks.retain <- rbind(Blocks.retain,OM.obj.org$Age_Blocks.retain_mal[RefSel+Iblk,]) 
          BlockSels.retain <- rbind(BlockSels.retain,OM.obj.org$Age_BlockSels.retain_mal[RefSel+Iblk,]) 
        }
    OM.obj$Age_Blocks.retain_mal <- Blocks.retain
    OM.obj$Age_BlockSels.retain_mal <- BlockSels.retain
   }
  
  Initial.mort <- NULL; for (Imet in 1:Nmetiers) Initial.mort <- rbind(Initial.mort,OM.obj.org$Age_Initial.mort_fem[SelOrig[Imet],])
  OM.obj$Age_Initial.mort_fem <- Initial.mort
  if (nsex > 1)
   {
    Initial.mort <- NULL; for (Imet in 1:Nmetiers) Initial.mort <- rbind(Initial.mort,OM.obj.org$Age_Initial.mort_mal[SelOrig[Imet],])
    OM.obj$Age_Initial.mort_mal <- Initial.mort
   }
  
 #  catches
  eqn.catch <- rep(0,Nmetiers.catch)
  for (Imet in 1:Nmetiers.catch) 
    for (Ifleet in 1:nfleet.catch)
     eqn.catch[Imet] <- eqn.catch[Imet] + OM.obj.org$eqn.catch[Ifleet] * Fleet.to.Metier[Ifleet,Imet]/sum(Fleet.to.Metier[Ifleet,]) 
  OM.obj$eqn.catch <- eqn.catch
  hist.catch <- matrix(0,nrow=nyears,ncol=1+Nmetiers.catch); hist.catch[,1] <- OM.obj$hist.catch[,1]
    for (Iyr in 1:nyears)
    for (Imet in 1:Nmetiers.catch) 
      for (Ifleet in 1:nfleet.catch)
       hist.catch[Iyr,Imet+1] <- hist.catch[Iyr,Imet+1] + OM.obj$hist.catch[Iyr,1+Ifleet] * Fleet.to.Metier[Ifleet,Imet]/sum(Fleet.to.Metier[Ifleet,]) 
  OM.obj$hist.catch <- hist.catch
  
  # Initial F
  Initial_F  <- rep(0,Nmetiers.catch)  
  for (Imet in 1:Nmetiers.catch) 
   for (Ifleet in 1:nfleet.catch)
     Initial_F[Imet] <-  Initial_F[Imet] + OM.obj.org$Initial_F[SelOrig[Imet]]* Fleet.to.Metier[Ifleet,Imet]/sum(Fleet.to.Metier[Ifleet,]) 
   OM.obj$Initial_F <- Initial_F
  
  # Areas and fleets
  area.fleets <- matrix(0,nrow=nareas,ncol=Nmetiers)
  for (Imet in 1:Nmetiers)
   for (Iarea in 1:nareas)
     area.fleets[Iarea,Imet] <- OM.obj.org$area.fleets[Iarea,SelOrig[Imet]]
  OM.obj$area.fleets <- area.fleets
  print(OM.obj.org$area.fleets)
  print(area.fleets)

  # --------------------------------------------------------------------------------------------------------------------------------
  # index data
  OM.obj$index.Use <- rep("No",Nmetiers);    for (Imet in 1:Nmetiers) if (IndexOrig[Imet]!=0) OM.obj$index.Use[Imet]    <-   OM.obj.org$index.Use[IndexOrig[Imet]]     else OM.obj$index.Use[Imet] <- "No"
  OM.obj$index.CV.past <- rep(0,Nmetiers);  for (Imet in 1:Nmetiers) if (IndexOrig[Imet]!=0) OM.obj$index.CV.past[Imet] <-  OM.obj.org$index.CV.past[IndexOrig[Imet]]
  OM.obj$index.CV.fut <- rep(0,Nmetiers);   for (Imet in 1:Nmetiers) if (IndexOrig[Imet]!=0) OM.obj$index.CV.fut[Imet] <-   OM.obj.org$index.CV.fut[IndexOrig[Imet]]  else OM.obj$index.CV.fut[Imet] <- 0
  OM.obj$index.freq <- rep(0,Nmetiers);     for (Imet in 1:Nmetiers) if (IndexOrig[Imet]!=0) OM.obj$index.freq[Imet] <-     OM.obj.org$index.freq[IndexOrig[Imet]]    else OM.obj$index.freq[Imet] <- -1
  OM.obj$index.q <- rep(0,Nmetiers);        for (Imet in 1:Nmetiers) if (IndexOrig[Imet]!=0) OM.obj$index.q[Imet] <-        OM.obj.org$index.q[IndexOrig[Imet]]       else OM.obj$index.q[Imet] <- 0
  index.tab <- matrix(-1,nrow=nyears,ncol=Nmetiers+1); index.tab[,1] <- OM.obj.org$index.tab[,1]
  for (Iyear in 1:nyears)
   for (Imet in 1:Nmetiers)
    if (IndexOrig[Imet]!=0) index.tab[Iyear,Imet+1] <- OM.obj$index.tab[Iyear,IndexOrig[Imet]+1]
  OM.obj$index.tab <- index.tab
  
  # discard data
  OM.obj$discard.Use <- rep("No",Nmetiers.catch);      for (Imet in 1:Nmetiers.catch) if (DataOrig[Imet]!=0) OM.obj$discard.Use[Imet]    <-   OM.obj.org$discard.Use[DataOrig[Imet]]     else OM.obj$discard.Use[Imet] <- "No"
  for (Imet in 1:Nmetiers.catch) if (is.na(OM.obj$discard.Use[Imet])) OM.obj$discard.Use[Imet] <- "F"
  OM.obj$discard.type <- rep(0,Nmetiers.catch);     for (Imet in 1:Nmetiers.catch) if (DataOrig[Imet]!=0) OM.obj$discard.type[Imet] <-     OM.obj.org$discard.type[DataOrig[Imet]]    else OM.obj$discard.type[Imet] <- 0
  OM.obj$discard.CV.past <- rep(0,Nmetiers.catch);  for (Imet in 1:Nmetiers.catch) if (DataOrig[Imet]!=0) OM.obj$discard.CV.past[Imet] <-  OM.obj.org$discard.CV.past[DataOrig[Imet]] else OM.obj$discard.CV.past[Imet] <- 0
  OM.obj$discard.CV.fut <- rep(0,Nmetiers.catch);   for (Imet in 1:Nmetiers.catch) if (DataOrig[Imet]!=0) OM.obj$discard.CV.fut[Imet] <-   OM.obj.org$discard.CV.fut[DataOrig[Imet]]  else OM.obj$discard.CV.fut[Imet] <- 0
  OM.obj$discard.freq <- rep(0,Nmetiers.catch);     for (Imet in 1:Nmetiers.catch) if (DataOrig[Imet]!=0) OM.obj$discard.freq[Imet] <-     OM.obj.org$discard.freq[DataOrig[Imet]]    else OM.obj$discard.freq[Imet] <- -1
  for (Imet in 1:Nmetiers.catch) if (OM.obj$discard.Use[Imet]=="F") OM.obj$discard.type[Imet] <- 0
  for (Imet in 1:Nmetiers.catch) if (OM.obj$discard.Use[Imet]=="F") OM.obj$discard.CV.past[Imet] <- 0
  for (Imet in 1:Nmetiers.catch) if (OM.obj$discard.Use[Imet]=="F") OM.obj$discard.CV.fut[Imet] <- 0
  for (Imet in 1:Nmetiers.catch) if (OM.obj$discard.Use[Imet]=="F") OM.obj$discard.freq[Imet] <- 0
  discard.tab <- matrix(-1,nrow=nyears,ncol=Nmetiers.catch+1); discard.tab[,1] <- OM.obj.org$discard.tab[,1]
  for (Iyear in 1:nyears)
   for (Imet in 1:Nmetiers.catch)
    if (OM.obj$discard.Use[Imet]=="T")
     if (DataOrig[Imet]!=0) discard.tab[Iyear,Imet+1] <- OM.obj$discard.tab[Iyear,DataOrig[Imet]+1]
  OM.obj$discard.tab <- discard.tab
  
  # length data
  OM.obj$length.Use <- rep("No",Nmetiers);      for (Imet in 1:Nmetiers) if (DataOrig[Imet]!=0) OM.obj$length.Use[Imet]    <-   OM.obj.org$length.Use[DataOrig[Imet]]     else OM.obj$length.Use[Imet] <- "No"
  length.ESS.past <- matrix(0,nrow=3,ncol=Nmetiers)
  length.ESS.fut <- matrix(0,nrow=3,ncol=Nmetiers)
  length.freq <- matrix(0,nrow=3,ncol=Nmetiers)
  for (Ipart in 1:3) for (Imet in 1:Nmetiers) 
    if (DataOrig[Imet]!=0) 
      { length.ESS.past[Ipart,Imet] <- OM.obj.org$length.ESS.past[Ipart,DataOrig[Imet]]; length.ESS.fut[Ipart,Imet] <- OM.obj.org$length.ESS.fut[Ipart,DataOrig[Imet]];length.freq[Ipart,Imet] <- OM.obj.org$length.freq[Ipart,DataOrig[Imet]] }
  OM.obj$length.ESS.past <- length.ESS.past  
  OM.obj$length.ESS.fut <- length.ESS.fut  
  OM.obj$length.freq <- length.freq  
  length.tab <- array(-1,dim=c(3,nyears,Nmetiers)); 
  for (Ipart in 1:3)
   for (Iyear in 1:nyears)
    for (Imet in 1:Nmetiers)
      if (DataOrig[Imet]!=0) length.tab[Ipart,Iyear,Imet] <- OM.obj$length.tab[Ipart,Iyear,DataOrig[Imet]]
  OM.obj$length.tab <- length.tab  
  
  # age data
  OM.obj$age.Use <- rep("No",Nmetiers);      for (Imet in 1:Nmetiers) if (DataOrig[Imet]!=0) OM.obj$age.Use[Imet]    <-   OM.obj.org$age.Use[DataOrig[Imet]]     else OM.obj$age.Use[Imet] <- "No"
  age.ESS.past <- matrix(0,nrow=3,ncol=Nmetiers)
  age.ESS.fut <- matrix(0,nrow=3,ncol=Nmetiers)
  age.freq <- matrix(0,nrow=3,ncol=Nmetiers)
  for (Ipart in 1:3) for (Imet in 1:Nmetiers) 
    if (DataOrig[Imet]!=0)
    { age.ESS.past[Ipart,Imet] <- OM.obj.org$age.ESS.past[Ipart,DataOrig[Imet]]; age.ESS.fut[Ipart,Imet] <- OM.obj.org$age.ESS.fut[Ipart,DataOrig[Imet]];age.freq[Ipart,Imet] <- OM.obj.org$age.freq[Ipart,DataOrig[Imet]] }
  OM.obj$age.ESS.past <- age.ESS.past  
  OM.obj$age.ESS.fut <- age.ESS.fut  
  OM.obj$age.freq <- age.freq  
  age.tab <- array(-1,dim=c(3,nyears,Nmetiers)); 
  for (Ipart in 1:3)
    for (Iyear in 1:nyears)
      for (Imet in 1:Nmetiers)
        if (DataOrig[Imet]!=0) age.tab[Ipart,Iyear,Imet] <- OM.obj$age.tab[Ipart,Iyear,DataOrig[Imet]]
  OM.obj$age.tab <- age.tab  
  
  # caa data
  OM.obj$caa.Use <- rep("No",Nmetiers);      for (Imet in 1:Nmetiers) if (DataOrig[Imet]!=0) OM.obj$caa.Use[Imet]    <-   OM.obj.org$caa.Use[DataOrig[Imet]]     else OM.obj$caa.Use[Imet] <- "No"
  caa.ESS.past <- matrix(0,nrow=3,ncol=Nmetiers)
  caa.ESS.fut <- matrix(0,nrow=3,ncol=Nmetiers)
  caa.freq <- matrix(0,nrow=3,ncol=Nmetiers)
  for (Ipart in 1:3) for (Imet in 1:Nmetiers) 
    if (DataOrig[Imet]!=0) 
    { caa.ESS.past[Ipart,Imet] <- OM.obj.org$caa.ESS.past[Ipart,DataOrig[Imet]]; caa.ESS.fut[Ipart,Imet] <- OM.obj.org$caa.ESS.fut[Ipart,DataOrig[Imet]];caa.freq[Ipart,Imet] <- OM.obj.org$caa.freq[Ipart,DataOrig[Imet]] }
  OM.obj$caa.ESS.past <- caa.ESS.past  
  OM.obj$caa.ESS.fut <- caa.ESS.fut  
  OM.obj$caa.freq <- caa.freq  
  caa.tab <- array(-1,dim=c(3,nyears,Nmetiers)); 
  for (Ipart in 1:3)
    for (Iyear in 1:nyears)
      for (Imet in 1:Nmetiers)
        if (DataOrig[Imet]!=0) caa.tab[Ipart,Iyear,Imet] <- OM.obj$caa.tab[Ipart,Iyear,DataOrig[Imet]]
  OM.obj$caa.tab <- caa.tab  
  
  # --------------------------------------------------------------------------------------------------------------------------------
  
  print("Saving OM")
  write.out.OM(OM.obj,RunFolder2,Control.Rule.Type)
  return(OM.obj)
  
}
# ===========================================================================================================================================

do.process.EM <- function(OM.obj,EM.obj,Fleet.to.Metier,Nmetiers,Nmetiers.catch,FileName1,FileName3,SelOrig,
                          DataOrig,SelOrigEst,PGMSY.Species="No")
{
  
  RunFolder2 <- paste0(OutputPath,FileName1)
  print(RunFolder2)
  RunFolder4 <- paste0(OutputPath,FileName3)
  print(RunFolder4)
 
  # Save the orignal object
  EM.obj.org <- EM.obj
  nfleets <- Nmetiers
  nyears <- OM.obj$nyears
  nfleet.catch <- Nmetiers.catch
  
  # Update fleets
  OM.obj$nfleet.catch <- Nmetiers.catch

  # --------------------------------------------------------------------------------------------------------------

  N.orig.fleet <- length(EM.obj$size_selex_types.pattern)

  Block.devs <- array(NA,dim=c(N.orig.fleet,2,15,100))
  Block.phs <- array(NA,dim=c(N.orig.fleet,2,15,100))
  Ipoint <- 1
  for (Ifleet in 1:N.orig.fleet)
   {
   for (Ipar in 1:15)  
    if (EM.obj.org$size_selex_types.blocks.no[Ifleet,Ipar] > 0)
     {
      Iblock <- EM.obj.org$size_selex_types.blocks.no[Ifleet,Ipar]
      Npars.block <- EM.obj.org$nblocks.per.block[Iblock]
      for (Jpar in 1:Npars.block)
       {
        ParV <- EM.obj.org$size.sel.dev.blocks.vals[Ipoint]
        PhsV <- EM.obj.org$size.sel.dev.blocks.phs[Ipoint]
        Block.devs[Ifleet,1,Ipar,Jpar] <- ParV
        Block.phs[Ifleet,1,Ipar,Jpar] <- PhsV
        Ipoint <- Ipoint + 1
       }
     } # Selex
    for (Ipar in 1:15)  
     if (EM.obj.org$size_retain_types.blocks.no[Ifleet,Ipar] > 0)
      {
       Iblock <- EM.obj.org$size_retain_types.blocks.no[Ifleet,Ipar]
       Npars.block <- EM.obj.org$nblocks.per.block[Iblock]
       for (Jpar in 1:Npars.block)
        {
         ParV <- EM.obj.org$size.sel.dev.blocks.vals[Ipoint]
         PhsV <- EM.obj.org$size.sel.dev.blocks.phs[Ipoint]
         Block.devs[Ifleet,2,Ipar,Jpar] <- ParV
         Block.phs[Ifleet,2,Ipar,Jpar] <- PhsV
         Ipoint <- Ipoint + 1
        }
      } # Retenson
    } # Fleets x parameters

  EM.obj$size.sel.dev.blocks.vals <- NULL
  EM.obj$size.sel.dev.blocks.phs <- NULL
  for (Imet in 1:Nmetiers)
   if (SelOrigEst[Imet]==0)
    {
     for (Ipar in 1:15)
      for (Jpar in 1:100)
       if (!is.na(Block.devs[SelOrig[Imet],1,Ipar,Jpar]))
        {
         EM.obj$size.sel.dev.blocks.vals <- c(EM.obj$size.sel.dev.blocks.vals,Block.devs[SelOrig[Imet],1,Ipar,Jpar])
         EM.obj$size.sel.dev.blocks.phs <- c(EM.obj$size.sel.dev.blocks.phs,Block.phs[SelOrig[Imet],1,Ipar,Jpar])
       }
     for (Ipar in 1:15)
      for (Jpar in 1:100)
       if (!is.na(Block.devs[SelOrig[Imet],2,Ipar,Jpar]))
        {
         EM.obj$size.sel.dev.blocks.vals <- c(EM.obj$size.sel.dev.blocks.vals,Block.devs[SelOrig[Imet],2,Ipar,Jpar])
         EM.obj$size.sel.dev.blocks.phs <- c(EM.obj$size.sel.dev.blocks.phs,Block.phs[SelOrig[Imet],2,Ipar,Jpar])
        }
   }

  # selectivity (size)
  EM.obj$size_selex_types.pattern    <- rep(0,Nmetiers)
  EM.obj$size_selex_types.special    <- rep(0,Nmetiers)
  EM.obj$size_selex_types.male       <- rep(0,Nmetiers)
  EM.obj$size_selex_types.sel.pars  <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size_selex_types.sel.phase <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size_selex_types.blocks.no <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size_selex_types.blocks.fn <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size.sel.ann.devs.use <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size.sel.ann.devs.miny <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size.sel.ann.devs.maxy <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size.sel.ann.devs.phs <-  matrix(0,nrow=Nmetiers,ncol=15)

  for (Imet in 1:Nmetiers) 
   if (SelOrigEst[Imet]==0)  
    {
     EM.obj$size_selex_types.pattern[Imet]    <- EM.obj.org$size_selex_types.pattern[SelOrig[Imet]]
     EM.obj$size_selex_types.special[Imet]    <- EM.obj.org$size_selex_types.special[SelOrig[Imet]]
     EM.obj$size_selex_types.male[Imet]       <- EM.obj.org$size_selex_types.male[SelOrig[Imet]]
     EM.obj$size_selex_types.sel.pars[Imet,]  <- EM.obj.org$size_selex_types.sel.pars[SelOrig[Imet],]
     EM.obj$size_selex_types.sel.phase[Imet,] <- EM.obj.org$size_selex_types.sel.phase[SelOrig[Imet],]
     EM.obj$size_selex_types.blocks.no[Imet,] <- EM.obj.org$size_selex_types.blocks.no[SelOrig[Imet],]
     EM.obj$size_selex_types.blocks.fn[Imet,] <- EM.obj.org$size_selex_types.blocks.fn[SelOrig[Imet],]
     EM.obj$size.sel.ann.devs.use[Imet,] <- EM.obj.org$size.sel.ann.devs.use[SelOrig[Imet],]
     EM.obj$size.sel.ann.devs.miny[Imet,] <- EM.obj.org$size.sel.ann.devs.miny[SelOrig[Imet],]
     EM.obj$size.sel.ann.devs.maxy[Imet,] <- EM.obj.org$size.sel.ann.devs.maxy[SelOrig[Imet],]
     EM.obj$size.sel.ann.devs.phs[Imet,] <- EM.obj.org$size.sel.ann.devs.phs[SelOrig[Imet],]
     
   }
   else
    {
     EM.obj$size_selex_types.pattern[Imet]    <- 15
     EM.obj$size_selex_types.special[Imet]    <- SelOrigEst[Imet]
     EM.obj$size_selex_types.male[Imet]       <- 0
     EM.obj$size_selex_types.sel.pars[Imet,]  <- 0
     EM.obj$size_selex_types.sel.phase[Imet,] <- 0
     EM.obj$size_selex_types.blocks.no[Imet,] <- 0
     EM.obj$size_selex_types.blocks.fn[Imet,] <- 0
     EM.obj$size.sel.ann.devs.use[Imet,] <- 0
     EM.obj$size.sel.ann.devs.miny[Imet,] <- 0
     EM.obj$size.sel.ann.devs.maxy[Imet,] <- 0
     EM.obj$size.sel.ann.devs.phs[Imet,] <- 0
    }

  # retention
  EM.obj$size_retain_types.pattern    <- rep(0,Nmetiers)
  EM.obj$size_retain_types.ret.pars  <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size_retain_types.ret.phase <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size_retain_types.sel.phase <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size_retain_types.blocks.no <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size_retain_types.blocks.fn <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size_retain_types.mort.pars <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size_retain_types.mort.phase <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size.ret.ann.devs.use <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size.ret.ann.devs.miny <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size.ret.ann.devs.maxy <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$size.ret.ann.devs.phs <-  matrix(0,nrow=Nmetiers,ncol=15)
  
  for (Imet in 1:Nmetiers) 
   if (SelOrigEst[Imet]==0)  
    {
      EM.obj$size_retain_types.pattern[Imet]    <- EM.obj.org$size_retain_types.pattern[SelOrig[Imet]]
      if (is.na(EM.obj$size_retain_types.pattern[Imet] )) EM.obj$size_retain_types.pattern[Imet] <- 0
      EM.obj$size_retain_types.ret.pars[Imet,]  <- EM.obj.org$size_retain_types.ret.pars[SelOrig[Imet],]
      EM.obj$size_retain_types.ret.phase[Imet,] <- EM.obj.org$size_retain_types.ret.phase[SelOrig[Imet],]
      EM.obj$size_retain_types.mort.pars[Imet,]  <- EM.obj.org$size_retain_types.mort.pars[SelOrig[Imet],]
      EM.obj$size_retain_types.mort.phase[Imet,] <- EM.obj.org$size_retain_types.mort.phase[SelOrig[Imet],]
      EM.obj$size_retain_types.blocks.no[Imet,] <- EM.obj.org$size_retain_types.blocks.no[SelOrig[Imet],]
      EM.obj$size_retain_types.blocks.fn[Imet,] <- EM.obj.org$size_retain_types.blocks.fn[SelOrig[Imet],]
      EM.obj$size.ret.ann.devs.use[Imet,] <- EM.obj.org$size.ret.ann.devs.use[SelOrig[Imet],]
      EM.obj$size.ret.ann.devs.miny[Imet,] <- EM.obj.org$size.ret.ann.devs.miny[SelOrig[Imet],]
      EM.obj$size.ret.ann.devs.maxy[Imet,] <- EM.obj.org$size.ret.ann.devs.maxy[SelOrig[Imet],]
      EM.obj$size.ret.ann.devs.phs[Imet,] <- EM.obj.org$size.ret.ann.devs.phs[SelOrig[Imet],]
   }
  else
   {
    EM.obj$size_retain_types.pattern[Imet]    <- -1*SelOrigEst[Imet]
    EM.obj$size_retain_types.ret.pars[Imet,]  <- 0
    EM.obj$size_retain_types.ret.phase[Imet,] <- 0
    EM.obj$size_retain_types.blocks.no[Imet,] <- 0
    EM.obj$size_retain_types.blocks.fn[Imet,] <- 0
    EM.obj$size_retain_types.mort.pars[Imet,]  <- 0
    EM.obj$size_retain_types.mort.phase[Imet,] <- 0
    EM.obj$size.ret.ann.devs.use[Imet,] <- 0
    EM.obj$size.ret.ann.devs.miny[Imet,] <- 0
    EM.obj$size.ret.ann.devs.maxy[Imet,] <- 0
    EM.obj$size.ret.ann.devs.phs[Imet,] <- 0
    
   }

  Block.devs <- array(NA,dim=c(N.orig.fleet,2,15,100))
  Block.phs <- array(NA,dim=c(N.orig.fleet,2,15,100))
  Ipoint <- 1
  for (Ifleet in 1:N.orig.fleet)
  {
    for (Ipar in 1:15)  
      if (EM.obj.org$age_selex_types.blocks.no[Ifleet,Ipar] > 0)
      {
        Iblock <- EM.obj.org$age_selex_types.blocks.no[Ifleet,Ipar]
        Npars.block <- EM.obj.org$nblocks.per.block[Iblock]
        for (Jpar in 1:Npars.block)
        {
          ParV <- EM.obj.org$age.sel.dev.blocks.vals[Ipoint]
          PhsV <- EM.obj.org$age.sel.dev.blocks.phs[Ipoint]
          Block.devs[Ifleet,1,Ipar,Jpar] <- ParV
          Block.phs[Ifleet,1,Ipar,Jpar] <- PhsV
          Ipoint <- Ipoint + 1
        }
      } # Selex
    for (Ipar in 1:15)  
      if (EM.obj.org$age_retain_types.blocks.no[Ifleet,Ipar] > 0)
      {
        Iblock <- EM.obj.org$age_retain_types.blocks.no[Ifleet,Ipar]
        Npars.block <- EM.obj.org$nblocks.per.block[Iblock]
        for (Jpar in 1:Npars.block)
        {
          ParV <- EM.obj.org$age.sel.dev.blocks.vals[Ipoint]
          PhsV <- EM.obj.org$age.sel.dev.blocks.phs[Ipoint]
          Block.devs[Ifleet,2,Ipar,Jpar] <- ParV
          Block.phs[Ifleet,2,Ipar,Jpar] <- PhsV
          Ipoint <- Ipoint + 1
        }
      } # Retenson
  } # Fleets x parameters
  
  EM.obj$age.sel.dev.blocks.vals <- NULL
  EM.obj$age.sel.dev.blocks.phs <- NULL
  for (Imet in 1:Nmetiers)
    if (SelOrigEst[Imet]==0)
    {
      for (Ipar in 1:15)
        for (Jpar in 1:100)
          if (!is.na(Block.devs[SelOrig[Imet],1,Ipar,Jpar]))
          {
            EM.obj$age.sel.dev.blocks.vals <- c(EM.obj$age.sel.dev.blocks.vals,Block.devs[SelOrig[Imet],1,Ipar,Jpar])
            EM.obj$age.sel.dev.blocks.phs <- c(EM.obj$age.sel.dev.blocks.phs,Block.phs[SelOrig[Imet],1,Ipar,Jpar])
          }
      for (Ipar in 1:15)
        for (Jpar in 1:100)
          if (!is.na(Block.devs[SelOrig[Imet],2,Ipar,Jpar]))
          {
            EM.obj$age.sel.dev.blocks.vals <- c(EM.obj$age.sel.dev.blocks.vals,Block.devs[SelOrig[Imet],2,Ipar,Jpar])
            EM.obj$age.sel.dev.blocks.phs <- c(EM.obj$age.sel.dev.blocks.phs,Block.phs[SelOrig[Imet],2,Ipar,Jpar])
          }
    }

  # selectivity (age)
  EM.obj$age_selex_types.pattern    <- rep(0,Nmetiers)
  EM.obj$age_selex_types.special    <- rep(0,Nmetiers)
  EM.obj$age_selex_types.male       <- rep(0,Nmetiers)
  EM.obj$age_selex_types.sel.pars  <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age_selex_types.sel.phase <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age_selex_types.blocks.no <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age_selex_types.blocks.fn <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age.sel.ann.devs.use <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age.sel.ann.devs.miny <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age.sel.ann.devs.maxy <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age.sel.ann.devs.phs <-  matrix(0,nrow=Nmetiers,ncol=15)
  for (Imet in 1:Nmetiers) 
    if (SelOrigEst[Imet]==0)  
    {
      EM.obj$age_selex_types.pattern[Imet]    <- EM.obj.org$age_selex_types.pattern[SelOrig[Imet]]
      EM.obj$age_selex_types.special[Imet]    <- EM.obj.org$age_selex_types.special[SelOrig[Imet]]
      EM.obj$age_selex_types.male[Imet]       <- EM.obj.org$age_selex_types.male[SelOrig[Imet]]
      EM.obj$age_selex_types.sel.pars[Imet,]  <- EM.obj.org$age_selex_types.sel.pars[SelOrig[Imet],]
      EM.obj$age_selex_types.sel.phase[Imet,] <- EM.obj.org$age_selex_types.sel.phase[SelOrig[Imet],]
      EM.obj$age_selex_types.blocks.no[Imet,] <- EM.obj.org$age_selex_types.blocks.no[SelOrig[Imet],]
      EM.obj$age_selex_types.blocks.fn[Imet,] <- EM.obj.org$age_selex_types.blocks.fn[SelOrig[Imet],]
      EM.obj$age.sel.ann.devs.use[Imet,] <- EM.obj.org$age.sel.ann.devs.use[SelOrig[Imet],]
      EM.obj$age.sel.ann.devs.miny[Imet,] <- EM.obj.org$age.sel.ann.devs.miny[SelOrig[Imet],]
      EM.obj$age.sel.ann.devs.maxy[Imet,] <- EM.obj.org$age.sel.ann.devs.maxy[SelOrig[Imet],]
      EM.obj$age.sel.ann.devs.phs[Imet,] <- EM.obj.org$age.sel.ann.devs.phs[SelOrig[Imet],]
      
    }
  else
  {
    EM.obj$age_selex_types.pattern[Imet]    <- 15
    EM.obj$age_selex_types.special[Imet]    <- SelOrigEst[Imet]
    EM.obj$age_selex_types.male[Imet]       <- 0
    EM.obj$age_selex_types.sel.pars[Imet,]  <- 0
    EM.obj$age_selex_types.sel.phase[Imet,] <- 0
    EM.obj$age_selex_types.blocks.no[Imet,] <- 0
    EM.obj$age_selex_types.blocks.fn[Imet,] <- 0
    EM.obj$age.sel.ann.devs.use[Imet,] <- 0
    EM.obj$age.sel.ann.devs.miny[Imet,] <- 0
    EM.obj$age.sel.ann.devs.maxy[Imet,] <- 0
    EM.obj$age.sel.ann.devs.phs[Imet,] <- 0
  }

  # retention
  EM.obj$age_retain_types.pattern    <- rep(0,Nmetiers)
  EM.obj$age_retain_types.ret.pars  <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age_retain_types.ret.phase <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age_retain_types.sel.phase <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age_retain_types.blocks.no <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age_retain_types.blocks.fn <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age_retain_types.mort.pars <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age_retain_types.mort.phase <- matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age.ret.ann.devs.use <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age.ret.ann.devs.miny <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age.ret.ann.devs.maxy <-  matrix(0,nrow=Nmetiers,ncol=15)
  EM.obj$age.ret.ann.devs.phs <-  matrix(0,nrow=Nmetiers,ncol=15)
  
  for (Imet in 1:Nmetiers) 
    if (SelOrigEst[Imet]==0)  
    {
      EM.obj$age_retain_types.pattern[Imet]    <- EM.obj.org$age_retain_types.pattern[SelOrig[Imet]]
      if (is.na(EM.obj$age_retain_types.pattern[Imet] )) EM.obj$age_retain_types.pattern[Imet] <- 0
      EM.obj$age_retain_types.ret.pars[Imet,]  <- EM.obj.org$age_retain_types.ret.pars[SelOrig[Imet],]
      EM.obj$age_retain_types.ret.phase[Imet,] <- EM.obj.org$age_retain_types.ret.phase[SelOrig[Imet],]
      EM.obj$age_retain_types.mort.pars[Imet,]  <- EM.obj.org$age_retain_types.mort.pars[SelOrig[Imet],]
      EM.obj$age_retain_types.mort.phase[Imet,] <- EM.obj.org$age_retain_types.mort.phase[SelOrig[Imet],]
      EM.obj$age_retain_types.blocks.no[Imet,] <- EM.obj.org$age_retain_types.blocks.no[SelOrig[Imet],]
      EM.obj$age_retain_types.blocks.fn[Imet,] <- EM.obj.org$age_retain_types.blocks.fn[SelOrig[Imet],]
      EM.obj$age.ret.ann.devs.use[Imet,] <- EM.obj.org$age.ret.ann.devs.use[SelOrig[Imet],]
      EM.obj$age.ret.ann.devs.miny[Imet,] <- EM.obj.org$age.ret.ann.devs.miny[SelOrig[Imet],]
      EM.obj$age.ret.ann.devs.maxy[Imet,] <- EM.obj.org$age.ret.ann.devs.maxy[SelOrig[Imet],]
      EM.obj$age.ret.ann.devs.phs[Imet,] <- EM.obj.org$age.ret.ann.devs.phs[SelOrig[Imet],]
    }
  else
  {
    EM.obj$age_retain_types.pattern[Imet]    <- -1*SelOrigEst[Imet]
    EM.obj$age_retain_types.ret.pars[Imet,]  <- 0
    EM.obj$age_retain_types.ret.phase[Imet,] <- 0
    EM.obj$age_retain_types.blocks.no[Imet,] <- 0
    EM.obj$age_retain_types.blocks.fn[Imet,] <- 0
    EM.obj$age_retain_types.mort.pars[Imet,]  <- 0
    EM.obj$age_retain_types.mort.phase[Imet,] <- 0
    EM.obj$age.ret.ann.devs.use[Imet,] <- 0
    EM.obj$age.ret.ann.devs.miny[Imet,] <- 0
    EM.obj$age.ret.ann.devs.maxy[Imet,] <- 0
    EM.obj$age.ret.ann.devs.phs[Imet,] <- 0
    
  }
 
  # ============================================================================================================
  
  OM.obj$nfleets <- Nmetiers
  #OM.obj$nfleet.catch <- length(which(Base %in% 1:OM.obj$nfleet.catch))
  print("Saving EM")
  write.out.EM(OM.obj,EM.obj,RunFolder2,Control.Rule.Type,Byproduct.species=PGMSY.Species)
  
  # General file
  print("Saving General")
  write.out.Gen(OM.obj,RunFolder4,RunType=RunType,Reduced.Uncertain=Reduce.Uncertain)
  
}  
  
# ===========================================================================================================================================
# ===========================================================================================================================================

DO.Create <- function(Species.Names,FileName)
{
 Reduce.Uncertain <- 0; Nproj <- 50; Control.Rule.Type <<- "CSIRO"
 RunType <<- "Not Test"
 RunType <<- "Test"

 Metiers <- read.csv(paste0(BasePath,FileName),head=F)
 #print(Metiers)

 # Name of this run
 RunName <- Metiers[1,2]
 # Number of species/stocks
 Nspecies <- as.numeric(Metiers[2,2]);
 # Number of metiers
 N.metiers.final <- as.numeric(Metiers[3,2]); 
 # How many fleets
 N.catch.metiers.final <- as.numeric(Metiers[4,2]);
 # names of metiers
 MetierNames <- Metiers[5,1+1:N.metiers.final];
 # WHich ones have TACS
 WithTACs <- Metiers[6,1+1:N.metiers.final]
 print(WithTACs)
 PGMSY.Primary.Species.by.fleet <- as.numeric(Metiers[7,1+1:N.catch.metiers.final]);
 
 PGMSY.Primary.Species <- as.numeric(Metiers[9,1+1:Nspecies]);
 Indicator.Primary.Species <- as.numeric(Metiers[10,1+1:Nspecies]);
 Ipnt <- 13

 Spec.Names <- rep(NA,Nspecies)
 for (Ispecies in 1:Nspecies)
  {
   # Identify the species and the 
   Ispec <- as.numeric(Metiers[Ipnt,2]); Ipnt <- Ipnt + 1
   N.metiers.spec <- as.numeric(Metiers[Ipnt,2]); Ipnt <- Ipnt + 1
   N.catch.metiers.spec <- as.numeric(Metiers[Ipnt,2]); Ipnt <- Ipnt + 1
   Last.TAC.spec <- as.numeric(Metiers[Ipnt,2:4]); Ipnt <- Ipnt + 1
   Last.TAC.specs <<- rbind(Last.TAC.specs,Last.TAC.spec)
   print(Last.TAC.specs)
   Assess.Freq <- as.numeric(Metiers[Ipnt,2]); Ipnt <- Ipnt + 1
   Assessment.Frequency <- c(Assessment.Frequency,Assess.Freq)
   Ipnt <- Ipnt + 1
   Species <- Species.Names[Ispec]
   Species2 <- gsub(" ",".",Species)
   print(Species2)
   Spec.Names[Ispecies] <- Species2
   
   # Check all is OK with specifications for multispecies run
   if (PGMSY.Primary.Species[Ispecies]>0 & Indicator.Primary.Species[Ispecies]>0) { cat(paste0("Species ",Species," cannot be both PGMSY and Indicator\n")); stop()} 
   
   # Decide if this a byproduct species
   Is.PGMSY.Species <- "No"
   if (PGMSY.Primary.Species[Ispecies]>0) Is.PGMSY.Species <- "Yes"

   # Load the files created for the single species version
   RunFolder2a <- paste0(InputPath,Species2,".OM.Sav")
   RunFolder3a <- paste0(InputPath,Species2,".EM.Sav")
   load(file=RunFolder2a)
   OM.obj1 <-  OM.obj
   OM.obj1$fleetnames <- MetierNames
   #print(str(OM.obj1))
   load(file=RunFolder3a)
   EM.obj1 <-  EM.obj
   #print(str(EM.obj1))
   print("loaded")

   # Links between fleets in the single species MSE and those in the multispecies MSE
   Base <- rep(0,N.metiers.spec); for (Imet in 1:N.metiers.spec) Base[Imet] <- as.numeric(Metiers[Ipnt,1+Imet]); Ipnt <- Ipnt + 2
   SelOrig <- rep(0,N.metiers.final); for (Imet in 1:N.metiers.final) SelOrig[Imet] <- as.numeric(Metiers[Ipnt,1+Imet]); Ipnt <- Ipnt + 1
   SelOrigEst <- rep(0,N.metiers.final); for (Imet in 1:N.metiers.final) SelOrigEst[Imet] <- as.numeric(Metiers[Ipnt,1+Imet]); Ipnt <- Ipnt + 1
   DataOrig <- rep(0,N.metiers.final); for (Imet in 1:N.metiers.final) DataOrig[Imet] <- as.numeric(Metiers[Ipnt,1+Imet]); Ipnt <- Ipnt + 1 
   IndexOrig <- rep(0,N.metiers.final); for (Imet in 1:N.metiers.final) IndexOrig[Imet] <- as.numeric(Metiers[Ipnt,1+Imet]); Ipnt <- Ipnt + 1 
   
   # Now link fleets to metiers
   Fleet.to.Metier <- matrix(0,nrow=N.catch.metiers.spec,ncol=N.catch.metiers.final)
   for (Ifleet in 1:N.catch.metiers.spec)
    for (Imet in 1:N.catch.metiers.final)
     Fleet.to.Metier[Ifleet,Imet] <- as.numeric(Metiers[Ipnt+Ifleet-1,1+Imet])
   print(Fleet.to.Metier)
 
   cat("Processing\n")
   Base <- NULL
   OM.obj <- do.process.OM(OM.obj=OM.obj,Fleet.to.Metier=Fleet.to.Metier,
                           Nmetiers=N.metiers.final,Nmetiers.catch=N.catch.metiers.final,
                           FileName1=paste0(Species2,".OM"),MetierNames=MetierNames,
                           SelOrig=SelOrig,DataOrig=DataOrig,IndexOrig=IndexOrig)
   do.process.EM(OM.obj1,EM.obj,Fleet.to.Metier=Fleet.to.Metier,
                 Nmetiers=N.metiers.final,Nmetiers.catch=N.catch.metiers.final,
                 FileName1=paste0(Species2,".EM"),
                 FileName3=paste0("General_",Species2,".OM"),
                 SelOrig=SelOrig,DataOrig=DataOrig,
                 SelOrigEst=SelOrigEst,PGMSY.Species=Is.PGMSY.Species)
  
   Ipnt <- Ipnt + N.catch.metiers.spec
   cat("Done:", Species2,Ipnt,"\n")

   InputFolder2a <- paste0(InputPath,Species2,".OM.Sav")
   InputFolder3a <- paste0(InputPath,Species2,".EM.Sav")
   OutputFolder2a <- paste0(OutputPath,Species2,".OM.Sav")
   OutputFolder3a <- paste0(OutputPath,Species2,".EM.Sav")
   file.copy(InputFolder2a,OutputFolder2a)
   file.copy(InputFolder3a,OutputFolder3a)
   
   }
 RunFolder4 <- paste0(OutputPath,paste0("General_",RunName,".OM"))
 print(RunFolder4)

 #print(str(OM.obj))
 write.out.Gen2(OM.obj,RunFolder4,RunType=RunType,Spec.Names, MetierNames=MetierNames,WithTACs=WithTACs,Yr1=2024,
                Reduced.Uncertain=Reduce.Uncertain,N.catch.metiers=N.catch.metiers.final,
                PGMSY.Primary.Species.by.fleet=PGMSY.Primary.Species.by.fleet,
                PGMSY.Primary.Species=PGMSY.Primary.Species,
                Indicator.Primary.Species=Indicator.Primary.Species,
                Last.TAC.specs,Assessment.Frequency)
 
 
}
Last.TAC.specs <- NULL
Assessment.Frequency <- NULL

BasePath <- "C:/Research/NewRat/Newrat_spat/"
InputPath <- "C:/Research/NewRat/Newrat_spat/Inputs CSIRO/"
OutputPath <- "C:/Research/NewRat/Newrat_spat/Inputs CSIROM/"
Species.Names <- c("Bight redfish","Blue grenadier","Deepwater flathead","Morwong","Orange roughy east","Pink ling","Redfish","School whiting","Silver warehou","Tiger flathead","Mirror dory")
DO.Create(Species.Names,"fleet-metierv0.csv")
#DO.Create(Species.Names,"fleet-metierv1.csv")

BasePath <- "C:/Research/NewRat/Newrat_spat/"
InputPath <- "C:/Research/NewRat/Newrat_spat/Inputs Other/"
OutputPath <- "C:/Research/NewRat/Newrat_spat/Inputs CaitLinM/"
Species.Names <- c("Sardine","Mackerel","Anchovy","Squid")
#DO.Create(Species.Names,"Caitlin.csv")

BasePath <- "C:/Research/NewRat/Newrat_spat/"
InputPath <- "C:/Research/NewRat/Newrat_spat/Inputs Other/"
OutputPath <- "C:/Research/NewRat/Newrat_spat/Inputs OtherM/"
Species.Names <- c("Gummy","Dusky","Whiskery_Shark","Sandbar_shark")
#DO.Create(Species.Names,"WASharks.csv")
