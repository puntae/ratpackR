# =======================================================================================================================
DataGen <- function(Istock,Year,BioPreds)
{
  # This function generates data for one year
  
  Nsex <- Stock[[Istock]]$Nsex
  MaxAge <- Stock[[Istock]]$MaxAge
  Nlen <- Stock[[Istock]]$Nlen
  YrOffset <- Stock[[Istock]]$YrOffset
  NassArea <- Stock[[Istock]]$NassArea
  FleetsToAreas <- Stock[[Istock]]$FleetsToAreas
  Nfleet <- General$Nfleet
  Ncat_fleet<-  General$Ncat_fleet
  
  # Landed catches (Assumed know exactly)
  for (IassArea in 1:NassArea)
  {
    Use <- which(FleetsToAreas==IassArea)
    for (Ifleet in 1:General$Ncat_fleet)  
     {
      Stock[[Istock]]$Data$Catches[IassArea,Ifleet,Year] <<- sum(Stock[[Istock]]$CatchRetained[Use,Ifleet,Year]) 
      if (TestCase==F) Stock[[Istock]]$Data$Catches[IassArea,Ifleet,Year] <<- Stock[[Istock]]$Data$Catches[IassArea,Ifleet,Year]*Stock[[Istock]]$Data$Catch.Bias[IassArea,Year,Ifleet]*exp(Stock[[Istock]]$Data$Catch.devs[IassArea,Year,Ifleet])
    }
   } # IassArea

  # Index data
  for (IassArea in 1:NassArea)
    for (Ifleet in 1:Nfleet)
      if (Stock[[Istock]]$Data$Index.spec[IassArea,Year,Ifleet] > 0)
      {
        Use <- which(FleetsToAreas==IassArea)
        TrueBiomass <- sum(BioPreds[Use,Ifleet]) 
        if (TrueBiomass > 0)
         {
          Stock[[Istock]]$Data$NindexData[IassArea] <<- Stock[[Istock]]$Data$NindexData[IassArea]+1
          IndexNo <- Stock[[Istock]]$Data$NindexData[IassArea]
          Stock[[Istock]]$Data$IndexData[IassArea,IndexNo,1:3] <<- c(Year+YrOffset,7,Ifleet)
          TrueBiomass <- (TrueBiomass)^Stock[[Istock]]$Data$Index.Beta[Ifleet]
           if (Year >= Stock[[Istock]]$Data$Index.Start.Qinc[Ifleet])
            TrueBiomass <- TrueBiomass * exp(Stock[[Istock]]$Data$Index.rate.Qinc[Ifleet]*(Year-Stock[[Istock]]$Data$Index.Start.Qinc[Ifleet]))
          if (TestCase==F)  Stock[[Istock]]$Data$IndexData[IassArea,IndexNo,4]   <<- Stock[[Istock]]$Data$Index.q[Ifleet]*TrueBiomass*exp(Stock[[Istock]]$Data$Index.dev[IassArea,Year,Ifleet])
          if (TestCase==T || Deter.Index.data == T)  Stock[[Istock]]$Data$IndexData[IassArea,IndexNo,4]   <<- Stock[[Istock]]$Data$Index.q[Ifleet]*TrueBiomass
          Stock[[Istock]]$Data$IndexData[IassArea,IndexNo,5]   <<- Stock[[Istock]]$Data$Index.spec[IassArea,Year,Ifleet]*Stock[[Istock]]$Data$Index.CV.bias[Ifleet]
          Stock[[Istock]]$Data$IndexData[IassArea,IndexNo,6]   <<- IassArea
         }
      } # Index data
  
  # Discard data
  for (Ifleet in 1:Ncat_fleet)
    for (IassArea in 1:NassArea)
      if (Stock[[Istock]]$Data$Discard.spec[IassArea,Year,Ifleet] > 0  & Stock[[Istock]]$CatchTotal[IassArea,Ifleet,Year] > 1.0e-5)
       {
        Use <- which(FleetsToAreas==IassArea)
        #cat(Ifleet,Year,Stock[[Istock]]$Data$NdiscardData,"\n")
        Stock[[Istock]]$Data$NdiscardData <<- Stock[[Istock]]$Data$NdiscardData+1
        Discard <- Stock[[Istock]]$CatchTotal[IassArea,Ifleet,Year] - Stock[[Istock]]$CatchRetained[IassArea,Ifleet,Year]+1.0e-20
        if (Stock[[Istock]]$Data$Discard.type[Ifleet]==2) Discard <- Discard/(Stock[[Istock]]$CatchTotal[IassArea,Ifleet,Year]+1.0e-20)
        IndexNo <- Stock[[Istock]]$Data$NdiscardData
        Stock[[Istock]]$Data$DiscardData[IndexNo,1:3] <<- c(Year+YrOffset,7,Ifleet)
        if (TestCase==F) Stock[[Istock]]$Data$DiscardData[IndexNo,4]   <<- Discard*exp(Stock[[Istock]]$Data$Discard.dev[IassArea,Year,Ifleet])+1.0e-20
        if (TestCase==T || Deter.Discard.data == T) Stock[[Istock]]$Data$DiscardData[IndexNo,4]   <<- Discard+1.0e-20
        Stock[[Istock]]$Data$DiscardData[IndexNo,5]   <<- Stock[[Istock]]$Data$Discard.spec[IassArea,Year,Ifleet]
       } # ifleet (discard)
  
  # Length data
  for (IassArea in 1:NassArea)
    for (Itype in 1:3)
      for (Ifleet in 1:Nfleet)
        if (Stock[[Istock]]$Data$Length.spec[IassArea,Itype,Year,Ifleet] > 0)
        { 
          Use <- which(FleetsToAreas==IassArea)
          Expected <- matrix(0,nrow=Nsex,ncol=Nlen)
          for (Iarea in Use)
           {
            if (Itype==1) Expected <- Expected + Stock[[Istock]]$CatchAtLenRet[Iarea,Ifleet,,,Year]+1.0e-10
            if (Itype==2) Expected <- Expected + Stock[[Istock]]$CatchAtLenTot[Iarea,Ifleet,,,Year]-Stock[[Istock]]$CatchAtLenRet[Iarea,Ifleet,,,Year]+1.0e-10
            if (Itype==3) Expected <- Expected + Stock[[Istock]]$CatchAtLenTot[Iarea,Ifleet,,,Year]+1.0e-10
           }
          if (Nsex==2) Expected <- c(Expected[1,],Expected[2,])
          Expected <- Expected /sum(Expected)
          if (is.na(Expected[1])) cat("Error-length (NA unexpected)",Istock,Itype,Ifleet,Year,"\n")
          if (sum(Expected)<=0) 
            cat("Error-length",Istock,Itype,Ifleet,Year,"\n")
          else
          {
            Stock[[Istock]]$Data$NlengthData <<- Stock[[Istock]]$Data$NlengthData+1
            set.seed(Stock[[Istock]]$Data$Length.seeds[IassArea,Itype,Year,Ifleet])
            Nsamp <- Stock[[Istock]]$Data$Length.spec[IassArea,Itype,Year,Ifleet]
            Observed <- rmultinom(1,Nsamp,prob=Expected)
            Observed <- as.vector(Observed)
            if (TestCase==T || Deter.Length.data == T) Observed <- Nsamp*Expected
            if (Itype==1) Part <- 2; if (Itype==2) Part <- 1; if (Itype==3) Part <- 0;
            if (Nsex==1) Vec1 <- c(IassArea,Year+YrOffset,7,Ifleet,0,Part,Nsamp)
            if (Nsex==2) Vec1 <- c(IassArea,Year+YrOffset,7,Ifleet,3,Part,Nsamp)
            Vec1 <- c(Vec1,Observed)
            Stock[[Istock]]$Data$LengthData <<- rbind(Stock[[Istock]]$Data$LengthData,Vec1)
          } # Area (length)
        } # Ifleet (length)
  
  # Age data
  for (IassArea in 1:NassArea)
    for (Itype in 1:3)
      for (Ifleet in 1:Nfleet)
        if (Stock[[Istock]]$Data$Age.spec[IassArea,Itype,Year,Ifleet] > 0)
        {
          Use <- which(FleetsToAreas==IassArea)
          Expected <- matrix(0,nrow=Nsex,ncol=MaxAge)
          for (Iarea in Use)
          {
            if (Itype==1) Expected <- Expected + Stock[[Istock]]$CatchAtAgeRet[Iarea,Ifleet,,,Year]+1.0e-10
            if (Itype==2) Expected <- Expected + Stock[[Istock]]$CatchAtAgeTot[Iarea,Ifleet,,,Year]-Stock[[Iarea,Istock]]$CatchAtAgeRet[Ifleet,,,Year]+1.0e-10
            if (Itype==3) Expected <- Expected + Stock[[Istock]]$CatchAtAgeTot[Iarea,Ifleet,,,Year]+1.0e-10
          } 
          # Allow for ageing error
          if (Nsex==1) Expected <- as.vector(Stock[[Istock]]$AgeErrorMatrix[1,,] %*% Expected[1,])
          if (Nsex==2) Expected[1,] <- Stock[[Istock]]$AgeErrorMatrix[1,,] %*% Expected[1,]
          if (Nsex==2) Expected[2,] <- Stock[[Istock]]$AgeErrorMatrix[1,,] %*% Expected[2,]
          if (Nsex==2) Expected <- c(Expected[1,],Expected[2,])
          
          if (is.na(Expected[1])) cat("Error-Age (NA inexpected)",Istock,Itype,Ifleet,Year,"\n")
          if (sum(Expected)<=0) 
            cat("Error-Age",Istock,Itype,Ifleet,Year,"\n")
          else
          {
            Stock[[Istock]]$Data$NageData <<- Stock[[Istock]]$Data$NageData+1
            set.seed(Stock[[Istock]]$Data$Age.seeds[IassArea,Itype,Year,Ifleet])
            Nsamp <- Stock[[Istock]]$Data$Age.spec[IassArea,Itype,Year,Ifleet]
            Observed <- as.vector(rmultinom(1,Nsamp,prob=Expected))
            if (TestCase==T || Deter.Age.data == T) Observed <- Nsamp*Expected
            if (Itype==1) Part <- 2; if (Itype==2) Part <- 1; if (Itype==3) Part <- 0;
            if (Nsex==1) Vec1 <- c(IassArea,Year+YrOffset,7,Ifleet,0,Part,1,-1,-1,Nsamp)
            if (Nsex==2) Vec1 <- c(IassArea,Year+YrOffset,7,Ifleet,3,Part,1,-1,-1,Nsamp)
            Vec1 <- c(Vec1,Observed)
            Stock[[Istock]]$Data$AgeData <<- rbind(Stock[[Istock]]$Data$AgeData,Vec1)
          }
        } # Ifleet (age)
  
  # CAA data
  for (IassArea in 1:NassArea)
    for (Itype in 1:3)
      for (Ifleet in 1:Nfleet)
        if (Stock[[Istock]]$Data$CAA.spec[IassArea,Itype,Year,Ifleet] > 0)
        {
          if (Itype==1) Part <- 2; if (Itype==2) Part <- 1; if (Itype==3) Part <- 0;
          # First generate the length-class for which there are age data
          Use <- which(FleetsToAreas==IassArea)
          ExpectedV <- matrix(0,nrow=Nsex,ncol=Nlen)
          for (Iarea in Use)
          {
            if (Itype==1) ExpectedV <- ExpectedV + Stock[[Istock]]$CatchAtLenRet[Iarea,Ifleet,,,Year]+1.0e-10
            if (Itype==2) ExpectedV <- ExpectedV + Stock[[Istock]]$CatchAtLenTot[Iarea,Ifleet,,,Year]-Stock[[Istock]]$CatchAtLenRet[Iarea,Ifleet,,,Year]+1.0e-10
            if (Itype==3) ExpectedV <- ExpectedV + Stock[[Istock]]$CatchAtLenTot[Iarea,Ifleet,,,Year]+1.0e-10
          }
          for (Isex in 1:Nsex)
          {
            if (Nsex==1) Expected <- ExpectedV[1,]
            if (Nsex==2) Expected <- ExpectedV[Isex,]
            if (is.na(Expected[1])) cat("Error-CAA (NA inexpected)",Istock,Itype,Ifleet,Year,"\n")
            if (sum(Expected)<=0) 
              cat("Error-CAA1",Istock,Itype,Ifleet,Year,"\n")
            else
            {
              # Account for ageing error
              set.seed(Stock[[Istock]]$Data$Age.seeds[IassArea,Itype,Year,Ifleet])
              NsampLength <- Stock[[Istock]]$Data$CAA.spec[IassArea,Itype,Year,Ifleet]/2
              LenSampleSize <- as.vector(rmultinom(1,NsampLength,prob=Expected))
              
              # Now generate the conditional data
              for (Ilen in 1:length(LenSampleSize))
                if (LenSampleSize[Ilen] >= 1)
                {
                  if (Itype==1) Expected <- Stock[[Istock]]$CatchAtCAARet[Iarea,Ifleet,Isex,,Ilen,Year]+1.0e-10
                  if (Itype==2) Expected <- Stock[[Istock]]$CatchAtCAATot[Iarea,Ifleet,Isex,,Ilen,Year]-Stock[[Istock]]$CatchAtCAARet[Iarea,Ifleet,Isex,,Ilen,Year]+1.0e-10
                  if (Itype==3) Expected <- Stock[[Istock]]$CatchAtCAATot[Iarea,Ifleet,Isex,,Ilen,Year]+1.0e-10
                  Expected <- as.vector(Stock[[Istock]]$AgeErrorMatrix[1,,] %*% Expected)
                  if (sum(Expected)<=0) 
                    cat("Error-CAA2",Istock,Itype,Ifleet,Ilen,Year,"\n")
                  else
                  {
                    Nsamp <- LenSampleSize[Ilen]
                    Observed <- as.vector(rmultinom(1,Nsamp,prob=Expected))
                    if (TestCase==T || Deter.Age.data==T) Observed <- Nsamp*Expected
                    if (Nsex==1) Vec1 <- c(IassArea,Year+YrOffset,7,Ifleet,0,Part,1,Ilen,Ilen,Nsamp)
                    if (Nsex==2) Vec1 <- c(IassArea,Year+YrOffset,7,Ifleet,Isex,Part,1,Ilen,Ilen,Nsamp)
                    if (Nsex==1) Vec1 <- c(Vec1,Observed)
                    if (Nsex==2 & Isex==1) Vec1 <- c(Vec1,Observed,rep(0,MaxAge))
                    if (Nsex==2 & Isex==2) Vec1 <- c(Vec1,rep(0,MaxAge),Observed)
                    Stock[[Istock]]$Data$CAAData <<- rbind(Stock[[Istock]]$Data$CAAData,Vec1)
                  }
                } # Use data
            } # Ilen
          } # Isex
        } # Ifleet (CAA)
  
  Num.Pred <- Stock[[Istock]]$Num.Pred
  # Predator number data
  if (Num.Pred>0)
  {
    for (Ipred in 1:Num.Pred)
      if (Stock[[Istock]]$Data$Pred.no.spec[Year,Ipred]>0)  
      {
        Pred.nums <- Stock[[Istock]]$Pred.No[Year,Ipred]
        Vec1 <- c(Year+YrOffset,7,General$Nfleet+Ipred,0,0)
        if (TestCase==F) Vec1[4] <- Pred.nums*exp(Stock[[Istock]]$Data$Pred.no.dev[Year,Ipred])
        if (TestCase==T || Deter.Discard.data == T) Vec1[4] <- Pred.nums
        Vec1[5] <- Stock[[Istock]]$Data$Pred.no.spec[Year,Ipred]
        Stock[[Istock]]$Data$Pred.no.Data <<- rbind(Stock[[Istock]]$Data$Pred.no.Data,Vec1)
      }
  } # Predator number data
  
  # Predator consumption data
  if (Num.Pred>0)
  {
    for (Ipred in 1:Num.Pred)
      if (Stock[[Istock]]$Data$Consump.spec[Year,Ipred] > 0)
      {
        Comsump <-  Stock[[Istock]]$ConsumpTotal[Ipred,Year]
        Vec1 <- c(Year+YrOffset,7,General$Nfleet+Ipred,0,0)
        if (TestCase==F) Vec1[4] <- Comsump*exp(Stock[[Istock]]$Data$Consump.devs[Year,Ipred])
        if (TestCase==T || Deter.Discard.data == T) Vec1[4] <- Comsump
        Vec1[5] <- Stock[[Istock]]$Data$Consump.spec[Year,Ipred]
        Stock[[Istock]]$Data$Consump.Data <<- rbind(Stock[[Istock]]$Data$Consump.Data,Vec1)
      }
  } # Predator number data
  
  # Predator Age data
  if (Num.Pred>0)
  {
    for (Ipred in 1:Num.Pred)
      if (Stock[[Istock]]$Data$Pred.age.spec[Year,Ipred] > 0)
      { 
        Expected <- Stock[[Istock]]$CompumpAtAge[Ipred,,,Year]
        if (Nsex==2) Expected <- c(Expected[1,],Expected[2,])
        if (sum(Expected)<=0) 
          cat("Error-Pred-Age",Istock,Ipred,Year,"\n")
        else
        {
          set.seed(Stock[[Istock]]$Data$Pred.age.seeds[Year,Ipred])
          Nsamp <- Stock[[Istock]]$Data$Pred.age.spec[Year,Ipred]
          Observed <- as.vector(rmultinom(1,Nsamp,prob=Expected))
          if (TestCase==T || Deter.Age.data == T) Observed <- Nsamp*Expected
          if (Nsex==1) Vec1 <- c(Year+YrOffset,7,General$Nfleet+Ipred,0,0,1,-1,-1,Nsamp)
          if (Nsex==2) Vec1 <- c(Year+YrOffset,7,General$Nfleet+Ipred,3,0,1,-1,-1,Nsamp)
          Vec1 <- c(Vec1,Observed)
          Stock[[Istock]]$Data$PredAgeData <<- rbind(Stock[[Istock]]$Data$PredAgeData,Vec1)
        }
      }
  } # Ipred (age)
  
}
