setwd("C:/Research/NewRat/NewRat_tst")



# ========================================================================================================================================================

Fig1 <- function(Output,Istock,col,lab,Do.print=T)
{
 Index <- which(!is.na(Output[,col]))
 Output <- Output[Index,]
 Index <- which(Output[,1]==Istock)
 Output <- Output[Index,]
 MinY <- as.numeric(min(Output[,3]))
 MaxY <- as.numeric(max(Output[,3]))
 cat(MinY,MaxY,"\n")
 
 Yrs <- MinY:MaxY
 Nyear <- length(Yrs)
 quants <- matrix(0,nrow=5,ncol=Nyear)
 for (Iyear in 1:Nyear)
  {
   Index <- Output[,3] == MinY+Iyear-1
   quants[,Iyear] <- quantile(as.numeric(Output[Index,col]),prob=c(0.05,0.25,0.5,0.75,0.95))
 }
 if (Do.print==T) print(quants[,(Nyear-7):Nyear])
 #print(quants[,(Nyear-50):Nyear])
 plot(Yrs,quants[3,],xlab="Year",ylab=lab,ylim=c(0,max(quants)*1.2),type="l",cex.lab=1.5,cex.axis=1.5)
 xx <- c(Yrs,rev(Yrs))
 yy <- c(quants[1,],rev(quants[5,]))
 polygon(xx,yy,col="gray10")
 xx <- c(Yrs,rev(Yrs))
 yy <- c(quants[2,],rev(quants[4,]))
 polygon(xx,yy,col="gray90")
 lines(Yrs,quants[3,],col="black",lwd=2)
 abline(v=105,col="orange",lwd=4)
 print("done fig1")
}

# ========================================================================================================================================================
# ========================================================================================================================================================

Fig2 <- function(Output,OutputE,Istock,col,col2,year,lab,diff=F,YrOffset=-1,skip=0,YrOffset1)
{
  Index <- which(!is.na(Output[,col])) & Output[,1] == Istock
  Output <- Output[Index,]
  Nsim <- as.numeric(max(Output[,2]))
  MinY <- as.numeric(min(Output[,3]))
  MaxY <- as.numeric(max(Output[,3]))
  print(MaxY)
  NhistY <- 2024
  Nyear <- min(MaxY-MinY+1,NhistY-MinY-1+51)
  print(Nyear)
  OutputE <- OutputE[OutputE[,1] == Istock & OutputE[,4]==col2,]
  OutputE <- OutputE[OutputE[,1] == Istock & OutputE[,3]==year,]
  Index <- is.na(OutputE)
  OutputE[Index] <- 0

  Error <- matrix(0,nrow=MaxY,ncol=Nsim)
  for(Iyear in 1:Nyear)
   for (Isim in 1:Nsim)
    {
     Index <- Output[,2] == Isim & Output[,3] == Iyear+YrOffset
     True <- as.numeric(Output[Index,col])
     Index <- OutputE[,2] == Isim & OutputE[,3] == year
     Est <- as.numeric(OutputE[Index,4+Iyear+skip])
     if (diff==F) Error[Iyear,Isim] = (Est-True)/Est*100
     if (diff==T) Error[Iyear,Isim] = (Est-True)*100
    } 

  Yrs <- MinY:(MinY+Nyear-1)
  quants <- matrix(0,nrow=5,ncol=Nyear)
  for (Iyear in 1:Nyear)
   {
    quants[,Iyear] <- quantile(Error[Iyear,],prob=c(0.05,0.25,0.5,0.75,0.95),na.rm=T)
   }
  print(quants)
  ymax <- max(20,max(abs(quants))*1.2)
  print(ymax)
  plot(Yrs,quants[3,],xlab="Year",ylab=paste0(lab," Year =",year),ylim=c(-ymax,ymax),type="l",cex.lab=1.5,cex.axis=1.5)
  xx <- c(Yrs,rev(Yrs))
  yy <- c(quants[1,],rev(quants[5,]))
  polygon(xx,yy,col="gray10")
  xx <- c(Yrs,rev(Yrs))
  yy <- c(quants[2,],rev(quants[4,]))
  polygon(xx,yy,col="gray90")
  lines(Yrs,quants[3,],col="black",lwd=2)
  abline(v=YrOffset1+year-1,col="orange",lwd=4)
  abline(h=0,col="green",lwd=5)

}

# ========================================================================================================================================================
# ========================================================================================================================================================

Fig3 <- function(OutputP,Istock,Year,Nsex=1,True,Ext)
{
  par(mfrow=c(4,4),oma=c(2,3,2,2),mar=c(5,6,2,1))
  Index <- True[,1] == Istock
  True <- as.numeric(True[Index,-c(1:4)])
  Npars <- length(True)
  Heads <- c(rep("M",Nsex),rep("Len-A1",Nsex),rep("Len-A2",Nsex),rep("Len-Kappa",Nsex),rep("CV1",Nsex),rep("CV2",Nsex),rep("LogR0",1))
  Npars <- length(Heads)
  for (Ipar in 1:Npars)
  {
   Index <- which(OutputP[,1]==Istock & OutputP[,3]==Year)  
   Est <- OutputP[Index,-(1:4)]
   Est <- as.numeric(Est[,Ipar])
   Error <- Est - True[Ipar]
   quants <- quantile(Error,prob=c(0.05,0.5,0.95))/True[Ipar]*100
   cat(Ext,Istock,Year,Heads[Ipar],True[Ipar],round(quants,3),"\n")
   if (abs(quants[3])>1 | abs(quants[1])>1) cat(Ext,Istock,Year,Heads[Ipar],True[Ipar],round(quants,3),"\n")
   hist(Error,xlab=Heads[Ipar],ylab="Density",main="")
   abline(v=0,lwd=3,col="green")
  }
  

}
# ========================================================================================================================================================


DO.graphs <- function(Ext,Ext2,FitYrs=c(1,9,17,25,33,37),skip=0,YrOffset1=0,Ispec=Ispec)
{
 Output <<- read.table(paste0("Results/",Ext2,"Project.",Ext),skip=1,comment="?",col.names=paste0("C",1:200),fill=T,nrows=9)
 OutputE <<- read.table(paste0("Results/",Ext2,"Estimates.",Ext),skip=1,col.names=paste0("C",1:200),fill=T)
 OutputP <<- read.table(paste0("Results/",Ext2,"Parameters.",Ext),skip=1,col.names=paste0("C",1:200),fill=T)
 print(Output[1:8,1:4])

 Nspec <- as.numeric(Output[1,2])
 Nstock <- as.numeric(Output[2,2])
 Nfleet <- as.numeric(Output[3,2])
 Ncat_fleet <- as.numeric(Output[4,2])
 Nsex <- rep(0,Nstock)
 YrOffset <- rep(0,Nstock)
 for (Istock in 1:Nstock) Nsex[Istock] <- as.numeric(Output[5+(Istock-1)*3,2])
 for (Istock in 1:Nstock) YrOffset[Istock] <- as.numeric(Output[6+(Istock-1)*3,2])

 Output <<- read.table(paste0("Results/",Ext2,"Project.",Ext),skip=7+2*Nstock,comment="?",col.names=paste0("C",1:200),fill=T)
 print(head(Output))
 True <- OutputP[1:Nstock,]
 print(True)
 
 if (Graphs[1])
  {
   par(mfrow=c(3,Nstock),oma=c(2,3,2,2),mar=c(5,6,2,1))
   for (Istock in 1:Nstock)
    {
     print(Istock)
     Fig1(Output,Istock,col=3+Ncat_fleet*3+2+2+2,lab="Depletion (%)",Do.print=T)
     abline(h=48,lwd=2,col="green")
     abline(h=20,lwd=2,col="red")
    }
   for (Istock in 1:Nstock)
    {
     Fig1(Output,Istock,col=3+Ncat_fleet*3+2+1,lab="Retained catch (t)",Do.print=F)
    }
   for (Istock in 1:Nstock)
    {
     Fig1(Output,Istock,col=3+Ncat_fleet*3+2+2,lab="Total catch (t)",Do.print=F)
    }
  }

 if (Graphs[2])
  {
   for (Istock in 1:Nstock)
    {
     par(mfrow=c(3,2),oma=c(2,3,2,2),mar=c(5,6,2,1))
     for (Iy in FitYrs)
      Fig2(Output,OutputE,Istock,col=3+Ncat_fleet*3+2+2+1,col2=1,year=Iy,lab="SSB",YrOffset=YrOffset[Istock],skip=skip,YrOffset1=YrOffset1)
     par(mfrow=c(3,2),oma=c(2,3,2,2),mar=c(5,6,2,1))
     for (Iy in FitYrs)
      Fig2(Output,OutputE,Istock,col=3+Ncat_fleet*3+2+2+2,col2=2,year=Iy,lab="Depl",YrOffset=YrOffset[Istock],skip=skip,YrOffset1=YrOffset1)
     par(mfrow=c(3,2),oma=c(2,3,2,2),mar=c(5,6,2,1))
     for (Iy in FitYrs)
      Fig2(Output,OutputE,Istock,col=3+Ncat_fleet*3+2+2+4,col2=3,year=Iy,lab="rec_dev",diff=T,YrOffset=YrOffset[Istock],skip=skip,YrOffset1=YrOffset1)
    }
  }

 if (Graphs[3])
  {
   for (Iyr in 1:length(FitYrs))
    for (Istock in 1:Nstock)
    Fig3(OutputP,Istock,Year=FitYrs[Iyr],Nsex=Nsex[Istock],True,Ext)
  }
}

# =======================================================================================================================================

Graphs <- c(T,T,T)
DO.graphs("P.cod",Ext2="ResultsA2/",FitYrs=c(1,6,11,16,21,25),skip=0,YrOffset1=2024,Ispec=1)
#DO.graphs("Silver.warehou",FitYrs=c(1),skip=0,YrOffset1=2024,Ispec=1)
##for (Ispec in 1:2)
 #DO.graphs("Multi1",FitYrs=c(1,5,9,13,21,25),skip=0,YrOffset1=2024,Ispec=1)
