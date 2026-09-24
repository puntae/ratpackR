library(r4ss)
library(readxl)
BasePath <- "C:/Research/NewRat/newrat_tst/Inputs/"
setwd(BasePath)

RatpackPath <- "C:/Research/NewRat/newrat_tst/Results/"

#AssignmentPath <- "C:/Research/csiro/Species25/M analysis/Diagnostics/"
#setwd(AssignmentPath)

# =====================================================================================================================

do.check <- function(Species,Species2,FileName1,FileName2,FileName3,AssignmentPath,SuperDiag=T,Fleet=1)
{
  RunFolder <- paste0(AssignmentPath,Species)
  print(RunFolder)

  starter <- SS_readstarter(file.path(RunFolder, "starter.ss"),verbose=F)
  dat <<- SS_readdat(file.path(RunFolder, starter$datfile),verbose=F)
  ctl <<- SS_readctl(file.path(RunFolder, starter$ctlfile),datlist=dat)
  styr <- dat$styr
  endyr <- dat$endyr
  print(endyr)
  nyears <- endyr - styr + 1
  nyear1 <- endyr - styr + 1
  nages <- dat$Nages+1
  nsex <- dat$Nsexes
  nfleets <- dat$Nfleets
  nfleet.catch <- length(which(dat$fleetinfo$type==1))
  ages <- 0:dat$Nages
  
  rep.filename <- paste0("REPORT.SSO")
  comp.filename <- paste0("COMPREPORT.SSO")
  model0 <<- SS_output(dir=RunFolder,repfile=rep.filename,covar=F,verbose=F,printstat=F)
  nlen <- length(model0$lbinspop)
  timeseries <<- model0$timeseries[-c(1:2),]
 
  if (SuperDiag==F)
   {
    load("E://save.sav")
    print(str(ObjSave))
    ObjSave <<- ObjSave
    par(mfrow=c(4,4))
   for (Isex in 1:nsex)
     for (Ifleet in 1:nfleet.catch)
      {
       Index <- which(model0$ageselex$Factor=="Asel2" & model0$ageselex$Yr==endyr+1 & model0$ageselex$Sex==Isex& model0$ageselex$Fleet==Ifleet)
       True.selex <- model0$ageselex[Index,-c(1:7)] 
       plot(ages,True.selex)
       Est.selex <- ObjSave$SelAge[Ifleet,Isex,,1]
       lines(ages,Est.selex,col="red")
    
       Index <- which(model0$ageselex$Factor=="sel*ret*wt" & model0$ageselex$Yr==endyr+1 & model0$ageselex$Sex==Isex& model0$ageselex$Fleet==Ifleet)
       True.wght <- model0$ageselex[Index,-c(1:7)] 
       Est.wght <- ObjSave$SelRetWghtAge[Ifleet,Isex,,1]
       #Est.wght <- ObjSave$SelWghtAge[Ifleet,Isex,,1]
       plot(ages,True.wght)
       lines(ages,Est.wght,col="red")
      }
     AA
    }
  
  modelA <<- read.table(file=paste0(RatpackPath,"Project.",Species2),skip=8,head=T,comment="?")
 
  SSB.true <- timeseries$SpawnBio[1:nyear1]
  Rec.true <- timeseries$Recruit_0[1:nyears]
  if (Fleet==1) F.true <- timeseries$`F:_1`[1:nyears]
  if (Fleet==2) F.true <- timeseries$`F:_2`[1:nyears]
  
  years <- styr:(endyr+1)
  SSB.est <- modelA$SSB[1:nyear1]
  Rec.est <- modelA$Recruitment[1:nyears]
  if (Fleet==1) F.est <- modelA$Full_F_1[1:nyears]
  if (Fleet==2) F.est <- modelA$Full_F_2[1:nyears]
  
  years <- styr:(endyr)
  ymax <- max(SSB.true,SSB.est)*1.05
  plot(years,SSB.true,lwd=2,lty=1,type="l",ylim=c(0,ymax),col="red")
  points(years,SSB.est,pch=16,col="blue")
  max.err.SSB <- max(abs(SSB.est-SSB.true)/SSB.true)*100
  text(endyr,ymax*0.95,Species,adj=1,cex=1.5)
  #print(SSB.true)
  #print(SSB.est)
  
  years <- styr:(endyr)
  ymax <- max(Rec.true,Rec.est)*1.05
  plot(years,Rec.true,lwd=2,lty=1,type="l",ylim=c(0,ymax),col="red")
  points(years,Rec.est,pch=16,col="blue")
  max.err.Rec <- max(abs(Rec.est-Rec.true)/Rec.true)*100
  
  years <- styr:(endyr)
  ymax <- max(F.true,F.est)*1.05
  plot(years,F.true,lwd=2,lty=1,type="l",ylim=c(0,ymax),col="red")
  points(years,F.est,pch=16,col="blue")
  #print(F.true)
  #print(F.est)
  cat(Species2,max.err.SSB,max.err.Rec,"\n")

}

AssignmentPath1 <- "C:/Research/csiro/Species25/M analysis/Diagnostics/"
AssignmentPath2 <- "C:/Research/NewRat/"


Png <- F
par(mfrow=c(5,3),oma=c(5,5,5,5),mar=c(5,4,2,1))
#for (Ispec in c(1,2,3,4,5,6,7,8,9,10))
#for (Ispec in c(1:10))
#for (Ispec in c(1, 3:10,12,13:14))
#for (Ispec in c(1,3:15))
for (Ispec in c(1,2,3:15))
#for (Ispec in c(15))
    {
   if (Png==T & Ispec==1) png(filename="C:/Research/NewRat/newrat_tst/Check1.png",height=1000,width=800)  
   if (Png==T & Ispec==6) png(filename="C:/Research/NewRat/newrat_tst/Check2.png",height=1000,width=800)  
   #if (Ispec %in% c(1,6)) par(mfrow=c(5,3),oma=c(5,5,5,5),mar=c(5,4,2,1))
    
  if (Ispec==1)  { Species <- "Bight redfish"; RunFolder <- "Bight redfish Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }  
  if (Ispec==2)  { Species <- "Blue grenadier"; RunFolder <- "Blue grenadier Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==3)  { Species <- "Deepwater flathead"; RunFolder <- "Deepwater flathead Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==4)  { Species <- "Morwong"; RunFolder <- "Morwong Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==5)  { Species <- "Orange roughy east"; RunFolder <- "Orange Roughy East Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==6)  { Species <- "Pink ling"; RunFolder <- "Pink ling Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==7)  { Species <- "Redfish"; RunFolder <- "Redfish Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==8)  { Species <- "School whiting"; RunFolder <- "School whiting Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==9)  { Species <- "Silver warehou"; RunFolder <- "Silver warehou Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==10) { Species <- "Tiger flathead"; RunFolder <- "Tiger flathead Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==11) { Species <- "P cod"; RunFolder <- "P. cod/"; AssignmentPath=AssignmentPath2; Fleet= 2 }
  if (Ispec==12) { Species <- "Milk shark"; RunFolder <- "Milk shark/"; AssignmentPath=AssignmentPath2; Fleet= 2 }
  if (Ispec==13) { Species <- "Runze"; RunFolder <- "Runze/"; AssignmentPath=AssignmentPath2; Fleet= 2 }
  if (Ispec==14) { Species <- "Perch"; RunFolder <- "Perch/"; AssignmentPath=AssignmentPath2; Fleet= 2 }
  if (Ispec==15) { Species <- "Herring"; RunFolder <- "Herring/"; AssignmentPath=AssignmentPath2; Fleet= 2 }
   
  #Species <- "Base_250519_GasSnap_selex24"
  print(AssignmentPath)
  Species2 <- gsub(" ",".",Species)
  print(AssignmentPath)
  print(Species2)
  
  Outcomes <- do.check(Species,Species2,FileName1=paste0(Species2,".OM"),FileName2=paste0(Species2,".EM"),FileName3=paste0("General_",Species2,".OM"),AssignmentPath=AssignmentPath,Fleet=Fleet)  
  if (Png==T & Ispec==5) dev.off()
  if (Png==T & Ispec==10) dev.off()
  if (Png==T & Ispec==13) dev.off()
  #RunFolder <- paste0(AssignmentPath,Species)
}
