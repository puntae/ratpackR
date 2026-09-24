library(r4ss)
library(readxl)
BasePath <- "C:/Research/NewRat/newrat_tst/Inputs/"
setwd(BasePath)

RatpackPath <- "C:/Research/NewRat/newrat_tst/Results/"

#AssignmentPath <- "C:/Research/csiro/Species25/M analysis/Diagnostics/"
#setwd(AssignmentPath)

# =====================================================================================================================

do.check <- function(Species,Species2,AssignmentPath,Final,SuperDiag=T,Fleet=1)
{
  
  Nspec <- length(Species)
  modelA <<- read.table(file=paste0(RatpackPath,"Project.",Final),skip=5+3*Nspec,head=T,comment="?")
  print(modelA)
  
  for (Ispec in 1:Nspec)
   {
    RunFolder <- paste0(AssignmentPath[Ispec],Species[Ispec])
    print(RunFolder)

    starter <- SS_readstarter(file.path(RunFolder, "starter.ss"),verbose=F)
    dat <<- SS_readdat(file.path(RunFolder, starter$datfile),verbose=F)
    ctl <<- SS_readctl(file.path(RunFolder, starter$ctlfile),datlist=dat)
    styr <- dat$styr
    endyr <- dat$endyr
    print(endyr)
    nyears <- endyr - styr + 1
    nyear1 <- endyr+1 - styr + 1
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
   
    SSB.true <- timeseries$SpawnBio[1:nyear1]
    Rec.true <- timeseries$Recruit_0[1:nyears]
    if (Fleet==1) F.true <- timeseries$`F:_1`[1:nyears]
    if (Fleet==2) F.true <- timeseries$`F:_2`[1:nyears]
  
    years <- styr:(endyr+1)
    modelB <- modelA[modelA[,1]==Ispec,]
    print(modelB)
    SSB.est <- as.numeric(modelB$SSB[1:nyear1])
    print(SSB.est)
    Rec.est <- as.numeric(modelB$Recruitment[1:nyears])
    if (Fleet==1) F.est <- as.numeric(modelB$Full_F_1[1:nyears])
    if (Fleet==2) F.est <- as.numeric(modelB$Full_F_2[1:nyears])
  
    years <- styr:(endyr+1)
    ymax <- max(SSB.true,SSB.est)*1.05
    plot(years,SSB.true,lwd=2,lty=1,type="l",ylim=c(0,ymax),col="red")
    points(years,SSB.est,pch=16,col="blue")
    max.err.SSB <- max(abs(SSB.est-SSB.true)/SSB.true)*100
    text(endyr,ymax*0.95,Species[Ispec],adj=1,cex=1.5)
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
    cat(Species2[Ispec],max.err.SSB,max.err.Rec,"\n")
  }
}

AssignmentPath1 <- "C:/Research/csiro/Species25/M analysis/Diagnostics/"
AssignmentPath2 <- "C:/Research/NewRat/"


#for (Ispec in c(1,2,3,4,5,6,7,8,9,10))
#for (Ispec in c(1:10))

Species <- NULL; RunFolder <- NULL; AssignmentPath <- NULL

for (Ispec in c(9,10))
 {
    
  if (Ispec==1)  { Species <- c(Species,"Bight redfish"); RunFolder <- RunFiolder <- c(RunFolder,"Bight redfish Major/"); AssignmentPath=c(AssignmentPath,AssignmentPath1);Fleet=1 }  
  if (Ispec==2)  { Species <- "Blue grenadier"; RunFolder <- "Blue grenadier Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==3)  { Species <- "Deepwater flathead"; RunFolder <- "Deepwater flathead Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==4)  { Species <- "Morwong"; RunFolder <- "Morwong Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==5)  { Species <- "Orange roughy east"; RunFolder <- "Orange Roughy East Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==6)  { Species <- "Pink ling"; RunFolder <- "Pink ling Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==7)  { Species <- "Redfish"; RunFolder <- "Redfish Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==8)  { Species <- "School whiting"; RunFolder <- "School whiting Major/"; AssignmentPath=AssignmentPath1;Fleet=1 }
  if (Ispec==9)  { Species <- c(Species,"Silver warehou"); RunFolder <- c(RunFolder,"Silver warehou Major/"); AssignmentPath<-c(AssignmentPath,AssignmentPath1);Fleet=1 }
  if (Ispec==10) { Species <- c(Species,"Tiger flathead"); RunFolder <- c(RunFolder,"Tiger flathead Major/"); AssignmentPath<-c(AssignmentPath,AssignmentPath1);Fleet=1 }
  if (Ispec==11) { Species <- "Snapper"; RunFolder <- "Snapper/"; AssignmentPath=AssignmentPath2; Fleet= 2 }
  if (Ispec==12) { Species <- "P cod"; RunFolder <- "P. cod/"; AssignmentPath=AssignmentPath2; Fleet= 2 }
  if (Ispec==13) { Species <- "Milk shark"; RunFolder <- "Milk shark/"; AssignmentPath=AssignmentPath2; Fleet= 2 }
  if (Ispec==14) { Species <- "Runze"; RunFolder <- "Runze/"; AssignmentPath=AssignmentPath2; Fleet= 2 }
 }
    
    if (Png==T & Ispec==1) png(filename="C:/Research/NewRat/newrat_tst/Check1.png",height=1000,width=800)  
    if (Png==T & Ispec==6) png(filename="C:/Research/NewRat/newrat_tst/Check2.png",height=1000,width=800)  
    if (Ispec %in% c(1,6)) par(mfrow=c(5,3),oma=c(5,5,5,5),mar=c(5,4,2,1))
    
  Species2 <- gsub(" ",".",Species)
  print(Species)
  print(AssignmentPath)
  print(Species2)
  
  
  Png <- F
  par(mfrow=c(5,3),oma=c(5,5,5,5),mar=c(5,4,2,1))
  Outcomes <- do.check(Species,Species2,AssignmentPath,Final="Multi1",Fleet=Fleet)  
  if (Png==T & Ispec==5) dev.off()
  if (Png==T & Ispec==10) dev.off()
  if (Png==T & Ispec==13) dev.off()
  #RunFolder <- paste0(AssignmentPath,Species)

