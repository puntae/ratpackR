rm(list=ls())
# Path <- "C:/Research/NewRat/"
Path <- "C:/MSE/ratpackR/"
setwd(Path)

library(r4ss)
library(RTMB)

cat("# -=============================================================================================================\n")
cat("\n")
cat("Compare OM with original SS files \n")
cat("Compare OM with estimates \n")
cat("\n")
cat("# -=============================================================================================================\n")

ShowEM <- T

Do.plot.tst <- function(Species,Species2,Species3=NULL,OriginalName,OriginalPath,ResultsPath,
                        TempPath,SuperDiag=T,Fleet=1,add=0,UseSpec=1,Narea=1,NextraCat=0)
 {
  
  if (is.null(Species3)) Species3 <- Species2
  OriginalFolder <- paste0(Path,OriginalPath,OriginalName,"/")
  print(OriginalFolder)
  OMFolder <- paste0(Path,ResultsPath)
  print(OMFolder)
  EMFolder <- paste0(Path,TempPath)
  print(EMFolder)
  
  rep.filename <- paste0("REPORT.SSO")
  comp.filename <- paste0("COMPREPORT.SSO")
  
  # Original assessment
  starter <- SS_readstarter(file.path(OriginalFolder, "starter.ss"),verbose=F)
  dat <<- SS_readdat(file.path(OriginalFolder, starter$datfile),verbose=F)
  ctl <<- SS_readctl(file.path(OriginalFolder, starter$ctlfile),datlist=dat)
  modelOrig <<- SS_output(dir=OriginalFolder,repfile=rep.filename,covar=F,verbose=F,printstat=F,warn=F,hidewarn=T)
  
  styr <- dat$styr
  endyr <- dat$endyr
  nyears <- endyr - styr + 1
  nyear1 <- endyr - styr + 1
  nages <- dat$Nages+1
  nsex <- dat$Nsexes
  nfleets <- dat$Nfleets
  nfleet.catch <- length(which(dat$fleetinfo$type==1))
  ages <- 0:dat$Nages
  Nspec <- 1
  
  FileName <- paste0(OMFolder,"ProjectAll_",Species3,"_00.out")
  print(FileName)
  modelA <<- read.table(file=FileName,head=T,comment="?")
  modelA <<- modelA[which(modelA[,1]==UseSpec),]

  for (Iyrs in 1:length(add))  
   {
    years <- styr:(endyr+add[Iyrs])
    nyear1 <- endyr+add[Iyrs] - styr + 1
    True <- as.numeric(modelA$Total_SSB[1:nyear1])
    
    if(add[Iyrs]==0)
     {
      Original <- modelOrig$timeseries$SpawnBio[-c(1,2)]
      Original <- Original[1:nyear1]
      Original <- modelOrig$recruit$SpawnBio[which(modelOrig$recruit$Yr >= styr)]
      Original <- Original[1:nyear1]
    }
    
    RunFolder <- paste0(EMFolder,"SS_Sim_1_",Species2,"_",nyears+NextraCat+add[Iyrs],"_1")
    print(RunFolder)
    starter <- SS_readstarter(file.path(RunFolder, "starter.ss"),verbose=F)
    dat <<- SS_readdat(file.path(RunFolder, starter$datfile),verbose=F)
    ctl <<- SS_readctl(file.path(RunFolder, starter$ctlfile),datlist=dat)
    model0 <<- SS_output(dir=RunFolder,repfile=rep.filename,covar=F,verbose=F,printstat=F,warn=F,hidewarn=T)
    Est <- model0$timeseries$SpawnBio[-c(1,2)]
    Est <- Est[1:nyear1]

    # Now do plots
    #print(Original)
    #print(True)
    ymax <- max(Original,True,Est)
    plot(years,True,col="green",ylim=c(0,ymax*1.1),yaxs="i",xaxs="i",lwd=2,type="l")
    if (ShowEM==T) lines(years,Est,col="red",lwd=2)
    if (add[Iyrs]==0) lines(years,Original,col="blue",lwd=2)
    text(styr+(nyears-1)*0.025,ymax*0.1,Species,adj=0,cex=1.2)
    if (Iyrs==1) legend("topright",legend=c("Original","OM","EM"),col=c("blue","green","red"),lty=1,lwd=2)
    if (Iyrs>1) legend("topright",legend=c("OM","EM"),col=c("green","red"),lty=1,lwd=2)
  }
  
}

# ========================================================================================================================

par(mfrow=c(2,2),oma=c(2,2,2,2),mar=c(5,4,2,1))

#for (Ispec in c("A1","A2","B1",paste0("C",c(1:11)),paste0("D",c(1:16)), paste0("E",c(1:2)), "F1" ) )
# for (Ispec in c(paste0("C",c(1:11))) )
#for (Ispec in c(paste0("D",c(6,19,28))) )
# for (Ispec in c(paste0("D",c(11))) )
#for (Ispec in c(paste0("D",c(6,12,19,28)),paste0("C",c(1:11))) )
for (Ispec in   c(paste0("D",c(6:12, 20, 22, 28)) ))

    #for (Ispec in c(paste0("E",c(1:4))) )
 {
  Species <- NULL; Species3 <- NULL;UseSpec <- 1; Narea <- 1; NextraCat <- 0
  #if (Ispec=="A1") { Species <- "Tiger flathead"; OriginalName <- "Flathead_Tuned"; OriginalPath <- "Base Buffer Files/"; ResultsPath <- "Results/Tiger.flathead/"; TempPath <- "Temp/Assess_temp_Tiger.flathead/"  }
  #if (Ispec=="A2") { Species <- "School whiting"; OriginalName <- "Whiting_Tuned"; OriginalPath <- "Base Buffer Files/"; ResultsPath <- "Results/School.whiting/"; TempPath <- "Temp/Assess_temp_School.whiting/"  }
  
  if (Ispec=="C1") { Species <- "Bight Redfish"; OriginalName <- "Bight Redfish"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Bight.redfish/"; TempPath <- "Temp/Assess_temp_Bight.redfish/"  }
  if (Ispec=="C2") { Species <- "Blue Grenadier"; OriginalName <- "Blue Grenadier"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Blue.grenadier/"; TempPath <- "Temp/Assess_temp_Blue.grenadier/"  }
  if (Ispec=="C3") { Species <- "Deepwater flathead"; OriginalName <- "Deepwater flathead"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Deepwater.flathead/"; TempPath <- "Temp/Assess_temp_Deepwater.flathead/"  }
  if (Ispec=="C4") { Species <- "Morwong"; OriginalName <- "Morwong"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Morwong/"; TempPath <- "Temp/Assess_temp_Morwong/"  }
  if (Ispec=="C5") { Species <- "Orange Roughy East"; OriginalName <- "Orange Roughy East"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Orange.Roughy.East/"; TempPath <- "Temp/Assess_temp_Orange.Roughy.East/"  }
  #if (Ispec=="C6") { Species <- "Pink Ling"; OriginalName <- "Pink ling"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Pink.ling/"; TempPath <- "Temp/Assess_temp_Pink.ling/"; NextraCat <- 2  }
  if (Ispec=="C7") { Species <- "Redfish"; OriginalName <- "Redfish"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Redfish/"; TempPath <- "Temp/Assess_temp_Redfish/"  }
  if (Ispec=="C8") { Species <- "School Whiting"; OriginalName <- "School whiting"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/School.whiting/"; TempPath <- "Temp/Assess_temp_School.whiting/"; NextraCat <- 2  }
  if (Ispec=="C9") { Species <- "Silver Warehou"; OriginalName <- "Silver warehou"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Silver.warehou/"; TempPath <- "Temp/Assess_temp_Silver.warehou/"; NextraCat <- 2  }
  if (Ispec=="C10") { Species <- "Tiger flathead"; OriginalName <- "Tiger flathead"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Tiger.flathead/"; TempPath <- "Temp/Assess_temp_Tiger.flathead/"; NextraCat <- 4  }
  if (Ispec=="C11") { Species <- "Mirror dory"; OriginalName <- "Mirror dory"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Mirror.dory/"; TempPath <- "Temp/Assess_temp_Mirror.dory/"; NextraCat <- 17  }
  
  if (Ispec=="D1") { Species <- "Milk shark"; OriginalName <- "Milk shark"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Milk.shark/"; TempPath <- "Temp/Assess_temp_Milk.shark/"  }
  if (Ispec=="D2") { Species <- "Perch"; OriginalName <- "Perch"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Perch/"; TempPath <- "Temp/Assess_temp_Perch/"  }
  if (Ispec=="D3") { Species <- "Herring"; OriginalName <- "Herring"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Herring/"; TempPath <- "Temp/Assess_temp_Herring/"  }
  if (Ispec=="D4") { Species <- "SSageselemod2"; OriginalName <- "SSageselemod2"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/SSageselemod2/"; TempPath <- "Temp/Assess_temp_SSageselemod2/"  }
  if (Ispec=="D5") { Species <- "Runze"; OriginalName <- "Runze"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Runze/"; TempPath <- "Temp/Assess_temp_Runze/"  }
  if (Ispec=="D6") { Species <- "P cod"; OriginalName <- "P cod"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/P.cod/"; TempPath <- "Temp/Assess_temp_P.cod/"  }
  if (Ispec=="D7") { Species <- "Bluespot_Pilbara"; OriginalName <- "Bluespot_Pilbara"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Bluespot_Pilbara/"; TempPath <- "Temp/Assess_temp_Bluespot_Pilbara/"  }
  if (Ispec=="D8") { Species <- "Red_Emperor_Kimberley"; OriginalName <- "Red_Emperor_Kimberley"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Red_Emperor_Kimberley/"; TempPath <- "Temp/Assess_temp_Red_Emperor_Kimberley/"  }
  if (Ispec=="D9") { Species <- "Red_Emperor_Pilbara"; OriginalName <- "Red_Emperor_Pilbara"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Red_Emperor_Pilbara/"; TempPath <- "Temp/Assess_temp_Red_Emperor_Pilbara/"  }
  if (Ispec=="D10") { Species <- "SandySprat"; OriginalName <- "SandySprat"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/SandySprat/"; TempPath <- "Temp/Assess_temp_SandySprat/"  }
  if (Ispec=="D11") { Species <- "WA_Dhufish"; OriginalName <- "WA_Dhufish"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/WA_Dhufish/"; TempPath <- "Temp/Assess_temp_WA_Dhufish/"  }
  if (Ispec=="D12") { Species <- "Sandbar_Shark"; OriginalName <- "Sandbar_Shark"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Sandbar_Shark/"; TempPath <- "Temp/Assess_temp_Sandbar_Shark/"; NextraCat <- 4  }
  if (Ispec=="D13") { Species <- "Sandbar_Shark2"; OriginalName <- "Sandbar_Shark2"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Sandbar_Shark2/"; TempPath <- "Temp/Assess_temp_Sandbar_Shark2/"  }
  if (Ispec=="D14") { Species <- "Sardine"; OriginalName <- "Sardine"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Sardine/"; TempPath <- "Temp/Assess_temp_Sardine/"  }
  if (Ispec=="D15") { Species <- "Mackerel"; OriginalName <- "Mackerel"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Mackerel/"; TempPath <- "Temp/Assess_temp_Mackerel/"  }
  if (Ispec=="D16") { Species <- "Anchovy"; OriginalName <- "Anchovy"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Anchovy/"; TempPath <- "Temp/Assess_temp_Anchovy/"  }
  if (Ispec=="D17") { Species <- "Squid"; OriginalName <- "Squid"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Squid/"; TempPath <- "Temp/Assess_temp_Squid/"  }
  if (Ispec=="D18") { Species <- "Red_Emperor_Pilbara_2A"; OriginalName <- "Red_Emperor_Pilbara_2A"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Red_Emperor_Pilbara_2A/"; TempPath <- "Temp/Assess_temp_Red_Emperor_Pilbara_2A/"; Narea <- 2  }
  
  if (Ispec=="D19") { Species <- "Gummy"; OriginalName <- "Gummy"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Gummy/"; TempPath <- "Temp/Assess_temp_Gummy/"; NextraCat <- 2  }
  if (Ispec=="D20") { Species <- "Quillback"; OriginalName <- "Quillback"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Quillback/"; TempPath <- "Temp/Assess_temp_Quillback/"  }
  if (Ispec=="D21") { Species <- "Dusky"; OriginalName <- "Dusky"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Dusky/"; TempPath <- "Temp/Assess_temp_Dusky/"; NextraCat <- 2  }
  if (Ispec=="D22") { Species <- "Goldband"; OriginalName <- "goldband"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Goldband/"; TempPath <- "Temp/Assess_temp_Goldband/"  }
  if (Ispec=="D23") { Species <- "Snapper_Gascoyne"; OriginalName <- "Snapper_Gascoyne"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Snapper_Gascoyne/"; TempPath <- "Temp/Assess_temp_Snapper_Gascoyne/"  }
  if (Ispec=="D24") { Species <- "Snapper_North"; OriginalName <- "Snapper_North"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Snapper_North/"; TempPath <- "Temp/Assess_temp_Snapper_North/"  }
  if (Ispec=="D25") { Species <- "LM_CL3"; OriginalName <- "LM_CL3"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/LM_CL3/"; TempPath <- "Temp/Assess_temp_LM_CL3/"  }
  if (Ispec=="D26") { Species <- "WCDSC"; OriginalName <- "WCDSC"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/WCDSC/"; TempPath <- "Temp/Assess_temp_WCDSC/"  }
  if (Ispec=="D27") { Species <- "Rankin"; OriginalName <- "Rankin"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Rankin/"; TempPath <- "Temp/Assess_temp_Rankin/"  }
  if (Ispec=="D28") { Species <- "Whiskery_Shark"; OriginalName <- "Whiskery_Shark"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Whiskery_Shark/"; TempPath <- "Temp/Assess_temp_Whiskery_Shark/"; NextraCat <- 2  }
  if (Ispec=="D29") { Species <- "Red_Emperor_Pilbara_2A_nodevs"; OriginalName <- "Red_Emperor_Pilbara_2A_nodevs"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Red_Emperor_Pilbara_2A_nodevs/"; TempPath <- "Temp/Assess_temp_Red_Emperor_Pilbara_2A_nodevs/"; Narea <- 2  }
  if (Ispec=="D30") { Species <- "Red_Emperor_Pilbara_2A_nodevs_CVfix"; OriginalName <- "Red_Emperor_Pilbara_2A_nodevs_CVfix"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/Red_Emperor_Pilbara_2A_nodevs_CVfix/"; TempPath <- "Temp/Assess_temp_Red_Emperor_Pilbara_2A_nodevs_CVfix/"; Narea <- 2  }

  if (Ispec=="E1") { Species <- "Pink ling";      OriginalName <- "Pink ling";      Species3 <- "Multi-CSIRO"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Multi-CSIRO/"; TempPath <- "Temp/Assess_temp_Multi-CSIRO/"; UseSpec <- 1  }
  if (Ispec=="E2") { Species <- "Tiger flathead"; OriginalName <- "Tiger flathead"; Species3 <- "Multi-CSIRO"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Multi-CSIRO/"; TempPath <- "Temp/Assess_temp_Multi-CSIRO/"; UseSpec <- 2 }
  if (Ispec=="E3") { Species <- "School whiting"; OriginalName <- "School whiting"; Species3 <- "Multi-CSIRO"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Multi-CSIRO/"; TempPath <- "Temp/Assess_temp_Multi-CSIRO/"; UseSpec <- 3 }
  if (Ispec=="E4") { Species <- "Mirror dory"; OriginalName <- "Mirror dory"; Species3 <- "Multi-CSIRO"; OriginalPath <- "Base CSIRO Files/"; ResultsPath <- "Results/Multi-CSIRO/"; TempPath <- "Temp/Assess_temp_Multi-CSIRO/"; UseSpec <- 4 }
  
  
  #if (Ispec=="F1") { Species <- "P cod"; OriginalName <- "P cod"; Species3 <- "P_cod_Area"; OriginalPath <- "Base Other Files/"; ResultsPath <- "Results/P_cod_Area/"; TempPath <- "Temp/Assess_temp_P_cod_Area/"; Narea <- 3 }
  
  if (!is.null(Species))
    {
    Species2 <- gsub(" ",".",Species)  
    add <- c(0,15)
    if (!is.null(Species)) Outcomes <- Do.plot.tst(Species,Species2,Species3=Species3,OriginalName,OriginalPath,
                                                 ResultsPath,TempPath,add=add,UseSpec=UseSpec,Narea=Narea,NextraCat=NextraCat)
   }
 }
  
  

