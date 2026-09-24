

AssignmentPath1 <- "C:/Research/csiro/Species25/M analysis/Diagnostics/"
AssignmentPath2 <- "C:/Research/NewRat/"
ResultsPath2 <- "C:/Research/NewRat/Newrat_tst/Temp/"


SpeciesA <- c("Bight.redfish","Blue grandier","Deepwater.flathead","Morwong","orange.roughy.east","pink.ling","redfish","School.whiting","silver.warehou","Tiger.flathead","P.cod","Milk.shark","Runze","Perch","Herring","p.cod.OMF")
SpeciesB <- c("Bight redfish/","Blue grandier","Deepwater flathead/","Morwong/","orange roughy east/","pink ling/","redfish/","School whiting/","silver warehou/","Tiger flathead/","P cod/","Milk shark/","Runze/","Perch","Herring","P cod OMF")
AssignP <- c(rep(AssignmentPath1,10),rep(AssignmentPath2,6))

Do.plot.tst <- function(Ispec,add=0)
 {
  # Truth

  for (Iyrs in 1:length(add))  
   {
    RunFolder <- paste0(AssignP[Ispec],SpeciesB[Ispec])
    print(RunFolder)
    starter <- SS_readstarter(file.path(RunFolder, "starter.ss"),verbose=F)
    dat <<- SS_readdat(file.path(RunFolder, starter$datfile),verbose=F)
    ctl <<- SS_readctl(file.path(RunFolder, starter$ctlfile),datlist=dat)
    styr <- dat$styr
    endyr <- dat$endyr
    years <- styr:endyr
    nyears <- endyr - styr + 1
    nyear1 <- endyr+1 - styr + 1
    rep.filename <- paste0("REPORT.SSO")
    comp.filename <- paste0("COMPREPORT.SSO")
    True <- model0$timeseries$SpawnBio[-c(1,2)]
    True <- True[1:nyears]
  
    RunFolder <- paste0(ResultsPath2,"Assess_Temp_",SpeciesA[Ispec],"/SS_Sim_1_",SpeciesA[Ispec],"_",nyears)
    print(RunFolder)
    starter <- SS_readstarter(file.path(RunFolder, "starter.ss"),verbose=F)
    dat <<- SS_readdat(file.path(RunFolder, starter$datfile),verbose=F)
    ctl <<- SS_readctl(file.path(RunFolder, starter$ctlfile),datlist=dat)
    model0 <<- SS_output(dir=RunFolder,repfile=rep.filename,covar=F,verbose=F,printstat=F,warn=F,hidewarn=T)
    Est <- model0$timeseries$SpawnBio[-c(1,2)]
    Est <- Est[1:nyears]
    ymax <- max(True,Est)
    plot(years,True,col="green",ylim=c(0,ymax*1.1),yaxs="i",xaxs="i",lwd=2,type="l")
    lines(years,Est,col="red",lwd=2)
    text(styr+(nyears-1)*0.025,ymax*0.1,SpeciesA[Ispec],adj=0,cex=1.2)
  }
  
}

par(mfrow=c(4,3),oma=c(2,2,2,2),mar=c(5,4,2,1))

#for (Ispec in c(1:7,9:14))
  for (Ispec in c(16))
  {
 Do.plot.tst(Ispec,add=c(0)) 
  
  
}
  