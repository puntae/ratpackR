MatchTable<-function(Table,Char1=NULL,Char2=NULL,Char3=NULL,Char4=NULL,Char5=NULL,Char6=NULL)
{
  ii <- rep(T,length(Table[,1]))
  if (!is.null(Char1)) ii <- ii & (Table[,1]==Char1)
  if (!is.null(Char2)) ii <- ii & (Table[,2]==Char2)
  if (!is.null(Char3)) ii <- ii & (Table[,3]==Char3)
  if (!is.null(Char4)) ii <- ii & (Table[,4]==Char4)
  if (!is.null(Char5)) ii <- ii & (Table[,5]==Char5)
  if (!is.null(Char6)) ii <- ii & (Table[,6]==Char6)
  ii <- seq(1:length(Table[,1]))[ii]
  if (length(ii) == 0) { cat("failed",Char1,Char2,Char3,Char4,Char5,Char6,"\n"); AAA }
  return(ii)
}

FindZ <- function(z,msyl)
{
  val <- 1-(z+1)*msyl^z
  return(val)
}  

# ====================================================================================================

#install.packages("TMB")
#library(TMB)
CurrentDir <- getwd()
print(CurrentDir)
args <- commandArgs(trailingOnly=TRUE)
Directory <- args[1]
Directory <- ""
print(Directory)

DatFileName <- paste(Directory,"dynTier4.dat",sep="")
print(DatFileName)
DatFile <- read.table(DatFileName,comment.char = "?",fill=T,blank.lines.skip=T,stringsAsFactors=F,col.names=1:100)
Index <- MatchTable(DatFile,Char1="#",Char2="Rlibrary",Char3="path"); Rlibrary <- DatFile[Index+1,1]
print(Index)
print(Rlibrary)
.libPaths( c(Rlibrary, "C:\\Users\\lit060\\Documents\\R\\win-library\\4.0" ,"C:/Users/pun009/Documents/R/win-library/4.1", .libPaths() ) )
print(.libPaths())

require(TMB)

# Compile the code
#compile("dynTier4.cpp")
#compile("dynTier4.cpp", flags="-Wno-ignored-attributes")
dyn.load(dynlib("dynTier4"))

# Now read further

CtlFileName <- paste(Directory,"dynTier4.ctl",sep="")
CtlFile <- read.table(CtlFileName,comment.char = "?",fill=T,blank.lines.skip=T,stringsAsFactors=F,col.names=1:100)

# Read the data file
Index <- MatchTable(DatFile,Char1="#",Char2="Number",Char4="fleets"); Nfleet <- as.numeric(DatFile[Index+1,1])
Index <- MatchTable(DatFile,Char1="#",Char2="Range",Char4="years"); Yr1 <- as.numeric(DatFile[Index+1,1]); Yr2 <- as.numeric(DatFile[Index+1,2]);
Nyears <- Yr2-Yr1+1
cat("Nfleet and year range",Nfleet,Yr1,Yr2,Nyears,"\n")
Index <- MatchTable(DatFile,Char1="#",Char2="Year,",Char3="Catch");
TheData <- matrix(0,nrow=Nyears,ncol=(1+2*Nfleet))
for (Iyear in 1:Nyears)
 for (II in 1:(1+2*Nfleet)) TheData[Iyear,II] <- as.numeric(DatFile[Index+Iyear,II])

# Extract the data
Catch <- apply(as.matrix(TheData[,2:(1+Nfleet)],ncol=Nfleet),1,sum)
IndexData <- as.matrix(TheData[,(2+Nfleet):(1+2*Nfleet)])
Years <- TheData[,1]

# how many Cpue series
UseCPUE <- apply(IndexData,2,max) > 0
Ncpue <- sum(UseCPUE)
IndexData <- as.matrix(IndexData[,UseCPUE],ncol=Nfleet)

cat("Number of CPUE series: ",Ncpue,"\n")

# Read the CTL file 
# Set MSYL (BMSY/B0)
Index <- MatchTable(CtlFile,Char1="#",Char2="MSYL"); MSYL <- as.numeric(CtlFile[Index+1,1])
# Decide to estimate (1) or fix (0) r
Index <- MatchTable(CtlFile,Char1="#",Char2="EstR"); EstR <- as.numeric(CtlFile[Index+1,1])
# Decide to estimate (1) or fix (0) z
Index <- MatchTable(CtlFile,Char1="#",Char2="EstZ"); EstZ <- as.numeric(CtlFile[Index+1,1])
# Range of years to define MSY (if it is pre-specified) and BMSY (if it is pre-specified)
Index <- MatchTable(CtlFile,Char1="#",Char2="BMSY"); MSYRY1 <- as.numeric(CtlFile[Index+1,1]); MSYRY2 <- as.numeric(CtlFile[Index+1,2])
Index <- MatchTable(CtlFile,Char1="#",Char2="Target"); Btarg <- as.numeric(CtlFile[Index+1,1])
Index <- MatchTable(CtlFile,Char1="#",Char2="MSY"); MSYPriorY1 <- as.numeric(CtlFile[Index+1,1]); MSYPriorY2 <- as.numeric(CtlFile[Index+1,2])
Index <- MatchTable(CtlFile,Char1="#",Char2="CV(MSYL)"); CVMSYL <- as.numeric(CtlFile[Index+1,1])
Index <- MatchTable(CtlFile,Char1="#",Char2="R(prior)"); PriorMeanR <- as.numeric(CtlFile[Index+1,1]);PriorSDr <- as.numeric(CtlFile[Index+1,2])
Index <- MatchTable(CtlFile,Char1="#",Char2="Process"); ProcessError <- as.numeric(CtlFile[Index+1,1])

# Specify the years to define MSY 
#MSYPriorY1 <- 1970
#MSYPriorY2 <- 1990

# set a weight on MSYL
#CVMSYL <- 0.0002

# Prior on r
#PriorMeanR = 0.15
#PriorSDr = 0.0001

cat(MSYL,EstR,EstZ,MSYRY1,MSYRY2,Btarg,MSYPriorY1,MSYPriorY2,CVMSYL,PriorMeanR,PriorSDr,"\n")
#EstR <- 1
#EstZ <- 0



# Compute median catch as a proxy for MSY
CIndex <- which(TheData[,1] >= MSYPriorY1 & TheData[,1] <= MSYPriorY2)
MSYINPUT= median(Catch[CIndex])

# find the  z corresponding to BMSY/B0
fitz <- uniroot(FindZ,lower=0.0001,upper=10,msyl=MSYL)
z <- fitz$root
MSYL <- (1.0/(z+1.0))^(1.0/z)

#ProcessError <- 2

data <- list(Year=Years,C=Catch,I=IndexData,MSYL=MSYL,EstR=EstR,EstZ=EstZ,MSYINPUT=MSYINPUT,MSYRY1=MSYRY1,MSYRY2=MSYRY2,
             Ncpue=Ncpue,CVMSYL=CVMSYL,MSYPriorY1=MSYPriorY1,MSYPriorY2=MSYPriorY2,
             PriorMeanR=PriorMeanR,PriorSDr=PriorSDr,ProcessError=ProcessError)
Nyear <- length(Catch)

# Initial values for the parameters
rhat <- exp(-1.1)
Khat <- MSYINPUT/(rhat*MSYL*(1-MSYL^z))
#cat(rhat,MSYhat,Khat*rhat/4.0,"\n")
Q <- rep(0,Ncpue)
for (Ifleet in 1:Ncpue) Q[Ifleet] <- mean(IndexData[IndexData[,Ifleet]>0,Ifleet])/(Khat/2)
logSigma = -2.3

parameters <- list(logR=log(rhat), logK=log(Khat), logQ=log(Q), logSigma=logSigma,FF=rep(-2,Nyear),logz=log(z),logSigmaR=log(0.1),Rec_dev=rep(0,Nyear))
if (data$EstR==0 & data$EstZ==0) map <- list(logz=factor(NA),logR=factor(NA))
if (data$EstR==1 & data$EstZ==0) map <- list(logz=factor(NA))
if (data$EstR==0 & data$EstZ==1) map <- list(logr=factor(NA))
if (data$EstR==1 & data$EstZ==1) map <- NULL
if (ProcessError==0) map <- c(map,list(logSigmaR=factor(NA),Rec_dev=rep(factor(NA),length(parameters$Rec_dev))))
if (ProcessError==1) map <- c(map,list(logSigmaR=factor(NA)))
#print(str(parameters))

################################################################################

 # fit the model
 if (ProcessError %in% c(0,1))
  model <- MakeADFun(data=data, parameters,map=map,DLL="dynTier4",control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)
 if (ProcessError %in% c(2))
  model <- MakeADFun(data=data, parameters,map=map,random=c("Rec_dev"),DLL="dynTier4",control=list(eval.max=10000,iter.max=1000,rel.tol=1e-15),silent=T)
 fit <- nlminb(model$par, model$fn, model$gr)
 if (ProcessError != 2)
  for (i in 1:8) 
   {
    BestP <- model$env$last.par.best
    if (ProcessError==2) 
     { 
      Index <- which(names(BestP)=="Rec_dev"); 
      BestP <- BestP[-Index]; 
     }
    fit <- nlminb(BestP, model$fn, model$gr)
    }

 # extract results
 rep <- sdreport(model)
 rep2 <- summary(rep)

 # Summarize ALL to screen
 print(rep2)

 Results <- NULL
 Results$B <- model$report()$B[1:Nyear]
 Results$C <- Catch
 Results$t <- Years
 Results$I <- IndexData
 Results$Ihat <- model$report()$Ihat
 Results$Chat <- model$report()$Chat
 r <- rep2[row.names(rep2) == "r",1]
 k <- rep2[row.names(rep2) == "k",1]
 z <- rep2[row.names(rep2) == "z",1]
 MSYL <- rep2[row.names(rep2) == "MSYLOut",1]
 Bcurr <- model$report()$B[Nyear+1]
 
 
# ################################################################################

 # Project using the SESSF control rule
 Ftarg <- r*(1.0-Btarg^z)
 Nproj <- 50
 Bproj <- rep(0,Nproj+1)
 Cproj <- rep(0,Nproj)
 Bproj[1] <- Bcurr
 for (Iyear in 1:Nproj)
  {
   if (Bproj[Iyear]/k < 0.2)  
    Fval <- 0
  else
   if (Bproj[Iyear]/k > 0.35)
    Fval <- Ftarg
   else
    Fval <- Ftarg*(Bproj[Iyear]/k-0.2)/(Btarg-0.2)  
   Cproj[Iyear] <- Bproj[Iyear]*Fval
   Bproj[Iyear+1] <- Bproj[Iyear] + r*Bproj[Iyear]*(1.0-(Bproj[Iyear]/k)^z)-Cproj[Iyear]
   if (Bproj[Iyear+1] < 1) Bproj[Iyear+1] <- 1
 } 
 Results$Cproj <- Cproj
 #print(Cproj)
 #print(Bproj/k)



# ################################################################################
 
 write.table(rep2, 'dynTier4.rep')
 write.table(data.frame(max(rep$gradient.fixed),row.names="Maximum gradient component:"), 'dynTier4.rep', append = TRUE,col.names=F)
 write.table(data.frame(Results$B[1],row.names="Binit"), 'dynTier4.rep', append = TRUE,col.names=F)
 write.table(data.frame(Bcurr,row.names="Bcurr"), 'dynTier4.rep', append = TRUE,col.names=F)
 write.table(data.frame(Results$Cproj[1],row.names="RBC"), 'dynTier4.rep', append = TRUE,col.names=F)
 write(t(Cproj), 'dynTier4.rep', append = TRUE,ncol=1)
 for (Index in 1:Ncpue)
  {
   write("Index", 'dynTier4.rep',append = TRUE,ncol=2)
   write(Index, 'dynTier4.rep',append = TRUE,ncol=2)
   write(t(cbind(IndexData[,Index],Results$Ihat[,Index])), 'dynTier4.rep', append = TRUE,ncol=2)
  } 
 write("Biomass", 'dynTier4.rep', append = TRUE,ncol=1)
 write(t(cbind(1:Nyear,Results$B)), 'dynTier4.rep', append = TRUE,ncol=2)
 write("Catch", 'dynTier4.rep', append = TRUE,ncol=1)
 write(t(cbind(1:Nyear,Catch,Results$Chat)), 'dynTier4.rep', append = TRUE,ncol=3)
 
# #############################################################################

 Plot <- T
 if (Plot==T) 
  {  
   par(mfrow=c(4,2))
   matplot(Results$t, cbind(Results$C), type="l", xlab="Year", ylab="Catch (kt)")
   matplot(Results$t, cbind(Results$C,Results$B), type="l", xlab="Year", ylab="Biomass and Catch (kt)")
   matplot(Results$t, Results$B/Results$B[1], type="l", xlab="Year", ylab="depletion",ylim=c(0,1.1))
   for (Icpue in 1:Ncpue)
    {  
     plot(Results$I[,Icpue]~Results$t, ylim=c(0,1.1*max(Results$I[,Icpue])), ylab="Index",pch=16,yaxs='i')
     lines(Results$Ihat[,Icpue]~Results$t, )
    }  
 
  Bio <- seq(from=0,to=k,length=100)
  SY <- r*Bio*(1.0-(Bio/k)^z)
  plot(Bio,SY,xlab="Biomass",ylab="Surplus production",type='l',lty=1,ylim=c(0,max(SY)*1.1),yaxs='i')
  Bio2 <- MSYL*k
  SY <- r*Bio2*(1.0-(Bio2/k)^z)
  lines(c(Bio2,Bio2),c(0,SY))
 }
 
 
 

