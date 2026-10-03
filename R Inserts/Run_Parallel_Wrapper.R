library(doParallel)
library(foreach)
library(Rcpp)
library(RTMB)
library(RcppArmadillo)

# 1. Source the core files (leave these completely unmodified)
source("ratpackR.R") 

# 2. Pre-compile C++ on the master node to populate the cache
Current_folder <- getwd()
RcppCache <- paste0(Current_folder, "/RcppCache")
if (!dir.exists(RcppCache)) dir.create(RcppCache)

message("\nPre-compiling C++ model on master node...")
env_before <- ls(envir = globalenv())
Rcpp::sourceCpp("R inserts/Model.cpp", cacheDir = RcppCache)
env_after <- ls(envir = globalenv())
cpp_objects <- setdiff(env_after, env_before)


# 3. Overwrite Write_starter to inject dynamic parallel seeds
Write_starter <- function(Folder, Istock, Isim, Year) {
  file.name <- paste0(Folder,"starter.ss")
  write(paste0("# starter.ss for",Folder),file=file.name)
  write("data.dat",file=file.name,append=T)
  write("data.ctl",file=file.name,append=T)
  if (Estimation.test %in% c(0,1,2,3,4,5,6) || Stock[[Istock]]$Ass$Use.par=="Yes")
    write("1 # 0=use init values in control file; 1=use ss.par",file=file.name,append=T)
  else
    write("0 # 0=use init values in control file; 1=use ss.par",file=file.name,append=T)
  write("0 # run display detail (0,1,2)",file=file.name,append=T)
  write("1 # detailed output (0=minimal for data-limited, 1=high (w/ wtatage.ss_new), 2=brief)",file=file.name,append=T)
  write("0 # # write 1st iteration details to echoinput.sso file (0,1)",file=file.name,append=T) 
  write("0 # write parm values to ParmTrace.sso (0=no,1=good,active; 2=good,all; 3=every_iter,all_parms; 4=every,active)",file=file.name,append=T)
  write("0 # write to cumreport.sso (0=no,1=like&timeseries; 2=add survey fits)",file=file.name,append=T)
  write("1 # Include prior_like for non-estimated parameters (0,1)",file=file.name,append=T)
  write("1 # Use Soft Boundaries to aid convergence (0,1) (recommended)",file=file.name,append=T)
  write("0 # Number of bootstrap datafiles to produce",file=file.name,append=T)
  if (Estimation.test %in% c(0,1,5,6)|| Stock[[Istock]]$Ass$Estimate=="No")
    write("0 # Turn off estimation for parameters entering after this phase",file=file.name,append=T)
  else
    write("10 # Turn off estimation for parameters entering after this phase",file=file.name,append=T)
  write("0 # MCeval burn interval",file=file.name,append=T)
  write("1 # MCeval thin interval",file=file.name,append=T)
  write("0 # jitter initial parm value by this fraction",file=file.name,append=T)
  write("-1 # min yr for sdreport outputs (-1 for styr)",file=file.name,append=T)
  write("-2 # max yr for sdreport outputs (-1 for endyr; -2 for endyr+Nforecastyrs",file=file.name,append=T)
  write("0 # N individual STD years",file=file.name,append=T)
  write("#vector of year values ",file=file.name,append=T)
  write("0.0001 # final convergence criteria (e.g. 1.0e-04)",file=file.name,append=T)
  write("0 # retrospective year relative to end year (e.g. -4)",file=file.name,append=T)
  write("1 # min age for calc of summary biomass",file=file.name,append=T)
  write("1 # Depletion basis:  denom is: 0=skip; 1=rel X*B0; 2=rel X*Bmsy; 3=rel X*B_styr",file=file.name,append=T)
  write("1.0 # Fraction (X) for Depletion denominator (e.g. 0.4)",file=file.name,append=T)
  write("4 # (1-SPR)_reporting:  0=skip; 1=rel(1-SPR); 2=rel(1-SPR_MSY); 3=rel(1-SPR_Btarget); 4=notrel",file=file.name,append=T)
  write("1 # # Annual_F_units: 0=skip; 1=exploitation(Bio); 2=exploitation(Num); 3=sum(Apical_F's); 4=true F for range of ages; 5=unweighted avg. F for range of ages",file=file.name,append=T)
  write("#COND 10 15 #_min and max age over which average F will be calculated with F_reporting=4 or 5",file=file.name,append=T)
  write("3 # F_std_basis: 0=raw_annual_F; 1=F/Fspr; 2=F/Fmsy ; 3=F/Fbtgt; where F means annual_F",file=file.name,append=T)
  write("0 # MCMC output detail: integer part (0=default; 1=adds obj func components); and decimal part (added to SR_LN(R0) on first call to mcmc)",file=file.name,append=T)
  write("0 # ALK tolerance (example 0.0001)",file=file.name,append=T)
  
  # Guaranteed unique seed based on the simulation, year, and stock[cite: 4]
  ss_seed <- (Isim * 100000) + (Year * 100) + Istock
  write(paste0(ss_seed, " # random number seed for bootstrap data"), file=file.name, append=T)
  
  write("3.30 # check value for end of file and for version control",file=file.name,append=T)
}


# 4. Overwrite Run_Tier1a with worker safety, on.exit, and tryCatch rules
Run_Tier1a <- function(Isim, Istock, Iarea, Year) {
  
  # Store the worker's current working directory (SimFolder) and guarantee we return to it
  orig_dir <- getwd()
  on.exit(setwd(orig_dir), add = TRUE)
  
  Folder <- paste0(General$RunFolder, "SS_Sim_", Isim, "_", General$Stock.Names[Istock], "_", Year, "_", Iarea, "/")
  if (!dir.exists(Folder)) dir.create((Folder), recursive = TRUE)
  
  Nhist <- Stock[[Istock]]$Nhist
  
  File.from <- paste0(Path, "CodeBase/SS.exe")
  File.to <- paste0(Folder, "SS.exe")
  file.copy(File.from, File.to)
  
  Write_starter(Folder, Istock, Isim, Year)
  Write_data(Folder, Istock, Iarea, Year)
  Write_ctl(Folder, Istock, Iarea, Year)
  Write_forecast(Folder, Istock, Year)
  
  setwd(Folder)
  file.copy("ss3.par", "ss3par.par", overwrite = T)
  
  # Note: invisible=TRUE added to prevent desktop window lockups from 13 simultaneous command prompts
  if (Stock[[Istock]]$Ass$SS_est_opt == "Full") vv <- system("SS", intern = T, invisible = TRUE)
  if (Stock[[Istock]]$Ass$SS_est_opt == "First.Full.Only" & Year == Stock[[Istock]]$Nhist) vv <- system("SS", intern = T, invisible = TRUE)
  if (Stock[[Istock]]$Ass$SS_est_opt == "First.Full.Only" & Year != Stock[[Istock]]$Nhist) vv <- system("SS -est", intern = T, invisible = TRUE)
  if (Stock[[Istock]]$Ass$SS_est_opt == "EstOnly") vv <- system("SS -est", intern = T, invisible = TRUE)
  if (file.exists("ss3par.par")) file.remove("ss3par.par", showWarnings = F)
  
  # Verify Stock Synthesis actually produced a valid report[cite: 4]
  if (!file.exists("Report.sso") || file.info("Report.sso")$size == 0) {
    stop(paste("SS3 crashed or failed to write Report.sso in Sim", Isim, "Year", Year, "for area", Iarea))
  }
  
  # Safely read the output using local assignment (<-) to prevent memory bloat in the worker[cite: 4]
  modeloutput <- tryCatch({
    SS_output(dir = Folder, covar = F, printstats = FALSE, verbose = FALSE, warn = FALSE)
  }, error = function(e) {
    stop(paste("r4ss failed to parse the model output in Sim", Isim, "Year", Year, ". Original error:", e$message))
  })
  
  ParsOut <- c(modeloutput$Growth_Parameters$M_age0[1],
               modeloutput$Growth_Parameters$L_a_A1[1], modeloutput$Growth_Parameters$L_a_A2[1], modeloutput$Growth_Parameters$K[1],
               modeloutput$Growth_Parameters$CVmin[1], modeloutput$Growth_Parameters$CVmax[1])
  Index <- which(modeloutput$parameters[,2] == "SR_LN(R0)"); Est <- modeloutput$parameters[Index,3]; ParsOut <- c(ParsOut, Est)
  ParsOut2 <- c(modeloutput$parameters[,3], 0)
  
  Offset <- Stock[[Istock]]$Ass$SS_early_dev_yr1 + 1
  if (Stock[[Istock]]$Ass$SS_early_dev_yr1 == 0) Offset <- 0
  SSB0 <- modeloutput$derived_quants[1,2]
  Index1 <- which(modeloutput$derived_quants$Label == paste0("SSB_", modeloutput$startyr))
  N.out.years <- modeloutput$endyr - modeloutput$startyr + 1 + modeloutput$N_forecast_yrs
  if (N.out.years > Nhist + General$Nproj + 1) N.out.years <- Nhist + General$Nproj + 1
  
  # Global assignment correctly updates the Stock list local to the parallel worker[cite: 4]
  Stock[[Istock]]$Ass$SSB.estimates[Isim, Year-Nhist+1, Iarea, 1:N.out.years] <<- modeloutput$derived_quants$Value[Index1+1:N.out.years-1]
  Stock[[Istock]]$Ass$Depl.estimates[Isim, Year-Nhist+1, Iarea, 1:N.out.years] <<- modeloutput$derived_quants$Value[Index1+1:N.out.years-1] / SSB0 * 100
  Index1 <- which(modeloutput$recruit$Yr == modeloutput$startyr)
  Stock[[Istock]]$Ass$Recr.estimates[Isim, Year-Nhist+1, Iarea, 1:N.out.years] <<- modeloutput$recruit$dev[Index1+1:N.out.years-1]
  
  Stock[[Istock]]$Ass$Par.estimates[Isim, Year-Nhist+1, Iarea, 1:length(ParsOut)] <<- ParsOut
  Stock[[Istock]]$Ass$M.estimates[Isim, Year-Nhist+1, Iarea, 1:(Nhist+General$Nproj+1)] <<- modeloutput$Natural_Mortality$`0`[4:(3+Nhist+General$Nproj+1)]
  Stock[[Istock]]$Ass$AllPar.estimates[Isim, Year-Nhist+1, Iarea, 1:length(ParsOut2)] <<- ParsOut2
  Stock[[Istock]]$Ass$MaxGrads[Isim, Year-Nhist+1, Iarea] <<- modeloutput$maximum_gradient_component
  
  Quants <- modeloutput$derived_quants
  Index <- which("ForeCatch_" == substr(rownames(Quants), 1, 10))
  TACs <- as.numeric(Quants[Index,2])
  Stock[[Istock]]$TheTACs <<- TACs
  
  if (Stock[[Istock]]$Ass$Has.insurance) Do.Insurance(Isim, Istock, Year)
  
  if (Stock[[Istock]]$Ass$Close.in.MHW == "Yes" & Stock[[Istock]]$Mcat.Index[Year+1] == 1) Stock[[Istock]]$TheTACs[1] <- 0
  
  TACs <- Stock[[Istock]]$TheTACs
  
  if (Estimation.test < 99) { return(TACs) }
  
  # Tidy up
  file.remove("wtatage.ss_new")
  file.remove("derived_posteriors.sso")
  file.remove("Forecast-report.sso")
  file.remove("posterior_vectors.sso")
  file.remove("rebuild.sso")
  file.remove("SIS_table.sso")
  file.remove("ss_summary.sso")
  file.remove("fmin.log")
  file.remove("ss.log")
  file.remove("posterior_obj_func.sso")
  file.remove("posteriors.sso")
  file.remove("ParmTrace.sso")
  file.remove("runnumber.ss")
  file.remove("ss3.bar")
  if (file.exists("ss3.b01")) file.remove("ss3.b01", showWarnings = F)
  if (file.exists("ss3.b02")) file.remove("ss3.b02", showWarnings = F)
  if (file.exists("ss3.b03")) file.remove("ss3.b03", showWarnings = F)
  
  if (!Estimation.test %in% c(99) & (Stock[[Istock]]$Ass$Clean.up == "Default" || Stock[[Istock]]$Ass$Clean.up == "Full")) {
    file.remove("SS.exe")
    file.remove("echoinput.sso")
    file.remove("warning.sso")
    file.remove("compreport.sso")
    file.remove("cumreport.sso")
    file.remove("covar.sso")
    file.remove("ss3.par")
    file.remove("ss3.rep")
    if (file.exists("ss3.p01")) file.remove("ss3.p01", showWarnings = F)
    if (file.exists("ss3.r01")) file.remove("ss3.r01", showWarnings = F)
    if (file.exists("ss3.p02")) file.remove("ss3.p02", showWarnings = F)
    if (file.exists("ss3.r02")) file.remove("ss3.r02", showWarnings = F)
    if (file.exists("ss3.p03")) file.remove("ss3.p03", showWarnings = F)
    if (file.exists("ss3.r03")) file.remove("ss3.r03", showWarnings = F)
  }
  
  if (Stock[[Istock]]$Ass$Clean.up == "Full") {
    file.remove("Report.SSO")
    file.remove("starter.ss")
    file.remove("data.dat")
    file.remove("data.ctl")
    file.remove("forecast.ss")
  }
  
  return(TACs)
}


# 5. Overwrite the DoRun function in the global environment
DoRun <- function(GeneralFile, RunNo, UseParallel = TRUE) {
  
  General <- Read.General.File(GeneralFile, RunNo)
  General <<- General
  
  if (TestCase == T) { 
    General$Nsim <- 1
    General <<- General 
  }
  if (exists("DebugFile")) write("", file = DebugFile)
  
  n_cores <- max(1, parallel::detectCores() - 1)
  
  # Generate Seeds
  TheSeeds <- 1234; set.seed(TheSeeds)
  Seeds <- matrix(floor(runif(General$Nsim * General$Nstocks, 1, 1000000)), 
                  nrow = General$Nsim, ncol = General$Nstocks)
  Seeds <<- Seeds
  
  R0mult <- rep(1, General$Nstocks)
  R0mult <<- R0mult
  
  # Phase 1: Sequential Initialization & R0 Tuning
  DummyFile <- tempfile()
  OrigFiles <- list(
    ProjAll = General$ProjAllSaveName, ProjFleet = General$ProjFleetSaveName,
    ProjFleetArea = General$ProjFleetAreaSaveName, Est = General$EstSaveName,
    Par = General$ParSaveName, AllPar = General$AllParSaveName,
    Insurance = General$InsuranceSaveName, Multi = General$MultiSaveName,
    WA = General$WASaveName, Log = General$LogName, RunFolder = General$RunFolder
  )
  
  General$ProjAllSaveName <- DummyFile; General$ProjFleetSaveName <- DummyFile
  General$ProjFleetAreaSaveName <- DummyFile; General$EstSaveName <- DummyFile
  General$ParSaveName <- DummyFile; General$AllParSaveName <- DummyFile
  General$InsuranceSaveName <- DummyFile; General$MultiSaveName <- DummyFile
  General$WASaveName <- DummyFile; General$LogName <- DummyFile
  General <<- General
  
  Stock_Tune <- vector(mode = "list", length = General$Nstocks)
  R0Save1 <- vector(mode = "list", length = General$Nstocks)
  Isim_tune <- 1
  for (Istock in 1:General$Nstocks) {
    Stock_Tune[[Istock]] <- suppressWarnings(SetSpec(Isim_tune, Istock, General$Stock.Names[Istock], Seed = Seeds[Isim_tune, Istock]))
    R0Save1[[Istock]]$Save1 <- Stock_Tune[[Istock]]$R00
    R0Save1[[Istock]]$Save2 <- Stock_Tune[[Istock]]$R00.orig
    R0Save1[[Istock]]$Save3 <- Stock_Tune[[Istock]]$R0
  }
  
  # Push Stock to the global environment so WriteBiol and Do.Project can find it
  Stock <<- Stock_Tune 
  
  for (Istock in 1:General$Nstocks) WriteBiol(Istock, General$BiolSaveName, General$FolderResults)
  
  # Tuning logic
  for (Istock in 1:General$Nstocks) {
    if (!is.na(Stock_Tune[[Istock]]$Target.Depletion)) {
      General$Do.any.projections <- F; General <<- General
      R0mult.min <- 0; R0mult.max <- 10
      for (Iloop in 1:10) {
        R0mult[Istock] <- (R0mult.min + R0mult.max) / 2.0
        R0mult <<- R0mult
        Stock_Tune[[Istock]]$R00 <- R0Save1[[Istock]]$Save1 * R0mult[Istock]
        Stock_Tune[[Istock]]$R00.orig <- R0Save1[[Istock]]$Save2 * R0mult[Istock]
        Stock_Tune[[Istock]]$R0 <- R0Save1[[Istock]]$Save3 * R0mult[Istock]
        Stock_Tune[[Istock]]$Data$NindexData <- rep(0, Stock_Tune[[Istock]]$NassArea)
        Stock_Tune[[Istock]]$Data$NdiscardData <- 0
        Stock <<- Stock_Tune 
        
        Do.Project(Isim_tune)
        
        YearProj <- Stock_Tune[[Istock]]$Nhist + 1
        Bcurrent <- sum(sum(Stock_Tune[[Istock]]$SSB[, YearProj]))
        B0 <- sum(Stock_Tune[[Istock]]$SSB0 * Stock_Tune[[Istock]]$R0[, YearProj] / Stock_Tune[[Istock]]$R00)
        Depletion <- Bcurrent / B0
        if (Depletion > Stock_Tune[[Istock]]$Target.Depletion) R0mult.max <- R0mult[Istock] else R0mult.min <- R0mult[Istock]
      } 
      General$Do.any.projections <- T; General <<- General
    }
  }
  
  # Restore output file paths for the workers
  General$ProjAllSaveName <- OrigFiles$ProjAll; General$ProjFleetSaveName <- OrigFiles$ProjFleet
  General$ProjFleetAreaSaveName <- OrigFiles$ProjFleetArea; General$EstSaveName <- OrigFiles$Est
  General$ParSaveName <- OrigFiles$Par; General$AllParSaveName <- OrigFiles$AllPar
  General$InsuranceSaveName <- OrigFiles$Insurance; General$MultiSaveName <- OrigFiles$Multi
  General$WASaveName <- OrigFiles$WA; General$LogName <- OrigFiles$Log
  General$RunFolder <- OrigFiles$RunFolder
  General <<- General
  
  # Phase 2: Parallel Simulations
  if (UseParallel) {
    message("\n==========================================================")
    message("   INITIALISING PARALLEL EXECUTION")
    message("   Total CPU Cores Detected: ", parallel::detectCores())
    message("   Allocated Cores for Workers: ", n_cores)
    message("   Total Simulations Queued: ", General$Nsim)
    message("==========================================================\n")
    
    cl <- parallel::makeCluster(n_cores, outfile = "")
    doParallel::registerDoParallel(cl)
    parallel::clusterExport(cl, c("Path", "RcppCache"), envir = .GlobalEnv)
    
    # We use a worker-specific cache directory to eliminate Rcpp SQLite locking issues
    parallel::clusterEvalQ(cl, {
      if (exists("Path")) setwd(Path)
      worker_cache <- file.path(tempdir(), paste0("RcppCache_", Sys.getpid()))
      if (!dir.exists(worker_cache)) dir.create(worker_cache, recursive = TRUE)
      Rcpp::sourceCpp("R inserts/Model.cpp", cacheDir = worker_cache)
    })
  } else {
    foreach::registerDoSEQ()
  }
  
  # Explicitly prevent foreach from exporting C++ pointers and the massive Stock object
  vars_to_exclude <- c("Stock", "Stock_Tune", if (exists("cpp_objects")) cpp_objects)
  safe_globals <- setdiff(ls(envir = globalenv()), vars_to_exclude)
  export_vars <- unique(c("General", "R0mult", "Seeds", "RunNo", safe_globals))
  
  results <- foreach(Isim = 1:General$Nsim, .packages = c("r4ss", "Rcpp", "RTMB", "RcppArmadillo"), .export = export_vars, .noexport = vars_to_exclude, .options.snow = list(preschedule = FALSE)) %dopar% {
    
    Worker_Step <- "Isolating Output Paths"
    tryCatch({
      
      # Worker Safety: Force the worker back to the root directory if it crashes
      on.exit(setwd(Path), add = TRUE)
      
      # Isolate outputs to a unique SimFolder
      SimFolder <- paste0(General$RunFolder, "Sim_", Isim, "/")
      if (!dir.exists(SimFolder)) dir.create(SimFolder, recursive = TRUE, showWarnings = FALSE)
      
      General$RunFolder <- SimFolder # This ensures Run_Tier1a nests SS_Sim inside SimFolder safely
      General$FolderResults <- SimFolder
      General$BiolSaveName <- paste0(SimFolder, "HistBiol_Sim", Isim, ".Out")
      General$ProjAllSaveName <- paste0(SimFolder, "ProjectAll_Sim", Isim, ".Out")
      General$ProjFleetSaveName <- paste0(SimFolder, "ProjectFleet_Sim", Isim, ".Out")
      General$ProjFleetAreaSaveName <- paste0(SimFolder, "ProjectFleetArea_Sim", Isim, ".Out")
      General$EstSaveName <- paste0(SimFolder, "Estimates_Sim", Isim, ".Out")
      General$ParSaveName <- paste0(SimFolder, "Parameters_Sim", Isim, ".Out")
      General$AllParSaveName <- paste0(SimFolder, "AllParameters_Sim", Isim, ".Out")
      General$InsuranceSaveName <- paste0(SimFolder, "ProjectIns_Sim", Isim, ".Out")
      General$MultiSaveName <- paste0(SimFolder, "ProjectMulti_Sim", Isim, ".Out")
      General$WASaveName <- paste0(SimFolder, "ProjectWA_Sim", Isim, ".Out")
      General$LogName <- paste0(SimFolder, "LogFile_Sim", Isim, ".Out")
      
      assign("General", General, envir = .GlobalEnv)
      assign("R0mult", R0mult, envir = .GlobalEnv)
      assign("Seeds", Seeds, envir = .GlobalEnv)
      if (exists("Path")) assign("Path", Path, envir = .GlobalEnv)
      
      set.seed(Seeds[Isim, 1])
      
      Worker_Step <- "Running SetSpec Locally"
      # Build the stock locally using isolated cache paths. This avoids zero-byte read crashes.
      Stock <- vector("list", length = General$Nstocks)
      for (Istock in 1:General$Nstocks) {
        Stock[[Istock]] <- suppressWarnings(SetSpec(Isim, Istock, General$Stock.Names[Istock], Seed = Seeds[Isim, Istock]))
        if (!is.na(Stock[[Istock]]$Target.Depletion)) {
          Stock[[Istock]]$R00 <- Stock[[Istock]]$R00 * R0mult[Istock]
          Stock[[Istock]]$R00.orig <- Stock[[Istock]]$R00.orig * R0mult[Istock]
          Stock[[Istock]]$R0 <- Stock[[Istock]]$R0 * R0mult[Istock]
        }
      }
      assign("Stock", Stock, envir = .GlobalEnv)
      
      Worker_Step <- "WriteBiol & WriteLog"
      for (Istock in 1:General$Nstocks) WriteBiol(Istock, General$BiolSaveName, General$FolderResults)
      for (Istock in 1:General$Nstocks) WriteLog(Isim, Istock, General$LogName)
      
      Worker_Step <- "Do.Project (Running Core Projections)"
      Do.Project(Isim)
      
      Worker_Step <- "Writing Final Outputs"
      for (Istock in 1:General$Nstocks) WriteProjFiles(Isim, Istock, General$ProjAllSaveName, General$ProjFleetSaveName, General$ProjFleetAreaSaveName)
      for (Istock in 1:General$Nstocks) WriteEst(Isim, Istock, General$EstSaveName, General$ParSaveName, General$AllParSaveName)
      for (Istock in 1:General$Nstocks) WriteInsurance(Isim, Istock, General$InsuranceSaveName)
      WriteMulti(Isim, General$MultiSaveName)
      for (Istock in 1:General$Nstocks) WriteWA(Isim, Istock, General$WASaveName)
      
      return(list(status = "SUCCESS", Isim = Isim, Files = c(General$ProjAllSaveName, General$ProjFleetSaveName, General$ProjFleetAreaSaveName, General$EstSaveName, General$ParSaveName, General$AllParSaveName, General$InsuranceSaveName, General$MultiSaveName, General$WASaveName, General$LogName)))
      
    }, error = function(e) { 
      # Capture stack trace for granular debugging if necessary
      calls <- sys.calls()
      trace_str <- paste(sapply(calls, function(x) paste(deparse(x), collapse = " ")), collapse = "\n  ")
      err_msg <- sprintf("\nCRASH AT STEP: '%s'\nError: %s\nTraceback:\n  %s\n", Worker_Step, e$message, trace_str)
      return(list(status = "ERROR", Isim = Isim, message = err_msg)) 
    })
  }
  
  if (UseParallel) parallel::stopCluster(cl)
  
  # Phase 3: Concatenate Files
  message("\nCombining isolated simulation output files back to Master...")
  master_files <- c(OrigFiles$ProjAll, OrigFiles$ProjFleet, OrigFiles$ProjFleetArea, OrigFiles$Est, OrigFiles$Par, OrigFiles$AllPar, OrigFiles$Insurance, OrigFiles$Multi, OrigFiles$WA, OrigFiles$Log)
  for (Isim in 1:General$Nsim) {
    if (inherits(results[[Isim]], "list") && results[[Isim]]$status == "SUCCESS") {
      sim_files <- results[[Isim]]$Files
      for (j in seq_along(master_files)) {
        if (file.exists(sim_files[j])) {
          file.append(master_files[j], sim_files[j])
          file.remove(sim_files[j])
        }
      }
    } else {
      cat("WARNING: Simulation", Isim, "failed or crashed.\n")
      cat(results[[Isim]]$message, "\n")
    }
  }
  message("All simulations complete.")
}