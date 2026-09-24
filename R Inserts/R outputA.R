library(r4ss)
mydirectory <- "C:/Research/NewRat/Temp/Assess_temp_8a/"

mymodelrun <- "SS_Sim_1_EX1a_55"
path <- paste0(mydirectory, mymodelrun)
#model0 <- SS_output(path,covar=T)
#SS_plots(replist=model0, png=T)

# create temporary directory and copy files into it
dir_prof <- file.path(paste0(path), "profile")


copy_SS_inputs(
  dir.old = path,
  dir.new = dir_prof,
  create.dir = TRUE,
  overwrite = TRUE,
  copy_par = TRUE,
  verbose = TRUE
)

# read starter file
starter <- SS_readstarter(file.path(path, "starter.ss"))
# change control file name in the starter file
starter[["ctlfile"]] <- "control_modified.ss"
# make sure the prior likelihood is calculated
# for non-estimated quantities
starter[["prior_like"]] <- 1
# write modified starter file
SS_writestarter(starter, dir = dir_prof, overwrite = TRUE)

# vector of values to profile over
M.vec <- seq(0.1, 0.4, 0.025)
Nprofile <- length(M.vec)
# run profile command
prof.table <- profile(
  dir = dir_prof,
  oldctlfile = "data.ctl",                      # This must be the control file for the run
  newctlfile = "control_modified.ss",
  string = "NatM_uniform_Fem_GP_1",
  profilevec = M.vec
)


# read the output files (with names like Report1.sso, Report2.sso, etc.)
profilemodels <- SSgetoutput(dirvec = dir_prof, keyvec = 1:Nprofile)
# summarize output
profilesummary <- SSsummarize(profilemodels)
SSplotProfile(profilesummary,profile.string = "NatM_uniform_Fem_GP_1",
              profile.label = "Natural mortlaity (M))",)

par(mfrow=c(2,2))
PinerPlot(profilesummary,component = "Catch_like",profile.string = "NatM_uniform_Fem_GP_1",profile.label = "Natural mortlaity (M))",main="Changes in catch likelihoods by fleet")
PinerPlot(profilesummary,component = "Surv_like",,profile.string = "NatM_uniform_Fem_GP_1",profile.label = "Natural mortlaity (M))",main="Changes in survey likelihoods by fleet")
PinerPlot(profilesummary,component = "Length_like",,profile.string = "NatM_uniform_Fem_GP_1",profile.label = "Natural mortlaity (M))",main="Changes in length likelihoods by fleet")
PinerPlot(profilesummary,component = "Age_like",,profile.string = "NatM_uniform_Fem_GP_1",profile.label = "Natural mortlaity (M))",main="Changes in age likelihoods by fleet")

AA Completed

# Stock-recruitment steepness
# ===========================

# vector of values to profile over
h.vec <- seq(0.3, 0.9, .1)
Nprofile <- length(h.vec)
# run profile command
prof.table <- profile(
  dir = dir_prof,
  oldctlfile = "data.ctl",
  newctlfile = "control_modified.ss",
  string = "steep", # subset of parameter label
  profilevec = h.vec
)

# read the output files (with names like Report1.sso, Report2.sso, etc.)
profilemodels <- SSgetoutput(dirvec = dir_prof, keyvec = 1:Nprofile)
# summarize output
profilesummary <- SSsummarize(profilemodels)
SSplotProfile(profilesummary)

par(mfrow=c(2,2))
PinerPlot(profilesummary,component = "Catch_like",profile.string = "steep",profile.label = "Steepness")
PinerPlot(profilesummary,component = "Surv_like",,profile.string = "steep",profile.label = "Steepness")
PinerPlot(profilesummary,component = "Length_like",,profile.string = "steep",profile.label = "Steepness")
PinerPlot(profilesummary,component = "Age_like",,profile.string = "steep",profile.label = "Steepness")


A
