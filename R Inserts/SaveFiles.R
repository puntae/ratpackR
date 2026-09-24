write.out.Gen <- function(Gen.obj,RunFolder4,RunType="Not Test",Reduced.Uncertain=0,Nproj=50,Narea=1)
{
  RunFolder1 <- paste0(RunFolder4,"")
  cat("Saving",RunFolder4,"\n")
  #print(str(OM.obj))
  
  
  write("# Generated general OM file",RunFolder4)
  write("# Run designator",RunFolder4,append=T)
  write("00",RunFolder4,append=T)
  write("# Number of species",RunFolder4,append=T)
  write("1",RunFolder4,append=T)
  write("# Stocks-per-species",RunFolder4,append=T)
  write("1",RunFolder4,append=T)
  write("# Species/stock names",RunFolder4,append=T)
  write(Gen.obj$Species2,RunFolder4,append=T)
  write("#Number_of_catch_fleets",RunFolder4,append=T)
  write(Gen.obj$nfleet.catch,RunFolder4,append=T)
  write("#Number_of_survey_fleets",RunFolder4,append=T)
  write(Gen.obj$nfleets-Gen.obj$nfleet.catch,RunFolder4,append=T)
  write("# Number of areas",RunFolder4,append=T)
  write(Narea,RunFolder4,append=T)
  write("# Fleet-species links (rows species, cols fleets)",RunFolder4,append=T)
  write(1,RunFolder4,append=T)
  write("#Fleet.names",RunFolder4,append=T)
  write(paste(Gen.obj$fleetnames),ncol=Gen.obj$nfleets,RunFolder4,append=T)
  write(paste0("#First_projection_year ",Gen.obj$endyr+1),RunFolder4,append=T)
  write("#TACs_apply_to_fleets",RunFolder4,append=T)
  write(paste(rep("Yes",Gen.obj$nfleets)),ncol=Gen.obj$nfleets,RunFolder4,append=T)
  write("#Multispecies",RunFolder4,append=T)
  write("No",RunFolder4,append=T)
  write("#Impose_profit_constraint_on_F No",RunFolder4,append=T)
  write("#Use.PGMSY No",RunFolder4,append=T)
  write("",RunFolder4,append=T)
  write("# Full run",RunFolder4,append=T)
  if (RunType!="Test")
   {
    write("#Number_of_simulations 100",RunFolder4,append=T)
    write(paste0("#Number_of_projection_years ",Nproj),RunFolder4,append=T)
    write("#Assessment_frequency\n 1",RunFolder4,append=T)
    write("#TestCase No",RunFolder4,append=T)
    write(paste0("#Reduced_uncertainty ",Reduced.Uncertain),RunFolder4,append=T)
   }
  if (RunType=="Test")
   {
    write("#Number_of_simulations 2",RunFolder4,append=T)
    write("#Number_of_projection_years 25",RunFolder4,append=T)
    write("#Assessment_frequency\n 15",RunFolder4,append=T)
    write("#TestCase No",RunFolder4,append=T)
    write("#Reduced_uncertainty 1",RunFolder4,append=T)
  }
  
 write("\n#Delay-for-dats-sets: Index, Discard, Length, marginal age, caa",RunFolder4,append=T)
 write("                      0      0        0       0             0 ",RunFolder4,append=T)  

  
}

write.out.Gen2 <- function(Gen.obj,RunFolder4,RunType="Not Test",Spec.Names,MetierNames,WithTACs,Yr1=2024,
                           Reduced.Uncertain=0,Nproj=50,N.catch.metiers,
                           PGMSY.Primary.Species.by.fleet,PGMSY.Primary.Species,
                           Indicator.Primary.Species,Assessment.Frequency)
{
  RunFolder1 <- paste0(RunFolder4,"")
  cat("Saving",RunFolder4,"\n")
  #print(str(OM.obj))
  Nmetiers <- length(MetierNames)
  Nspec <- length(Spec.Names)
  
  write("# Generated general OM file",RunFolder4)
  write("# Run designator",RunFolder4,append=T)
  write("00",RunFolder4,append=T)
  write("# Number of species",RunFolder4,append=T)
  write(Nspec,RunFolder4,append=T)
  write("# Stocks-per-species",RunFolder4,append=T)
  write(rep(1,Nspec),RunFolder4,append=T)
  write("# Species/stock names",RunFolder4,append=T)
  write(paste(Spec.Names),RunFolder4,append=T)
  write("#Number_of_catch_fleets",RunFolder4,append=T)
  write(N.catch.metiers,RunFolder4,append=T)
  write("#Number_of_survey_fleets",RunFolder4,append=T)
  write(Nmetiers-N.catch.metiers,RunFolder4,append=T)
  write("# Number of areas",RunFolder4,append=T)
  write(Gen.obj$nareas,RunFolder4,append=T)
  write("# Fleet-species links (rows species, cols fleets)",RunFolder4,append=T)
  write(1,RunFolder4,append=T)
  write("#Fleet.names",RunFolder4,append=T)
  write(paste(MetierNames),ncol=Gen.obj$nfleets,RunFolder4,append=T)
  write(paste0("#First_projection_year ",Yr1),RunFolder4,append=T)
  write("#TACs_apply_to_fleets",RunFolder4,append=T)
  write(paste0(WithTACs),ncol=Nmetiers,RunFolder4,append=T)
  write("#Multispecies",RunFolder4,append=T)
  write("Yes",RunFolder4,append=T)
  write("#Impose_profit_constraint_on_F No",RunFolder4,append=T)
  write("#Multispecies.Wght1 100",RunFolder4,append=T)
  write("#Multispecies.Wght2 1000",RunFolder4,append=T)
  write("#Multispecies.Wght3 1",RunFolder4,append=T)
  write("",RunFolder4,append=T)
  write("#PGMSY_Primary_Species",RunFolder4,append=T)
  write(PGMSY.Primary.Species,ncol=Nspec,RunFolder4,append=T)
  write("#PGMSY.Limit 0.25",RunFolder4,append=T)
  write("#PGMSY.Linked_Species_by_fleet",RunFolder4,append=T)
  write(PGMSY.Primary.Species.by.fleet,ncol=N.catch.metiers,RunFolder4,append=T)
  write("",RunFolder4,append=T)
  write("#Indicator_Primary_Species",RunFolder4,append=T)
  write(Indicator.Primary.Species,ncol=Nspec,RunFolder4,append=T)
  write("",RunFolder4,append=T)
  write("#Indicator_Final_TACs",RunFolder4,append=T)
  write(t(Last.TAC.specs),ncol=3,RunFolder4,append=T)
  write("",RunFolder4,append=T)
  
  write("# Full run",RunFolder4,append=T)
  if (RunType!="Test")
  {
    write("#Number_of_simulations 100",RunFolder4,append=T)
    write(paste0("#Number_of_projection_years ",Nproj),RunFolder4,append=T)
    write(c("#Assessment_frequency",rep(1,Nstocks)),RunFolder4,append=T)
    write("#TestCase No",RunFolder4,append=T)
    write(paste0("#Reduced_uncertainty ",Reduced.Uncertain),RunFolder4,append=T)
  }
  if (RunType=="Test")
  {
    write("#Number_of_simulations 2",RunFolder4,append=T)
    write("#Number_of_projection_years 25",RunFolder4,append=T)
    write(c("#Assessment_frequency",Assessment.Frequency),RunFolder4,append=T)
    write("#TestCase No",RunFolder4,append=T)
    write("#Reduced_uncertainty 1",RunFolder4,append=T)
  }
  write("\n#Delay-for-dats-sets: Index, Discard, Length, marginal age, caa",RunFolder4,append=T)
  write("                      0      0        0       0             0 ",RunFolder4,append=T)  
  
}
#=================================================================================================================

write.out.OM <- function(OM.obj,RunFolder2,Control.Rule.Type)
{
  RunFolder2 <- paste0(RunFolder2,"")
  cat(RunFolder2,"\n")
  #print(str(OM.obj))

  styr <- OM.obj$styr
  endyr <- OM.obj$endyr
  nsex <- OM.obj$nsex
  nsexUse <- OM.obj$nsexUse
  nages <- OM.obj$nages
  nyears <- OM.obj$nyears
  nyear1 <- OM.obj$nyear1
  nlen <- OM.obj$nlen
  lbins <- OM.obj$lbins
  nfleets <- OM.obj$nfleets
  nfleet.catch  <- OM.obj$nfleet.catch
  Default.Proj <- OM.obj$Default.Proj
  FillProjection <- OM.obj$FillProjection
  if (FillProjection==F)  Default.Proj <- 0
  cat(Default.Proj,FillProjection,"\n")
  OM.OBJ <<- OM.obj
  ctl <- NULL
  dat <- NULL
  model0 <- NULL
  
  write("# Generated OM file",RunFolder2)
  write(paste0("# First projection year\n",OM.obj$endyr+1),RunFolder2,append=T)
  write(paste0("# Maximum age\n",nages),RunFolder2,append=T)
  write(paste0("# Number of sexes\n",nsex),RunFolder2,append=T)
  write(paste0("# Number of sexes(V2)\n",nsexUse),RunFolder2,append=T)
  write(paste0("# Number of historical years\n",nyear1-1),RunFolder2,append=T)
  
  if (OM.obj$nareas>1)
   {
    write("# Relative density spatially",RunFolder2,append=T)
    write(OM.obj$Rel.Dens,ncol=length(OM.obj$Rel.Dens),RunFolder2,append=T)
    write("\n",RunFolder2,append=T)
    
    write("# Spatial mode inputs",RunFolder2,append=T)
    write("#Spatial:Movement_rates",RunFolder2,append=T)
    write(paste0("#",seq(from=0,to=OM.obj$nages-1)),ncol=nages+1,RunFolder2,append=T)
    write(t(OM.obj$movement.rate),ncol=nages,RunFolder2,append=T)
   
    write("#Spatial:Closed_areas (in future)",RunFolder2,append=T)
    write("#Spatial:N_Closed_areas 0",RunFolder2,append=T)
    write("#Spatial:Close_area_specs (area, year closed; year open)",RunFolder2,append=T)  
    write("\n",RunFolder2,append=T)
    
    write("#Spatial:Open_fleets",RunFolder2,append=T)
    write(t(OM.obj$area.fleets),ncol=nfleets,RunFolder2,append=T)
    write("\n",RunFolder2,append=T)
  }
  
  write(paste0("# Number of length bins\n",nlen),RunFolder2,append=T)
  write("# lower length of length bins",RunFolder2,append=T)
  lbins <- c(lbins,2*lbins[nlen]-lbins[nlen-1])
  lbins <- OM.obj$lbins
  write(lbins,RunFolder2,append=T,ncol=nlen+1)
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
  
  write("#M.type",RunFolder2,append=T)
  write(OM.obj$natM_type,RunFolder2,append=T)
  
  write("# Base M",RunFolder2,append=T)
  for (Isex in 1:nsex)
  write(OM.obj$Mvals[Isex,],ncol=nages,RunFolder2,append=T)
  
  write("\n#Growth_model",RunFolder2,append=T)
  write(OM.obj$Growth_model,RunFolder2,append=T)
  write("#Growth parameters",RunFolder2,append=T)
  write("#Growth:min/max age",RunFolder2,append=T)
  write(c(OM.obj$Age_for_L1,OM.obj$Age_for_L2),RunFolder2,append=T)
  write("#Growth:L_at_Amin by sex",RunFolder2,append=T)
  write(OM.obj$LAmin,ncol=length(OM.obj$LAmin),RunFolder2,append=T)
  write("#Growth:L_at_Amax by sex ",RunFolder2,append=T)
  write(OM.obj$LAmax,ncol=length(OM.obj$LAmax),RunFolder2,append=T)
  write("#Growth:k by sex = ",RunFolder2,append=T)
  write(OM.obj$Kappa,ncol=length(OM.obj$Kappa),RunFolder2,append=T)
  if (OM.obj$Growth_model==2)
   {
    write("#Growth:Richards by sex ",RunFolder2,append=T)
    write(OM.obj$Richards,ncol=length(OM.obj$Richards),RunFolder2,append=T)
   }
  write("#Growth:CV1 age 0 by sex",RunFolder2,append=T)
  write(OM.obj$CVyoung,ncol=length(OM.obj$CVyoung),RunFolder2,append=T)
  write("#Growth:CV2 age max by sex ",RunFolder2,append=T)
  write(OM.obj$CVold,ncol=length(OM.obj$CVold),RunFolder2,append=T)
  write("\n#Growth:Length-weight relationship ",RunFolder2,append=T)
  write("#Growth:weight-len_a by sex ",RunFolder2,append=T)
  write(OM.obj$Wtlen_1,ncol=length(OM.obj$Wtlen_2),RunFolder2,append=T)
  write("#Growth:weight-len_b by sex ",RunFolder2,append=T)
  write(OM.obj$Wtlen_2,ncol=length(OM.obj$Wtlen_2),RunFolder2,append=T)
  
  write("#Growth:maturity_option  ",RunFolder2,append=T)
  write(OM.obj$maturity_option,RunFolder2,append=T)
  if (OM.obj$maturity_option==4)
   {
    write("#_Age_Fecundity by growth pattern",RunFolder2,append=T)
    write(OM.obj$age_maturity,ncol=length(OM.obj$age_maturity),RunFolder2,append=T)
   }
  write("#Growth:first_mature_age  ",RunFolder2,append=T)
  write(OM.obj$first_mat_age,RunFolder2,append=T)
  write("#Growth:maturity_a50  ",RunFolder2,append=T)
  write(OM.obj$Mat50,ncol=length(OM.obj$Mat50),RunFolder2,append=T)
  write("#Growth:maturity_slope ",RunFolder2,append=T)
  write(OM.obj$Mat_Slope,ncol=length(OM.obj$Mat_Slope),RunFolder2,append=T)
  write("#Growth:Fecundity.option ",RunFolder2,append=T)
  write(OM.obj$fecundity_option,RunFolder2,append=T)
  write("#Growth:Egg_1  ",RunFolder2,append=T)
  write(OM.obj$Egg_1,ncol=length(OM.obj$Egg_1),RunFolder2,append=T)
  write("#Growth:Egg_2  ",RunFolder2,append=T)
  write(OM.obj$Egg_2,ncol=length(OM.obj$Egg_2),RunFolder2,append=T)
  
  
  write("#Growth:CVoption ",RunFolder2,append=T)
  write(OM.obj$CV_Growth_Pattern,RunFolder2,append=T)
  
  write("#Growth:Time-varying.N",RunFolder2,append=T)
  write(length(OM.obj$TimeVarying[,1]),RunFolder2,append=T)
  write("#Growth:Time-varying",RunFolder2,append=T)
  if (length(OM.obj$TimeVarying) >0)
   {
    write(colnames(OM.obj$TimeVarying)[-2],ncol=length(OM.obj$TimeVarying[1,-2]),RunFolder2,append=T)
    for (Iline in 1:length(OM.obj$TimeVarying[,1]))
     {
       TimeVar <- (as.numeric(OM.obj$TimeVarying[Iline,-2]));TimeVar[1] <- TimeVar[1]-styr+1
       write(TimeVar,ncol=length(TimeVar),RunFolder2,append=T)
      }
    }
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
  
  write("# Stock and recruitment",RunFolder2,append=T)
  write(paste0("#SR:R0 ",OM.obj$R0),RunFolder2,append=T)
  write(paste0("#SR:Steepness ",OM.obj$Steep),RunFolder2,append=T)
  write(paste0("#SR:SigmaR ",OM.obj$SigmaR),RunFolder2,append=T)
  write(paste0("#SR:ProwR ",OM.obj$auto),RunFolder2,append=T)
  write("",RunFolder2,append=T) 
  write("#Early_devs",RunFolder2,append=T)
  write(OM.obj$Early.devs,RunFolder2,append=T,ncol=nages) 
 if (length(OM.obj$OM.Early_R0Mult) > 0)
   {
    write(paste0("#Early_R0Mult ",OM.obj$OM.Early_R0Mult),RunFolder2,append=T)
    write(paste0("#Early_R0Mult_Block_end ",OM.obj$OM.Early_Early_R0Mult_Block_end[1]," ",OM.obj$OM.Early_Early_R0Mult_Block_end[2]),ncol=2,RunFolder2,append=T)
    }  
  write(paste("#Late_Devs",1,length(OM.obj$Late.devs)),RunFolder2,append=T)
  write(OM.obj$Late.devs,ncol=length(OM.obj$Late.devs),RunFolder2,append=T)
  
  write("#SR:Use_Steepness_Equi",RunFolder2,append=T)
  write(OM.obj$Use_steep_init_equi,RunFolder2,append=T)
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
  
  write("#Selex:Size_Initial_fem",RunFolder2,append=T)
  write(t(OM.obj$Size_Initial.sel_fem),RunFolder2,append=T,ncol=nlen)
  write("#Selex:Size_Nblocks_fem",RunFolder2,append=T)
  write(OM.obj$Size_Nblocks.selex_fem,RunFolder2,append=T,ncol=nfleets)
  write("#Selex:Size_Blocks_fem",RunFolder2,append=T)
  if (sum(OM.obj$Size_Nblocks.selex_fem)>0) write(t(OM.obj$Size_Blocks.selex_fem-styr+1),RunFolder2,append=T,ncol=2)
  write("#Selex:Size_Block.selex_fem",RunFolder2,append=T)
  if (sum(OM.obj$Size_Nblocks.selex_fem)>0) write(t(OM.obj$Size_BlockSels.selex_fem),RunFolder2,append=T,ncol=nlen)
  
  write("#Retain:Size_Initial_fem",RunFolder2,append=T)
  write(t(OM.obj$Size_Initial.ret_fem[1:nfleet.catch,]),RunFolder2,append=T,ncol=nlen)
  write("#Retain:Size_Nblocks_fem",RunFolder2,append=T)
  write(OM.obj$Size_Nblocks.retain_fem,RunFolder2,append=T,ncol=nfleets)
  write("#Retain:Size_Blocks_fem",RunFolder2,append=T)
  if (sum(OM.obj$Size_Nblocks.retain_fem)>0) write(t(OM.obj$Size_Blocks.retain_fem-styr+1),RunFolder2,append=T,ncol=2)
  write("#Retain:Size_Block.selex_fem",RunFolder2,append=T)
  if (sum(OM.obj$Size_Nblocks.retain_fem)>0) write(t(OM.obj$Size_BlockSels.retain_fem),RunFolder2,append=T,ncol=nlen)
  
  write("#Mort:Size_Initial_fem",RunFolder2,append=T)
  write(t(OM.obj$Size_Initial.mort_fem[1:nfleet.catch,]),RunFolder2,append=T,ncol=nlen)
  
  
  write("#Selex:Age_Initial_fem",RunFolder2,append=T)
  write(t(OM.obj$Age_Initial.sel_fem),RunFolder2,append=T,ncol=nages)
  write("#Selex:Age_Nblocks_fem",RunFolder2,append=T)
  write(OM.obj$Age_Nblocks.selex_fem,RunFolder2,append=T,ncol=nfleets)
  write("#Selex:Age_Blocks_fem",RunFolder2,append=T)
  if (sum(OM.obj$Age_Nblocks.selex_fem)>0) write(t(OM.obj$Age_Blocks.selex_fem-styr+1),RunFolder2,append=T,ncol=2)
  write("#Selex:Age_Block.selex_fem",RunFolder2,append=T)
  if (sum(OM.obj$Age_Nblocks.selex_fem)>0) write(t(OM.obj$Age_BlockSels.selex_fem),RunFolder2,append=T,ncol=nages)

  write("#Retain:Age_Initial_fem",RunFolder2,append=T)
  write(t(OM.obj$Age_Initial.ret_fem[1:nfleet.catch,]),RunFolder2,append=T,ncol=nages)
  write("#Retain:Age_Nblocks_fem",RunFolder2,append=T)
  write(OM.obj$Age_Nblocks.retain_fem,RunFolder2,append=T,ncol=nfleets)
  write("#Retain:Age_Blocks_fem",RunFolder2,append=T)
  if (sum(OM.obj$Age_Nblocks.retain_fem)>0) write(t(OM.obj$Age_Blocks.retain_fem-styr+1),RunFolder2,append=T,ncol=2)
  write("#Retain:Age_Block.selex_fem",RunFolder2,append=T)
  if (sum(OM.obj$Age_Nblocks.retain_fem)>0) write(t(OM.obj$Age_BlockSels.retain_fem),RunFolder2,append=T,ncol=nages)
  
  write("#Mort:Age_Initial_fem",RunFolder2,append=T)
  write(t(OM.obj$Age_Initial.mort_fem[1:nfleet.catch,]),RunFolder2,append=T,ncol=nages)
  
  if (nsex > 1)
   {
    write("#Selex:Size_Initial_mal",RunFolder2,append=T)
    write(t(OM.obj$Size_Initial.sel_mal),RunFolder2,append=T,ncol=nlen)
    write("#Selex:Size_Nblocks_mal",RunFolder2,append=T)
    write(OM.obj$Size_Nblocks.selex_mal,RunFolder2,append=T,ncol=nfleets)
    write("#Selex:Size_Blocks_mal",RunFolder2,append=T)
    if (sum(OM.obj$Size_Nblocks.selex_mal)>0) write(t(OM.obj$Size_Blocks.selex_mal-styr+1),RunFolder2,append=T,ncol=2)
    write("#Selex:Size_Block.selex_mal",RunFolder2,append=T)
    if (sum(OM.obj$Size_Nblocks.selex_mal)>0) write(t(OM.obj$Size_BlockSels.selex_mal),RunFolder2,append=T,ncol=nlen)
 
    write("#Retain:Size_Initial_mal",RunFolder2,append=T)
    write(t(OM.obj$Size_Initial.ret_mal[1:nfleet.catch,]),RunFolder2,append=T,ncol=nlen)
    write("#Retain:Size_Nblocks_mal",RunFolder2,append=T)
    write(OM.obj$Size_Nblocks.retain_mal,RunFolder2,append=T,ncol=nfleets)
    write("#Retain:Size_Blocks_mal",RunFolder2,append=T)
    if (sum(OM.obj$Size_Nblocks.retain_mal)>0) write(t(OM.obj$Size_Blocks.retain_mal-styr+1),RunFolder2,append=T,ncol=2)
    write("#Retain:Size_Block.selex_mal",RunFolder2,append=T)
    if (sum(OM.obj$Size_Nblocks.retain_mal)>0) write(t(OM.obj$Size_BlockSels.retain_mal),RunFolder2,append=T,ncol=nlen)
  
    write("#Mort:Size_Initial_mal",RunFolder2,append=T)
    write(t(OM.obj$Size_Initial.mort_mal[1:nfleet.catch,]),RunFolder2,append=T,ncol=nlen)
  
    write("#Selex:Age_Initial_mal",RunFolder2,append=T)
    write(t(OM.obj$Age_Initial.sel_mal),RunFolder2,append=T,ncol=nages)
    write("#Selex:Age_Nblocks_mal",RunFolder2,append=T)
    write(OM.obj$Age_Nblocks.selex_mal,RunFolder2,append=T,ncol=nfleets)
    write("#Selex:Age_Blocks_mal",RunFolder2,append=T)
    if (sum(OM.obj$Age_Nblocks.selex_mal)>0) write(t(OM.obj$Age_Blocks.selex_mal-styr+1),RunFolder2,append=T,ncol=2)
    write("#Selex:Age_Block.selex_mal",RunFolder2,append=T)
    if (sum(OM.obj$Age_Nblocks.selex_mal)>0) write(t(OM.obj$Age_BlockSels.selex_mal),RunFolder2,append=T,ncol=nages)
  
    write("#Retain:Age_Initial_mal",RunFolder2,append=T)
    write(t(OM.obj$Age_Initial.ret_mal[1:nfleet.catch,]),RunFolder2,append=T,ncol=nages)
    write("#Retain:Age_Nblocks_mal",RunFolder2,append=T)
    write(OM.obj$Age_Nblocks.retain_mal,RunFolder2,append=T,ncol=nfleets)
    write("#Retain:Age_Blocks_mal",RunFolder2,append=T)
    if (sum(OM.obj$Age_Nblocks.retain_mal)>0) write(t(OM.obj$Age_Blocks.retain_mal-styr+1),RunFolder2,append=T,ncol=2)
    write("#Retain:Age_Block.selex_mal",RunFolder2,append=T)
    if (sum(OM.obj$Age_Nblocks.retain_mal)>0) write(t(OM.obj$Age_BlockSels.retain_mal),RunFolder2,append=T,ncol=nages)
  
    write("#Mort:Age_Initial_mal",RunFolder2,append=T)
    write(t(OM.obj$Age_Initial.mort_mal[1:nfleet.catch,]),RunFolder2,append=T,ncol=nages)
  }
  
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
  write(paste0("#Maximum_F ",OM.obj$maxF),RunFolder2,append=T)
  
  write("#Use_Initial_F T",RunFolder2,append=T)
  write("#Initial_F ",RunFolder2,append=T)
  write(OM.obj$Initial_F,RunFolder2,append=T,ncol=nfleets)
  
  # =======================================================================================================================
  
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
 
  write(paste0("#Env_links ",OM.obj$N.env.link),RunFolder2,append=T)
  if (OM.obj$N.env.link>0) write(t(OM.obj$Link.vars),ncol=3,RunFolder2,append=T)
  write(paste0("#Env_vars ",OM.obj$Env.vars),RunFolder2,append=T)
  write(paste0("#EnvIndexOverRide ",OM.obj$Env.override),RunFolder2,append=T)
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
  
  write("#Econ_parameters",RunFolder2,append=T)
  for (Ifleet in 1:nfleet.catch)  write(paste0("1 1 1 1  # Fleet",Ifleet),RunFolder2,append=T)
  
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
  write("#Use.Implementation.Err 0",RunFolder2,append=T)
  
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
  
  write("#Assessment:Bound_mult 2 # Bound range for estimated parameters",RunFolder2,append=T)
  write("#Assessment:Jitter_SD 0.1 # Jitter values ",RunFolder2,append=T)
  
  write("\n# ==================================================================================================================================================================================================\n",RunFolder2,append=T)
  
  write("#Catches:Project_type (1= catches; 2=F; 3=Pre-spcified F)" ,RunFolder2,append=T)
  write(OM.obj$project.type,RunFolder2,append=T)
  write("#Catches:Eqilibrium",RunFolder2,append=T)
  write(t(OM.obj$eqn.catch),ncol=nfleet.catch,RunFolder2,append=T)
  write("#Catches:Historical",RunFolder2,append=T)
  write(t(OM.obj$hist.catch),ncol=1+nfleet.catch,RunFolder2,append=T)
  
  write("\n# ==================================================================================================================================================================================================\n",RunFolder2,append=T)
  
  if (FillProjection==T) write("#Extended_Data Yes",RunFolder2,append=T) else write("#Extended_Data No",RunFolder2,append=T)
  write(c("#Extended_data_length ",Default.Proj,"\n"),ncol=3,RunFolder2,append=T)

  write("#Catch_data" ,RunFolder2,append=T)
  write("#Catch:Base_CV" ,RunFolder2,append=T)
  write(rep(0,nfleets),RunFolder2,append=T,ncol=nfleets)
  write("#Catch:Base_Bias" ,RunFolder2,append=T)
  for (Ifleet in 1:nfleets)
    write(c(3,styr,1,endyr+1,1,endyr+100,1),ncol=7,RunFolder2,append=T) 

  write("\n# ==================================================================================================================================================================================================\n",RunFolder2,append=T)
  
  write("#Index_data" ,RunFolder2,append=T)
  write("#Index:Is_index" ,RunFolder2,append=T)
  write(OM.obj$index.Use,RunFolder2,append=T,ncol=nfleets)
  write("#Index:Base_CVs_past" ,RunFolder2,append=T)
  write(OM.obj$index.CV.past,RunFolder2,append=T,ncol=nfleets)
  write("#Index:Base_CVs_future" ,RunFolder2,append=T)
  write(OM.obj$index.CV.fut,RunFolder2,append=T,ncol=nfleets)
  write("#Index:Frequency_future" ,RunFolder2,append=T)
  write(OM.obj$index.freq,RunFolder2,append=T,ncol=nfleets)
  write("#Index:q" ,RunFolder2,append=T)
  write(OM.obj$index.q,RunFolder2,append=T,ncol=nfleets)
  write("##Index:Beta" ,RunFolder2,append=T)
  write(rep(1,length(OM.obj$index.Use)),RunFolder2,append=T,ncol=nfleets)
  write("##Index:Qinc.Yr1" ,RunFolder2,append=T)
  write(rep(styr,length(OM.obj$index.Use)),RunFolder2,append=T,ncol=nfleets)
  write("#Index:Qinc.Rate" ,RunFolder2,append=T)
  write(rep(0,length(OM.obj$index.Use)),RunFolder2,append=T,ncol=nfleets)
  write("#Index:CV.bias" ,RunFolder2,append=T)
  write(rep(1,length(OM.obj$index.Use)),RunFolder2,append=T,ncol=nfleets)
  write("#Index:Frequency_past (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
  write(t(OM.obj$index.tab),ncol=1+nfleets,RunFolder2,append=T)
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
 
  write("#Discard_data" ,RunFolder2,append=T)
  write("#Discard:Is_data" ,RunFolder2,append=T)
  write(OM.obj$discard.Use,RunFolder2,append=T,ncol=nfleets)
  write("#Discard:type (1=Biomass; 2=proportion)?" ,RunFolder2,append=T)
  write(OM.obj$discard.type,RunFolder2,append=T,ncol=nfleets)
  write("#Discard:Base_CVs_past" ,RunFolder2,append=T)
  write(OM.obj$discard.CV.past,RunFolder2,append=T,ncol=nfleets)
  write("#Discard:Base_CVs_future" ,RunFolder2,append=T)
  write(OM.obj$discard.CV.fut,RunFolder2,append=T,ncol=nfleets)
  write("#Discard:Frequency_future" ,RunFolder2,append=T)
  write(OM.obj$discard.freq,RunFolder2,append=T,ncol=nfleets)
  write("#Discard:Frequency_past (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
  write(t(OM.obj$discard.tab),ncol=1+nfleet.catch,RunFolder2,append=T)
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
  
  # Process the length data
  write("#Length_data" ,RunFolder2,append=T)
  write("#Length:Is_length" ,RunFolder2,append=T)
  write(OM.obj$length.Use,RunFolder2,append=T,ncol=nfleets)
  write("#Length:Base_EFN_past (retained, discarded, total) " ,RunFolder2,append=T)
  write(t(OM.obj$length.ESS.past),RunFolder2,append=T,ncol=nfleets)
  write("#Length:Base_EFN_future (retained, discarded) " ,RunFolder2,append=T)
  write(t(OM.obj$length.ESS.fut),RunFolder2,append=T,ncol=nfleets)
  write("#Length:Frequency_future (retained, discarded) " ,RunFolder2,append=T)
  write(t(OM.obj$length.freq),RunFolder2,append=T,ncol=nfleets)
  
  for (Ipart in 1:3)
  {
    if (Ipart==1) write("#Length:Frequency_past_retained (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
    if (Ipart==2) write("#Length:Frequency_past_discarded (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
    if (Ipart==3) write("#Length:Frequency_past_total (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
    
    for (Iyear in 1:(nyears+Default.Proj))  
      write(c(Iyear+styr-1,OM.obj$length.tab[Ipart,Iyear,]),ncol=1+nfleets,RunFolder2,append=T)
    write("" ,RunFolder2,append=T)
  }
  
  # Process the age data
  write("#Age_data" ,RunFolder2,append=T)
  write("#Age:Is_age" ,RunFolder2,append=T)
  write(OM.obj$age.Use,RunFolder2,append=T,ncol=nfleets)
  write("#Age:Base_EFN_past (retained, discarded, total) " ,RunFolder2,append=T)
  write(t(OM.obj$age.ESS.past),RunFolder2,append=T,ncol=nfleets)
  write("#Age:Base_EFN_future (retained, discarded) " ,RunFolder2,append=T)
  write(t(OM.obj$age.ESS.fut),RunFolder2,append=T,ncol=nfleets)
  write("#Age:Frequency_future (retained, discarded) " ,RunFolder2,append=T)
  write(t(OM.obj$age.freq),RunFolder2,append=T,ncol=nfleets)
  
  for (Ipart in 1:3)
  {
    if (Ipart==1) write("#Age:Frequency_past_retained (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
    if (Ipart==2) write("#Age:Frequency_past_discarded (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
    if (Ipart==3) write("#Age:Frequency_past_total (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
    
    for (Iyear in 1:(nyears+Default.Proj))  
      write(c(Iyear+styr-1,OM.obj$age.tab[Ipart,Iyear,]),ncol=1+nfleets,RunFolder2,append=T)
    write("" ,RunFolder2,append=T)
  }
  
  # Process the conditional age-at-length data
  write("#CAA_data" ,RunFolder2,append=T)
  write("#CAA:Is_CAA" ,RunFolder2,append=T)
  write(OM.obj$caa.Use,RunFolder2,append=T,ncol=nfleets)
  write("#CAA:Base_EFN_past (retained, discarded, total) " ,RunFolder2,append=T)
  write(t(OM.obj$caa.ESS.past),RunFolder2,append=T,ncol=nfleets)
  write("#CAA:Base_EFN_future (retained, discarded) " ,RunFolder2,append=T)
  write(t(OM.obj$caa.ESS.fut),RunFolder2,append=T,ncol=nfleets)
  write("#CAA:Frequency_future (retained, discarded) ",RunFolder2,append=T)
  write(t(OM.obj$caa.freq),RunFolder2,append=T,ncol=nfleets)
  
  for (Ipart in 1:3)
  {
    if (Ipart==1) write("#CAA:Frequency_past_retained (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
    if (Ipart==2) write("#CAA:Frequency_past_discarded (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
    if (Ipart==3) write("#CAA:Frequency_past_total (-1 no, 1 base, 2 double etc)" ,RunFolder2,append=T)
    
    for (Iyear in 1:(nyears+Default.Proj))  
      write(c(Iyear+styr-1,OM.obj$caa.tab[Ipart,Iyear,]),ncol=1+nfleets,RunFolder2,append=T)
    write("" ,RunFolder2,append=T)
  }
  
  # Process the ageing error data
  if (OM.OBJ$N_ageerror_definitions >0)
   {
    write("#age_reading_error Yes" ,RunFolder2,append=T)
    write(paste0(c("#n.age_reading_error ",paste(OM.OBJ$N_ageerror_definitions))),ncol=3,RunFolder2,append=T)
    write("#age_reading_error_means" ,RunFolder2,append=T)
    write(t(OM.obj$age_error_mean) ,ncol=OM.OBJ$N_ageerror_definitions+1,RunFolder2,append=T)
    write("#age_reading_error_sds" ,RunFolder2,append=T)
    write(t(OM.obj$age_error_sd) ,ncol=OM.OBJ$N_ageerror_definitions+1,RunFolder2,append=T)
   }
  else
    write("#age_reading_error No" ,RunFolder2,append=T)
  write("" ,RunFolder2,append=T)
  
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)
  
  write("#Pred:Number_of_predators",RunFolder2,append=T) 
  write(0,RunFolder2,append=T) 

  write(paste0("\n#EnvIndex:Num ",OM.obj$EnvIndex.Num),RunFolder2,append=T) 
  if (OM.obj$EnvIndex.Num > 0)
    for (Ienv.index in 1:OM.obj$EnvIndex.Num)
    {
      write(paste0("#EnvIndex:",Ienv.index),RunFolder2,append=T) 
      write(t(OM.obj$env.data[Ienv.index,,]),ncol=3,RunFolder2,append=T) 
    }  
  
  write("\n# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------\n",RunFolder2,append=T)

  # Output catastrophic mortality
  write(paste0("\n#Catastrophic_mortality"),RunFolder2,append=T) 
  write(t(OM.obj$Cata.mort),ncol=3,RunFolder2,append=T) 
  
  
  
  
  if (!(Control.Rule.Type %in% c("CSIRO","NPFMC","WA"))) { cat("Default OM HCR is missing - variable Control.Rule.Type"); stop() }
  
  if (Control.Rule.Type=="CSIRO")
  {
    write("",RunFolder2,append=T)
    write("#MSY 3",RunFolder2,append=T)
    write("#SPR_target 0.48",RunFolder2,append=T)
    write("#Biomass_target 0.48",RunFolder2,append=T)
    write("#Forecast_type 3",RunFolder2,append=T)
    write("#Control_rule_inflection 0.35",RunFolder2,append=T)
    write("#Control_rule_Biomass_level_for_no_F 0.2",RunFolder2,append=T)
    write("#Control_rule_Protection_level_for_no_F 0",RunFolder2,append=T)
    write("#Buffer 1.0",RunFolder2,append=T)
    write("",RunFolder2,append=T)
  }
  if (Control.Rule.Type=="NPFMC")
  {
    write("",RunFolder2,append=T)
    write("#MSY 3",RunFolder2,append=T)
    write("#SPR_target 0.40",RunFolder2,append=T)
    write("#Biomass_target 0.40",RunFolder2,append=T)
    write("#Forecast_type 3",RunFolder2,append=T)
    write("#Control_rule_inflection 0.40",RunFolder2,append=T)
    write("#Control_rule_Biomass_level_for_no_F 0.02",RunFolder2,append=T)
    write("#Control_rule_Protection_level_for_no_F 0.2",RunFolder2,append=T)
    write("",RunFolder2,append=T)
  }
  if (Control.Rule.Type=="WA")
   {
    write("",RunFolder2,append=T)
    write("#MSY 3",RunFolder2,append=T)
    write("#SPR_target 0.48",RunFolder2,append=T)
    write("#Biomass_target 0.48",RunFolder2,append=T)
    write("#Forecast_type 3",RunFolder2,append=T)
    write("#Control_rule_inflection 0.30",RunFolder2,append=T)
    write("#Control_rule_Biomass_level_for_no_F 0.20",RunFolder2,append=T)
    write("#Control_rule_Protection_level_for_no_F 0",RunFolder2,append=T)
    write("",RunFolder2,append=T)
   }
  
}

##================================================================================================================================
##================================================================================================================================


write.out.EM <- function(OM.obj,EM.obj,RunFolder3,Control.Rule.Type,Byproduct.species="No")
{
  RunFolder3 <- paste0(RunFolder3,"")
  cat(RunFolder3,"\n")
  #print(str(OM.obj))
  
  styr <- OM.obj$styr
  endyr <- OM.obj$endyr
  nsex <- OM.obj$nsex
  nages <- OM.obj$nages
  nyears <- OM.obj$nyears
  nyear1 <- OM.obj$nyear1
  nlen <- OM.obj$nlen
  lbins <- OM.obj$lbins
  nfleets <- OM.obj$nfleets
  nfleet.catch  <- OM.obj$nfleet.catch
  OM.OBJ <<- OM.obj
  EM.OBJ <<- EM.obj
  ctl <- NULL
  dat <- NULL
  model0 <- NULL

  write("# Generated EM file",RunFolder3)
  if (CaseAll==0)
   {
    write("#Ass_Basic:Type Tier_0",RunFolder3,append=T)     # here for testing
    write("##Ass_Basic:Type Tier_1a",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_4a (DT4)",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_4b (Tier 4)",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_5 (0: Zero catch; 1: Average catch; 21: ABC+HCR 1: Tier 3 HCR; 22: ABC+HCR 2: Lagged recovery; 23: ABC+HCR 3: Long-term resilience; 25: ABC+HCR 5: Maximize productivity; 27: ABC+HCR 7: Risk Table Bridging; 28: ABC+HCR 8: Adjust effective spawning biomass; 29: ABC+HCR 9: Forecast informed HCR 5; 210: ABC+HCR 10: Maximize productivity)",RunFolder3,append=T)     # here for testing
    write("##Ass_Basic:Type Tier_6a",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_7",RunFolder3,append=T)
  }
  if (CaseAll==1)
   {
    write("##Ass_Basic:Type Tier_0",RunFolder3,append=T)     # here for testing
    if (Byproduct.species== "No") write("#Ass_Basic:Type Tier_1a",RunFolder3,append=T)
    if (Byproduct.species=="Yes") write("##Ass_Basic:Type Tier_1a",RunFolder3,append=T)
    if (Byproduct.species== "No") write("##Ass_Basic:Type Tier_4a (DT4)",RunFolder3,append=T)
    if (Byproduct.species=="Yes") write("#Ass_Basic:Type Tier_4a (DT4)",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_4b (Tier 4)",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_5 (0: Zero catch; 1: Average catch; 21: ABC+HCR 1: Tier 3 HCR; 22: ABC+HCR 2: Lagged recovery; 23: ABC+HCR 3: Long-term resilience; 25: ABC+HCR 5: Maximize productivity; 27: ABC+HCR 7: Risk Table Bridging; 28: ABC+HCR 8: Adjust effective spawning biomass; 29: ABC+HCR 9: Forecast informed HCR 5; 210: ABC+HCR 10: Maximize productivity)",RunFolder3,append=T)     # here for testing
    write("##Ass_Basic:Type Tier_6a",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_7",RunFolder3,append=T)
  }
  if (CaseAll==3)
   {
    write("##Ass_Basic:Type Tier_0",RunFolder3,append=T)     # here for testing
    write("##Ass_Basic:Type Tier_1a",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_4a (DT4)",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_4b (Tier 4)",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_5 (0: Zero catch; 1: Average catch; 21: ABC+HCR 1: Tier 3 HCR; 22: ABC+HCR 2: Lagged recovery; 23: ABC+HCR 3: Long-term resilience; 25: ABC+HCR 5: Maximize productivity; 27: ABC+HCR 7: Risk Table Bridging; 28: ABC+HCR 8: Adjust effective spawning biomass; 29: ABC+HCR 9: Forecast informed HCR 5; 210: ABC+HCR 10: Maximize productivity)",RunFolder3,append=T)     # here for testing
    write("#Ass_Basic:Type Tier_6a",RunFolder3,append=T)
    write("##Ass_Basic:Type Tier_7",RunFolder3,append=T)
  }
  
  write("",RunFolder3,append=T)     
  write("#Ass_Basic:Clean.up Default                  # None Default Full",RunFolder3,append=T)
  write("#Ass_Basic.Estimate Yes                      # Conduct an assessment: Yes / No",RunFolder3,append=T)
  write("#Ass_Basic.Use.SS3.par No                    # Started with the par file: Yes / No",RunFolder3,append=T)
  write("",RunFolder3,append=T) 
  
  write("#Ass_Basic.NassArea 1",RunFolder3,append=T) 
  write("#Ass_Basic.AssAreas",RunFolder3,append=T) 
  write(rep(1,OM.OBJ$nareas),RunFolder3,append=T) 
  write("",RunFolder3,append=T) 

  write("# Stock Synthesis stuff",RunFolder3,append=T)
  
  write(paste0("\n#SS:original.last.year ",OM.obj$endyr,"\n"),RunFolder3,append=T)
  
  write(paste0("#SS:Nblock_Patterns ",EM.obj$Nblocks),RunFolder3,append=T)
  cat("#SS:blocks_per_pattern ",EM.obj$nblocks.per.block,"\n",file=RunFolder3,append=T)
  for (Iblk in 1:EM.obj$Nblocks)
   write(EM.obj$block.years[Iblk,],ncol=length(EM.obj$block.years[Iblk,]),RunFolder3,append=T)
  
  write("",RunFolder3,append=T) 
  write(paste0("#SS:parameter_offset_approach ",EM.obj$param.offset),RunFolder3,append=T)
  write(paste0(c("#SS:parameter_M_phase ",paste(EM.OBJ$M.phase))),ncol=3,RunFolder3,append=T)
  write(paste0(c("#SS:parameter_Len1_phase ",paste(EM.obj$Lamin.phase))),ncol=3,RunFolder3,append=T)
  write(paste0(c("#SS:parameter_Len2_phase ",paste(EM.obj$Lamax.phase))),ncol=3,RunFolder3,append=T)
  write(paste0(c("#SS:parameter_Kappa_phase ",paste(EM.obj$Kappa.phase))),ncol=3,RunFolder3,append=T)
  if (OM.obj$Growth_model==2)
   write(paste0(c("#SS:parameter_Richards_phase ",paste(EM.obj$Richards.phase))),ncol=3,RunFolder3,append=T)
  write(paste0(c("#SS:parameter_CV1_phase ",paste(EM.obj$CV.young.phase))),ncol=3,RunFolder3,append=T)
  write(paste0(c("#SS:parameter_CV2_phase ",paste(EM.obj$CV.old.phase))),ncol=3,RunFolder3,append=T)
  write(paste0(c("#SS:parameter_first_mat_age ",paste(EM.obj$first_mat_age))),ncol=3,RunFolder3,append=T)
  write("#SS:MG_env.dev",RunFolder3,append=T) 
  write(EM.obj$MG_env.dev,ncol=100,RunFolder3,append=T)    
  write("#SS:MG_blocks",RunFolder3,append=T) 
  write(EM.obj$MG_blocks,ncol=100,RunFolder3,append=T)    
  write("#SS:MG_block_fns",RunFolder3,append=T) 
  write(EM.obj$MG_block_fn,ncol=100,RunFolder3,append=T)    
  write("#SS:MG_N_dev_vals",RunFolder3,append=T) 
  write(length(EM.obj$MG.dev.vals),ncol=100,RunFolder3,append=T)    
  write("#SS:MG_dev_vals",RunFolder3,append=T) 
  write(EM.obj$MG.dev.vals,ncol=100,RunFolder3,append=T)    
  write("#SS:MG_dev_phs",RunFolder3,append=T) 
  write(EM.obj$MG.dev.phs,ncol=100,RunFolder3,append=T)    
  
  # ----------------------------------------------------------------------------------------------------------
  
  write("",RunFolder3,append=T) 
  write("#SS:parameter_R0_mult 1",RunFolder3,append=T) 
  write(paste0("#SS:parameter_logR0_phase ",EM.obj$logR0.phase),RunFolder3,append=T)
  write(paste0("#SS:parameter_Steep_phase ",EM.obj$steep.phase),RunFolder3,append=T)
  write(paste0("#SS:parameter_SigmaR_phase ", EM.obj$sigmaR.phase),RunFolder3,append=T)
  write(paste0("#SS:parameter_regime_phase ", EM.obj$regime.phase),RunFolder3,append=T)
  write("#SS:SR_env.dev",RunFolder3,append=T) 
  write(EM.obj$SR_env.dev,ncol=15,RunFolder3,append=T)    
  write("#SS:SR_blocks",RunFolder3,append=T) 
  write(EM.obj$SR_blocks,ncol=15,RunFolder3,append=T)    
  write("#SS:SR_block_fns",RunFolder3,append=T) 
  write(EM.obj$SR_block_fn,ncol=15,RunFolder3,append=T)    
  write("#SS:SR_dev_vals",RunFolder3,append=T) 
  write(EM.obj$SR.dev.vals,ncol=15,RunFolder3,append=T)    
  write("#SS:SR_dev_phs",RunFolder3,append=T) 
  write(EM.obj$SR.dev.phs,ncol=15,RunFolder3,append=T)    
  
  write("",RunFolder3,append=T) 
  write(paste0("#SS:Rec_dev_main_start ",EM.obj$Rec_dev_main.start),RunFolder3,append=T)
  write(paste0("#SS:Rec_dev_end ",EM.obj$Rec_dev_end),RunFolder3,append=T)
  write(paste0("#SS:Rec_dev_early_start ",EM.obj$Rec_dev_early.start),RunFolder3,append=T)
  write(paste0("#SS_last_yr_nobias_adj ",EM.obj$ast_yr_nobias_adj),RunFolder3,append=T)
  write(paste0("#SS_first_yr_fullbias_adj ",EM.obj$first_yr_fullbias_adj),RunFolder3,append=T)
  write(paste0("#SS_last_yr_fullbias_adj ",EM.obj$last_yr_fullbias_adj),RunFolder3,append=T)
  write(paste0("#SS_end_yr_for_ramp ",EM.obj$end_yr_for_ramp),RunFolder3,append=T)
  
  write("",RunFolder3,append=T) 
  write(paste0("#Early_devs_est ",-EM.obj$Rec_dev_early),RunFolder3,append=T) 
  if (-EM.obj$Rec_dev_early>0)
    write(EM.obj$early.dev,ncol=length(EM.obj$early.dev),RunFolder3,append=T)
  
  write(paste("#Late_Devs_est",EM.obj$late.devs.est.y1,EM.obj$late.devs.est.y2),RunFolder3,append=T)
  write(EM.obj$late.devs,ncol=length(EM.obj$late.devs),RunFolder3,append=T)
  
  write("#SS:Use_Steepness_Equi",RunFolder3,append=T)
  write(OM.obj$Use_steep_init_equi,RunFolder3,append=T)
  
  write("",RunFolder3,append=T) 
  
  write("##SS_est_opt Full                             # EstOnly First.Full.Only Full",RunFolder3,append=T) 
  write("#SS_est_opt EstOnly                           # EstOnly First.Full.Only Full",RunFolder3,append=T) 
  write("#SS_pred_opt None                             # None, Opt_5 Opt_3 Env_predators",RunFolder3,append=T) 
 
  write("",RunFolder3,append=T) 
  write("#SS:Length_selex_pattern",RunFolder3,append=T) 
  write(EM.obj$size_selex_types.pattern,ncol=length(EM.obj$size_selex_types.pattern),RunFolder3,append=T)
  write("#SS:Length_selex_pattern_male",RunFolder3,append=T) 
  write(EM.obj$size_selex_types.male,ncol=length(EM.obj$size_selex_types.pattern),RunFolder3,append=T)
  write("#SS:Mirrored_size_selex",RunFolder3,append=T) 
  write(EM.obj$size_selex_types.special,ncol=length(EM.obj$size_selex_types.special),RunFolder3,append=T)
  write("#SS:N.estimated.selex.size.pars",RunFolder3,append=T) 
  write(EM.obj$size_nsel.pars,ncol=length(EM.obj$size_nsel.pars),RunFolder3,append=T)
  write("#SS:Selex_size_parameters",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets) write(EM.obj$size_selex_types.sel.pars[Ifleet,],ncol=15,RunFolder3,append=T)   
  write("#SS:Selex_size_par_phase",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size_selex_types.sel.phase[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_size_par_blocks",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size_selex_types.blocks.no[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_size_par_fns",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size_selex_types.blocks.fn[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_size_ann_devs_use",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size.sel.ann.devs.use[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_size_ann_devs_miny",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size.sel.ann.devs.miny[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_size_ann_devs_maxy",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size.sel.ann.devs.maxy[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_size_ann_devs_phs",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size.sel.ann.devs.phs [Ifleet,],ncol=15,RunFolder3,append=T)    
  
  write("#SS:Length_retain_pattern",RunFolder3,append=T) 
  write(EM.obj$size_retain_types.pattern,ncol=nfleets,RunFolder3,append=T)
  write("#SS:Retain_size_parameters",RunFolder3,append=T) 
  for (Ifleet in 1:nfleet.catch) write( EM.obj$size_retain_types.ret.pars[Ifleet,],ncol=15,RunFolder3,append=T)   
  write("#SS:Retain_size_par_phase",RunFolder3,append=T) 
  for (Ifleet in 1:nfleet.catch)  write(EM.obj$size_retain_types.ret.phase[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_size_par_blocks",RunFolder3,append=T) 
  for (Ifleet in 1:nfleet.catch)  write(EM.obj$size_retain_types.blocks.no[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_size_par_fns",RunFolder3,append=T) 
  for (Ifleet in 1:nfleet.catch)  write(EM.obj$size_retain_types.blocks.fn[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_size_ann_devs_use",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size.ret.ann.devs.use[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_size_ann_devs_miny",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size.ret.ann.devs.miny[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_size_ann_devs_maxy",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size.ret.ann.devs.maxy[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_size_ann_devs_phs",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size.ret.ann.devs.phs [Ifleet,],ncol=15,RunFolder3,append=T)    
  
  write("#SS:Mort_size_parameters",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets) write( EM.obj$size_retain_types.mort.pars[Ifleet,],ncol=15,RunFolder3,append=T)   
  write("#SS:Mort_size_par_phase",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$size_retain_types.mort.phase[Ifleet,],ncol=15,RunFolder3,append=T)    
  
  write("#SS:Time_varying_size_pars",RunFolder3,append=T) 
  cat("#SS:Number_of_non-dev_time_varying_pars_size",length(EM.obj$size.sel.dev.blocks.vals),"\n",file=RunFolder3,append=T) 
  cat("#SS:Number_of_time_varying_pars_size",length(EM.obj$size.sel.dev.blocks.vals)+length(EM.obj$size.sel.dev.vals)+length(EM.obj$size.ret.dev.vals),"\n",file=RunFolder3,append=T) 
  write("#SS:Time_varying_size_pars_vals",RunFolder3,append=T) 
  cat(EM.obj$size.sel.dev.blocks.vals,EM.obj$size.sel.dev.vals,EM.obj$size.ret.dev.vals,"\n",file=RunFolder3,append=T) 
  write("#SS:Time_varying_size_pars_phs",RunFolder3,append=T) 
  cat(EM.obj$size.sel.dev.blocks.phs,EM.obj$size.sel.dev.phs,EM.obj$size.ret.dev.phs,"\n",file=RunFolder3,append=T) 
  
  write("",RunFolder3,append=T) 
  write("#SS:Age_selex_pattern",RunFolder3,append=T) 
  write(EM.obj$age_selex_types.pattern,ncol=length(EM.obj$age_selex_types.pattern),RunFolder3,append=T)
  write("#SS:Age_selex_pattern_male",RunFolder3,append=T) 
  write(EM.obj$age_selex_types.male,ncol=length(EM.obj$size_selex_types.pattern),RunFolder3,append=T)
  write("#SS:Mirrored_age_selex",RunFolder3,append=T) 
  write(EM.obj$age_selex_types.special,ncol=length(EM.obj$age_selex_types.special),RunFolder3,append=T)
  write("#SS:N.estimated.selex.age.pars",RunFolder3,append=T) 
  write(EM.obj$age_nsel.pars,ncol=length(EM.obj$age_nsel.pars),RunFolder3,append=T)
  write("#SS:Selex_age_parameters",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets) write(EM.obj$age_selex_types.sel.pars[Ifleet,],ncol=15,RunFolder3,append=T)   
  write("#SS:Selex_age_par_phase",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age_selex_types.sel.phase[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_age_par_blocks",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age_selex_types.blocks.no[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_age_par_fns",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age_selex_types.blocks.fn[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_age_ann_devs_use",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age.sel.ann.devs.use[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_age_ann_devs_miny",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age.sel.ann.devs.miny[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_age_ann_devs_maxy",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age.sel.ann.devs.maxy[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Selex_age_ann_devs_phs",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age.sel.ann.devs.phs [Ifleet,],ncol=15,RunFolder3,append=T)    
  
  write("#SS:Age_retain_pattern",RunFolder3,append=T) 
  write(EM.obj$age_retain_types.pattern,ncol=nfleets,RunFolder3,append=T)
  write("#SS:Retain_age_parameters",RunFolder3,append=T) 
  for (Ifleet in 1:nfleet.catch) write( EM.obj$age_retain_types.ret.pars[Ifleet,],ncol=15,RunFolder3,append=T)   
  write("#SS:Retain_age_par_phase",RunFolder3,append=T) 
  for (Ifleet in 1:nfleet.catch)  write(EM.obj$age_retain_types.ret.phase[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_age_par_blocks",RunFolder3,append=T) 
  for (Ifleet in 1:nfleet.catch)  write(EM.obj$age_retain_types.blocks.no[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_age_par_fns",RunFolder3,append=T) 
  for (Ifleet in 1:nfleet.catch)  write(EM.obj$age_retain_types.blocks.fn[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_age_ann_devs_use",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age.ret.ann.devs.use[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_age_ann_devs_miny",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age.ret.ann.devs.miny[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_age_ann_devs_maxy",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age.ret.ann.devs.maxy[Ifleet,],ncol=15,RunFolder3,append=T)    
  write("#SS:Retain_age_ann_devs_phs",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age.ret.ann.devs.phs [Ifleet,],ncol=15,RunFolder3,append=T)    
 
  write("#SS:Mort_age_parameters",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets) write( EM.obj$age_retain_types.mort.pars[Ifleet,],ncol=15,RunFolder3,append=T)   
  write("#SS:Mort_age_par_phase",RunFolder3,append=T) 
  for (Ifleet in 1:nfleets)  write(EM.obj$age_retain_types.mort.phase[Ifleet,],ncol=15,RunFolder3,append=T)    

    write("#SS:Time_varying_age_pars",RunFolder3,append=T) 
  cat("#SS:Number_of_non-dev_time_varying_pars_age",length(EM.obj$age.sel.dev.blocks.vals),"\n",file=RunFolder3,append=T) 
  cat("#SS:Number_of_time_varying_pars_age",length(EM.obj$age.sel.dev.blocks.vals)+length(EM.obj$age.sel.dev.vals)+length(EM.obj$age.ret.dev.vals),"\n",file=RunFolder3,append=T) 
  write("#SS:Time_varying_age_pars_vals",RunFolder3,append=T) 
  cat(EM.obj$age.sel.dev.blocks.vals,EM.obj$age.sel.dev.vals,EM.obj$age.ret.dev.vals,"\n",file=RunFolder3,append=T) 
  write("#SS:Time_varying_age_pars_phs",RunFolder3,append=T) 
  cat(EM.obj$age.sel.dev.blocks.phs,EM.obj$age.sel.dev.phs,EM.obj$age.ret.dev.phs,"\n",file=RunFolder3,append=T) 
  
  
  write("\n#                EnV        Init Phase TV  Yr1   Yr2  Sigma",RunFolder3,append=T)
  write("#SS:TimeVarM   	   0            9    -6  0    0     0    0.5",RunFolder3,append=T)
  write("#SS:TimeVarLenA1    0            9    -6  0    0     0    0.5",RunFolder3,append=T)
  write("#SS:TimeVarLenA2    0            9    -6  0    0     0    0.5",RunFolder3,append=T)
  write("#SS:TimeVarKappa    0            9    -6  0    0     0    0.5",RunFolder3,append=T)
  write("#SS:TimeVarRecr     0            9    -6  0    0     0    0.5",RunFolder3,append=T)
  write("#SS:TimeVarR0Off    0            9    -6  0    0     0    0.5",RunFolder3,append=T)
  
  indices.with.use <- which(OM.obj$index.Use=="T") 
  Nindices <- length(indices.with.use)
  Nindices <- nfleets
  
  write("",RunFolder3,append=T)
  write(c("#SS:UseQinit",rep(1,Nindices)),ncol=Nindices+1,RunFolder3,append=T) 
  write(c("#SS:Qinit   ",rep(1,Nindices)),ncol=Nindices+1,RunFolder3,append=T) 
  write(c("#SS:Qphase  ",rep(1,Nindices)),ncol=Nindices+1,RunFolder3,append=T) 
   
  write("",RunFolder3,append=T)
  write(paste0("#SS:Early_dev_phase      ",EM.obj$recdev_early_phase),RunFolder3,append=T)
  write(paste0("#SS:Early_dev_first_year ",0),RunFolder3,append=T)
  write(paste0("#SS:Max_bias_adjust      ",EM.obj$max_bias_adj),RunFolder3,append=T)
  
  write("",RunFolder3,append=T)
  write("#Tier_4a:logr_init -1",RunFolder3,append=T)
  write("#Tier_4a:K_init_mult 1.2",RunFolder3,append=T)
  write("#Tier_4a:alpha 0.2",RunFolder3,append=T)
  write("#Tier_4a:beta 0.35",RunFolder3,append=T)
  write("#Tier_4a:DT4.target 0.48",RunFolder3,append=T)
  write("#Tier_4a:MSYPriorY1 1980",RunFolder3,append=T)
  write("#Tier_4a:MSYPriorY2 1990",RunFolder3,append=T)
  write("#Tier_4a:MSYRY1 1985",RunFolder3,append=T)
  write("#Tier_4a:MSYRY2 1995",RunFolder3,append=T)
  write("#Tier_4a:MSYL 0.4",RunFolder3,append=T)
  write("#Tier_4a:EstR Yes",RunFolder3,append=T)
  write("#Tier_4a:EstZ No",RunFolder3,append=T)
  write("#Tier_4a:ProcessError 1",RunFolder3,append=T)
  write("#Tier_4a:CVMSYL 0.002",RunFolder3,append=T)
  write("#Tier_4a:PriorMeanR 0.15",RunFolder3,append=T)
  write("#Tier_4a:PriorSDr 0.0001",RunFolder3,append=T)

  write("",RunFolder3,append=T)
  write("#Tier_4b:Cpue_Area 1",RunFolder3,append=T)
  write("#Tier_4b:Cpue_Index 1",RunFolder3,append=T)
  write("#Tier_4b:Cpue_avg_yr 5",RunFolder3,append=T)
  write("#Tier_4b:HCR_target 0.48",RunFolder3,append=T)
  write("#Tier_4b:Cpue_target_mult 1",RunFolder3,append=T)
  write("#Tier_4b:Cpue_limit_mult 0.5",RunFolder3,append=T)
  write("#Tier_4b:Max.catch 500",RunFolder3,append=T)
  write("#Tier_4b:Cpue_Yr1 1986",RunFolder3,append=T)
  write("#Tier_4b:Cpue_Yr2 1992",RunFolder3,append=T)
  
    
  write("",RunFolder3,append=T)
  write("#Tier_0:Fixed 0",RunFolder3,append=T)
  
  if (Control.Rule.Type=="CSIRO")
   {
    write("",RunFolder3,append=T)
    write("#Tier_1:MSY 3",RunFolder3,append=T)
    write("#Tier_1:SPR_target 0.48",RunFolder3,append=T)
    write("#Tier_1:Biomass_target 0.48",RunFolder3,append=T)
    write("#Tier_1:Forecast_type 3",RunFolder3,append=T)
    write("#Tier_1:Control_rule_inflection 0.35",RunFolder3,append=T)
    write("#Tier_1:Control_rule_Biomass_level_for_no_F 0.2",RunFolder3,append=T)
    write("#Tier_1:Control_rule_Protection_level_for_no_F 0",RunFolder3,append=T)
    write("#Tier_1:Buffer 1.0",RunFolder3,append=T)
    write("",RunFolder3,append=T)
   }
  if (Control.Rule.Type=="NPFMC")
   {
    write("",RunFolder3,append=T)
    write("#Tier_1:MSY 3",RunFolder3,append=T)
    write("#Tier_1:SPR_target 0.40",RunFolder3,append=T)
    write("#Tier_1:Biomass_target 0.40",RunFolder3,append=T)
    write("#Tier_1:Forecast_type 3",RunFolder3,append=T)
    write("#Tier_1:Control_rule_inflection 0.40",RunFolder3,append=T)
    write("#Tier_1:Control_rule_Biomass_level_for_no_F 0.02",RunFolder3,append=T)
    write("#Tier_1:Control_rule_Protection_level_for_no_F 0.2",RunFolder3,append=T)
    write("#Tier_1:Buffer 1.0",RunFolder3,append=T)
    write("",RunFolder3,append=T)
  }

  write("",RunFolder3,append=T)
  write("#Tier_5:Option 0",RunFolder3,append=T)
  write("#Tier_5:Years 1 2",RunFolder3,append=T)
  
  write("====================================================================================================================",RunFolder3,append=T)
  write(" Management specifications",RunFolder3,append=T)
  write("====================================================================================================================",RunFolder3,append=T)

  write("",RunFolder3,append=T)
  write("#Buffer_specifications:Default 1",RunFolder3,append=T)
  write("#Buffer_specifications:Minimum 1",RunFolder3,append=T)
  write("#Buffer_specifications:Stale_rate 0.05",RunFolder3,append=T)
  
  write("",RunFolder3,append=T)
  write("#Reallocation_specifications:Option 0  (0=None, 1=pre-specified, 2=close some fisheries)",RunFolder3,append=T)
  write("#Reallocation_specifications:Values",RunFolder3,append=T)
  write(rep(1, nfleet.catch),RunFolder3,append=T)

  write("",RunFolder3,append=T)
  write("#Constraints:Min_TAC 0",RunFolder3,append=T)
  write("#Constraints:Min_change -100",RunFolder3,append=T)
  write("#Constraints:Max_change -100",RunFolder3,append=T)
  write("#Constraints:Last_TAC -100",RunFolder3,append=T)
  write("",RunFolder3,append=T)
  
  #write("#Use.insurance",RunFolder3,append=T)
  #write("#Insurance_option 0",RunFolder3,append=T)
  #write("#Insurance_risk_ave 1",RunFolder3,append=T)
  #write("",RunFolder3,append=T)
  
  write("#Close.if.MHW No",RunFolder3,append=T)
  
}

##================================================================================================================================
##================================================================================================================================
