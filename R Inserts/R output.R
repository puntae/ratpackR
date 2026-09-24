library(r4ss)
mydirectory <- "C:/Research/NewRat/Newrat_tst/Temp/Assess_temp_Bight.redfish/"
mymodelrun <- "SS_Sim_1_Bight.redfish_62"

mydirectory <- "C:/Research/NewRat/Newrat_tst/Temp/Assess_temp_P.cod/"
mymodelrun <- "SS_Sim_1_p.cod_48"
#mydirectory <- "C:/Research/NewRat/Newrat_tst/Temp/Assess_temp_Pink.ling/"
#mymodelrun <- "SS_Sim_1_Pink.ling_54"
#mydirectory <- "C:/Research/NewRat/Newrat_tst/Temp/Assess_temp_Tiger.flathead/"
#mymodelrun <- "SS_Sim_1_Tiger.flathead_107"
#mydirectory <- "C:/Research/NewRat/Newrat_tst/Temp/Assess_temp_SSageselemod2/"
#mymodelrun <- "SS_Sim_1_SSageselemod2_48"
#mydirectory <- "C:/Research/NewRat/Newrat_tst/Temp/Assess_temp_Orange.roughy.east/"
#mymodelrun <- "SS_Sim_1_Orange.roughy.east_41"
#mydirectory <- "C:/Research/NewRat/Newrat_tst/Temp/Assess_temp_P.cod.OMF/"
#mymodelrun <- "SS_Sim_1_p.cod.OMF_48"

mydirectory <- "C:/Research/NewRat/"
mymodelrun <- "P cod"
model0 <- SS_output(dir=paste0(mydirectory, mymodelrun),covar=T)
SS_plots(replist=model0, png=T)
