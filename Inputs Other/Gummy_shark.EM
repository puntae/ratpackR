# Generated EM file
##Ass_Basic:Type Tier_0
#Ass_Basic:Type Tier_1a
##Ass_Basic:Type Tier_4a
##Ass_Basic:Type Tier_4b (Tier 4)
##Ass_Basic:Type Tier_5 (0: Zero catch; 1: Average catch; 21: ABC+HCR 1: Tier 3 HCR; 22: ABC+HCR 2: Lagged recovery; 23: ABC+HCR 3: Long-term resilience; 25: ABC+HCR 5: Maximize productivity; 27: ABC+HCR 7: Risk Table Bridging; 28: ABC+HCR 8: Adjust effective spawning biomass; 29: ABC+HCR 9: Forecast informed HCR 5; 210: ABC+HCR 10: Maximize productivity)
##Ass_Basic:Type Tier_6a
##Ass_Basic:Type Tier_7

#Ass_Basic:Clean.up Default                  # None Default Full
#Ass_Basic.Estimate Yes                      # Conduct an assessment: Yes / No
#Ass_Basic.Use.SS3.par No                    # Started with the par file: Yes / No

#Ass_Basic.NassArea 1
#Ass_Basic.AssAreas
1

# Stock Synthesis stuff

#SS:original.last.year 2023

#SS:Nblock_Patterns 2
#SS:blocks_per_pattern  1 1 
1941 2023 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
1940 1940 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

#SS:parameter_offset_approach 1
#SS:parameter_M_phase  NA NA
#SS:parameter_Len1_phase  -3 -3
#SS:parameter_Len2_phase  -2 -2
#SS:parameter_Kappa_phase  -3 -3
#SS:parameter_CV1_phase  -4 -4
#SS:parameter_CV2_phase  -4 -4
#SS:parameter_first_mat_age  3
#SS:MG_env.dev
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:MG_blocks
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:MG_block_fns
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:MG_N_dev_vals
0
#SS:MG_dev_vals

#SS:MG_dev_phs


#SS:parameter_R0_mult 1
#SS:parameter_logR0_phase 1
#SS:parameter_Steep_phase -4
#SS:parameter_SigmaR_phase -6
#SS:parameter_regime_phase -99
#SS:SR_env.dev
0 0 0 0 0
#SS:SR_blocks
0 0 0 0 0
#SS:SR_block_fns
0 0 0 0 0
#SS:SR_dev_vals

#SS:SR_dev_phs


#SS:Rec_dev_main_start 45
#SS:Rec_dev_end 0
#SS:Rec_dev_early_start -20
#SS_last_yr_nobias_adj 37.5637999999999
#SS_first_yr_fullbias_adj 42.1793
#SS_last_yr_fullbias_adj -4.19669999999996
#SS_end_yr_for_ramp -3.65259999999989

#Early_devs_est 20
-0.00711252 -0.0079057 -0.00884271 -0.00980368 -0.010919 -0.0120988 -0.0133404 -0.0146415 -0.0161699 -0.0173481 -0.0187991 -0.0203721 -0.0219916 -0.024694 -0.0301404 -0.0416131 -0.0624838 -0.0875306 -0.100972 -0.104947
#Late_Devs_est 46 83
-0.097114 -0.100285 -0.150538 -0.219131 -0.247868 -0.185317 -0.0831521 -0.18041 -0.136492 0.237644 0.352732 0.414859 0.299719 0.135321 0.0689329 0.139954 0.31153 0.324413 0.0553135 -0.0366396 -0.0735898 0.0287965 0.16864 -0.0726853 -0.113581 -0.0397188 0.0524404 0.0683939 -0.0361277 -0.110301 -0.100309 -0.0237946 0.0560517 0.0520477 0.00840783 -0.0025721 -2.12978e-05 4.12834e-09
#SS:Use_Steepness_Equi
0

##SS_est_opt Full                             # EstOnly First.Full.Only Full
#SS_est_opt EstOnly                           # EstOnly First.Full.Only Full
#SS_pred_opt None                             # None, Opt_5 Opt_3 Env_predators

#SS:Length_selex_pattern
1 15 24 24 24 15 24 24
#SS:Length_selex_pattern_male
0 0 0 0 0 0 0 0
#SS:Mirrored_size_selex
0 1 0 0 0 3 0 0
#SS:N.estimated.selex.size.pars
2 0 6 6 6 0 6 6
#SS:Selex_size_parameters
112.999 25.9813 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
110.14 -20 4.87 6.57 -999 -999 0 0 0 0 0 0 0 0 0
-8.5 -0.88 -1.95 -1.5 0.06 110.242 0 0 0 0 0 0 0 0 0
-20 4.87862 6.57872 -999 -999 -8.5 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
-0.88 -1.95 -1.5 0.06 117.357 -20 0 0 0 0 0 0 0 0 0
4.8821 5.94822 -999 -999 -9.39058 -0.88 0 0 0 0 0 0 0 0 0
#SS:Selex_size_par_phase
-2 -2 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
-2 -2 -2 -2 -2 -2 0 0 0 0 0 0 0 0 0
2 4 3 4 -2 -2 0 0 0 0 0 0 0 0 0
2 4 3 4 -2 -2 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
3 -4 3 -4 -2 -2 0 0 0 0 0 0 0 0 0
3 -4 3 -4 -2 -2 0 0 0 0 0 0 0 0 0
#SS:Selex_size_par_blocks
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_size_par_fns
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_size_ann_devs_use
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_size_ann_devs_miny
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_size_ann_devs_maxy
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_size_ann_devs_phs
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Length_retain_pattern
0 0 0 0 0 0 0 0
#SS:Retain_size_parameters
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_par_phase
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_par_blocks
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_par_fns
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_ann_devs_use
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_ann_devs_miny
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_ann_devs_maxy
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_ann_devs_phs
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Mort_size_parameters
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Mort_size_par_phase
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Time_varying_size_pars
#SS:Number_of_non-dev_time_varying_pars_size 0 
#SS:Number_of_time_varying_pars_size 0 
#SS:Time_varying_size_pars_vals
 
#SS:Time_varying_size_pars_phs
 

#SS:Age_selex_pattern
0 0 0 0 0 0 0 0
#SS:Age_selex_pattern_male
0 0 0 0 0 0 0 0
#SS:Mirrored_age_selex
0 0 0 0 0 0 0 0
#SS:N.estimated.selex.age.pars
0 0 0 0 0 0 0 0
#SS:Selex_age_parameters
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_age_par_phase
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_age_par_blocks
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_age_par_fns
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_age_ann_devs_use
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_age_ann_devs_miny
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_age_ann_devs_maxy
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_age_ann_devs_phs
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Age_retain_pattern
0 0 0 0 0 0 0 0
#SS:Retain_age_parameters
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_age_par_phase
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_age_par_blocks
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_age_par_fns
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_age_ann_devs_use
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_age_ann_devs_miny
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_age_ann_devs_maxy
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_age_ann_devs_phs
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Mort_age_parameters
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Mort_age_par_phase
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Time_varying_age_pars
#SS:Number_of_non-dev_time_varying_pars_age 0 
#SS:Number_of_time_varying_pars_age 0 
#SS:Time_varying_age_pars_vals
 
#SS:Time_varying_age_pars_phs
 

#                EnV        Init Phase TV  Yr1   Yr2  Sigma
#SS:TimeVarM   	   0            9    -6  0    0     0    0.5
#SS:TimeVarLenA1    0            9    -6  0    0     0    0.5
#SS:TimeVarLenA2    0            9    -6  0    0     0    0.5
#SS:TimeVarKappa    0            9    -6  0    0     0    0.5
#SS:TimeVarRecr     0            9    -6  0    0     0    0.5
#SS:TimeVarR0Off    0            9    -6  0    0     0    0.5

#SS:UseQinit 1 1 1 1 1 1 1 1
#SS:Qinit    1 1 1 1 1 1 1 1
#SS:Qphase   1 1 1 1 1 1 1 1

#SS:Early_dev_phase      3
#SS:Early_dev_first_year 0
#SS:Max_bias_adjust      0.1265

#Tier_4a:logr_init -1
#Tier_4a:K_init_mult 1.2
#Tier_4a:alpha 0.2
#Tier_4a:beta 0.35
#Tier_4a:DT4.target 0.48
#Tier_4a:MSYPriorY1 1980
#Tier_4a:MSYPriorY2 1990
#Tier_4a:MSYRY1 1985
#Tier_4a:MSYRY2 1995
#Tier_4a:MSYL 0.4
#Tier_4a:EstR Yes
#Tier_4a:EstZ No
#Tier_4a:ProcessError 1
#Tier_4a:CVMSYL 0.002
#Tier_4a:PriorMeanR 0.15
#Tier_4a:PriorSDr 0.0001

#Tier_4b:Cpue_Area 1
#Tier_4b:Cpue_Index 1
#Tier_4b:Cpue_avg_yr 5
#Tier_4b:HCR_target 0.48
#Tier_4b:Cpue_target_mult 1
#Tier_4b:Cpue_limit_mult 0.5
#Tier_4b:Max.catch 500
#Tier_4b:Cpue_Yr1 1986
#Tier_4b:Cpue_Yr2 1992

#Tier_0:Fixed 0

#Tier_1:MSY 3
#Tier_1:SPR_target 0.48
#Tier_1:Biomass_target 0.456
#Tier_1:Forecast_type 3
#Tier_1:Control_rule_inflection 0.342
#Tier_1:Control_rule_Biomass_level_for_no_F 0.256
#Tier_1:Control_rule_Protection_level_for_no_F 0
#Tier_1:Buffer 1.0


#Tier_5:Option 0
#Tier_5:Years 1 2
====================================================================================================================
 Management specifications
====================================================================================================================

#Buffer_specifications:Default 1
#Buffer_specifications:Minimum 1
#Buffer_specifications:Stale_rate 0.05

#Reallocation_specifications:Option 2  (0=None, 1=pre-specified, 2=close some fisheries)
#Reallocation_specifications:Values
0 0.06 0 0 0 0 0 0.94

#Constraints:Min_TAC 0
#Constraints:Min_change -100
#Constraints:Max_change -100
#Constraints:Last_TAC -100

#Close.if.MHW No
