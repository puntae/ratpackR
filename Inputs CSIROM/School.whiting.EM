# Generated EM file
##Ass_Basic:Type Tier_0
##Ass_Basic:Type Tier_1a
#Ass_Basic:Type Tier_4a (DT4)
##Ass_Basic:Type Tier_4b (Tier 4)
##Ass_Basic:Type Tier_5 (0: Zero catch; 1: Average catch; 21: ABC+HCR 1: Tier 3 HCR; 22: ABC+HCR 2: Lagged recovery; 23: ABC+HCR 3: Long-term resilience; 25: ABC+HCR 5: Maximize productivity; 27: ABC+HCR 7: Risk Table Bridging; 28: ABC+HCR 8: Adjust effective spawning biomass; 29: ABC+HCR 9: Forecast informed HCR 5; 210: ABC+HCR 10: Maximize productivity)
##Ass_Basic:Type Tier_6a
##Ass_Basic:Type Tier_7

#Ass_Basic:Clean.up Default                  # None Default Full
#Ass_Basic.Estimate Yes                      # Conduct an assessment: Yes / No
#Ass_Basic.Use.SS3.par No                    # Started with the par file: Yes / No

#Ass_Basic.NassArea 1
#Ass_Basic.AssAreas
1 1 1 1

# Stock Synthesis stuff

#SS:original.last.year 2025

#SS:Nblock_Patterns 6
#SS:blocks_per_pattern  1 1 1 1 1 1 
1942 2025 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
1941 1941 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
2019 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
2019 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
2017 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
2017 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

#SS:parameter_offset_approach 3
#SS:parameter_M_phase  -3
#SS:parameter_Len1_phase  2
#SS:parameter_Len2_phase  2
#SS:parameter_Kappa_phase  3
#SS:parameter_CV1_phase  3
#SS:parameter_CV2_phase  -3
#SS:parameter_first_mat_age  1
#SS:MG_env.dev
0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:MG_blocks
0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:MG_block_fns
0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:MG_N_dev_vals
0
#SS:MG_dev_vals

#SS:MG_dev_phs


#SS:parameter_R0_mult 1
#SS:parameter_logR0_phase 1
#SS:parameter_Steep_phase -4
#SS:parameter_SigmaR_phase -3
#SS:parameter_regime_phase -4
#SS:SR_env.dev
0 0 0 0 0
#SS:SR_blocks
0 0 0 0 0
#SS:SR_block_fns
0 0 0 0 0
#SS:SR_dev_vals

#SS:SR_dev_phs


#SS:Rec_dev_main_start 39
#SS:Rec_dev_end 5
#SS:Rec_dev_early_start 0
#SS_last_yr_nobias_adj 27.8199999999999
#SS_first_yr_fullbias_adj 51.3800000000001
#SS_last_yr_fullbias_adj -9.31999999999994
#SS_end_yr_for_ramp 14.26

#Early_devs_est 0
#Late_Devs_est 40 79
-0.364976 0.354834 -0.483471 -0.1555 0.884276 -0.0493213 0.0869004 0.200348 -0.117158 0.0912693 0.184474 -0.0706551 -0.0200554 -0.557513 0.0167207 0.029474 0.188692 0.277502 0.0677277 -0.260929 0.136417 -0.0850863 0.495748 0.274441 0.287476 -0.3072 -0.110203 0.136088 -0.361713 0.0331091 -0.256709 -0.00531711 -0.167148 0.296344 0.16481 -0.365053 -0.0067571 0.128945 -0.182214 -0.408618
#SS:Use_Steepness_Equi
0

##SS_est_opt Full                             # EstOnly First.Full.Only Full
#SS_est_opt EstOnly                           # EstOnly First.Full.Only Full
#SS_pred_opt None                             # None, Opt_5 Opt_3 Env_predators

#SS:Length_selex_pattern
1 15 1 15 15
#SS:Length_selex_pattern_male
0 0 0 0 0
#SS:Mirrored_size_selex
0 1 0 1 3
#SS:N.estimated.selex.size.pars
2 0 2 0 2 0 2 0 2 0 2 0 0 0 0 0 0 0 0
#SS:Selex_size_parameters
17.1734 4.2845 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
16.9094 2.74232 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_size_par_phase
3 3 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
3 3 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_size_par_blocks
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
#SS:Selex_size_ann_devs_use
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
#SS:Selex_size_ann_devs_maxy
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
#SS:Length_retain_pattern
1 -1 1 -1 -3
#SS:Retain_size_parameters
14.9529 0.923623 10 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
9.84799 2.56035 10 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_par_phase
3 3 -3 -3 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
3 3 -3 -3 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_par_blocks
5 6 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
3 4 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_par_fns
2 2 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
2 2 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_ann_devs_use
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
#SS:Retain_size_ann_devs_maxy
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
#SS:Mort_size_parameters
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
#SS:Time_varying_size_pars
#SS:Number_of_non-dev_time_varying_pars_size 4 
#SS:Number_of_time_varying_pars_size 4 
#SS:Time_varying_size_pars_vals
16.0707 2.07298 14.0799 3.56854 
#SS:Time_varying_size_pars_phs
3 3 3 3 

#SS:Age_selex_pattern
0 15 0 15 15
#SS:Age_selex_pattern_male
0 0 0 0 0
#SS:Mirrored_age_selex
0 1 0 1 3
#SS:N.estimated.selex.age.pars
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Selex_age_parameters
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
#SS:Selex_age_par_blocks
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
#SS:Selex_age_ann_devs_use
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
#SS:Selex_age_ann_devs_maxy
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
#SS:Age_retain_pattern
0 -1 0 -1 -3
#SS:Retain_age_parameters
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
#SS:Retain_age_par_blocks
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
#SS:Retain_age_ann_devs_use
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
#SS:Retain_age_ann_devs_maxy
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
#SS:Mort_age_parameters
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

#SS:UseQinit 1 1 1 1 1
#SS:Qinit    1 1 1 1 1
#SS:Qphase   1 1 1 1 1

#SS:Early_dev_phase      -4
#SS:Early_dev_first_year 0
#SS:Max_bias_adjust      0.9203

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
#Tier_1:Biomass_target 0.48
#Tier_1:Forecast_type 3
#Tier_1:Control_rule_inflection 0.35
#Tier_1:Control_rule_Biomass_level_for_no_F 0.2
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

#Reallocation_specifications:Option 0  (0=None, 1=pre-specified, 2=close some fisheries)
#Reallocation_specifications:Values
1 1 1 1 1

#Constraints:Min_TAC 0
#Constraints:Min_change -100
#Constraints:Max_change -100
#Constraints:Last_TAC -100

#Close.if.MHW No
