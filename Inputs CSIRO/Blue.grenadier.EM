# Generated EM file
##Ass_Basic:Type Tier_0
#Ass_Basic:Type Tier_1a
##Ass_Basic:Type Tier_4a (DT4)
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

#SS:original.last.year 2021

#SS:Nblock_Patterns 2
#SS:blocks_per_pattern  1 1 
1960 2021 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
1959 1959 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

#SS:parameter_offset_approach 3
#SS:parameter_M_phase  4 5
#SS:parameter_Len1_phase  2 -2
#SS:parameter_Len2_phase  2 2
#SS:parameter_Kappa_phase  2 2
#SS:parameter_CV1_phase  3 3
#SS:parameter_CV2_phase  3 3
#SS:parameter_first_mat_age  1
#SS:MG_env.dev
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:MG_blocks
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:MG_block_fns
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
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


#SS:Rec_dev_main_start 14
#SS:Rec_dev_end 3
#SS:Rec_dev_early_start 0
#SS_last_yr_nobias_adj 10.3800000000001
#SS_first_yr_fullbias_adj 19.3648000000001
#SS_last_yr_fullbias_adj -4.64619999999991
#SS_end_yr_for_ramp 6.72409999999991

#Early_devs_est 0
#Late_Devs_est 15 59
-0.41315 -0.405826 -0.125965 0.329238 1.17648 1.29191 0.00608418 0.208267 0.468524 0.222162 1.07228 1.16936 1.18061 1.49615 -0.742223 -1.02271 -0.621572 -0.47965 -0.92121 -0.913647 2.89622 1.94374 -0.0213839 -1.01769 -1.98176 -2.53424 -2.62589 -2.06559 -1.59727 0.958986 -0.964236 -2.27346 -0.431591 -1.14971 -1.00924 -0.223374 1.46885 1.23877 0.824982 0.491109 0.83475 0.995338 1.19524 1.16587 0.90645
#SS:Use_Steepness_Equi
0

##SS_est_opt Full                             # EstOnly First.Full.Only Full
#SS_est_opt EstOnly                           # EstOnly First.Full.Only Full
#SS_pred_opt None                             # None, Opt_5 Opt_3 Env_predators

#SS:Length_selex_pattern
1 24 15 15 0 1
#SS:Length_selex_pattern_male
3 0 0 0 0 3
#SS:Mirrored_size_selex
0 0 1 2 0 0
#SS:N.estimated.selex.size.pars
5 6 0 0 0 5
#SS:Selex_size_parameters
93.3969 23.5822 -20.9483 -10.8109 1 0 0 0 0 0 0 0 0 0 0
50.2596 -0.562967 4.8732 3.40124 -3.75157 0.151687 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
63.7 11.3 -6.9 0 1 0 0 0 0 0 0 0 0 0 0
#SS:Selex_size_par_phase
3 3 3 3 -5 0 0 0 0 0 0 0 0 0 0
4 4 4 3 3 4 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
-5 -5 -5 -5 -5 0 0 0 0 0 0 0 0 0 0
#SS:Selex_size_par_blocks
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
#SS:Selex_size_ann_devs_use
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
#SS:Selex_size_ann_devs_maxy
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
#SS:Length_retain_pattern
0 1 0 -2
#SS:Retain_size_parameters
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
55.4481 3.11784 9 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_par_phase
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
5 5 -5 -5 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_par_blocks
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_size_par_fns
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
#SS:Retain_size_ann_devs_miny
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
#SS:Retain_size_ann_devs_phs
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
#SS:Mort_size_par_phase
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
10 10 10 10 10 10
#SS:Age_selex_pattern_male
0 0 0 0 0 0
#SS:Mirrored_age_selex
0 0 0 0 0 0
#SS:N.estimated.selex.age.pars
0 0 0 0 0 0
#SS:Selex_age_parameters
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
#SS:Selex_age_par_blocks
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
#SS:Selex_age_ann_devs_use
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
#SS:Selex_age_ann_devs_maxy
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
#SS:Age_retain_pattern
0 0 0 0
#SS:Retain_age_parameters
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_age_par_phase
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_age_par_blocks
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#SS:Retain_age_par_fns
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
#SS:Retain_age_ann_devs_miny
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
#SS:Retain_age_ann_devs_phs
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
#SS:Mort_age_par_phase
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

#SS:UseQinit 1 1 1 1 1 1
#SS:Qinit    1 1 1 1 1 1
#SS:Qphase   1 1 1 1 1 1

#SS:Early_dev_phase      -3
#SS:Early_dev_first_year 0
#SS:Max_bias_adjust      0.88

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
1 1 1 1

#Constraints:Min_TAC 0
#Constraints:Min_change -100
#Constraints:Max_change -100
#Constraints:Last_TAC -100

#Close.if.MHW No
