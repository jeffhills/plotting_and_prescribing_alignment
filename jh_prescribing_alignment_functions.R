
# predict_postop_pt_function <- function(preop_pt = 15, rad_pre_c2_tilt = 1.5430008, rad_1y_c2pa_change = -6.3748676) {
#   pt_change <- 0.45374535+0.47046549*rad_pre_c2_tilt+0.78984904*rad_1y_c2pa_change 
#   
#   preop_pt + pt_change
# }

predict_postop_pt_function <- function(postop_c2pa = 19.231308,
         preop_pt = 25.203136,
         preop_c2_tilt = 1.5912083){
  round(-0.018071724+0.83058112*postop_c2pa+0.17554658*preop_pt-0.26959853*preop_c2_tilt, 2)
}

estimate_l1s1_from_pi_l1pa_function <- function(pelvic_incidence = 51.66,
                                                l1_pelvic_angle = 3.86) {
  round(-7.7906507+1.4044415*pelvic_incidence-1.6407995*l1_pelvic_angle, 2) 
  }

estimate_t4pa_from_l1s1_pi_l1pa_function <- function(l1_s1 = 58.895,
                                                     pelvic_incidence = 51.66,
                                                     l1_pelvic_angle = 3.86) {
  round(-1.6252735-0.34568389*l1_s1+0.45785914*pelvic_incidence+0.36773259*l1_pelvic_angle, 2) 
  }


############# PREDICT PJK RISK ############
pjk_risk_function <- function(age = 66.1,
                              sex = "Female",
                              uiv_region = "Lower Thoracic",
                              preop_c2_t9pa_mismatch = 12.170434,
                              pelvic_incidence = 54.630633) {
  
  prediction <- -3.5046884+0.022398521*age-0.47900877*(sex=="Male")-0.75690545*(uiv_region=="Upper Thoracic")+0.19040964*preop_c2_t9pa_mismatch-0.0097770523*pelvic_incidence-0.099232384*(uiv_region=="Upper Thoracic")*preop_c2_t9pa_mismatch 
  
  round(plogis(prediction), 2)
  }

############### COMPUTE TARGET PELVIC ANGLES #####################
target_l4pa_function <- function(pelvic_incidence = 51.75934) {-10.140792+0.36078574*pelvic_incidence }

target_l1pa_function <- function(pelvic_incidence = 51.813768) {-19 + 0.5*pelvic_incidence}

target_t9pa_function <- function(pelvic_incidence = 51.75934) {-21.161451+0.42939559*pelvic_incidence }

# Consider computing target angles based on proximal segment angles:

target_l1pa_by_all_proximal_sa_function <- function(pelvic_incidence = 51.75934,c2_c3 = 7.2418723,c3_c4 = 2.3525818,c4_c5 = -3.5602989,c5_c6 = -1.0274844,c6_c7 = 5.0640539,c7_t1 = 7.6006935,t1_t2 = 0.91990553,t2_t3 = -2.4063826,t3_t4 = -4.9977997,t4_t5 = -5.2867851,t5_t6 = -5.6352776,t6_t7 = -5.5675075,t7_t8 = -5.0805516,t8_t9 = -3.5695003,t9_t10 = -2.5956493,t10_t11 = -3.2007225,t11_t12 = -2.3724442,t12_l1 = -0.4781281) {-14.02456+0.44383426*pelvic_incidence+0.073890582*c2_c3+0.096636404*c3_c4-0.007548419*c4_c5+0.096200108*c5_c6+0.15981649*c6_c7+0.096102752*c7_t1+0.022867653*t1_t2+0.14987884*t2_t3+0.16926871*t3_t4+0.13426867*t4_t5+0.10401362*t5_t6+0.073644138*t6_t7+0.16707351*t7_t8+0.22912857*t8_t9+0.229962*t9_t10+0.22680411*t10_t11+0.36013135*t11_t12+0.4652786*t12_l1 }

target_l1pa_by_proximal_sa_to_t9_function <- function(pelvic_incidence = 51.75934,t9_t10 = -2.5956493,t10_t11 = -3.2007225,t11_t12 = -2.3724442,t12_l1 = -0.4781281) {-16.169583+0.43757943*pelvic_incidence+0.22606995*t9_t10+0.23857642*t10_t11+0.30851034*t11_t12+0.36924227*t12_l1 }

target_l4pa_by_all_proximal_sa_function <- function(pelvic_incidence = 51.75934,c2_c3 = 7.2418723,c3_c4 = 2.3525818,c4_c5 = -3.5602989,c5_c6 = -1.0274844,c6_c7 = 5.0640539,c7_t1 = 7.6006935,t1_t2 = 0.91990553,t2_t3 = -2.4063826,t3_t4 = -4.9977997,t4_t5 = -5.2867851,t5_t6 = -5.6352776,t6_t7 = -5.5675075,t7_t8 = -5.0805516,t8_t9 = -3.5695003,t9_t10 = -2.5956493,t10_t11 = -3.2007225,t11_t12 = -2.3724442,t12_l1 = -0.4781281,l1_l2 = 9.4329344,l2_l3 = 9.4329344,l3_l4 = 10.829323) {-8.4104096+0.29880999*pelvic_incidence+0.043624079*c2_c3+0.069467112*c3_c4-0.021559367*c4_c5+0.050808206*c5_c6+0.077471645*c6_c7+0.040147095*c7_t1+0.040270531*t1_t2+0.068083498*t2_t3+0.087144042*t3_t4+0.033860915*t4_t5+0.024978178*t5_t6+0.018483101*t6_t7+0.090298058*t7_t8+0.10786551*t8_t9+0.1271408*t9_t10+0.096577939*t10_t11+0.15785489*t11_t12+0.20951609*t12_l1+0.17239328*l1_l2 + 0*l2_l3+0.14749418*l3_l4 }#############

jh_pso_degree_calculator <- function(pso_level, current_lpa, desired_lpa){
  pso_level <- str_to_lower(pso_level)
  
  weight <- case_when(
    pso_level == "l5" ~ -0.56,
    pso_level == "l4" ~ -0.49,
    pso_level == "l3" ~ -0.32,
    pso_level == "l2" ~ -0.2,
    pso_level == "l1" ~ -0.1,
  )
  
  lpa_change_needed <- current_lpa - desired_lpa 
  
  pso_degree_needed <- lpa_change_needed/weight*-1
  
  pso_degree_needed
}

jh_pso_degree_calculator_for_l4pa_target <- function(pso_level, current_l4pa, desired_l4pa){
  pso_level <- str_to_lower(pso_level)
  
  weight <- case_when(
    pso_level == "l5" ~ -0.2789,
    pso_level == "l4" ~ -0.1268
  )
  
  l4pa_change_needed <- current_l4pa - desired_l4pa 
  
  pso_degree_needed <- l4pa_change_needed/weight*-1
  
  pso_degree_needed
}

l5_s1_for_desired_l1pa_function <- function(l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l1_l2 = 2.765,l2_l3 = 6.565,l3_l4 = 11.44,l4_l5 = 12.57) {-3.2776435-1.6177716*l1_pelvic_angle+1.0156583*pelvic_incidence-0.11287819*l1_l2-0.30829527*l2_l3-0.4548459*l3_l4-0.81832782*l4_l5 }

l4_l5_for_desired_l1pa_function <- function(l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l1_l2 = 2.765,l2_l3 = 6.565,l3_l4 = 11.44,l5_s1 = 24.885) {-3.9414208-1.6435821*l1_pelvic_angle+1.04675*pelvic_incidence-0.12765624*l1_l2-0.30268006*l2_l3-0.41039784*l3_l4-0.96102997*l5_s1 }

l3_l4_for_desired_l1pa_function <- function(l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l1_l2 = 2.765,l2_l3 = 6.565,l4_l5 = 12.57,l5_s1 = 24.885) {-2.1294261-1.6675848*l1_pelvic_angle+1.0786729*pelvic_incidence-0.12306038*l1_l2-0.14035848*l2_l3-0.76182565*l4_l5-0.99157239*l5_s1 }

l2_l3_for_desired_l1pa_function <- function(l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l1_l2 = 2.765,l3_l4 = 11.44,l4_l5 = 12.57,l5_s1 = 24.885) {-3.9236937-1.380391*l1_pelvic_angle+0.89381487*pelvic_incidence+0.14941534*l1_l2-0.16876433*l3_l4-0.67557929*l4_l5-0.80810723*l5_s1 }

l1_l2_for_desired_l1pa_function <- function(l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l2_l3 = 6.565,l3_l4 = 11.44,l4_l5 = 12.57,l5_s1 = 24.885) {-4.128642-0.83494688*l1_pelvic_angle+0.68381549*pelvic_incidence+0.32214436*l2_l3-0.31901829*l3_l4-0.61431329*l4_l5-0.63792192*l5_s1 }



# compute_lpa_from_segment_levels_function <- function(pelvic_incidence = 51.813768,l1_l2 = 1.6817735,l2_l3 = 9.4508485,l3_l4 = 10.772589,l4_l5 = 16.66046,l5_s1 = 23.947641) {-0.8553283+0.60405938*pelvic_incidence-0.048137209*l1_l2-0.17727093*l2_l3-0.32237285*l3_l4-0.48541991*l4_l5-0.56294456*l5_s1 }

pt_function <- function(tpa = 6.3128833,t1_l1 = -40.878235,l1_s1 = 61.507721,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99) {1.73748+0.71970033*tpa-0.081113318*t1_l1-0.14596707*l1_s1+0.21338716*pelvic_incidence-0.03022352*l1_pelvic_angle }


recompute_tpa_estimation_function <- function(pelvic_incidence = 51.813768,l1_s1 = 61.507721,l1_pelvic_angle = 3.99,t1_t10 = -34.521921) {-1.6967976+0.57886789*pelvic_incidence-0.42474421*l1_s1+0.16316696*l1_pelvic_angle-0.10664331*t1_t10 }


final_recompute_tpa_estimation_function <- function(t1_l1 = -40.878235,l1_s1 = 61.507721,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99) {-3.3307018-0.22082989*t1_l1-0.52888684*l1_s1+0.62555837*pelvic_incidence+0.21351602*l1_pelvic_angle }


# target lpa function 
# target_l1pa_function <- function(pelvic_incidence = 51.813768) {-20.756157+0.48031254*pelvic_incidence}

# target tpa functions

target_tpa_function_uiv_t2 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = 0.94618014) {1.5231616+0.02763269*pelvic_incidence+0.85064863*l1_pelvic_angle+0.099964075*t1_uiv }
target_tpa_function_uiv_t3 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -1.2088311) {1.70789+0.027886814*pelvic_incidence+0.84868888*l1_pelvic_angle+0.068917268*t1_uiv }
target_tpa_function_uiv_t4 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -6.0813202) {1.9836536+0.02789039*pelvic_incidence+0.84661008*l1_pelvic_angle+0.055743457*t1_uiv }
target_tpa_function_uiv_t5 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -11.697777) {2.0678352+0.028190792*pelvic_incidence+0.84562542*l1_pelvic_angle+0.038458263*t1_uiv }
target_tpa_function_uiv_t6 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -17.5) {2.0531331+0.028920874*pelvic_incidence+0.84586576*l1_pelvic_angle+0.027025629*t1_uiv }
target_tpa_function_uiv_t7 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -22.772762) {1.9312848+0.029170745*pelvic_incidence+0.84501571*l1_pelvic_angle+0.015570263*t1_uiv }
target_tpa_function_uiv_t8 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -27.797818) {1.8042018+0.029251167*pelvic_incidence+0.84399235*l1_pelvic_angle+0.0082900481*t1_uiv }
target_tpa_function_uiv_t9 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -31.579794) {1.5543092+0.02808007*pelvic_incidence+0.84453728*l1_pelvic_angle-0.0021910686*t1_uiv }
target_tpa_function_uiv_t10 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -34.521921) {1.1851506+0.023566833*pelvic_incidence+0.84947722*l1_pelvic_angle-0.018723385*t1_uiv }
target_tpa_function_uiv_t11 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -37.985557) {0.8339583+0.013825771*pelvic_incidence+0.86385382*l1_pelvic_angle-0.037864861*t1_uiv }
target_tpa_function_uiv_t12 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = 43.650197) {-0.0039168638-0.013645844*pelvic_incidence+0.91687395*l1_pelvic_angle+0.080217847*t1_uiv }
target_tpa_function_uiv_l1 <- function(pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -40.878235) {-0.35239447-0.034912779*pelvic_incidence+0.97523477*l1_pelvic_angle-0.11367415*t1_uiv }

## target L1S1 functions:

target_l1_s1_function_uiv_t2 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99, t1_uiv = 0.94618014) {-0.78390516-0.8541968*tpa+1.3860027*pelvic_incidence-0.95156823*l1_pelvic_angle+0.0154249*t1_uiv }

target_l1_s1_function_uiv_t3 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -1.2088311) {-0.701207-0.85868568*tpa+1.3858943*pelvic_incidence-0.94604767*l1_pelvic_angle+0.042140798*t1_uiv }

target_l1_s1_function_uiv_t4 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -6.0813202) {-0.760968-0.8529425*tpa+1.3860797*pelvic_incidence-0.95353883*l1_pelvic_angle+0.0018121196*t1_uiv }

target_l1_s1_function_uiv_t5 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -11.697777) {-1.2708752-0.84670088*tpa+1.386236*pelvic_incidence-0.9603437*l1_pelvic_angle-0.040524015*t1_uiv }

target_l1_s1_function_uiv_t6 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -17.5) {-2.0869719-0.84318115*tpa+1.3845711*pelvic_incidence-0.96656309*l1_pelvic_angle-0.078299231*t1_uiv }

target_l1_s1_function_uiv_t7 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -22.772762) {-3.1515569-0.84273773*tpa+1.3808751*pelvic_incidence-0.96783852*l1_pelvic_angle-0.11274674*t1_uiv }

target_l1_s1_function_uiv_t8 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -27.797818) {-4.6263441-0.84372226*tpa+1.3711517*pelvic_incidence-0.95659991*l1_pelvic_angle-0.15982086*t1_uiv }

target_l1_s1_function_uiv_t9 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -31.579794) {-5.1422323-0.8559117*tpa+1.3509356*pelvic_incidence-0.92562774*l1_pelvic_angle-0.18865146*t1_uiv }

target_l1_s1_function_uiv_t10 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -34.521921) {-5.7309518-0.88949573*tpa+1.3283402*pelvic_incidence-0.8602138*l1_pelvic_angle-0.22364937*t1_uiv }

target_l1_s1_function_uiv_t11 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -37.985557) {-5.7065843-0.94913004*tpa+1.2920949*pelvic_incidence-0.74301577*l1_pelvic_angle-0.24981569*t1_uiv }

target_l1_s1_function_uiv_t12 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = 43.650197) {-6.7340885-1.094308*tpa+1.2267352*pelvic_incidence-0.46328088*l1_pelvic_angle+0.31649091*t1_uiv }

target_l1_s1_function_uiv_l1 <- function(tpa = 6.3128833,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99,t1_uiv = -40.878235) {-6.0707779-1.2471896*tpa+1.205252*pelvic_incidence-0.2239274*l1_pelvic_angle-0.3443794*t1_uiv }


target_uiv_s1_function <- function(l1_pelvic_angle, pelvic_incidence, t1_uiv, tpa, uiv){
  if(uiv == "T2"){
    uiv_s1 <-  -16.359288+1.3634681*l1_pelvic_angle+0.85403114*pelvic_incidence+0.26409409*t1_uiv-2.1364448*tpa
  }
  if(uiv == "T3"){
    uiv_s1 <- -14.740007+1.3181786*l1_pelvic_angle+0.85714161*pelvic_incidence-0.17787558*t1_uiv-2.1160646*tpa
  }
  if(uiv == "T4"){
    uiv_s1 <-   -10.237867+1.3159351*l1_pelvic_angle+0.85582215*pelvic_incidence-0.16071812*t1_uiv-2.1340071*tpa
  }
  if(uiv == "T5"){
    uiv_s1 <-   -5.7882495+1.3008463*l1_pelvic_angle+0.8579471*pelvic_incidence-0.1549356*t1_uiv-2.1223542*tpa
  }
  if(uiv == "T6"){
    uiv_s1 <-   -1.5692035+1.3067624*l1_pelvic_angle+0.87785449*pelvic_incidence-0.13033582*t1_uiv-2.1034693*tpa 
  }
  if(uiv == "T7"){
    uiv_s1 <-   1.0347153+1.2667369*l1_pelvic_angle+0.89868149*pelvic_incidence-0.18492344*t1_uiv-2.0699599*tpa
  }
  if(uiv == "T8"){
    uiv_s1 <-  2.258998+1.1787861*l1_pelvic_angle+0.93237964*pelvic_incidence-0.23611004*t1_uiv-2.0409742*tpa
  }
  if(uiv == "T9"){
    uiv_s1 <-   0.32086187+1.0592139*l1_pelvic_angle+0.99356539*pelvic_incidence-0.28849053*t1_uiv-1.9860014*tpa
  }
  if(uiv == "T10"){
    uiv_s1 <-  -1.2887932+0.88485713*l1_pelvic_angle+1.0341659*pelvic_incidence-0.32929021*t1_uiv-1.8878055*tpa
  }
  if(uiv == "T11"){
    uiv_s1 <-  -3.9259699+0.64298682*l1_pelvic_angle+1.0940039*pelvic_incidence-0.38067382*t1_uiv-1.7591169*tpa
  }
  if(uiv == "T12"){
    uiv_s1 <-  -7.0129402+0.30554656*l1_pelvic_angle+1.1458131*pelvic_incidence+0.40465953*t1_uiv-1.5550413*tpa
  }
  if(uiv == "L1"){
    uiv_s1 <-  -6.0707779-0.2239274*l1_pelvic_angle+1.205252*pelvic_incidence-0.3443794*t1_uiv-1.2471896*tpa
  }
  uiv_s1
  
}


##################### NEW OPTIMIZATION FUNCTIONS ######################
compute_segmental_lordosis_range_by_pi_function <- function(pelvic_incidence){
  range_list <- list()
  
  range_list$min_l5_s1 <- 10.408967+0.013929381* pelvic_incidence-3.0046014e-05*pmax(pelvic_incidence-24,0)^3+1.5596983e-05*pmax(pelvic_incidence-42,0)^3+4.0520455e-05*pmax(pelvic_incidence-60,0)^3-7.6478055e-06*pmax(pelvic_incidence-78,0)^3-1.8423619e-05*pmax(pelvic_incidence-96,0)^3
  range_list$max_l5_s1 <- 41.018009-0.14397804* pelvic_incidence+9.2685903e-05*pmax(pelvic_incidence-24,0)^3-0.00028895757*pmax(pelvic_incidence-42,0)^3+0.00042783603*pmax(pelvic_incidence-60,0)^3-0.00035954296*pmax(pelvic_incidence-78,0)^3+0.0001279786*pmax(pelvic_incidence-96,0)^3
  
  range_list$min_l4_l5 <- -0.4518589+0.12610528* pelvic_incidence+9.1910983e-05*pmax(pelvic_incidence-24,0)^3-0.00036860101*pmax(pelvic_incidence-42,0)^3+0.00043108816*pmax(pelvic_incidence-60,0)^3-0.00012401723*pmax(pelvic_incidence-78,0)^3-3.0380905e-05*pmax(pelvic_incidence-96,0)^3
  
  range_list$max_l4_l5 <- 23.30343-0.01338107* pelvic_incidence+0.00018743572*pmax(pelvic_incidence-24,0)^3-0.00060007394*pmax(pelvic_incidence-42,0)^3+0.00072781194*pmax(pelvic_incidence-60,0)^3-0.00040514492*pmax(pelvic_incidence-78,0)^3+8.9971206e-05*pmax(pelvic_incidence-96,0)^3
  
  range_list$min_l3_l4 <- -7.1777623+0.25902148* pelvic_incidence-9.4729898e-05*pmax(pelvic_incidence-24,0)^3+0.00022222281*pmax(pelvic_incidence-42,0)^3-0.00024900242*pmax(pelvic_incidence-60,0)^3+0.00021025601*pmax(pelvic_incidence-78,0)^3-8.8746503e-05*pmax(pelvic_incidence-96,0)^3
  
  range_list$max_l3_l4 <- 10.353063+0.16521222* pelvic_incidence-2.8337622e-05*pmax(pelvic_incidence-24,0)^3+6.0777442e-05*pmax(pelvic_incidence-42,0)^3-2.6507929e-05*pmax(pelvic_incidence-60,0)^3-1.596598e-05*pmax(pelvic_incidence-78,0)^3+1.0034089e-05*pmax(pelvic_incidence-96,0)^3 
  
  range_list$min_l2_l3 <- -8.9032528+0.25107274* pelvic_incidence-9.7232461e-05*pmax(pelvic_incidence-24,0)^3+0.00030716779*pmax(pelvic_incidence-42,0)^3-0.00037058998*pmax(pelvic_incidence-60,0)^3+0.00020860645*pmax(pelvic_incidence-78,0)^3-4.7951792e-05*pmax(pelvic_incidence-96,0)^3
  
  range_list$max_l2_l3 <- 8.3469516+0.15917512* pelvic_incidence-2.8049006e-05*pmax(pelvic_incidence-24,0)^3+0.00013532709*pmax(pelvic_incidence-42,0)^3-0.00014498172*pmax(pelvic_incidence-60,0)^3-3.8218025e-06*pmax(pelvic_incidence-78,0)^3+4.1525441e-05*pmax(pelvic_incidence-96,0)^3
  
  range_list$min_l1_l2 <- -8.9032528+0.25107274* pelvic_incidence-9.7232461e-05*pmax(pelvic_incidence-24,0)^3+0.00030716779*pmax(pelvic_incidence-42,0)^3-0.00037058998*pmax(pelvic_incidence-60,0)^3+0.00020860645*pmax(pelvic_incidence-78,0)^3-4.7951792e-05*pmax(pelvic_incidence-96,0)^3
  
  range_list$max_l1_l2 <- 8.3469516+0.15917512* pelvic_incidence-2.8049006e-05*pmax(pelvic_incidence-24,0)^3+0.00013532709*pmax(pelvic_incidence-42,0)^3-0.00014498172*pmax(pelvic_incidence-60,0)^3-3.8218025e-06*pmax(pelvic_incidence-78,0)^3+4.1525441e-05*pmax(pelvic_incidence-96,0)^3
  
  return(range_list)
  
}

# screening_optimized_pso_level_lordosis_values <- function(pelvic_incidence,
#                                                                l1_l2_start, 
#                                                                l2_l3_start, 
#                                                                l3_l4_start, 
#                                                                l4_l5_start, 
#                                                                l5_s1_start,
#                                                                desired_l1pa, 
#                                                                non_modifiable = NULL) {
#   # Define the equation constants
#   constant1 <- -0.8553283
#   constant2 <- 0.60405938
#   constant3 <- -0.048137209
#   constant4 <- -0.17727093
#   constant5 <- -0.32237285
#   constant6 <- -0.48541991
#   constant7 <- -0.56294456
#   
#   # Define the objective function to minimize
#   objective <- function(x) {
#     l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, x[1])
#     l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, x[2])
#     l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, x[3])
#     l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, x[4])
#     l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, x[5])
#     
#     l1pa_diff <- desired_l1pa - (constant1 + constant2 * pelvic_incidence + constant3 * l1_l2 + constant4 * l2_l3 + constant5 * l3_l4 + constant6 * l4_l5 + constant7 * l5_s1)
#     sum_diff_sq <- l1pa_diff^2
#     
#     return(sum_diff_sq)
#   }
#   
#   # Set the starting values
#   start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
#   
#   # Use the Nelder-Mead method to find the new values
#   result <- optim(start_values, objective, method = "Nelder-Mead")
#   
#   # Extract the optimized values
#   new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
#   new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
#   new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
#   new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
#   new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
#   
#   
#   
#     
#     # Return the new values
#     return(list(l1_l2 = new_l1_l2, 
#                 l2_l3 = new_l2_l3,
#                 l3_l4 = new_l3_l4, 
#                 l4_l5 = new_l4_l5,
#                 l5_s1 = new_l5_s1))
#   
#   
# }

### Estimate C2PA based on all segment angles:
estimate_c2pa_by_segment_angles_function <- function(pelvic_incidence = 51.75934,c2_c3 = 7.2418723,c3_c4 = 2.3525818,c4_c5 = -3.5602989,c5_c6 = -1.0274844,c6_c7 = 5.0640539,c7_t1 = 7.6006935,t1_t2 = 0.91990553,t2_t3 = -2.4063826,t3_t4 = -4.9977997,t4_t5 = -5.2867851,t5_t6 = -5.6352776,t6_t7 = -5.5675075,t7_t8 = -5.0805516,t8_t9 = -3.5695003,t9_t10 = -2.5956493,t10_t11 = -3.2007225,t11_t12 = -2.3724442,t12_l1 = -0.4781281,l1_l2 = 9.4329344,l2_l3 = 9.4329344,l3_l4 = 10.829323,l4_l5 = 16.604356,l5_s1 = 23.962803) {3.4012531+0.73821384*pelvic_incidence-0.018245053*c2_c3+0.032565312*c3_c4-0.0080538546*c4_c5-0.079163606*c5_c6-0.05851955*c6_c7-0.11024772*c7_t1-0.066980974*t1_t2-0.077286616*t2_t3-0.16573273*t3_t4-0.13414171*t4_t5-0.29557704*t5_t6-0.29613537*t6_t7-0.20529031*t7_t8-0.24658276*t8_t9-0.31277171*t9_t10-0.33952342*t10_t11-0.53084966*t11_t12-0.61709964*t12_l1-0.78140907*l1_l2NA*l2_l3-0.58582961*l3_l4-0.68621048*l4_l5-0.73429114*l5_s1 }

estimate_t4pa_by_segment_angles_function <- function(pelvic_incidence = 51.75934,t4_t5 = -5.2867851,t5_t6 = -5.6352776,t6_t7 = -5.5675075,t7_t8 = -5.0805516,t8_t9 = -3.5695003,t9_t10 = -2.5956493,t10_t11 = -3.2007225,t11_t12 = -2.3724442,t12_l1 = -0.4781281,l1_l2 = 9.4329344,l2_l3 = 9.4329344,l3_l4 = 10.829323,l4_l5 = 16.604356,l5_s1 = 23.962803) {
value <- 1.823421+0.71023982*pelvic_incidence+0.058950934*t4_t5-0.089272646*t5_t6-0.11025113*t6_t7-0.079388449*t7_t8-0.11275605*t8_t9-0.19063858*t9_t10-0.18579882*t10_t11-0.40553368*t11_t12-0.49248536*t12_l1-0.67656593*l1_l2 -0.62256365*l2_l3 -0.54256365*l3_l4 -0.64512353*l4_l5 -0.70240693*l5_s1 
value  
}

  estimate_t9pa_by_segment_angles_function <- function(pelvic_incidence = 51.75934,t9_t10 = -2.5956493,t10_t11 = -3.2007225,t11_t12 = -2.3724442,t12_l1 = -0.4781281,l1_l2 = 9.4329344,l2_l3 = 9.4329344,l3_l4 = 10.829323,l4_l5 = 16.604356,l5_s1 = 23.962803) {0.75210312+0.67215468*pelvic_incidence+0.053470087*t9_t10-0.017375742*t10_t11-0.21574515*t11_t12-0.32073926*t12_l1-0.52601682*l1_l2NA*l2_l3-0.46793282*l3_l4-0.59719649*l4_l5-0.65617842*l5_s1 }
  # estimate_l1pa_by_segment_angles_function <- function(pelvic_incidence = 51.75934,l1_l2 = 9.4329344,l2_l3 = 9.4329344,l3_l4 = 10.829323,l4_l5 = 16.604356,l5_s1 = 23.962803) {-0.69511147+0.59711132*pelvic_incidence-0.19584192*l1_l2NA*l2_l3-0.31393893*l3_l4-0.47965972*l4_l5-0.55809141*l5_s1 }
  
  estimate_l1pa_by_lumbar_segment_angles_function <- function(l1_l2 = 2.765,
                                                              l2_l3 = 6.565,
                                                              l3_l4 = 11.44,
                                                              l4_l5 = 12.57,
                                                              l5_s1 = 24.885,
                                                              pelvic_incidence = 51.66) {
    -2.8027097-0.053209638*l1_l2-0.18966575*l2_l3-0.2754969*l3_l4-0.50404662*l4_l5-0.58264784*l5_s1+0.62320748*pelvic_incidence 
  }
  
  estimate_l4pa_by_segment_angles_function <- function(pelvic_incidence = 51.75934,l4_l5 = 16.604356,l5_s1 = 23.962803) {-1.8950945+0.36581368*pelvic_incidence-0.1267721*l4_l5-0.278859*l5_s1 }
  
  
############ COMPUTING OPTIMAL SEGMENT ANGLES ###################
############ COMPUTING OPTIMAL SEGMENT ANGLES ###################
############ COMPUTING OPTIMAL SEGMENT ANGLES ###################
############ COMPUTING OPTIMAL SEGMENT ANGLES ###################
############ COMPUTING OPTIMAL SEGMENT ANGLES ###################

############################ UIV OF L4 ##########################
  compute_optimized_segmental_angles_uiv_l4 <- function(pelvic_incidence, 
                                                        l4_l5_start,
                                                        l5_s1_start,
                                                        desired_l4pa, 
                                                        nonmodifiable = NULL) {
    
    segmental_lordosis_range_list <- compute_segmental_lordosis_range_by_pi_function(pelvic_incidence = pelvic_incidence)
    
    nonmodifiable <- str_to_lower(str_replace_all(nonmodifiable, "-", "_"))
    
    results_list <- list()
    # Define the equation constants
    coeff_l4pa <- -1.8950945
    coeff_pelvic_incidence <- 0.36581368
    coeff_l4_l5 <- -0.1267721
    coeff_l5_s1 <- -0.278859
    
    if(length(nonmodifiable) == 0){
      l4_l5_modifiable <- TRUE
      l5_s1_modifiable <- TRUE
    }else{
      l4_l5_modifiable <- if_else("l4_l5" %in% nonmodifiable, FALSE, TRUE)
      l5_s1_modifiable <- if_else("l5_s1" %in% nonmodifiable, FALSE, TRUE) 
    }
    
    if(l4_l5_modifiable == FALSE & l5_s1_modifiable == FALSE){
      new_l4_l5 <- l4_l5_start
      new_l5_s1 <- l5_s1_start
      
      results_list$l4_l5 <- new_l4_l5
      results_list$l5_s1 <- new_l5_s1
      results_list$needs_pso <- "yes"
      
    }else{
      if(l4_l5_modifiable == TRUE  && l5_s1_modifiable == FALSE){
        new_l4_l5 <- (desired_l4pa + 1.8950945 + 0.278859*l5_s1_start - 0.36581368*pelvic_incidence)/-0.1267721 
        new_l5_s1 <- l5_s1_start
        
        if(between(new_l4_l5, segmental_lordosis_range_list$min_l4_l5, segmental_lordosis_range_list$max_l4_l5) == FALSE){
          results_list$l4_l5 <- 3.0407048+0.34618218*pelvic_incidence-0.50845476*desired_l4pa
          results_list$l5_s1 <- l5_s1_start
          results_list$needs_pso <- "yes"
        }else{
          results_list$l4_l5 <- new_l4_l5
          results_list$l5_s1 <- new_l5_s1
          results_list$needs_pso <- "no"
        }
        
        
        
      }else if(l4_l5_modifiable == FALSE && l5_s1_modifiable == TRUE){
        new_l4_l5 <- l4_l5_start
        new_l5_s1 <- (desired_l4pa + 1.8950945 + 0.1267721*l4_l5_start - 0.36581368*pelvic_incidence)/-0.278859 
        
        results_list$l4_l5 <- new_l4_l5
        results_list$l5_s1 <- new_l5_s1
        results_list$needs_pso <- "no"
        
      }else{
        new_l5_s1 <- 0.70766215+0.83830614*pelvic_incidence-2.4786414*desired_l4pa
        new_l4_l5 <- (desired_l4pa + 1.8950945 + 0.278859*new_l5_s1 - 0.36581368*pelvic_incidence)/-0.1267721 
        
        results_list$l4_l5 <- new_l4_l5
        results_list$l5_s1 <- new_l5_s1
        results_list$needs_pso <- "no"
      }
    } 
    
    
    # Return the new values
    return(results_list)
    
  }

############################ UIV OF L2 ##########################
# compute_optimized_lumbar_segmental_lordosis_values <- function(pelvic_incidence,
#                                  l1_l2_start, 
#                                  l2_l3_start, 
#                                  l3_l4_start, 
#                                  l4_l5_start, 
#                                  l5_s1_start,
#                                  desired_l1pa, 
#                                  non_modifiable = NULL) {
#   
#   non_modifiable <- str_to_lower(str_replace_all(non_modifiable, "-", "_"))
#   
#   results_list <- list()
#   
#   # Define the equation constants
#   constant1 <- -0.8553283
#   constant2 <- 0.60405938
#   constant3 <- -0.048137209
#   constant4 <- -0.17727093
#   constant5 <- -0.32237285
#   constant6 <- -0.48541991
#   constant7 <- -0.56294456
#   
#   # Define the objective function to minimize the change at each given level
#   objective <- function(x) {
#     l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, x[1])
#     l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, x[2])
#     l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, x[3])
#     l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, x[4])
#     l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, x[5])
#     
#     l1pa_diff <- desired_l1pa - (constant1 + constant2 * pelvic_incidence + constant3 * l1_l2 + constant4 * l2_l3 + constant5 * l3_l4 + constant6 * l4_l5 + constant7 * l5_s1)
#     sum_diff_sq <- l1pa_diff^2
#     
#     return(sum_diff_sq)
#   }
#   
#   # Set the starting values
#   start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
#   
#   # Use the Nelder-Mead method to find the new values
#   result <- optim(start_values, objective, method = "Nelder-Mead")
#   
#   optim
#   
#   # Extract the optimized values
#   new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
#   new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
#   new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
#   new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
#   new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
#   
#   segmental_lordosis_range_list <- compute_segmental_lordosis_range_by_pi_function(pelvic_incidence = pelvic_incidence)
#   
#   if (!is.null(new_l1_l2) && new_l1_l2 < segmental_lordosis_range_list$min_l1_l2) {
#     # Set the starting values
#     new_l1_l2 <- segmental_lordosis_range_list$min_l1_l2
#     
#     l1_l2_start <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, new_l1_l2)
#     
#     start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
#     
#     non_modifiable <- unique(append(non_modifiable, "l1_l2"))
#     
#     # Use the Nelder-Mead method to find the new values
#     result <- optim(start_values, objective, method = "Nelder-Mead")
#     
#     # Extract the optimized values
#     new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
#     new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
#     new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
#     new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
#     new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
#     
#     results_list$min_l1_l2 <- "min l1_l2 if statement utilized"
#   }
#   
#   if (!is.null(new_l2_l3) && new_l2_l3 > segmental_lordosis_range_list$min_l2_l3) {
#     # Set the starting values
#     new_l2_l3 <- segmental_lordosis_range_list$min_l2_l3
#     
#     l2_l3_start <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, new_l2_l3)
#     
#     start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
#     
#     non_modifiable <- unique(append(non_modifiable, "l2_l3"))
#     
#     # Use the Nelder-Mead method to find the new values
#     result <- optim(start_values, objective, method = "Nelder-Mead")
#     
#     # Extract the optimized values
#     new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
#     new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
#     new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
#     new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
#     new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
#     
#     results_list$min_l2_l3 <- "min l2_l3 if statement utilized"
#   }
#   
#   if (!is.null(new_l3_l4) && new_l3_l4 > segmental_lordosis_range_list$min_l3_l4) {
#     new_l3_l4 <- segmental_lordosis_range_list$min_l3_l4
#     
#     l3_l4_start <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, new_l3_l4)
#     
#     start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
#     
#     non_modifiable <- unique(append(non_modifiable, "l3_l4"))
#     
#     # Use the Nelder-Mead method to find the new values
#     result <- optim(start_values, objective, method = "Nelder-Mead")
#     
#     # Extract the optimized values
#     new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
#     new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
#     new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
#     new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
#     new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
#     
#     results_list$min_l3_l4 <- "min l3_l4 if statement utilized"
#     
#     # return("Maximum value exceeded for l3_l4")
#   }
#   
#   if (!is.null(new_l5_s1) && new_l5_s1 > segmental_lordosis_range_list$max_l5_s1) {
#     new_l5_s1 <- segmental_lordosis_range_list$max_l5_s1
#     
#     l5_s1_start <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, new_l5_s1)
#     
#     start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
#     
#     non_modifiable <- unique(append(non_modifiable, "l5_s1"))
#     
#     # Use the Nelder-Mead method to find the new values
#     result <- optim(start_values, objective, method = "Nelder-Mead")
#     
#     # Extract the optimized values
#     new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
#     new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
#     new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
#     new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
#     new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
#     
#     results_list$max_l5_s1 <- "max l5_s1 if statement utilized"
#   }
#   
#   if (!is.null(new_l4_l5) && new_l4_l5 > segmental_lordosis_range_list$max_l4_l5) {
#     new_l4_l5 <- segmental_lordosis_range_list$max_l4_l5
#     
#     l4_l5_start <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, new_l4_l5)
#     
#     start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
#     
#     non_modifiable <- unique(append(non_modifiable, "l4_l5"))
#     
#     # Use the Nelder-Mead method to find the new values
#     result <- optim(start_values, objective, method = "Nelder-Mead")
#     
#     # Extract the optimized values
#     new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
#     new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
#     new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
#     new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
#     new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
#     
#     results_list$max_l4_l5 <- "max l4_l5 if statement utilized"
#     
#   }
#   
#   if (!is.null(new_l3_l4) && new_l3_l4 > segmental_lordosis_range_list$max_l3_l4) {
#     new_l3_l4 <- segmental_lordosis_range_list$max_l3_l4
#     
#     l3_l4_start <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, new_l3_l4)
#     
#     start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
#     
#     non_modifiable <- unique(append(non_modifiable, "l3_l4"))
#     
#     # Use the Nelder-Mead method to find the new values
#     result <- optim(start_values, objective, method = "Nelder-Mead")
#     
#     # Extract the optimized values
#     new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
#     new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
#     new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
#     new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
#     new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
#     
#     results_list$max_l3_l4 <- "max l3_l4 if statement utilized"
#     
#     # return("Maximum value exceeded for l3_l4")
#   }
#   
#   if (!is.null(new_l2_l3) && new_l2_l3 > segmental_lordosis_range_list$max_l2_l3) {
#     # Set the starting values
#     new_l2_l3 <- segmental_lordosis_range_list$max_l2_l3
#     
#     l2_l3_start <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, new_l2_l3)
#     
#     start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
#     
#     non_modifiable <- unique(append(non_modifiable, "l2_l3"))
#     
#     # Use the Nelder-Mead method to find the new values
#     result <- optim(start_values, objective, method = "Nelder-Mead")
#     
#     # Extract the optimized values
#     new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
#     new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
#     new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
#     new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
#     new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
#     
#     results_list$max_l2_l3 <- "max l2_l3 if statement utilized"
#     
#     # return("Maximum value exceeded for l2_l3")
#   }
#   
#   
#   if (!is.null(new_l1_l2) && new_l1_l2 > segmental_lordosis_range_list$max_l1_l2) {
#     # Set the starting values
#     new_l1_l2 <- segmental_lordosis_range_list$max_l1_l2
#     
#     l1_l2_start <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, new_l1_l2)
#     
#     start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
#     
#     non_modifiable <- unique(append(non_modifiable, "l1_l2"))
#     
#     # Use the Nelder-Mead method to find the new values
#     result <- optim(start_values, objective, method = "Nelder-Mead")
#     
#     # Extract the optimized values
#     new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
#     new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
#     new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
#     new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
#     new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
#     
#     results_list$max_l1_l2 <- "max l1_l2 if statement utilized"
#     
#     # return("Maximum value exceeded for l2_l3")
#   }
#   
#   
#   if(
#     # between(new_l1_l2, segmental_lordosis_range_list$min_l1_l2, segmental_lordosis_range_list$max_l1_l2) == FALSE |
#     #  between(new_l2_l3, segmental_lordosis_range_list$min_l2_l3, segmental_lordosis_range_list$max_l2_l3) == FALSE |
#     #  between(new_l3_l4, segmental_lordosis_range_list$min_l3_l4, segmental_lordosis_range_list$max_l3_l4) == FALSE |
#     #  between(new_l4_l5, segmental_lordosis_range_list$min_l4_l5, segmental_lordosis_range_list$max_l4_l5) == FALSE |
#      between(new_l5_s1, segmental_lordosis_range_list$min_l5_s1, segmental_lordosis_range_list$max_l5_s1) == FALSE){
#     
#     needs_pso <- "yes"
#     
#     results_list$segmental_range_exceeded <- "final segmental range if statement utilized"
# 
#   }else{
#     needs_pso <- "no"
#   }
#   
#   new_l1pa <- (constant1 + constant2 * pelvic_incidence + constant3 * new_l1_l2 + constant4 * new_l2_l3 + constant5 * new_l3_l4 + constant6 * new_l4_l5 + constant7 * new_l5_s1)
#   
#   if(abs(new_l1pa - desired_l1pa) > 2){
#     needs_pso <- "yes"
#   }
#   
#   results_list$l1_l2 <- new_l1_l2
#   results_list$l2_l3 <- new_l2_l3
#   results_list$l3_l4 <- new_l3_l4
#   results_list$l4_l5 <- new_l4_l5
#   results_list$l5_s1 <- new_l5_s1
#   results_list$needs_pso <- needs_pso
#   
#   # Return the new values
#   return(results_list)
# 
# }
  
  # compute_optimized_lumbar_segmental_lordosis_values <- function(pelvic_incidence,
  #                                                                l1_l2_start, 
  #                                                                l2_l3_start, 
  #                                                                l3_l4_start, 
  #                                                                l4_l5_start, 
  #                                                                l5_s1_start,
  #                                                                desired_l1pa, 
  #                                                                non_modifiable = NULL) {
  #   
  #   non_modifiable <- str_to_lower(str_replace_all(non_modifiable, "-", "_"))
  #   
  #   results_list <- list()
  #   
  #   # Define the equation constants
  #   constant1 <- -0.8553283
  #   constant2 <- 0.60405938
  #   constant3 <- -0.048137209
  #   constant4 <- -0.17727093
  #   constant5 <- -0.32237285
  #   constant6 <- -0.48541991
  #   constant7 <- -0.56294456
  #   
  #   # Define the objective function to minimize the change at each given level
  #   objective <- function(x) {
  #     l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, x[1])
  #     l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, x[2])
  #     l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, x[3])
  #     l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, x[4])
  #     l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, x[5])
  #     
  #     l1pa_diff <- desired_l1pa - (constant1 + constant2 * pelvic_incidence + constant3 * l1_l2 + constant4 * l2_l3 + constant5 * l3_l4 + constant6 * l4_l5 + constant7 * l5_s1)
  #     sum_diff_sq <- l1pa_diff^2
  #     
  #     return(sum_diff_sq)
  #   }
  #   
  #   # Set the starting values
  #   start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
  #   
  #   # Use the Nelder-Mead method to find the new values
  #   result <- optim(start_values, objective, method = "Nelder-Mead")
  #   
  #   # Extract the optimized values
  #   new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
  #   new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
  #   new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
  #   new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
  #   new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
  #   
  #   segmental_lordosis_range_list <- compute_segmental_lordosis_range_by_pi_function(pelvic_incidence = pelvic_incidence)
  #   
  #   new_sa_values_df <- tibble(level = c("l5_s1", "l4_l5", "l3_l4", "l2_l3", "l1_l2"), 
  #                              value = c(new_l5_s1, new_l4_l5, new_l3_l4, new_l2_l3, new_l1_l2))
  # 
  #   
  #   confirming_segment_angles_df <- enframe(segmental_lordosis_range_list) %>%
  #     unnest(value) %>%
  #     mutate(min_max = if_else(str_detect(name, "min"), "min", "max")) %>%
  #     mutate(level = str_remove_all(name, "min_|max_")) %>%
  #     select(level, min_max, value) %>%
  #     pivot_wider(names_from = min_max, values_from = value) %>%
  #     left_join(new_sa_values_df) %>%
  #     mutate(confirmed_value = case_when(
  #       value < min ~ min,
  #       value > max ~ max, 
  #       TRUE ~ value
  #     )) %>%
  #     mutate(confirmed_value_limit = case_when(
  #       value < min ~ "out_of_range",
  #       value > max ~ "out_of_range", 
  #       TRUE ~ "within_range"
  #     ))
  #   
  #   new_segment_angles_list <- as.list(confirming_segment_angles_df$confirmed_value)
  #   
  #   names(new_segment_angles_list) <- confirming_segment_angles_df$level
  #   
  #   # if(any(confirming_segment_angles_df$confirmed_value_limit == "out_of_range")){
  #   #   
  #   #   levels_maxed <- (confirming_segment_angles_df %>%
  #   #                      filter(confirmed_value_limit == "out_of_range"))$level
  #   #   
  #   #   l1_l2_start = new_segment_angles_list$l1_l2 
  #   #   l2_l3_start = new_segment_angles_list$l2_l3
  #   #   l3_l4_start = new_segment_angles_list$l3_l4 
  #   #   l4_l5_start = new_segment_angles_list$l4_l5
  #   #   l5_s1_start = new_segment_angles_list$l5_s1
  #   #   
  #   #   start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
  #   #   
  #   #   non_modifiable <- unique(append(non_modifiable, levels_maxed))
  #   #   
  #   #   # Use the Nelder-Mead method to find the new values
  #   #   result <- optim(start_values, objective, method = "Nelder-Mead")
  #   #   
  #   #   # Extract the optimized values
  #   #   new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
  #   #   new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
  #   #   new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
  #   #   new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
  #   #   new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
  #   #   
  #   # }
  #   # 
  #   # new_sa_values_df <- tibble(level = c("l5_s1", "l4_l5", "l3_l4", "l2_l3", "l1_l2"), 
  #   #                            value = c(new_l5_s1, new_l4_l5, new_l3_l4, new_l2_l3, new_l1_l2))
  #   # 
  #   # 
  #   # confirming_segment_angles_df <- enframe(segmental_lordosis_range_list) %>%
  #   #   unnest(value) %>%
  #   #   mutate(min_max = if_else(str_detect(name, "min"), "min", "max")) %>%
  #   #   mutate(level = str_remove_all(name, "min_|max_")) %>%
  #   #   select(level, min_max, value) %>%
  #   #   pivot_wider(names_from = min_max, values_from = value) %>%
  #   #   left_join(new_sa_values_df) %>%
  #   #   mutate(confirmed_value = case_when(
  #   #     value < min ~ min,
  #   #     value > max ~ max, 
  #   #     TRUE ~ value
  #   #   )) %>%
  #   #   mutate(confirmed_value_limit = case_when(
  #   #     value < min ~ "out_of_range",
  #   #     value > max ~ "out_of_range", 
  #   #     TRUE ~ "within_range"
  #   #   ))
  # 
  #   
  #   if(any(confirming_segment_angles_df$confirmed_value_limit == "out_of_range")){
  #     needs_pso <- "yes"
  #     results_list$segmental_range_exceeded <- "final segmental range if statement utilized"
  #   }else{
  #     needs_pso <- "no"
  #   }
  #   
  #   new_l1pa <- (constant1 + constant2 * pelvic_incidence + constant3 * new_segment_angles_list$l1_l2 + constant4 * new_segment_angles_list$l2_l3 + constant5 * new_segment_angles_list$l3_l4 + constant6 * new_segment_angles_list$l4_l5 + constant7 * new_segment_angles_list$l5_s1)
  #   
  #   if(abs(new_l1pa - desired_l1pa) > 2){
  #     needs_pso <- "yes"
  #   }
  #   
  #   results_list$l1_s1 <- new_l1_l2 + new_l2_l3 + new_l3_l4 + new_l4_l5 + new_l5_s1 
  #   
  #   results_list$l1_l2 <- new_l1_l2
  #   results_list$l2_l3 <- new_l2_l3
  #   results_list$l3_l4 <- new_l3_l4
  #   results_list$l4_l5 <- new_l4_l5
  #   results_list$l5_s1 <- new_l5_s1
  #   results_list$estimated_l1pa <- round(new_l1pa, 1)
  #   results_list$needs_pso <- needs_pso
  #   
  #   # Return the new values
  #   return(results_list)
  #   
  # }
  
  
  
  
  compute_optimized_lumbar_segmental_lordosis_values <- function(pelvic_incidence,
                                                                 l1_l2_start, 
                                                                 l2_l3_start, 
                                                                 l3_l4_start, 
                                                                 l4_l5_start, 
                                                                 l5_s1_start,
                                                                 desired_l1pa, 
                                                                 non_modifiable = NULL, 
                                                                 pso_option_number = 1, 
                                                                 uiv_region = "upper") {
    
    non_modifiable <- str_to_lower(str_replace_all(non_modifiable, "-", "_"))
    
    results_list <- list()
    
    # Define the equation constants
    constant1 <- -2.8027097
    constant2 <- 0.62320748
    constant3 <- -0.053209638
    constant4 <- -0.18966575
    constant5 <- -0.2754969
    constant6 <- -0.50404662
    constant7 <- -0.58264784
    
    # Define the objective function to minimize the change at each given level
    objective <- function(x) {
      l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, x[1])
      l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, x[2])
      l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, x[3])
      l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, x[4])
      l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, x[5])
      
      l1pa_diff <- desired_l1pa - (constant1 + constant2 * pelvic_incidence + constant3 * l1_l2 + constant4 * l2_l3 + constant5 * l3_l4 + constant6 * l4_l5 + constant7 * l5_s1)
      
      sum_diff_sq <- l1pa_diff^2
      
      return(sum_diff_sq)
    }
    
    # Set the starting values
    start_values <- c(l1_l2_start, l2_l3_start, l3_l4_start, l4_l5_start, l5_s1_start)
    
    start_values_list <- list()
    start_values_list$l1_l2 <- l1_l2_start
    start_values_list$l2_l3 <- l2_l3_start
    start_values_list$l3_l4 <- l3_l4_start
    start_values_list$l4_l5 <- l4_l5_start
    start_values_list$l5_s1 <- l5_s1_start
    
    # Use the Nelder-Mead method to find the new values
    result <- optim(start_values, objective, method = "Nelder-Mead")
    
    # Extract the optimized values
    new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
    new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
    new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
    new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
    new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
    
    segmental_lordosis_range_list <- compute_segmental_lordosis_range_by_pi_function(pelvic_incidence = pelvic_incidence)
    
    new_sa_values_df <- tibble(level = c("l5_s1", "l4_l5", "l3_l4", "l2_l3", "l1_l2"), 
                               value = c(new_l5_s1, new_l4_l5, new_l3_l4, new_l2_l3, new_l1_l2))
    
    
    confirming_segment_angles_df <- enframe(segmental_lordosis_range_list) %>%
      unnest(value) %>%
      mutate(min_max = if_else(str_detect(name, "min"), "min", "max")) %>%
      mutate(level = str_remove_all(name, "min_|max_")) %>%
      select(level, min_max, value) %>%
      pivot_wider(names_from = min_max, values_from = value) %>%
      left_join(new_sa_values_df) %>%
      mutate(confirmed_value = case_when(
        value < min ~ min,
        value > max ~ max, 
        TRUE ~ value
      )) %>%
      mutate(confirmed_value_limit = case_when(
        value < min ~ "out_of_range",
        value > max ~ "out_of_range", 
        TRUE ~ "within_range"
      ))
    
    new_segment_angles_list <- as.list(confirming_segment_angles_df$confirmed_value)
    
    names(new_segment_angles_list) <- confirming_segment_angles_df$level
    
        new_l1pa <- estimate_l1pa_by_lumbar_segment_angles_function(l1_l2 = new_segment_angles_list$l1_l2, 
                                                                l2_l3 = new_segment_angles_list$l2_l3,
                                                                l3_l4 = new_segment_angles_list$l3_l4, 
                                                                l4_l5 = new_segment_angles_list$l4_l5,
                                                                l5_s1 = new_segment_angles_list$l5_s1, 
                                                                pelvic_incidence = pelvic_incidence)
    
    l1_s1_target_range <- c(estimate_l1s1_from_pi_l1pa_function(pelvic_incidence = pelvic_incidence, l1_pelvic_angle = new_l1pa) - 6, estimate_l1s1_from_pi_l1pa_function(pelvic_incidence = pelvic_incidence, l1_pelvic_angle = new_l1pa) + 6)
    
    results_list$l1_s1 <- new_segment_angles_list$l1_l2 + new_segment_angles_list$l2_l3 + new_segment_angles_list$l3_l4 + new_segment_angles_list$l4_l5 + new_segment_angles_list$l5_s1 

    if(abs(new_l1pa - desired_l1pa) > 3){
      needs_pso <- "yes"
    }else if(between(results_list$l1_s1, l1_s1_target_range[1], l1_s1_target_range[2]) == FALSE){
      needs_pso <- "yes"
    }else{
      needs_pso <- "no"
    }
    
    pso_level <- "na"
    
    if(needs_pso == "yes"){
        if(uiv_region == "upper"){
          pso_options_df <- compute_pso_segment_angles_for_upper_t_uiv_function(pelvic_incidence = pelvic_incidence,
                                                                                l5_segment_angle = l5_s1_start,
                                                                                l4_segment_angle = l4_l5_start,
                                                                                l3_segment_angle = l3_l4_start,
                                                                                l2_segment_angle = l2_l3_start,
                                                                                l1_segment_angle = l1_l2_start
          )
        }else{
          pso_options_df <- compute_pso_segment_angles_for_lower_t_uiv_function(pelvic_incidence = pelvic_incidence,
                                                                                l5_segment_angle = l5_s1_start,
                                                                                l4_segment_angle = l4_l5_start,
                                                                                l3_segment_angle = l3_l4_start,
                                                                                l2_segment_angle = l2_l3_start,
                                                                                l1_segment_angle = l1_l2_start
                                                                                # l5_segment_angle = new_segment_angles_list$l5_s1,
                                                                                # l4_segment_angle = new_segment_angles_list$l4_l5,
                                                                                # l3_segment_angle = new_segment_angles_list$l3_l4,
                                                                                # l2_segment_angle = new_segment_angles_list$l2_l3,
                                                                                # l1_segment_angle = new_segment_angles_list$l1_l2
          )
        }

        pso_level <- (pso_options_df %>%
                        filter(row_number() == pso_option_number))$segment[[1]]

        pso_adjustment <- (pso_options_df %>%
                             filter(row_number() == pso_option_number))$adjustment[[1]]

        # new_segment_angles_list <- as.list(confirming_segment_angles_df$confirmed_value)

        new_segment_angles_list$l5_s1 <- if_else(pso_level == "L5", l5_s1_start + pso_adjustment, l5_s1_start)
        new_segment_angles_list$l4_l5 <- if_else(pso_level == "L4", l4_l5_start + pso_adjustment, l4_l5_start)
        new_segment_angles_list$l3_l4 <- if_else(pso_level == "L3", l3_l4_start + pso_adjustment, l3_l4_start)
        new_segment_angles_list$l2_l3 <- if_else(pso_level == "L2", l2_l3_start + pso_adjustment, l2_l3_start)
        new_segment_angles_list$l1_l2 <- if_else(pso_level == "L1", l1_l2_start + pso_adjustment, l1_l2_start)
    }
       
    # if(needs_pso == "yes" & length(non_modifiable) >= 4){
    #   
    #   if(uiv_region == "upper"){
    #     pso_options_df <- compute_pso_segment_angles_for_upper_t_uiv_function(pelvic_incidence = pelvic_incidence, 
    #                                                                           l5_segment_angle = l5_s1_start, 
    #                                                                           l4_segment_angle = l4_l5_start, 
    #                                                                           l3_segment_angle = l3_l4_start,
    #                                                                           l2_segment_angle = l2_l3_start, 
    #                                                                           l1_segment_angle = l1_l2_start
    #     ) 
    #   }else{
    #     pso_options_df <- compute_pso_segment_angles_for_lower_t_uiv_function(pelvic_incidence = pelvic_incidence, 
    #                                                                           l5_segment_angle = l5_s1_start,
    #                                                                           l4_segment_angle = l4_l5_start,
    #                                                                           l3_segment_angle = l3_l4_start,
    #                                                                           l2_segment_angle = l2_l3_start,
    #                                                                           l1_segment_angle = l1_l2_start
    #                                                                           # l5_segment_angle = new_segment_angles_list$l5_s1, 
    #                                                                           # l4_segment_angle = new_segment_angles_list$l4_l5, 
    #                                                                           # l3_segment_angle = new_segment_angles_list$l3_l4,
    #                                                                           # l2_segment_angle = new_segment_angles_list$l2_l3, 
    #                                                                           # l1_segment_angle = new_segment_angles_list$l1_l2
    #     ) 
    #   }
    #   
    #   pso_level <- (pso_options_df %>%
    #                   filter(row_number() == pso_option_number))$segment[[1]]
    #   
    #   pso_adjustment <- (pso_options_df %>%
    #                        filter(row_number() == pso_option_number))$adjustment[[1]] 
    #   
    #   # new_segment_angles_list <- as.list(confirming_segment_angles_df$confirmed_value)
    #   
    #   new_segment_angles_list$l5_s1 <- if_else(pso_level == "L5", l5_s1_start + pso_adjustment, l5_s1_start)
    #   new_segment_angles_list$l4_l5 <- if_else(pso_level == "L4", l4_l5_start + pso_adjustment, l4_l5_start)
    #   new_segment_angles_list$l3_l4 <- if_else(pso_level == "L3", l3_l4_start + pso_adjustment, l3_l4_start)
    #   new_segment_angles_list$l2_l3 <- if_else(pso_level == "L2", l2_l3_start + pso_adjustment, l2_l3_start)
    #   new_segment_angles_list$l1_l2 <- if_else(pso_level == "L1", l1_l2_start + pso_adjustment, l1_l2_start)
    #   
    # 
    # }else if(needs_pso == "yes" & length(non_modifiable) < 4){
    #   lordosis_needed_list <- list()
    #   lordosis_needed_list$l1_l2 <- l4_l5_for_desired_l1pa_function(
    #     l1_pelvic_angle = desired_l1pa,
    #     pelvic_incidence = pelvic_incidence,
    #     l1_l2 = start_values_list$l1_l2,
    #     l2_l3 = start_values_list$l2_l3,
    #     l3_l4 = start_values_list$l3_l4,
    #     l5_s1 = start_values_list$l5_s1
    #   )
    # 
    #   lordosis_needed_list$l2_l3 <- l3_l4_for_desired_l1pa_function(
    #     l1_pelvic_angle = desired_l1pa,
    #     pelvic_incidence = pelvic_incidence,
    #     l1_l2 = start_values_list$l1_l2,
    #     l2_l3 = start_values_list$l2_l3,
    #     l4_l5 = start_values_list$l4_l5,
    #     l5_s1 = start_values_list$l5_s1
    #   )
    # 
    #   lordosis_needed_list$l3_l4 <- l3_l4_for_desired_l1pa_function(
    #     l1_pelvic_angle = desired_l1pa,
    #     pelvic_incidence = pelvic_incidence,
    #     l1_l2 = start_values_list$l1_l2,
    #     l2_l3 = start_values_list$l2_l3,
    #     l4_l5 = start_values_list$l4_l5,
    #     l5_s1 = start_values_list$l5_s1
    #   )
    # 
    #   lordosis_needed_list$l4_l5 <- l4_l5_for_desired_l1pa_function(
    #     l1_pelvic_angle = desired_l1pa,
    #     pelvic_incidence = pelvic_incidence,
    #     l1_l2 = start_values_list$l1_l2,
    #     l2_l3 = start_values_list$l2_l3,
    #     l3_l4 = start_values_list$l3_l4,
    #     l5_s1 = start_values_list$l5_s1
    #   )
    # 
    # 
    #   lordosis_needed_list$l5_s1 <- l5_s1_for_desired_l1pa_function(
    #     l1_pelvic_angle = desired_l1pa,
    #     pelvic_incidence = pelvic_incidence,
    #     l1_l2 = start_values_list$l1_l2,
    #     l2_l3 = start_values_list$l2_l3,
    #     l3_l4 = start_values_list$l3_l4,
    #     l4_l5 = start_values_list$l4_l5
    #   )
    # 
    #   identifying_pso_level_df <- confirming_segment_angles_df  %>%
    #     left_join(tibble(level = names(start_values_list), starting_value = start_values_list) %>%
    #                 unnest(starting_value)) %>%
    #     left_join(tibble(level = names(lordosis_needed_list), total_lordosis_needed = lordosis_needed_list) %>%
    #                 unnest(total_lordosis_needed)) %>%
    #     mutate(lordosis_needed = total_lordosis_needed - starting_value) %>%
    #     mutate(starting_value_normal_offset = case_when(
    #       starting_value < min ~ starting_value - min,
    #       starting_value > max ~ starting_value - max,
    #       TRUE ~ 0
    #     )) %>%
    #     mutate(starting_value_needed_offset = starting_value - lordosis_needed) %>%
    #     mutate(needed_value_normal_offset = case_when(
    #       # between(x = lordosis_needed, left = min, right = max) ~ 0,
    #       lordosis_needed < min ~ starting_value - min,
    #       lordosis_needed > max ~ starting_value - max,
    #       TRUE ~ 0
    #     )) %>%
    #     filter(level %in% non_modifiable)
    # 
    #   if(nrow(identifying_pso_level_df %>%
    #           filter(needed_value_normal_offset == 0))>0){
    # 
    #     pso_level_df <- identifying_pso_level_df %>%
    #       filter(needed_value_normal_offset == 0) %>%
    #       filter(lordosis_needed == min(lordosis_needed)) %>%
    #       mutate(pso_level = level) %>%
    #       separate(pso_level, into = c("pso_level", "pso_inferior"), sep = "_") %>%
    #       select(level, starting_value, total_lordosis_needed, pso_level)
    #   }else{
    #     pso_level_df <- identifying_pso_level_df %>%
    #       filter(needed_value_normal_offset == 0) %>%
    #       filter(needed_value_normal_offset == min(needed_value_normal_offset)) %>%
    #       mutate(pso_level = level) %>%
    #       separate(pso_level, into = c("pso_level", "pso_inferior"), sep = "_") %>%
    #       select(level, starting_value, total_lordosis_needed, pso_level)
    #   }
    # 
    # 
    #   pso_level <- pso_level_df$pso_level
    #   pso_value <- pso_level_df$total_lordosis_needed
    # 
    # 
    #   # Set the starting values
    #   start_values <- c(if_else(pso_level == "l1",
    #                             pso_value,
    #                             l1_l2_start),
    #                     if_else(pso_level == "l2",
    #                             pso_value,
    #                             l2_l3_start),
    #                     if_else(pso_level == "l3",
    #                             pso_value,
    #                             l3_l4_start),
    #                     if_else(pso_level == "l4",
    #                             pso_value,
    #                             l4_l5_start),
    #                     if_else(pso_level == "l5",
    #                             pso_value,
    #                             l5_s1_start))
    # 
    #   non_modifiable <- non_modifiable[!(non_modifiable %in% pso_level_df$level)]
    # 
    #   # Use the Nelder-Mead method to find the new values
    #   result <- optim(start_values, objective, method = "Nelder-Mead")
    # 
    #   # Extract the optimized values
    #   new_l1_l2 <- ifelse("l1_l2" %in% non_modifiable, l1_l2_start, result$par[1])
    #   new_l2_l3 <- ifelse("l2_l3" %in% non_modifiable, l2_l3_start, result$par[2])
    #   new_l3_l4 <- ifelse("l3_l4" %in% non_modifiable, l3_l4_start, result$par[3])
    #   new_l4_l5 <- ifelse("l4_l5" %in% non_modifiable, l4_l5_start, result$par[4])
    #   new_l5_s1 <- ifelse("l5_s1" %in% non_modifiable, l5_s1_start, result$par[5])
    # 
    #   segmental_lordosis_range_list <- compute_segmental_lordosis_range_by_pi_function(pelvic_incidence = pelvic_incidence)
    # 
    #   new_sa_values_df <- tibble(level = c("l5_s1", "l4_l5", "l3_l4", "l2_l3", "l1_l2"),
    #                              value = c(new_l5_s1, new_l4_l5, new_l3_l4, new_l2_l3, new_l1_l2))
    # 
    # 
    #   confirming_segment_angles_df <- enframe(segmental_lordosis_range_list) %>%
    #     unnest(value) %>%
    #     mutate(min_max = if_else(str_detect(name, "min"), "min", "max")) %>%
    #     mutate(level = str_remove_all(name, "min_|max_")) %>%
    #     select(level, min_max, value) %>%
    #     pivot_wider(names_from = min_max, values_from = value) %>%
    #     left_join(new_sa_values_df) %>%
    #     mutate(confirmed_value = case_when(
    #       value < min ~ min,
    #       value > max ~ max,
    #       TRUE ~ value
    #     )) %>%
    #     mutate(confirmed_value_limit = case_when(
    #       value < min ~ "out_of_range",
    #       value > max ~ "out_of_range",
    #       TRUE ~ "within_range"
    #     ))
    # 
    #   new_segment_angles_list <- as.list(confirming_segment_angles_df$confirmed_value)
    # 
    #   names(new_segment_angles_list) <- confirming_segment_angles_df$level
    # 
    #   new_l1pa <- estimate_l1pa_by_lumbar_segment_angles_function(l1_l2 = new_segment_angles_list$l1_l2,
    #                                                               l2_l3 = new_segment_angles_list$l2_l3,
    #                                                               l3_l4 = new_segment_angles_list$l3_l4,
    #                                                               l4_l5 = new_segment_angles_list$l4_l5,
    #                                                               l5_s1 = new_segment_angles_list$l5_s1,
    #                                                               pelvic_incidence = pelvic_incidence)
    # 
    #   results_list$l1_s1 <- new_segment_angles_list$l1_l2 + new_segment_angles_list$l2_l3 + new_segment_angles_list$l3_l4 + new_segment_angles_list$l4_l5 + new_segment_angles_list$l5_s1
    # 
    # }
    
    
    
    results_list$l1_l2 <- new_segment_angles_list$l1_l2
    results_list$l2_l3 <- new_segment_angles_list$l2_l3
    results_list$l3_l4 <- new_segment_angles_list$l3_l4
    results_list$l4_l5 <- new_segment_angles_list$l4_l5
    results_list$l5_s1 <- new_segment_angles_list$l5_s1
    results_list$estimated_l1pa <- round(new_l1pa, 1)
    results_list$needs_pso <- needs_pso
    results_list$pso_level <- pso_level
    
    # Return the new values
    return(results_list)
    
  }
  
