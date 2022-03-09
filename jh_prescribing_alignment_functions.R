jh_pso_degree_calculator <- function(pso_level, current_lpa, desired_lpa){
  pso_level <- str_to_lower(pso_level)
  
  # weight <- case_when(
  #   pso_level == "l5" ~ -0.56,
  #   pso_level == "l4" ~ -0.49,
  #   pso_level == "l3" ~ -0.32,
  #   pso_level == "l2" ~ -0.18,
  #   pso_level == "l1" ~ -0.05,
  # )
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

target_lpa_function <- function(pelvic_incidence = 51.813768) {-20.756157+0.48031254*pelvic_incidence}
compute_lpa_from_segment_levels_function <- function(pelvic_incidence = 51.813768,l1_l2 = 1.6817735,l2_l3 = 9.4508485,l3_l4 = 10.772589,l4_l5 = 16.66046,l5_s1 = 23.947641) {-0.8553283+0.60405938*pelvic_incidence-0.048137209*l1_l2-0.17727093*l2_l3-0.32237285*l3_l4-0.48541991*l4_l5-0.56294456*l5_s1 }

pt_function <- function(tpa = 6.3128833,t1_l1 = -40.878235,l1_s1 = 61.507721,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99) {1.73748+0.71970033*tpa-0.081113318*t1_l1-0.14596707*l1_s1+0.21338716*pelvic_incidence-0.03022352*l1_pelvic_angle }


recompute_tpa_estimation_function <- function(pelvic_incidence = 51.813768,l1_s1 = 61.507721,l1_pelvic_angle = 3.99,t1_t10 = -34.521921) {-1.6967976+0.57886789*pelvic_incidence-0.42474421*l1_s1+0.16316696*l1_pelvic_angle-0.10664331*t1_t10 }


final_recompute_tpa_estimation_function <- function(t1_l1 = -40.878235,l1_s1 = 61.507721,pelvic_incidence = 51.813768,l1_pelvic_angle = 3.99) {-3.3307018-0.22082989*t1_l1-0.52888684*l1_s1+0.62555837*pelvic_incidence+0.21351602*l1_pelvic_angle }


# target lpa function 
target_lpa_function <- function(pelvic_incidence = 51.813768) {-20.756157+0.48031254*pelvic_incidence}

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