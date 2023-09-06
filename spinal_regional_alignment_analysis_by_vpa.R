id_focal_deformity_function <- function(pelvic_incidence,
                                        l4_pelvic_angle, 
                                        l1_pelvic_angle, 
                                        t9_pelvic_angle, 
                                        t4_pelvic_angle, 
                                        c2_pelvic_angle){
  
  results_list <- list()
  interpretation_list <- list()
  
  uiv <- "Cannot Determine"
  
  ################## L4PA ####################
  expected_l4pa <- -10.741726+0.38137254* pelvic_incidence-0.0002368504*pmax(pelvic_incidence-34.900559,0)^3+0.0019373174*pmax(pelvic_incidence-46.078545,0)^3-0.0034453892*pmax(pelvic_incidence-51.75934,0)^3+0.0019796111*pmax(pelvic_incidence-57.450709,0)^3-0.00023468891*pmax(pelvic_incidence-69.886338,0)^3
  
  expected_l4pa_range <- c(expected_l4pa - 3.3, expected_l4pa + 3.3)
  
  lower_l4pa <- round(expected_l4pa_range[1], 0)
  upper_l4pa <- round(expected_l4pa_range[2], 0)
  
  if(between(l4_pelvic_angle, expected_l4pa_range[1], expected_l4pa_range[2])){
    results_list$l4pa <- glue("L4-S1 within normal range for\nPI of {pelvic_incidence}.")
    interpretation_list$l4pa <- "normal"
  }else{
    if(l4_pelvic_angle < expected_l4pa_range[1]){
      results_list$l4pa <- glue("L4-S1 hyperlordotic deformity.\nL4PA = {round(l4_pelvic_angle, 0)} vs Expected L4PA = {lower_l4pa} to {upper_l4pa}")
      interpretation_list$l4pa <- "low"
      
      uiv <- "Lower Lumbar"
    }else{
      results_list$l4pa <- glue("L4-S1 flatback deformity.\nL4PA = {round(l4_pelvic_angle, 0)} vs Expected L4PA = {lower_l4pa} to {upper_l4pa}")
      interpretation_list$l4pa <- "high"
      
      uiv <- "Lower Lumbar"
    }
  }
  
  ################## L1PA ####################
  expected_l1pa <- -2.0150417-0.1716774* pelvic_incidence+0.00013229696*pmax(pelvic_incidence-34.900559,0)^3-0.0011915712*pmax(pelvic_incidence-46.078545,0)^3+0.0027171795*pmax(pelvic_incidence-51.75934,0)^3-0.0020516967*pmax(pelvic_incidence-57.450709,0)^3+0.00039379152*pmax(pelvic_incidence-69.886338,0)^3+1.7587452* l4_pelvic_angle-0.0023651719*pmax(l4_pelvic_angle-1.5671738,0)^3+0.017497084*pmax(l4_pelvic_angle-6.0087935,0)^3-0.031157773*pmax(l4_pelvic_angle-8.274715,0)^3+0.019063624*pmax(l4_pelvic_angle-10.777879,0)^3-0.0030377619*pmax(l4_pelvic_angle-16.154477,0)^3
  
  expected_l1pa_range <- c(expected_l1pa - 3.15, expected_l1pa + 3.15)
  
  lower_l1pa <- round(expected_l1pa_range[1], 0)
  upper_l1pa <- round(expected_l1pa_range[2], 0)
  
  if(between(l1_pelvic_angle, expected_l1pa_range[1], expected_l1pa_range[2])){
    results_list$l1pa <- glue("L1-L4 within normal range for\nPI of {pelvic_incidence} and L4PA of {round(l4_pelvic_angle, 0)}.")
    interpretation_list$l1pa <- "normal"
  }else{
    if(l1_pelvic_angle < expected_l1pa_range[1]){
      results_list$l1pa <- glue("L1-L4 hyperlordotic deformity.\nL1PA = {round(l1_pelvic_angle, 0)} vs Expected L1PA = {lower_l1pa} to {upper_l1pa}")
      interpretation_list$l1pa <- "low"
    }else{
      results_list$l1pa <- glue("L1-L4 flatback deformity.\nL1PA = {round(l1_pelvic_angle, 0)} vs Expected L1PA = {lower_l1pa} to {upper_l1pa}")
      interpretation_list$l1pa <- "high"
      uiv <- "Upper Lumbar"
    }
  }
  
  ################## T9PA ####################
  expected_t9pa <- 5.1190568-0.075935912* pelvic_incidence+0.00051580538*pmax(pelvic_incidence-34.900559,0)^3-0.0026988729*pmax(pelvic_incidence-46.078545,0)^3+0.003526495*pmax(pelvic_incidence-51.75934,0)^3-0.0014246497*pmax(pelvic_incidence-57.450709,0)^3+8.1222213e-05*pmax(pelvic_incidence-69.886338,0)^3-0.75464023* l4_pelvic_angle-0.0057977251*pmax(l4_pelvic_angle-1.5671738,0)^3+0.049669918*pmax(l4_pelvic_angle-6.0087935,0)^3-0.083368289*pmax(l4_pelvic_angle-8.274715,0)^3+0.044184106*pmax(l4_pelvic_angle-10.777879,0)^3-0.0046880092*pmax(l4_pelvic_angle-16.154477,0)^3+1.5756467* l1_pelvic_angle-0.0010569573*pmax(l1_pelvic_angle+5.9350206,0)^3+5.0647269e-05*pmax(l1_pelvic_angle-0.068995368,0)^3+0.0057803894*pmax(l1_pelvic_angle-3.8566573,0)^3-0.0054703576*pmax(l1_pelvic_angle-7.2427704,0)^3+0.00069627834*pmax(l1_pelvic_angle-15.871535,0)^3
  
  expected_t9pa_range <- c(expected_t9pa - 3.45, expected_t9pa + 3.45)
  
  lower_t9pa <- round(expected_t9pa_range[1], 0)
  upper_t9pa <- round(expected_t9pa_range[2], 0)
  
  if(between(t9_pelvic_angle, expected_t9pa_range[1], expected_t9pa_range[2])){
    results_list$t9pa <- glue("Thoracolumbar junction within normal range for\nlumbar alignment and PI of {pelvic_incidence}.")
    interpretation_list$t9pa <- "normal"
  }else{
    if(t9_pelvic_angle < expected_t9pa_range[1]){
      if(interpretation_list$l1pa == "high"){
        results_list$t9pa <- glue("Compensatory hyperlordotic thoracolumbar junction.\nT9PA = {round(t9_pelvic_angle, 0)} vs Expected T9PA = {lower_t9pa} to {upper_t9pa}")
      }else{
        results_list$t9pa <- glue("Hyperlordotic thoracolumbar junction.\nT9PA = {round(t9_pelvic_angle, 0)} vs Expected T9PA = {lower_t9pa} to {upper_t9pa}")
      }
      interpretation_list$t9pa <- "low"
      uiv <- "Upper Lumbar"
    }else{
      results_list$t9pa <- glue("Kyphotic thoracolumbar deformity.\nT9PA = {round(t9_pelvic_angle, 0)} vs Expected T9PA = {lower_t9pa} to {upper_t9pa}")
      interpretation_list$t9pa <- "high"
      uiv <- "Lower Thoracic"
    }
  }
  
  ################## T4PA ####################
  expected_t4pa <- 2.7595219-0.0094370888* pelvic_incidence+0.00033258412*pmax(pelvic_incidence-34.900559,0)^3-0.0022159413*pmax(pelvic_incidence-46.078545,0)^3+0.0033802704*pmax(pelvic_incidence-51.75934,0)^3-0.0016206014*pmax(pelvic_incidence-57.450709,0)^3+0.00012368827*pmax(pelvic_incidence-69.886338,0)^3+0.27384103* l4_pelvic_angle-0.0069663906*pmax(l4_pelvic_angle-1.5671738,0)^3+0.047395743*pmax(l4_pelvic_angle-6.0087935,0)^3-0.06839712*pmax(l4_pelvic_angle-8.274715,0)^3+0.029704966*pmax(l4_pelvic_angle-10.777879,0)^3-0.0017371987*pmax(l4_pelvic_angle-16.154477,0)^3-0.51785714* l1_pelvic_angle+0.0007798906*pmax(l1_pelvic_angle+5.9350206,0)^3-0.0056811305*pmax(l1_pelvic_angle-0.068995368,0)^3+0.010912147*pmax(l1_pelvic_angle-3.8566573,0)^3-0.0067609393*pmax(l1_pelvic_angle-7.2427704,0)^3+0.00075003187*pmax(l1_pelvic_angle-15.871535,0)^3+1.3939898* t9_pelvic_angle-0.00060480117*pmax(t9_pelvic_angle+9.6726719,0)^3+0.0046037538*pmax(t9_pelvic_angle+2.6396905,0)^3-0.0092450297*pmax(t9_pelvic_angle-0.8007183,0)^3+0.0064088465*pmax(t9_pelvic_angle-4.3948571,0)^3-0.0011627694*pmax(t9_pelvic_angle-12.436566,0)^3
  
  expected_t4pa_range <- c(expected_t4pa - 3.15, expected_t4pa + 3.15)
  
  lower_t4pa <- round(expected_t4pa_range[1], 0)
  upper_t4pa <- round(expected_t4pa_range[2], 0)
  
  if(between(t4_pelvic_angle, expected_t4pa_range[1], expected_t4pa_range[2])){
    results_list$t4pa <- glue("Thoracic alignment within normal range for\nthoracolumbar alignment and PI of {pelvic_incidence}.")
    interpretation_list$t4pa <- "normal"
  }else{
    if(t4_pelvic_angle < expected_t4pa_range[1]){
      if(interpretation_list$l1pa == "high" | interpretation_list$t9pa == "high"){
        results_list$t4pa <- glue("Suspect compensatory hypokyphotic thoracic spine.\nT4PA = {round(t4_pelvic_angle, 0)} vs Expected T4PA = {lower_t4pa} to {upper_t4pa}")
      }else{
        results_list$t4pa <- glue("Hypokyphotic thoracic spine.\nT4PA = {round(t4_pelvic_angle, 0)} vs Expected T4PA = {lower_t4pa} to {upper_t4pa}")
      }
    }else{
      results_list$t4pa <- glue("Kyphotic thoracic alignment.\nT4PA = {round(t4_pelvic_angle, 0)} vs Expected T4PA = {lower_t4pa} to {upper_t4pa}")
      uiv <- "Upper Thoracic"
    }
  }
  
  if(c2_pelvic_angle- t9_pelvic_angle > 12){
    uiv <- "Upper Thoracic"
  }
  
  
  return_list <- list()
  
  return_list$upper_thoracic_analyzed <- results_list$t4pa
  
  return_list$tl_junction_analyzed <- results_list$t9pa
  
  return_list$upper_lumbar_analyzed <- results_list$l1pa
  
  return_list$lower_lumbar_analyzed <- results_list$l4pa
  
  return_list$recommended_uiv <- uiv
  
  #UIV OPTIONS: 
  # Lower Lumbar
  # Upper Lumbar
  # Lower Thoracic
  # Upper Thoracic
  # Cannot Determine
  
  return(return_list)
  
}




