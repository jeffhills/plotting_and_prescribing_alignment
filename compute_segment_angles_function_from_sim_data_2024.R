compute_segment_angles_list_function <- function(pelvic_incidence = 50,
                                                 l1_s1 = 199,
                                                 t10_l2 = 199,
                                                 c2_c7 = 15,
                                                 l1pa = 10,
                                                 t9pa = 12,
                                                 t4pa = 10,
                                                 c2pa = 14){
  
  segment_angle_list <- list()
  if(l1_s1 == 199){
    segment_angle_list$l5_segment_angle = -2.95268 + 0.61976*pelvic_incidence + -1.78216*l1pa + 0.86139*t9pa + -0.23088*t4pa + 0.00902*c2pa
    segment_angle_list$l4_segment_angle = -0.99992 + 0.4322*pelvic_incidence + -0.83441*l1pa + 0.12699*t9pa + -0.03442*t4pa + 0.00188*c2pa
    segment_angle_list$l3_segment_angle = 1.01174 + 0.23977*pelvic_incidence + 0.13618*l1pa + -0.62322*t9pa + 0.1653*t4pa + -0.00527*c2pa
    segment_angle_list$l2_segment_angle = 3.06498 + 0.04565*pelvic_incidence + 1.11029*l1pa + -1.36978*t9pa + 0.36035*t4pa + -0.01163*c2pa
    segment_angle_list$l1_segment_angle = 5.1352 + -0.15965*pelvic_incidence + 2.16163*l1pa + -2.20166*t9pa + 0.59184*t4pa + -0.02092*c2pa
    segment_angle_list$t12_segment_angle = -2.18821 + -0.17619*pelvic_incidence + 1.19855*l1pa + -0.61893*t9pa + -0.09767*t4pa + -0.02077*c2pa
    segment_angle_list$t11_segment_angle = -1.21961 + -0.11549*pelvic_incidence + 0.55813*l1pa + 0.27486*t9pa + -0.54468*t4pa + 0.01861*c2pa
    segment_angle_list$t10_segment_angle = -0.2786 + -0.05642*pelvic_incidence + -0.06477*l1pa + 1.14371*t9pa + -0.97896*t4pa + 0.05682*c2pa
    segment_angle_list$t9_segment_angle = 0.90929 + 0.01205*pelvic_incidence + -0.80648*l1pa + 2.20935*t9pa + -1.52994*t4pa + 0.10985*c2pa
    segment_angle_list$t8_segment_angle = 0.75236 + 0.02528*pelvic_incidence + -0.87125*l1pa + 2.1823*t9pa + -1.44734*t4pa + 0.08556*c2pa
    segment_angle_list$t7_segment_angle = -0.86928 + 0.06245*pelvic_incidence + -0.8133*l1pa + 1.39597*t9pa + -0.64304*t4pa + -0.08429*c2pa
    segment_angle_list$t6_segment_angle = -1.62965 + 0.05062*pelvic_incidence + -0.57232*l1pa + 0.87716*t9pa + -0.27599*t4pa + -0.14326*c2pa
    segment_angle_list$t5_segment_angle = -2.34639 + 0.03947*pelvic_incidence + -0.34515*l1pa + 0.38812*t9pa + 0.06996*t4pa + -0.19882*c2pa
    segment_angle_list$t4_segment_angle = -5.16367 + 0.08443*pelvic_incidence + 0.26966*l1pa + -1.8813*t9pa + 2.41618*t4pa + -0.95558*c2pa
    segment_angle_list$t3_segment_angle = -5.73766 + 0.06969*pelvic_incidence + 0.49302*l1pa + -2.27877*t9pa + 2.59858*t4pa + -0.92875*c2pa
    segment_angle_list$t2_segment_angle = 2.85893 + -0.07629*pelvic_incidence + 0.14398*l1pa + -0.47223*t9pa + 1.42411*t4pa + -0.94559*c2pa
    segment_angle_list$t1_segment_angle = 3.14975 + -0.07721*pelvic_incidence + 0.09065*l1pa + -0.27925*t9pa + 1.19501*t4pa + -0.85617*c2pa
    segment_angle_list$c7_segment_angle = 3.41758 + -0.07805*pelvic_incidence + 0.04153*l1pa + -0.10152*t9pa + 0.98402*t4pa + -0.77382*c2pa
    segment_angle_list$c6_segment_angle = 3.75373 + -0.06134*pelvic_incidence + 0.05505*l1pa + -0.10837*t9pa + 0.74694*t4pa + -0.57488*c2pa
    segment_angle_list$c5_segment_angle = 4.48284 + -0.00247*pelvic_incidence + 0.0775*l1pa + -0.29012*t9pa + 0.401*t4pa + -0.18107*c2pa
    segment_angle_list$c4_segment_angle = 4.57797 + -0.00277*pelvic_incidence + 0.06005*l1pa + -0.22699*t9pa + 0.32606*t4pa + -0.15182*c2pa
    segment_angle_list$c3_segment_angle = 4.673 + -0.00307*pelvic_incidence + 0.04262*l1pa + -0.16393*t9pa + 0.2512*t4pa + -0.1226*c2pa
    segment_angle_list$c2_segment_angle = 4.77988 + -0.00341*pelvic_incidence + 0.02302*l1pa + -0.09301*t9pa + 0.16701*t4pa + -0.08974*c2pa
    segment_angle_list$c1_segment_angle = 4.86235 + -0.00367*pelvic_incidence + 0.0079*l1pa + -0.03829*t9pa + 0.10204*t4pa + -0.06438*c2pa
  }else if(t10_l2 == 199){
    segment_angle_list$l5_segment_angle = -1.48364 + 0.94834*pelvic_incidence + -1.5613*l1pa + -0.0332*t9pa + 0.00702*t4pa + 0.00143*c2pa + -0.27899*l1_s1 + -8e-05*c2_c7
    segment_angle_list$l4_segment_angle = -0.75842 + 0.486*pelvic_incidence + -0.79823*l1pa + -0.01954*t9pa + 0.00462*t4pa + 0.00058*c2pa + -0.04568*l1_s1 + -6e-05*c2_c7
    segment_angle_list$l3_segment_angle = -0.01634 + 0.00949*pelvic_incidence + -0.01858*l1pa + 0.00367*t9pa + -0.0013*t4pa + -2e-05*c2pa + 0.19552*l1_s1 + -1e-05*c2_c7
    segment_angle_list$l2_segment_angle = 0.73082 + -0.47645*pelvic_incidence + 0.75935*l1pa + 0.05174*t9pa + -0.01767*t4pa + 0.00043*c2pa + 0.44332*l1_s1 + 0.00012*c2_c7
    segment_angle_list$l1_segment_angle = 1.52758 + -0.96737*pelvic_incidence + 1.61877*l1pa + -0.00267*t9pa + 0.00733*t4pa + -0.00243*c2pa + 0.68583*l1_s1 + 3e-05*c2_c7
    segment_angle_list$t12_segment_angle = 1.68766 + 0.67669*pelvic_incidence + 1.77267*l1pa + -2.94404*t9pa + 0.52525*t4pa + -0.0436*c2pa + -0.72436*l1_s1 + -0.00298*c2_c7
    segment_angle_list$t11_segment_angle = 1.28652 + 0.44771*pelvic_incidence + 0.93653*l1pa + -1.25797*t9pa + -0.13792*t4pa + 0.00618*c2pa + -0.47818*l1_s1 + 0.00039*c2_c7
    segment_angle_list$t10_segment_angle = 0.89468 + 0.22488*pelvic_incidence + 0.12315*l1pa + 0.38188*t9pa + -0.78256*t4pa + 0.05451*c2pa + -0.23862*l1_s1 + 0.00367*c2_c7
    segment_angle_list$t9_segment_angle = 0.51967 + -0.03434*pelvic_incidence + -0.84014*l1pa + 2.34438*t9pa + -1.57921*t4pa + 0.11997*c2pa + 0.03989*l1_s1 + 0.00808*c2_c7
    segment_angle_list$t8_segment_angle = 0.13237 + -0.08061*pelvic_incidence + -0.94441*l1pa + 2.47761*t9pa + -1.53663*t4pa + 0.09529*c2pa + 0.09031*l1_s1 + 0.00651*c2_c7
    segment_angle_list$t7_segment_angle = -1.93275 + -0.19947*pelvic_incidence + -0.98788*l1pa + 2.10393*t9pa + -0.82342*t4pa + -0.08357*c2pa + 0.2221*l1_s1 + -0.0047*c2_c7
    segment_angle_list$t6_segment_angle = -2.31945 + -0.14916*pelvic_incidence + -0.70384*l1pa + 1.41136*t9pa + -0.40313*t4pa + -0.14874*c2pa + 0.16908*l1_s1 + -0.00896*c2_c7
    segment_angle_list$t5_segment_angle = -2.68402 + -0.10174*pelvic_incidence + -0.43607*l1pa + 0.75853*t9pa + -0.007*t4pa + -0.21014*c2pa + 0.1191*l1_s1 + -0.01297*c2_c7
    segment_angle_list$t4_segment_angle = -3.2761 + -0.07359*pelvic_incidence + 0.19873*l1pa + -1.57521*t9pa + 2.52508*t4pa + -1.08072*c2pa + 0.12706*l1_s1 + -0.11478*c2_c7
    segment_angle_list$t3_segment_angle = -3.71022 + -0.02385*pelvic_incidence + 0.4634*l1pa + -2.14111*t9pa + 2.7414*t4pa + -1.04801*c2pa + 0.07272*l1_s1 + -0.10822*c2_c7
    segment_angle_list$t2_segment_angle = 5.19681 + 0.17139*pelvic_incidence + 0.3272*l1pa + -1.20547*t9pa + 1.70938*t4pa + -1.01241*c2pa + -0.21369*l1_s1 + -0.05452*c2_c7
    segment_angle_list$t1_segment_angle = 5.21147 + 0.16318*pelvic_incidence + 0.26565*l1pa + -0.98097*t9pa + 1.45403*t4pa + -0.91073*c2pa + -0.20682*l1_s1 + -0.04374*c2_c7
    segment_angle_list$c7_segment_angle = 5.22496 + 0.15562*pelvic_incidence + 0.20897*l1pa + -0.77422*t9pa + 1.21887*t4pa + -0.81709*c2pa + -0.2005*l1_s1 + -0.03381*c2_c7
    segment_angle_list$c6_segment_angle = -5.68609 + 0.04385*pelvic_incidence + -0.00906*l1pa + 0.07953*t9pa + -0.03005*t4pa + -0.08527*c2pa + -0.06211*l1_s1 + 0.4386*c2_c7
    segment_angle_list$c5_segment_angle = 1.41419 + -0.00685*pelvic_incidence + 0.03308*l1pa + -0.13226*t9pa + 0.13534*t4pa + -0.02958*c2pa + 0.01209*l1_s1 + 0.13495*c2_c7
    segment_angle_list$c4_segment_angle = 1.41898 + -0.00954*pelvic_incidence + 0.01294*l1pa + -0.05883*t9pa + 0.05181*t4pa + 0.00368*c2pa + 0.01434*l1_s1 + 0.13848*c2_c7
    segment_angle_list$c3_segment_angle = 1.42377 + -0.01222*pelvic_incidence + -0.00717*l1pa + 0.01453*t9pa + -0.03163*t4pa + 0.0369*c2pa + 0.01658*l1_s1 + 0.142*c2_c7
    segment_angle_list$c2_segment_angle = 1.42915 + -0.01524*pelvic_incidence + -0.02979*l1pa + 0.09703*t9pa + -0.12547*t4pa + 0.07427*c2pa + 0.0191*l1_s1 + 0.14596*c2_c7
    segment_angle_list$c1_segment_angle = 4.91431 + 0.00822*pelvic_incidence + 0.01587*l1pa + -0.0706*t9pa + 0.11055*t4pa + -0.0646*c2pa + -0.01009*l1_s1 + 5e-05*c2_c7
  }else{
    segment_angle_list$l5_segment_angle = -3.13581 + 0.83141*pelvic_incidence + -2.92405*l1pa + 1.13718*t9pa + 0.12578*t4pa + -0.00306*c2pa + -0.04774*l1_s1 + 0.30616*t10_l2 + -0.00042*c2_c7
    segment_angle_list$l4_segment_angle = -0.7608 + 0.48583*pelvic_incidence + -0.80019*l1pa + -0.01785*t9pa + 0.00479*t4pa + 0.00058*c2pa + -0.04535*l1_s1 + 0.00044*t10_l2 + -6e-05*c2_c7
    segment_angle_list$l3_segment_angle = 1.66601 + 0.12855*pelvic_incidence + 1.36906*l1pa + -1.18809*t9pa + -0.12223*t4pa + 0.00455*c2pa + -0.03995*l1_s1 + -0.31175*t10_l2 + 0.00034*c2_c7
    segment_angle_list$l2_segment_angle = 4.09699 + -0.23823*pelvic_incidence + 3.53585*l1pa + -2.33283*t9pa + -0.25963*t4pa + 0.00958*c2pa + -0.02783*l1_s1 + -0.62377*t10_l2 + 0.00081*c2_c7
    segment_angle_list$l1_segment_angle = -1.86638 + -1.20756*pelvic_incidence + -1.18066*l1pa + 2.40159*t9pa + 0.25129*t4pa + -0.01164*c2pa + 1.16087*l1_s1 + 0.62893*t10_l2 + -0.00067*c2_c7
    segment_angle_list$t12_segment_angle = 2.73194 + 0.7506*pelvic_incidence + 2.63402*l1pa + -3.6838*t9pa + 0.45019*t4pa + -0.04077*c2pa + -0.87052*l1_s1 + -0.19351*t10_l2 + -0.00276*c2_c7
    segment_angle_list$t11_segment_angle = 0.60404 + 0.39941*pelvic_incidence + 0.37361*l1pa + -0.77451*t9pa + -0.08887*t4pa + 0.00432*c2pa + -0.38266*l1_s1 + 0.12647*t10_l2 + 0.00025*c2_c7
    segment_angle_list$t10_segment_angle = -1.4696 + 0.05756*pelvic_incidence + -1.82697*l1pa + 2.05672*t9pa + -0.61261*t4pa + 0.04809*c2pa + 0.09231*l1_s1 + 0.43812*t10_l2 + 0.00318*c2_c7
    segment_angle_list$t9_segment_angle = 1.53631 + 0.0376*pelvic_incidence + -0.00159*l1pa + 1.6242*t9pa + -1.65229*t4pa + 0.12273*c2pa + -0.1024*l1_s1 + -0.18839*t10_l2 + 0.00829*c2_c7
    segment_angle_list$t8_segment_angle = 0.48846 + -0.05541*pelvic_incidence + -0.65069*l1pa + 2.22535*t9pa + -1.56222*t4pa + 0.09626*c2pa + 0.04047*l1_s1 + -0.06599*t10_l2 + 0.00659*c2_c7
    segment_angle_list$t7_segment_angle = -1.52228 + -0.17042*pelvic_incidence + -0.64931*l1pa + 1.81315*t9pa + -0.85292*t4pa + -0.08246*c2pa + 0.16464*l1_s1 + -0.07606*t10_l2 + -0.00461*c2_c7
    segment_angle_list$t6_segment_angle = -2.01306 + -0.12748*pelvic_incidence + -0.45111*l1pa + 1.19431*t9pa + -0.42515*t4pa + -0.14791*c2pa + 0.12619*l1_s1 + -0.05678*t10_l2 + -0.00889*c2_c7
    segment_angle_list$t5_segment_angle = -2.47574 + -0.087*pelvic_incidence + -0.26428*l1pa + 0.61098*t9pa + -0.02197*t4pa + -0.20957*c2pa + 0.08994*l1_s1 + -0.0386*t10_l2 + -0.01292*c2_c7
    segment_angle_list$t4_segment_angle = -3.17302 + -0.0663*pelvic_incidence + 0.28376*l1pa + -1.64823*t9pa + 2.51767*t4pa + -1.08044*c2pa + 0.11263*l1_s1 + -0.0191*t10_l2 + -0.11476*c2_c7
    segment_angle_list$t3_segment_angle = -3.71637 + -0.02429*pelvic_incidence + 0.45833*l1pa + -2.13675*t9pa + 2.74184*t4pa + -1.04803*c2pa + 0.07358*l1_s1 + 0.00114*t10_l2 + -0.10823*c2_c7
    segment_angle_list$t2_segment_angle = 5.12583 + 0.16637*pelvic_incidence + 0.26865*l1pa + -1.15519*t9pa + 1.71448*t4pa + -1.0126*c2pa + -0.20375*l1_s1 + 0.01315*t10_l2 + -0.05454*c2_c7
    segment_angle_list$t1_segment_angle = 5.15154 + 0.15894*pelvic_incidence + 0.21623*l1pa + -0.93853*t9pa + 1.45834*t4pa + -0.91089*c2pa + -0.19843*l1_s1 + 0.0111*t10_l2 + -0.04375*c2_c7
    segment_angle_list$c7_segment_angle = 5.17523 + 0.1521*pelvic_incidence + 0.16795*l1pa + -0.73899*t9pa + 1.22244*t4pa + -0.81722*c2pa + -0.19354*l1_s1 + 0.00922*t10_l2 + -0.03382*c2_c7
    segment_angle_list$c6_segment_angle = -5.69638 + 0.04312*pelvic_incidence + -0.01754*l1pa + 0.08681*t9pa + -0.02931*t4pa + -0.0853*c2pa + -0.06067*l1_s1 + 0.00191*t10_l2 + 0.4386*c2_c7
    segment_angle_list$c5_segment_angle = 1.41122 + -0.00706*pelvic_incidence + 0.03063*l1pa + -0.13016*t9pa + 0.13555*t4pa + -0.02959*c2pa + 0.01251*l1_s1 + 0.00055*t10_l2 + 0.13495*c2_c7
    segment_angle_list$c4_segment_angle = 1.41963 + -0.00949*pelvic_incidence + 0.01348*l1pa + -0.05929*t9pa + 0.05176*t4pa + 0.00368*c2pa + 0.01424*l1_s1 + -0.00012*t10_l2 + 0.13848*c2_c7
    segment_angle_list$c3_segment_angle = 1.42804 + -0.01192*pelvic_incidence + -0.00365*l1pa + 0.01151*t9pa + -0.03194*t4pa + 0.03692*c2pa + 0.01598*l1_s1 + -0.00079*t10_l2 + 0.142*c2_c7
    segment_angle_list$c2_segment_angle = 1.43749 + -0.01465*pelvic_incidence + -0.02291*l1pa + 0.09113*t9pa + -0.12606*t4pa + 0.07429*c2pa + 0.01793*l1_s1 + -0.00154*t10_l2 + 0.14597*c2_c7
    segment_angle_list$c1_segment_angle = 4.9127 + 0.00811*pelvic_incidence + 0.01454*l1pa + -0.06946*t9pa + 0.11066*t4pa + -0.0646*c2pa + -0.00987*l1_s1 + 3e-04*t10_l2 + 5e-05*c2_c7
  }
  
  return(segment_angle_list)
}

############################### PSO FUNCTION #################################
compute_pso_segment_angles_for_upper_t_uiv_function <- function(pelvic_incidence, l5_segment_angle, l4_segment_angle, l3_segment_angle, l2_segment_angle, l1_segment_angle) {
  # Define a vector of initial angles
  segment_angles <- c(l5_segment_angle, l4_segment_angle, l3_segment_angle, l2_segment_angle, l1_segment_angle)
  segment_names <- c("L5", "L4", "L3", "L2", "L1")
  
  starting_l1pa <- -0.95701027+0.5964649*pelvic_incidence-0.54476989*l5_segment_angle-0.42489229*l4_segment_angle-0.30168395*l3_segment_angle-0.17685974*l2_segment_angle-0.047199579*l1_segment_angle 
  
  # Function to compute L1PA
  compute_L1PA <- function(pelvic_incidence, angles) {
    -0.95701027 + 0.5964649 * pelvic_incidence - sum(c(0.54476989, 0.42489229, 0.30168395, 0.17685974, 0.047199579) * angles)
  }
  
  # Function to compute T4PA
  compute_T4PA <- function(l1pa_value, l1_s1, pelvic_incidence) {
    6.8194357 + 0.51959288 * l1pa_value - 0.51659266 * l1_s1 + 0.48005429 * pelvic_incidence
  }
  
  # Initialize best adjustment tracking
  best_adjustment <- list(Score = Inf)
  
  adjustments_list <- list()
  
  # Test adjustments for each segment
  for (i in 1:5) {
    for (adjustment in seq(-5, 50, by = 1)) {  # Fine-tuned adjustments
      # Adjust the segment angle
      temp_angles <- segment_angles
      temp_angles[i] <- segment_angles[i] + adjustment
      
      # Recompute L1PA and l1_s1
      new_L1PA <- compute_L1PA(pelvic_incidence, temp_angles)
      new_l1_s1 <- sum(temp_angles)
      new_T4PA <- compute_T4PA(new_L1PA, new_l1_s1, pelvic_incidence)
      
      # Check against the mandatory goal
      t4pa_l1pa_diff <- new_T4PA - new_L1PA
      if (t4pa_l1pa_diff >= -3 && t4pa_l1pa_diff <= 2) {
        # Check against the other goals with tolerance
        goal2_diff <- abs(new_L1PA - (pelvic_incidence * 0.5 - 19))
        goal3_diff <- abs(new_l1_s1 - (pelvic_incidence * 1.4 - 1.7 * new_L1PA - 2))
        error <- goal2_diff + goal3_diff  # Combine error from less strict goals
        
        adjustments_list[[length(adjustments_list) + 1]] <- list(Segment = segment_names[i], Adjustment = adjustment, new_L1PA = new_L1PA, new_T4PA = new_T4PA, new_l1_s1 = new_l1_s1, error = error) 
        
        
        # Update the best adjustment if this score is better
        # if (score < best_adjustment$Score) {
        #   best_adjustment <- list(Segment = segment_names[i], Adjustment = adjustment, new_L1PA = new_L1PA, new_T4PA = new_T4PA, new_l1_s1 = new_l1_s1, Score = score)
        # }
      }
    }
  }
  
  # return(best_adjustment)
  # Convert the list to a tibble, sort by score, and select the top two results
  results_tibble <- bind_rows(adjustments_list) %>%
    clean_names() %>%
    arrange(error) %>%
    mutate(initial_l1pa = starting_l1pa) %>%
    mutate(l1pa_change = new_l1pa - initial_l1pa) %>%
    select(segment, adjustment, initial_l1pa, new_l1pa, l1pa_change, everything()) %>%
    group_by(segment) %>%
    filter(row_number() == 1) %>%
    ungroup()
  # slice_head(n = 2)
  
  return(results_tibble)
}
