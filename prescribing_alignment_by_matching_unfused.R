### predict recipricol changes
predict_postop_c2_c7_function <- function(rad_pre_c2_c7 = 10.95, final_t4pa_goal = 10, starting_t4pa = 20) {
  rad_6w_t4pa_change <- final_t4pa_goal - starting_t4pa
  
  round(5.33485+0.7683201*rad_pre_c2_c7+0.27750464*rad_6w_t4pa_change, 2)
  
}

c2_segment_weight = 0.3
c3_segment_weight = 0.2
c4_segment_weight = 0
c5_segment_weight = 0
c6_segment_weight = 0.2
c7_segment_weight = 0.3


### required functions for estimating vpas and segment angles:
compute_c2_t1_function <- function(segment_angles_list){
  sum(segment_angles_list$c2_segment_angle, 
      segment_angles_list$c3_segment_angle,
      segment_angles_list$c4_segment_angle, 
      segment_angles_list$c5_segment_angle, 
      segment_angles_list$c6_segment_angle, 
      segment_angles_list$c7_segment_angle
  )
}


compute_t1_t10_function <- function(segment_angles_list){
  sum(segment_angles_list$t1_segment_angle, 
      segment_angles_list$t2_segment_angle,
      segment_angles_list$t3_segment_angle, 
      segment_angles_list$t4_segment_angle, 
      segment_angles_list$t5_segment_angle, 
      segment_angles_list$t6_segment_angle,
      segment_angles_list$t7_segment_angle, 
      segment_angles_list$t8_segment_angle, 
      segment_angles_list$t9_segment_angle
  )
}

prescribe_t10_l2_function <- function(pelvic_incidence = 51.66,
                                      c2_t1 = 18.93865,
                                      t1_t10 = -34.53485) {
  -21.689486 + 0.25868468*pelvic_incidence - 0.25117713*c2_t1 - 0.36508389*t1_t10 
  # t10_l2 = l1_sa + t12_sa +t11_sa +t10_sa
  # c2_t1 = c2_sa + c3_sa + c4_sa + c5_sa + c6_sa + c7_sa
  # t1_t10 = t1_sa + t2_sa + t3_sa + t4_sa + t5_sa + t6_sa + t7_sa + t8_sa + t9_sa 
}


prescribe_l1pa_function <- function(pelvic_incidence = 51.66,
                                    c2_t1 = 18.93865,
                                    t1_t10 = -34.53485,
                                    t10_l2 = -0.17825){
  -12.314247+0.40522994*pelvic_incidence+0.083763022*c2_t1+0.17177363*t1_t10+0.34250299*t10_l2 
  # t10_l2 = l1_sa + t12_sa +t11_sa +t10_sa
  # c2_t1 = c2_sa + c3_sa + c4_sa + c5_sa + c6_sa + c7_sa
  # t1_t10 = t1_sa + t2_sa + t3_sa + t4_sa + t5_sa + t6_sa + t7_sa + t8_sa + t9_sa 
}



l1pa_computed_by_segment_angles_function <- function(pelvic_incidence, l5_s1, l4_l5, l3_l4, l2_l3) {
  return(-2.7031182 + 0.61410488 * pelvic_incidence - 0.57421502 * l5_s1 - 0.49327403 * l4_l5 - 0.27054146 * l3_l4 - 0.21642196 * l2_l3)
}

# Define the function to compute the normal range for l2_s1
prescribe_l2_s1_range_function <- function(pelvic_incidence, l1_pelvic_angle) {
  l2_s1 <- -4.545335 + 1.3067339 * pelvic_incidence - 1.868834 * l1_pelvic_angle
  return(c(l2_s1 - 2, l2_s1 + 2))
}

prescribe_l5_s1_function <- function(l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l2_s1 = 55.36) {-2.3599362-2.1166053*l1_pelvic_angle+1.2350614*pelvic_incidence-0.50776405*l2_s1 }

prescribe_l4_l5_function <- function(l1_pelvic_angle = 3.86,
                                     pelvic_incidence = 51.66,
                                     l2_s1 = 55.36,
                                     l5_s1 = 24.885) {
  -3.9222722-1.5907198*l1_pelvic_angle+0.95679068*pelvic_incidence-0.1492548*l2_s1-0.73183754*l5_s1 
}

prescribe_l3_l4_function <- function(l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l2_s1 = 55.36,l4_s1 = 37.77) {
  1.541835-0.14606462*l1_pelvic_angle+0.074639566*pelvic_incidence+0.46829948*l2_s1-0.5173523*l4_s1 
}

prescribe_l2_l3_function <- function(l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l3_s1 = 49.12) {-5.5651491-0.73526583*l1_pelvic_angle+0.5989486*pelvic_incidence-0.30334265* l3_s1-0.00081186076*pmax(l3_s1-35.5825,0)^3+0.0069772375*pmax(l3_s1-44.759,0)^3-0.01340758*pmax(l3_s1-49.12,0)^3+0.0085272521*pmax(l3_s1-53.20125,0)^3-0.0012850483*pmax(l3_s1-61.0765,0)^3 }

# Helper function to check if a value is within a range
within_range <- function(value, range) {
  return(value >= range[1] && value <= range[2])
}



prescribe_l2_s1_segment_angles_function <- function(segment_angles_list,
                                                    pelvic_incidence, 
                                                    rigid_segments, 
                                                    l1pa_target_range) {
  rigid_segments <- str_to_lower(rigid_segments)
  
  starting_l5_segment_angle <- segment_angles_list$l5_segment_angle
  starting_l4_segment_angle <- segment_angles_list$l4_segment_angle
  starting_l3_segment_angle <- segment_angles_list$l3_segment_angle
  starting_l2_segment_angle <- segment_angles_list$l2_segment_angle
  
  l1pa_goal <- median(l1pa_target_range)
  
  starting_l1pa <- -2.7031182 + 0.61410488 * pelvic_incidence - 0.57421502 * starting_l5_segment_angle - 0.49327403 * starting_l4_segment_angle - 0.27054146 * starting_l3_segment_angle - 0.21642196 * starting_l2_segment_angle
  
  l2_s1 <- starting_l5_segment_angle + starting_l4_segment_angle + starting_l3_segment_angle + starting_l2_segment_angle
  
  
  l2_s1_goal_range <- prescribe_l2_s1_range_function(pelvic_incidence = pelvic_incidence, l1_pelvic_angle = l1pa_goal)
  
  l2_s1_goal <- median(l2_s1_goal_range)
  
  new_segment_angle_list <- list()
  pso_candidate_list <- list()
  pso_candidate_vector <- c()
  target_segmental_alignment_list <- list()
  
  ##### ##### ##### L5 ##### ##### #####
  ##### ##### ##### L5 ##### ##### #####
  l5_s1_target <- prescribe_l5_s1_function(l1_pelvic_angle = l1pa_goal, pelvic_incidence = pelvic_incidence, l2_s1 = l2_s1_goal)
  
  if(any(str_detect(rigid_segments, "l5_segment"))){
    new_segment_angle_list$l5_segment_angle <- starting_l5_segment_angle
  }else{
    new_segment_angle_list$l5_segment_angle <- l5_s1_target
  }
  if(between(new_segment_angle_list$l5_segment_angle, l5_s1_target- 2, l5_s1_target + 2) == FALSE){
    # pso_candidate_list$l5 <- "no"
    pso_candidate_vector <- append(pso_candidate_vector, "l5")
  }else{
    
  }
  
  
  target_segmental_alignment_list$l5_segment_angle <- l5_s1_target
  
  ##### ##### ##### L4 ##### ##### #####
  ##### ##### ##### L4 ##### ##### #####
  l4_l5_target <- prescribe_l4_l5_function(l1_pelvic_angle = l1pa_goal,
                                           pelvic_incidence = pelvic_incidence, 
                                           l2_s1 = l2_s1_goal,
                                           l5_s1 = new_segment_angle_list$l5_segment_angle)
  
  if(any(str_detect(rigid_segments, "l4_segment"))){
    new_segment_angle_list$l4_segment_angle <- starting_l4_segment_angle
  }else{
    new_segment_angle_list$l4_segment_angle <- l4_l5_target
  }
  if(between(new_segment_angle_list$l4_segment_angle, l4_l5_target- 2, l4_l5_target + 2) == FALSE){
    # pso_candidate_list$l4 <- "no"
    pso_candidate_vector <- append(pso_candidate_vector, "l4")
  }else{
    # pso_candidate_list$l4 <- "yes"
  }
  target_segmental_alignment_list$l4_segment_angle <- l4_l5_target
  
  ##### ##### ##### L3 ##### ##### #####
  ##### ##### ##### L3 ##### ##### #####
  l3_l4_target <- prescribe_l3_l4_function(l1_pelvic_angle = l1pa_goal,
                                           pelvic_incidence = pelvic_incidence, 
                                           l2_s1 = l2_s1_goal,
                                           l4_s1 = (new_segment_angle_list$l5_segment_angle + new_segment_angle_list$l4_segment_angle))
  
  if(any(str_detect(rigid_segments, "l3_segment"))){
    new_segment_angle_list$l3_segment_angle <- starting_l3_segment_angle
  }else{
    new_segment_angle_list$l3_segment_angle <- l3_l4_target
  }
  if(between(new_segment_angle_list$l3_segment_angle, l3_l4_target-2, l3_l4_target + 2) == FALSE){
    # pso_candidate_list$l3 <- "no"
    pso_candidate_vector <- append(pso_candidate_vector, "l3")
  }else{
    # pso_candidate_list$l3 <- "yes"
  }
  target_segmental_alignment_list$l3_segment_angle <- l3_l4_target
  
  ##### ##### ##### L2 ##### ##### #####
  ##### ##### ##### L2 ##### ##### #####
  l2_l3_target <- prescribe_l2_l3_function(l1_pelvic_angle = l1pa_goal,
                                           pelvic_incidence = pelvic_incidence, 
                                           l3_s1 = (new_segment_angle_list$l5_segment_angle + new_segment_angle_list$l4_segment_angle + new_segment_angle_list$l3_segment_angle))
  
  if(any(str_detect(rigid_segments, "l2_segment"))){
    new_segment_angle_list$l2_segment_angle <- starting_l2_segment_angle
  }else{
    new_segment_angle_list$l2_segment_angle <- l2_l3_target
  }
  if(between(new_segment_angle_list$l2_segment_angle, l2_l3_target- 2, l2_l3_target + 2) == FALSE){
    # pso_candidate_list$l2 <- "no"
    pso_candidate_vector <- append(pso_candidate_vector, "l2")
  }else{
    # pso_candidate_list$l2 <- "yes"
  }
  target_segmental_alignment_list$l2_segment_angle <- l2_l3_target
  
  computed_new_l1pa <- round(l1pa_computed_by_segment_angles_function(pelvic_incidence = pelvic_incidence, 
                                                                      l5_s1 = new_segment_angle_list$l5_segment_angle, 
                                                                      l4_l5 = new_segment_angle_list$l4_segment_angle, 
                                                                      l3_l4 = new_segment_angle_list$l3_segment_angle, 
                                                                      l2_l3 = new_segment_angle_list$l2_segment_angle), 0)
  
  new_l2_s1 <- new_segment_angle_list$l5_segment_angle + new_segment_angle_list$l4_segment_angle + new_segment_angle_list$l3_segment_angle+ new_segment_angle_list$l2_segment_angle
  
  # if(between(computed_new_l1pa, l1pa_target_range[1], l1pa_target_range[2]) && between(new_l2_s1, l2_s1_goal_range[1], l2_s1_goal_range[2])){
  #   needs_pso <- "no"
  # }else{
  #   needs_pso <- "yes"
  # }
  if(between(computed_new_l1pa, l1pa_target_range[1], l1pa_target_range[2])){
    needs_pso <- "no"
  }else{
    needs_pso <- "yes"
  }
  
  return(list(new_segment_angles = new_segment_angle_list, 
              l1pa_goal = l1pa_target_range,
              computed_l1pa = computed_new_l1pa,
              l2_s1_goal = l2_s1_goal,
              new_l2_s1 = new_l2_s1,
              target_segmental_alignment_list = target_segmental_alignment_list,
              pso_candidates = pso_candidate_vector, 
              needs_pso = needs_pso))
}

# Function to identify the most appropriate level for PSO
compute_pso_options_df <- function(segment_angles_list, pelvic_incidence, l1pa_goal, l2_s1_goal, pso_candidate_levels) {
  
  l2_s1_goal <- l2_s1_goal
  # Extract current segment angles
  l2_segment_angle <- segment_angles_list$l2_segment_angle
  l3_segment_angle <- segment_angles_list$l3_segment_angle
  l4_segment_angle <- segment_angles_list$l4_segment_angle
  l5_segment_angle <- segment_angles_list$l5_segment_angle
  
  # Coefficients for changes in L1PA
  l1pa_coefficients <- list(l5_segment_angle_coef = -0.5596789,
                            l4_segment_angle_coef = -0.4111246,
                            l3_segment_angle_coef = -0.3084623,
                            l2_segment_angle_coef = -0.2552592)
  # Calculate the current L1PA
  current_l1pa <- l1pa_computed_by_segment_angles_function(pelvic_incidence, l5_segment_angle, l4_segment_angle, l3_segment_angle, l2_segment_angle)
  # Calculate the current L2-S1
  current_l2_s1 <- l2_segment_angle + l3_segment_angle + l4_segment_angle + l5_segment_angle
  
  l1pa_change_needed <- l1pa_goal - current_l1pa
  
  segment_angle_changes_needed_list <- list()  
  
  total_l2_s1_by_pso_level_list <- list()
  new_l1pa_by_pso_level_list <- list()
  
  new_segment_angles_by_pso_level_list <- list()
  
  
  if(any(pso_candidate_levels == "l5")){
    segment_angle_changes_needed_list$l5_pso <- l1pa_change_needed/l1pa_coefficients$l5_segment_angle_coef
    
    new_l5_segment_angle <- l5_segment_angle + segment_angle_changes_needed_list$l5_pso
    
    new_segment_angles_by_pso_level_list$l5_pso_segment_angles <- list(l5_segment_angle = new_l5_segment_angle,
                                                                       l4_segment_angle = l4_segment_angle,
                                                                       l3_segment_angle = l3_segment_angle,
                                                                       l2_segment_angle = l2_segment_angle)
    
    total_l2_s1_by_pso_level_list$l5_pso <- sum(unlist(new_segment_angles_by_pso_level_list$l5))
    
    new_l1pa_by_pso_level_list$l5_pso <- l1pa_computed_by_segment_angles_function(pelvic_incidence = pelvic_incidence, 
                                                                                  l5_s1 = new_segment_angles_by_pso_level_list$l5_pso_segment_angles$l5_segment_angle,
                                                                                  l4_l5 = new_segment_angles_by_pso_level_list$l5_pso_segment_angles$l4_segment_angle, 
                                                                                  l3_l4 = new_segment_angles_by_pso_level_list$l5_pso_segment_angles$l3_segment_angle,  
                                                                                  l2_l3 = new_segment_angles_by_pso_level_list$l5_pso_segment_angles$l2_segment_angle 
    )
  }
  
  if(any(pso_candidate_levels == "l4")){
    segment_angle_changes_needed_list$l4_pso <- l1pa_change_needed/l1pa_coefficients$l4_segment_angle_coef
    
    new_l4_segment_angle <- l4_segment_angle + segment_angle_changes_needed_list$l4_pso
    
    new_segment_angles_by_pso_level_list$l4_pso_segment_angles <- list(l5_segment_angle = l5_segment_angle,
                                                                       l4_segment_angle = new_l4_segment_angle,
                                                                       l3_segment_angle = l3_segment_angle,
                                                                       l2_segment_angle = l2_segment_angle)
    
    total_l2_s1_by_pso_level_list$l4_pso <- sum(unlist(new_segment_angles_by_pso_level_list$l4))
    
    new_l1pa_by_pso_level_list$l4_pso <- l1pa_computed_by_segment_angles_function(pelvic_incidence = pelvic_incidence, 
                                                                                  l5_s1 = new_segment_angles_by_pso_level_list$l4_pso_segment_angles$l5_segment_angle,
                                                                                  l4_l5 = new_segment_angles_by_pso_level_list$l4_pso_segment_angles$l4_segment_angle, 
                                                                                  l3_l4 = new_segment_angles_by_pso_level_list$l4_pso_segment_angles$l3_segment_angle,  
                                                                                  l2_l3 = new_segment_angles_by_pso_level_list$l4_pso_segment_angles$l2_segment_angle 
    )
  }
  
  if(any(pso_candidate_levels == "l3")){
    segment_angle_changes_needed_list$l3_pso <- l1pa_change_needed/l1pa_coefficients$l3_segment_angle_coef
    
    new_l3_segment_angle <- l3_segment_angle + segment_angle_changes_needed_list$l3_pso
    
    new_segment_angles_by_pso_level_list$l3_pso_segment_angles <- list(l5_segment_angle = l5_segment_angle,
                                                                       l4_segment_angle = l4_segment_angle,
                                                                       l3_segment_angle = new_l3_segment_angle,
                                                                       l2_segment_angle = l2_segment_angle)
    
    total_l2_s1_by_pso_level_list$l3_pso <- sum(unlist(new_segment_angles_by_pso_level_list$l3))
    
    new_l1pa_by_pso_level_list$l3_pso <- l1pa_computed_by_segment_angles_function(pelvic_incidence = pelvic_incidence, 
                                                                                  l5_s1 = new_segment_angles_by_pso_level_list$l3_pso_segment_angles$l5_segment_angle,
                                                                                  l4_l5 = new_segment_angles_by_pso_level_list$l3_pso_segment_angles$l4_segment_angle, 
                                                                                  l3_l4 = new_segment_angles_by_pso_level_list$l3_pso_segment_angles$l3_segment_angle,  
                                                                                  l2_l3 = new_segment_angles_by_pso_level_list$l3_pso_segment_angles$l2_segment_angle 
    )
  }
  
  if(any(pso_candidate_levels == "l2")){
    segment_angle_changes_needed_list$l2_pso <- l1pa_change_needed/l1pa_coefficients$l2_segment_angle_coef
    
    new_l2_segment_angle <- l2_segment_angle + segment_angle_changes_needed_list$l2_pso
    
    new_segment_angles_by_pso_level_list$l2_pso_segment_angles <- list(l5_segment_angle = l5_segment_angle,
                                                                       l4_segment_angle = l4_segment_angle,
                                                                       l3_segment_angle = l3_segment_angle,
                                                                       l2_segment_angle = new_l2_segment_angle)
    
    total_l2_s1_by_pso_level_list$l2_pso <- sum(unlist(new_segment_angles_by_pso_level_list$l2))
    
    new_l1pa_by_pso_level_list$l2_pso <- l1pa_computed_by_segment_angles_function(pelvic_incidence = pelvic_incidence, 
                                                                                  l5_s1 = new_segment_angles_by_pso_level_list$l2_pso_segment_angles$l5_segment_angle,
                                                                                  l4_l5 = new_segment_angles_by_pso_level_list$l2_pso_segment_angles$l4_segment_angle, 
                                                                                  l3_l4 = new_segment_angles_by_pso_level_list$l2_pso_segment_angles$l3_segment_angle,  
                                                                                  l2_l3 = new_segment_angles_by_pso_level_list$l2_pso_segment_angles$l2_segment_angle 
    )
  }
  
  pso_options_l1pa_df <- enframe(new_l1pa_by_pso_level_list) %>%
    unnest() %>%
    rename(pso_level = name, new_l1pa = value)
  
  pso_options_l2s1_df <-  enframe(total_l2_s1_by_pso_level_list) %>%
    unnest(value)  %>%
    rename(pso_level = name, new_l2_s1 = value)
  
  pso_options_summary_df <- pso_options_l1pa_df %>%
    mutate(l1pa_goal = l1pa_goal) %>%
    mutate(l1pa_error = new_l1pa - l1pa_goal) %>%
    left_join(pso_options_l2s1_df)  %>%
    mutate(l2s1_goal = l2_s1_goal) %>%
    mutate(l2_s1_error = new_l2_s1 - l2_s1_goal) %>%
    mutate(total_error = (l1pa_error^2 + l2_s1_error^2)^0.5) %>%
    mutate(pso_level = str_remove_all(pso_level, "_pso"))%>%
    arrange(total_error)
  
  
  
  return(list(pso_options_summary_df = pso_options_summary_df,
              new_l1pa_by_pso_level_list = new_l1pa_by_pso_level_list, 
              total_l2_s1_by_pso_level_list = total_l2_s1_by_pso_level_list,
              new_segment_angles_by_pso_level_list = new_segment_angles_by_pso_level_list, 
              segment_angle_changes = segment_angle_changes_needed_list
  ))
}

### now need function for determining final T10-L2
### functions needed 
prescribe_l1_l2_function <- function(l1_pelvic_angle = 3.86,
                                     l2_s1 = 55.36,
                                     t10_l2 = -0.17825,
                                     t1_t10 = -34.53485) {
  -6.7489053+0.14363156*l1_pelvic_angle+0.10675749*l2_s1+0.30178728*t10_l2-0.089073178*t1_t10 
}

prescribe_t12_l1_function <- function(l1_l2 = 2.765,
                                      t10_l2 = -0.17825,
                                      t1_t10 = -34.53485) {
  1.9503983-0.16611319*l1_l2+0.25817635*t10_l2-0.042760872*t1_t10 
}

prescribe_t11_t12_function <- function(t12_l1 = 2.9258,l1_l2 = 2.765,t10_l2 = -0.17825,t1_t10 = -34.53485) {
  -0.31527913-0.52572415*t12_l1-0.43806389*l1_l2+0.54048369*t10_l2-0.018009895*t1_t10 
}


##### ##### ##### ##### ##### prescribe_t10_l2 ##### ##### ##### ##### ##### ##### 
prescribe_t10_l2_after_lumbar_function <- function(l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l2_s1 = 55.36,t1_t10 = -34.53485,c2_t1 = 18.93865) {-2.1621379+0.57870185*l1_pelvic_angle+0.073090484*pelvic_incidence-0.26750716*l2_s1-0.44442592*t1_t10-0.26591165*c2_t1 }


prescribe_t10_l2_segment_angles_function <- function(segment_angles_list,
                                                     t10_l2_target,
                                                     pelvic_incidence, 
                                                     new_l1pa,
                                                     new_l2_s1,
                                                     t1_t10,
                                                     rigid_segments = c("")) {
  
  rigid_segments <- str_to_lower(rigid_segments)
  
  starting_t10_segment_angle <- segment_angles_list$t10_segment_angle
  starting_t11_segment_angle <- segment_angles_list$t11_segment_angle
  starting_t12_segment_angle <- segment_angles_list$t12_segment_angle
  starting_l1_segment_angle <- segment_angles_list$l1_segment_angle
  
  t10_l2 <- starting_t10_segment_angle + starting_t11_segment_angle + starting_t12_segment_angle + starting_l1_segment_angle
  
  # if(between(t10_l2, t10_l2_target - 4, t10_l2_target + 4))
  
  
  new_segment_angle_list <- list()
  pso_candidate_list <- list()
  pso_candidate_vector <- c("")
  target_segmental_alignment_list <- list()
  
  ##### ##### ##### L1 ##### ##### #####
  ##### ##### ##### L1 ##### ##### #####
  l1_l2_target <- prescribe_l1_l2_function(l1_pelvic_angle = new_l1pa, l2_s1 = new_l2_s1, t10_l2 = t10_l2_target, t1_t10 = t1_t10)
  
  if(any(str_detect(rigid_segments, "l1_segment"))){
    new_segment_angle_list$l1_segment_angle <- starting_l1_segment_angle
  }else{
    new_segment_angle_list$l1_segment_angle <- l1_l2_target
  }
  
  if(between(new_segment_angle_list$l1_segment_angle, l1_l2_target-4, l1_l2_target + 4) == FALSE){
    pso_candidate_vector <- append(pso_candidate_vector, "l1")
  }
  
  ##### ##### ##### t12 ##### ##### #####
  ##### ##### ##### t12 ##### ##### #####
  t12_l1_target <- prescribe_t12_l1_function(l1_l2 = new_segment_angle_list$l1_segment_angle,
                                             t10_l2 = t10_l2_target,
                                             t1_t10 = t1_t10)
  
  if(any(str_detect(rigid_segments, "t12_segment"))){
    new_segment_angle_list$t12_segment_angle <- starting_t12_segment_angle
  }else{
    new_segment_angle_list$t12_segment_angle <- t12_l1_target
  }
  if(between(new_segment_angle_list$t12_segment_angle, t12_l1_target-4, t12_l1_target + 4) == FALSE){
    pso_candidate_vector <- append(pso_candidate_vector, "t12")
  }
  
  ##### ##### ##### t11 ##### ##### #####
  ##### ##### ##### t11 ##### ##### #####
  t11_t12_target <- prescribe_t11_t12_function(t12_l1 = new_segment_angle_list$t12_segment_angle, 
                                               l1_l2 = new_segment_angle_list$l1_segment_angle,
                                               t10_l2 = t10_l2_target, t1_t10 = t1_t10)
  
  if(any(str_detect(rigid_segments, "t11_segment"))){
    new_segment_angle_list$t11_segment_angle <- starting_t11_segment_angle
  }else{
    new_segment_angle_list$t11_segment_angle <- t11_t12_target
  }
  if(between(new_segment_angle_list$t11_segment_angle, t11_t12_target-4, t11_t12_target + 4) == FALSE){
    pso_candidate_vector <- append(pso_candidate_vector, "t11")
  }
  
  ##### ##### ##### t10 ##### ##### #####
  ##### ##### ##### t10 ##### ##### #####
  t10_t11_target <- t10_l2_target - (new_segment_angle_list$t11_segment_angle + new_segment_angle_list$t12_segment_angle + new_segment_angle_list$l1_segment_angle)
  
  if(any(str_detect(rigid_segments, "t10_segment"))){
    new_segment_angle_list$t10_segment_angle <- starting_t10_segment_angle
  }else{
    new_segment_angle_list$t10_segment_angle <- t10_t11_target
  }
  if(between(new_segment_angle_list$t10_segment_angle, t10_t11_target-4, t10_t11_target + 4) == FALSE){
    pso_candidate_vector <- append(pso_candidate_vector, "t10")
  }
  
  current_t10_l2 <-  new_segment_angle_list$t10_segment_angle + new_segment_angle_list$t11_segment_angle + new_segment_angle_list$t12_segment_angle + new_segment_angle_list$l1_segment_angle  
  
  if(between(current_t10_l2, t10_l2_target - 4, t10_l2_target + 4)){
    needs_pso <- "no"
  }else{
    needs_pso <- "yes"
  }
  
  return(list(new_segment_angles = new_segment_angle_list, 
              t10_l2_goal = t10_l2_target,
              new_t10_l2 = current_t10_l2,
              new_segment_angle_list = new_segment_angle_list,
              pso_candidates = pso_candidate_vector, 
              needs_pso = needs_pso))
  
}



############################################################# BUILD FULL T11 UIV FUNCTION  ###########################################
############################################################# BUILD FULL T11 UIV FUNCTION  ###########################################
############################################################# BUILD FULL T11 UIV FUNCTION  ###########################################
############################################################# BUILD FULL T11 UIV FUNCTION  ###########################################
############################################################# BUILD FULL T11 UIV FUNCTION  ###########################################


build_t11_spine_plot_function <- function(pso_option_number = 1,
                                          preop_age, 
                                          preop_sex,
                                          preop_pelvic_incidence, 
                                          preop_pt, 
                                          preop_l1pa,
                                          preop_t9pa,
                                          preop_t4pa,
                                          preop_c2pa, 
                                          l1pa_line_color, 
                                          t4pa_line_color, 
                                          c2pa_line_color, 
                                          preop_segment_angles_input_list_reactive,
                                          preop_rigid_levels_vector_reactive
                                          ){

  ############################################### MATCHING ALIGNMENT STARTS ##############################################
  
  alignment_targets_list <- list()

  starting_pi <- preop_pelvic_incidence
  
  starting_segment_angles <- preop_segment_angles_input_list_reactive
  
  preop_c2_t1 <- compute_c2_t1_function(segment_angles_list = starting_segment_angles)
  
  preop_t1_t10 <- compute_t1_t10_function(segment_angles_list = starting_segment_angles)
  
  prescribed_t10_l2 <- prescribe_t10_l2_function(pelvic_incidence = starting_pi, 
                                                 c2_t1 = preop_c2_t1, 
                                                 t1_t10 = preop_t1_t10)
  
  prescribed_l1pa <- prescribe_l1pa_function(pelvic_incidence = starting_pi, 
                                             c2_t1 = preop_c2_t1, 
                                             t1_t10 = preop_t1_t10, 
                                             t10_l2 = prescribed_t10_l2)
  
  prescribed_segment_angles <- starting_segment_angles
  
  rigid_levels <- preop_rigid_levels_vector_reactive
  
  
  
  prescribed_l2_s1_list <- prescribe_l2_s1_segment_angles_function(segment_angles_list = starting_segment_angles, 
                                                                   pelvic_incidence = starting_pi, 
                                                                   rigid_segments = rigid_levels, 
                                                                   l1pa_target_range = c(prescribed_l1pa - 3, prescribed_l1pa + 3)
  )
  
  if(prescribed_l2_s1_list$needs_pso == "yes"){
    pso_options_list <- compute_pso_options_df(segment_angles_list = prescribed_l2_s1_list$new_segment_angles,
                                               pelvic_incidence = starting_pi, 
                                               pso_candidate_levels = prescribed_l2_s1_list$pso_candidates, 
                                               l1pa_goal = prescribed_l1pa, 
                                               l2_s1_goal = prescribed_l2_s1_list$l2_s1_goal)
    
    pso_options_df <- pso_options_list$pso_options_summary_df 

    ############ pso option ############
    
    if(nrow(pso_options_df)>1){
      pso_option <- pso_option_number
    }else{
      pso_option <- 1
    }
    
    pso_level <- str_to_upper(pso_options_df$pso_level[[pso_option]])
    
    pso_l2s1_segment_angles <- pso_options_list$new_segment_angles_by_pso_level_list[[paste0(pso_options_df$pso_level[[pso_option]], "_pso_segment_angles")]]
    
    prescribed_segment_angles$l5_segment_angle <- pso_l2s1_segment_angles$l5_segment_angle
    prescribed_segment_angles$l4_segment_angle <- pso_l2s1_segment_angles$l4_segment_angle
    prescribed_segment_angles$l3_segment_angle <- pso_l2s1_segment_angles$l3_segment_angle
    prescribed_segment_angles$l2_segment_angle <- pso_l2s1_segment_angles$l2_segment_angle
    
  }else{
    pso_level <- "na"
    prescribed_segment_angles$l5_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l5_segment_angle
    prescribed_segment_angles$l4_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l4_segment_angle
    prescribed_segment_angles$l3_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l3_segment_angle
    prescribed_segment_angles$l2_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l2_segment_angle
    
  }
  
  
  ### now compute the TL junction #####
  
  final_prescription_l1pa_calculated <- l1pa_computed_by_segment_angles_function(pelvic_incidence = starting_pi, 
                                                                                 l5_s1 = prescribed_segment_angles$l5_segment_angle, 
                                                                                 l4_l5 = prescribed_segment_angles$l4_segment_angle, 
                                                                                 l3_l4 = prescribed_segment_angles$l3_segment_angle, 
                                                                                 l2_l3 = prescribed_segment_angles$l2_segment_angle)
  
  final_l2_s1_prescription <- prescribed_segment_angles$l5_segment_angle + prescribed_segment_angles$l4_segment_angle +prescribed_segment_angles$l3_segment_angle +prescribed_segment_angles$l2_segment_angle
  
  alignment_targets_list$"L1PA" <- paste0(round(prescribed_l1pa -3, 0), "º to ", round(prescribed_l1pa +3, 0), "º")
  alignment_targets_list$"L2-S1" <- paste0(round(prescribed_l2_s1_list$l2_s1_goal - 4, 0), "º to ", round(prescribed_l2_s1_list$l2_s1_goal + 4, 0), "º")
  
  t10_l2_after_lspine_goal <- prescribe_t10_l2_after_lumbar_function(l1_pelvic_angle = final_prescription_l1pa_calculated, 
                                                                     pelvic_incidence = starting_pi, 
                                                                     l2_s1 = final_l2_s1_prescription, 
                                                                     t1_t10 = preop_t1_t10, 
                                                                     c2_t1 = preop_c2_t1)
  
  
  alignment_targets_list$"T10-L2" <- paste0(round(t10_l2_after_lspine_goal -4, 0), "º to ", round(t10_l2_after_lspine_goal +4, 0), "º")
  
  t10_l2_prescribed_list <- prescribe_t10_l2_segment_angles_function(segment_angles_list = prescribed_segment_angles,
                                                                     t10_l2_target = t10_l2_after_lspine_goal, 
                                                                     pelvic_incidence = starting_pi,
                                                                     new_l1pa = final_prescription_l1pa_calculated, 
                                                                     new_l2_s1 = final_l2_s1_prescription, 
                                                                     t1_t10 = preop_t1_t10, 
                                                                     rigid_segments = rigid_levels)
  
  
  if(t10_l2_prescribed_list$needs_pso == "yes"){
    
    set_min_to_zero <- function(t10_l2_segment_angles_list) {
      # Find the minimum value in the list
      min_value <- min(unlist(t10_l2_segment_angles_list))
      # Find the name of the element with the minimum value
      min_name <- names(t10_l2_segment_angles_list)[which(unlist(t10_l2_segment_angles_list) == min_value)]
      # Set the minimum value to 0
      t10_l2_segment_angles_list[[min_name]] <- 0
      # Return the modified list
      return(t10_l2_segment_angles_list)
    }
    
    t10_l2_prescribed_list$new_segment_angles <- set_min_to_zero(t10_l2_prescribed_list$new_segment_angles)
    prescribed_segment_angles$l1_segment_angle <- t10_l2_prescribed_list$new_segment_angles$l1_segment_angle
    prescribed_segment_angles$t12_segment_angle <- t10_l2_prescribed_list$new_segment_angles$t12_segment_angle
    prescribed_segment_angles$t11_segment_angle <- t10_l2_prescribed_list$new_segment_angles$t11_segment_angle
    # prescribed_segment_angles$t10_segment_angle <- t10_l2_prescribed_list$new_segment_angles$t10_segment_angle
    
  }else{
    
    prescribed_segment_angles$l1_segment_angle <- t10_l2_prescribed_list$new_segment_angles$l1_segment_angle
    prescribed_segment_angles$t12_segment_angle <- t10_l2_prescribed_list$new_segment_angles$t12_segment_angle
    prescribed_segment_angles$t11_segment_angle <- t10_l2_prescribed_list$new_segment_angles$t11_segment_angle
    # prescribed_segment_angles$t10_segment_angle <- t10_l2_prescribed_list$new_segment_angles$t10_segment_angle
    
  }
  
  final_t10_l2 <- prescribed_segment_angles$l1_segment_angle + prescribed_segment_angles$t12_segment_angle + prescribed_segment_angles$t11_segment_angle + prescribed_segment_angles$t10_segment_angle
  
  ############################################################ MATCHING ALIGNMENT ENDS #############################################
  ############################################################ MATCHING ALIGNMENT ENDS #############################################
  ############################################################ MATCHING ALIGNMENT ENDS #############################################
  ############################################################ MATCHING ALIGNMENT ENDS #############################################
  ############################################################ MATCHING ALIGNMENT ENDS #############################################
  checking_vpa_list <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi, 
                                                                  pt_value = 10,
                                                                  segment_angle_list = prescribed_segment_angles)
  
  current_c2_t1 <- sum(unlist(prescribed_segment_angles[18:23]))
  
  predicted_c2_c7_lordosis <- predict_postop_c2_c7_function(rad_pre_c2_c7 = current_c2_t1,
                                                            final_t4pa_goal = checking_vpa_list$t4pa_value,
                                                            starting_t4pa = preop_t4pa)
  
  prescribed_segment_angles$c2_segment_angle <- predicted_c2_c7_lordosis*c2_segment_weight
  prescribed_segment_angles$c3_segment_angle <- predicted_c2_c7_lordosis*c3_segment_weight
  prescribed_segment_angles$c4_segment_angle <- predicted_c2_c7_lordosis*c4_segment_weight
  prescribed_segment_angles$c5_segment_angle <- predicted_c2_c7_lordosis*c5_segment_weight
  prescribed_segment_angles$c6_segment_angle <- predicted_c2_c7_lordosis*c6_segment_weight
  prescribed_segment_angles$c7_segment_angle <- predicted_c2_c7_lordosis*c7_segment_weight
  
  
  new_vpa_list <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi, 
                                                                  pt_value = 10,
                                                                  segment_angle_list = prescribed_segment_angles)
  
  
  predicted_pt <- predict_postop_pt_function(preop_pt = preop_pt, 
                                             preop_c2_tilt = (preop_c2pa - preop_pt), 
                                             postop_c2pa = (new_vpa_list$c2pa_value))
  
  
  spine_prescribed_list <- build_full_spine_function_new(pelv_inc_value = starting_pi,
                                                         segment_angle_list = prescribed_segment_angles,
                                                         pt_value = predicted_pt[1],
                                                         spine_faces = "right",
                                                         # pso_levels = as_vector(pso_list)
                                                         pso_levels = pso_level
  )
  
  
  
  spine_geoms_df <- spine_prescribed_list$spine_df %>%
    select(object, geom, geom_alpha)  
  
  lines_list <- list()
  
  lines_list$l1pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$l1pa_line_curve_sf,
                                                             color_input = l1pa_line_color, 
                                                             line_size = 1
  )
  
  lines_list$t4pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$t4pa_line_curve_sf,
                                                             color_input = t4pa_line_color,
                                                             line_size = 0.75
  )
  
  lines_list$c2pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$c2pa_line_extended_curve_sf,
                                                             color_input = c2pa_line_color,
                                                             line_size = 0.5
  )
  
  preop_t4pa_l1pa_mismatch = preop_t4pa - preop_l1pa
  
  uiv_for_rod <- 18 ## for T11 UIV
  
  instrumented_vertebral_body_list <- spine_prescribed_list$vertebral_body_list[uiv_for_rod:length(spine_prescribed_list$vertebral_body_list)]
  
  sp_matrix_for_rod <- do.call(rbind, map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ rbind(instrumented_vertebral_body_list[[.x]]$sp)))
  
  rod_sf <- st_buffer(x = st_linestring(sp_matrix_for_rod), dist = 0.5, nQuadSegs = 5, joinStyle = "ROUND", endCapStyle = "ROUND")
  
  screw_list <- st_multipolygon(compact(map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ instrumented_vertebral_body_list[[.x]]$screw_sf)))
  
  ################# Plot labels ###############
  alignment_targets_df <- enframe(alignment_targets_list) %>%
    unnest() %>%
    mutate(x = -23) %>%
    mutate(y = c(seq(from = 26, by = -4, length = length(alignment_targets_list)))) %>%
    mutate(label = paste(name, value, sep = " = "))
  
  
  measurements_list <- list()
  measurements_list$"C2 Tilt" <- round(spine_prescribed_list$spine_list$c2_tilt_value)
  measurements_list$"C2PA" <- round(spine_prescribed_list$spine_list$c2pa_value, 0)
  measurements_list$"T1PA" <- round(spine_prescribed_list$spine_list$t1pa_value, 0)
  measurements_list$"T4PA" <- round(spine_prescribed_list$spine_list$t4pa_value, 0)
  measurements_list$"L1PA" <- round(spine_prescribed_list$spine_list$l1pa_value, 0)
  measurements_list$"LL" <-  round(spine_prescribed_list$spine_list$lumbar_lordosis, 0)
  measurements_list$"TK" <- round(spine_prescribed_list$spine_list$thoracic_kyphosis, 0)
  measurements_list$"PT" <- round(spine_prescribed_list$spine_list$pelvic_tilt, 0)
  
  measurements_df <- enframe(measurements_list) %>%
    unnest() %>%
    mutate(x = 18) %>%
    mutate(y = c(seq(from = 74, by = -4, length = length(measurements_list)-2), 7, 3)) %>%
    mutate(label = paste(name, value, sep = " = ")) 
  

  segment_angles_list_for_plot <- list()
  segment_angles_list_for_plot$"T10-L2" <- as.character(round(final_t10_l2, 0))
  segment_angles_list_for_plot$"L1-L2" <- as.character(round(prescribed_segment_angles$l1_segment_angle, 0))
  segment_angles_list_for_plot$"L2-L3" <- as.character(round(prescribed_segment_angles$l2_segment_angle, 0))
  segment_angles_list_for_plot$"L3-L4" <- as.character(round(prescribed_segment_angles$l3_segment_angle, 0))
  segment_angles_list_for_plot$"L4-L5" <- as.character(round(prescribed_segment_angles$l4_segment_angle, 0))
  segment_angles_list_for_plot$"L5-S1" <- as.character(round(prescribed_segment_angles$l5_segment_angle, 0))
  
  
  if(pso_level != "na"){
    pso_label <- paste("PSO at", pso_level)
  }else{
    pso_label <- " "
  }
 
  segment_angles_df <- enframe(segment_angles_list_for_plot) %>%
    unnest() %>%
    mutate(x = 18) %>%
    mutate(y = c(seq(from = 35, by = -4, length = length(segment_angles_list_for_plot)))) %>%
    mutate(label = paste(name, value, sep = " = "))
  

  pjk_risk <- pjk_risk_function(age = preop_age,
                                sex = preop_sex, 
                                preop_c2pa_t9pa_mismatch = (preop_c2pa - preop_t9pa),
                                pelvic_incidence = starting_pi,
                                # postop_l1pa_undercorrection = round(spine_prescribed_list$spine_list$l1pa_value, 0) - (round(spine_prescribed_list$spine_list$l1pa_value, 0)*0.5-17),
                                # postop_t4l1pa_mismatch = round(spine_prescribed_list$spine_list$t4pa_value, 0) - round(spine_prescribed_list$spine_list$l1pa_value, 0), 
                                uiv_region = "Lower Thoracic"
                                  
  )
  
  
  pjk_risk_color <- case_when(
    pjk_risk > 0.45 ~ "red",
    between(pjk_risk, 0.35, 0.45) ~ "darkorange",
    between(pjk_risk, 0.2, 0.35) ~ "orange",
    pjk_risk < 0.2 ~ "darkgreen"
  )
  
  ggplot() +
    geom_sf(data = spine_geoms_df,
            aes(geometry = geom,
                alpha = geom_alpha
            ),
            color = "black",
            fill = "grey90") +
    geom_sf(data = st_multipolygon(x = screw_list), fill = "grey55") +
    geom_sf(data = rod_sf, fill = "grey55") +
    draw_text(text = measurements_df$label, x = measurements_df$x, y = measurements_df$y, size = 14) +
    draw_text(text = "Regional\nTargets", x = -23, y = max(alignment_targets_df$y)+7, size = 14, fontface = "bold") +
    draw_text(text = alignment_targets_df$label, x = alignment_targets_df$x, y = alignment_targets_df$y, size = 12) +
    draw_text(text = segment_angles_df$label, x = segment_angles_df$x, y = segment_angles_df$y, size = 14) +
    draw_text(text = "UIV: T11", x = -26, y = 56, size = 14, fontface = "bold") +
    draw_text(text = pso_label, x = -26, y = 12, size = 16, fontface = "bold", color = "blue") +
    draw_text(text = paste("PJK Risk = ", pjk_risk), x = 0, y = -5, size = 16, color = pjk_risk_color, fontface = "bold") +
    lines_list +
    xlim(-35, 32) +
    # ylim(-6, 110) +
    theme_void() +
    # labs(title = "T11 UIV") +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA)
    ) +
    scale_alpha_identity()
}


############################################################## UPPER T UIV FUNCTIONS ##########################################

prescribe_l1_l2_function_t4l1pa <- function(t4_pelvic_angle = 3.3,
                                            l1_pelvic_angle = 3.86,
                                            pelvic_incidence = 51.66) {
  
  -2.4802818-0.71643864*t4_pelvic_angle+0.89785487*l1_pelvic_angle+0.077909797*pelvic_incidence
}


prescribe_t12_l1_function_t4l1pa <- function(t4_pelvic_angle = 3.3,
                                             l1_pelvic_angle = 3.86,
                                             l2_s1 = 55.36,
                                             l1_l2 = 2.765,
                                             pelvic_incidence = 51.66) {
  3.4341578-0.57137432*t4_pelvic_angle+0.39111593*l1_pelvic_angle-0.18056706*l2_s1-0.056357755*l1_l2+0.19307184*pelvic_incidence 
}
# prescribe_t12_l1_function_t4l1pa()

prescribe_t11_t12_function_t4l1pa <-  function(t4_pelvic_angle = 3.3,
                                               l1_pelvic_angle = 3.86,
                                               t12_s1 = 61.48815,
                                               pelvic_incidence = 51.66) {
  2.9557886-0.94742169*t4_pelvic_angle+0.82619395*l1_pelvic_angle-0.28172993*t12_s1+0.22258719*pelvic_incidence 
}


prescribe_t10_t11_function_t4l1pa <- function(t4_pelvic_angle = 3.3,
                                              l1_pelvic_angle = 3.86,
                                              t11_s1 = 59.33755,
                                              pelvic_incidence = 51.66) {
  3.2431541-1.2054659*t4_pelvic_angle+0.86886472*l1_pelvic_angle-0.45583262*t11_s1+0.39802175*pelvic_incidence 
}

prescribe_t9_t10_function_t4l1pa <- function(t4_pelvic_angle = 3.3,
                                             l1_pelvic_angle = 3.86,
                                             t10_t11 = -3.2588,
                                             t11_t12 = -2.4078,
                                             t12_s1 = 61.48815,
                                             pelvic_incidence = 51.66) {
  1.6696779-1.2865699*t4_pelvic_angle+0.70996238*l1_pelvic_angle-0.20801106*t10_t11-0.38525448*t11_t12-0.56734468*t12_s1+0.58366434*pelvic_incidence 
}



############################################################# BUILD FULL UPPER THORACIC UIV FUNCTION  ###########################################
############################################################# BUILD FULL UPPER THORACIC UIV FUNCTION  ###########################################
############################################################# BUILD FULL UPPER THORACIC UIV FUNCTION  ###########################################
############################################################# BUILD FULL UPPER THORACIC UIV FUNCTION  ###########################################
############################################################# BUILD FULL UPPER THORACIC UIV FUNCTION  ###########################################


######################################### TL segment weights  #########################################
t4pa_coefficient_values <- c(-0.00643713846808736, -0.0403277381976167, -0.0615647444318114, -0.0507642465685891, -0.0924827963044182, -0.234525336295404, -0.213921607300027, -0.272517721877618, -0.380590634879677, -0.493285124553993)

t4pa_coefficient_names <- c('t4_t5', 't5_t6', 't6_t7', 't7_t8', 't8_t9', 't9_t10', 't10_t11', 't11_t12', 't12_l1', 'l1_l2')

tl_segments_t4pa_weight_df <- tibble(level = t4pa_coefficient_names, weight = t4pa_coefficient_values) %>%
  separate(level,into = c("level", "distal"), sep = "_") %>%
  mutate(level = paste0(level, "_segment_angle")) %>%
  select(level, weight) %>%
  arrange(weight)

#########################################functions for defining normal #########################################
prescribing_t4pa_by_matching_function <- function(l1_pelvic_angle = 3.86,c2_t1 = 18.93865,t1_t4 = -6.131) {
  -0.6609962+0.9059163*l1_pelvic_angle+0.070538214*c2_t1+0.14492222*t1_t4 
}


normal_l1_l2_function_t4l1pa <- function(t4_pelvic_angle = 3.3,l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l2_s1 = 55.36) {
  -2.7836307-0.75002363*t4_pelvic_angle+0.78978607*l1_pelvic_angle+0.17450142*pelvic_incidence-0.074628581*l2_s1
}

normal_t12_l1_function_t4l1pa <- function(t4_pelvic_angle = 3.3,l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l2_s1 = 55.36) {
  3.591037-0.52910468*t4_pelvic_angle+0.34660536*l1_pelvic_angle+0.18323733*pelvic_incidence-0.17636116*l2_s1
}

normal_t11_t12_function_t4l1pa <- function(t4_pelvic_angle = 3.3,l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l2_s1 = 55.36) {2.4421199-0.61873912*t4_pelvic_angle+0.40407968*l1_pelvic_angle+0.212932*pelvic_incidence-0.28142783*l2_s1 }

normal_t10_t11_function_t4l1pa <- function(t4_pelvic_angle = 3.3,l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l2_s1 = 55.36) {1.531923-0.36581928*t4_pelvic_angle+0.084732847*l1_pelvic_angle+0.21112462*pelvic_incidence-0.26972073*l2_s1 }

normal_t9_t10_function_t4l1pa <- function(t4_pelvic_angle = 3.3,l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l2_s1 = 55.36) {0.03978204-0.23668979*t4_pelvic_angle-0.076827202*l1_pelvic_angle+0.22683621*pelvic_incidence-0.238851*l2_s1 }

normal_t8_t9_function_t4l1pa <- function(t4_pelvic_angle = 3.3,l1_pelvic_angle = 3.86,pelvic_incidence = 51.66,l2_s1 = 55.36) {-0.15365204-0.10729732*t4_pelvic_angle-0.22848748*l1_pelvic_angle+0.20641717*pelvic_incidence-0.23217261*l2_s1 }




######################################## ADJUST TL JUNCTION ALINMENT AND IDENTIFYING PSO LEVEL FUNCTION#####################
######################################### identifying pso   #########################################
adjusting_tl_segment_angles_function <- function(segment_angles_list_input, 
                                                 pelvic_incidence_input = 50, 
                                                 rigid_levels_input = c(""), 
                                                 pso_level_input = c(""), 
                                                 pso_modifier = 0,
                                                 flexible_modifier = 0) {
  current_segment_angles_list <- segment_angles_list_input
  pelvic_incidence <- pelvic_incidence_input
  rigid_levels <- rigid_levels_input
  
  l1_flexible_modifier  <- flexible_modifier * -0.493
  t12_flexible_modifier  <- flexible_modifier * -0.381
  t11_flexible_modifier  <- flexible_modifier * -0.273
  t10_flexible_modifier  <- flexible_modifier * -0.214
  t9_flexible_modifier  <- flexible_modifier * -0.235
  # t8_flexible_modifier  <- flexible_modifier * -0.092
  # t6_flexible_modifier  <- flexible_modifier * -0.062
  # t7_flexible_modifier  <- flexible_modifier * -0.051
  # t5_flexible_modifier  <- flexible_modifier * -0.04
  # t4_flexible_modifier  <- flexible_modifier * -0.006
  
  output_list <- list()
  ##### NORMAL TL JUNCTION ANGLES ######
  checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pelvic_incidence,
                                                                   pt_value = 10,
                                                                   segment_angle_list = current_segment_angles_list)
  
  current_c2_t1 <- sum(unlist(current_segment_angles_list[18:23]))
  current_t1_t4 <- sum(unlist(current_segment_angles_list[15:17]))
  current_l2_s1 <- sum(unlist(current_segment_angles_list[1:4]))
  
  current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
  
  target_t4pa <- prescribing_t4pa_by_matching_function(l1_pelvic_angle = checking_vpas$l1pa_value,
                                                       c2_t1 = current_c2_t1, 
                                                       t1_t4 = current_t1_t4)
  target_t4pa_range <- c(target_t4pa - 1, target_t4pa + 1)
  
  output_list$target_t4pa <- target_t4pa
  output_list$target_t4pa_range <- target_t4pa_range
  
  normal_expected_segment_angles_df <- tibble(t4pa_goal = target_t4pa, 
                                              pelvic_incidence = pelvic_incidence, 
                                              l1pa = checking_vpas$l1pa_value, 
                                              l2_s1 = current_l2_s1) %>%
    mutate(normal_l1_segment_angle = normal_l1_l2_function_t4l1pa(l1_pelvic_angle = l1pa,
                                                                  pelvic_incidence = pelvic_incidence, 
                                                                  l2_s1 = l2_s1, 
                                                                  t4_pelvic_angle = t4pa_goal)) %>%
    mutate(normal_t12_segment_angle = normal_t12_l1_function_t4l1pa(l1_pelvic_angle = l1pa,
                                                                    pelvic_incidence = pelvic_incidence, 
                                                                    l2_s1 = l2_s1, 
                                                                    t4_pelvic_angle = t4pa_goal)) %>%
    mutate(normal_t11_segment_angle = normal_t11_t12_function_t4l1pa(l1_pelvic_angle = l1pa,
                                                                     pelvic_incidence = pelvic_incidence, 
                                                                     l2_s1 = l2_s1, 
                                                                     t4_pelvic_angle = t4pa_goal)) %>%
    mutate(normal_t10_segment_angle = normal_t10_t11_function_t4l1pa(l1_pelvic_angle = l1pa,
                                                                     pelvic_incidence = pelvic_incidence, 
                                                                     l2_s1 = l2_s1, 
                                                                     t4_pelvic_angle = t4pa_goal)) %>%
    mutate(normal_t9_segment_angle = normal_t9_t10_function_t4l1pa(l1_pelvic_angle = l1pa,
                                                                   pelvic_incidence = pelvic_incidence, 
                                                                   l2_s1 = l2_s1, 
                                                                   t4_pelvic_angle = t4pa_goal)) %>%
    mutate(normal_t8_segment_angle = normal_t8_t9_function_t4l1pa(l1_pelvic_angle = l1pa,
                                                                  pelvic_incidence = pelvic_incidence, 
                                                                  l2_s1 = l2_s1, 
                                                                  t4_pelvic_angle = t4pa_goal)) %>%
    select(contains("segment_angle")) %>%
    pivot_longer(cols = everything(), names_to = "level", values_to = "normal_segment_angles") %>%
    mutate(level = str_remove_all(level, "normal_"))
  
  ############# NOW START ADJUSTING THE SEGMENT ANGLES ###############
  pso_candidate_list <- list()
  specific_tl_segments_list <- list()
  current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
  
  ##################### L1 #####################
  if (between(checking_vpas$t4pa_value, target_t4pa_range[1], target_t4pa_range[2]) == FALSE) {
    specific_tl_segments_list$prescribed_l1_l2 <- prescribe_l1_l2_function_t4l1pa(t4_pelvic_angle = target_t4pa,
                                                                                  l1_pelvic_angle = checking_vpas$l1pa_value, 
                                                                                  pelvic_incidence = pelvic_incidence)
    
    if (!("l1_segment" %in% rigid_levels)) {
      current_segment_angles_list$l1_segment_angle <- specific_tl_segments_list$prescribed_l1_l2[1] + l1_flexible_modifier
      
    } else if (between(current_segment_angles_list$l1_segment_angle, specific_tl_segments_list$prescribed_l1_l2 - 3, specific_tl_segments_list$prescribed_l1_l2 + 3)) {
      NA
    } else {
      pso_candidate_list$l1_segment <- "l1"
    }
    if("l1" %in% pso_level_input){
      current_segment_angles_list$l1_segment_angle <- specific_tl_segments_list$prescribed_l1_l2[1] + pso_modifier
    }
    checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pelvic_incidence,
                                                                     pt_value = 10,
                                                                     segment_angle_list = current_segment_angles_list)
    
    current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
  }
  
  ##################### T12  #####################
  if (between(checking_vpas$t4pa_value, target_t4pa_range[1], target_t4pa_range[2]) == FALSE) {
    specific_tl_segments_list$prescribed_t12_l1 <- prescribe_t12_l1_function_t4l1pa(t4_pelvic_angle = target_t4pa,
                                                                                    l1_pelvic_angle = checking_vpas$l1pa_value, 
                                                                                    pelvic_incidence = pelvic_incidence, 
                                                                                    l2_s1 = sum(unlist(current_segment_angles_list[1:4])), 
                                                                                    l1_l2 = current_segment_angles_list$l1_segment_angle)
    
    if (!("t12_segment" %in% rigid_levels)) {
      current_segment_angles_list$t12_segment_angle <- specific_tl_segments_list$prescribed_t12_l1[1] + t12_flexible_modifier
    } else if (between(current_segment_angles_list$t12_segment_angle, specific_tl_segments_list$prescribed_t12_l1 - 3, specific_tl_segments_list$prescribed_t12_l1 + 3)) {
      NA
    } else {
      pso_candidate_list$t12_segment <- "t12"
      if("t12" %in% pso_level_input){
        current_segment_angles_list$t12_segment_angle <- specific_tl_segments_list$prescribed_t12_l1[1] + pso_modifier
      }
    }
    
    checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pelvic_incidence,
                                                                     pt_value = 10,
                                                                     segment_angle_list = current_segment_angles_list)
    
    current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
  }
  
  ##################### T11  ##################### 
  if (between(checking_vpas$t4pa_value, target_t4pa_range[1], target_t4pa_range[2]) == FALSE) {
    specific_tl_segments_list$prescribed_t11_t12 <- prescribe_t11_t12_function_t4l1pa(t4_pelvic_angle = target_t4pa,
                                                                                      l1_pelvic_angle = checking_vpas$l1pa_value, 
                                                                                      pelvic_incidence = pelvic_incidence,   
                                                                                      t12_s1 = sum(unlist(current_segment_angles_list[1:6])))
    
    if (!("t11_segment" %in% rigid_levels)) {
      current_segment_angles_list$t11_segment_angle <- specific_tl_segments_list$prescribed_t11_t12[1] + t11_flexible_modifier
    } else if (between(current_segment_angles_list$t11_segment_angle, specific_tl_segments_list$prescribed_t11_t12 - 3, specific_tl_segments_list$prescribed_t11_t12 + 3)) {
      NA
    } else {
      pso_candidate_list$t11_segment <- "t11"
      if("t11" %in% pso_level_input){
        current_segment_angles_list$t11_segment_angle <- specific_tl_segments_list$prescribed_t11_t12[1] + pso_modifier
      }
    }
    checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pelvic_incidence,
                                                                     pt_value = 10,
                                                                     segment_angle_list = current_segment_angles_list)
    
    current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
  }
  
  ##################### T10  #####################
  if (between(checking_vpas$t4pa_value, target_t4pa_range[1], target_t4pa_range[2]) == FALSE) {
    specific_tl_segments_list$prescribed_t10_t11 <- prescribe_t10_t11_function_t4l1pa(t4_pelvic_angle = target_t4pa,
                                                                                      l1_pelvic_angle = checking_vpas$l1pa_value, 
                                                                                      pelvic_incidence = pelvic_incidence, 
                                                                                      t11_s1 = sum(unlist(current_segment_angles_list[1:7])))
    
    if (!("t10_segment" %in% rigid_levels)) {
      current_segment_angles_list$t10_segment_angle <- specific_tl_segments_list$prescribed_t10_t11[1] + t10_flexible_modifier
    } else if (between(current_segment_angles_list$t10_segment_angle, specific_tl_segments_list$prescribed_t10_t11 - 3, specific_tl_segments_list$prescribed_t10_t11 + 3)) {
      NA
    } else {
      pso_candidate_list$t10_segment <- "t10"
      if("t10" %in% pso_level_input){
        current_segment_angles_list$t10_segment_angle <- specific_tl_segments_list$prescribed_t10_t11[1] + pso_modifier
      }
    }
    checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pelvic_incidence,
                                                                     pt_value = 10,
                                                                     segment_angle_list = current_segment_angles_list)
    
    current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
  }
  
  ##################### T9  #####################
  if (between(checking_vpas$t4pa_value, target_t4pa_range[1], target_t4pa_range[2]) == FALSE) {
    specific_tl_segments_list$prescribed_t9_t10 <- prescribe_t9_t10_function_t4l1pa(t4_pelvic_angle = target_t4pa,
                                                                                    l1_pelvic_angle = checking_vpas$l1pa_value, 
                                                                                    pelvic_incidence = pelvic_incidence,
                                                                                    t10_t11 = current_segment_angles_list$t10_segment_angle,
                                                                                    t11_t12 = current_segment_angles_list$t11_segment_angle, 
                                                                                    t12_s1 = sum(unlist(current_segment_angles_list[1:6])))
    
    if (!("t9_segment" %in% rigid_levels)) {
      current_segment_angles_list$t9_segment_angle <- specific_tl_segments_list$prescribed_t9_t10[1]  + t9_flexible_modifier
    } else if (between(current_segment_angles_list$t9_segment_angle, specific_tl_segments_list$prescribed_t9_t10 - 3, specific_tl_segments_list$prescribed_t9_t10 + 3)) {
      NA
    } else {
      pso_candidate_list$t9_segment <- "t9"
      if("t9" %in% pso_level_input){
        current_segment_angles_list$t9_segment_angle <- specific_tl_segments_list$prescribed_t9_t10[1] + pso_modifier
      }
    }
    checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pelvic_incidence,
                                                                     pt_value = 10,
                                                                     segment_angle_list = current_segment_angles_list)
    
    current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
  }
  
  if (between(checking_vpas$t4pa_value, target_t4pa_range[1], target_t4pa_range[2]) == FALSE) {
    output_list$needs_pso <- TRUE
  } else {
    output_list$needs_pso <- FALSE
  }
  
  output_list$t4pa_positive_offset <- checking_vpas$t4pa_value - target_t4pa
  
  output_list$segment_angles_list <- current_segment_angles_list
  output_list$pso_candidate_list <- pso_candidate_list
  output_list$specific_tl_segments_list <- specific_tl_segments_list
  output_list$normal_expected_segment_angles_df <- normal_expected_segment_angles_df
  
  return(output_list)
}

######################################### BUILD FULL UPPER THORACIC UIV PLOT FUCTION   ###########################################

######################################### BUILD FULL UPPER THORACIC UIV PLOT FUCTION   ###########################################

######################################### BUILD FULL UPPER THORACIC UIV PLOT FUNCTION  ###########################################

######################################### BUILD FULL UPPER THORACIC UIV PLOT FUCTION   ###########################################

######################################### BUILD FULL UPPER THORACIC UIV PLOT FUCTION   ###########################################

######################################### BUILD FULL UPPER THORACIC UIV PLOT FUNCTION  ###########################################

######################################### BUILD FULL UPPER THORACIC UIV PLOT FUCTION   ###########################################

######################################### BUILD FULL UPPER THORACIC UIV PLOT FUCTION   ###########################################

######################################### BUILD FULL UPPER THORACIC UIV PLOT FUNCTION  ###########################################


build_upper_t_uiv_spine_plot_function <- function(pso_option_number = 1,
                                          preop_age, 
                                          preop_sex,
                                          preop_pelvic_incidence, 
                                          preop_pt, 
                                          preop_l1pa,
                                          preop_t9pa,
                                          preop_t4pa,
                                          preop_c2pa, 
                                          l1pa_line_color, 
                                          t4pa_line_color, 
                                          c2pa_line_color, 
                                          preop_segment_angles_input_list_reactive,
                                          preop_rigid_levels_vector_reactive
){
  
  ############################################### MATCHING ALIGNMENT STARTS ##############################################
  
  alignment_targets_list <- list()
  pso_list <- list()
  
  
  starting_pi <- preop_pelvic_incidence
  starting_segment_angles <- preop_segment_angles_input_list_reactive
  preop_c2_t1 <- compute_c2_t1_function(segment_angles_list = starting_segment_angles)
  preop_t1_t10 <- compute_t1_t10_function(segment_angles_list = starting_segment_angles)
  prescribed_t10_l2 <- prescribe_t10_l2_function(pelvic_incidence = starting_pi, 
                                                 c2_t1 = preop_c2_t1, 
                                                 t1_t10 = preop_t1_t10)
  
  prescribed_l1pa <- prescribe_l1pa_function(pelvic_incidence = starting_pi, 
                                             c2_t1 = preop_c2_t1, 
                                             t1_t10 = preop_t1_t10, 
                                             t10_l2 = prescribed_t10_l2)
  
  prescribed_segment_angles <- starting_segment_angles
  
  rigid_levels <- preop_rigid_levels_vector_reactive
  
  
  
  prescribed_l2_s1_list <- prescribe_l2_s1_segment_angles_function(segment_angles_list = starting_segment_angles, 
                                                                   pelvic_incidence = starting_pi, 
                                                                   rigid_segments = rigid_levels, 
                                                                   l1pa_target_range = c(prescribed_l1pa - 2, prescribed_l1pa + 2)
  )
  
  if(prescribed_l2_s1_list$needs_pso == "yes"){
    pso_options_list <- compute_pso_options_df(segment_angles_list = prescribed_l2_s1_list$new_segment_angles,
                                               pelvic_incidence = starting_pi, 
                                               pso_candidate_levels = prescribed_l2_s1_list$pso_candidates, 
                                               l1pa_goal = prescribed_l1pa, 
                                               l2_s1_goal = prescribed_l2_s1_list$l2_s1_goal)
    
    pso_options_df <- pso_options_list$pso_options_summary_df 
    
    ############ pso option ############
    if(nrow(pso_options_df)>1){
      pso_option <- pso_option_number
    }else{
      pso_option <- 1
    }
    
    pso_level <- str_to_upper(pso_options_df$pso_level[[pso_option]])
    
    
    pso_list$lumbar_pso <- pso_level
    
    pso_l2s1_segment_angles <- pso_options_list$new_segment_angles_by_pso_level_list[[paste0(pso_options_df$pso_level[[pso_option]], "_pso_segment_angles")]]
    
    prescribed_segment_angles$l5_segment_angle <- pso_l2s1_segment_angles$l5_segment_angle
    prescribed_segment_angles$l4_segment_angle <- pso_l2s1_segment_angles$l4_segment_angle
    prescribed_segment_angles$l3_segment_angle <- pso_l2s1_segment_angles$l3_segment_angle
    prescribed_segment_angles$l2_segment_angle <- pso_l2s1_segment_angles$l2_segment_angle
    
  }else{
    pso_level <- "na"
    prescribed_segment_angles$l5_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l5_segment_angle
    prescribed_segment_angles$l4_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l4_segment_angle
    prescribed_segment_angles$l3_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l3_segment_angle
    prescribed_segment_angles$l2_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l2_segment_angle
    
  }
  
  
  
  
  final_prescription_l1pa_calculated <- l1pa_computed_by_segment_angles_function(pelvic_incidence = starting_pi, 
                                                                                 l5_s1 = prescribed_segment_angles$l5_segment_angle, 
                                                                                 l4_l5 = prescribed_segment_angles$l4_segment_angle, 
                                                                                 l3_l4 = prescribed_segment_angles$l3_segment_angle, 
                                                                                 l2_l3 = prescribed_segment_angles$l2_segment_angle)
  
  final_l2_s1_prescription <- prescribed_segment_angles$l5_segment_angle + prescribed_segment_angles$l4_segment_angle +prescribed_segment_angles$l3_segment_angle +prescribed_segment_angles$l2_segment_angle
  
  alignment_targets_list$"L1PA" <- paste0(round(prescribed_l1pa -2, 0), "º to ", round(prescribed_l1pa +2, 0), "º")
  alignment_targets_list$"L2-S1" <- paste0(round(prescribed_l2_s1_list$l2_s1_goal - 3, 0), "º to ", round(prescribed_l2_s1_list$l2_s1_goal + 3, 0), "º")
  
  ######################## now compute the TL junction ###################################
  
  checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi,
                                                                   pt_value = 10,
                                                                   segment_angle_list = prescribed_segment_angles)
  current_c2_t1 <- sum(unlist(prescribed_segment_angles[18:23]))
  current_t1_t4 <- sum(unlist(prescribed_segment_angles[15:17]))
  current_l2_s1 <- sum(unlist(prescribed_segment_angles[1:4]))

  target_t4pa <- prescribing_t4pa_by_matching_function(l1_pelvic_angle = checking_vpas$l1pa_value,
                                                       c2_t1 = current_c2_t1, 
                                                       t1_t4 = current_t1_t4)
  
  predicted_c2_c7_lordosis <- predict_postop_c2_c7_function(rad_pre_c2_c7 = current_c2_t1,
                                final_t4pa_goal = target_t4pa,
                                starting_t4pa = preop_t4pa)
  
  prescribed_segment_angles$c2_segment_angle <- predicted_c2_c7_lordosis*c2_segment_weight
  prescribed_segment_angles$c3_segment_angle <- predicted_c2_c7_lordosis*c3_segment_weight
  prescribed_segment_angles$c4_segment_angle <- predicted_c2_c7_lordosis*c4_segment_weight
  prescribed_segment_angles$c5_segment_angle <- predicted_c2_c7_lordosis*c5_segment_weight
  prescribed_segment_angles$c6_segment_angle <- predicted_c2_c7_lordosis*c6_segment_weight
  prescribed_segment_angles$c7_segment_angle <- predicted_c2_c7_lordosis*c7_segment_weight
  
  alignment_targets_list$"T4PA" <- paste0(round(target_t4pa -2, 0), "º to ", round(target_t4pa +2, 0), "º")
  
  t4pa_positive_offset <- checking_vpas$t4pa_value - target_t4pa
  
    tl_segment_angles_modified_output_list <-  adjusting_tl_segment_angles_function(segment_angles_list_input = prescribed_segment_angles, 
                                                            pelvic_incidence_input = starting_pi,
                                                            rigid_levels_input = rigid_levels)
  
  ######## IDENTIFYING PSO LEVEL #######
  if(tl_segment_angles_modified_output_list$needs_pso){
    specific_segment_angles_df <- enframe( tl_segment_angles_modified_output_list$specific_tl_segments_list) %>%
      unnest(value) %>%
      mutate(name = str_remove_all(name, "prescribed_")) %>%
      separate(name, sep = "_", into = c("name", "distal")) %>%
      mutate(name = paste0(name, "_segment_angle")) %>%
      select(name, prescribed_segment_angle = value)
    
    pso_options_df <- enframe(tl_segment_angles_modified_output_list$pso_candidate_list) %>%
      unnest(value) %>%
      select(name, pso_level = value) %>%
      mutate(name = paste0(name, "_angle")) %>%
      left_join(enframe(tl_segment_angles_modified_output_list$segment_angles_list) %>%
                  unnest())  %>%
      left_join(specific_segment_angles_df) %>%
      rename(level = name) %>%
      left_join(tl_segments_t4pa_weight_df) %>%
      left_join(tl_segment_angles_modified_output_list$normal_expected_segment_angles_df) %>%
      mutate(normal_weighted_offset =(normal_segment_angles - value)*weight) %>%
      arrange(normal_weighted_offset) %>%
      mutate(pso_option = row_number()) %>%
      select(pso_option, level, pso_level, prescribed_segment_angle)
    
    tl_pso_level <- str_remove_all(pso_options_df$level[1], "_segment_angle")
    
    add_lordosis_modifier <- 0
    
    while (tl_segment_angles_modified_output_list$needs_pso == TRUE) {
    # while (abs(tl_segment_angles_modified_output_list$t4pa_positive_offset) >2) {
      if(tl_segment_angles_modified_output_list$t4pa_positive_offset > 0){
        add_lordosis_modifier <- add_lordosis_modifier + 1
      }else{
        add_lordosis_modifier <- add_lordosis_modifier -1
      }
      
      if(nrow(pso_options_df) >0){
        tl_segment_angles_modified_output_list <-  adjusting_tl_segment_angles_function(segment_angles_list_input = prescribed_segment_angles, 
                                                                                        pelvic_incidence_input = starting_pi, 
                                                                                        rigid_levels_input = rigid_levels, 
                                                                                        pso_level_input = tl_pso_level, 
                                                                                        pso_modifier = add_lordosis_modifier)
      }else{
        tl_segment_angles_modified_output_list <-  adjusting_tl_segment_angles_function(segment_angles_list_input = prescribed_segment_angles, 
                                                                                        pelvic_incidence_input = starting_pi, 
                                                                                        rigid_levels_input = rigid_levels, 
                                                                                        pso_level_input = tl_pso_level,
                                                                                          flexible_modifier = add_lordosis_modifier*-1)
        }
      
    }
    
    prescribed_segment_angles <- tl_segment_angles_modified_output_list$segment_angles_list
    
    if(nrow(pso_options_df) >0){
      pso_list$tl_pso <- tl_pso_level
    }
  }else{
    prescribed_segment_angles <- tl_segment_angles_modified_output_list$segment_angles_list
  }
    
  
  ############################################################ MATCHING ALIGNMENT ENDS #############################################
  ############################################################ MATCHING ALIGNMENT ENDS #############################################
  ############################################################ MATCHING ALIGNMENT ENDS #############################################
  ############################################################ MATCHING ALIGNMENT ENDS #############################################
  ############################################################ MATCHING ALIGNMENT ENDS #############################################
  
  new_vpa_list <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi, 
                                                                  pt_value = 10,
                                                                  segment_angle_list = prescribed_segment_angles)
  predicted_pt <- predict_postop_pt_function(preop_pt = preop_pt, 
                                             preop_c2_tilt = (preop_c2pa - preop_pt), 
                                             postop_c2pa = (new_vpa_list$c2pa_value))
  
  
  
  spine_prescribed_list <- build_full_spine_function_new(pelv_inc_value = starting_pi,
                                                         segment_angle_list = prescribed_segment_angles,
                                                         pt_value = predicted_pt[1],
                                                         spine_faces = "right",
                                                         # pso_levels = as_vector(pso_list)
                                                         pso_levels = str_to_upper(as_vector(pso_list))
  )
  
  
  
  spine_geoms_df <- spine_prescribed_list$spine_df %>%
    select(object, geom, geom_alpha)  
  
  lines_list <- list()
  
  lines_list$l1pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$l1pa_line_curve_sf,
                                                             color_input = l1pa_line_color, 
                                                             line_size = 1
  )
  
  lines_list$t4pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$t4pa_line_curve_sf,
                                                             color_input = t4pa_line_color,
                                                             line_size = 0.75
  )
  
  lines_list$c2pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$c2pa_line_extended_curve_sf,
                                                             color_input = c2pa_line_color,
                                                             line_size = 0.5
  )
  
  # preop_t4pa_l1pa_mismatch = preop_t4pa - preop_l1pa
  
  ############### DETERMINE UIV ###################
  postop_c2pa_t4pa_mismatch <- new_vpa_list$c2pa_value - new_vpa_list$t4pa_value
  
  if(postop_c2pa_t4pa_mismatch > 10){
    uiv_for_rod <- 2 ## for C2 UIV
    uiv_label <- "UIV: C2"
  }else if(sum(unlist(prescribed_segment_angles[12:22])) < -5){
    uiv_for_rod <- 9 ## for T2 UIV
    uiv_label <- "UIV: T2"
  }else{
    uiv_for_rod <- 11 ## for T4 UIV
    uiv_label <- "UIV: T4"
  }
  
  
  instrumented_vertebral_body_list <- spine_prescribed_list$vertebral_body_list[uiv_for_rod:length(spine_prescribed_list$vertebral_body_list)]
  
  sp_matrix_for_rod <- do.call(rbind, map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ rbind(instrumented_vertebral_body_list[[.x]]$sp)))
  
  rod_sf <- st_buffer(x = st_linestring(sp_matrix_for_rod), dist = 0.5, nQuadSegs = 5, joinStyle = "ROUND", endCapStyle = "ROUND")
  
  screw_list <- st_multipolygon(compact(map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ instrumented_vertebral_body_list[[.x]]$screw_sf)))
  
  ################# Plot labels ###############
  alignment_targets_df <- enframe(alignment_targets_list) %>%
    unnest() %>%
    mutate(x = -23) %>%
    mutate(y = c(seq(from = 26, by = -4, length = length(alignment_targets_list)))) %>%
    mutate(label = paste(name, value, sep = " = "))
  
  
  measurements_list <- list()
  measurements_list$"C2 Tilt" <- round(spine_prescribed_list$spine_list$c2_tilt_value)
  measurements_list$"C2PA" <- round(spine_prescribed_list$spine_list$c2pa_value, 0)
  measurements_list$"T1PA" <- round(spine_prescribed_list$spine_list$t1pa_value, 0)
  measurements_list$"T4PA" <- round(spine_prescribed_list$spine_list$t4pa_value, 0)
  measurements_list$"L1PA" <- round(spine_prescribed_list$spine_list$l1pa_value, 0)
  measurements_list$"LL" <-  round(spine_prescribed_list$spine_list$lumbar_lordosis, 0)
  measurements_list$"TK" <- round(spine_prescribed_list$spine_list$thoracic_kyphosis, 0)
  measurements_list$"PT" <- round(spine_prescribed_list$spine_list$pelvic_tilt, 0)
  
  measurements_df <- enframe(measurements_list) %>%
    unnest() %>%
    mutate(x = 18) %>%
    mutate(y = c(seq(from = 74, by = -4, length = length(measurements_list)-2), 7, 3)) %>%
    mutate(label = paste(name, value, sep = " = ")) 
  
  final_t10_l2 <- prescribed_segment_angles$l1_segment_angle + prescribed_segment_angles$t12_segment_angle + prescribed_segment_angles$t11_segment_angle + prescribed_segment_angles$t10_segment_angle
  
  segment_angles_list_for_plot <- list()
  segment_angles_list_for_plot$"T10-L2" <- as.character(round(final_t10_l2, 0))
  segment_angles_list_for_plot$"L1-L2" <- as.character(round(prescribed_segment_angles$l1_segment_angle, 0))
  segment_angles_list_for_plot$"L2-L3" <- as.character(round(prescribed_segment_angles$l2_segment_angle, 0))
  segment_angles_list_for_plot$"L3-L4" <- as.character(round(prescribed_segment_angles$l3_segment_angle, 0))
  segment_angles_list_for_plot$"L4-L5" <- as.character(round(prescribed_segment_angles$l4_segment_angle, 0))
  segment_angles_list_for_plot$"L5-S1" <- as.character(round(prescribed_segment_angles$l5_segment_angle, 0))
  

  if(length(pso_list) > 0){
    pso_label <- paste("PSO at", glue_collapse(str_to_upper(as_vector(pso_list)), sep = ", ", last = " & "))
  }else{
    pso_label <- " "
  }
  
  segment_angles_df <- enframe(segment_angles_list_for_plot) %>%
    unnest() %>%
    mutate(x = 18) %>%
    mutate(y = c(seq(from = 35, by = -4, length = length(segment_angles_list_for_plot)))) %>%
    mutate(label = paste(name, value, sep = " = "))
  
  
  pjk_risk <- pjk_risk_function(age = preop_age, 
                                sex = preop_sex, 
                                preop_c2pa_t9pa_mismatch = (preop_c2pa - preop_t9pa), 
                                pelvic_incidence = starting_pi, 
                                uiv_region = "Upper Thoracic",
                                postop_l1pa_undercorrection = new_vpa_list$l1pa_value - prescribed_l1pa, 
                                postop_t4l1pa_mismatch = new_vpa_list$t4pa_value - new_vpa_list$l1pa_value)
  
  
  pjk_risk_color <- case_when(
    pjk_risk > 0.45 ~ "red",
    between(pjk_risk, 0.35, 0.45) ~ "darkorange",
    between(pjk_risk, 0.2, 0.35) ~ "orange",
    pjk_risk < 0.2 ~ "darkgreen"
  )
  
  ggplot() +
    geom_sf(data = spine_geoms_df,
            aes(geometry = geom,
                alpha = geom_alpha
            ),
            color = "black",
            fill = "grey90") +
    geom_sf(data = st_multipolygon(x = screw_list), fill = "grey55") +
    geom_sf(data = rod_sf, fill = "grey55") +
    draw_text(text = measurements_df$label, x = measurements_df$x, y = measurements_df$y, size = 14) +
    draw_text(text = "Regional\nTargets", x = -23, y = max(alignment_targets_df$y)+7, size = 14, fontface = "bold") +
    draw_text(text = alignment_targets_df$label, x = alignment_targets_df$x, y = alignment_targets_df$y, size = 12) +
    draw_text(text = segment_angles_df$label, x = segment_angles_df$x, y = segment_angles_df$y, size = 14) +
    draw_text(text = uiv_label, x = -26, y = 56, size = 14, fontface = "bold") +
    draw_text(text = pso_label, x = -26, y = 12, size = 16, fontface = "bold", color = "blue") +
    draw_text(text = paste("PJK Risk = ", pjk_risk), x = 0, y = -5, size = 16, color = pjk_risk_color, fontface = "bold") +
    lines_list +
    xlim(-35, 32) +
    # ylim(-6, 110) +
    theme_void() +
    # labs(title = "T11 UIV") +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA)
    ) +
    scale_alpha_identity()
}
