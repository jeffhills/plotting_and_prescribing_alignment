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
  return(c(l2_s1 - 4, l2_s1 + 4))
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
  if(between(new_segment_angle_list$l5_segment_angle, l5_s1_target-4, l5_s1_target + 4) == FALSE){
    # pso_candidate_list$l5 <- "no"
    pso_candidate_vector <- append(pso_candidate_vector, "l5")
  }else{
    # pso_candidate_list$l5 <- "yes"
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
  if(between(new_segment_angle_list$l4_segment_angle, l4_l5_target-4, l4_l5_target + 4) == FALSE){
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
  if(between(new_segment_angle_list$l3_segment_angle, l3_l4_target-4, l3_l4_target + 4) == FALSE){
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
  if(between(new_segment_angle_list$l2_segment_angle, l2_l3_target-4, l2_l3_target + 4) == FALSE){
    # pso_candidate_list$l2 <- "no"
    pso_candidate_vector <- append(pso_candidate_vector, "l2")
  }else{
    # pso_candidate_list$l2 <- "yes"
  }
  target_segmental_alignment_list$l2_segment_angle <- l2_l3_target
  
  computed_new_l1pa <- l1pa_computed_by_segment_angles_function(pelvic_incidence = pelvic_incidence, 
                                                                l5_s1 = new_segment_angle_list$l5_segment_angle, 
                                                                l4_l5 = new_segment_angle_list$l4_segment_angle, 
                                                                l3_l4 = new_segment_angle_list$l3_segment_angle, 
                                                                l2_l3 = new_segment_angle_list$l2_segment_angle)
  
  new_l2_s1 <- new_segment_angle_list$l5_segment_angle + new_segment_angle_list$l4_segment_angle + new_segment_angle_list$l3_segment_angle+ new_segment_angle_list$l2_segment_angle
  
  if(between(computed_new_l1pa, l1pa_target_range[1], l1pa_target_range[2]) && between(new_l2_s1, l2_s1_goal_range[1], l2_s1_goal_range[2])){
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
prescribe_l1_l2_function <- function(l1_pelvic_angle = 3.86,l2_s1 = 55.36,t10_l2 = -0.17825,t1_t10 = -34.53485) {
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