jh_make_colored_line_geom_function <- function(sf_geom_input, 
                                               color_input = "darkgrey", 
                                               line_size = 1, 
                                               line_type = "solid"){
  geom_sf(data = sf_geom_input, 
          color = color_input, 
          fill = color_input, 
          linewidth = line_size,
          linetype = line_type,
          lineend="round", 
          linejoin="round") 
}


###########

predict_pt_function <- function(pelvic_incidence = 51.813768,t1_l1 = -40.878235,l1_s1 = 61.507721,l1_pelvic_angle = 3.99) {-0.65962719+0.66360172*pelvic_incidence-0.24004466*t1_l1-0.52660711*l1_s1+0.12344403*l1_pelvic_angle }


jh_compute_pelvic_angles_and_pt_function <- function(pelvic_incidence_start = 50,
                                                     segment_angle_list_start = segment_angle_list,
                                                     cervical_lordosis_start = 20, 
                                                     pso_input = c("")
){
  initial_spine_build_simulation_list <- build_full_spine_function_new(pelv_inc_value = pelvic_incidence_start,
                                                                       segment_angle_list = segment_angle_list_start, 
                                                                       cervical_lordosis = cervical_lordosis_start, 
                                                                       pt_value = 0, spine_faces = "right", pso_levels = pso_input)
  
  #C2PA
  c2pa_value <- initial_spine_build_simulation_list$spine_list$c2pa_value
  
  #T1PA
  t1pa_value <- initial_spine_build_simulation_list$spine_list$t1pa_value
  
  #T4PA
  t4pa_value <- initial_spine_build_simulation_list$spine_list$t4pa_value
  
  #T9PA
  t9pa_value <- initial_spine_build_simulation_list$spine_list$t9pa_value
  
  #L1PA
  l1pa_value <- initial_spine_build_simulation_list$spine_list$l1pa_value
  
  #L4PA
  l4pa_value <- initial_spine_build_simulation_list$spine_list$l4pa_value
  
  pt_predicted <- round(-0.023418739+0.83835713*c2pa_value+0.065802858*pelvic_incidence_start)
  # pt_predicted <- round(1.531932+0.77877765*c2pa_value+0.067338772*pelvic_incidence_start, 0)
  
  return(list(c2pa_value = c2pa_value,
              predicted_pt = pt_predicted,
              t1pa_value = t1pa_value,
              t4pa_value = t4pa_value,
              t9pa_value = t9pa_value,
              l1pa_value = l1pa_value,
              l4pa_value = l4pa_value
              # l1pa_value = initial_spine_build_simulation_list$spine_list$l1pa_value)
              )
  )
  
}

###### VERTEBRAL BODY BUILD FUNCTION

vertebral_body_build_function_new <- function(vertebral_slope, 
                                          inferior_vert_list, 
                                          endplate_width = 5, 
                                          endplate_height = 4, 
                                          wedge_body = TRUE, 
                                          build_lines = FALSE,
                                          line_down_length = 15,
                                          line_up_length =15, 
                                          horizontal_line_length = 15,
                                          horizontal_line_down_length = 12,
                                          horizontal_line_up_length = 10,
                                          posterior_lordosis_line_length = 30, 
                                          spine_facing = "right",
                                          c2_body = FALSE,
                                          pso = FALSE){
  
  spine_orientation <- if_else(spine_facing == "left", 1, -1)
  
  #inf list requires sl, sa, sp
  
  if(pso == TRUE){
    box_slope <- inferior_vert_list$sl
    
    body_a <- endplate_width * cos(box_slope) ## endplate width as a straight line on x
    body_b <- endplate_height * sin(box_slope) ## height as a straight line on y
    body_c <- endplate_height * cos(box_slope) ## this number correlates to the height
    body_d <- endplate_width * sin(box_slope) ## this number correlates to the width of the endplate
    
    vert_ip <- c(inferior_vert_list$sp[[1]] - spine_orientation*0.5 * sin(inferior_vert_list$sl), inferior_vert_list$sp[[2]] + 0.5 * cos(inferior_vert_list$sl))
    
    vert_ia <- c(inferior_vert_list$sa[[1]] - spine_orientation*0.5 * sin(inferior_vert_list$sl), inferior_vert_list$sa[[2]] + 0.5 * cos(inferior_vert_list$sl))
    vert_ia_w <- vert_ia
    vert_sa <- c(vert_ia[[1]]  - spine_orientation*body_b, vert_ia[[2]] + body_c)
    
    vert_sp <-  c(vert_sa[[1]] + spine_orientation*body_a, vert_sa[[2]] + body_d) 
    
    pso_cut_too_long_length <- vert_sa[[1]]  - vert_ip[[1]]
    pso_cut_height <- tan(vertebral_slope)*pso_cut_too_long_length
    post_box_line <- st_linestring(rbind(vert_sp, vert_ip))
    post_pso_line <- st_linestring(rbind(vert_sa, c(vert_sa[[1]] - pso_cut_too_long_length, vert_sa[[2]] + pso_cut_height)))
    
    vert_sp <- st_intersection(x = post_box_line, y = post_pso_line)
    if(st_is_empty(vert_sp)){
      vert_sp <- vert_ip
    }
    
    # vert_ia_w <- c(inferior_vert_list$sa[[1]] - spine_orientation*0.5 * sin(vertebral_slope), inferior_vert_list$sa[[2]] + 0.5 * cos(vertebral_slope))
    vert_body <- rbind(vert_ip, vert_ia, vert_sa, vert_sp, vert_ip) ## binds the corners to make a square
    
    screw_sf <- NULL
  }else{
    ### picture a normal square with a rotated square inside it
    body_a <- endplate_width * cos(vertebral_slope) ## endplate width as a straight line on x
    body_b <- endplate_height * sin(vertebral_slope) ## height as a straight line on y
    body_c <- endplate_height * cos(vertebral_slope) ## this number correlates to the height
    body_d <- endplate_width * sin(vertebral_slope) ## this number correlates to the width of the endplate
    
    ### start at the IP - making a 0.5 rectangle on top of the inferior body (essentially the disk)
    vert_ip <- c(inferior_vert_list$sp[[1]] - spine_orientation*0.5 * sin(inferior_vert_list$sl), inferior_vert_list$sp[[2]] + 0.5 * cos(inferior_vert_list$sl)) ## ip for inferior posterior corner
    
    ### then go to the IA - starting at the IP, go along the X axis the length of 'body_a', then down or up the length of body_d
    vert_ia <- c(vert_ip[[1]] - spine_orientation*body_a, vert_ip[[2]] - body_d) ## ia for inferior anterior corner
    ### then go to the SA - starting at the IA, go along the x axis the length of body_b, then up the height of body_c
    vert_sa <- c(vert_ia[[1]] - spine_orientation*body_b, vert_ia[[2]] + body_c) ## sa for superior anterior corner
    ## then to SP - starting at SA, go along x axis the length of body_a, then up or down the length of body_d
    vert_sp <- c(vert_sa[[1]] + spine_orientation*body_a, vert_sa[[2]] + body_d) ## sp for superior posterior corner, closing the vert
    vert_ia_w <- c(inferior_vert_list$sa[[1]] - spine_orientation*0.5 * sin(vertebral_slope), inferior_vert_list$sa[[2]] + 0.5 * cos(vertebral_slope))

    if(wedge_body == TRUE){
      vert_body <- rbind(vert_ip, vert_ia_w, vert_sa, vert_sp, vert_ip) ## binds the corners to make a square
    }else{
      vert_body <- rbind(vert_ip, vert_ia, vert_sa, vert_sp, vert_ip) ## binds the corners to make a square
    }
    anterior_screw_point_y <- ((vert_sa + vert_ia)/2)[2]
    anterior_screw_point_x <- vert_sp[1] - (vert_sp[1] - vert_sa[1])*0.9
    
    screw_sf <- st_buffer(x = st_linestring(rbind((vert_sp + vert_ip)/2,
                                                  c(anterior_screw_point_x, anterior_screw_point_y))), 
                          dist = 0.5, nQuadSegs = 1, mitreLimit = 3)
  }
  

  vert_body_sf <- st_polygon(list(vert_body)) ## convert to an sf figure
  vert_body_centroid_sf <- st_centroid(vert_body_sf)
  

  
  if(build_lines == TRUE){
    sup_endplate_parallel_line <-  st_linestring(rbind(vert_sp, # start at superior posterior corner of body and then --> end here:
                                                       c(vert_sp[[1]] - horizontal_line_length * cos(vertebral_slope), # X coord
                                                         vert_sp[[2]] - horizontal_line_length * sin(vertebral_slope)))) # Y coord
    
    inf_endplate_parallel_line <- st_linestring(rbind(vert_ip,  # start at inferior posterior corner of body and then --> end here:
                                                      c(vert_ip[[1]] - horizontal_line_length * cos(vertebral_slope),  
                                                        vert_ip[[2]] - horizontal_line_length * sin(vertebral_slope))))
    
    line_down <- st_linestring(rbind(vert_sp,  # start at superior posterior corner of body and then --> go here:
                                     c(vert_sp[[1]] - horizontal_line_down_length * cos(vertebral_slope),  # X - This XY coordinate extends a line parallel to endplate
                                       vert_sp[[2]] - horizontal_line_down_length * sin(vertebral_slope)), # Y - 
                                     c(vert_sp[[1]] - horizontal_line_down_length * cos(vertebral_slope) + sin(vertebral_slope)*line_down_length,  #X this starts at the prior point and then adds sin(vert_slope*lenght)
                                       vert_sp[[2]] - horizontal_line_down_length * sin(vertebral_slope) - cos(vertebral_slope)*line_down_length))) #Y - this XY coord ends the line 
    
    sp_up <- c(vert_sp[[1]], vert_sp[[2]] + 0.2) ## Creates a small distance so that you can see the line
    
    line_up <- st_linestring(rbind(sp_up,  # start at superior posterior corner of body and then --> go here:
                                   c(sp_up[[1]] - horizontal_line_up_length * cos(vertebral_slope),  # X coord
                                     sp_up[[2]] - horizontal_line_up_length * sin(vertebral_slope)), # Y
                                   c(sp_up[[1]] - horizontal_line_up_length * cos(vertebral_slope) - sin(vertebral_slope)*(line_up_length),  # X coord - this is the end of the line
                                     sp_up[[2]] - horizontal_line_up_length * sin(vertebral_slope) + cos(vertebral_slope)*(line_up_length)) # Y coord
    ))
    
    inf_endplate_line_up <- st_linestring(rbind(vert_ip,  # start at superior posterior corner of body and then --> go here:
                                                c(vert_ip[[1]] - horizontal_line_up_length * cos(vertebral_slope),  # X coord
                                                  vert_ip[[2]] - horizontal_line_up_length * sin(vertebral_slope)), # Y
                                                c(vert_ip[[1]] - horizontal_line_up_length * cos(vertebral_slope) - sin(vertebral_slope)*(line_up_length),  # X coord - this is the end of the line
                                                  vert_ip[[2]] - horizontal_line_up_length * sin(vertebral_slope) + cos(vertebral_slope)*(line_up_length)) # Y coord
    ))
    
    line_direction <- if_else(spine_facing == "right", -1, 1)
    
    sup_endplate_line_posterior_output <-  st_linestring(rbind(vert_sa, # start at superior anterior corner of body and then --> end here:
                                                               c(vert_sa[[1]] + line_direction*posterior_lordosis_line_length * cos(vertebral_slope), # X coord
                                                                 vert_sa[[2]] + posterior_lordosis_line_length * sin(vertebral_slope)))) # Y coord
    
    inf_endplate_line_posterior_output <-  st_linestring(rbind(vert_ia_w, # start at inferior anterior corner of body and then --> end here:
                                                               c(vert_ia_w[[1]] + line_direction*horizontal_line_length * cos(vertebral_slope), # X coord
                                                                 vert_ia_w[[2]] + horizontal_line_length * sin(vertebral_slope)))) # Y coord
    
  }else{
    sup_endplate_parallel_line <- NULL
    inf_endplate_parallel_line <- NULL
    line_down <- NULL
    line_up <- NULL
    inf_endplate_line_up <- NULL
    sup_endplate_line_posterior_output <- NULL
    inf_endplate_line_posterior_output <- NULL
  }
  
  if(c2_body == TRUE){
    
    ####
    dens_a <- endplate_width/2 * cos(vertebral_slope) ## endplate width
    dens_b <- endplate_height*2 * sin(vertebral_slope) ## height
    dens_c <- endplate_height*2 * cos(vertebral_slope) ## this number correlates to the height
    dens_d <- endplate_width/2 * sin(vertebral_slope) ## this number correlates to the width of the endplate
    
    dens_ip <- vert_body_centroid_sf ## ip for inferior posterior corner
    dens_ia <- c(dens_ip[[1]] - spine_orientation*dens_a, dens_ip[[2]] - dens_d/2) ## ia for inferior anterior corner
    dens_sa <- c(dens_ia[[1]] - spine_orientation*dens_b, dens_ia[[2]] + dens_c) ## sa for superior anterior corner
    dens_sp <- c(dens_sa[[1]] + spine_orientation*dens_a, dens_sa[[2]] + dens_d/2) ## sp for superior posterior corner, closing the vert

    dens <- rbind(dens_ip, dens_ia, dens_sa, dens_sp, dens_ip) ## binds the corners to make a square
    
    dens_sf <- st_polygon(list(dens)) 
    
    c2_with_dens <- st_union(vert_body_sf, dens_sf)
    
    dens_centroid <- st_centroid(dens_sf)
    
  }else{
    c2_with_dens <- NULL
    dens_centroid <- NULL
  }
  
  
  return(list(vert_body=vert_body, 
              vert_body_sf=vert_body_sf, 
              vert_body_center_sf=vert_body_centroid_sf, 
              sl = vertebral_slope, 
              sp = vert_sp, 
              sa = vert_sa, 
              ip = vert_ip, 
              ia = vert_ia,
              sup_endplate_line = sup_endplate_parallel_line,
              inf_endplate_line = inf_endplate_parallel_line, 
              vert_line_angle_down_sf = line_down,
              vert_line_angle_up_sf = line_up, 
              vert_line_inf_endplate_angle_up_sf = inf_endplate_line_up,
              sup_endplate_line_posterior = sup_endplate_line_posterior_output, 
              inf_endplate_line_posterior = inf_endplate_line_posterior_output, 
              screw_sf = screw_sf,
              c2_with_dens = c2_with_dens, 
              dens_centroid = dens_centroid))
}


################### BUILD THE SPINE ##########################

build_full_spine_function_new <- function(pelv_inc_value = 50, 
                                      pt_value = 10,
                                      segment_angle_list,
                                      planned_l1_pelvic_angle = 10,
                                      # l1_s1 = 60,
                                      cervical_lordosis = 20,
                                      l1l4_angle_line_length = 10,
                                      l4s1_angle_line_length = 10,
                                      posterior_line_length = 30,
                                      fem_head_center_x = 0, 
                                      spine_faces = "right",
                                      pso_levels = c("")
                                      ) {
  
  t1_l1 <- (segment_angle_list$t12_segment_angle +
                                               segment_angle_list$t11_segment_angle + 
                                               segment_angle_list$t10_segment_angle + 
                                               segment_angle_list$t9_segment_angle + 
                                               segment_angle_list$t8_segment_angle + 
                                               segment_angle_list$t7_segment_angle +
                                               segment_angle_list$t6_segment_angle + 
                                               segment_angle_list$t5_segment_angle + 
                                               segment_angle_list$t4_segment_angle + 
                                               segment_angle_list$t3_segment_angle +
                                               segment_angle_list$t2_segment_angle + 
                                               segment_angle_list$t1_segment_angle)
  
  spinal_kyphosis_input <- t1_l1
  
  l1_s1 <- (segment_angle_list$l5_segment_angle + segment_angle_list$l4_segment_angle + segment_angle_list$l3_segment_angle + segment_angle_list$l2_segment_angle + segment_angle_list$l1_segment_angle)
  
  if(pt_value == 99){
    pt_value <- predict_pt_function(pelvic_incidence = pelv_inc_value, t1_l1 = t1_l1, l1_s1 = l1_s1, l1_pelvic_angle = planned_l1_pelvic_angle)
    # pt_value <- predict_pt_by_c2pa_function(c2pa_value = , pelvic_incidence)
  }else{
    pt_value <- pt_value
  }

  spine_orientation <- if_else(spine_faces == "left", 1, -1)
  
  ss_value <- pelv_inc_value - pt_value
  
  p_inc <- (pi / 180) * (pelv_inc_value)
  pt <- (pi / 180) * (pt_value)
  ss <- (pi / 180) * (ss_value)
  
  
  l5s <- (pi / 180) * (ss_value  - segment_angle_list$l5_segment_angle)
  l4s <- (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + segment_angle_list$l4_segment_angle))
  l3s <- (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + segment_angle_list$l4_segment_angle + segment_angle_list$l3_segment_angle))
  l2s <- (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + segment_angle_list$l4_segment_angle + segment_angle_list$l3_segment_angle + segment_angle_list$l2_segment_angle))
  l1s <- (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + segment_angle_list$l4_segment_angle + segment_angle_list$l3_segment_angle + segment_angle_list$l2_segment_angle + segment_angle_list$l1_segment_angle))
  # l4s <- (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:2])))
  # l3s <- (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:3])))
  # l2s <- (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:4])))
  # l1s <- (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:5])))
  

  ###########################
  
  ## Starting point is at the center of the femoral heads, then build sacrum and go up.
  fem_head_center <- st_point(c(fem_head_center_x, 3)) # Center of femoral head
  fem_head_center_sf <- fem_head_center
  s1_mid <-  c(fem_head_center[[1]] + spine_orientation*15 * sin(pt),
               fem_head_center[[2]] + 15 * cos(pt))
  
  sac_inf <-  c(s1_mid[[1]] + spine_orientation*10 * sin(p_inc - pt), s1_mid[[2]] - 10 * cos(p_inc - pt))
  s1p <- c(s1_mid[[1]] + spine_orientation*2.5 * cos(ss), s1_mid[[2]] + 2.5 * sin(ss))
  s1a <- c(s1_mid[[1]] - spine_orientation*2.5 * cos(ss), s1_mid[[2]] - 2.5 * sin(ss))
  sacrum_sf <- st_polygon(list(rbind(sac_inf, s1p, s1a, sac_inf)))
  s1_mid_sf <- st_point(s1_mid)
  sac_inf_sf <- st_point(sac_inf)
  
  
  ############################################# LUMBAR ############################################
  ############################################# LUMBAR ############################################
  ############################################# LUMBAR ############################################
  ############################################# LUMBAR ############################################
  ############################################# LUMBAR ############################################


  s1_anterior_screw_point <-c(s1_mid[[1]] - spine_orientation*2.2 * cos((pi / 180) * (ss_value+20)), 
                              s1_mid[[2]] - 2.2 * sin((pi / 180) * (ss_value+20)))
  # screw_sf <- st_buffer(x = st_linestring(rbind(s1p, c(s1_anterior_screw_point_x, s1_anterior_screw_point_y))), dist = 0.5, nQuadSegs = 1, mitreLimit = 3)
  sacrum_list <- list(sl = ss, 
                      sa = s1a,
                      sp = s1p, 
                      screw_sf = st_buffer(x = st_linestring(rbind(s1p, s1_anterior_screw_point)), 
                                           dist = 0.5, 
                                           nQuadSegs = 1, 
                                           mitreLimit = 3))
  
  #inf list requires sl, sa, sp
  
  l4s1_horizontal_length <- 12
  horizontal_line_down_length <- 10
  

  l5_list <- vertebral_body_build_function_new(vertebral_slope = l5s, 
                                           inferior_vert_list = sacrum_list,
                                           endplate_width = 5, 
                                           endplate_height = 4, 
                                           wedge_body = FALSE,
                                           spine_facing = spine_faces, 
                                           pso = if_else(any(str_to_lower(pso_levels) == "l5"), TRUE, FALSE))

  l4_list <- vertebral_body_build_function_new(vertebral_slope = l4s, inferior_vert_list = l5_list, endplate_width = 5, endplate_height = 4, wedge_body = FALSE, spine_facing = spine_faces, 
                                           build_lines = TRUE, 
                                           line_down_length = l4s1_angle_line_length, 
                                           line_up_length = l1l4_angle_line_length, 
                                           horizontal_line_down_length = l4s1_horizontal_length, 
                                           horizontal_line_up_length = 9, 
                                           horizontal_line_length = 40, 
                                           posterior_lordosis_line_length = posterior_line_length, 
                                           pso = if_else(any(str_to_lower(pso_levels) == "l4"), TRUE, FALSE))
  
  l3_list <- vertebral_body_build_function_new(vertebral_slope = l3s, inferior_vert_list = l4_list, endplate_width = 5, endplate_height = 4, wedge_body = if_else(segment_angle_list$l3_segment_angle < 0, TRUE, FALSE), spine_facing = spine_faces, 
                                               pso = if_else(any(str_to_lower(pso_levels) == "l3"), TRUE, FALSE))
  
  l2_list <- vertebral_body_build_function_new(vertebral_slope = l2s, inferior_vert_list = l3_list, endplate_width = 5, endplate_height = 4, wedge_body = if_else(segment_angle_list$l2_segment_angle < 0, TRUE, FALSE), spine_facing = spine_faces, 
                                               pso = if_else(any(str_to_lower(pso_levels) == "l2"), TRUE, FALSE))
  
  l1_list <- vertebral_body_build_function_new(vertebral_slope = l1s, inferior_vert_list = l2_list, endplate_width = 5, endplate_height = 4, wedge_body = if_else(segment_angle_list$l1_segment_angle < 0, TRUE, FALSE), spine_facing = spine_faces, 
                                           build_lines = TRUE, 
                                           line_down_length = l1l4_angle_line_length, 
                                           horizontal_line_down_length = horizontal_line_down_length, 
                                           horizontal_line_length = 40, 
                                           posterior_lordosis_line_length = posterior_line_length, 
                                           pso = if_else(any(str_to_lower(pso_levels) == "l1"), TRUE, FALSE))
  
  
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  # segment_angle_list$l5_segment_angle + segment_angle_list$l4_segment_angle + segment_angle_list$l3_segment_angle + segment_angle_list$l2_segment_angle + segment_angle_list$l1_segment_angle
  t12s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                                segment_angle_list$l4_segment_angle + 
                                                segment_angle_list$l3_segment_angle + 
                                                segment_angle_list$l2_segment_angle + 
                                                segment_angle_list$l1_segment_angle +
                                                segment_angle_list$t12_segment_angle))
  t11s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                                segment_angle_list$l4_segment_angle + 
                                                segment_angle_list$l3_segment_angle + 
                                                segment_angle_list$l2_segment_angle + 
                                                segment_angle_list$l1_segment_angle +
                                                segment_angle_list$t12_segment_angle +
                                                segment_angle_list$t11_segment_angle))
  t10s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                                segment_angle_list$l4_segment_angle + 
                                                segment_angle_list$l3_segment_angle + 
                                                segment_angle_list$l2_segment_angle + 
                                                segment_angle_list$l1_segment_angle +
                                                segment_angle_list$t12_segment_angle +
                                                segment_angle_list$t11_segment_angle + 
                                                segment_angle_list$t10_segment_angle))
  t9s <-          (pi / 180) * (ss_value  -  (segment_angle_list$l5_segment_angle + 
                                                segment_angle_list$l4_segment_angle + 
                                                segment_angle_list$l3_segment_angle + 
                                                segment_angle_list$l2_segment_angle + 
                                                segment_angle_list$l1_segment_angle +
                                                segment_angle_list$t12_segment_angle +
                                                segment_angle_list$t11_segment_angle + 
                                                segment_angle_list$t10_segment_angle + 
                                                segment_angle_list$t9_segment_angle))
  t8s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                               segment_angle_list$l4_segment_angle + 
                                               segment_angle_list$l3_segment_angle + 
                                               segment_angle_list$l2_segment_angle + 
                                               segment_angle_list$l1_segment_angle +
                                               segment_angle_list$t12_segment_angle +
                                               segment_angle_list$t11_segment_angle + 
                                               segment_angle_list$t10_segment_angle + 
                                               segment_angle_list$t9_segment_angle + 
                                               segment_angle_list$t8_segment_angle))
  t7s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                               segment_angle_list$l4_segment_angle + 
                                               segment_angle_list$l3_segment_angle + 
                                               segment_angle_list$l2_segment_angle + 
                                               segment_angle_list$l1_segment_angle +
                                               segment_angle_list$t12_segment_angle +
                                               segment_angle_list$t11_segment_angle + 
                                               segment_angle_list$t10_segment_angle + 
                                               segment_angle_list$t9_segment_angle + 
                                               segment_angle_list$t8_segment_angle + 
                                               segment_angle_list$t7_segment_angle))
  t6s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                               segment_angle_list$l4_segment_angle + 
                                               segment_angle_list$l3_segment_angle + 
                                               segment_angle_list$l2_segment_angle + 
                                               segment_angle_list$l1_segment_angle +
                                               segment_angle_list$t12_segment_angle +
                                               segment_angle_list$t11_segment_angle + 
                                               segment_angle_list$t10_segment_angle + 
                                               segment_angle_list$t9_segment_angle + 
                                               segment_angle_list$t8_segment_angle + 
                                               segment_angle_list$t7_segment_angle +
                                               segment_angle_list$t6_segment_angle))
  t5s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                               segment_angle_list$l4_segment_angle + 
                                               segment_angle_list$l3_segment_angle + 
                                               segment_angle_list$l2_segment_angle + 
                                               segment_angle_list$l1_segment_angle +
                                               segment_angle_list$t12_segment_angle +
                                               segment_angle_list$t11_segment_angle + 
                                               segment_angle_list$t10_segment_angle + 
                                               segment_angle_list$t9_segment_angle + 
                                               segment_angle_list$t8_segment_angle + 
                                               segment_angle_list$t7_segment_angle +
                                               segment_angle_list$t6_segment_angle + 
                                               segment_angle_list$t5_segment_angle ))
  t4s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                               segment_angle_list$l4_segment_angle + 
                                               segment_angle_list$l3_segment_angle + 
                                               segment_angle_list$l2_segment_angle + 
                                               segment_angle_list$l1_segment_angle +
                                               segment_angle_list$t12_segment_angle +
                                               segment_angle_list$t11_segment_angle + 
                                               segment_angle_list$t10_segment_angle + 
                                               segment_angle_list$t9_segment_angle + 
                                               segment_angle_list$t8_segment_angle + 
                                               segment_angle_list$t7_segment_angle +
                                               segment_angle_list$t6_segment_angle + 
                                               segment_angle_list$t5_segment_angle + 
                                               segment_angle_list$t4_segment_angle))
  t3s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                               segment_angle_list$l4_segment_angle + 
                                               segment_angle_list$l3_segment_angle + 
                                               segment_angle_list$l2_segment_angle + 
                                               segment_angle_list$l1_segment_angle +
                                               segment_angle_list$t12_segment_angle +
                                               segment_angle_list$t11_segment_angle + 
                                               segment_angle_list$t10_segment_angle + 
                                               segment_angle_list$t9_segment_angle + 
                                               segment_angle_list$t8_segment_angle + 
                                               segment_angle_list$t7_segment_angle +
                                               segment_angle_list$t6_segment_angle + 
                                               segment_angle_list$t5_segment_angle + 
                                               segment_angle_list$t4_segment_angle + 
                                               segment_angle_list$t3_segment_angle))
  t2s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                               segment_angle_list$l4_segment_angle + 
                                               segment_angle_list$l3_segment_angle + 
                                               segment_angle_list$l2_segment_angle + 
                                               segment_angle_list$l1_segment_angle +
                                               segment_angle_list$t12_segment_angle +
                                               segment_angle_list$t11_segment_angle + 
                                               segment_angle_list$t10_segment_angle + 
                                               segment_angle_list$t9_segment_angle + 
                                               segment_angle_list$t8_segment_angle + 
                                               segment_angle_list$t7_segment_angle +
                                               segment_angle_list$t6_segment_angle + 
                                               segment_angle_list$t5_segment_angle + 
                                               segment_angle_list$t4_segment_angle + 
                                               segment_angle_list$t3_segment_angle +
                                               segment_angle_list$t2_segment_angle))
  t1s <-          (pi / 180) * (ss_value  - (segment_angle_list$l5_segment_angle + 
                                               segment_angle_list$l4_segment_angle + 
                                               segment_angle_list$l3_segment_angle + 
                                               segment_angle_list$l2_segment_angle + 
                                               segment_angle_list$l1_segment_angle +
                                               segment_angle_list$t12_segment_angle +
                                               segment_angle_list$t11_segment_angle + 
                                               segment_angle_list$t10_segment_angle + 
                                               segment_angle_list$t9_segment_angle + 
                                               segment_angle_list$t8_segment_angle + 
                                               segment_angle_list$t7_segment_angle +
                                               segment_angle_list$t6_segment_angle + 
                                               segment_angle_list$t5_segment_angle + 
                                               segment_angle_list$t4_segment_angle + 
                                               segment_angle_list$t3_segment_angle +
                                               segment_angle_list$t2_segment_angle + 
                                               segment_angle_list$t1_segment_angle))
  
  
  
  t12_list <- vertebral_body_build_function_new(vertebral_slope = t12s, inferior_vert_list = l1_list, endplate_width = 4.9, endplate_height = 3.8, wedge_body = if_else(segment_angle_list$t12_segment_angle < 0, TRUE, FALSE), build_lines = TRUE, spine_facing = spine_faces, 
                                                pso = if_else(any(str_to_lower(pso_levels) == "t12"), TRUE, FALSE))
  
  t11_list <- vertebral_body_build_function_new(vertebral_slope = t11s, inferior_vert_list = t12_list, endplate_width = 4.8, endplate_height = 3.7, wedge_body = if_else(segment_angle_list$t11_segment_angle < 0, TRUE, FALSE), spine_facing = spine_faces, 
                                                pso = if_else(any(str_to_lower(pso_levels) == "t11"), TRUE, FALSE))
  
  t10_list <- vertebral_body_build_function_new(vertebral_slope = t10s, inferior_vert_list = t11_list, endplate_width = 4.8, endplate_height = 3.6, wedge_body = if_else(segment_angle_list$t10_segment_angle < 0, TRUE, FALSE), spine_facing = spine_faces, 
                                                pso = if_else(any(str_to_lower(pso_levels) == "t10"), TRUE, FALSE))
  
  t9_list <- vertebral_body_build_function_new(vertebral_slope = t9s, inferior_vert_list = t10_list, endplate_width = 4.7, endplate_height = 3.4, wedge_body = TRUE, spine_facing = spine_faces)
  
  t8_list <- vertebral_body_build_function_new(vertebral_slope = t8s, inferior_vert_list = t9_list, endplate_width = 4.6, endplate_height = 3.3, wedge_body = TRUE, spine_facing = spine_faces)
  
  t7_list <- vertebral_body_build_function_new(vertebral_slope = t7s, inferior_vert_list = t8_list, endplate_width = 4.5, endplate_height = 3.2, wedge_body = TRUE, spine_facing = spine_faces)
  
  t6_list <- vertebral_body_build_function_new(vertebral_slope = t6s, inferior_vert_list = t7_list, endplate_width = 4.4, endplate_height = 3, wedge_body = TRUE, spine_facing = spine_faces)
  
  t5_list <- vertebral_body_build_function_new(vertebral_slope = t5s, inferior_vert_list = t6_list, endplate_width = 4.3, endplate_height = 2.8, wedge_body = TRUE, spine_facing = spine_faces)
  
  t4_list <- vertebral_body_build_function_new(vertebral_slope = t4s, inferior_vert_list = t5_list, endplate_width = 4.2, endplate_height = 2.6, wedge_body = TRUE, build_lines = TRUE, spine_facing = spine_faces)
  
  t3_list <- vertebral_body_build_function_new(vertebral_slope = t3s, inferior_vert_list = t4_list, endplate_width = 4.1, endplate_height = 2.4, wedge_body = TRUE, spine_facing = spine_faces)
  
  t2_list <- vertebral_body_build_function_new(vertebral_slope = t2s, inferior_vert_list = t3_list, endplate_width = 4, endplate_height = 2.2, wedge_body = TRUE, spine_facing = spine_faces)
  
  t1_list <- vertebral_body_build_function_new(vertebral_slope = t1s, inferior_vert_list = t2_list, endplate_width = 3.8, endplate_height = 2, wedge_body = TRUE, build_lines = TRUE, spine_facing = spine_faces)
  
  
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  
  spine_orientation <- if_else(spine_faces == "left", 1, -1)
  
  c7_segment_angle <- cervical_lordosis/6
  c6_segment_angle <- c7_segment_angle
  c5_segment_angle <- c7_segment_angle
  c4_segment_angle <- c7_segment_angle
  c3_segment_angle <- c7_segment_angle
  c2_segment_angle <- c7_segment_angle
  c1_segment_angle <- c7_segment_angle
  
  segment_angle_list <- append(segment_angle_list, c(c7_segment_angle, c6_segment_angle, c5_segment_angle, c4_segment_angle, c3_segment_angle, c2_segment_angle, c1_segment_angle))
  
  c7_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:18]))), 
                                           inferior_vert_list = t1_list,
                                           endplate_width = 3.6, 
                                           endplate_height = 1.5, 
                                           wedge_body = TRUE, build_lines = TRUE,
                                           spine_facing = spine_faces)
  c6_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:19]))),
                                           inferior_vert_list = c7_list,
                                           endplate_width = 3.4,
                                           endplate_height = 1.5,
                                           wedge_body = TRUE, 
                                           spine_facing = spine_faces)
  c5_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:20]))), 
                                           inferior_vert_list = c6_list,
                                           endplate_width = 3.2,
                                           endplate_height = 1.5, 
                                           wedge_body = TRUE, 
                                           spine_facing = spine_faces)
  c4_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:21]))), 
                                           inferior_vert_list = c5_list,
                                           endplate_width = 3,
                                           endplate_height = 1.5,
                                           wedge_body = TRUE,
                                           spine_facing = spine_faces)
  c3_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:22]))),
                                           inferior_vert_list = c4_list,
                                           endplate_width = 3,
                                           endplate_height = 1.5,
                                           wedge_body = TRUE,
                                           spine_facing = spine_faces)
  c2_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:23]))),
                                           inferior_vert_list = c3_list, 
                                           endplate_width = 3,
                                           endplate_height = 1.5, 
                                           wedge_body = TRUE, 
                                           build_lines = TRUE,
                                           spine_facing = spine_faces,
                                           c2_body = TRUE)
  c1_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:24]))),
                                           inferior_vert_list = c2_list, 
                                           endplate_width = 3.25, 
                                           endplate_height = 1.5,
                                           wedge_body = TRUE,
                                           spine_facing = spine_faces)
  
  head_center <- st_point(c(c1_list$sa[[1]] + spine_orientation*1, 
                            c1_list$sa[[2]] +4))
  
  skull <- st_linestring(x = rbind(c(head_center[[1]] -spine_orientation*1, head_center[[2]]-.25),
                                   c(head_center[[1]] +spine_orientation*1, head_center[[2]])))
  
  skull_sf <-  st_buffer(skull, dist = 5.5, endCapStyle = "ROUND")
  
  
  ## triangle:
  jaw <- st_polygon(list(rbind(c(head_center[[1]], head_center[[2]] -4), ## Most Right point
                               c(head_center[[1]] - spine_orientation*5.5, head_center[[2]] -1), ## top point
                               c(head_center[[1]] - spine_orientation*5.5, head_center[[2]]-8), ## bottom
                               c(head_center[[1]], head_center[[2]] -4)))) ## most right
  
  jaw_sf <-  st_buffer(jaw, dist = 1.2, endCapStyle = "ROUND")
  
  head_sf <- st_union(skull_sf, jaw_sf)
  
  
  ############################################# DRAW LINES ############################################
  ############################################# DRAW LINES ############################################
  ############################################# DRAW LINES ############################################
  ############################################# DRAW LINES ############################################
  ############################################# DRAW LINES ############################################
  # 
  # ## pelvic incidence
  # fem_head_s1_distance <- st_distance(x = s1_mid_sf, y = fem_head_center)
  # 
  # pelvic_incidence_line_sf <- st_linestring(rbind(fem_head_center,
  #                                                 s1_mid_sf, 
  #                                                 c(s1_mid[[1]] + spine_orientation*fem_head_s1_distance * sin(p_inc - pt),
  #                                                   s1_mid[[2]] - fem_head_s1_distance * cos(p_inc - pt))
  # ))
  # 
  # ############# COMPUTE PELVIC ANGLES AND GENERATE LINES   ############# 
  # #### L4PA #### 
  # fem_head_l4_distance <- st_distance(x = l4_list$vert_body_center_sf, y = fem_head_center)
  # 
  # fem_head_l4_vertical_distance <- st_distance(x = st_point(x = c(fem_head_center[1], l4_list$vert_body_center_sf[2])), y = fem_head_center)
  # 
  # if(spine_faces == "left"){
  #   l4_tilt_value <- if_else(l4_list$vert_body_center_sf[1] <= fem_head_center[1], 
  #                            abs(acos(fem_head_l4_vertical_distance/fem_head_l4_distance)*180/pi),
  #                            abs(acos(fem_head_l4_vertical_distance/fem_head_l4_distance)*180/pi)*-1)
  # }else{
  #   l4_tilt_value <- if_else(l4_list$vert_body_center_sf[1] >= fem_head_center[1], 
  #                            abs(acos(fem_head_l4_vertical_distance/fem_head_l4_distance)*180/pi),
  #                            abs(acos(fem_head_l4_vertical_distance/fem_head_l4_distance)*180/pi)*-1)
  # }
  # 
  # l4_tilt_line_sf <- st_linestring(rbind(fem_head_center, 
  #                                        l4_list$vert_body_center_sf,
  #                                        c(l4_list$vert_body_center_sf[1], fem_head_center[2])))
  # 
  # l4pa_line_sf <- st_linestring(rbind(l4_list$vert_body_center_sf,
  #                                     fem_head_center,
  #                                     s1_mid_sf))
  # 
  # l4pa_value <- l4_tilt_value + pt_value
  # 
  # #### L1PA #### 
  # fem_head_l1_distance <- st_distance(x = l1_list$vert_body_center_sf, y = fem_head_center)
  # 
  # fem_head_l1_vertical_distance <- st_distance(x = st_point(x = c(fem_head_center[1], l1_list$vert_body_center_sf[2])), y = fem_head_center)
  # 
  # if(spine_faces == "left"){
  #   l1_tilt_value <- if_else(l1_list$vert_body_center_sf[1] <= fem_head_center[1], 
  #                            abs(acos(fem_head_l1_vertical_distance/fem_head_l1_distance)*180/pi),
  #                            abs(acos(fem_head_l1_vertical_distance/fem_head_l1_distance)*180/pi)*-1)
  # }else{
  #   l1_tilt_value <- if_else(l1_list$vert_body_center_sf[1] >= fem_head_center[1], 
  #                            abs(acos(fem_head_l1_vertical_distance/fem_head_l1_distance)*180/pi),
  #                            abs(acos(fem_head_l1_vertical_distance/fem_head_l1_distance)*180/pi)*-1)
  # }
  # 
  # l1_tilt_line_sf <- st_linestring(rbind(fem_head_center, 
  #                                        l1_list$vert_body_center_sf,
  #                                        c(l1_list$vert_body_center_sf[1], fem_head_center[2])))
  # 
  # l1pa_line_sf <- st_linestring(rbind(l1_list$vert_body_center_sf,
  #                                    fem_head_center,
  #                                    s1_mid_sf))
  # 
  # l1pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
  #                                                      line_st_geometry = l1pa_line_sf,
  #                                                      distance_of_curve = st_length(st_linestring(rbind(l2_list$vert_body_center_sf,
  #                                                                                                        fem_head_center)))/2.5)
  # 
  # l1pa_value <- l1_tilt_value + pt_value
  # 
  # #### T9PA #### 
  # fem_head_t9_distance <- st_distance(x = t9_list$vert_body_center_sf, y = fem_head_center)
  # 
  # fem_head_t9_vertical_distance <- st_distance(x = st_point(x = c(fem_head_center[1], t9_list$vert_body_center_sf[2])), y = fem_head_center)
  # 
  # if(spine_faces == "left"){
  #   t9_tilt_value <- if_else(t9_list$vert_body_center_sf[1] <= fem_head_center[1], 
  #                            abs(acos(fem_head_t9_vertical_distance/fem_head_t9_distance)*180/pi),
  #                            abs(acos(fem_head_t9_vertical_distance/fem_head_t9_distance)*180/pi)*-1)
  # }else{
  #   t9_tilt_value <- if_else(t9_list$vert_body_center_sf[1] >= fem_head_center[1], 
  #                            abs(acos(fem_head_t9_vertical_distance/fem_head_t9_distance)*180/pi),
  #                            abs(acos(fem_head_t9_vertical_distance/fem_head_t9_distance)*180/pi)*-1)
  # }
  # 
  # t9_tilt_line_sf <- st_linestring(rbind(fem_head_center, 
  #                                        t9_list$vert_body_center_sf,
  #                                        c(t9_list$vert_body_center_sf[1], fem_head_center[2])))
  # 
  # t9pa_line_sf <- st_linestring(rbind(t9_list$vert_body_center_sf,
  #                                     fem_head_center,
  #                                     s1_mid_sf))
  # 
  # t9pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
  #                                                      line_st_geometry = t9pa_line_sf,
  #                                                      distance_of_curve = st_length(st_linestring(rbind(t11_list$vert_body_center_sf,
  #                                                                                                        fem_head_center)))/2.5)
  # 
  # t9pa_value <- t9_tilt_value + pt_value
  # 
  # #### T4PA #### 
  # fem_head_t4_distance <- st_distance(x = t4_list$vert_body_center_sf, y = fem_head_center_sf)
  # 
  # fem_head_t4_vertical_distance <- st_distance(x = st_point(x = c(fem_head_center_sf[1], t4_list$vert_body_center_sf[2])), y = fem_head_center_sf)
  # 
  # if(spine_faces == "left"){
  #   t4_tilt_value <- if_else(t4_list$vert_body_center_sf[1] <= fem_head_center_sf[1], 
  #                            abs(acos(fem_head_t4_vertical_distance/fem_head_t4_distance)*180/pi),
  #                            -1*abs(acos(fem_head_t4_vertical_distance/fem_head_t4_distance)*180/pi))
  # }else{
  #   t4_tilt_value <- if_else(t4_list$vert_body_center_sf[1] >= fem_head_center_sf[1], 
  #                            abs(acos(fem_head_t4_vertical_distance/fem_head_t4_distance)*180/pi),
  #                            -1*abs(acos(fem_head_t4_vertical_distance/fem_head_t4_distance)*180/pi))
  # }
  # t4_tilt_line_sf <- st_linestring(rbind(fem_head_center, 
  #                                        t4_list$vert_body_center_sf,
  #                                        c(t4_list$vert_body_center_sf[1], fem_head_center[2])))
  # 
  # 
  # t4pa_line_sf <- st_linestring(rbind(t4_list$vert_body_center_sf,
  #                                     fem_head_center,
  #                                     s1_mid_sf))
  # 
  # t4pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
  #                                                      line_st_geometry = t4pa_line_sf,
  #                                                      distance_of_curve = st_length(st_linestring(rbind(t11_list$vert_body_center_sf,
  #                                                                                                        fem_head_center)))/2.5)
  # 
  # t4pa_value <- t4_tilt_value + pt_value
  # 
  # # ############# T9PA ############
  # # fem_head_t9_distance <- st_distance(x = t9_list$vert_body_center_sf, y = fem_head_center_sf)
  # # 
  # # fem_head_t9_vertical_distance <- st_distance(x = st_point(x = c(fem_head_center_sf[1], t9_list$vert_body_center_sf[2])), y = fem_head_center_sf)
  # # 
  # # if(spine_faces == "left"){
  # #   t9_tilt_value <- if_else(t9_list$vert_body_center_sf[1] <= fem_head_center_sf[1], 
  # #                            abs(acos(fem_head_t9_vertical_distance/fem_head_t9_distance)*180/pi),
  # #                            -1*abs(acos(fem_head_t9_vertical_distance/fem_head_t9_distance)*180/pi))
  # # }else{
  # #   t9_tilt_value <- if_else(t9_list$vert_body_center_sf[1] >= fem_head_center_sf[1], 
  # #                            abs(acos(fem_head_t9_vertical_distance/fem_head_t9_distance)*180/pi),
  # #                            -1*abs(acos(fem_head_t9_vertical_distance/fem_head_t9_distance)*180/pi))
  # # }
  # # t9_tilt_line_sf <- st_linestring(rbind(fem_head_center, 
  # #                                        t9_list$vert_body_center_sf,
  # #                                        c(t9_list$vert_body_center_sf[1], fem_head_center[2])))
  # # 
  # # 
  # # t9pa_line_sf <- st_linestring(rbind(t9_list$vert_body_center_sf,
  # #                                    fem_head_center,
  # #                                    s1_mid_sf))
  # # 
  # # t9pa_value <- t9_tilt_value + pt_value
  # 
  # 
  # #### T1PA ####
  # fem_head_t1_distance <- st_distance(x = t1_list$vert_body_center_sf, y = fem_head_center_sf)
  # 
  # fem_head_t1_vertical_distance <- st_distance(x = st_point(x = c(fem_head_center_sf[1], t1_list$vert_body_center_sf[2])), y = fem_head_center_sf)
  # 
  # if(spine_faces == "left"){
  #   t1_tilt_value <- if_else(t1_list$vert_body_center_sf[1] <= fem_head_center_sf[1], 
  #                            abs(acos(fem_head_t1_vertical_distance/fem_head_t1_distance)*180/pi),
  #                            -1*abs(acos(fem_head_t1_vertical_distance/fem_head_t1_distance)*180/pi))
  # }else{
  #   t1_tilt_value <- if_else(t1_list$vert_body_center_sf[1] >= fem_head_center_sf[1], 
  #                            abs(acos(fem_head_t1_vertical_distance/fem_head_t1_distance)*180/pi),
  #                            -1*abs(acos(fem_head_t1_vertical_distance/fem_head_t1_distance)*180/pi))
  # }
  # t1_tilt_line_sf <- st_linestring(rbind(fem_head_center, 
  #                                        t1_list$vert_body_center_sf,
  #                                        c(t1_list$vert_body_center_sf[1], fem_head_center[2])))
  # 
  # t1pa_line_sf <- st_linestring(rbind(t1_list$vert_body_center_sf,
  #                                     fem_head_center,
  #                                     s1_mid_sf))
  # 
  # t1pa_line_sf <- st_linestring(rbind(t1_list$vert_body_center_sf,
  #                                     fem_head_center,
  #                                     s1_mid_sf))
  # 
  # t1pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
  #                                                      line_st_geometry = t1pa_line_sf,
  #                                                      distance_of_curve = st_length(st_linestring(rbind(t10_list$vert_body_center_sf,
  #                                                                                                        fem_head_center)))/2.5)
  # 
  # t1pa_value <- t1_tilt_value + pt_value
  # 
  # 
  # 
  # #### C2PA ####
  # fem_head_c2_distance <- st_distance(x = c2_list$dens_centroid, y = fem_head_center_sf)
  # 
  # fem_head_c2_vertical_distance <- st_distance(x = st_point(x = c(fem_head_center_sf[1], c2_list$dens_centroid[2])), y = fem_head_center_sf)
  # 
  # if(spine_faces == "left"){
  #   c2_tilt_value <- if_else(c2_list$dens_centroid[1] <= fem_head_center_sf[1], 
  #                            abs(acos(fem_head_c2_vertical_distance/fem_head_c2_distance)*180/pi),
  #                            -1*abs(acos(fem_head_c2_vertical_distance/fem_head_c2_distance)*180/pi))
  # }else{
  #   c2_tilt_value <- if_else(c2_list$dens_centroid[1] >= fem_head_center_sf[1], 
  #                            abs(acos(fem_head_c2_vertical_distance/fem_head_c2_distance)*180/pi),
  #                            -1*abs(acos(fem_head_c2_vertical_distance/fem_head_c2_distance)*180/pi))
  # }
  # 
  # 
  # c2_tilt_line_sf <- st_linestring(rbind(fem_head_center, 
  #                                        c2_list$dens_centroid,
  #                                        c(c2_list$dens_centroid[1], fem_head_center[2])))
  # 
  # c2pa_line_sf <- st_linestring(rbind(c2_list$dens_centroid,
  #                                    fem_head_center,
  #                                    s1_mid_sf))
  # 
  # pt_run <- fem_head_center[[1]] - s1_mid_sf[[1]]
  # pt_rise <- fem_head_center[[2]] - s1_mid_sf[[2]]
  # extended_c2pa_line_point <- c((fem_head_center[[1]] - pt_run*2.5), (fem_head_center[[2]] - pt_rise*2.5))
  # 
  # extended_c2pa_line_sf <- st_linestring(rbind(extended_c2pa_line_point,
  #                                           fem_head_center, 
  #                                           c2_list$dens_centroid)
  # )
  # 
  # extended_c2pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
  #                                                               line_st_geometry = extended_c2pa_line_sf,
  #                                                               distance_of_curve = st_length(st_linestring(rbind(extended_c2pa_line_point,
  #                                                                                                                 fem_head_center)))/1.5)
  # 
  # c2pa_value <- c2_tilt_value + pt_value
  # 
  # 
  # ####
  # # C2 VERT HIP ANGLES
  t1_c2_ha_line_sf <- st_linestring(rbind(t1_list$vert_body_center_sf,
                                          c2_list$dens_centroid,
                                          fem_head_center))

  t9_c2_ha_line_sf <- st_linestring(rbind(t9_list$vert_body_center_sf,
                                          c2_list$dens_centroid,
                                          fem_head_center))
  # 
  # 
  # ## PT
  # pt_line_sf <- st_linestring(rbind(fem_head_center,
  #                                   s1_mid_sf,
  #                                   c(s1_mid[1], fem_head_center[2])
  # ))
  # 
  # 
  # ## ss
  # ss_anterior_inferior <- c(s1_mid[[1]] - spine_orientation*10 * cos(ss), s1_mid[[2]] - 10 * sin(ss))
  # 
  # ss_line_sf <- st_linestring(rbind(c(ss_anterior_inferior[1] - 5, s1p[2]), 
  #                                   s1p,
  #                                   ss_anterior_inferior))
  # 
  # 
  # s1_endplate_line_posterior <-  st_linestring(rbind(s1a, # start at superior posterior corner of body and then --> end here:
  #                                                    c(s1a[[1]] + spine_orientation*st_length(l1_list$sup_endplate_line_posterior) * cos(ss), s1a[[2]] + st_length(l1_list$sup_endplate_line_posterior)* sin(ss))
  #                                                    # c(s1p[[2]], # X coord
  #                                                    #   s1a[[2]] + (s1p[[2]] - s1a[[1]])*tan(ss)))
  # ))
  # 
  # # l1_s1_value <- 
  # l1s1_line_sf_1 <- l1_list$sup_endplate_line_posterior
  # l1s1_line_sf_2 <- s1_endplate_line_posterior
  # 
  # l4s1_line_sf_1 <- l4_list$sup_endplate_line_posterior
  # l4s1_line_sf_2 <- s1_endplate_line_posterior
  # 
  # ############### TK Lines
  # 
  # t4_line_sf <- t4_list$sup_endplate_line
  # 
  # t12_line_sf <- t12_list$inf_endplate_line
  # 
  # ############### Cervical Lines
  # 
  # c2_line_sf <- c2_list$inf_endplate_line_posterior
  # 
  # c7_line_sf <- c7_list$inf_endplate_line_posterior
  lines_list <- list()
  
  #####   ##### SPINOPELVIC MEASURES  #####   ##### 
  ##### Pelvic Incidence #####
  fem_head_s1_distance <- st_distance(x = s1_mid_sf, y = fem_head_center)
  
  lines_list$pelvic_incidence_line_sf <- st_linestring(rbind(fem_head_center,
                                                             s1_mid_sf, 
                                                             c(s1_mid[[1]] + spine_orientation*fem_head_s1_distance * sin(p_inc - pt),
                                                               s1_mid[[2]] - fem_head_s1_distance * cos(p_inc - pt))
  ))
  
  lines_list$pelvic_incidence_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = s1_mid_sf, 
                                                                              line_st_geometry = lines_list$pelvic_incidence_line_sf,
                                                                              distance_of_curve = st_length(st_linestring(rbind(s1_mid_sf,
                                                                                                                                fem_head_center)))/2)
  
  ##### PT #####
  lines_list$pt_line_sf <- st_linestring(rbind(fem_head_center,
                                               s1_mid_sf,
                                               c(s1_mid[1], fem_head_center[2])
  ))
  
  lines_list$pt_line_up_sf <- st_linestring(rbind(s1_mid_sf,
                                                  fem_head_center,
                                                  c(fem_head_center[1], s1_mid[2])
  ))
  
  lines_list$pt_line_up_curve_sf <- jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
                                                                 line_st_geometry = lines_list$pt_line_up_sf,
                                                                 distance_of_curve = st_length(st_linestring(rbind(fem_head_center,
                                                                                                                   s1_mid_sf)))/1.5)
  
  ##### SS #####
  ss_anterior_inferior <- c(s1_mid[[1]] - spine_orientation*10 * cos(ss), s1_mid[[2]] - 10 * sin(ss))
  
  lines_list$ss_line_sf <- st_linestring(rbind(c(ss_anterior_inferior[1] - 5, s1p[2]), 
                                               s1p,
                                               ss_anterior_inferior))
  
  
  
  
  #####   ##### VERTEBRAL PELVIC ANGLES  #####   ##### 
  
  ############# L4PA ############
  l4_tilt_value <- jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                  fem_head_center_input = fem_head_center, 
                                                                                  vertebral_body_list = l4_list)
  
  lines_list$l4pa_line_sf <- st_linestring(rbind(l4_list$vert_body_center_sf,
                                                 fem_head_center,
                                                 s1_mid_sf))
  
  lines_list$l4pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
                                                                  line_st_geometry = lines_list$l4pa_line_sf,
                                                                  distance_of_curve = st_length(st_linestring(rbind(l4_list$vert_body_center_sf,
                                                                                                                    fem_head_center)))/2.5)
  
  l4pa_value <- l4_tilt_value + pt_value
  
  
  ############# L1PA ############
  l1_tilt_value <- jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                  fem_head_center_input = fem_head_center, 
                                                                                  vertebral_body_list = l1_list)
  
  lines_list$l1pa_line_sf <- st_linestring(rbind(l1_list$vert_body_center_sf,
                                                 fem_head_center,
                                                 s1_mid_sf))
  
  lines_list$l1pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
                                                                  line_st_geometry = lines_list$l1pa_line_sf,
                                                                  distance_of_curve = st_length(st_linestring(rbind(l2_list$vert_body_center_sf,
                                                                                                                    fem_head_center)))/3)
  
  l1pa_value <- l1_tilt_value + pt_value
  
  ############# T9PA ############
  t9_tilt_value <- jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                  fem_head_center_input = fem_head_center, 
                                                                                  vertebral_body_list = t9_list)
  
  lines_list$t9pa_line_sf <- st_linestring(rbind(t9_list$vert_body_center_sf,
                                                 fem_head_center,
                                                 s1_mid_sf))
  
  lines_list$t9pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
                                                                  line_st_geometry = lines_list$t9pa_line_sf,
                                                                  distance_of_curve = st_length(st_linestring(rbind(l1_list$vert_body_center_sf,
                                                                                                                    fem_head_center)))/3)
  
  t9pa_value <- t9_tilt_value + pt_value
  
  ############# T4PA ############
  
  t4_tilt_value <- jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                  fem_head_center_input = fem_head_center, 
                                                                                  vertebral_body_list = t4_list)
  
  lines_list$t4pa_line_sf <- st_linestring(rbind(t4_list$vert_body_center_sf,
                                                 fem_head_center,
                                                 s1_mid_sf))
  
  lines_list$t4pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
                                                                  line_st_geometry = lines_list$t4pa_line_sf,
                                                                  distance_of_curve = st_length(st_linestring(rbind(t12_list$vert_body_center_sf,
                                                                                                                    fem_head_center)))/3)
  
  t4pa_value <- t4_tilt_value + pt_value
  
  
  ############# T1PA ############
  
  t1_tilt_value <- jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                  fem_head_center_input = fem_head_center, 
                                                                                  vertebral_body_list = t1_list)
  
  lines_list$t1pa_line_sf <- st_linestring(rbind(t1_list$vert_body_center_sf,
                                                 fem_head_center,
                                                 s1_mid_sf))
  
  lines_list$t1pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
                                                                  line_st_geometry = lines_list$t1pa_line_sf,
                                                                  distance_of_curve = st_length(st_linestring(rbind(t11_list$vert_body_center_sf,
                                                                                                                    fem_head_center)))/3)
  
  t1pa_value <- t1_tilt_value + pt_value
  
  ############# C2PA & C2 Tilt############
  
  c2_tilt_value <- jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                  fem_head_center_input = fem_head_center, 
                                                                                  vertebral_body_list = c2_list)
  
  lines_list$c2_tilt_line_sf <- st_linestring(rbind(fem_head_center, 
                                                    c2_list$dens_centroid,
                                                    c(c2_list$dens_centroid[1], fem_head_center[2])))
  
  lines_list$c2_tilt_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = c2_list$dens_centroid, 
                                                                line_st_geometry = lines_list$c2_tilt_line_sf,
                                                                distance_of_curve = st_length(st_linestring(rbind(c2_list$dens_centroid,
                                                                                                                  fem_head_center)))/2.5)
  
  lines_list$c2_tilt_line_up_sf <- st_linestring(rbind(c2_list$dens_centroid,
                                                       fem_head_center, 
                                                       c(fem_head_center[1], c2_list$dens_centroid[2])))
  
  lines_list$c2_tilt_up_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
                                                                   line_st_geometry = lines_list$c2_tilt_line_up_sf,
                                                                   distance_of_curve = st_length(st_linestring(rbind(c2_list$dens_centroid,
                                                                                                                     fem_head_center)))/2.5)
  
  
  
  lines_list$c2pa_line_sf <- st_linestring(rbind(c2_list$dens_centroid,
                                                 fem_head_center,
                                                 s1_mid_sf))
  
  lines_list$c2pa_line_curve_sf <-   jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
                                                                  line_st_geometry = lines_list$c2pa_line_sf,
                                                                  distance_of_curve = st_length(st_linestring(rbind(t11_list$vert_body_center_sf,
                                                                                                                    fem_head_center)))/3)
  
  c2pa_value <- c2_tilt_value + pt_value
  
  
  pt_run <- fem_head_center[[1]] - s1_mid_sf[[1]]
  
  pt_rise <- fem_head_center[[2]] - s1_mid_sf[[2]]
  
  extended_c2pa_line_point <- c((fem_head_center[[1]] - pt_run*2.5), (fem_head_center[[2]] - pt_rise*2.5))
  
  lines_list$c2pa_line_extended_sf <- st_linestring(rbind(extended_c2pa_line_point,
                                                          fem_head_center, 
                                                          c2_list$dens_centroid)
  )
  
  lines_list$c2pa_line_extended_curve_sf <- jh_plot_angle_curve_function(vertex_vector = fem_head_center, 
                                                                         line_st_geometry = lines_list$c2pa_line_extended_sf,
                                                                         distance_of_curve = st_length(st_linestring(rbind(extended_c2pa_line_point,
                                                                                                                           fem_head_center)))/1.5)
  
  
  
  ### ### SAGITTAL COBBS ### ###
  
  s1_endplate_line_posterior <-  st_linestring(rbind(s1a, # start at superior posterior corner of body and then --> end here:
                                                     c(s1a[[1]] + spine_orientation*st_length(l1_list$sup_endplate_line_posterior) * cos(ss), s1a[[2]] + st_length(l1_list$sup_endplate_line_posterior)* sin(ss))
                                                     # c(s1p[[2]], # X coord
                                                     #   s1a[[2]] + (s1p[[2]] - s1a[[1]])*tan(ss)))
  ))
  
  lines_list$l1s1_line_sf_1 <- l1_list$sup_endplate_line_posterior
  lines_list$l1s1_line_sf_2 <- s1_endplate_line_posterior
  
  lines_list$l4s1_line_sf_1 <- l4_list$sup_endplate_line_posterior
  lines_list$l4s1_line_sf_2 <- s1_endplate_line_posterior
  
  ############### TK Lines
  
  lines_list$t4_line_sf <- st_crop(x = st_geometry(t4_list$sup_endplate_line_posterior), xmin = -20, xmax = 50, ymin = 0, ymax = 100)
  
  lines_list$t12_line_sf <- t12_list$inf_endplate_line_posterior
  
  ############### Cervical Lines
  
  lines_list$c2_line_sf <- c2_list$inf_endplate_line_posterior
  
  lines_list$c7_line_sf <- c7_list$inf_endplate_line_posterior
  
  ############## BODY CENTERS
  t1_center_circle_sf <- st_buffer(x = t1_list$vert_body_center_sf,
                                   dist = 1,
                                   endCapStyle = "ROUND")
  
  l1_center_circle_sf <- st_buffer(x = l1_list$vert_body_center_sf,
                                   dist = 1,
                                   endCapStyle = "ROUND")
  
  s1_center_circle_sf <- st_buffer(x = s1_mid_sf,
                                   dist = 1,
                                   endCapStyle = "ROUND")
  
  
  fem_center_circle_sf <- st_buffer(x = fem_head_center,
                                    dist = 1,
                                    endCapStyle = "ROUND")
  
  fem_head_sf <-  st_buffer(fem_head_center,
                            dist = 2,
                            endCapStyle = "ROUND")
  # 
  
  
  
  #############################################
  
  lines_list <- map(.x = lines_list,
                    .f = ~ st_geometry(.x))
  
  spine_list <- list(pelvic_incidence =  pelv_inc_value, 
                     pelvic_tilt =   pt_value,
                     lumbar_lordosis =  l1_s1,
                     l4pa_value =  l4pa_value,
                     l1pa_value =  l1pa_value,
                     t9pa_value = t9pa_value,
                     t4pa_value = t4pa_value,
                     t1pa_value =  t1pa_value,
                     c2pa_value = c2pa_value,
                     c2_tilt_value = c2_tilt_value,
                     thoracic_kyphosis =   spinal_kyphosis_input,
                     cervical_lordosis =  cervical_lordosis,
                     # pi_line_sf = st_geometry(pelvic_incidence_line_sf),
                     # pt_line_sf = st_geometry(pt_line_sf),
                     # ss_line_sf = st_geometry(ss_line_sf),
                     # l1s1_line_sf_1 = st_geometry(l1s1_line_sf_1),
                     # l1s1_line_sf_2 = st_geometry(l1s1_line_sf_2),
                     # l4s1_line_sf_1 = st_geometry(l4s1_line_sf_1),
                     # l4s1_line_sf_2 = st_geometry(l4s1_line_sf_2),
                     # t4_line_sf = st_geometry(st_zm(x = st_buffer(x = t4_line_sf, dist = 0.2, endCapStyle = "ROUND", joinStyle = "ROUND"))),
                     # t12_line_sf = st_geometry(st_zm(x = st_buffer(x = t12_line_sf, dist = 0.2, endCapStyle = "ROUND", joinStyle = "ROUND"))),
                     # c2_line_sf = st_geometry(st_zm(x = st_buffer(x = c2_line_sf, dist = 0.2, endCapStyle = "ROUND", joinStyle = "ROUND"))),
                     # c7_line_sf = st_geometry(st_zm(x = st_buffer(x = c7_line_sf, dist = 0.2, endCapStyle = "ROUND", joinStyle = "ROUND"))),
                     ### VPAs
                     # l1pa_line_sf = st_geometry(l1pa_line_sf),
                     # l1_tilt_line_sf = st_geometry(l1_tilt_line_sf),
                     # l1pa_line_curve_sf = st_geometry(l1pa_line_curve_sf),
                     # t9pa_line_sf = st_geometry(t9pa_line_sf),
                     # t9pa_line_curve_sf = st_geometry(t9pa_line_curve_sf),
                     # t4pa_line_sf = st_geometry(t4pa_line_sf),
                     # t4pa_line_curve_sf = st_geometry(t4pa_line_curve_sf),
                     # t4_tilt_line_sf = st_geometry(t4_tilt_line_sf),
                     # t1pa_line_sf = st_geometry(t1pa_line_sf),
                     # t1pa_line_curve_sf = st_geometry(t1pa_line_curve_sf),
                     # t1_tilt_line_sf = st_geometry(t1_tilt_line_sf),
                     # c2pa_line_sf = st_geometry(c2pa_line_sf),
                     # extended_c2pa_line_sf = st_geometry(extended_c2pa_line_sf),
                     # extended_c2pa_line_curve_sf = st_geometry(extended_c2pa_line_curve_sf),
                     # c2_tilt_line_sf = st_geometry(c2_tilt_line_sf),
                     #HA's
                     t1_c2_ha_line_sf = st_geometry(t1_c2_ha_line_sf),
                     t9_c2_ha_line_sf = st_geometry(t9_c2_ha_line_sf),
                     fem_head_geom = st_geometry(st_zm(x = fem_head_sf)),
                     t1_center_circle_sf = st_geometry(st_zm(x = t1_center_circle_sf)),
                     l1_center_circle_sf = st_geometry(st_zm(x = l1_center_circle_sf)),
                     s1_center_circle_sf = st_geometry(st_zm(x = s1_center_circle_sf)),
                     fem_center_circle_sf = st_geometry(st_zm(x = fem_center_circle_sf)),
                     sacrum_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = sacrum_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     l5_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = l5_list$vert_body_sf, dist = -0.8, endCapStyle = "ROUND"), dist = 0.8, endCapStyle = "ROUND"))),
                     l4_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = l4_list$vert_body_sf, dist = -0.7, endCapStyle = "ROUND"), dist = 0.7, endCapStyle = "ROUND"))),
                     l3_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = l3_list$vert_body_sf, dist = -0.7, endCapStyle = "ROUND"), dist = 0.7, endCapStyle = "ROUND"))),
                     l2_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = l2_list$vert_body_sf, dist = -0.6, endCapStyle = "ROUND"), dist = 0.6, endCapStyle = "ROUND"))),
                     l1_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = l1_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t12_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t12_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t11_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t11_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t10_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t10_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t9_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t9_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t8_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t8_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t7_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t7_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t6_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t6_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t5_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t5_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t4_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t4_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t3_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t3_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t2_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t2_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     t1_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t1_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     c7_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c7_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     c6_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c6_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     c5_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c5_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     c4_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c4_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     c3_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c3_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     c2_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c2_list$c2_with_dens, dist = -0.2, endCapStyle = "ROUND"), dist = 0.2, endCapStyle = "ROUND"))),
                     c1_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c1_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     head_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = head_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND")))
  )
  
  spine_geoms_df <- tibble(
    # pelvic_incidence =  pelv_inc_value,
                           # pelvic_tilt =   pt_value,
                           # l1_pelvic_angle =  l1pa_value,
                           # lumbar_lordosis =  l1_s1,
                           # t1_pelvic_angle =  t1pa_value,
                           # thoracic_kyphosis =   spinal_kyphosis_input,
                           # cervical_lordosis =  cervical_lordosis,
                           # lumbar_spine_color = lumbar_color,
                           # thoracic_spine_color = thoracic_color,pt_line_sf
                           # pelvic_incidence_line_sf = st_geometry(st_zm(x = st_buffer(pelvic_incidence_line_sf, dist = 0.15, endCapStyle = "ROUND"))),
                           # pt_line_sf = st_geometry(st_zm(x = st_buffer(pt_line_sf, dist = 0.15, endCapStyle = "ROUND"))),
                           # ss_line_sf = st_geometry(st_zm(x = st_buffer(ss_line_sf, dist = 0.15, endCapStyle = "ROUND"))),
                           # l1s1_line_sf_1 = st_geometry(st_zm(x = st_buffer(x = l1s1_line_sf_1, dist = 0.2, endCapStyle = "ROUND", joinStyle = "ROUND"))),
                           # l1s1_line_sf_2 = st_geometry(st_zm(x = st_buffer(x = l1s1_line_sf_2, dist = 0.2, endCapStyle = "ROUND", joinStyle = "ROUND"))),
                           # t4_line_sf = st_geometry(st_zm(x = st_buffer(x = t4_line_sf, dist = 0.2, endCapStyle = "ROUND", joinStyle = "ROUND"))),
                           # t12_line_sf = st_geometry(st_zm(x = st_buffer(x = t12_line_sf, dist = 0.2, endCapStyle = "ROUND", joinStyle = "ROUND"))),
                           # c2_line_sf = st_geometry(st_zm(x = st_buffer(x = c2_line_sf, dist = 0.2, endCapStyle = "ROUND", joinStyle = "ROUND"))),
                           # c7_line_sf = st_geometry(st_zm(x = st_buffer(x = c7_line_sf, dist = 0.2, endCapStyle = "ROUND", joinStyle = "ROUND"))),
                           # tpa_line_sf = st_geometry(st_zm(x = st_buffer(tpa_line_sf, dist = 0.25, endCapStyle = "ROUND"))),
                           # lpa_line_sf = st_geometry(st_zm(x = st_buffer(lpa_line_sf, dist = 0.25, endCapStyle = "ROUND"))),
                           # t1_tilt_line_sf = st_geometry(t1_tilt_line_sf),
                           # l1_tilt_line_sf = st_geometry(l1_tilt_line_sf),
                           # c2pa_line_sf = st_geometry(c2pa_line_sf),
                           # c2_tilt_line_sf = st_geometry(c2_tilt_line_sf),
                           # t1pa_line_sf = st_geometry(t1pa_line_sf),
                           # t1_tilt_line_sf = st_geometry(t1_tilt_line_sf),
                           # t4pa_line_sf = st_geometry(t4pa_line_sf),
                           # t4_tilt_line_sf = st_geometry(t4_tilt_line_sf),
                           # l1pa_line_sf = st_geometry(l1pa_line_sf),
                           # l1_tilt_line_sf = st_geometry(l1_tilt_line_sf),
                           fem_head_geom = st_geometry(st_zm(x = fem_head_sf)),
                           # t1_center_circle_sf = st_geometry(st_zm(x = t1_center_circle_sf)),
                           # l1_center_circle_sf = st_geometry(st_zm(x = l1_center_circle_sf)),
                           # s1_center_circle_sf = st_geometry(st_zm(x = s1_center_circle_sf)),
                           # fem_center_circle_sf = st_geometry(st_zm(x = fem_center_circle_sf)),
                           sacrum_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = sacrum_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           l5_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = l5_list$vert_body_sf, dist = -0.8, endCapStyle = "ROUND"), dist = 0.8, endCapStyle = "ROUND"))),
                           l4_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = l4_list$vert_body_sf, dist = -0.7, endCapStyle = "ROUND"), dist = 0.7, endCapStyle = "ROUND"))),
                           l3_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = l3_list$vert_body_sf, dist = -0.7, endCapStyle = "ROUND"), dist = 0.7, endCapStyle = "ROUND"))),
                           l2_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = l2_list$vert_body_sf, dist = -0.6, endCapStyle = "ROUND"), dist = 0.6, endCapStyle = "ROUND"))),
                           l1_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = l1_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t12_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t12_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t11_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t11_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t10_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t10_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t9_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t9_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t8_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t8_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t7_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t7_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t6_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t6_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t5_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t5_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t4_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t4_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t3_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t3_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t2_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t2_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           t1_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = t1_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           c7_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c7_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           c6_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c6_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           c5_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c5_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           c4_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c4_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           c3_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c3_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           c2_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c2_list$c2_with_dens, dist = -0.3, endCapStyle = "ROUND"), dist = 0.3, endCapStyle = "ROUND"))),
                           c1_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c1_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                           head_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = head_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND")))
                           ) %>%
    pivot_longer(cols = contains("geom"), names_to = "object", values_to = "geom")%>%
    mutate(geom_alpha = case_when(
      object == "head_geom" ~ 0.7, 
      object == "c1_geom" ~ 0.5,
      TRUE ~ 1
    ))
  

  iliac_screw_point <- st_centroid(st_buffer(st_linestring(rbind(fem_head_center,
                                                          s1_mid_sf)), dist = 1))
  ilium_list <- list(sp = s1p, 
                     screw_sf = st_buffer(x = st_linestring(rbind(s1p, iliac_screw_point)), dist = 0.5, nQuadSegs = 1, mitreLimit = 3))
  
  vertebral_body_list <- list(c1_list = c1_list, 
                              c2_list = c2_list,
                              c3_list = c3_list,
                              c4_list = c4_list,
                              c5_list = c5_list,
                              c6_list = c6_list,
                              c7_list = c7_list,
                              t1_list = t1_list,
                              t2_list = t2_list,
                              t3_list = t3_list,
                              t4_list = t4_list,
                              t5_list = t5_list,
                              t6_list = t6_list,
                              t7_list = t7_list,
                              t8_list = t8_list,
                              t9_list = t9_list,
                              t10_list = t10_list,
                              t11_list = t11_list,
                              t12_list = t12_list,
                              l1_list = l1_list,
                              l2_list = l2_list,
                              l3_list = l3_list,
                              l4_list = l4_list,
                              l5_list = l5_list,
                              sacrum_list = sacrum_list,
                              ilium_list = ilium_list)
  
  
  return(list(spine_list = spine_list, 
              spine_df = spine_geoms_df, 
              lines_list = lines_list,
              vertebral_body_list = vertebral_body_list))
  
}

