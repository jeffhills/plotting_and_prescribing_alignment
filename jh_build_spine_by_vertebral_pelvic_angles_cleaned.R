library(sf)
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

jh_building_spine_compute_vertebral_tilt_by_direction_function <- function(spine_faces_input, fem_head_center_input, vertebral_body_list){
  if(is.null(vertebral_body_list$dens_centroid)){
    vertebral_body_center <- vertebral_body_list$vert_body_center_sf
  }else{
    vertebral_body_center <- vertebral_body_list$dens_centroid
  }
  
  fem_head_vert_distance <- st_distance(x = vertebral_body_center, y = fem_head_center_input)
  
  fem_head_vert_vertical_distance <- st_distance(x = st_point(x = c(fem_head_center_input[1], vertebral_body_center[2])), 
                                                 y = fem_head_center_input)
  
  
  if(spine_faces_input == "left"){
    tilt_value <- if_else(vertebral_body_center[1] <= fem_head_center_input[1],
                          abs(acos(fem_head_vert_vertical_distance/fem_head_vert_distance)*180/pi),
                          abs(acos(fem_head_vert_vertical_distance/fem_head_vert_distance)*180/pi)*-1)
  }else{
    tilt_value <- if_else(vertebral_body_center[1] >= fem_head_center_input[1],
                          abs(acos(fem_head_vert_vertical_distance/fem_head_vert_distance)*180/pi),
                          abs(acos(fem_head_vert_vertical_distance/fem_head_vert_distance)*180/pi)*-1)
  }
  tilt_value
}


jh_plot_angle_curve_function <- function(line_st_geometry = NULL, 
                                         vertex_vector = c(0,0),
                                         distance_of_curve = 7 
){
  angle_curve_geom <-  st_intersection(x = st_convex_hull(line_st_geometry), 
                                       y = st_multilinestring(st_buffer(st_point(x = vertex_vector), 
                                                                        dist = distance_of_curve)))
  
  angle_curve_sf <- st_multilinestring(list(st_coordinates(line_st_geometry),
                                            st_coordinates(angle_curve_geom)))
  
  angle_curve_sf
  
}



####################################################################################


segment_angle_function_using_lpa_tpa <- function(l1_pelvic_angle_input = 10,
                                                 pelvic_incidence_input = 50,
                                                 l1_s1_lordosis_input = 50,
                                                 pt_input = 20,
                                                 t4_t12_input = 25,
                                                 t4_pelvic_angle_input = 10
){
  
  lumbar_segment_angles_list <- lumbar_segment_angle_function(l1_s1_input = l1_s1_lordosis_input, l1_pelvic_angle_input = l1_pelvic_angle_input, pelvic_incidence_input = pelvic_incidence_input)
  
  thoracic_segment_angles_list <- thoracic_segment_angles_function(l1_s1_input = l1_s1_lordosis_input, l1_pelvic_angle_input = l1_pelvic_angle_input, pelvic_incidence_input = pelvic_incidence_input, t4_t12_input = t4_t12_input, t4_pelvic_angle_input = t4_pelvic_angle_input)
  
  l1_segment_angle <- lumbar_segment_angles_list$l1_segment_angle
  l2_segment_angle <- lumbar_segment_angles_list$l2_segment_angle
  l3_segment_angle <- lumbar_segment_angles_list$l3_segment_angle
  l4_segment_angle <- lumbar_segment_angles_list$l4_segment_angle
  l5_segment_angle <- lumbar_segment_angles_list$l5_segment_angle
  
  segment_angle_list <-
    list(
      l5_segment_angle = l5_segment_angle,
      l4_segment_angle = l4_segment_angle,
      l3_segment_angle = l3_segment_angle,
      l2_segment_angle = l2_segment_angle,
      l1_segment_angle = l1_segment_angle,
      t12_segment_angle = thoracic_segment_angles_list$t12_segment_angle,
      t11_segment_angle = thoracic_segment_angles_list$t11_segment_angle,
      t10_segment_angle = thoracic_segment_angles_list$t10_segment_angle,
      t9_segment_angle = thoracic_segment_angles_list$t9_segment_angle,
      t8_segment_angle = thoracic_segment_angles_list$t8_segment_angle,
      t7_segment_angle = thoracic_segment_angles_list$t7_segment_angle,
      t6_segment_angle = thoracic_segment_angles_list$t6_segment_angle,
      t5_segment_angle = thoracic_segment_angles_list$t5_segment_angle,
      t4_segment_angle = thoracic_segment_angles_list$t4_segment_angle,
      t3_segment_angle = thoracic_segment_angles_list$t3_segment_angle,
      t2_segment_angle = thoracic_segment_angles_list$t2_segment_angle,
      t1_segment_angle = thoracic_segment_angles_list$t1_segment_angle
    )
  
  return(segment_angle_list = segment_angle_list)
  
}

predict_pt_function <- function(pelvic_incidence = 51.813768,t1_l1 = -40.878235,l1_s1 = 61.507721,l1_pelvic_angle = 3.99) {-0.65962719+0.66360172*pelvic_incidence-0.24004466*t1_l1-0.52660711*l1_s1+0.12344403*l1_pelvic_angle }

predict_pt_by_c2pa_function <- function(c2pa_value, pelvic_incidence){
  round(-0.023418739+0.83835713*c2pa_value+0.065802858*c2pa_value, 1)
}

jh_compute_pelvic_angles_and_pt_function <- function(pelvic_incidence_start = 50,
                                                     segment_angle_list_start = segment_angle_list,
                                                     # l1pa_start = 10,
                                                     cervical_lordosis_start = 20, 
                                                     pso_input = c("")
){
  # initial_spine_build_simulation_list <- build_full_spine_function_new(pelv_inc_value = pelvic_incidence_start,
  #                                                                      segment_angle_list = segment_angle_list_start, 
  #                                                                      # planned_l1_pelvic_angle = l1pa_start, 
  #                                                                      cervical_lordosis = cervical_lordosis_start, 
  #                                                                      pt_value = 0, 
  #                                                                      spine_faces = "right", 
  #                                                                      pso_levels = pso_input)
  
  initial_spine_build_simulation_list <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pelvic_incidence_start, pt_value = 0, segment_angle_list = segment_angle_list_start)
  
  pt_predicted <- round(-0.023418739+0.83835713*initial_spine_build_simulation_list$spine_list$c2pa_value+0.065802858*pelvic_incidence_start)
  # pt_predicted <- round(1.531932+0.77877765*c2pa_value+0.067338772*pelvic_incidence_start, 0)
  
  return(list(predicted_pt = pt_predicted,
              l1pa_value = initial_spine_build_simulation_list$spine_list$l1pa_value,
              t4pa_value = initial_spine_build_simulation_list$spine_list$t4pa_value,
              tpa_value = initial_spine_build_simulation_list$spine_list$t1pa_value,
              c2pa_value = initial_spine_build_simulation_list$spine_list$c2pa_value))
  
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
    
    screw_sf <- st_buffer(x = (st_linestring(rbind((vert_sp + vert_ip)/2,
                                                   c(anterior_screw_point_x, anterior_screw_point_y)))) - c(0.5,-0.5), 
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
    
    # dens <- rbind(dens_ip, dens_ia, dens_sa, dens_sp, dens_ip) ## binds the corners to make a square
    
    dens <- rbind(vert_ip, vert_ia, dens_sa, dens_sp, dens_ip, vert_sp, vert_ip) ## binds the corners to make a square
    
    # vert_body <- rbind(vert_ip, vert_ia, vert_sa, vert_sp, vert_ip) ## binds the corners to make a square
    
    dens_sf <- st_polygon(list(dens)) 
    
    c2_with_dens <- dens_sf
    
    # c2_with_dens <- st_union(vert_body_sf, dens_sf)
    
    dens_centroid <- st_centroid(st_polygon(list(rbind(dens_ip, dens_ia, dens_sa, dens_sp, dens_ip))) )
    
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


######################################## BUILDING FULL SPINE BY VERTEBRAL PELVIC ANGLES ONLY ######################################## 
######################################## BUILDING FULL SPINE BY VERTEBRAL PELVIC ANGLES ONLY ######################################## 
######################################## BUILDING FULL SPINE BY VERTEBRAL PELVIC ANGLES ONLY ######################################## 
######################################## BUILDING FULL SPINE BY VERTEBRAL PELVIC ANGLES ONLY ######################################## 
######################################## BUILDING FULL SPINE BY VERTEBRAL PELVIC ANGLES ONLY ######################################## 
######################################## BUILDING FULL SPINE BY VERTEBRAL PELVIC ANGLES ONLY ######################################## 

################## First need to ESTIMATE what the Segment Angles Are #################

# estimate pt 
## DEFINE PT PREDICTION FUNCTION
pt_by_c2pa_pi_function <- function(c2pa_input = 19.706756,pelvic_incidence_input = 55.207959) {
  round(-0.023418739+0.83835713*rad_6w_c2pa+0.065802858*rad_6w_s1pi, 1)
}

## estimate L1s1 and TK & CL
estimate_l1s1_by_l1pa_t9pa_function <- function(rad_l1pa = 8.5386609,rad_t9pa = 7.7816796,rad_pi = 54.458408) {
  0.90456185+0.97354738*rad_l1pa-2.375636*rad_t9pa+1.1267077*rad_pi 
}

jh_estimate_l1s1_from_pi_l1pa_t9pa_function <- function(pelvic_incidence = 51.66,
                                                        l1_pelvic_angle = 3.86,
                                                        t9_pelvic_angle = 0.8) {
  -6.2738793 + 1.2314381 * pelvic_incidence + 0.76770609 * l1_pelvic_angle -
    2.2902174 * t9_pelvic_angle
}

estimate_tk_by_pelvic_angles_function <-
  function(rad_l1pa = 8.5386609,
           rad_t9pa = 7.7816796,
           rad_t4pa = 13.186254,
           rad_c2pa = 20.321441) {
    (-6.1237937 - 2.5691366 * rad_l1pa + 7.2495706 * rad_t9pa - 2.5211443 * rad_t4pa -
       1.9875228 * rad_c2pa)*-1
  }

# estimate_cervical_lordosis_by_pelvic_angles_function <- function(rad_t4pa = 13.186254,
#                                                                  rad_c2pa = 20.321441) {
#   (-10.121229-2.8580181*rad_t4pa+2.8455292*rad_c2pa)*1
# }

estimate_cervical_lordosis_by_pelvic_angles_function <- function(rad_t9pa = 18.201023,
                                                                 rad_t4pa = 19.872244,
                                                                 rad_c2pa = 25.037304) {
  -7.8442273-0.78207256*rad_t9pa-0.77342221*rad_t4pa+1.7021151*rad_c2pa 
  }

################## Now that you can estimate LL, TK and CL, compute ESTIMATED LUMBAR, THORACIC, and CERVICAL SEGMENT ANGLES #################

lumbar_segment_angle_function <- function(l1_s1_input = 39.5,
                                          l1_pelvic_angle_input = 11.222271,
                                          pelvic_incidence_input = 55.435164){ 
  l5_sa <- 2.028759-0.2004133*l1_s1_input-1.2948269*l1_pelvic_angle_input+0.72916193*pelvic_incidence_input 
  
  l4_sa <- -4.2323446-0.14264788*l1_s1_input-0.66483692*l5_sa-1.6659845*l1_pelvic_angle_input+1.0011084*pelvic_incidence_input
  
  l3_sa <- -5.6312247+0.098489688*l1_s1_input-0.91616568*l5_sa-0.53156915*l4_sa-1.4696836*l1_pelvic_angle_input+0.92315489*pelvic_incidence_input
  
  l2_sa <- -2.3078228+0.47785577*l1_s1_input-0.80732669*l5_sa-0.73245566*l4_sa-0.51444405*l3_sa-0.62688324*l1_pelvic_angle_input+0.38754186*pelvic_incidence_input
  l1_sa <- 1.4066407e-13+1*l1_s1_input-1*l5_sa-1*l4_sa-1*l3_sa-1*l2_sa-5.0984901e-16*l1_pelvic_angle_input+2.1368709e-16*pelvic_incidence_input
  
  lumbar_segment_angles_list <- list(l5_segment_angle = l5_sa,
                                     l4_segment_angle = l4_sa,
                                     l3_segment_angle = l3_sa,
                                     l2_segment_angle = l2_sa,
                                     l1_segment_angle = l1_sa)
  return(lumbar_segment_angles_list)
}

thoracic_segment_angles_function <- function(l1_s1_input = 39.5,l1_pelvic_angle_input = 11.222271,pelvic_incidence_input = 55.435164,t4_t12_input = -32.7,t4_pelvic_angle_input = 19.369452) {
  t4_t12_input <- t4_t12_input*-1
  t12_sa <- -1.2432187-0.56338566*l1_s1_input+0.87843878*l1_pelvic_angle_input+0.5103392*pelvic_incidence_input-0.21307175*t4_t12_input-1.3178337*t4_pelvic_angle_input
  
  t11_sa <- -0.83916975-0.29121935*t12_sa-0.61968425*l1_s1_input+0.71116474*l1_pelvic_angle_input+0.62515338*pelvic_incidence_input-0.10468428*t4_t12_input-1.3168397*t4_pelvic_angle_input
  
  t10_sa <- 0.82052592-0.11383187*t11_sa-0.44191482*t12_sa-0.5843131*l1_s1_input+0.62310746*l1_pelvic_angle_input+0.58612084*pelvic_incidence_input-0.054484117*t4_t12_input-1.2046572*t4_pelvic_angle_input
  
  t9_sa <- 0.44589952-0.027082825*t10_sa-0.29467934*t11_sa-0.4252468*t12_sa-0.44179798*l1_s1_input+0.46525383*l1_pelvic_angle_input+0.46289389*pelvic_incidence_input+0.027649066*t4_t12_input-0.91085339*t4_pelvic_angle_input
  
  t8_sa <- -0.76354758+0.079321328*t9_sa-0.33399877*t10_sa-0.38545404*t11_sa-0.41123736*t12_sa-0.43370819*l1_s1_input+0.44832215*l1_pelvic_angle_input+0.45705009*pelvic_incidence_input+0.066780019*t4_t12_input-0.882922*t4_pelvic_angle_input
  
  t7_sa <- -0.36947144-0.048194619*t8_sa-0.23318337*t9_sa-0.39080782*t10_sa-0.4541907*t11_sa-0.41299988*t12_sa-0.36722696*l1_s1_input+0.42801275*l1_pelvic_angle_input+0.3722355*pelvic_incidence_input+0.14692544*t4_t12_input-0.77428477*t4_pelvic_angle_input
  
  t6_sa <- -1.0883547-0.07454856*t7_sa-0.23280179*t8_sa-0.30433902*t9_sa-0.3709043*t10_sa-0.43049871*t11_sa-0.3236001*t12_sa-0.22309461*l1_s1_input+0.25654313*l1_pelvic_angle_input+0.2321191*pelvic_incidence_input+0.22127623*t4_t12_input-0.46755303*t4_pelvic_angle_input
  
  t5_sa <- 0.69162214-0.31975675*t6_sa-0.4412255*t7_sa-0.50623331*t8_sa-0.53913213*t9_sa-0.57230033*t10_sa-0.65197163*t11_sa-0.48260958*t12_sa-0.29316955*l1_s1_input+0.37372636*l1_pelvic_angle_input+0.27267218*pelvic_incidence_input+0.39331364*t4_t12_input-0.63336495*t4_pelvic_angle_input
  
  t4_sa <- -0.28175955-0.56942085*t5_sa-0.63585333*t6_sa-0.69326886*t7_sa-0.77121564*t8_sa-0.72964636*t9_sa-0.78017101*t10_sa-0.81672537*t11_sa-0.48154746*t12_sa-0.14032371*l1_s1_input+0.17613688*l1_pelvic_angle_input+0.13210963*pelvic_incidence_input+0.65367631*t4_t12_input-0.28707275*t4_pelvic_angle_input
  
  t3_sa <- -2.5099283+0.32644445*t4_sa+0.010587101*t5_sa-0.09069735*t6_sa+0.028017449*t7_sa+0.024184383*t8_sa-0.051232677*t9_sa+0.014130663*t10_sa+0.043587369*t11_sa+0.048197154*t12_sa+0.087459387*l1_s1_input-0.18412973*l1_pelvic_angle_input-0.070866287*pelvic_incidence_input+0.035919594*t4_t12_input+0.25213504*t4_pelvic_angle_input
  
  t2_sa <- -1.1310498+0.35405136*t3_sa+0.023685093*t4_sa-0.066344556*t5_sa-0.011666527*t6_sa-0.06165322*t7_sa-0.047877164*t8_sa-0.023500339*t9_sa-0.02582784*t10_sa+0.018402494*t11_sa+0.029379976*t12_sa+0.042692784*l1_s1_input-0.10099911*l1_pelvic_angle_input-0.053272235*pelvic_incidence_input+0.012689279*t4_t12_input+0.15739916*t4_pelvic_angle_input
  
  t1_sa <- -0.63351857+0.38606222*t2_sa-0.024390745*t3_sa-0.017735187*t4_sa-0.09823423*t5_sa-0.055845049*t6_sa-0.13693589*t7_sa+0.019874629*t8_sa-0.060870147*t9_sa-0.0083761595*t10_sa-0.022734773*t11_sa+0.042125893*t12_sa+0.016763235*l1_s1_input-0.071824176*l1_pelvic_angle_input-0.040060676*pelvic_incidence_input+0.011215865*t4_t12_input+0.11641339*t4_pelvic_angle_input
  
  return(list(t12_segment_angle = t12_sa,
              t11_segment_angle = t11_sa,
              t10_segment_angle = t10_sa,
              t9_segment_angle = t9_sa,
              t8_segment_angle = t8_sa,
              t7_segment_angle = t7_sa,
              t6_segment_angle = t6_sa,
              t5_segment_angle = t5_sa,
              t4_segment_angle = t4_sa,
              t3_segment_angle = t3_sa,
              t2_segment_angle = t2_sa,
              t1_segment_angle = t1_sa))
}

cervical_segment_angles_function <- function(rad_t9pa_input,
                                             rad_t4pa_input, 
                                             rad_c2pa_input){
  
  # cervical_lordosis <- estimate_cervical_lordosis_by_pelvic_angles_function(rad_t9pa = rad_t9pa_input,
  #                                                                           rad_t4pa = rad_t4pa_input, 
  #                                                                           rad_c2pa = rad_c2pa_input)
  
  cervical_lordosis <- -7.8442273-0.78207256*rad_t9pa_input-0.77342221*rad_t4pa_input+1.7021151*rad_c2pa_input 
  
  cervical_segment_angle <- cervical_lordosis/6
  
  return(list(
    c7_segment_angle = cervical_segment_angle, 
    c6_segment_angle = cervical_segment_angle,
    c5_segment_angle = cervical_segment_angle,
    c4_segment_angle = cervical_segment_angle,
    c3_segment_angle = cervical_segment_angle,
    c2_segment_angle = cervical_segment_angle,
    c1_segment_angle = cervical_segment_angle))
}

################## COMPILE FULL SPINE ESTIMATED SEGMENT ANGLES LIST #################
compute_estimated_segment_angle_list_from_pelvic_angles_function <- function(pelvic_incidence_input = 50,
                                                                             l1_pelvic_angle_input = 10,
                                                                             l1s1_angle_input = 99,
                                                                             t9_pelvic_angle_input = 10,
                                                                             t4_pelvic_angle_input = 10,
                                                                             c2_pelvic_angle_input = 10
)
{
  
  # l1s1_estimate <- estimate_l1s1_by_l1pa_t9pa_function(rad_l1pa = l1_pelvic_angle_input,
  #                                                      rad_t9pa = t9_pelvic_angle_input, 
  #                                                      rad_pi = pelvic_incidence_input)
  
  if(l1s1_angle_input == 99){
    l1s1_estimate <- jh_estimate_l1s1_from_pi_l1pa_t9pa_function(pelvic_incidence = pelvic_incidence_input, 
                                                                 l1_pelvic_angle = l1_pelvic_angle_input, 
                                                                 t9_pelvic_angle = t9_pelvic_angle_input) 
  }else{
    l1s1_estimate <- l1s1_angle_input
  }
  
  lumbar_segment_angles_list <- lumbar_segment_angle_function(l1_s1_input = l1s1_estimate, 
                                                              l1_pelvic_angle_input = l1_pelvic_angle_input,
                                                              pelvic_incidence_input = pelvic_incidence_input)
  
  tk_estimate <- estimate_tk_by_pelvic_angles_function(rad_l1pa = l1_pelvic_angle_input,
                                                       rad_t9pa = t9_pelvic_angle_input, 
                                                       rad_t4pa = t4_pelvic_angle_input, 
                                                       rad_c2pa = c2_pelvic_angle_input)
  
  thoracic_segment_angles_list <- thoracic_segment_angles_function(l1_s1_input = l1s1_estimate, 
                                                                   l1_pelvic_angle_input = l1_pelvic_angle_input, 
                                                                   pelvic_incidence_input = pelvic_incidence_input, 
                                                                   t4_t12_input = tk_estimate, 
                                                                   t4_pelvic_angle_input = t4_pelvic_angle_input)
  
  
  cervical_segment_angles_list <- cervical_segment_angles_function(rad_t9pa_input = t9_pelvic_angle_input,
                                                                   rad_t4pa_input =  t4_pelvic_angle_input,
                                                                   rad_c2pa_input = c2_pelvic_angle_input)
  
  segment_angle_list <-
    list(
      l5_segment_angle = lumbar_segment_angles_list$l5_segment_angle,
      l4_segment_angle = lumbar_segment_angles_list$l4_segment_angle,
      l3_segment_angle = lumbar_segment_angles_list$l3_segment_angle,
      l2_segment_angle = lumbar_segment_angles_list$l2_segment_angle,
      l1_segment_angle = lumbar_segment_angles_list$l1_segment_angle,
      t12_segment_angle = thoracic_segment_angles_list$t12_segment_angle,
      t11_segment_angle = thoracic_segment_angles_list$t11_segment_angle,
      t10_segment_angle = thoracic_segment_angles_list$t10_segment_angle,
      t9_segment_angle = thoracic_segment_angles_list$t9_segment_angle,
      t8_segment_angle = thoracic_segment_angles_list$t8_segment_angle,
      t7_segment_angle = thoracic_segment_angles_list$t7_segment_angle,
      t6_segment_angle = thoracic_segment_angles_list$t6_segment_angle,
      t5_segment_angle = thoracic_segment_angles_list$t5_segment_angle,
      t4_segment_angle = thoracic_segment_angles_list$t4_segment_angle,
      t3_segment_angle = thoracic_segment_angles_list$t3_segment_angle,
      t2_segment_angle = thoracic_segment_angles_list$t2_segment_angle,
      t1_segment_angle = thoracic_segment_angles_list$t1_segment_angle,
      c7_segment_angle = cervical_segment_angles_list$c7_segment_angle,
      c6_segment_angle = cervical_segment_angles_list$c6_segment_angle,
      c5_segment_angle = cervical_segment_angles_list$c5_segment_angle,
      c4_segment_angle = cervical_segment_angles_list$c4_segment_angle,
      c3_segment_angle = cervical_segment_angles_list$c3_segment_angle,
      c2_segment_angle = cervical_segment_angles_list$c2_segment_angle,
      c1_segment_angle = cervical_segment_angles_list$c1_segment_angle
    )
  
  return(segment_angle_list)
  
}

################## NOW BUILD A QUICK SPINE AND MEASURE THE PELVIC ANGLES FOR TESTING WHETHER THEY ARE CORRECT #################
################## NOW BUILD A QUICK SPINE AND MEASURE THE PELVIC ANGLES FOR TESTING WHETHER THEY ARE CORRECT #################
################## NOW BUILD A QUICK SPINE AND MEASURE THE PELVIC ANGLES FOR TESTING WHETHER THEY ARE CORRECT #################
################## NOW BUILD A QUICK SPINE AND MEASURE THE PELVIC ANGLES FOR TESTING WHETHER THEY ARE CORRECT #################

build_spine_for_checking_pelvic_angles_function <- function(pelv_inc_value = 50, 
                                                            pt_value = 10,
                                                            segment_angle_list,
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
  
  spine_orientation <- if_else(spine_faces == "left", 1, -1)
  
  ss_value <- pelv_inc_value - pt_value
  
  p_inc <- (pi / 180) * (pelv_inc_value)
  pt <- (pi / 180) * (pt_value)
  ss <- (pi / 180) * (ss_value)
  
  
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
  
  sacrum_list <- list(sl = ss, 
                      sa = s1a,
                      sp = s1p)
  
  #inf list requires sl, sa, sp
  
  l4s1_horizontal_length <- 12
  horizontal_line_down_length <- 10
  
  l5_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - segment_angle_list$l5_segment_angle), 
                                               inferior_vert_list = sacrum_list,
                                               endplate_width = 5, 
                                               endplate_height = 4, 
                                               wedge_body = FALSE,
                                               spine_facing = spine_faces, 
                                               pso = FALSE)
  
  l4_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:2]))), 
                                               inferior_vert_list = l5_list, 
                                               endplate_width = 5,
                                               endplate_height = 4, 
                                               wedge_body = FALSE, 
                                               spine_facing = spine_faces, 
                                               build_lines = FALSE,
                                               pso = FALSE)
  
  l3_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:3]))),  
                                               inferior_vert_list = l4_list, 
                                               endplate_width = 5,
                                               endplate_height = 4, 
                                               wedge_body = if_else(segment_angle_list$l3_segment_angle < 0, TRUE, FALSE),
                                               spine_facing = spine_faces, 
                                               pso = FALSE)
  
  l2_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:4]))), 
                                               inferior_vert_list = l3_list, 
                                               endplate_width = 5, 
                                               endplate_height = 4, 
                                               wedge_body = if_else(segment_angle_list$l2_segment_angle < 0, TRUE, FALSE), 
                                               spine_facing = spine_faces, 
                                               pso = FALSE)
  
  l1_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:5]))), 
                                               inferior_vert_list = l2_list,
                                               endplate_width = 5, 
                                               endplate_height = 4, 
                                               wedge_body = if_else(segment_angle_list$l1_segment_angle < 0, TRUE, FALSE),
                                               spine_facing = spine_faces, 
                                               build_lines = FALSE,
                                               pso = FALSE)
  
  
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  # segment_angle_list$l5_segment_angle + segment_angle_list$l4_segment_angle + segment_angle_list$l3_segment_angle + segment_angle_list$l2_segment_angle + segment_angle_list$l1_segment_angle
  
  
  
  
  t12_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:6]))), 
      inferior_vert_list = l1_list,
      endplate_width = 4.9,
      endplate_height = 3.8,
      wedge_body = if_else(segment_angle_list$t12_segment_angle < 0, TRUE, FALSE),
      build_lines = FALSE,
      spine_facing = spine_faces,
      pso = FALSE
    )
  
  t11_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:7]))), 
      inferior_vert_list = t12_list,
      endplate_width = 4.8,
      endplate_height = 3.7,
      wedge_body = if_else(segment_angle_list$t11_segment_angle < 0, TRUE, FALSE),
      spine_facing = spine_faces,
      pso = FALSE
    )
  
  t10_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:8]))), 
      inferior_vert_list = t11_list,
      endplate_width = 4.8,
      endplate_height = 3.6,
      wedge_body = if_else(segment_angle_list$t10_segment_angle < 0, TRUE, FALSE),
      spine_facing = spine_faces,
      pso = FALSE
    )
  
  t9_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:9]))), 
      inferior_vert_list = t10_list,
      endplate_width = 4.7,
      endplate_height = 3.4,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t8_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:10]))), 
      inferior_vert_list = t9_list,
      endplate_width = 4.6,
      endplate_height = 3.3,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t7_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:11]))), 
      inferior_vert_list = t8_list,
      endplate_width = 4.5,
      endplate_height = 3.2,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t6_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:12]))), 
      inferior_vert_list = t7_list,
      endplate_width = 4.4,
      endplate_height = 3,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t5_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:13]))), 
      inferior_vert_list = t6_list,
      endplate_width = 4.3,
      endplate_height = 2.8,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t4_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:14]))), 
      inferior_vert_list = t5_list,
      endplate_width = 4.2,
      endplate_height = 2.6,
      wedge_body = TRUE,
      build_lines = FALSE,
      spine_facing = spine_faces
    )
  
  t3_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:15]))), 
      inferior_vert_list = t4_list,
      endplate_width = 4.1,
      endplate_height = 2.4,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t2_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:16]))), 
      inferior_vert_list = t3_list,
      endplate_width = 4,
      endplate_height = 2.2,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t1_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:17]))), 
      inferior_vert_list = t2_list,
      endplate_width = 3.8,
      endplate_height = 2,
      wedge_body = TRUE,
      build_lines = FALSE,
      spine_facing = spine_faces
    )
  
  
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  
  spine_orientation <- if_else(spine_faces == "left", 1, -1)
  
  if(length(segment_angle_list)>17){
    c7_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:18]))), 
                                                 inferior_vert_list = t1_list,
                                                 endplate_width = 3.6, 
                                                 endplate_height = 1.5, 
                                                 wedge_body = TRUE, 
                                                 build_lines = FALSE,
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
                                                 build_lines = FALSE,
                                                 spine_facing = spine_faces,
                                                 c2_body = TRUE)
    
    c1_list <- vertebral_body_build_function_new(vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:24]))),
                                                 inferior_vert_list = c2_list, 
                                                 endplate_width = 3.25, 
                                                 endplate_height = 1.5,
                                                 wedge_body = TRUE,
                                                 spine_facing = spine_faces)  
  }
  
  
  ############################################# COMPUTE PELVIC ANGLES ############################################
  ############################################# COMPUTE PELVIC ANGLES ############################################
  ############################################# COMPUTE PELVIC ANGLES ############################################
  ############################################# COMPUTE PELVIC ANGLES ############################################
  ############################################# COMPUTE PELVIC ANGLES ############################################
  
  spine_pelvic_angles_list <- list()
  
  #### l4PA #### 
  l4_tilt_value <-  jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                 fem_head_center_input = fem_head_center, 
                                                                 vertebral_body_list = l4_list)

  spine_pelvic_angles_list$l4pa_value <- l4_tilt_value + pt_value
  
  #### L1PA #### 
  l1_tilt_value <-  jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                   fem_head_center_input = fem_head_center, 
                                                                                   vertebral_body_list = l1_list)
  
  spine_pelvic_angles_list$l1pa_value <- l1_tilt_value + pt_value
  
  #### T9PA #### 
  t9_tilt_value <-  jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                   fem_head_center_input = fem_head_center, 
                                                                                   vertebral_body_list = t9_list)
  
  spine_pelvic_angles_list$t9pa_value <- t9_tilt_value + pt_value
  
  #### T4PA #### 
  t4_tilt_value <-  jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                   fem_head_center_input = fem_head_center, 
                                                                                   vertebral_body_list = t4_list)
  
  spine_pelvic_angles_list$t4pa_value <- t4_tilt_value + pt_value
  
  
  #### T1PA ####
  t1_tilt_value <-  jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                   fem_head_center_input = fem_head_center, 
                                                                                   vertebral_body_list = t1_list)
  
  spine_pelvic_angles_list$t1pa_value <- t1_tilt_value + pt_value
  
  
  
  #### C2PA ####
  if(length(segment_angle_list)>17){

    c2_tilt_value <-  jh_building_spine_compute_vertebral_tilt_by_direction_function(spine_faces_input = spine_faces, 
                                                                                     fem_head_center_input = fem_head_center, 
                                                                                     vertebral_body_list = c2_list)
    
    spine_pelvic_angles_list$c2pa_value <- c2_tilt_value + pt_value
  }
  

  spine_pelvic_angles_list <- map(.x = spine_pelvic_angles_list, .f = ~ round(.x, 1))
  
  
  return(spine_pelvic_angles_list)
}


################## NOW - COMPUTE ESTIMATED SEGMENT ANGLES, COMPUTE THE ACTUAL PELVIC ANGLES BY BUILDING THE SPINE for CHECKING, then ADJUST the SEGMENT ANGLES UNTIL THEY ARE CORRECT #################
################## NOW - COMPUTE ESTIMATED SEGMENT ANGLES, COMPUTE THE ACTUAL PELVIC ANGLES BY BUILDING THE SPINE for CHECKING, then ADJUST the SEGMENT ANGLES UNTIL THEY ARE CORRECT #################
################## NOW - COMPUTE ESTIMATED SEGMENT ANGLES, COMPUTE THE ACTUAL PELVIC ANGLES BY BUILDING THE SPINE for CHECKING, then ADJUST the SEGMENT ANGLES UNTIL THEY ARE CORRECT #################
# this returns the final segment angles
segment_angles_function_from_vertebral_pelvic_angles_function <- function(pelvic_incidence_input, 
                                                                          l1pa_input, 
                                                                          l1s1_input = 99,
                                                                          t9pa_input, 
                                                                          t4pa_input, 
                                                                          c2pa_input){
  
  pi_value <- pelvic_incidence_input
  
  l1pa_floating_value <- l1pa_input
  t9pa_floating_value <- t9pa_input 
  t4pa_floating_value <- t4pa_input 
  c2pa_floating_value <- c2pa_input 
  
  sa_test <- compute_estimated_segment_angle_list_from_pelvic_angles_function(pelvic_incidence_input = pi_value, 
                                                                              l1_pelvic_angle_input = l1pa_floating_value, 
                                                                              l1s1_angle_input = l1s1_input,
                                                                              t9_pelvic_angle_input = t9pa_floating_value, 
                                                                              t4_pelvic_angle_input = t4pa_floating_value, 
                                                                              c2_pelvic_angle_input = c2pa_floating_value)
  
  
  testing_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pi_value, 
                                                                  pt_value = 10,
                                                                  segment_angle_list = sa_test)
  
  
  
  while(round(testing_vpas$c2pa_value, 0) != c2pa_input){
    # c2pa_floating_value <- c2pa_floating_value - (testing_vpas$c2pa_value - c2pa_floating_value)*0.5  
    if(testing_vpas$c2pa_value > c2pa_floating_value){
      c2pa_floating_value <- c2pa_floating_value - 0.5
    }else{
      c2pa_floating_value <- c2pa_floating_value + 0.5
    }
    
    sa_test <- compute_estimated_segment_angle_list_from_pelvic_angles_function(pelvic_incidence_input = pi_value, 
                                                                                l1_pelvic_angle_input = l1pa_floating_value,
                                                                                l1s1_angle_input = l1s1_input,
                                                                                t9_pelvic_angle_input = t9pa_floating_value, 
                                                                                t4_pelvic_angle_input = t4pa_floating_value, 
                                                                                c2_pelvic_angle_input = c2pa_floating_value)
    
    testing_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pi_value, 
                                                                    pt_value = 10,
                                                                    segment_angle_list =sa_test)
  }
  
  while(round(testing_vpas$t4pa_value, 0) != t4pa_input){
    if(testing_vpas$t4pa_value > t4pa_floating_value){
      t4pa_floating_value <- t4pa_floating_value - 0.5
    }else{
      t4pa_floating_value <- t4pa_floating_value + 0.5
    }
    
    sa_test <- compute_estimated_segment_angle_list_from_pelvic_angles_function(pelvic_incidence_input = pi_value, 
                                                                                l1_pelvic_angle_input = l1pa_floating_value, 
                                                                                l1s1_angle_input = l1s1_input,
                                                                                t9_pelvic_angle_input = t9pa_floating_value, 
                                                                                t4_pelvic_angle_input = t4pa_floating_value, 
                                                                                c2_pelvic_angle_input = c2pa_floating_value)
    
    testing_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pi_value, 
                                                                    pt_value = 10,
                                                                    segment_angle_list =sa_test)
  }
  
  while(round(testing_vpas$t9pa_value, 0) != t9pa_input){
    # t9pa_floating_value <- t9pa_floating_value - (testing_vpas$t9pa_value - t9pa_floating_value)*0.5  
    if(testing_vpas$t9pa_value > t9pa_floating_value){
      t9pa_floating_value <- t9pa_floating_value - 0.5
    }else{
      t9pa_floating_value <- t9pa_floating_value + 0.5
    }
    
    
    sa_test <- compute_estimated_segment_angle_list_from_pelvic_angles_function(pelvic_incidence_input = pi_value, 
                                                                                l1_pelvic_angle_input = l1pa_floating_value, 
                                                                                l1s1_angle_input = l1s1_input,
                                                                                t9_pelvic_angle_input = t9pa_floating_value, 
                                                                                t4_pelvic_angle_input = t4pa_floating_value, 
                                                                                c2_pelvic_angle_input = c2pa_floating_value)
    
    testing_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pi_value, 
                                                                    pt_value = 10,
                                                                    segment_angle_list =sa_test)
  }
  
  
  while(round(testing_vpas$l1pa_value, 0) != round(l1pa_input, 0)){
    # l1pa_floating_value <- l1pa_floating_value - (testing_vpas$l1pa_value - l1pa_floating_value)*0.5  
    if(testing_vpas$l1pa_value > l1pa_floating_value){
      l1pa_floating_value <- l1pa_floating_value - 0.5
    }else{
      l1pa_floating_value <- l1pa_floating_value + 0.5
    }
    
    sa_test <- compute_estimated_segment_angle_list_from_pelvic_angles_function(pelvic_incidence_input = pi_value, 
                                                                                l1_pelvic_angle_input = l1pa_floating_value, 
                                                                                l1s1_angle_input = l1s1_input,
                                                                                t9_pelvic_angle_input = t9pa_floating_value, 
                                                                                t4_pelvic_angle_input = t4pa_floating_value, 
                                                                                c2_pelvic_angle_input = c2pa_floating_value)
    
    testing_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = pi_value, 
                                                                    pt_value = 10,
                                                                    segment_angle_list =sa_test)
  }
  
  return(sa_test)
  
}

################## PUTTING ALL TOGETHER - BUILD A FULL SPINE FROM ONLY THE VERTEBRAL PELVIC ANGLES INPUT #################
################## PUTTING ALL TOGETHER - BUILD A FULL SPINE FROM ONLY THE VERTEBRAL PELVIC ANGLES INPUT #################
################## PUTTING ALL TOGETHER - BUILD A FULL SPINE FROM ONLY THE VERTEBRAL PELVIC ANGLES INPUT #################
################## PUTTING ALL TOGETHER - BUILD A FULL SPINE FROM ONLY THE VERTEBRAL PELVIC ANGLES INPUT #################
################## PUTTING ALL TOGETHER - BUILD A FULL SPINE FROM ONLY THE VERTEBRAL PELVIC ANGLES INPUT #################

build_full_spine_from_vertebral_pelvic_angles_function <- function(pelv_inc_value = 50, 
                                                                   pt_value = 10,
                                                                   l1pa_value_input,
                                                                   l1s1_value_input = 199,
                                                                   t10_l2_value_input = -44,
                                                                   c2_c7_value_input = 29,
                                                                   t9pa_value_input,
                                                                   t4pa_value_input,
                                                                   c2pa_value_input,
                                                                   planned_l1_pelvic_angle = 10,
                                                                   l1l4_angle_line_length = 10,
                                                                   l4s1_angle_line_length = 10,
                                                                   posterior_line_length = 30,
                                                                   fem_head_center_x = 0, 
                                                                   spine_faces = "right",
                                                                   pso_levels = c(""), 
                                                                   return_spine_plot = "no"
) {
  
  # segment_angle_list <- segment_angles_function_from_vertebral_pelvic_angles_function(pelvic_incidence_input = pelv_inc_value,
  #                                                                                     l1s1_input = l1s1_value_input,
  #                                                                                     l1pa_input = l1pa_value_input,
  #                                                                                     t9pa_input = t9pa_value_input,
  #                                                                                     t4pa_input = t4pa_value_input,
  #                                                                                     c2pa_input = c2pa_value_input)
  
  # segment_angle_list <- compute_sa_list_from_sim_data_function(pelvic_incidence = pelv_inc_value,
  #                                                              l1_s1 = l1s1_value_input,
  #                                                              # t10_l2 = t10_l2_value_input, 
  #                                                              c2_c7 = c2_c7_value_input,
  #                                                              l1pa = l1pa_value_input, 
  #                                                              t9pa = t9pa_value_input, 
  #                                                              t4pa = t4pa_value_input, 
  #                                                              c2pa = c2pa_value_input)
  # 
  
  segment_angle_list <- compute_segment_angles_list_function(pelvic_incidence = pelv_inc_value,
                                       l1_s1 = l1s1_value_input,
                                       t10_l2 = t10_l2_value_input,
                                       c2_c7 = c2_c7_value_input,
                                       l1pa = l1pa_value_input,
                                       t9pa = t9pa_value_input,
                                       t4pa = t4pa_value_input,
                                       c2pa = c2pa_value_input
                                       )
  
  # segment_angle_list <-  compute_segment_angle_list_pi_vpa_function(pelvic_incidence = pelv_inc_value,
  #                                            # l1_s1 = l1s1_value_input,
  #                                            # t10_l2 = t10_l2_value_input, 
  #                                            # c2_c7 = c2_c7_value_input,
  #                                            l1pa = l1pa_value_input, 
  #                                            t9pa = t9pa_value_input, 
  #                                            t4pa = t4pa_value_input, 
  #                                            c2pa = c2pa_value_input
  #                                            )
  
  l1_s1 <- (segment_angle_list$l5_segment_angle + segment_angle_list$l4_segment_angle + segment_angle_list$l3_segment_angle + segment_angle_list$l2_segment_angle + segment_angle_list$l1_segment_angle)
  
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
  
  cervical_lordosis <- (segment_angle_list$c7_segment_angle +
                          segment_angle_list$c6_segment_angle + 
                          segment_angle_list$c5_segment_angle + 
                          segment_angle_list$c4_segment_angle + 
                          segment_angle_list$c3_segment_angle + 
                          segment_angle_list$c2_segment_angle)
  
  if(pt_value == 99){
    pt_value <- predict_pt_function(pelvic_incidence = pelv_inc_value, t1_l1 = t1_l1, l1_s1 = l1_s1, l1_pelvic_angle = planned_l1_pelvic_angle)
  }else{
    pt_value <- pt_value
  }
  
  spine_orientation <- if_else(spine_faces == "left", 1, -1)
  
  ss_value <- pelv_inc_value - pt_value
  
  p_inc <- (pi / 180) * (pelv_inc_value)
  pt <- (pi / 180) * (pt_value)
  ss <- (pi / 180) * (ss_value)
  
  
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
  
  
  l5_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - segment_angle_list$l5_segment_angle),
      inferior_vert_list = sacrum_list,
      endplate_width = 5,
      endplate_height = 4,
      wedge_body = FALSE,
      spine_facing = spine_faces,
      pso = if_else(any(str_to_lower(pso_levels) == "l5"), TRUE, FALSE)
    )
  
  l4_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:2]))), 
      inferior_vert_list = l5_list,
      endplate_width = 5,
      endplate_height = 4,
      wedge_body = FALSE,
      spine_facing = spine_faces,
      build_lines = TRUE,
      line_down_length = l4s1_angle_line_length,
      line_up_length = l1l4_angle_line_length,
      horizontal_line_down_length = l4s1_horizontal_length,
      horizontal_line_up_length = 9,
      horizontal_line_length = 40,
      posterior_lordosis_line_length = posterior_line_length,
      pso = if_else(any(str_to_lower(pso_levels) == "l4"), TRUE, FALSE)
    )
  
  l3_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:3]))), 
      inferior_vert_list = l4_list,
      endplate_width = 5,
      endplate_height = 4,
      wedge_body = if_else(segment_angle_list$l3_segment_angle < 0, TRUE, FALSE),
      spine_facing = spine_faces,
      pso = if_else(any(str_to_lower(pso_levels) == "l3"), TRUE, FALSE)
    )
  
  l2_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:4]))), 
      inferior_vert_list = l3_list,
      endplate_width = 5,
      endplate_height = 4,
      wedge_body = if_else(segment_angle_list$l2_segment_angle < 0, TRUE, FALSE),
      spine_facing = spine_faces,
      pso = if_else(any(str_to_lower(pso_levels) == "l2"), TRUE, FALSE)
    )
  
  l1_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:5]))), 
      inferior_vert_list = l2_list,
      endplate_width = 5,
      endplate_height = 4,
      wedge_body = if_else(segment_angle_list$l1_segment_angle < 0, TRUE, FALSE),
      spine_facing = spine_faces,
      build_lines = TRUE,
      line_down_length = l1l4_angle_line_length,
      horizontal_line_down_length = horizontal_line_down_length,
      horizontal_line_length = 40,
      posterior_lordosis_line_length = posterior_line_length,
      pso = if_else(any(str_to_lower(pso_levels) == "l1"), TRUE, FALSE)
    )
  
  
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  ############################################# THORACIC ############################################
  
  t12_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:6]))), 
      inferior_vert_list = l1_list,
      endplate_width = 4.9,
      endplate_height = 3.8,
      wedge_body = if_else(segment_angle_list$t12_segment_angle < 0, TRUE, FALSE),
      build_lines = TRUE,
      spine_facing = spine_faces,
      pso = if_else(any(str_to_lower(pso_levels) == "t12"), TRUE, FALSE)
    )
  
  t11_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:7]))), 
      inferior_vert_list = t12_list,
      endplate_width = 4.8,
      endplate_height = 3.7,
      wedge_body = if_else(segment_angle_list$t11_segment_angle < 0, TRUE, FALSE),
      spine_facing = spine_faces,
      pso = if_else(any(str_to_lower(pso_levels) == "t11"), TRUE, FALSE)
    )
  
  t10_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:8]))), 
      inferior_vert_list = t11_list,
      endplate_width = 4.8,
      endplate_height = 3.6,
      wedge_body = if_else(segment_angle_list$t10_segment_angle < 0, TRUE, FALSE),
      spine_facing = spine_faces,
      pso = if_else(any(str_to_lower(pso_levels) == "t10"), TRUE, FALSE)
    )
  
  t9_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:9]))), 
      inferior_vert_list = t10_list,
      endplate_width = 4.7,
      endplate_height = 3.4,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t8_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:10]))), 
      inferior_vert_list = t9_list,
      endplate_width = 4.6,
      endplate_height = 3.3,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t7_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:11]))), 
      inferior_vert_list = t8_list,
      endplate_width = 4.5,
      endplate_height = 3.2,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t6_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:12]))), 
      inferior_vert_list = t7_list,
      endplate_width = 4.4,
      endplate_height = 3,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t5_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:13]))), 
      inferior_vert_list = t6_list,
      endplate_width = 4.3,
      endplate_height = 2.8,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t4_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:14]))), 
      inferior_vert_list = t5_list,
      endplate_width = 4.2,
      endplate_height = 2.6,
      wedge_body = TRUE,
      build_lines = TRUE,
      spine_facing = spine_faces
    )
  
  t3_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:15]))), 
      inferior_vert_list = t4_list,
      endplate_width = 4.1,
      endplate_height = 2.4,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t2_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:16]))), 
      inferior_vert_list = t3_list,
      endplate_width = 4,
      endplate_height = 2.2,
      wedge_body = TRUE,
      spine_facing = spine_faces
    )
  
  t1_list <-
    vertebral_body_build_function_new(
      vertebral_slope = (pi / 180) * (ss_value  - (Reduce("+", segment_angle_list[1:17]))), 
      inferior_vert_list = t2_list,
      endplate_width = 3.8,
      endplate_height = 2,
      wedge_body = TRUE,
      build_lines = TRUE,
      spine_facing = spine_faces
    )
  
  
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  ############################################# CERVICAL ############################################
  
  spine_orientation <- if_else(spine_faces == "left", 1, -1)
  
  
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
  
  

  # lines_list <- list(lines_list = map(.x = lines_list,
  #                                     .f = ~ st_geometry(.x))
  # )
  lines_list <- map(.x = lines_list,
                    .f = ~ st_geometry(.x))

  spine_list <- list(pelvic_incidence =  pelv_inc_value, 
                     pelvic_tilt =   pt_value,
                     lumbar_lordosis =  l1_s1,
                     l4_pelvic_angle_value =  l4pa_value,
                     l1_pelvic_angle_value =  l1pa_value,
                     t9_pelvic_angle_value =  t9pa_value,
                     t4_pelvic_angle_value = t4pa_value,
                     t1_pelvic_angle_value =  t1pa_value,
                     c2_pelvic_angle_value = c2pa_value,
                     c2_tilt_value = c2_tilt_value,
                     thoracic_kyphosis =   spinal_kyphosis_input,
                     cervical_lordosis =  cervical_lordosis,
                     fem_head_geom = st_geometry(st_zm(x = fem_head_sf)),
                     fem_head_centroid_vector = as_vector(st_centroid(x = fem_center_circle_sf)),
                     s1_centroid_vector = as_vector(st_centroid(s1_center_circle_sf)),
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
                     c2_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c2_list$c2_with_dens, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     c1_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c1_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
                     head_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = head_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND")))
  )
  
  
  spine_geoms_df <- tibble(
    fem_head_geom = st_geometry(st_zm(x = fem_head_sf)),
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
    c2_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c2_list$c2_with_dens, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
    c1_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = c1_list$vert_body_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND"))),
    head_geom = st_geometry(st_zm(st_buffer(x = st_buffer(x = head_sf, dist = -0.5, endCapStyle = "ROUND"), dist = 0.5, endCapStyle = "ROUND")))
  ) %>%
    pivot_longer(cols = contains("geom"), names_to = "object", values_to = "geom") %>%
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
  
  
  full_list <- list(spine_list = spine_list, 
       spine_df = spine_geoms_df, 
       lines_list = lines_list,
       vertebral_body_list = vertebral_body_list)
  
  if(return_spine_plot == "yes"){
    spine_plotting_geoms_df <- spine_geoms_df %>%
      select(object, geom, geom_alpha)
    
    
    full_list$spine_plot <- ggplot() +
      geom_sf(data = spine_plotting_geoms_df, 
              color = "black", 
              fill = "grey90",
              aes(geometry = geom, 
                  alpha = geom_alpha
              )) +
      jh_make_colored_line_geom_function(sf_geom_input =lines_list$l1pa_line_curve_sf, 
                                         color_input = "blue", 
                                         line_size =  1) +
      jh_make_colored_line_geom_function(sf_geom_input =lines_list$t9pa_line_curve_sf, 
                                         color_input = "orange", 
                                         line_size =  1) +
      jh_make_colored_line_geom_function(sf_geom_input = lines_list$t4pa_line_curve_sf, 
                                         color_input = "purple", 
                                         line_size =  1) +
      jh_make_colored_line_geom_function(sf_geom_input = lines_list$c2pa_line_curve_sf, 
                                         color_input = "darkgreen", 
                                         line_size =  1) +
      theme_nothing()
    
  }
  
  return(full_list)
  
}
