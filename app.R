## ATTEMPT WITH CLEANED CODE - FUNCTION FOR VERT BODIES
# library(colourpicker)
library(shiny)
# library(reactlog)
library(sf)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(shinyBS)
# library(kableExtra)
# library(rms)
library(svglite)
library(glue)
library(cowplot)
library(janitor)
# library(rms)
# library(Hmisc)
library(cowplot)
# library(assertr)
library(lubridate)
library(shinydashboard)
library(magick)
# library(ggforce)

source("jh_functions.R", local = TRUE)
source("compute_segment_angles_function_from_sim_data_2024.R", local = TRUE)
source("function_segment_angles_separated.R", local = TRUE)

source("jh_spine_build_NEW_function.R", local = TRUE)

source("jh_prescribing_alignment_functions.R", local = TRUE)

source("spinal_regional_alignment_analysis_by_vpa.R", local = TRUE)

# source("jh_build_spine_by_vertebral_pelvic_angles_only.R", local = TRUE)
source("jh_build_spine_by_vertebral_pelvic_angles_cleaned.R", local = TRUE)
source("prescribing_alignment_by_matching_unfused.R", local = TRUE)
source("xray_segment_angles_model_functions.R", local = TRUE)

jh_calculate_distance_between_2_points_function <- function(point_1, point_2){
  sqrt((point_1[1] - point_2[1])^2 + 
         (point_1[2] - point_2[2])^2)
}

jh_compute_vpa_from_xray_data_function <- function(fem_head_center = c(0,0), 
                                                   vertebral_centroid = c(0.15, 0.3), 
                                                   spine_facing = "left",
                                                   pelvic_tilt = 15){
  
  fem_head_to_centroid_length <- jh_calculate_distance_between_2_points_function(point_1 = vertebral_centroid, 
                                                                                 point_2 = fem_head_center)
  
  fem_head_to_centroid_x_length <- jh_calculate_distance_between_2_points_function(point_1 = vertebral_centroid, 
                                                                                   point_2 = c(fem_head_center[1],
                                                                                               vertebral_centroid[2]))
  
  tilt_orientation_modifier <- case_when(
    spine_facing == "left" & fem_head_center[[1]] > vertebral_centroid[[1]] ~ 1,
    spine_facing == "left" & fem_head_center[[1]] < vertebral_centroid[[1]] ~ -1,
    spine_facing == "right" & fem_head_center[[1]] > vertebral_centroid[[1]] ~ -1,
    spine_facing == "right" & fem_head_center[[1]] < vertebral_centroid[[1]] ~ 1
  )
  
  vertebral_tilt <- asin(fem_head_to_centroid_x_length/fem_head_to_centroid_length)*180/pi*tilt_orientation_modifier
  
  vpa <- pelvic_tilt + vertebral_tilt 
  
  return(vpa)
  
}

compute_perpendicular_points <- function(x1, y1, x2, y2, distance = 0.01) {
  # Midpoint
  midpoint_x <- (x1 + x2) / 2
  midpoint_y <- (y1 + y2) / 2
  
  # Slope of the line (tangent)
  slope <- (y2 - y1) / (x2 - x1)
  
  # Perpendicular slope (-1 / slope)
  perpendicular_slope <- -1 / slope
  
  # Calculate the change in x and y for perpendicular points
  delta_x <- distance / sqrt(1 + perpendicular_slope^2)
  delta_y <- perpendicular_slope * delta_x
  
  # Two perpendicular points
  point_1 <- c(midpoint_x + delta_x, midpoint_y + delta_y)
  point_2 <- c(midpoint_x - delta_x, midpoint_y - delta_y)
  
  return(tibble(x1 = point_1[1], y1 = point_1[2], x2 = point_2[1], y2 = point_2[2]))
}

# calculate_pelvic_incidence_line_coordinates <- function(fem_head_center = c(0,0), 
#                                                         s1_anterior, 
#                                                         s1_posterior, 
#                                                         spine_facing = "left") {
#   
#   # Step 1: Calculate the center (midpoint)
#   center_x <- (s1_anterior[1] + s1_posterior[1]) / 2
#   center_y <- (s1_anterior[2] + s1_posterior[2]) / 2
#   center <- c(center_x, center_y)
#   
#   # Step 2: Calculate the length of the line between s1_anterior and s1_posterior
#   line_length <- sqrt((s1_anterior[1] - s1_posterior[1])^2 + (s1_anterior[2] - s1_posterior[2])^2)
#   
#   # Step 3: Calculate the perpendicular slope (negative reciprocal of original slope)
#   # If the line is vertical (undefined slope), we set the perpendicular to be horizontal
#   if (s1_anterior[1] == s1_posterior[1]) {
#     slope_perpendicular <- 0  # Horizontal line
#   } else {
#     original_slope <- (s1_posterior[2] - s1_anterior[2]) / (s1_posterior[1] - s1_anterior[1])
#     slope_perpendicular <- -1 / original_slope
#     if(spine_facing == "right"){
#       slope_perpendicular <- slope_perpendicular*-1
#     }
#   }
#   
#   # Step 4: Calculate the length of the perpendicular line (5 times the original length)
#   extended_length <- 3 * line_length
#   
#   # Step 5: Calculate the inferior point
#   # The displacement (dx, dy) is based on the perpendicular slope and the extended length
#   if (s1_anterior[1] == s1_posterior[1]) {
#     # For vertical lines, the perpendicular is horizontal
#     dx <- extended_length
#     dy <- 0
#   } else if (s1_anterior[2] == s1_posterior[2]) {
#     # For horizontal lines, the perpendicular is vertical
#     dx <- 0
#     dy <- extended_length
#   } else {
#     # For all other lines, use the slope to calculate displacement
#     dx <- extended_length / sqrt(1 + slope_perpendicular^2)
#     dy <- slope_perpendicular * dx
#   }
#   
#   # Inferior point is displaced from the center by (dx, dy)
#   inferior_x <- center[1] + dx
#   inferior_y <- center[2] + dy
#   inferior <- c(inferior_x, inferior_y)
#   
#   pi_line_coordinates_df <- tibble(spine_point = c("fem_head_center", "s1_center", "s1_inferior"), 
#          x = c(fem_head_center[1], 
#                center[1],
#                inferior_x),
#          y = c(fem_head_center[2],
#                center[2],
#                inferior_y)
#          )
#   
#   # Return the center and inferior points as a list
#   # return(list(center = center, inferior = inferior))
#   pi_line_coordinates_df
# }
calculate_pelvic_incidence_line_coordinates <- function(fem_head_center = c(0,0), 
                                                        s1_anterior, 
                                                        s1_posterior, 
                                                        spine_facing = "left",
                                                        pelvic_tilt = 10, 
                                                        pelvic_incidence_value = 50) {
  
  # Step 1: Calculate the center (midpoint)
  center_x <- (s1_anterior[1] + s1_posterior[1]) / 2
  center_y <- (s1_anterior[2] + s1_posterior[2]) / 2
  center <- c(center_x, center_y)
  
  # Step 2: Calculate the length of the line between s1_anterior and s1_posterior
  line_length <- sqrt((s1_anterior[1] - s1_posterior[1])^2 + (s1_anterior[2] - s1_posterior[2])^2)
  
  # Step 4: Calculate the length of the perpendicular line (5 times the original length)
  extended_length <- 3 * line_length
  
  if (s1_anterior[1] == s1_posterior[1]) {
    # For vertical lines, the perpendicular is horizontal
    dx <- extended_length
    dy <- 0
  } else if (s1_anterior[2] == s1_posterior[2]) {
    # For horizontal lines, the perpendicular is vertical
    dx <- 0
    dy <- extended_length
  } else {
    pt_pi_diff <- pelvic_incidence_value - pelvic_tilt
    
    pt_pi_diff_rad <- pt_pi_diff*(pi/180)
    
    orientation_modifier <- if_else(spine_facing == "left", -1, 1)
    
    dx <- sin(pt_pi_diff_rad)*extended_length*orientation_modifier
    dy <- cos(pt_pi_diff_rad)*extended_length
  }
  
  # Inferior point is displaced from the center by (dx, dy)
  inferior_x <- center[1] - dx
  inferior_y <- center[2] - dy
  inferior <- c(inferior_x, inferior_y)
  
  pi_line_coordinates_df <- tibble(spine_point = c("fem_head_center", "s1_center", "s1_inferior"), 
                                   x = c(fem_head_center[1], 
                                         center[1],
                                         inferior_x),
                                   y = c(fem_head_center[2],
                                         center[2],
                                         inferior_y)
  )
  # Return the center and inferior points as a list
  # return(list(center = center, inferior = inferior))
  pi_line_coordinates_df
}

all_possible_lumbar_segments_angles_with_lpa_df <- read_csv("all_possible_lumbar_segment_angles_for_lpa.csv")

# reactlog_enable()

spinal_segments_labels_vector <- c('L5-S1', 'L4-L5', 'L3-L4', 'L2-L3', 'L1-L2', 
                                   'T12-L1', 'T11-T12', 'T10-T11', 'T9-T10', 'T8-T9', 'T7-T8', 'T6-T7', 'T5-T6', 'T4-T5', 'T3-T4', 'T2-T3', 'T1-T2',
                                   'C7-T1', 'C6-C7', 'C5-C6', 'C4-C5', 'C3-C4', 'C2-C3', 'C1-C2')

create_spine_segment_input_function <- function(segment_input_label, segment_value_input){
  segment_id <- paste0("preop_", str_to_lower(str_replace_all(segment_input_label, pattern = "-", "_")), "_segment")
  rigid_segment_id <- str_replace_all(segment_id, "_segment", "_rigid")
  segment_label <- segment_input_label
  initial_value <- segment_value_input
  
  div(
    class = "segment-input",
    span(segment_label, 
         class = "segment-label"),
    div(
      numericInput(inputId = segment_id, 
                   label = NULL, 
                   value = round(initial_value, 0), 
                   step = 1, 
                   width = "45%"),
      class = "custom-numeric-input",
    ),
    div(
      prettyCheckbox(
        inputId = rigid_segment_id,
        label = NULL,
        value = FALSE,
        bigger = TRUE,
        status = "danger",
        shape = "curve"
      )
    )
  )
}

create_spine_rigid_level_input_function <- function(segment_input_label){
  segment_id <- paste0("preop_", str_to_lower(str_replace_all(segment_input_label, pattern = "-", "_")), "_segment")
  rigid_segment_id <- str_replace_all(segment_id, "_segment", "_rigid_xray")
  segment_label <- segment_input_label
  # initial_value <- segment_value_input
  
  div(
    class = "segment-input",
    span(segment_label, 
         class = "segment-label"),
    div(
      prettyCheckbox(
        inputId = rigid_segment_id,
        label = NULL,
        value = FALSE,
        bigger = TRUE,
        status = "danger",
        shape = "curve"
      )
    )
  )
}


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Solaspine"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plot Spine Alignment",
               tabName = "plot_alignment", 
               icon = icon("fas fa-scale-balanced")
               ),
      menuItem("Prescribe Spinal Alignment",
               tabName = "prescribe_alignment", 
               icon = icon("calculator")
               ),
      menuItem("Upload & Spine Alignment",
               tabName = "upload_measure_xray", 
               icon = icon("fas fa-ruler-combined")
      )
    )
  ),
  dashboardBody(
    tags$script(HTML("
  var zoomLevel = 1;
  var panX = 0;
  var panY = 0;
  var isPanning = false;
  var startX, startY;

  function applyTransform() {
    var imgElement = document.querySelector('#image_container img');
    if (imgElement) {
      imgElement.style.transform = 'scale(' + zoomLevel + ') translate(' + panX + 'px, ' + panY + 'px)';
    }
  }

  // Zoom in/out on scroll
  document.getElementById('image_container').addEventListener('wheel', function(event) {
    event.preventDefault();
    if (event.deltaY > 0) {
      zoomLevel *= 0.9; // Zoom out
    } else {
      zoomLevel *= 1.1; // Zoom in
    }
    applyTransform(); // Apply the updated zoom level
  });

  // Start panning on right-click (mousedown)
  document.getElementById('image_container').addEventListener('mousedown', function(event) {
    if (event.button === 2) { // Right-click for panning
      isPanning = true;
      startX = event.clientX;
      startY = event.clientY;
      var imgElement = document.querySelector('#image_container img');
      if (imgElement) {
        imgElement.style.cursor = 'grabbing';
      }
    }
  });

  // Stop panning on mouseup
  document.addEventListener('mouseup', function(event) {
    isPanning = false;
    var imgElement = document.querySelector('#image_container img');
    if (imgElement) {
      imgElement.style.cursor = 'crosshair'; // Reset to crosshair
    }
  });

  // Handle mouse movement for panning
  document.addEventListener('mousemove', function(event) {
    if (isPanning) {
      var deltaX = event.clientX - startX;
      var deltaY = event.clientY - startY;
      panX += deltaX;
      panY += deltaY;
      applyTransform(); // Apply the updated pan
      startX = event.clientX;
      startY = event.clientY;
    }
  });

  // Prevent the right-click menu from showing up
  document.addEventListener('contextmenu', function(event) {
    event.preventDefault();
  });
")),
    tags$style(HTML("
  .segment-input {
    display: flex;
    align-items: center; /* Align items to the center vertically */
    justify-content: space-between; /* Ensure space between label and input */
    margin-bottom: 0px; /* Adjusts the spacing between the inputs */
    font-size: 12px;
  }
  .segment-label {
    margin-right: 5px; /* Slightly increase space for the label */
    white-space: nowrap; /* Prevent labels from wrapping */
    font-size: 12px;
  }
  .segment-input .form-group {
    margin-bottom: 1px; /* Reduce default margin-bottom of form-group */
  }
  .custom-numeric-input {
    padding: 0; /* Remove padding from the numeric input container */
    margin: 0; /* Remove margin from the numeric input container */
    text-align: -webkit-left;
  }
  .custom-numeric-input .form-control {
    padding: 2px 5px; /* Adjust padding inside the numeric input */
    margin-bottom: 0px; /* Ensure no extra margin below the input */
    text-align: -webkit-left;
    width: 50px;
  }
")),
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "plot_alignment",
              fluidRow(
                column(width = 3, 
                       box(width = 12,
                           title = tags$div(style = "font-size:20px; font-weight:bold", "Alignment Parameters"),
                           status = "primary",
                           sliderInput(
                             "pelvic_incidence",
                             "Pelvic Incidence:",
                             min = 25,
                             max = 90,
                             value = 50
                           ),
                           radioGroupButtons(inputId = "plot_lumbar_distribution_using",
                                             label = "Define Lumbar Distribution by:", 
                                             choices = c("L1 Pelvic Angle", "Lordosis by Level"), 
                                             direction = "vertical", 
                                             justified = TRUE, 
                                             selected = "L1 Pelvic Angle"
                           ),
                           conditionalPanel(condition = "input.plot_lumbar_distribution_using == 'L1 Pelvic Angle'", 
                                            sliderInput(
                                              "l1_s1_lordosis",
                                              "L1-S1 Lordosis:",
                                              min = 0,
                                              max = 90,
                                              value = 55
                                            ),
                                            sliderTextInput(
                                              inputId = "l1_pelvic_angle",
                                              label = "L1 Pelvic Angle",
                                              choices = seq(-20, 45, by = 1),
                                              selected =  5
                                            )
                           ),
                           conditionalPanel(condition = "input.plot_lumbar_distribution_using == 'Lordosis by Level'",
                                            column(width = 12,
                                                   sliderInput(
                                                     "l1_sa",
                                                     "L1-S1 Lordosis",
                                                     min = -25,
                                                     max = 90,
                                                     value = 55
                                                   ),
                                                   sliderInput(
                                                     "l2_sa",
                                                     "L2-S1 Lordosis",
                                                     min = -25,
                                                     max = 80,
                                                     value = 55
                                                   ),
                                                   sliderInput(
                                                     "l3_sa",
                                                     "L3-S1 Lordosis",
                                                     min = -25,
                                                     max = 70,
                                                     value = 45
                                                   ),
                                                   sliderInput(
                                                     "l4_sa",
                                                     "L4-S1 Lordosis",
                                                     min = -25,
                                                     max = 60,
                                                     value = 35
                                                   ),
                                                   sliderInput(
                                                     "l5_sa",
                                                     "L5-S1 Lordosis",
                                                     min = -25,
                                                     max = 45,
                                                     value = 20
                                                   ))
                           ),
                           radioGroupButtons(
                             inputId = "thoracic_distribution_format",
                             label = NULL,
                             choices = c("Select Spinal Kyphosis", 
                                         "Select Segmental Thoracic Kyphosis"),
                             selected = "Select Spinal Kyphosis", 
                             direction = "vertical",
                             justified = TRUE
                           ),
                           conditionalPanel(condition = "input.thoracic_distribution_format == 'Select Spinal Kyphosis'",
                                            sliderInput(
                                              "spinal_kyphosis",
                                              "Spinal Kyphosis",
                                              min = -20,
                                              max = 80,
                                              value = 30
                                            ),
                                            sliderTextInput(inputId = "tk_range", 
                                                            label = "Choose Spinal Kyphosis Range",
                                                            choices = c("T12",
                                                                        "T11",
                                                                        "T10",
                                                                        "T9",
                                                                        "T8",
                                                                        "T7",
                                                                        "T6",
                                                                        "T5",
                                                                        "T4",
                                                                        "T3",
                                                                        "T2",
                                                                        "T1"),
                                                            selected = c("T11", "T2")
                                            )
                           ),
                           conditionalPanel(condition = "input.thoracic_distribution_format == 'Select Segmental Thoracic Kyphosis'",
                                            column(width = 10, 
                                                   sliderInput(
                                                     "t12_sa",
                                                     "T12-L1 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 0
                                                   ),
                                                   sliderInput(
                                                     "t11_sa",
                                                     "T11-T12 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 5
                                                   ),
                                                   sliderInput(
                                                     "t10_sa",
                                                     "T10-T11 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 15
                                                   ),
                                                   sliderInput(
                                                     "t9_sa",
                                                     "T9-T10 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 20
                                                   ),
                                                   sliderInput(
                                                     "t8_sa",
                                                     "T8-T9 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 20
                                                   ),
                                                   sliderInput(
                                                     "t7_sa",
                                                     "T7-T8 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 20
                                                   ),
                                                   sliderInput(
                                                     "t6_sa",
                                                     "T6-T7 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 20
                                                   ),
                                                   sliderInput(
                                                     "t5_sa",
                                                     "T5-T6 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 20
                                                   ),
                                                   sliderInput(
                                                     "t4_sa",
                                                     "T4-T5 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 20
                                                   ),
                                                   sliderInput(
                                                     "t3_sa",
                                                     "T3-T4 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 20
                                                   ),
                                                   sliderInput(
                                                     "t2_sa",
                                                     "T2-T3 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 20
                                                   ),
                                                   sliderInput(
                                                     "t1_sa",
                                                     "T1-T2 Kyphosis",
                                                     min = -40,
                                                     max = 40,
                                                     value = 20
                                                   ))
                                            
                           ),
                           h5(strong("Additional Options")),
                           sliderInput(
                             "cervical_lordosis",
                             "Cervical Lordosis",
                             min = -10,
                             max = 30,
                             value = 15
                           ), 
                           radioButtons(inputId = "pt_input",inline = TRUE,
                                        label = "Pelvic Tilt:",
                                        choices = list("Predict PT", "Input PT")),
                           conditionalPanel(condition = "input.pt_input == 'Input PT'",
                                            sliderInput(inputId =  "pt_value_input",
                                                        label = "Pelvic tilt:",
                                                        min = -10,
                                                        max = 60,
                                                        value = 20
                                            )
                           )
                       ),
                       box(width = 12,
                           title = tags$div(style = "font-size:16px; font-weight:bold", "Predict Postop PT"),
                           solidHeader = TRUE, 
                           status = "success",
                           radioButtons(inputId = "pt_prediction_model_uiv",
                                        inline = TRUE,
                                        label = "Predict for:",
                                        choices = c("Upper T UIV" = "upper_t_uiv",
                                                    "Lower T UIV" = "lower_t_uiv"), 
                                        selected = "lower_t_uiv"
                           ),
                           radioButtons(inputId = "pt_prediction_model_choice",
                                        inline = TRUE,
                                        label = "Predict using:",
                                        choices = c("By C2PA & PT" = "c2pa_and_pt",
                                                    "Add L1PA" = "c2pa_and_pt_and_l1pa"), 
                                        selected = "c2pa_and_pt"
                           ),
                           numericInput(inputId = "predict_pt_preop_pt", label = "Preop PT", value = 25),
                           numericInput(inputId = "predict_pt_preop_c2pa", label = "Preop C2PA", value = 20),
                           conditionalPanel(condition = "input.pt_prediction_model_choice == 'c2pa_and_pt_and_l1pa'",
                                            numericInput(inputId = "predict_pt_preop_l1pa", 
                                                         label = "Preop L1PA", value = 15),
                           ),
                           numericInput(inputId = "predict_pt_postop_c2pa", label = "Postop C2PA", value = 11),
                           conditionalPanel(condition = "input.pt_prediction_model_choice == 'c2pa_and_pt_and_l1pa'",
                                            numericInput(inputId = "predict_pt_postop_l1pa", 
                                                         label = "Postop L1PA", value = 15),
                           ),
                           tags$div(style = "font-size:16px; font-weight:bold; color:darkblue; font-family:sans-serif; font-style:italic", 
                                    htmlOutput(outputId = "predicted_pt")
                           )
                       ),
                       box(width = 12,
                           title = tags$div(style = "font-size:14px; font-weight:bold", "Prescribe Postop TL Alignment for Lower T UIV"),
                           solidHeader = TRUE, 
                           status = "success", 
                           collapsible = TRUE,
                           # numericInput(inputId = "prescribe_tl_preop_t10_l2", label = "Preop T10-L2 Lordosis:", value = 25),
                           sliderTextInput(
                             inputId = "prescribe_tl_preop_t10_l2",
                             label = "Preop T10-L2 Lordosis:", 
                             choices = seq(from = -50,
                                           to = 30,
                                           by = 1),
                             grid = TRUE
                           ),
                           numericInput(inputId = "prescribe_tl_preop_l1pa", label = "Preop L1PA", value = 20),
                           numericInput(inputId = "prescribe_tl_preop_l1s1", label = "Preop L1-S1 Lordosis", value = 20),
                           numericInput(inputId = "prescribe_tl_postop_l1pa", label = "Postop L1PA", value = 20),
                           numericInput(inputId = "prescribe_tl_postop_l1s1", label = "Postop L1-S1 Lordosis", value = 20),
                           tags$div(style = "font-size:16px; font-weight:bold; color:darkgreen; font-family:sans-serif; font-style:italic", 
                                    htmlOutput(outputId = "prescribed_t10_l2")
                           )
                       )
                ),
                ######################### SPINE PLOT COLUMN ###########################
                ######################### SPINE PLOT COLUMN ###########################
                ######################### SPINE PLOT COLUMN ###########################
                ######################### SPINE PLOT COLUMN ###########################
                column(width = 6, 
                       box(width = 12, 
                           title = tags$div(style = "font-size:14px; font-weight:bold", "Spinal Alignment"),
                         plotOutput("spine_base", height = 700),
                         span(textOutput("messages_ll"), style="color:red"),
                         downloadButton(outputId = "download_figure", label = "Download the Figure"),
                         downloadButton(outputId = "download_plain_figure", label = "Download Plain Spine Figure")  
                       )
                ),
                
                ######################### PLOT SETTINGS COLUMN ###########################
                ######################### PLOT SETTINGS COLUMN ###########################
                ######################### PLOT SETTINGS COLUMN ###########################
                ######################### PLOT SETTINGS COLUMN ###########################
                column(width = 3, 
                       box(width = 12, title = "Plot Graphics",
                           fixedRow(
                             column(width = 5, 
                                    dropdownButton(
                                      circle = TRUE,label = "Edit plot settings",
                                      icon = icon("gear"),
                                      h5(strong("Plot Settings")),
                                      colorPickr(inputId = "plot_background_color",
                                                 label = "Choose Plot Background Color", 
                                                 selected = "white",
                                                 opacity = TRUE),
                                      # colourpicker::colourInput(
                                      #     inputId = "plot_background_color",
                                      #     label = "Choose Plot Background Color",
                                      #     value = "white",
                                      #     allowTransparent = TRUE
                                      # ),
                                      switchInput("spine_coloring",
                                                  "Change Spine Color with Values",
                                                  labelWidth = "130px", 
                                                  value = FALSE),
                                      h4(strong("Pelvic Lines:")),
                                      colorPickr(inputId = "pi_line_color", label = "Choose PT Line Color", "darkred"),
                                      colorPickr(inputId = "pt_line_color", label = "Choose PT Line Color", "darkviolet"),
                                      switchInput("pt_line_vertex_at_hips",
                                                  "Vertex at hips?",
                                                  labelWidth = "130px", 
                                                  value = TRUE),
                                      colorPickr(inputId = "ss_line_color", label = "Choose SS Line Color", "yellow"),
                                      colorPickr(inputId = "c2_tilt_line_color", label = "Choose C2 Tilt Line Color", "#F090D8"),
                                      h4(strong("Vertebral Pelvic Angle Lines:")),
                                      colorPickr(inputId = "c2pa_line_color",
                                                 label = "Choose C2 Pelvic Angle Line Color", 
                                                 selected = "darkgreen"),
                                      colorPickr(inputId = "t1pa_line_color", label = "Choose T1PA Line Color", "darkblue"),
                                      colorPickr(inputId = "t9pa_line_color", label = "Choose T9 Pelvic Angle Line Color", "#CC79A7"),
                                      colorPickr(inputId = "t4pa_line_color", label = "Choose T4 Pelvic Angle Line Color", "purple"),
                                      colorPickr(inputId = "l1pa_line_color", label = "Choose L1 Pelvic Angle Line Color", "blue"),
                                      colorPickr(inputId = "t1_c2_ha_line_color", label = "Choose T1-C2-Hip Angle Line Color", "orange"),
                                      colorPickr(inputId = "t9_c2_ha_line_color", label = "Choose T9-C2-Hip Angle Line Color", "darkorange"),
                                      h4(strong("Sagittal Cobb Angle Lines:")),
                                      colorPickr(inputId = "tk_line_color", label = "Choose TK Line Color", "blue"),
                                      br(),
                                      switchInput("lines_posterior",
                                                  "Show Lordosis Lines Posteriorly",
                                                  labelWidth = "130px", value = TRUE),
                                      colorPickr(inputId = "l1s1_line_color", label = "Choose L1-S1 Color", "#00B97A"),
                                      sliderInput(
                                        "l1l4_angle_line_length",
                                        "L1-L4 Angle Display Length",
                                        min = 0,
                                        max = 30,
                                        value = 10
                                      ),
                                      colorPickr(inputId = "l1l4_line_color", label = "Choose L1-L4 Line Color", "darkred"),
                                      br(),
                                      sliderInput(
                                        "l4s1_angle_line_length",
                                        "L4-S1 Angle Display Length",
                                        min = 0,
                                        max = 30,
                                        value = 10
                                      ),
                                      colorPickr(inputId = "l4s1_line_color", label = "Choose L4-S1 Line Color", "#00754D"),
                                      sliderInput(
                                        "posterior_lordosis_line_lengths",
                                        "Posterior Lordosis Line Lengths",
                                        min = 0,
                                        max = 70,
                                        value = 30
                                      )
                                    )
                             ),
                             column(width = 7, 
                                    h4("Plot Settings"))
                           ),
                           switchInput("cone_of_economy_show",
                                       "Show Cone of Economy",
                                       labelWidth = "130px", 
                                       value = FALSE),
                           switchInput("c2_tilt_line_show",
                                       "Show C2 Tilt",
                                       labelWidth = "130px"),
                           h4("Pelvic Angles:"),
                           switchInput("c2pa_line_show",
                                       "Show C2 Pelvic Angle",
                                       labelWidth = "130px"),
                           switchInput("t1pa_line_show",
                                       "Show T1PA",
                                       labelWidth = "130px"),
                           switchInput("t4pa_line_show",
                                       "Show T4PA",
                                       labelWidth = "130px"),
                           switchInput("t9pa_line_show",
                                       "Show T9PA",
                                       labelWidth = "130px"),
                           switchInput("l1pa_line_show",
                                       "Show L1PA",
                                       labelWidth = "130px"),
                           switchInput("t1_c2_ha_line_show",
                                       "Show T1-C2-HA",
                                       labelWidth = "130px"),
                           switchInput("t9_c2_ha_line_show",
                                       "Show T9-C2-HA",
                                       labelWidth = "130px"),
                           h4("Sagittal Cobb Angles:"),
                           switchInput("tk_line_show",
                                       "Show TK",
                                       labelWidth = "130px"),
                           switchInput("l1s1_line_show",
                                       "Show L1-S1 Angle",
                                       labelWidth = "130px"),
                           # switchInput("l1l4_line_show",
                           #             "Show L1-L4 Angle",
                           #             labelWidth = "130px"),
                           switchInput("l4s1_line_show",
                                       "Show L4-S1 Angle",
                                       labelWidth = "130px"),
                           h4("Pelvic Parameters:"),
                           switchInput("pt_line_show",
                                       "Show PT",
                                       labelWidth = "130px"),
                           switchInput("ss_line_show",
                                       "Show SS",
                                       labelWidth = "130px"),
                           switchInput("pi_line_show",
                                       "Show PI",
                                       labelWidth = "130px")
                       )
                )
              )
      ),
      tabItem(tabName = "prescribe_alignment",
              column(width = 2,
                     style = "primary",
                       box(width = 12,
                           h3(strong("Patient Factors:")),
                           sliderInput(
                             "preop_age",
                             "Patient Age:",
                             min = 18,
                             max = 90,
                             value = 60
                           ),
                           radioGroupButtons(
                             inputId = "preop_sex",
                             label = "Sex:",
                             choices = c("Male", "Female"),
                             selected = "Female"
                           ),
                           hr(),
                           h3(strong("Pre-operative Alignment:")),
                           numericInput(
                             inputId = "preop_pelvic_incidence", 
                             label = "Pelvic Incidence", 
                             value = 64,
                             min = 25, 
                             max = 110
                           ),
                           numericInput(
                             "preop_pt",
                             "Preop Pelvic Tilt:",
                             min = -15,
                             max = 60,
                             value = 42
                           ),
                           numericInput(
                             "preop_l1s1",
                             "Preop L1-S1:",
                             min = -60,
                             max = 120,
                             value = 20
                           ),
                           numericInput(
                             "preop_t10_l2",
                             "Preop T10-L2 Lordosis:",
                             min = -75,
                             max = 30,
                             value = -10
                           ),
                           numericInput(
                             "preop_l1pa",
                             "Preop L1 Pelvic Angle:",
                             min = -15,
                             max = 45,
                             value = 25
                           ),
                           numericInput(
                             "preop_t9pa",
                             "Preop T9 Pelvic Angle:",
                             min = -10,
                             max = 40,
                             value = 30
                           ),
                           numericInput(
                             "preop_t4pa",
                             "Preop T4 Pelvic Angle:",
                             min = -15,
                             max = 70,
                             value = 32
                           ),
                           numericInput(
                             "preop_c2pa",
                             "Preop C2 Pelvic Angle:",
                             min = 0,
                             max = 80,
                             value = 36
                           ),
                           numericInput(
                             "preop_c2c7",
                             "Preop C2-C7 Lordosis:",
                             min = -45,
                             max = 80,
                             value = 15
                           ),
                           br(), 
                           # awesomeCheckboxGroup(
                           #   inputId = "rigid_lumbar_levels",
                           #   label = "Select any Rigid Lumbar Levels", 
                           #   # choices = c("T10-T11", "T11-T12", "T12-L1", "L1-L2", "L2-L3", "L3-L4", "L4-L5", "L5-S1"),
                           #   choices = c("L5-S1", "L4-L5", "L3-L4", "L2-L3", "L1-L2", "T12-L1", "T11-T12", "T10-T11"), 
                           #   inline = TRUE,
                           #   status = "danger"
                           # ),
                           hr()
                           # actionBttn(
                           #   inputId = "build_preop_spine",
                           #   label = "Simulate Preop Spine",
                           #   style = "unite", 
                           #   color = "success"
                           # ),
                           # h5("(May take up to 20 seconds)")
                           )
                     
                     ), 
              ########################### PREOP ALIGNMENT PLOT ############################
              ########################### PREOP ALIGNMENT PLOT ############################
              ########################### PREOP ALIGNMENT PLOT ############################
              ########################### PREOP ALIGNMENT PLOT ############################
              ########################### PREOP ALIGNMENT PLOT ############################
              column(width = 4,
                     uiOutput(outputId = "preop_segment_angles_ui"), 
                     column(width = 7, 
                            plotOutput(outputId = "preop_spine_alignment", height = 800),
                            textOutput(outputId = "rigid_levels_text")
                     )
                     
              ), 
              ########################### PRESCRIBED ALIGNMENT PLOT ############################
              ########################### PRESCRIBED ALIGNMENT PLOT ############################
              ########################### PRESCRIBED ALIGNMENT PLOT ############################
              ########################### PRESCRIBED ALIGNMENT PLOT ############################
              ########################### PRESCRIBED ALIGNMENT PLOT ############################
              column(width = 6, 
                     actionBttn(
                       inputId = "compute_plan",
                       label = "Compute Plan",
                       style = "unite", 
                       color = "danger"
                     ),
                     h3("Prescribed Alignment:"),
                     fluidRow(
                       plotOutput(outputId = "spine_plan_lower_t", height = 650),
                     ),
                     hr(),
                     fluidRow(
                       plotOutput(outputId = "spine_plan_upper_t", height = 650),
                     )
              ),
              fluidRow(
                tableOutput(outputId = "measures_table")
              )
              # fluidRow(
              #   plotOutput(outputId = "preop_risk_plot", height = 400)
              # )
      ),
      tabItem(tabName = "upload_measure_xray",
              column(width = 1, 
                     fileInput(inputId = "xray_file", "Upload an Xray", capture = "environment", accept = "image/*"),
                     br(),
                     conditionalPanel(
                       condition = "input.xray_file_uploaded == true",  # Show only if xray_file is not null
                       fluidRow(
                         h6("Click to set orientation:"),
                         prettyToggle(
                           inputId = "xray_orientation",
                           fill = TRUE,
                           value = TRUE,
                           status_on = "info", 
                           status_off = "info",
                           bigger = TRUE, 
                           width = "100%", 
                           shape = "curve",
                           label_on = "Facing Left", 
                           label_off = "Facing Right",
                           outline = FALSE,
                           icon_on = icon("arrow-left"),
                           icon_off = icon("arrow-right")
                         ),
                         div(
                           style = "text-align: center;",  # Center the image horizontally
                           uiOutput("icon_xray_orientation")  # Replaces imageOutput
                         ),
                         uiOutput(outputId = "preop_xray_rigid_segments_ui")
                       ),
                       br(),
                       br(),
                       switchInput(inputId = "xray_input_all_centroids", 
                                   # label = "Input all spine centroids",
                                   label = NULL,
                                   size = "mini",
                                   value = FALSE,
                       ),
                       br()
                     ),
                     div(
                       style = "display: none;",  # Hide the entire div, including the switch
                     switchInput(
                       inputId = "xray_file_uploaded",
                       size = "mini", label = NULL,
                       value = FALSE, 
                       onLabel = "Y", 
                       offLabel = "N",
                     ),
                     switchInput(
                       inputId = "all_centroids_recorded",
                       size = "mini",
                       label = NULL,
                       value = FALSE, 
                       onLabel = "Y", 
                       offLabel = "N",
                     )
                     # all_centroids_recorded
                     )
              ),   
              conditionalPanel(
                condition = "input.xray_file_uploaded == true",
                column(width = 3, 
                       box(title = NULL, collapsible = FALSE, width = 12, 
                           tags$div(
                             style = 
                               "font-size: 20px; 
                       font-weight: bold; 
                       color: yellow; 
                       font-family: arial; 
                       font-style: italic; 
                       text-align: center; 
                       background-color: black; 
                       padding: 3px; 
                       border-radius: 12px;  /* Rounded corners */
                         display: block;"
                             , 
                             htmlOutput(outputId = "xray_click_instructions")
                           ), 
                           # uiOutput(outputId = "xray_plot_ui"),
                           div(
                             id = "image_container",
                             plotOutput(
                               outputId = "xray",
                               click = "xray_click",  # Clicks are still processed by Shiny
                               height = "auto",  # Set to 100% to fill the container
                               width = "100%"    # Set to 100% to fill the container
                             ),
                             
                             # Style for container and image
                             tags$style(HTML("
      #image_container {
        overflow: hidden;
        width: 100%;
        height: 750px;
        position: relative;
        display: flex;
      }
      #image_container img {
        transition: transform 0.15s ease;
        cursor: crosshair;
              width: 100%;  /* Ensure the image fills the width of the container */
      height: auto; /* Maintain aspect ratio */
      }
      #image_container img:active {
        cursor: grabbing;
      }
    ")),
                             
                             # JavaScript to handle zoom and pan on the client side
                             tags$script(HTML("
      var zoomLevel = 1;
      var panX = 0;
      var panY = 0;
      var isPanning = false;
      var startX, startY;

      function applyTransform() {
        var imgElement = document.querySelector('#image_container img');
        if (imgElement) {
          imgElement.style.transform = 'scale(' + zoomLevel + ') translate(' + panX + 'px, ' + panY + 'px)';
        }
      }

      // Zoom in/out on scroll
      document.getElementById('image_container').addEventListener('wheel', function(event) {
        event.preventDefault();
        if (event.deltaY > 0) {
          zoomLevel *= 0.9; // Zoom out
        } else {
          zoomLevel *= 1.1; // Zoom in
        }
        applyTransform(); // Apply the updated zoom level
      });

      // Start panning on right-click (mousedown)
      document.getElementById('image_container').addEventListener('mousedown', function(event) {
        if (event.button === 2) { // Right-click for panning
          isPanning = true;
          startX = event.clientX;
          startY = event.clientY;
          var imgElement = document.querySelector('#image_container img');
          if (imgElement) {
            imgElement.style.cursor = 'grabbing';
          }
        }
      });

      // Stop panning on mouseup
      document.addEventListener('mouseup', function(event) {
        isPanning = false;
        var imgElement = document.querySelector('#image_container img');
        if (imgElement) {
          imgElement.style.cursor = 'crosshair'; // Reset to crosshair
        }
      });

      // Handle mouse movement for panning
      document.addEventListener('mousemove', function(event) {
        if (isPanning) {
          var deltaX = event.clientX - startX;
          var deltaY = event.clientY - startY;
          panX += deltaX;
          panY += deltaY;
          applyTransform(); // Apply the updated pan
          startX = event.clientX;
          startY = event.clientY;
        }
      });

      // Prevent the right-click menu from showing up
      document.addEventListener('contextmenu', function(event) {
        event.preventDefault();
      });
    "))
                           ),
                           fluidRow(
                             column(width = 7, 
                                    actionBttn(inputId = "xray_delete_last_point", 
                                               size = "sm",
                                               label = "Delete Last",
                                               style = "jelly", 
                                               color = "success",
                                               icon = icon("delete-left")
                                    )
                             ),
                             column(width = 5, 
                                    actionBttn(
                                      size = "sm",
                                      inputId = "xray_reset_points",
                                      label = "Reset",
                                      style = "unite", 
                                      color = "danger",
                                      icon = icon("trash-can")
                                    )
                             ))
                     )
                )),
              column(width = 1, 
                     conditionalPanel(
                       condition = "input.xray_file_uploaded == true",
                     tableOutput(outputId = "alignment_parameters_df")
                     )
                     ),
              column(width = 2, 
                     conditionalPanel(
                       condition = "input.xray_file_uploaded == true",
                     uiOutput(outputId = "preop_spine_simulation_xray_ui") 
                     )
                     ),
              conditionalPanel(
                condition = "input.xray_file_uploaded == true",
              column(width = 4, 
                     actionBttn(
                       inputId = "compute_plan_xray",
                       label = "Compute Plan",
                       style = "unite", 
                       color = "danger"
                     ),
                     # h3("Prescribed Alignment:"),
                     fluidRow(
                       plotOutput(outputId = "spine_plan_lower_t_xray", height = 650),
                     ),
                     hr(),
                     fluidRow(
                       plotOutput(outputId = "spine_plan_upper_t_xray", height = 650),
                     )
              )
              )
      )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    ######################################## ASSEMBLE REACTIVE UI'S   ########################################
    ######################################## ASSEMBLE REACTIVE UI'S   ########################################
    #   "predict_pt_preop_pt", label = "Preop PT"),
    # numericInput(inputId = "predict_pt_preop_c2pa", label = "Preop C2PA"),
    # numericInput(inputId = "predict_pt_postop_c2pa", label = "Postop C2PA"),
    # textOutput(outputId = "predicted_pt")
    ### predicting PT
    output$predicted_pt <- renderText({
      
      #### PREDICTION FUNCTIONS ###
        
        
        #### PREDICTION FUNCTIONS END ###
        
        if(input$pt_prediction_model_uiv == "upper_t_uiv"){
          predicting_pt_function <- function(postop_c2pa = 21.298135,
                                             preop_c2pa = 28.33507,
                                             preop_pt = 26.092608) {
            1.4817532+0.77386313*postop_c2pa-0.31861799*preop_c2pa+0.49868069*preop_pt }
          
          predicted_pt <- predicting_pt_function(postop_c2pa = input$predict_pt_postop_c2pa, preop_c2pa = input$predict_pt_preop_c2pa, preop_pt = input$predict_pt_preop_pt)
          
        }else{
          if(input$pt_prediction_model_choice == "c2pa_and_pt"){
            predicting_pt_lower_t_uiv_by_c2pa_pt_function <- function(preop_pt = 24.168039,
                                                                      preop_c2pa = 25.165139,
                                                                      postop_c2pa = 18.427933
            ) {
              predicted_pt <- -0.16693402+0.60333401* preop_pt-0.0004011702*pmax(preop_pt-14.610783,0)^3+0.00073921326*pmax(preop_pt-24.168039,0)^3-0.00033804306*pmax(preop_pt-35.510044,0)^3-0.39091716* preop_c2pa+0.0002074262*pmax(preop_c2pa-13.487531,0)^3-0.00036754744*pmax(preop_c2pa-25.165139,0)^3+0.00016012124*pmax(preop_c2pa-40.292689,0)^3+0.79018266*postop_c2pa 
              
              round(predicted_pt, 1)
            }
            predicted_pt <- predicting_pt_lower_t_uiv_by_c2pa_pt_function(preop_pt = input$predict_pt_preop_pt, 
                                                                               preop_c2pa = input$predict_pt_preop_c2pa, 
                                                                               postop_c2pa = input$predict_pt_postop_c2pa
            )
          
          }else if(input$pt_prediction_model_choice == "c2pa_and_pt_and_l1pa"){
            predicting_pt_lower_t_uiv_by_c2pa_pt_l1pa_function <- function(preop_pt = 24.168039,
                                                                           preop_c2pa = 25.165139,
                                                                           preop_l1pa = 12.159217,
                                                                           postop_l1pa = 9.1099133,
                                                                           postop_c2pa = 18.427933) {
              predicted_pt <- 0.17891039 + 0.57925383* preop_pt-0.00036858808*pmax(preop_pt-14.610783,0)^3+0.00067917607*pmax(preop_pt-24.168039,0)^3-0.00031058799*pmax(preop_pt-35.510044,0)^3-0.29549706* preop_c2pa+0.00019181124*pmax(preop_c2pa-13.487531,0)^3-0.00033987862*pmax(preop_c2pa-25.165139,0)^3+0.00014806738*pmax(preop_c2pa-40.292689,0)^3-0.14545127*preop_l1pa+0.22297945*postop_l1pa+
                0.66158104*postop_c2pa 
              
              round(predicted_pt, 1)
            }
            
            predicted_pt <- predicting_pt_lower_t_uiv_by_c2pa_pt_l1pa_function(preop_pt = input$predict_pt_preop_pt, 
                                                                               preop_c2pa = input$predict_pt_preop_c2pa, 
                                                                               preop_l1pa = input$predict_pt_preop_l1pa, 
                                                                               postop_l1pa = input$predict_pt_postop_l1pa,
                                                                               postop_c2pa = input$predict_pt_postop_c2pa
            ) 
        }

        
          }
        
        paste("Predicted PT = ", round(predicted_pt), "º", sep = "")
        
    })
    
    ############ PRESCRIBING T10-L2 ALIGNMENT
    output$prescribed_t10_l2 <- renderText({
      
      prescribe_t10_l2_function <- function(preop_t10_l2 = -6,
               preop_l1pa = 12.159217,
               postop_l1pa = 9.1099133,
               preop_l1s1 = 36.3,
               postop_l1s1 = 53.5) {
        prescribed_t10_l2_lordosis <- -11.180538+0.4917326*preop_t10_l2-1.0704503*preop_l1pa+1.3448489*postop_l1pa-0.38980884*preop_l1s1+0.42280467*postop_l1s1 
        prescribed_t10_l2_lordosis
      }
      
      prescribed_t10_l2_value <- prescribe_t10_l2_function(preop_t10_l2 = input$prescribe_tl_preop_t10_l2,
                                  preop_l1pa = input$prescribe_tl_preop_l1pa,
                                postop_l1pa = input$prescribe_tl_postop_l1pa,
                                preop_l1s1 = input$prescribe_tl_preop_l1s1, 
                                postop_l1s1 = input$prescribe_tl_postop_l1s1)
      
      if(prescribed_t10_l2_value < 0){
        paste("Prescribed T10-L2 Kyphosis = ", round(prescribed_t10_l2_value*-1), "º", sep = "")
      }else{
        paste("Prescribed T10-L2 Lordosis = ", round(prescribed_t10_l2_value), "º", sep = "")  
      }
      
    })
    
    #########################

    
    ### DEFINE THE DATAFRAME WITH THE LUMBAR SEGMENT ANGLES
    lumbar_segment_angles_reactive_df <- reactive({
        all_possible_lumbar_segments_angles_with_lpa_df %>%
            filter(pelvic_incidence == input$pelvic_incidence, l1_s1 == req(input$l1_s1_lordosis)) %>%
            arrange(lpa)
    }) 
    
    observe({
        starting_lpa <- round(-5.66118950268869 + (input$pelvic_incidence * 0.655589171809974) +
                                  (req(input$l1_s1_lordosis) * -0.378435237176394), 0)

        updateSliderTextInput(session = session,
                              inputId = "l1_pelvic_angle",
                              label = "L1 Pelvic Angle",
                              selected = starting_lpa
                              # choices = seq(min(lumbar_segment_angles_reactive_df()$lpa), max(lumbar_segment_angles_reactive_df()$lpa), by = 1)
                              )
    })
    
    
    
    ######################################## ASSEMBLE SEGMENT ANGLES   ########################################
    ######################################## ASSEMBLE SEGMENT ANGLES   ########################################
    segment_angle_list_reactive <- reactive({
        
        pelv_inc_value <- input$pelvic_incidence
        cervical_lordosis <- input$cervical_lordosis
        spinal_kyphosis_input = as.integer(input$spinal_kyphosis)
        
        tk_lower_input <- case_when(
            input$tk_range[1] == "T12"~ 0,
            input$tk_range[1] == "T11" ~ 1,
            input$tk_range[1] == "T10" ~ 2,
            input$tk_range[1] == "T9" ~ 3,
            input$tk_range[1] == "T8" ~ 4,
            input$tk_range[1] == "T7" ~ 5,
            input$tk_range[1] == "T6" ~ 6,
            input$tk_range[1] == "T5" ~ 7,
            input$tk_range[1] == "T4" ~ 8,
            input$tk_range[1] == "T3" ~ 9,
            input$tk_range[1] == "T2" ~ 10,
            input$tk_range[1] == "T1" ~ 11)
        
        tk_upper_input <- case_when(
            input$tk_range[2] == "T12"~ 0,
            input$tk_range[2] == "T11" ~ 1,
            input$tk_range[2] == "T10" ~ 2,
            input$tk_range[2] == "T9" ~ 3,
            input$tk_range[2] == "T8" ~ 4,
            input$tk_range[2] == "T7" ~ 5,
            input$tk_range[2] == "T6" ~ 6,
            input$tk_range[2] == "T5" ~ 7,
            input$tk_range[2] == "T4" ~ 8,
            input$tk_range[2] == "T3" ~ 9,
            input$tk_range[2] == "T2" ~ 10,
            input$tk_range[2] == "T1" ~ 11)
        
        
        if(input$plot_lumbar_distribution_using == "L1 Pelvic Angle"){
            
            l1_s1_lordosis_input <- if_else(is.null(input$l1_s1_lordosis), as.integer(60), input$l1_s1_lordosis)
            
            l1_pelvic_angle <- if_else(is.null(input$l1_pelvic_angle), as.integer(4), input$l1_pelvic_angle)
            
            # lumbar_alignment_df <- all_possible_lumbar_segments_angles_with_lpa_df %>%
            #     filter(pelvic_incidence == pelv_inc_value) %>%
            #     filter(l1_s1 == l1_s1_lordosis_input) %>%
            #     filter(lpa == l1_pelvic_angle)
            
            # lumbar_segment_angle_function
            
            
            segment_angle_list <- segment_angle_function_using_lpa(l1pa = l1_pelvic_angle, 
                                                                   pelvic_incidence = pelv_inc_value,
                                                                   l1_s1_lordosis = l1_s1_lordosis_input, 
                                                                   spinal_kyphosis = spinal_kyphosis_input,
                                                                   tk_upper = tk_upper_input, 
                                                                   tk_lower = tk_lower_input,
                                                                   # lumbar_all_combinations_df = lumbar_alignment_df,
                                                                   add_tk = 0)
            
            
        }
        
        
        if(input$plot_lumbar_distribution_using == "Lordosis by Level"){
            if(input$thoracic_distribution_format == "Select Segmental Thoracic Kyphosis"){
                segment_angle_list <- list(l5_segment_angle = (input$l5_sa),
                                           l4_segment_angle = (input$l4_sa - input$l5_sa),
                                           l3_segment_angle = (input$l3_sa - input$l4_sa),
                                           l2_segment_angle = (input$l2_sa - input$l3_sa),
                                           l1_segment_angle = (input$l1_sa - input$l2_sa),
                                           t12_segment_angle = if_else(is.null(input$t12_sa), as.integer(0), input$t12_sa*-1),
                                           t11_segment_angle = if_else(is.null(input$t11_sa), as.integer(0), input$t11_sa*-1),
                                           t10_segment_angle = if_else(is.null(input$t10_sa), as.integer(0), input$t10_sa*-1),
                                           t9_segment_angle = if_else(is.null(input$t9_sa), as.integer(0), input$t9_sa*-1),
                                           t8_segment_angle = if_else(is.null(input$t8_sa), as.integer(0), input$t8_sa*-1),
                                           t7_segment_angle = if_else(is.null(input$t7_sa), as.integer(0), input$t7_sa*-1),
                                           t6_segment_angle = if_else(is.null(input$t6_sa), as.integer(0), input$t6_sa*-1),
                                           t5_segment_angle = if_else(is.null(input$t5_sa), as.integer(0), input$t5_sa*-1),
                                           t4_segment_angle = if_else(is.null(input$t4_sa), as.integer(0), input$t4_sa*-1),
                                           t3_segment_angle = if_else(is.null(input$t3_sa), as.integer(0), input$t3_sa*-1),
                                           t2_segment_angle = if_else(is.null(input$t2_sa), as.integer(0), input$t2_sa*-1),
                                           t1_segment_angle = if_else(is.null(input$t1_sa), as.integer(0), input$t1_sa*-1)
                )
                
            }else{
                
                lumbar_segment_angles_list <- list(l1_segment_angle = (input$l1_sa - input$l2_sa),
                                                   l2_segment_angle = (input$l2_sa - input$l3_sa),
                                                   l3_segment_angle = (input$l3_sa - input$l4_sa),
                                                   l4_segment_angle = (input$l4_sa - input$l5_sa),
                                                   l5_segment_angle = (input$l5_sa))
                
                
                segment_angle_list <-  segment_angle_function_using_lumbar_segment_angles(pelvic_incidence = pelv_inc_value, 
                                                                                          defined_lumbar_segment_angles_list = lumbar_segment_angles_list, 
                                                                                          spinal_kyphosis = spinal_kyphosis_input, 
                                                                                          tk_upper = tk_upper_input, 
                                                                                          tk_lower = tk_lower_input, 
                                                                                          add_tk = 0)
            }
            
            
            
        }
        
        return(segment_angle_list)
        
    })
    
    ################ UPDATE PROXIMAL VPAs AS THEY CHANGE, TO PREVENT LIKELY FREEZES

    
    ######################################## BUILD SPINE FUNCTION ########################################
    ######################################## BUILD SPINE FUNCTION  ########################################
    
    ### first compute relevant pelvic angles and then determine predicted PT. MUST DO THIS FIRST. You first set PT = 0 so that pelvic angles can be measured, then predict PT, then build the real plot
    
    
    c2pa_pt_predicted_list_reactive <- reactive({
        # jh_compute_pelvic_angles_and_pt_function(pelvic_incidence_start = input$pelvic_incidence,
        #                                          segment_angle_list_start = segment_angle_list_reactive(), 
        #                                          # l1pa_start = input$l1_pelvic_angle,
        #                                          cervical_lordosis_start = input$cervical_lordosis)
      
      build_spine_for_checking_pelvic_angles_function(pelv_inc_value = input$pelvic_incidence,
                                                      segment_angle_list = segment_angle_list_reactive())
      
    })
    
    #################### NEW ATTEMPT
    
    build_spine_reactive_function <- function(){
        reactive({
            
            pelv_inc_value <- input$pelvic_incidence
            cervical_lordosis <- input$cervical_lordosis
            spinal_kyphosis <- input$spinal_kyphosis
            
            segment_angle_list <- segment_angle_list_reactive()
            
            
            l1_l4_lordosis <- segment_angle_list$l1_segment_angle + segment_angle_list$l2_segment_angle + segment_angle_list$l3_segment_angle
            l4_s1_lordosis <- segment_angle_list$l4_segment_angle + segment_angle_list$l5_segment_angle 
            l1_s1_lordosis <- l1_l4_lordosis + l4_s1_lordosis
            
            l1_s1 = l4_s1_lordosis + segment_angle_list$l1_segment_angle + segment_angle_list$l2_segment_angle + segment_angle_list$l3_segment_angle
            
            l4s1_angle_line_length <- input$l4s1_angle_line_length
            l1l4_angle_line_length <- input$l1l4_angle_line_length
            
            if(input$pt_input == "Input PT"){
              pt_value <- input$pt_value_input
            }else{
              # pt_value <- c2pa_pt_predicted_list_reactive()$predicted_pt
              pt_value <- predict_pt_function(pelvic_incidence = pelv_inc_value,
                                              t1_l1 = spinal_kyphosis*-1, 
                                              l1_s1 = input$l1_s1_lordosis, 
                                              l1_pelvic_angle = input$l1_pelvic_angle)
            }
            ss_value <- pelv_inc_value - pt_value
            
            spine_build_list <- build_full_spine_function_new(pelv_inc_value = pelv_inc_value,
                                                              segment_angle_list = segment_angle_list,  ### CURRENTLY THE SEGMENT ANGLE LIST DOESNT INCLUDE CERVICAL
                                                              cervical_lordosis = cervical_lordosis, 
                                                              pt_value = pt_value,
                                                              spine_faces = "right", 
                                                              posterior_line_length = input$posterior_lordosis_line_lengths)
            
            
            ### OFFSETS AND COLORS
            ### #### ####
            t1pa_value <- spine_build_list$spine_list$t1pa_value
            
            l1_s1_lordosis_offset <- abs(l1_s1_lordosis - (33 + pelv_inc_value*0.6))
            
            spinal_kyphosis_offset <- abs(spinal_kyphosis - (46.7+ -pelv_inc_value*0.05))
            
            sacral_slope_offset <- abs(ss_value - (8.3 + pelv_inc_value*0.62))
            
            t1pa_offset <- abs(t1pa_value - (-13.6+ pelv_inc_value*0.38))
            
            colors <- c("grey90", "#C28E8E", "#BD4C4C", "#E64343", "#FF0000")
            
            l1_s1_lordosis_color <- case_when(
                l1_s1_lordosis_offset < 12 ~ colors[1],
                between(l1_s1_lordosis_offset, 12, 15) ~ colors[2],
                between(l1_s1_lordosis_offset, 15, 21) ~ colors[3],
                between(l1_s1_lordosis_offset, 21, 26) ~ colors[4],
                l1_s1_lordosis_offset > 26 ~ colors[5]
            )
            
            spinal_kyphosis_color <- case_when(
                spinal_kyphosis_offset < 15 ~ colors[1],
                between(spinal_kyphosis_offset, 15, 20) ~ colors[2],
                between(spinal_kyphosis_offset, 20, 24) ~ colors[3],
                between(spinal_kyphosis_offset, 24, 30) ~ colors[4],
                spinal_kyphosis_offset > 30 ~ colors[5]
            )
            
            sacral_slope_color <- case_when(
                sacral_slope_offset < 9 ~ colors[1],
                between(sacral_slope_offset, 9, 13) ~ colors[2],
                between(sacral_slope_offset, 13, 16) ~ colors[3],
                between(sacral_slope_offset, 16, 19) ~ colors[4],
                sacral_slope_offset > 19 ~ colors[5]
            )
            
            ##################### CONE OF ECONOMY LINES AND REGIONS ###################################
            
            ######################### CONSTRUCT CONE OF ECONOMY AND COLORS ######################
            fem_head_center <- st_coordinates(st_centroid(spine_build_list$spine_list$fem_head_geom))
            #C2 Tilt Cone
            c2_quant_05 <- -6.4268
            c2_iqr_25 <- -4.4364
            c2_iqr_75 <- -1.0874
            c2_quant_95 <- 1.6953
            
            cone_point_ant <- c(fem_head_center[1] + tan((pi / 180) * (c2_iqr_75))*100,
                                fem_head_center[2] + 100)
            
            cone_point_post <- c(fem_head_center[1] + tan((pi / 180) * (c2_iqr_25))*100,
                                 fem_head_center[2] + 100)
            
            cone_of_economy_line1_sf <-st_linestring(rbind(fem_head_center, cone_point_ant))
            cone_of_economy_line2_sf <-st_linestring(rbind(fem_head_center, cone_point_post))
            
            cone_of_economy_sf <- st_polygon(list(rbind(fem_head_center, 
                                                        cone_point_ant, 
                                                        cone_point_post, 
                                                        fem_head_center)))
            
            
            cone_of_economy_line_sf <- st_multilinestring(list(cone_of_economy_line1_sf, cone_of_economy_line2_sf))
            
            anterior_to_cone_sf <- st_polygon(list(rbind(fem_head_center,
                                                         cone_point_ant, 
                                                         c(fem_head_center[[1]] - 30,
                                                           fem_head_center[[2]] + 100),
                                                         fem_head_center)))
            
            posterior_to_cone_sf <-  st_polygon(list(rbind(fem_head_center, 
                                                           cone_point_post,
                                                           c(fem_head_center[[1]] + 30,
                                                             fem_head_center[[2]] + 100), 
                                                           fem_head_center)))
            cone_color <- case_when(
                spine_build_list$spine_list$c2_tilt_value < c2_quant_05 ~ "red",
                between(spine_build_list$spine_list$c2_tilt_value, c2_quant_05, c2_iqr_25) ~ "yellow",
                between(spine_build_list$spine_list$c2_tilt_value, c2_iqr_25, c2_iqr_75) ~ "green",
                between(spine_build_list$spine_list$c2_tilt_value, c2_iqr_75, c2_quant_95) ~ "yellow",
                spine_build_list$spine_list$c2_tilt_value > c2_quant_95 ~ "red"
            )

            ######################### END ######################
            
            measurements_list <- list()
            measurements_list$"C2 Tilt" <- round(spine_build_list$spine_list$c2_tilt_value)
            measurements_list$"C2PA" <- round(spine_build_list$spine_list$c2pa_value, 0)
            measurements_list$"T1PA" <- round(spine_build_list$spine_list$t1pa_value, 0)
            measurements_list$"T4PA" <- round(spine_build_list$spine_list$t4pa_value, 0)
            measurements_list$"L1PA" <- round(spine_build_list$spine_list$l1pa_value, 0)
            measurements_list$"LL" <-  round(l1_s1, 0)
            measurements_list$"L1-L4" <- round(l1_l4_lordosis, 0)
            measurements_list$"L4-S1" <- round(l4_s1_lordosis, 0)
            measurements_list$"TK" <- round(spine_build_list$spine_list$thoracic_kyphosis, 0)
            measurements_list$"SS" <- round(ss_value, 0)
            measurements_list$"PT" <- round(pt_value, 0)
            measurements_list$"PI-LL" <- round(pelv_inc_value - l1_s1, digits = 0)
            measurements_list$"PI" <- pelv_inc_value
            
            measurements_df <- enframe(measurements_list) %>%
                unnest() %>%
                mutate(x = if_else(name == "PI", 0, -28)) %>%
                mutate(y = c(seq(from = 50, by = -4, length = length(measurements_list)-1), 3)) %>%
              mutate(y = if_else(name == "PI", -4, y)) %>%
                mutate(label = paste(name, value, sep = " = ")) 
            
            # facing_direction <- if_else(input$face_right == TRUE, -1, 1)
            
            vert_labels_df <- tibble(
                vert_level =
                    c(
                        "L5",
                        "L4",
                        "L3",
                        "L2",
                        "L1",
                        "T12",
                        "T11",
                        "T10",
                        "T9",
                        "T8",
                        "T7",
                        "T6",
                        "T5",
                        "T4",
                        "T3",
                        "T2",
                        "T1"
                    ),
                x = c(
                    spine_build_list$vertebral_body_list$l5_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$l4_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$l3_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$l2_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$l1_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t12_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t11_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t10_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t9_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t8_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t7_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t6_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t5_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t4_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t3_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t2_list$vert_body_center_sf[[1]],
                    spine_build_list$vertebral_body_list$t1_list$vert_body_center_sf[[1]]
                ),
                y = c(
                    spine_build_list$vertebral_body_list$l5_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$l4_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$l3_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$l2_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$l1_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t12_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t11_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t10_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t9_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t8_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t7_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t6_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t5_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t4_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t3_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t2_list$vert_body_center_sf[[2]],
                    spine_build_list$vertebral_body_list$t1_list$vert_body_center_sf[[2]]
                )
            ) 
        
            return(list(spine_build_list = spine_build_list, #1
                        measurements_df = measurements_df, #13 
                        vert_labels_df = vert_labels_df, #14
                        cone_of_economy_line_sf = cone_of_economy_line_sf,
                        cone_of_economy_sf = cone_of_economy_sf,
                        anterior_to_cone_sf = anterior_to_cone_sf,
                        posterior_to_cone_sf = posterior_to_cone_sf,
                        cone_color = cone_color,
                        l1_s1_lordosis_color = l1_s1_lordosis_color,
                        spinal_kyphosis_color = spinal_kyphosis_color,
                        sacral_slope_color = sacral_slope_color
            )
            )
            
            
            
        })
    }
    
    
    #################### END OF NEW
 
    
    # ###########################      ########################### END OF PREOP SPINE BUILD       ###########################       ###########################
    reactive_spine <- build_spine_reactive_function()
    ################################################################ END OF SPINE BUILD FUNCTIONS #################################
    
    lines_list_reactive <- reactive({
        lines_list <- list()
        
        line_size <- 1
        # facing_left_or_right <- c(if_else(input$face_right == TRUE, -1, 1), 1)
    
        if(input$c2pa_line_show == TRUE){
            lines_list$c2pa_line_sf <- jh_make_colored_line_geom_function(sf_geom_input = reactive_spine()$spine_build_list$lines_list$c2pa_line_extended_curve_sf, 
                                               color_input = input$c2pa_line_color, 
                                               line_size =  1.75)
        }
        
        if(input$c2_tilt_line_show == TRUE){
          lines_list$c2_tilt_line_sf <- jh_make_colored_line_geom_function(sf_geom_input = reactive_spine()$spine_build_list$lines_list$c2_tilt_up_curve_sf, 
                                                                        color_input = input$c2_tilt_line_color, 
                                                                        line_size =  line_size,
                                                                        line_type = "dashed")
        }
        
        if(input$t1pa_line_show == TRUE){
          lines_list$t1pa_line_sf <- jh_make_colored_line_geom_function(sf_geom_input = reactive_spine()$spine_build_list$lines_list$t1pa_line_curve_sf, 
                                                                        color_input = input$t1pa_line_color, 
                                                                        line_size =  1.5)
        }

        if(input$t4pa_line_show == TRUE){
            lines_list$t4pa_line_sf <- jh_make_colored_line_geom_function(sf_geom_input = reactive_spine()$spine_build_list$lines_list$t4pa_line_curve_sf, 
                                                                          color_input = input$t4pa_line_color, 
                                                                          line_size =  1.25)
        }
        if(input$t9pa_line_show == TRUE){
          lines_list$t9pa_line_sf <- jh_make_colored_line_geom_function(sf_geom_input = reactive_spine()$spine_build_list$lines_list$t9pa_line_curve_sf, 
                                                                        color_input = input$t9pa_line_color, 
                                                                        line_size =  1)
        }
        if(input$l1pa_line_show == TRUE){
            lines_list$l1pa_line <- jh_make_colored_line_geom_function(sf_geom_input = reactive_spine()$spine_build_list$lines_list$l1pa_line_curve_sf, 
                                                                          color_input = input$l1pa_line_color, 
                                                                          line_size =  0.75)
        }

        if(input$tk_line_show == TRUE){
            lines_list$t4_line_sf <- geom_sf(data = reactive_spine()$spine_build_list$lines_list$t4_line_sf,
                                           color = input$tk_line_color, 
                                           fill = input$tk_line_color, 
                                           size = line_size,lineend="round", linejoin="round")
            
            lines_list$t12_line <- geom_sf(data = reactive_spine()$spine_build_list$lines_list$t12_line_sf,
                                           color = input$tk_line_color, 
                                           fill = input$tk_line_color, 
                                           size = line_size, lineend="round", linejoin="round")
        }
        
        if(input$l1s1_line_show == TRUE){
            lines_list$l1_s1_line <- geom_sf(data = reactive_spine()$spine_build_list$lines_list$l1s1_line_sf_1, 
                                             color = input$l1s1_line_color, 
                                             fill = input$l1s1_line_color, 
                                             size = line_size,lineend="round", linejoin="round")
            lines_list$l1_s1_line2 <- geom_sf(data = reactive_spine()$spine_build_list$lines_list$l1s1_line_sf_2, 
                                             color = input$l1s1_line_color, 
                                             fill = input$l1s1_line_color, 
                                             size = line_size,lineend="round", linejoin="round")
        }

        
        if(input$l4s1_line_show == TRUE){
            lines_list$l4_s1_line <- geom_sf(data = reactive_spine()$spine_build_list$lines_list$l4s1_line_sf_1, 
                                             color = input$l1s1_line_color, 
                                             fill = input$l1s1_line_color, 
                                             size = line_size,
                                             lineend="round", 
                                             linejoin="round")
            
            lines_list$l4_s1_line2 <- geom_sf(data = reactive_spine()$spine_build_list$lines_list$l4s1_line_sf_2, 
                                              color = input$l4s1_line_color, 
                                              fill = input$l4s1_line_color, 
                                              size = line_size,
                                              lineend="round", 
                                              linejoin="round")
        }
        if(input$ss_line_show == TRUE){
          lines_list$ss_line <- geom_sf(data = reactive_spine()$spine_build_list$lines_list$ss_line_sf,
                                        color = input$ss_line_color, 
                                        fill = input$ss_line_color, 
                                        size = line_size,lineend="round", linejoin="round")
        }
        if(input$pt_line_show == TRUE){
          if(input$pt_line_vertex_at_hips){
            lines_list$pt_line <- geom_sf(data = reactive_spine()$spine_build_list$lines_list$pt_line_up_curve_sf,
                                          color = input$pt_line_color, 
                                          fill = input$pt_line_color, 
                                          size = line_size,lineend="round", linejoin="round")
          }else{
            lines_list$pt_line <- geom_sf(data = reactive_spine()$spine_build_list$lines_list$pt_line_sf,
                                          color = input$pt_line_color, 
                                          fill = input$pt_line_color, 
                                          size = line_size,lineend="round", linejoin="round")
          }
        }
        if(input$pi_line_show == TRUE){
          lines_list$pi_line <- geom_sf(data = reactive_spine()$spine_build_list$lines_list$pelvic_incidence_line_curve_sf,
                                        color = input$pi_line_color, 
                                        fill = input$pi_line_color, 
                                        size = line_size,
                                        lineend="round", 
                                        linejoin="round")
        }
        
        #####
        if(input$t1_c2_ha_line_show == TRUE){
          lines_list$t1_c2_ha_line_sf <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$t1_c2_ha_line_sf, 
                                                 color = input$t1_c2_ha_line_color, 
                                                 fill = input$t1_c2_ha_line_color, 
                                                 size = line_size,
                                                 lineend="round",
                                                 linejoin="round")
        }
        
        if(input$t9_c2_ha_line_show == TRUE){
          lines_list$t9_c2_ha_line_sf <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$t9_c2_ha_line_sf,
                                                 color = input$t9_c2_ha_line_color, 
                                                 fill = input$t9_c2_ha_line_color, 
                                                 size = line_size,
                                                 lineend="round", 
                                                 linejoin="round")
        }
        lines_list
    })
    
    cone_economy_list_reactive <- reactive({
        # facing_left_or_right <- c(if_else(input$face_right == TRUE, -1, 1), 1)
        facing_left_or_right <- 1
        ### CONE OF ECONOMY
        cone_of_economy_list <- list()
        
        
        cone_of_economy_sf <- reactive_spine()$cone_of_economy_sf*facing_left_or_right
        cone_of_economy_line_sf <- reactive_spine()$cone_of_economy_line_sf*facing_left_or_right
        anterior_to_cone_sf <- reactive_spine()$anterior_to_cone_sf*facing_left_or_right
        posterior_to_cone_sf <- reactive_spine()$posterior_to_cone_sf*facing_left_or_right
        anterior_to_cone_true <- reactive_spine()$anterior_to_cone_true
        posterior_to_cone_true <- reactive_spine()$posterior_to_cone_true
        cone_color <- reactive_spine()$cone_color
        outside_cone_color <- reactive_spine()$outside_cone_color
        
        inside_cone_color <- case_when(
            cone_color == "green" ~ "green",
            cone_color == "yellow" ~ "yellow",
            cone_color == "red" ~ "white"
        )
        
        outside_cone_color <- case_when(
            cone_color == "green" ~ "white",
            cone_color == "yellow" ~ "white",
            cone_color == "red" ~ "red"
        )
        
        
        if(input$cone_of_economy_show == TRUE){
            cone_of_economy_list$anterior_to_cone_sf <- geom_sf(data = anterior_to_cone_sf, color = outside_cone_color, fill = outside_cone_color, alpha = 0.7)
        }
        if(input$cone_of_economy_show == TRUE){
            cone_of_economy_list$posterior_to_cone_sf <- geom_sf(data = posterior_to_cone_sf, color = outside_cone_color, fill = outside_cone_color, alpha = 0.7)
        }
        if(input$cone_of_economy_show == TRUE){
            cone_of_economy_list$cone_of_economy_sf <- geom_sf(data = cone_of_economy_sf, colour = inside_cone_color, fill = inside_cone_color, alpha = 0.5)
        }
        if(input$cone_of_economy_show == TRUE){
            cone_of_economy_list$cone_of_economy_line_sf <- geom_sf(data = cone_of_economy_line_sf, color = "black") 
        }
        
        cone_of_economy_list
    })
    
    
    ### NEW PLOTTING
    spine_plot <- reactive({ 
            # ############ Lines List

            ###################
            l1_s1_lordosis_color <- if_else(input$spine_coloring == TRUE, reactive_spine()$l1_s1_lordosis_color, "grey90")  
            spinal_kyphosis_color <- if_else(input$spine_coloring == TRUE, reactive_spine()$spinal_kyphosis_color, "grey90")
            sacral_slope_color <- if_else(input$spine_coloring == TRUE, reactive_spine()$sacral_slope_color, "grey90")
            
            
            spine_geoms_df <- reactive_spine()$spine_build_list$spine_df %>%
                select(object, geom)  %>%
                mutate(fill_color = case_when(
                    str_starts(string = object, "l") ~ l1_s1_lordosis_color,
                    str_starts(string = object, "t") ~ spinal_kyphosis_color,
                    str_starts(string = object, "c") ~ "grey90",
                    str_starts(string = object, "head") ~ "grey90",
                    str_starts(string = object, "fem") ~ sacral_slope_color,
                    str_starts(string = object, "sac") ~ sacral_slope_color
                )) %>%
                mutate(alpha = case_when(
                    object == "head_geom" ~ 0.8, 
                    str_detect(object, "c1") ~ 0.8, 
                    TRUE ~ 1
                ))
            
            ggplot() +
                cone_economy_list_reactive() +
                geom_sf(data = spine_geoms_df, 
                        color = "black", 
                        aes(geometry = geom, 
                            alpha = alpha,
                            # x = facing_left_or_right,
                            # y = 1,
                            fill = fill_color)) +
                lines_list_reactive() +
                xlim(-40, 40) +
                ylim(-6, 110) +
                theme_void() +
                # labs(title = "The Cone of Economy") +
                theme(
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    # plot.title = element_text(
                    #     size = 16,
                    #     hjust = 0.5,
                    #     vjust = -0.5,
                    #     face = "bold.italic"
                    # ),
                    plot.background = element_blank()
                    # plot.background = element_rect(fill = "transparent", colour = NA),
                    # panel.background = element_rect(fill = "transparent", colour = NA)
                ) + 
                scale_fill_identity() +
                scale_alpha_identity()
            
        }
        )
    
    ### NEW PLOTTING END
    

    
    
    output$spine_base <- renderPlot({
        measurements_df <- reactive_spine()$measurements_df
        vert_labels_df <- reactive_spine()$vert_labels_df
        
        spine_plot()+
            geom_text(aes(measurements_df$x, measurements_df$y, label = measurements_df$label),
                      size = 5) +
            geom_text(
                aes(vert_labels_df$x, vert_labels_df$y, label = vert_labels_df$vert_level),
                size = 3
            )
    })
    
 
    
    output$download_figure <- downloadHandler(filename = function(){paste("spine_figure.svg")},
                                              content = function(figure){
                                                  ggsave(filename = figure, 
                                                         plot = spine_plot(),
                                                         units = "in",
                                                         width = 8,
                                                         height = 14,
                                                         dpi = 1200,
                                                         device = "svg")
                                              })
    
    plain_spine_plot <- reactive({
        
        spine_plot() + 
            theme_void()
    }
    )
    
    output$download_plain_figure <- downloadHandler(filename = function(){paste("spine_figure.svg")},
                                                    content = function(figure){
                                                        ggsave(filename = figure, 
                                                               plot = plain_spine_plot(),
                                                               units = "in",
                                                               width = 6,
                                                               height = 10,
                                                               dpi = 1200,
                                                               device = "svg")
                                                    })
    
    
    
    
    
    # ###########################  ########################### SPINE PLANNING   ########################### ###########################
    # ###########################  ########################### SPINE PLANNING   ########################### ###########################
    # ###########################  ########################### SPINE PLANNING   ########################### ###########################
    # ###########################  ########################### SPINE PLANNING   ########################### ###########################
    # ###########################  ########################### SPINE PLANNING   ########################### ###########################
    # ###########################  ########################### SPINE PLANNING   ########################### ###########################
    
    ## preop Figure

    spinal_segments_labels_vector <- c('L5-S1', 'L4-L5', 'L3-L4', 'L2-L3', 'L1-L2', 
                                       'T12-L1', 'T11-T12', 'T10-T11', 'T9-T10', 'T8-T9', 'T7-T8', 'T6-T7', 'T5-T6', 'T4-T5', 'T3-T4', 'T2-T3', 'T1-T2',
                                       'C7-T1', 'C6-C7', 'C5-C6', 'C4-C5', 'C3-C4', 'C2-C3', 'C1-C2')
    
    preop_segment_angles_list_reactive <- reactive({
      compute_segment_angles_list_function(
        pelvic_incidence = input$preop_pelvic_incidence,
        l1_s1 = input$preop_l1s1,
        t10_l2 = input$preop_t10_l2,
        c2_c7 = input$preop_c2c7,
        l1pa = input$preop_l1pa,
        t9pa = input$preop_t9pa,
        t4pa = input$preop_t4pa,
        c2pa = input$preop_c2pa
      )
    })
    
    output$preop_segment_angles_ui <- renderUI({
      preop_segment_angles_list <- preop_segment_angles_list_reactive()
      
      segment_angles_input_list <- rev(map2(preop_segment_angles_list, spinal_segments_labels_vector, function(value, label) {
        create_spine_segment_input_function(segment_input_label = label, segment_value_input = value)
      }))
      # create_spine_rigid_level_input_function
      
      column(width = 5,
             box(width = 12,title = "Cervical", 
                 collapsible = TRUE, 
                 collapsed = TRUE,
                 h5("Check box if Rigid Level"),
                 segment_angles_input_list[1:6] %>% tagList()
             ),
             box(width = 12,title = "Thoracic", 
                 collapsible = TRUE, 
                 collapsed = TRUE,
                 h5("Check box if Rigid Level"),
                 segment_angles_input_list[7:19] %>% tagList()
             ),
             box(width = 12,title = "Lumbar", 
                 collapsible = TRUE, 
                 collapsed = FALSE,
                 h5("Check box if Rigid Level"),
                 segment_angles_input_list[20:24]%>% tagList()
             )
      )
    })
    
    ## Collect the segment angles inputs
    preop_segment_angles_input_list_reactive <- reactive({
      map(spinal_segments_labels_vector, function(segment) {
        input[[paste0("preop_", str_to_lower(str_replace_all(segment, pattern = "-", "_")), "_segment")]]
      }) %>% 
        set_names(map_chr(spinal_segments_labels_vector, ~ paste0(str_to_lower(strsplit(.x, "-")[[1]][1]), "_segment_angle")))
      
    })
    
    preop_rigid_levels_vector_reactive <- reactive({
      # segment_id <- paste0("preop_", str_to_lower(str_replace_all(segment_input_label, pattern = "-", "_")), "_rigid")
      rigid_levels <- c("na")
      if(input$preop_l5_s1_rigid == TRUE){rigid_levels <- append(rigid_levels, "L5-S1")} 
      if(input$preop_l4_l5_rigid == TRUE){rigid_levels <- append(rigid_levels, "L4-L5")} 
      if(input$preop_l3_l4_rigid == TRUE){rigid_levels <- append(rigid_levels, "L3-L4")} 
      if(input$preop_l2_l3_rigid == TRUE){rigid_levels <- append(rigid_levels, "L2-L3")} 
      if(input$preop_l1_l2_rigid == TRUE){rigid_levels <- append(rigid_levels, "L1-L2")} 
      if(input$preop_t12_l1_rigid == TRUE){rigid_levels <- append(rigid_levels, "T12-L1")} 
      if(input$preop_t11_t12_rigid == TRUE){rigid_levels <- append(rigid_levels, "T11-T12")} 
      if(input$preop_t10_t11_rigid == TRUE){rigid_levels <- append(rigid_levels, "T10-T11")} 
      if(input$preop_t9_t10_rigid == TRUE){rigid_levels <- append(rigid_levels, "T10-T9")} 
      if(input$preop_t8_t9_rigid == TRUE){rigid_levels <- append(rigid_levels, "T8-T9")} 
      if(input$preop_t7_t8_rigid == TRUE){rigid_levels <- append(rigid_levels, "T7-T8")} 
      if(input$preop_t6_t7_rigid == TRUE){rigid_levels <- append(rigid_levels, "T6-T7")} 
      if(input$preop_t5_t6_rigid == TRUE){rigid_levels <- append(rigid_levels, "T5-T6")} 
      if(input$preop_t4_t5_rigid == TRUE){rigid_levels <- append(rigid_levels, "T4-T5")} 
      if(input$preop_t3_t4_rigid == TRUE){rigid_levels <- append(rigid_levels, "T3-T4")} 
      if(input$preop_t2_t3_rigid == TRUE){rigid_levels <- append(rigid_levels, "T2-T3")} 
      if(input$preop_t1_t2_rigid == TRUE){rigid_levels <- append(rigid_levels, "T1-T2")} 
      if(input$preop_c7_t1_rigid == TRUE){rigid_levels <- append(rigid_levels, "C7-T1")} 
      if(input$preop_c6_c7_rigid == TRUE){rigid_levels <- append(rigid_levels, "C6-C7")} 
      if(input$preop_c5_c6_rigid == TRUE){rigid_levels <- append(rigid_levels, "C5-C6")} 
      if(input$preop_c4_c5_rigid == TRUE){rigid_levels <- append(rigid_levels, "C4-C5")} 
      if(input$preop_c3_c4_rigid == TRUE){rigid_levels <- append(rigid_levels, "C3-C4")} 
      if(input$preop_c2_c3_rigid == TRUE){rigid_levels <- append(rigid_levels, "C2-C3")} 
      if(input$preop_c1_c2_rigid == TRUE){rigid_levels <- append(rigid_levels, "C1-C2")} 
        # set_names(map_chr(spinal_segments_labels_vector, ~ paste0(str_to_lower(strsplit(.x, "-")[[1]][1]), "_segment_angle")))
      if(length(rigid_levels)>0){
        rigid_levels <- map_chr(rigid_levels, ~ paste0(str_to_lower(strsplit(.x, "-")[[1]][1]), "_segment"))
      }
      rigid_levels
    })
    
    output$rigid_levels_text <- renderText({
      print(paste("Fused:", glue_collapse(preop_rigid_levels_vector_reactive(), sep = ", ")))
    })
    
    preop_spine_build_list_reactive <- reactive({
    # preop_spine_build_list_reactive <- eventReactive(input$build_preop_spine, ignoreInit = TRUE, ignoreNULL = TRUE, {
      
      # preop_segment_angles_input_list_reactive
      
      build_full_spine_from_vertebral_pelvic_angles_function(pelv_inc_value = input$preop_pelvic_incidence, 
                                                             pt_value = input$preop_pt, 
                                                             l1pa_value_input = input$preop_l1pa, 
                                                             l1s1_value_input = input$preop_l1s1,
                                                             t10_l2_value_input = input$preop_t10_l2,
                                                             t9pa_value_input = input$preop_t9pa, 
                                                             t4pa_value_input = input$preop_t4pa, 
                                                             c2pa_value_input = input$preop_c2pa, 
                                                             c2_c7_value_input = input$preop_c2c7,
                                                             input_segment_angles = "yes", 
                                                             segment_angles_input = preop_segment_angles_input_list_reactive()
                                                             )
    })
    
    
    
    preop_spine_figure_reactive <- reactive({
    # preop_spine_figure_reactive <- eventReactive(input$build_preop_spine, ignoreInit = TRUE, ignoreNULL = TRUE, {
        
      spine_preop_list <- preop_spine_build_list_reactive()
        

        spine_geoms_df <- spine_preop_list$spine_df %>%
              select(object, geom, geom_alpha)
        

        regional_analysis_list <- id_focal_deformity_function(pelvic_incidence = input$preop_pelvic_incidence,
                                                              l4_pelvic_angle = spine_preop_list$spine_list$l4_pelvic_angle_value, 
                                                              l1_pelvic_angle = input$preop_l1pa, 
                                                              t9_pelvic_angle = input$preop_t9pa, 
                                                              t4_pelvic_angle = input$preop_t4pa, 
                                                              c2_pelvic_angle = input$preop_c2pa
                                                              )
        

        
        line_geoms_list <- list()
        
        line_geoms_list$t9pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_preop_list$lines_list$t9pa_line_curve_sf,
                                                                        color_input = jh_colors_list$colorblind_palette_dark[2],
                                                                        line_size = 1.75
                                                                        )
        
        line_geoms_list$c2pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_preop_list$lines_list$c2pa_line_extended_curve_sf,
                                                                        color_input = jh_colors_list$colorblind_palette_dark[4], 
                                                                        line_size = 0.5
        )
        
        
        measurements_list <- list()
        measurements_list$"C2 Tilt" <- round(spine_preop_list$spine_list$c2_tilt_value)
        measurements_list$"C2PA" <- round(spine_preop_list$spine_list$c2_pelvic_angle_value, 0)
        # measurements_list$"T1PA" <- round(spine_preop_list$spine_list$t1pa_value, 0)
        measurements_list$"T4PA" <- round(spine_preop_list$spine_list$t4_pelvic_angle_value, 0)
        measurements_list$"T9PA" <- round(spine_preop_list$spine_list$t9_pelvic_angle_value, 0)
        measurements_list$"L1PA" <- round(spine_preop_list$spine_list$l1_pelvic_angle_value, 0)
        measurements_list$"L1-S1" <-  round(spine_preop_list$spine_list$lumbar_lordosis, 0)
        # measurements_list$"TK" <- round(spine_preop_list$spine_list$thoracic_kyphosis, 0)
        measurements_list$"PT" <- round(spine_preop_list$spine_list$pelvic_tilt, 0)
        
        measurements_df <- enframe(measurements_list) %>%
          unnest() %>%
          mutate(x = -8) %>%
          mutate(y = c(seq(from = 0, by = -3, length = length(measurements_list)))) %>%
          mutate(label = paste(name, value, sep = " = ")) 
        
        ggplot() +
            geom_sf(data = spine_geoms_df, 
                    color = "black", 
                    aes(geometry = geom, 
                        alpha = geom_alpha,
                        fill = "grey90")) +
          line_geoms_list +
          # xlim(-40, 40) +
          # xlim(-65, 40) +
          # xlim(-30, 40) +
          ylim(-20, 110) +
          theme_void() +
          labs(title = "Preop Alignment") +
          theme(
            axis.text = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(
              size = 16,
              hjust = 0.5,
              vjust = -0.5,
              face = "bold.italic"
            ),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA)
          ) + 
          draw_text(text = measurements_df$label,
                    x = measurements_df$x, 
                    y = measurements_df$y, size = 11) +
          scale_fill_identity() +
          scale_alpha_identity()
        
        
    })
        
    output$preop_spine_alignment <- renderPlot({
        preop_spine_figure_reactive()
    }) 
    
    
  
    
    #################### NEW VERSION SIMPLIFIED
    
   
    
    ############################ NEW NEW T11 UIV with risk ###############################
    ############################ NEW NEW T11 UIV with risk ###############################
    ############################ NEW NEW T11 UIV with risk ###############################
    ############################ NEW NEW T11 UIV with risk ###############################
    ############################ NEW NEW T11 UIV with risk ###############################
    ############################ NEW NEW T11 UIV with risk ###############################
    
    
    spine_plan_uiv_t11_option_1 <- eventReactive(input$compute_plan, {
      build_t11_spine_plot_function(pso_option_number = 1, 
                                    preop_age = input$preop_age,
                                    preop_sex = input$preop_sex,
                                    preop_pelvic_incidence = input$preop_pelvic_incidence,
                                    preop_pt = input$preop_pt,
                                    preop_l1pa = input$preop_l1pa,
                                    preop_t9pa = input$preop_t9pa,
                                    preop_t4pa = input$preop_t4pa,
                                    preop_c2pa = input$preop_c2pa,
                                    l1pa_line_color = input$l1pa_line_color,
                                    t4pa_line_color = input$t4pa_line_color,
                                    c2pa_line_color = input$c2pa_line_color, 
                                    preop_segment_angles_input_list_reactive = preop_segment_angles_input_list_reactive(),
                                    preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive()
                                    )
    })
  
    
    spine_plan_uiv_t11_option_2 <- eventReactive(input$compute_plan, {
      build_t11_spine_plot_function(pso_option_number = 2, 
                                    preop_age = input$preop_age,
                                    preop_sex = input$preop_sex,
                                    preop_pelvic_incidence = input$preop_pelvic_incidence,
                                    preop_pt = input$preop_pt,
                                    preop_l1pa = input$preop_l1pa,
                                    preop_t9pa = input$preop_t9pa,
                                    preop_t4pa = input$preop_t4pa,
                                    preop_c2pa = input$preop_c2pa,
                                    l1pa_line_color = input$l1pa_line_color,
                                    t4pa_line_color = input$t4pa_line_color,
                                    c2pa_line_color = input$c2pa_line_color, 
                                    preop_segment_angles_input_list_reactive = preop_segment_angles_input_list_reactive(),
                                    preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive()
      )
    })

    
    output$spine_plan_lower_t <- renderPlot({
      
      plot_grid(NULL, NULL, NULL,
                spine_plan_uiv_t11_option_1(), NULL, spine_plan_uiv_t11_option_2(), 
                nrow = 2, rel_heights = c(0.075, 0.925), 
                rel_widths = c(1, -0.15, 1)
                ) + 
        draw_text(text = "Lower Thoracic UIV", x = 0.5, y = 0.975, size = 18, fontfact = "bold")
      
      # plot_grid(spine_plan_uiv_t11_option_1(), spine_plan_uiv_t11_option_2(), nrow = 1)
      
    })
    
    ############################ END END T11 UIV with risk END END ###############################
    ############################ END END T11 UIV with risk END END ###############################
    ############################ END END T11 UIV with risk END END ###############################
    ############################ END END T11 UIV with risk END END ###############################
    ############################ END END T11 UIV with risk END END ###############################
    ############################ END END T11 UIV with risk END END ###############################
    
    
    ############################ NEW NEW T4 UIV with risk ###############################
    ############################ NEW NEW T4 UIV with risk ###############################
    ############################ NEW NEW T4 UIV with risk ###############################
    ############################ NEW NEW T4 UIV with risk ###############################
    ############################ NEW NEW T4 UIV with risk ###############################
    ############################ NEW NEW T4 UIV with risk ###############################
    
    
    spine_plan_uiv_t4 <- eventReactive(input$compute_plan, {
      build_upper_t_uiv_spine_plot_function(pso_option_number = 1, 
                                    preop_age = input$preop_age,
                                    preop_sex = input$preop_sex,
                                    preop_pelvic_incidence = input$preop_pelvic_incidence,
                                    preop_pt = input$preop_pt,
                                    preop_l1pa = input$preop_l1pa,
                                    preop_t9pa = input$preop_t9pa,
                                    preop_t4pa = input$preop_t4pa,
                                    preop_c2pa = input$preop_c2pa,
                                    l1pa_line_color = input$l1pa_line_color,
                                    t4pa_line_color = input$t4pa_line_color,
                                    c2pa_line_color = input$c2pa_line_color, 
                                    preop_segment_angles_input_list_reactive = preop_segment_angles_input_list_reactive(),
                                    preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive()
      )
    })
    
    
    # 
    # spine_plan_uiv_t4 <- eventReactive(input$compute_plan, {
    #   
    #   # preop_age = input$preop_age,
    #   # preop_sex = input$preop_sex,
    #   # preop_pelvic_incidence = input$preop_pelvic_incidence,
    #   # preop_pt = input$preop_pt,
    #   # preop_l1pa = input$preop_l1pa,
    #   # preop_t9pa = input$preop_t9pa,
    #   # preop_t4pa = input$preop_t4pa,
    #   # preop_c2pa = input$preop_c2pa,
    #   # l1pa_line_color = input$l1pa_line_color,
    #   # t4pa_line_color = input$t4pa_line_color,
    #   # c2pa_line_color = input$c2pa_line_color, 
    #   # preop_segment_angles_input_list_reactive = preop_segment_angles_input_list_reactive(),
    #   # preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive()
    #   
    #   pso_list <- list()
    #   
    #   starting_segment_angles <- preop_segment_angles_input_list_reactive()
    #   
    #   prescribed_segment_angles <- starting_segment_angles
    #   
    #   rigid_levels <- preop_rigid_levels_vector_reactive()
    #   
    #   starting_pi <- input$preop_pelvic_incidence
    #   prescribed_l1pa <- starting_pi*0.5-19
    #   
    #   pso_option_number <- 1
    #   
    #   prescribed_l2_s1_list <- prescribe_l2_s1_segment_angles_function(segment_angles_list = starting_segment_angles, 
    #                                                                    pelvic_incidence = starting_pi, 
    #                                                                    rigid_segments = rigid_levels, 
    #                                                                    l1pa_target_range = c(prescribed_l1pa - 3, prescribed_l1pa + 2)
    #   )
    #   
    #   if(prescribed_l2_s1_list$needs_pso == "yes"){
    #     pso_options_list <- compute_pso_options_df(segment_angles_list = prescribed_l2_s1_list$new_segment_angles,
    #                                                pelvic_incidence = starting_pi, 
    #                                                pso_candidate_levels = prescribed_l2_s1_list$pso_candidates, 
    #                                                l1pa_goal = prescribed_l1pa, 
    #                                                l2_s1_goal = prescribed_l2_s1_list$l2_s1_goal)
    #     
    #     pso_options_df <- pso_options_list$pso_options_summary_df 
    # 
    #     ############ pso option ############
    #     pso_level <- str_to_upper(pso_options_df$pso_level[[pso_option_number]])
    #     pso_list$lumbar_pso <- pso_level
    #     
    #     pso_l2s1_segment_angles <- pso_options_list$new_segment_angles_by_pso_level_list[[paste0(pso_options_df$pso_level[[pso_option_number]], "_pso_segment_angles")]]
    #     
    #     prescribed_segment_angles$l5_segment_angle <- pso_l2s1_segment_angles$l5_segment_angle
    #     prescribed_segment_angles$l4_segment_angle <- pso_l2s1_segment_angles$l4_segment_angle
    #     prescribed_segment_angles$l3_segment_angle <- pso_l2s1_segment_angles$l3_segment_angle
    #     prescribed_segment_angles$l2_segment_angle <- pso_l2s1_segment_angles$l2_segment_angle
    #     
    #   }else{
    #     pso_level <- "na"
    #     prescribed_segment_angles$l5_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l5_segment_angle
    #     prescribed_segment_angles$l4_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l4_segment_angle
    #     prescribed_segment_angles$l3_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l3_segment_angle
    #     prescribed_segment_angles$l2_segment_angle <- prescribed_l2_s1_list$new_segment_angles$l2_segment_angle
    #     
    #   }
    # 
    #   checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi,
    #                                                                    pt_value = 10,
    #                                                                    segment_angle_list = prescribed_segment_angles)
    #   
    #   current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #   
    #   target_t4pa <- checking_vpas$l1pa_value
    #   
    #   
    #   
    #   if(between(current_t4_l1pa_mismatch, -4, 1) == FALSE){
    #     ##################### L1 #####################
    #     prescribed_l1_l2 <- prescribe_l1_l2_function_t4l1pa(t4_pelvic_angle = target_t4pa,
    #                                                         l1_pelvic_angle = checking_vpas$l1pa_value, pelvic_incidence = starting_pi)
    #     
    #     if("l1_segment" %in% rigid_levels == FALSE){
    #       prescribed_segment_angles$l1_segment_angle <- prescribed_l1_l2[1]
    #     }else if(between(prescribed_segment_angles$l1_segment_angle, prescribed_l1_l2 - 3, prescribed_l1_l2 + 3)){
    #       NA
    #     }else{
    #       prescribed_segment_angles$l1_segment_angle <- prescribed_l1_l2[1]
    #       pso_list$l1 <- "L1"
    #     }
    #     checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi,
    #                                                                      pt_value = 10,
    #                                                                      segment_angle_list = prescribed_segment_angles)
    #     
    #     current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #   }
    #   
    #   
    #   
    #   if(between(current_t4_l1pa_mismatch, -4, 1) == FALSE){
    #     ##################### T12  #####################
    #     prescribed_t12_l1 <- prescribe_t12_l1_function_t4l1pa(t4_pelvic_angle = target_t4pa,
    #                                                           l1_pelvic_angle = checking_vpas$l1pa_value, 
    #                                                           pelvic_incidence = starting_pi, 
    #                                                           l2_s1 = sum(unlist(prescribed_segment_angles[1:4])), 
    #                                                           l1_l2 = prescribed_segment_angles$l1_segment_angle)
    #     
    #     if("t12_segment" %in% rigid_levels == FALSE){
    #       prescribed_segment_angles$t12_segment_angle <- prescribed_t12_l1[1]
    #     }else if(between(prescribed_segment_angles$t12_segment_angle, prescribed_t12_l1 - 3, prescribed_t12_l1 + 3)){
    #       NA
    #     }else{
    #       prescribed_segment_angles$t12_segment_angle <- prescribed_t12_l1[1]
    #       pso_list$t12 <- "T12"
    #     }
    #     
    #     checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi,
    #                                                                      pt_value = 10,
    #                                                                      segment_angle_list = prescribed_segment_angles)
    #     
    #     current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #   }
    #   
    #   
    #   checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi,
    #                                                                    pt_value = 10,
    #                                                                    segment_angle_list = prescribed_segment_angles)
    #   
    #   current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #   
    #   if(between(current_t4_l1pa_mismatch, -4, 1) == FALSE){
    #     ##################### T11  ##################### 
    #     prescribed_t11_t12 <- prescribe_t11_t12_function_t4l1pa(t4_pelvic_angle = target_t4pa,
    #                                                             l1_pelvic_angle = checking_vpas$l1pa_value, 
    #                                                             pelvic_incidence = starting_pi,   
    #                                                             t12_s1 = sum(unlist(prescribed_segment_angles[1:6]))
    #     )
    #     
    #     if("t11_segment" %in% rigid_levels == FALSE){
    #       prescribed_segment_angles$t11_segment_angle <- prescribed_t11_t12[1]
    #     }else if(between(prescribed_segment_angles$t11_segment_angle, prescribed_t11_t12 - 3, prescribed_t11_t12 + 3)){
    #       NA
    #     }else{
    #       prescribed_segment_angles$t11_segment_angle <- prescribed_t11_t12[1]
    #       pso_list$t11 <- "T11"
    #     }
    #     checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi,
    #                                                                      pt_value = 10,
    #                                                                      segment_angle_list = prescribed_segment_angles)
    #     
    #     current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #   }
    #   
    #   
    #   if(between(current_t4_l1pa_mismatch, -4, 1) == FALSE){
    #     ##################### T10  #####################
    #     prescribed_t10_t11 <- prescribe_t10_t11_function_t4l1pa(t4_pelvic_angle = target_t4pa,
    #                                                             l1_pelvic_angle = checking_vpas$l1pa_value, 
    #                                                             pelvic_incidence = starting_pi, 
    #                                                             t11_s1 = sum(unlist(prescribed_segment_angles[1:7]))
    #     )
    #     
    #     if("t10_segment" %in% rigid_levels == FALSE){
    #       prescribed_segment_angles$t10_segment_angle <- prescribed_t10_t11[1]
    #     }else if(between(prescribed_segment_angles$t10_segment_angle, prescribed_t10_t11 - 3, prescribed_t10_t11 + 3)){
    #       NA
    #     }else{
    #       prescribed_segment_angles$t10_segment_angle <- prescribed_t10_t11[1]
    #       pso_list$t10 <- "T10"
    #     }
    #     checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi,
    #                                                                      pt_value = 10,
    #                                                                      segment_angle_list = prescribed_segment_angles)
    #     
    #     current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #   }
    #   
    #   
    #   
    #   if(between(current_t4_l1pa_mismatch, -4, 1) == FALSE){
    #     ##################### T10  #####################
    #     prescribed_t9_t10 <- prescribe_t9_t10_function_t4l1pa(t4_pelvic_angle = target_t4pa,
    #                                                           l1_pelvic_angle = checking_vpas$l1pa_value, 
    #                                                           pelvic_incidence = starting_pi,
    #                                                           t10_t11 = prescribed_segment_angles$t10_segment_angle,
    #                                                           t11_t12 = prescribed_segment_angles$t11_segment_angle, 
    #                                                           t12_s1 = sum(unlist(prescribed_segment_angles[1:6]
    #                                                           ))
    #     )
    #     
    #     if("t9_segment" %in% rigid_levels == FALSE){
    #       prescribed_segment_angles$t9_segment_angle <- prescribed_t9_t10[1]
    #     }else if(between(prescribed_segment_angles$t9_segment_angle, prescribed_t9_t10 - 3, prescribed_t9_t10 + 3)){
    #       NA
    #     }else{
    #       prescribed_segment_angles$t9_segment_angle <- prescribed_t9_t10[1]
    #       pso_list$t10 <- "T9"
    #     }
    #     checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = starting_pi,
    #                                                                      pt_value = 10,
    #                                                                      segment_angle_list = prescribed_segment_angles)
    #     
    #     current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #   }
    #   
    #   attempt_number <- 1
    #   
    #   if(between(current_t4_l1pa_mismatch, -4, 1) == FALSE & attempt_number < 9){
    #     
    #     while(between(checking_vpas$t4pa_value, target_t4pa - 2, target_t4pa + 2) == FALSE){
    #       
    #       segment_modifier <- (checking_vpas$t4pa_value - t4pa_target)*0.25
    #       
    #       if("t8_segment" %in% rigid_levels  == FALSE){
    #         prescribed_segment_angles$t10_segment_angle <- prescribed_segment_angles$t11_segment_angle + segment_modifier
    #       }
    #       if("t7_segment" %in% rigid_levels  == FALSE){
    #         prescribed_segment_angles$t11_segment_angle <- prescribed_segment_angles$t11_segment_angle + segment_modifier
    #       }
    #       if("t6_segment" %in% rigid_levels  == FALSE){
    #         prescribed_segment_angles$t12_segment_angle <- prescribed_segment_angles$t12_segment_angle + segment_modifier
    #       }   
    #       
    #       
    #       checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = input$preop_pelvic_incidence, 
    #                                                                        pt_value = 10,
    #                                                                        segment_angle_list = prescribed_segment_angles)
    #       
    #       current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #       
    #       attempt_number + 1
    #       
    #     } ## end of while loop
    #     
    #   }
    #   
    #   
    #   ###################################################### DONE WITH PRESCRIBING, NOW PLOT ################################
    #   
    #   new_vpa_list <- checking_vpas
    #   
    #   new_l1_s1 <- sum(unlist(prescribed_segment_angles[1:5]))
    # 
    #   predicted_pt <- predict_postop_pt_function(preop_pt = input$preop_pt, 
    #                                              preop_c2_tilt = (input$preop_c2pa - input$preop_pt), 
    #                                              postop_c2pa = (new_vpa_list$c2pa_value))
    #   
    #   
    #   spine_prescribed_list <- build_full_spine_function_new(pelv_inc_value = input$preop_pelvic_incidence,
    #                                                          segment_angle_list = prescribed_segment_angles,
    #                                                          pt_value = predicted_pt[1],
    #                                                          spine_faces = "right",
    #                                                          # pso_levels = as_vector(pso_list)
    #                                                          pso_levels = as_vector(pso_list)
    #   )
    #   
    #   
    #   
    #   spine_geoms_df <- spine_prescribed_list$spine_df %>%
    #     select(object, geom, geom_alpha)  
    #   
    #   lines_list <- list()
    #   
    #   lines_list$l1pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$l1pa_line_curve_sf,
    #                                                              color_input = input$l1pa_line_color, 
    #                                                              line_size = 1
    #   )
    #   
    #   lines_list$t4pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$t4pa_line_curve_sf,
    #                                                              color_input = input$t4pa_line_color,
    #                                                              line_size = 0.75
    #   )
    #   
    #   lines_list$c2pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$c2pa_line_extended_curve_sf,
    #                                                              color_input = input$c2pa_line_color,
    #                                                              line_size = 0.25
    #   )
    #   
    #   preop_t4pa_l1pa_mismatch = input$preop_t4pa - input$preop_l1pa
    #   
    #   preop_c2pa_t4pa_mismatch <- input$preop_c2pa - input$preop_t4pa
    #   postop_c2pa_t4pa_mismatch <- new_vpa_list$c2pa_value - new_vpa_list$t4pa_value
    #   
    #   if(postop_c2pa_t4pa_mismatch > 10){
    #     uiv_for_rod <- 2 ## for C2 UIV
    #     uiv_label <- "UIV: C2"
    #   }else if(sum(unlist(prescribed_segment_angles[11:16])) < -5){
    #     uiv_for_rod <- 9 ## for T2 UIV
    #     uiv_label <- "UIV: T2"
    #   }else{
    #     uiv_for_rod <- 11 ## for T4 UIV
    #     uiv_label <- "UIV: T4"
    #   }
    #   
    #   instrumented_vertebral_body_list <- spine_prescribed_list$vertebral_body_list[uiv_for_rod:length(spine_prescribed_list$vertebral_body_list)]
    #   
    #   sp_matrix_for_rod <- do.call(rbind, map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ rbind(instrumented_vertebral_body_list[[.x]]$sp)))
    #   
    #   rod_sf <- st_buffer(x = st_linestring(sp_matrix_for_rod), dist = 0.5, nQuadSegs = 5, joinStyle = "ROUND", endCapStyle = "ROUND")
    #   
    #   screw_list <- st_multipolygon(compact(map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ instrumented_vertebral_body_list[[.x]]$screw_sf)))
    #   
    #   measurements_list <- list()
    #   measurements_list$"C2 Tilt" <- round(spine_prescribed_list$spine_list$c2_tilt_value)
    #   measurements_list$"C2PA" <- round(spine_prescribed_list$spine_list$c2pa_value, 0)
    #   measurements_list$"T1PA" <- round(spine_prescribed_list$spine_list$t1pa_value, 0)
    #   measurements_list$"T4PA" <- round(spine_prescribed_list$spine_list$t4pa_value, 0)
    #   measurements_list$"L1PA" <- round(spine_prescribed_list$spine_list$l1pa_value, 0)
    #   measurements_list$"LL" <-  round(spine_prescribed_list$spine_list$lumbar_lordosis, 0)
    #   measurements_list$"TK" <- round(spine_prescribed_list$spine_list$thoracic_kyphosis, 0)
    #   measurements_list$"PT" <- round(spine_prescribed_list$spine_list$pelvic_tilt, 0)
    #   # measurements_list$"L1PA Deviation" <- round(lpa_deviation, 0)
    #   # measurements_list$"T4PA-L1PA Mismatch" <- round(t4pa_l1pa_mismatch, 0)
    #   
    #   measurements_df <- enframe(measurements_list) %>%
    #     unnest() %>%
    #     # mutate(x = if_else(name == "PI", 10, -26)) %>%
    #     mutate(x = 18) %>%
    #     mutate(y = c(seq(from = 74, by = -4, length = length(measurements_list)-2), 7, 3)) %>%
    #     mutate(label = paste(name, value, sep = " = ")) 
    #   
    #   segment_angles_list_for_plot <- list()
    #   segment_angles_list_for_plot$"T10-L2" <- as.character(round(sum(unlist(prescribed_segment_angles[5:8])), 0))
    #   segment_angles_list_for_plot$"L1-L2" <- as.character(round(prescribed_segment_angles$l1_segment_angle, 0))
    #   segment_angles_list_for_plot$"L2-L3" <- as.character(round(prescribed_segment_angles$l2_segment_angle, 0))
    #   segment_angles_list_for_plot$"L3-L4" <- as.character(round(prescribed_segment_angles$l3_segment_angle, 0))
    #   segment_angles_list_for_plot$"L4-L5" <- as.character(round(prescribed_segment_angles$l4_segment_angle, 0))
    #   segment_angles_list_for_plot$"L5-S1" <- as.character(round(prescribed_segment_angles$l5_segment_angle, 0))
    #   
    #   pso_list <- Filter(function(x) x != "na", pso_list)
    #   
    #   if(length(pso_list)>1){
    #     pso_label <- paste("PSO at", glue_collapse(pso_level, sep = ", ", last = " & "))
    #   }else{
    #     pso_label <- " "
    #   }
    #   
    #   # if(length(pso_list)>0){
    #   #   pso_label <- paste("PSO at", glue_collapse(pso_list, sep = ", ", last = " & "))
    #   # }else{
    #   #   pso_label <- " "
    #   # }
    # 
    #   segment_angles_df <- enframe(segment_angles_list_for_plot) %>%
    #     unnest() %>%
    #     mutate(x = 18) %>%
    #     mutate(y = c(seq(from = 35, by = -4, length = length(segment_angles_list_for_plot)))) %>%
    #     mutate(label = paste(name, value, sep = " = "))
    #   
    #   
    #   pjk_risk <- pjk_risk_function(age = input$preop_age, 
    #                                 sex = input$preop_sex, 
    #                                 preop_c2pa_t9pa_mismatch = (input$preop_c2pa - input$preop_t9pa), 
    #                                 pelvic_incidence = input$preop_pelvic_incidence, 
    #                                 uiv_region = "Upper Thoracic",
    #                                 postop_l1pa_undercorrection = new_vpa_list$l1pa_value - prescribed_l1pa, 
    #                                 postop_t4l1pa_mismatch = new_vpa_list$t4pa_value - new_vpa_list$l1pa_value)
    #   
    #   pjk_risk_color <- case_when(
    #     pjk_risk > 0.45 ~ "red",
    #     between(pjk_risk, 0.35, 0.45) ~ "darkorange",
    #     between(pjk_risk, 0.2, 0.35) ~ "orange",
    #     pjk_risk < 0.2 ~ "darkgreen"
    #   )
    #   
    #   ggplot() +
    #     geom_sf(data = spine_geoms_df,
    #             aes(geometry = geom,
    #                 alpha = geom_alpha
    #             ),
    #             color = "black",
    #             fill = "grey90") +
    #     geom_sf(data = st_multipolygon(x = screw_list), fill = "grey55") +
    #     geom_sf(data = rod_sf, fill = "grey55") +
    #     draw_text(text = measurements_df$label, x = measurements_df$x, y = measurements_df$y, size = 14) +
    #     draw_text(text = segment_angles_df$label, x = segment_angles_df$x, y = segment_angles_df$y, size = 14) +
    #     draw_text(text = uiv_label, x = -30, y = 60, size = 14, fontface = "bold") +
    #     draw_text(text = pso_label, x = -20, y = 10, size = 16, fontface = "bold", color = "darkorange") +
    #     draw_text(text = paste("PJK Risk = ", pjk_risk), x = 0, y = -5, size = 16, color = pjk_risk_color, fontface = "bold") +
    #     lines_list +
    #     xlim(-35, 35) +
    #     # ylim(-6, 110) +
    #     theme_void() +
    #     labs(title = "Option 1") +
    #     theme(
    #       axis.text = element_blank(),
    #       axis.title = element_blank(),
    #       plot.background = element_rect(fill = "transparent", colour = NA),
    #       panel.background = element_rect(fill = "transparent", colour = NA)
    #     ) +
    #     scale_alpha_identity()
    #    
    # }
    # )
    
    
    # spine_plan_uiv_t4 <- eventReactive(input$compute_plan, {
    #   
    #   pso_list <- list()
    #   
    #   starting_segment_angles <- segment_angles_function_from_vertebral_pelvic_angles_function(pelvic_incidence_input = input$preop_pelvic_incidence,
    #                                                                                            l1pa_input = input$preop_l1pa,
    #                                                                                            l1s1_input = input$preop_l1s1,
    #                                                                                            t9pa_input = input$preop_t9pa, 
    #                                                                                            t4pa_input = input$preop_t4pa, 
    #                                                                                            c2pa_input = input$preop_c2pa)
    #   
    #   preop_cervical_lordosis <- starting_segment_angles$c7_segment_angle*6
    #   
    #   
    #   regional_analysis_list <- id_focal_deformity_function(pelvic_incidence = input$preop_pelvic_incidence,
    #                                                         l4_pelvic_angle = preop_spine_build_list_reactive()$spine_list$l4_pelvic_angle_value[1], 
    #                                                         l1_pelvic_angle = input$preop_l1pa,
    #                                                         t9_pelvic_angle = input$preop_t9pa, 
    #                                                         t4_pelvic_angle = input$preop_t4pa, 
    #                                                         c2_pelvic_angle = input$preop_c2pa)
    #   
    #   
    # 
    #   
    #   segment_angles_plan <- starting_segment_angles
    #   
    #   l1pa_target <- target_l1pa_function(pelvic_incidence = input$preop_pelvic_incidence)
    #   
    #   optimized_segment_angles_list <- compute_optimized_lumbar_segmental_lordosis_values(pelvic_incidence = input$preop_pelvic_incidence, 
    #                                                                                       l1_l2_start = starting_segment_angles$l1_segment_angle, 
    #                                                                                       l2_l3_start = starting_segment_angles$l2_segment_angle, 
    #                                                                                       l3_l4_start = starting_segment_angles$l3_segment_angle, 
    #                                                                                       l4_l5_start = starting_segment_angles$l4_segment_angle,
    #                                                                                       l5_s1_start = starting_segment_angles$l5_segment_angle, 
    #                                                                                       desired_l1pa = l1pa_target,
    #                                                                                       non_modifiable = preop_rigid_levels_vector_reactive(), 
    #                                                                                       uiv_region = "upper")
    #   
    # 
    #     segment_angles_plan$l1_segment_angle <-  optimized_segment_angles_list$l1_l2
    #     segment_angles_plan$l2_segment_angle <-  optimized_segment_angles_list$l2_l3
    #     segment_angles_plan$l3_segment_angle <-  optimized_segment_angles_list$l3_l4
    #     segment_angles_plan$l4_segment_angle <-  optimized_segment_angles_list$l4_l5
    #     segment_angles_plan$l5_segment_angle <-  optimized_segment_angles_list$l5_s1
    #     
    #     checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = input$preop_pelvic_incidence,
    #                                                                      pt_value = 10,
    #                                                                      segment_angle_list =segment_angles_plan)
    #     
    #     t4pa_lower_target <- checking_vpas$l1pa_value - 3
    #     t4pa_upper_target <- checking_vpas$l1pa_value + 1
    #     t4pa_target <- checking_vpas$l1pa_value - 1
    #     
    #     current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #     
    #     
    #     # while(round(checking_vpas$t4pa_value, 0) != round(t4pa_target, 0)){
    #     while(between(checking_vpas$t4pa_value, t4pa_lower_target, t4pa_upper_target) == FALSE){
    #       
    #       segment_modifier <- (checking_vpas$t4pa_value - t4pa_target)*0.5
    # 
    #       if("t10_segment" %in% preop_rigid_levels_vector_reactive()  == FALSE){
    #         segment_angles_plan$t10_segment_angle <- segment_angles_plan$t11_segment_angle + segment_modifier
    #       }
    #       if("t11_segment" %in% preop_rigid_levels_vector_reactive()  == FALSE){
    #         segment_angles_plan$t11_segment_angle <- segment_angles_plan$t11_segment_angle + segment_modifier
    #       }
    #       if("t12_segment" %in% preop_rigid_levels_vector_reactive()  == FALSE){
    #         segment_angles_plan$t12_segment_angle <- segment_angles_plan$t12_segment_angle + segment_modifier
    #       }   
    #       if("l1_segment" %in% preop_rigid_levels_vector_reactive()  == FALSE){
    #         segment_angles_plan$l1_segment_angle <- segment_angles_plan$l1_segment_angle + segment_modifier
    #       }
    #       
    #       checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = input$preop_pelvic_incidence, 
    #                                                                        pt_value = 10,
    #                                                                        segment_angle_list = segment_angles_plan)
    #       
    #       current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #       
    #       
    #     } ## end of while loop
    #   
    #   new_vpa_list <- checking_vpas
    #   
    #   new_l1_s1 <- segment_angles_plan$l1_segment_angle + segment_angles_plan$l2_segment_angle +segment_angles_plan$l3_segment_angle + segment_angles_plan$l4_segment_angle +segment_angles_plan$l5_segment_angle
    #   
    #   predicted_pt <- predict_postop_pt_function(preop_pt = input$preop_pt, 
    #                                              preop_c2_tilt = (input$preop_c2pa - input$preop_pt), 
    #                                              postop_c2pa = (new_vpa_list$c2pa_value))
    #   
    #   
    #   spine_prescribed_list <- build_full_spine_function_new(pelv_inc_value = input$preop_pelvic_incidence,
    #                                                          segment_angle_list = segment_angles_plan,
    #                                                          pt_value = predicted_pt[1],
    #                                                          spine_faces = "right",
    #                                                          # pso_levels = as_vector(pso_list)
    #                                                          pso_levels = as_vector(optimized_segment_angles_list$pso_level)
    #                                                          )
    #   
    #   
    #   
    #   spine_geoms_df <- spine_prescribed_list$spine_df %>%
    #     select(object, geom, geom_alpha)  
    #   
    #   lines_list <- list()
    #   
    #   lines_list$l1pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$l1pa_line_curve_sf,
    #                                                              color_input = input$l1pa_line_color, 
    #                                                              line_size = 1.75
    #   )
    #   
    #   lines_list$t4pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$t4pa_line_curve_sf,
    #                                                              color_input = input$t4pa_line_color,
    #                                                              line_size = 1
    #   )
    #   
    #   lines_list$c2pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$c2pa_line_extended_curve_sf,
    #                                                              color_input = input$c2pa_line_color,
    #                                                              line_size = 0.5
    #   )
    #   
    #   preop_t4pa_l1pa_mismatch = input$preop_t4pa - input$preop_l1pa
    #   preop_c2pa_t4pa_mismatch <- input$preop_c2pa - input$preop_t4pa
    #   postop_c2pa_t4pa_mismatch <- new_vpa_list$c2pa_value - new_vpa_list$t4pa_value
    #   # uiv_for_rod <- 11 ## for T4 UIV
    #   
    #   if(preop_c2pa_t4pa_mismatch - postop_c2pa_t4pa_mismatch > 10){
    #     uiv_for_rod <- 2 ## for C2 UIV
    #     uiv_label <- "UIV: C2"
    #   }else if(preop_c2pa_t4pa_mismatch - postop_c2pa_t4pa_mismatch > 5){
    #     uiv_for_rod <- 9 ## for T2 UIV
    #     uiv_label <- "UIV: T2"
    #   }else{
    #     uiv_for_rod <- 11 ## for T4 UIV
    #     uiv_label <- "UIV: T4"
    #   }
    #   
    #   instrumented_vertebral_body_list <- spine_prescribed_list$vertebral_body_list[uiv_for_rod:length(spine_prescribed_list$vertebral_body_list)]
    #   
    #   sp_matrix_for_rod <- do.call(rbind, map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ rbind(instrumented_vertebral_body_list[[.x]]$sp)))
    #   
    #   rod_sf <- st_buffer(x = st_linestring(sp_matrix_for_rod), dist = 0.5, nQuadSegs = 5, joinStyle = "ROUND", endCapStyle = "ROUND")
    #   
    #   screw_list <- st_multipolygon(compact(map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ instrumented_vertebral_body_list[[.x]]$screw_sf)))
    #   
    #   measurements_list <- list()
    #   measurements_list$"C2 Tilt" <- round(spine_prescribed_list$spine_list$c2_tilt_value)
    #   measurements_list$"C2PA" <- round(spine_prescribed_list$spine_list$c2pa_value, 0)
    #   measurements_list$"T1PA" <- round(spine_prescribed_list$spine_list$t1pa_value, 0)
    #   measurements_list$"T4PA" <- round(spine_prescribed_list$spine_list$t4pa_value, 0)
    #   measurements_list$"L1PA" <- round(spine_prescribed_list$spine_list$l1pa_value, 0)
    #   measurements_list$"LL" <-  round(spine_prescribed_list$spine_list$lumbar_lordosis, 0)
    #   measurements_list$"TK" <- round(spine_prescribed_list$spine_list$thoracic_kyphosis, 0)
    #   measurements_list$"PT" <- round(spine_prescribed_list$spine_list$pelvic_tilt, 0)
    # 
    #   
    #   measurements_df <- enframe(measurements_list) %>%
    #     unnest() %>%
    #     mutate(x = if_else(name == "PI", 10, -26)) %>%
    #     mutate(y = c(seq(from = 43, by = -4, length = length(measurements_list)-2), 7, 3)) %>%
    #     mutate(label = paste(name, value, sep = " = ")) 
    #   
    #   segment_angles_list_for_plot <- list()
    #   segment_angles_list_for_plot$"L1-L2" <- as.character(round(segment_angles_plan$l1_segment_angle, 0))
    #   segment_angles_list_for_plot$"L2-L3" <- as.character(round(segment_angles_plan$l2_segment_angle, 0))
    #   segment_angles_list_for_plot$"L3-L4" <- as.character(round(segment_angles_plan$l3_segment_angle, 0))
    #   segment_angles_list_for_plot$"L4-L5" <- as.character(round(segment_angles_plan$l4_segment_angle, 0))
    #   segment_angles_list_for_plot$"L5-S1" <- as.character(round(segment_angles_plan$l5_segment_angle, 0))
    #   
    #   
    #   if(optimized_segment_angles_list$pso_level != "na"){
    #     pso_label <- paste("PSO at", str_to_upper(optimized_segment_angles_list$pso_level))
    #   }else{
    #     pso_label <- " "
    #   }
    #   
    #   
    #   
    #   # segment_angles_list_for_plot
    #   
    #   segment_angles_df <- enframe(segment_angles_list_for_plot) %>%
    #     unnest() %>%
    #     mutate(x = 22) %>%
    #     mutate(y = c(seq(from = 35, by = -4, length = length(segment_angles_list_for_plot)))) %>%
    #     mutate(label = paste(name, value, sep = " = "))
    #   
    #   # preop_c2t9pa_mismatch_input <- input$preop_c2pa - input$preop_t9pa
    #   
    #   # pjk_risk <- pjk_risk_function(age = input$preop_age, 
    #   #                                          sex = input$preop_sex, 
    #   #                                          uiv_region = "Upper Thoracic",
    #   #                                          preop_c2_t9pa_mismatch = (input$preop_c2pa - input$preop_t9pa), 
    #   #                                          pelvic_incidence = input$preop_pelvic_incidence)
    #   
    #   
    #   # pjk_risk_function <- function(age = 64.5,
    #   #                               sex = "Female",
    #   #                               preop_c2pa_t9pa_mismatch = 13.829296,
    #   #                               pelvic_incidence = 54.630633,
    #   #                               postop_l1pa_undercorrection = 1.4441251,
    #   #                               postop_t4l1pa_mismatch = 0.66524477) {
    #   #   yhat <- -2.0602845-0.013447506*age+0.35942783*(sex=="Male")+
    #   #     0.093758989*preop_c2pa_t9pa_mismatch-0.01734184*rad_pre_s1pi-
    #   #     0.023547139* postop_l1pa_undercorrection+
    #   #     3.792562e-05*pmax(postop_l1pa_undercorrection+3.8109099,0)^3-6.1591888e-05*pmax(postop_l1pa_undercorrection-1.4441251,0)^3+
    #   #     2.3666268e-05*pmax(postop_l1pa_undercorrection-9.865413,0)^3-0.14680941* postop_t4l1pa_mismatch+
    #   #     0.0011675493*pmax(postop_t4l1pa_mismatch+4.4705169,0)^3-0.0020066687*pmax(postop_t4l1pa_mismatch-0.66524477,0)^3+
    #   #     0.00083911949*pmax(postop_t4l1pa_mismatch-7.8111338,0)^3 
    #   #   
    #   #   round(plogis(yhat), 2)
    #   # }
    #   
    #   # new_vpa_list$c2pa_value - new_vpa_list$t4pa_value
    #   
    #   
    #   pjk_risk <- pjk_risk_function(age = input$preop_age, 
    #                                 sex = input$preop_sex, 
    #                                 preop_c2pa_t9pa_mismatch = (input$preop_c2pa - input$preop_t9pa), 
    #                                 pelvic_incidence = input$preop_pelvic_incidence, 
    #                                 postop_l1pa_undercorrection = new_vpa_list$l1pa_value - l1pa_target, 
    #                                 postop_t4l1pa_mismatch = new_vpa_list$t4pa_value - new_vpa_list$l1pa_value,
    #                                 uiv_region = "Upper Thoracic")
    #   
    #   
    #   pjk_risk_color <- case_when(
    #     pjk_risk > 0.45 ~ "red",
    #     between(pjk_risk, 0.35, 0.45) ~ "darkorange",
    #     between(pjk_risk, 0.2, 0.35) ~ "orange",
    #     pjk_risk < 0.2 ~ "darkgreen"
    #   )
    #   
    #   ggplot() +
    #     geom_sf(data = spine_geoms_df,
    #             aes(geometry = geom,
    #                 alpha = geom_alpha
    #             ),
    #             color = "black",
    #             fill = "grey90") +
    #     geom_sf(data = st_multipolygon(x = screw_list), fill = "grey55") +
    #     geom_sf(data = rod_sf, fill = "grey55") +
    #     draw_text(text = measurements_df$label, x = measurements_df$x, y = measurements_df$y, size = 14) +
    #     draw_text(text = segment_angles_df$label, x = segment_angles_df$x, y = segment_angles_df$y, size = 14) +
    #     draw_text(text = uiv_label, x = -30, y = 60, size = 14, fontface = "bold") +
    #     draw_text(text = pso_label, x = 22, y = 5, size = 16, fontface = "bold") +
    #     draw_text(text = paste("PJK Risk = ", pjk_risk), x = 0, y = -5, size = 16, color = pjk_risk_color, fontface = "bold") +
    #     lines_list +
    #     xlim(-35, 35) +
    #     # ylim(-6, 110) +
    #     theme_void() +
    #     labs(title = uiv_label) +
    #     theme(
    #       axis.text = element_blank(),
    #       axis.title = element_blank(),
    #       plot.background = element_rect(fill = "transparent", colour = NA),
    #       panel.background = element_rect(fill = "transparent", colour = NA)
    #     ) +
    #     scale_alpha_identity()
    #   
    # }
    # )
    

    
    ############## PSO OPTION 2 UIV T4
    spine_plan_uiv_t4_option_2 <- eventReactive(input$compute_plan, {
      build_upper_t_uiv_spine_plot_function(pso_option_number = 2, 
                                            preop_age = input$preop_age,
                                            preop_sex = input$preop_sex,
                                            preop_pelvic_incidence = input$preop_pelvic_incidence,
                                            preop_pt = input$preop_pt,
                                            preop_l1pa = input$preop_l1pa,
                                            preop_t9pa = input$preop_t9pa,
                                            preop_t4pa = input$preop_t4pa,
                                            preop_c2pa = input$preop_c2pa,
                                            l1pa_line_color = input$l1pa_line_color,
                                            t4pa_line_color = input$t4pa_line_color,
                                            c2pa_line_color = input$c2pa_line_color, 
                                            preop_segment_angles_input_list_reactive = preop_segment_angles_input_list_reactive(),
                                            preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive()
      )
    })
    
    # spine_plan_uiv_t4_option_2 <- eventReactive(input$compute_plan, {
    #   
    #   pso_list <- list()
    #   
    #   starting_segment_angles <- segment_angles_function_from_vertebral_pelvic_angles_function(pelvic_incidence_input = input$preop_pelvic_incidence,
    #                                                                                            l1pa_input = input$preop_l1pa,
    #                                                                                            l1s1_input = input$preop_l1s1,
    #                                                                                            t9pa_input = input$preop_t9pa, 
    #                                                                                            t4pa_input = input$preop_t4pa, 
    #                                                                                            c2pa_input = input$preop_c2pa)
    #   
    #   preop_cervical_lordosis <- starting_segment_angles$c7_segment_angle*6
    #   
    #   
    #   regional_analysis_list <- id_focal_deformity_function(pelvic_incidence = input$preop_pelvic_incidence,
    #                                                         l4_pelvic_angle = preop_spine_build_list_reactive()$spine_list$l4_pelvic_angle_value[1], 
    #                                                         l1_pelvic_angle = input$preop_l1pa,
    #                                                         t9_pelvic_angle = input$preop_t9pa, 
    #                                                         t4_pelvic_angle = input$preop_t4pa, 
    #                                                         c2_pelvic_angle = input$preop_c2pa)
    #   
    #   
    # 
    #   
    #   
    #   segment_angles_plan <- starting_segment_angles
    #   
    #   l1pa_target <- target_l1pa_function(pelvic_incidence = input$preop_pelvic_incidence)
    #   
    #   optimized_segment_angles_list <- compute_optimized_lumbar_segmental_lordosis_values(pelvic_incidence = input$preop_pelvic_incidence, 
    #                                                                                       l1_l2_start = starting_segment_angles$l1_segment_angle, 
    #                                                                                       l2_l3_start = starting_segment_angles$l2_segment_angle, 
    #                                                                                       l3_l4_start = starting_segment_angles$l3_segment_angle, 
    #                                                                                       l4_l5_start = starting_segment_angles$l4_segment_angle,
    #                                                                                       l5_s1_start = starting_segment_angles$l5_segment_angle, 
    #                                                                                       desired_l1pa = l1pa_target,
    #                                                                                       non_modifiable = preop_rigid_levels_vector_reactive() , 
    #                                                                                       pso_option_number = 2, 
    #                                                                                       uiv_region = "upper")
    #   
    #   
    #   segment_angles_plan$l1_segment_angle <-  optimized_segment_angles_list$l1_l2
    #   segment_angles_plan$l2_segment_angle <-  optimized_segment_angles_list$l2_l3
    #   segment_angles_plan$l3_segment_angle <-  optimized_segment_angles_list$l3_l4
    #   segment_angles_plan$l4_segment_angle <-  optimized_segment_angles_list$l4_l5
    #   segment_angles_plan$l5_segment_angle <-  optimized_segment_angles_list$l5_s1
    #   
    #   checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = input$preop_pelvic_incidence,
    #                                                                    pt_value = 10,
    #                                                                    segment_angle_list =segment_angles_plan)
    #   
    #   t4pa_lower_target <- checking_vpas$l1pa_value - 3
    #   t4pa_upper_target <- checking_vpas$l1pa_value + 1
    #   t4pa_target <- checking_vpas$l1pa_value - 1
    #   
    #   current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #   
    #   
    #   # while(round(checking_vpas$t4pa_value, 0) != round(t4pa_target, 0)){
    #   while(between(checking_vpas$t4pa_value, t4pa_lower_target, t4pa_upper_target) == FALSE){
    #     
    #     segment_modifier <- (checking_vpas$t4pa_value - t4pa_target)*0.5
    #     
    #     if("T10-T11" %in% preop_rigid_levels_vector_reactive()  == FALSE){
    #       segment_angles_plan$t10_segment_angle <- segment_angles_plan$t11_segment_angle + segment_modifier
    #     }
    #     if("T11-T12" %in% preop_rigid_levels_vector_reactive()  == FALSE){
    #       segment_angles_plan$t11_segment_angle <- segment_angles_plan$t11_segment_angle + segment_modifier
    #     }
    #     if("T12-L1" %in% preop_rigid_levels_vector_reactive()  == FALSE){
    #       segment_angles_plan$t12_segment_angle <- segment_angles_plan$t12_segment_angle + segment_modifier
    #     }   
    #     if("L1-L2" %in% preop_rigid_levels_vector_reactive()  == FALSE){
    #       segment_angles_plan$l1_segment_angle <- segment_angles_plan$l1_segment_angle + segment_modifier
    #     }
    #     
    #     checking_vpas <- build_spine_for_checking_pelvic_angles_function(pelv_inc_value = input$preop_pelvic_incidence, 
    #                                                                      pt_value = 10,
    #                                                                      segment_angle_list = segment_angles_plan)
    #     
    #     current_t4_l1pa_mismatch <- checking_vpas$t4pa_value - checking_vpas$l1pa_value
    #     
    #     
    #   } ## end of while loop
    #   
    #   new_vpa_list <- checking_vpas
    #   
    #   new_l1_s1 <- segment_angles_plan$l1_segment_angle + segment_angles_plan$l2_segment_angle +segment_angles_plan$l3_segment_angle + segment_angles_plan$l4_segment_angle +segment_angles_plan$l5_segment_angle
    #   
    #   predicted_pt <- predict_postop_pt_function(preop_pt = input$preop_pt, 
    #                                              preop_c2_tilt = (input$preop_c2pa - input$preop_pt), 
    #                                              postop_c2pa = (new_vpa_list$c2pa_value))
    #   
    #   
    #   spine_prescribed_list <- build_full_spine_function_new(pelv_inc_value = input$preop_pelvic_incidence,
    #                                                          segment_angle_list = segment_angles_plan,
    #                                                          pt_value = predicted_pt[1],
    #                                                          spine_faces = "right",
    #                                                          # pso_levels = as_vector(pso_list)
    #                                                          pso_levels = as_vector(optimized_segment_angles_list$pso_level)
    #   )
    #   
    #   
    #   
    #   spine_geoms_df <- spine_prescribed_list$spine_df %>%
    #     select(object, geom, geom_alpha)  
    #   
    #   lines_list <- list()
    #   
    #   lines_list$l1pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$l1pa_line_curve_sf,
    #                                                              color_input = input$l1pa_line_color, 
    #                                                              line_size = 1.75
    #   )
    #   
    #   lines_list$t4pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$t4pa_line_curve_sf,
    #                                                              color_input = input$t4pa_line_color,
    #                                                              line_size = 1
    #   )
    #   
    #   lines_list$c2pa_line <- jh_make_colored_line_geom_function(sf_geom_input = spine_prescribed_list$lines_list$c2pa_line_extended_curve_sf,
    #                                                              color_input = input$c2pa_line_color,
    #                                                              line_size = 0.5
    #   )
    #   
    #   preop_t4pa_l1pa_mismatch = input$preop_t4pa - input$preop_l1pa
    #   
    #   preop_c2pa_t4pa_mismatch <- input$preop_c2pa - input$preop_t4pa
    #   postop_c2pa_t4pa_mismatch <- new_vpa_list$c2pa_value - new_vpa_list$t4pa_value
    #   
    #   if(preop_c2pa_t4pa_mismatch - postop_c2pa_t4pa_mismatch > 10){
    #     uiv_for_rod <- 2 ## for C2 UIV
    #     uiv_label <- "UIV: C2"
    #   }else if(preop_c2pa_t4pa_mismatch - postop_c2pa_t4pa_mismatch > 5){
    #     uiv_for_rod <- 9 ## for T2 UIV
    #     uiv_label <- "UIV: T2"
    #   }else{
    #     uiv_for_rod <- 11 ## for T4 UIV
    #     uiv_label <- "UIV: T4"
    #   }
    #   
    #   # uiv_for_rod <- 11 ## for T4 UIV
    #   
    #   instrumented_vertebral_body_list <- spine_prescribed_list$vertebral_body_list[uiv_for_rod:length(spine_prescribed_list$vertebral_body_list)]
    #   
    #   sp_matrix_for_rod <- do.call(rbind, map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ rbind(instrumented_vertebral_body_list[[.x]]$sp)))
    #   
    #   rod_sf <- st_buffer(x = st_linestring(sp_matrix_for_rod), dist = 0.5, nQuadSegs = 5, joinStyle = "ROUND", endCapStyle = "ROUND")
    #   
    #   screw_list <- st_multipolygon(compact(map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ instrumented_vertebral_body_list[[.x]]$screw_sf)))
    #   
    #   measurements_list <- list()
    #   measurements_list$"C2 Tilt" <- round(spine_prescribed_list$spine_list$c2_tilt_value)
    #   measurements_list$"C2PA" <- round(spine_prescribed_list$spine_list$c2pa_value, 0)
    #   measurements_list$"T1PA" <- round(spine_prescribed_list$spine_list$t1pa_value, 0)
    #   measurements_list$"T4PA" <- round(spine_prescribed_list$spine_list$t4pa_value, 0)
    #   measurements_list$"L1PA" <- round(spine_prescribed_list$spine_list$l1pa_value, 0)
    #   measurements_list$"LL" <-  round(spine_prescribed_list$spine_list$lumbar_lordosis, 0)
    #   measurements_list$"TK" <- round(spine_prescribed_list$spine_list$thoracic_kyphosis, 0)
    #   measurements_list$"PT" <- round(spine_prescribed_list$spine_list$pelvic_tilt, 0)
    #   # measurements_list$"L1PA Deviation" <- round(lpa_deviation, 0)
    #   # measurements_list$"T4PA-L1PA Mismatch" <- round(t4pa_l1pa_mismatch, 0)
    #   
    #   measurements_df <- enframe(measurements_list) %>%
    #     unnest() %>%
    #     mutate(x = if_else(name == "PI", 10, -26)) %>%
    #     mutate(y = c(seq(from = 43, by = -4, length = length(measurements_list)-2), 7, 3)) %>%
    #     mutate(label = paste(name, value, sep = " = ")) 
    #   
    #   segment_angles_list_for_plot <- list()
    #   segment_angles_list_for_plot$"L1-L2" <- as.character(round(segment_angles_plan$l1_segment_angle, 0))
    #   segment_angles_list_for_plot$"L2-L3" <- as.character(round(segment_angles_plan$l2_segment_angle, 0))
    #   segment_angles_list_for_plot$"L3-L4" <- as.character(round(segment_angles_plan$l3_segment_angle, 0))
    #   segment_angles_list_for_plot$"L4-L5" <- as.character(round(segment_angles_plan$l4_segment_angle, 0))
    #   segment_angles_list_for_plot$"L5-S1" <- as.character(round(segment_angles_plan$l5_segment_angle, 0))
    #   
    #   
    #   if(optimized_segment_angles_list$pso_level != "na"){
    #     pso_label <- paste("PSO at", str_to_upper(optimized_segment_angles_list$pso_level))
    #   }else{
    #     pso_label <- " "
    #   }
    #   
    #   
    #   
    #   # segment_angles_list_for_plot
    #   
    #   segment_angles_df <- enframe(segment_angles_list_for_plot) %>%
    #     unnest() %>%
    #     mutate(x = 22) %>%
    #     mutate(y = c(seq(from = 35, by = -4, length = length(segment_angles_list_for_plot)))) %>%
    #     mutate(label = paste(name, value, sep = " = "))
    #   
    # 
    #   
    #   pjk_risk <- pjk_risk_function(age = input$preop_age, 
    #                     sex = input$preop_sex, 
    #                     preop_c2pa_t9pa_mismatch = (input$preop_c2pa - input$preop_t9pa), 
    #                     pelvic_incidence = input$preop_pelvic_incidence, 
    #                     uiv_region = "Upper Thoracic",
    #                     postop_l1pa_undercorrection = new_vpa_list$l1pa_value - l1pa_target, 
    #                     postop_t4l1pa_mismatch = new_vpa_list$t4pa_value - new_vpa_list$l1pa_value)
    #   
    #   pjk_risk_color <- case_when(
    #     pjk_risk > 0.45 ~ "red",
    #     between(pjk_risk, 0.35, 0.45) ~ "darkorange",
    #     between(pjk_risk, 0.2, 0.35) ~ "orange",
    #     pjk_risk < 0.2 ~ "darkgreen"
    #   )
    #   
    #   ggplot() +
    #     geom_sf(data = spine_geoms_df,
    #             aes(geometry = geom,
    #                 alpha = geom_alpha
    #             ),
    #             color = "black",
    #             fill = "grey90") +
    #     geom_sf(data = st_multipolygon(x = screw_list), fill = "grey55") +
    #     geom_sf(data = rod_sf, fill = "grey55") +
    #     draw_text(text = measurements_df$label, x = measurements_df$x, y = measurements_df$y, size = 14) +
    #     draw_text(text = segment_angles_df$label, x = segment_angles_df$x, y = segment_angles_df$y, size = 14) +
    #     draw_text(text = uiv_label, x = -30, y = 60, size = 14, fontface = "bold") +
    #     draw_text(text = pso_label, x = 22, y = 5, size = 16, fontface = "bold") +
    #     draw_text(text = paste("PJK Risk = ", pjk_risk), x = 0, y = -5, size = 16, color = pjk_risk_color, fontface = "bold") +
    #     lines_list +
    #     xlim(-35, 35) +
    #     # ylim(-6, 110) +
    #     theme_void() +
    #     # labs(title = "Option 2") +
    #     theme(
    #       axis.text = element_blank(),
    #       axis.title = element_blank(),
    #       plot.background = element_rect(fill = "transparent", colour = NA),
    #       panel.background = element_rect(fill = "transparent", colour = NA)
    #     ) +
    #     scale_alpha_identity()
    #   
    # }
    # )
    
    
    
    ############################ END END T4 UIV with risk END END ###############################
    ############################ END END T4 UIV with risk END END ###############################
    ############################ END END T4 UIV with risk END END ###############################
    ############################ END END T4 UIV with risk END END ###############################
    ############################ END END T4 UIV with risk END END ###############################
    ############################ END END T4 UIV with risk END END ###############################
    
    
    
    
    
    
    # output$spine_plan <- renderPlot({
    #     # spine_plan_plot2()
    #   plot_grid(spine_plan_uiv_t11(), NULL, spine_plan_uiv_t4(), spine_plan_uiv_t4_option_2(), nrow = 2)
    # })
    
    # output$spine_plan_lower_t <- renderPlot({
    #   
    #   plot_grid(spine_plan_uiv_t4(), spine_plan_uiv_t4_option_2(), nrow = 1)
    #   
    # })
    
 output$spine_plan_upper_t <- renderPlot({
   
   plot_grid(NULL, NULL, NULL,
             spine_plan_uiv_t4(), NULL, spine_plan_uiv_t4_option_2(), 
             nrow = 2, rel_heights = c(0.075, 0.925),
             rel_widths = c(1, -0.15, 1)) + 
     draw_text(text = "Upper Thoracic UIV", x = 0.5, y = 0.975, size = 18, fontfact = "bold")
   
 })
    
    compute_1s1_from_pi_l1pa_function <- function(pelvic_incidence, l1pa){
      round( 1.4*pelvic_incidence - 1.7*l1pa, 0)
    }
    
    output$preop_risk_plot <- renderPlot({
  
      # l1pa_start <- round((0.5*input$preop_pelvic_incidence - 20) - 5, 0)
      
  
      # l1s1_middle <- compute_1s1_from_pi_l1pa_function(pelvic_incidence = input$preop_pelvic_incidence, l1pa = (0.5*input$preop_pelvic_incidence - 20))
      # l1s1_low <- l1s1_middle - 12
      # l1s1_highest <- l1s1_middle + 12
      # 
      # full_risk_df <- expand_grid(l1pa = c(l1pa_start:(l1pa_start + 10)), l1_s1 = c(l1s1_low, l1s1_middle, l1s1_highest)) %>%
      #   mutate(pelvic_incidence = input$preop_pelvic_incidence) %>%
      #   mutate(sex = input$preop_sex) %>%
      #   mutate(age = input$preop_age) %>%
      #   mutate(uiv = "T11") %>%
      #   mutate(preop_c2t9pa_mismatch = input$preop_c2pa - input$preop_t9pa) %>%
      #   mutate(pjk_risk = compute_pjk_risk_with_lower_thoracic_uiv(age = age,
      #                                                              sex = sex,
      #                                                              uiv = uiv,
      #                                                              pelvic_incidence = pelvic_incidence,
      #                                                              l1pa = l1pa,
      #                                                              l1_s1 = l1_s1,
      #                                                              pre_c2pa_t9pa_mismatch = preop_c2t9pa_mismatch)) %>%
      #   mutate(l1_s1 = fct_inorder(as.character(l1_s1)))
      
      preop_c2t9pa_mismatch_input <- input$preop_c2pa - input$preop_t9pa
      
      lower_thoracic_risk <- pjk_risk_function(age = input$preop_age, 
                                               sex = input$preop_sex, 
                                               uiv_region = "Lower Thoracic",
                                               preop_c2_t9pa_mismatch = preop_c2t9pa_mismatch_input, 
                                               pelvic_incidence = input$preop_pelvic_incidence)
      
      upper_thoracic_risk <- pjk_risk_function(age = input$preop_age, 
                        sex = input$preop_sex, 
                        uiv_region = "Upper Thoracic",
                        preop_c2_t9pa_mismatch = preop_c2t9pa_mismatch_input, 
                        pelvic_incidence = input$preop_pelvic_incidence)
      
      tibble(uiv = c("Upper Thoracic", "Lower Thoracic"), 
             pjk_risk = c(upper_thoracic_risk, lower_thoracic_risk)) %>%
        ggplot(aes(x =uiv, y = pjk_risk, color = uiv)) + 
        geom_point(size = 2) + 
        ylab("Risk of PJK") +
        xlab("UIV") +
        theme_minimal()
      
      # full_risk_df %>%
      #   ggplot(aes(x = l1pa, y = pjk_risk, color = l1_s1)) +
      #   geom_smooth() +
      #   ylab("Risk of PJK") +
      #   xlab("L1PA") +
      #   theme_minimal()
      
    })
    
    output$measures_table <- renderTable({
      # preop_spine_build_list_reactive()$spine_df %>%
        
        tibble(pelvic_tilt = preop_spine_build_list_reactive()$spine_list$pelvic_tilt,
               l1_pelvic_angle = preop_spine_build_list_reactive()$spine_list$l1_pelvic_angle_value, 
               lumbar_lordosis = preop_spine_build_list_reactive()$spine_list$lumbar_lordosis,
               t9_pelvic_angle = preop_spine_build_list_reactive()$spine_list$t9_pelvic_angle_value,
               t4_pelvic_angle = preop_spine_build_list_reactive()$spine_list$t4_pelvic_angle_value,
               c2_pelvic_angle = preop_spine_build_list_reactive()$spine_list$c2_pelvic_angle_value,
               thoracic_kyphosis = preop_spine_build_list_reactive()$spine_list$thoracic_kyphosis,
               cervical_lordosis = preop_spine_build_list_reactive()$spine_list$cervical_lordosis) %>%
        distinct() %>%
        pivot_longer(cols = everything()) %>%
        mutate(value = round(value, 0))
    })
    
    
    #############     #############  UPLOAD TAB #######################     ############# 
    #############     #############  UPLOAD TAB #######################     ############# 
    #############     #############  UPLOAD TAB #######################     ############# 
    #############     #############  UPLOAD TAB #######################     ############# 
    #############     #############  UPLOAD TAB #######################     ############# 
    #############     #############  UPLOAD TAB #######################     ############# 
    #############     #############  UPLOAD TAB #######################     ############# 
    #############     #############  UPLOAD TAB #######################     ############# 


    observeEvent(input$xray_file, {
      req(input$xray_file)
      updateSwitchInput(session = session, inputId = "xray_file_uploaded", value = TRUE)
    })
    
    click_coord_reactive_list <- reactiveValues(coords = list(), index = 1)
    
    imageState <- reactiveValues(zoomLevel = 1, panX = 0, panY = 0)
    
    # # Observe updates to zoom and pan from the front end
    # observeEvent(input$xray_click, priority = 1, {
    #   imageState$zoomLevel <- input$zoomLevel
    # }, ignoreInit = TRUE)
    # 
    # observeEvent(input$xray_click, priority = 2, {
    #   imageState$panX <- input$panX
    # }, ignoreInit = TRUE)
    # 
    # observeEvent(input$xray_click,priority = 3, {
    #   imageState$panY <- input$panY
    # }, ignoreInit = TRUE)
    # 
    # # When the plot is clicked, restore the previous zoom and pan state
    # # observeEvent(xray_click_coordinates_reactive_df(),, {
    # observeEvent(list(imageState), priority = 10, {
    #   session$sendCustomMessage('restoreTransform', list(
    #     zoomLevel = imageState$zoomLevel,
    #     panX = imageState$panX,
    #     panY = imageState$panY
    #   ))
    # })
    # observeEvent(input$zoomLevel, {
    #   imageState$zoomLevel <- input$zoomLevel
    # }, ignoreInit = TRUE)
    # 
    # observeEvent(input$panX, {
    #   imageState$panX <- input$panX
    # }, ignoreInit = TRUE)
    # 
    # observeEvent(input$panY, {
    #   imageState$panY <- input$panY
    # }, ignoreInit = TRUE)
    
    observeEvent(input$xray_click, {
      imageState$zoomLevel <- input$zoomLevel
    }, ignoreInit = TRUE)
    
    observeEvent(input$xray_click, {
      imageState$panX <- input$panX
    }, ignoreInit = TRUE)
    
    observeEvent(input$xray_click, {
      imageState$panY <- input$panY
    }, ignoreInit = TRUE)
    
    # When the plot is clicked, manually restore the previous zoom and pan state
    # observeEvent(list(input$xray_click, xray_reactive_plot() ), {
    #   session$sendCustomMessage('restoreTransform', list(
    #     zoomLevel = isolate(imageState$zoomLevel),  # Use isolate to avoid triggering reactivity
    #     panX = isolate(imageState$panX),
    #     panY = isolate(imageState$panY)
    #   ))
    # })
    
    observeEvent(input$all_centroids_recorded, {
      imageState$zoomLevel <- 1
      imageState$panX <- 0
      imageState$panY <- 0
      
      session$sendCustomMessage('restoreTransform', list(
        zoomLevel = isolate(imageState$zoomLevel),  # Use isolate to avoid triggering reactivity
        panX = isolate(imageState$panX),
        panY = isolate(imageState$panY)
      ))
    })
    
    
    # Reset button to clear all points
    observeEvent(input$xray_reset_points, {
      click_coord_reactive_list$coords <- list()
      click_coord_reactive_list$index <- 1
    })
    
    # Button to remove the last recorded point
    observeEvent(input$xray_delete_last_point, {
      if (click_coord_reactive_list$index > 1) {
        click_coord_reactive_list$coords[[click_coord_reactive_list$index - 1]] <- NULL
        click_coord_reactive_list$index <- click_coord_reactive_list$index - 1
      }
    })
    
    output$icon_xray_orientation <- renderUI({
      if (input$xray_orientation){
        image_object <- tags$img(
          src = "icon_facing_left.svg",  # Remove 'www/', correct path to icon
          style = "width: 50%; height: auto;",  # Set width to 50%, maintain aspect ratio
          alt = "Left Facing Icon"
        )
      }else{
        image_object <-tags$img(
          src = "icon_facing_right.svg",  # Remove 'www/', correct path to icon
          style = "width: 50%; height: auto;",  # Set width to 50%, maintain aspect ratio
          alt = "Right Facing Icon"
        )
      }
      
      div(
        style = "text-align: center;",  # Center the image
        actionLink(
          inputId = "orientation_icon_click",  # Input ID for action
          label = image_object
        )
      )
    })
    
    observeEvent(input$orientation_icon_click, {
      updatePrettyToggle(session = session, 
                         inputId = "xray_orientation",
                         value = if_else(input$xray_orientation == TRUE, FALSE, TRUE)
      )
    }
    )
    
    
    spine_orientation_reactive <- reactive({
      xray_orientation <- if_else(input$xray_orientation, "left", "right")
      xray_orientation
    })
    
    
    observeEvent(input$xray_click, {
      # if(nrow(xray_click_coordinates_reactive_df())==3){
      if(length(click_coord_reactive_list$coords)==3){
        fem_head_x <- click_coord_reactive_list$coords$fem_head_center[[1]]
        s1_anterior_superior_x <- click_coord_reactive_list$coords$s1_anterior_superior[[1]]
        s1_posterior_superior_x <- click_coord_reactive_list$coords$s1_posterior_superior[[1]]
        
        if(s1_anterior_superior_x < s1_posterior_superior_x){
          xray_orientation <- "left"
        }else{
          xray_orientation <- "right"
        }
        
        updatePrettyToggle(session = session, 
                           inputId = "xray_orientation",
                           value = if_else(xray_orientation == "left", TRUE, FALSE)
        )
      }
    }
    )
    
    
    # Define the labels for both modes
    get_spine_labels <- function(all_centroids = FALSE) {
      if (all_centroids) {
        return(c("fem_head_center", 
                 "s1_anterior_superior", "s1_posterior_superior", 
                 "l5_centroid", "l4_centroid", "l3_centroid", "l2_centroid", "l1_centroid", 
                 "t12_centroid", "t11_centroid", "t10_centroid", "t9_centroid", 
                 "t8_centroid", "t7_centroid", "t6_centroid", "t5_centroid", 
                 "t4_centroid", "t3_centroid", "t2_centroid", "t1_centroid", 
                 "c7_centroid", "c6_centroid", "c5_centroid", "c4_centroid", "c3_centroid", 
                 "c2_centroid"))
      } else {
        return(c("fem_head_center", 
                 "s1_anterior_superior", "s1_posterior_superior", 
                 "l4_centroid", "l1_centroid", "t9_centroid", "t4_centroid", 
                 "t1_centroid", "c2_centroid"))
      }
    }
    
    # Store clicks and assign them to the correct label
    observeEvent(input$xray_click, {
      spine_input_labels <- get_spine_labels(input$xray_input_all_centroids)
      
      if (click_coord_reactive_list$index <= length(spine_input_labels)) {
        target_name <- spine_input_labels[click_coord_reactive_list$index]
        click_coord_reactive_list$coords[[target_name]] <- c(input$xray_click$x, input$xray_click$y)
        click_coord_reactive_list$index <- click_coord_reactive_list$index + 1
      }
    })
    
    
    # Render instructions dynamically based on the number of recorded clicks
    output$xray_click_instructions <- renderText({
      spine_input_labels <- get_spine_labels(input$xray_input_all_centroids)
      click_count <- length(click_coord_reactive_list$coords)
      
      if (click_count < length(spine_input_labels)) {
        instruction <- spine_input_labels[click_count + 1]
        
        instruction <- str_replace_all(instruction, "fem_head_center", "Center of Hips")
        instruction <- str_replace_all(instruction, "_superior", "_superior Corner")
        
        instruction <- str_to_title(str_replace_all(instruction, "_", " "))
        
        # instruction <- paste("Click the", instruction)
        
        instruction <- glue("Click:<br>{instruction}")
        
      } else {
        instruction <- "All points recorded."
      }
      HTML("<div>", instruction, "</div>")
    })
    
    observeEvent(input$xray_click, {
      spine_input_labels <- get_spine_labels(input$xray_input_all_centroids)
      click_count <- length(click_coord_reactive_list$coords)
      if (click_count >= length(spine_input_labels)) {
      
        updateSwitchInput(session = session, inputId = "all_centroids_recorded", value = TRUE)
        }
      
    })

  
    
    # Reactive tibble for clicked coordinates
    xray_click_coordinates_reactive_df <- reactive({
      coords_df <- tibble(spine_point = character(), x = double(), y = double())
      
      if (length(click_coord_reactive_list$coords) > 0) {
        # Convert the named list to a tibble
        coords_df <- enframe(click_coord_reactive_list$coords, name = "spine_point", value = "coords")
        
        # Split 'coords' into 'x' and 'y' columns
        coords_df <- coords_df %>%
          mutate(x = map_dbl(coords, 1), y = map_dbl(coords, 2)) %>%
          select(spine_point, x, y)
      }
      
      coords_df
    })
    
  
    
    output$xray_click_df <- renderTable({
      xray_click_coordinates_reactive_df()
      
    })
    
    # Reactive tibble for centroid coordinates
    xray_centroid_coordinates_reactive_df <- reactive({
      # Get clicked coordinates dataframe
      xray_click_coordinates_df <- xray_click_coordinates_reactive_df()
      
      # Labels for spine points that we need to have in the final dataframe
      spine_coordinate_labels <- tibble(
        spine_point = c("fem_head_center", "s1_center", "l5_centroid", "l4_centroid", "l3_centroid",
                        "l2_centroid", "l1_centroid", "t12_centroid", "t11_centroid", "t10_centroid",
                        "t9_centroid", "t8_centroid", "t7_centroid", "t6_centroid", "t5_centroid",
                        "t4_centroid", "t3_centroid", "t2_centroid", "t1_centroid", "c7_centroid",
                        "c6_centroid", "c5_centroid", "c4_centroid", "c3_centroid", "c2_centroid")
      )
      
      # Calculate S1 center based on anterior and posterior clicks
      s1_center <- if ("s1_anterior_superior" %in% names(click_coord_reactive_list$coords) && 
                       "s1_posterior_superior" %in% names(click_coord_reactive_list$coords)) {
        s1_anterior <- click_coord_reactive_list$coords$s1_anterior_superior
        s1_posterior <- click_coord_reactive_list$coords$s1_posterior_superior
        c((s1_anterior[[1]] + s1_posterior[[1]]) / 2, (s1_anterior[[2]] + s1_posterior[[2]]) / 2)
      } else {
        c(NA, NA)
      }
      
      # Build the tibble including the S1 center
      current_coords_df <-  tibble(spine_point = "s1_center", x = s1_center[1], y = s1_center[2]) %>%
        bind_rows(xray_click_coordinates_df %>%
                    filter(spine_point != "s1_anterior_superior", 
                           spine_point != "s1_posterior_superior",
                           spine_point != "fem_head_center"))
      
      # If all centroids input is TRUE, we only display the clicked centroids
      if (input$xray_input_all_centroids) {
        final_coords_df <- current_coords_df %>%
          filter(!is.na(x))  # Only return clicked points
      } else if(any(xray_click_coordinates_df$spine_point == "c2_centroid")){
        
        spine_coordinate_labels_df <- tibble(
          spine_point = c("s1_center", "l5_centroid",
                          "l4_centroid", "l3_centroid", "l2_centroid", "l1_centroid", "t12_centroid",
                          "t11_centroid", "t10_centroid", "t9_centroid", "t8_centroid", "t7_centroid",
                          "t6_centroid", "t5_centroid", "t4_centroid", "t3_centroid", "t2_centroid",
                          "t1_centroid", "c7_centroid", "c6_centroid", "c5_centroid", "c4_centroid",
                          "c3_centroid", "c2_centroid")
        )%>%
          mutate(index_count = row_number())
        
        spine_coordinates_short_df <- spine_coordinate_labels_df %>%
          left_join(current_coords_df) %>%
          filter(!is.na(x))
        
        spine_y_filled_df <- spine_coordinate_labels_df %>%
          left_join(spine_coordinates_short_df) %>%
          mutate(y = zoo::na.approx(y, rule = 2)) %>%
          mutate(y = round(y, 3))
        
        final_coords_df <- spine_y_filled_df %>%
          mutate(x = spline(spine_coordinates_short_df$y, spine_coordinates_short_df$x, xout = spine_y_filled_df$y)$y) %>%
          select(spine_point, x, y) %>%
          mutate(x = round(x, 3))
        
      }else{
        final_coords_df <- current_coords_df  %>%
          filter(!is.na(x))  # Return what has been clicked so far
        
      }
      
      
      final_coords_df
    })
    
    
    output$centroid_coordinates_df <- renderTable({
      xray_centroid_coordinates_reactive_df()
      
    })
    
    alignment_parameters_reactivevalues_list <- reactiveValues()
    
    
    observeEvent(input$xray_click, {
      
      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
      
      if((any(names(alignment_parameters_list) == "pelvic_incidence") == FALSE) & any(xray_centroid_coordinates_reactive_df()$spine_point == "s1_center")){
        fem_head_center <- click_coord_reactive_list$coords$fem_head_center
        
        s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior[[1]] + click_coord_reactive_list$coords$s1_posterior_superior[[1]])/2,
                       (click_coord_reactive_list$coords$s1_anterior_superior[[2]] + click_coord_reactive_list$coords$s1_posterior_superior[[2]])/2)
        
        ### COMPUTE PT ###
        fem_head_to_s1_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
                                                                                 point_2 = s1_center) ## hypotenuse
        
        fem_head_to_s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
                                                                                   point_2 = c(s1_center[[1]], fem_head_center[[2]])) ## opposite

        pt_orientation_modifier <- case_when(
          input$xray_orientation & fem_head_center[[1]] < s1_center[[1]] ~ 1,
          input$xray_orientation & fem_head_center[[1]] > s1_center[[1]] ~ -1,
          input$xray_orientation == FALSE & fem_head_center[[1]] > s1_center[[1]] ~ 1,
          input$xray_orientation  == FALSE & fem_head_center[[1]] < s1_center[[1]] ~ -1
        )
        
        alignment_parameters_reactivevalues_list$pelvic_tilt <- asin(fem_head_to_s1_x_length/fem_head_to_s1_length)*180/pi*pt_orientation_modifier

        ### COMPUTE SS ###
        s1_length <- jh_calculate_distance_between_2_points_function(point_1 = click_coord_reactive_list$coords$s1_anterior_superior,
                                                                     point_2 = click_coord_reactive_list$coords$s1_posterior_superior)
        
        s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior[[1]],
                                                                                   click_coord_reactive_list$coords$s1_posterior_superior[[2]]),
                                                                       point_2 = click_coord_reactive_list$coords$s1_posterior_superior)
        
        alignment_parameters_reactivevalues_list$sacral_slope <- acos(s1_x_length/s1_length)*180/pi
        
        ### COMPUTE PI ###
        alignment_parameters_reactivevalues_list$pelvic_incidence <- alignment_parameters_reactivevalues_list$pelvic_tilt + alignment_parameters_reactivevalues_list$sacral_slope
        
      }
      
      ## COMPUTE ALL VPAs ##
      if(any(xray_centroid_coordinates_reactive_df()$spine_point == "c2_centroid")){
        
        vpa_df <- xray_centroid_coordinates_reactive_df() %>%
          filter(spine_point != "s1_center") %>%
          mutate(vpa = map2(.x = x, .y = y, .f = ~ jh_compute_vpa_from_xray_data_function(fem_head_center = click_coord_reactive_list$coords$fem_head_center,
                                                                                          vertebral_centroid = c(.x, .y),
                                                                                          spine_facing = if_else(input$xray_orientation, "left", "right"),
                                                                                          pelvic_tilt = alignment_parameters_reactivevalues_list$pelvic_tilt
          )
          )
          ) %>%
          unnest() %>%
          mutate(vpa_label = str_replace_all(spine_point, "_centroid", "pa")) %>%
          select(vpa_label, vpa)
        
        vpa_list <- as.list(vpa_df$vpa)
        names(vpa_list) <- vpa_df$vpa_label
        
        for (name in names(vpa_list)) {
          alignment_parameters_reactivevalues_list[[name]] <- vpa_list[[name]]
        }
      }
      
      
    })
    
  
    
    
    ###############
    
    # xray_estimated_segment_angles_reactive_list <- reactive({
    #   segment_angles_estimated_list <- list()
    # 
    #   if(any(names(alignment_parameters_reactive_list()) == "c2pa")){
    # 
    #   pelvic_incidence <- alignment_parameters_reactive_list()$pelvic_incidence
    # 
    #   spine_alignment_measures_list <- alignment_parameters_reactive_list()
    # 
    #   segment_angles_estimated_list$l5_segment_angle <- xray_l5_segment_angle_function(pelvic_incidence = pelvic_incidence,
    #                                                                                    l5_pelvic_angle = spine_alignment_measures_list$l5pa,
    #                                                                                    l4_pelvic_angle = spine_alignment_measures_list$l4pa)
    # 
    #   segment_angles_estimated_list$l4_segment_angle <- xray_l4_segment_angle_function(pelvic_incidence = pelvic_incidence,
    #                                                                                    l5_pelvic_angle = spine_alignment_measures_list$l5pa,
    #                                                                                    l4_pelvic_angle = spine_alignment_measures_list$l4pa,
    #                                                                                    l3_pelvic_angle = spine_alignment_measures_list$l3pa,
    #                                                                                    l5_s1 = sum(unlist(segment_angles_estimated_list)))
    # 
    # 
    #   segment_angles_estimated_list$l3_segment_angle <- xray_l3_segment_angle_function(pelvic_incidence = pelvic_incidence,
    #                                                                                    l4_pelvic_angle = spine_alignment_measures_list$l4pa,
    #                                                                                    l3_pelvic_angle = spine_alignment_measures_list$l3pa,
    #                                                                                    l2_pelvic_angle = spine_alignment_measures_list$l2pa,
    #                                                                                    l4_s1 = sum(unlist(segment_angles_estimated_list)))
    # 
    #   # L2 Segment Angle
    #   segment_angles_estimated_list$l2_segment_angle <- xray_l2_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     l3_pelvic_angle = spine_alignment_measures_list$l3pa,
    #     l2_pelvic_angle = spine_alignment_measures_list$l2pa,
    #     l1_pelvic_angle = spine_alignment_measures_list$l1pa,
    #     l3_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # L1 Segment Angle
    #   segment_angles_estimated_list$l1_segment_angle <- xray_l1_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     l2_pelvic_angle = spine_alignment_measures_list$l2pa,
    #     l1_pelvic_angle = spine_alignment_measures_list$l1pa,
    #     t12_pelvic_angle = spine_alignment_measures_list$t12pa,
    #     l2_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T12 Segment Angle
    #   segment_angles_estimated_list$t12_segment_angle <- xray_t12_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     l1_pelvic_angle = spine_alignment_measures_list$l1pa,
    #     t12_pelvic_angle = spine_alignment_measures_list$t12pa,
    #     t11_pelvic_angle = spine_alignment_measures_list$t11pa,
    #     l1_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T11 Segment Angle
    #   segment_angles_estimated_list$t11_segment_angle <- xray_t11_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t12_pelvic_angle = spine_alignment_measures_list$t12pa,
    #     t11_pelvic_angle = spine_alignment_measures_list$t11pa,
    #     t10_pelvic_angle = spine_alignment_measures_list$t10pa,
    #     t12_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T10 Segment Angle
    #   segment_angles_estimated_list$t10_segment_angle <- xray_t10_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t11_pelvic_angle = spine_alignment_measures_list$t11pa,
    #     t10_pelvic_angle = spine_alignment_measures_list$t10pa,
    #     t9_pelvic_angle = spine_alignment_measures_list$t9pa,
    #     t11_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T9 Segment Angle
    #   segment_angles_estimated_list$t9_segment_angle <- xray_t9_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t10_pelvic_angle = spine_alignment_measures_list$t10pa,
    #     t9_pelvic_angle = spine_alignment_measures_list$t9pa,
    #     t8_pelvic_angle = spine_alignment_measures_list$t8pa,
    #     t10_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T8 Segment Angle
    #   segment_angles_estimated_list$t8_segment_angle <- xray_t8_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t9_pelvic_angle = spine_alignment_measures_list$t9pa,
    #     t8_pelvic_angle = spine_alignment_measures_list$t8pa,
    #     t7_pelvic_angle = spine_alignment_measures_list$t7pa,
    #     t9_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T7 Segment Angle
    #   segment_angles_estimated_list$t7_segment_angle <- xray_t7_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t8_pelvic_angle = spine_alignment_measures_list$t8pa,
    #     t7_pelvic_angle = spine_alignment_measures_list$t7pa,
    #     t6_pelvic_angle = spine_alignment_measures_list$t6pa,
    #     t8_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T6 Segment Angle
    #   segment_angles_estimated_list$t6_segment_angle <- xray_t6_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t7_pelvic_angle = spine_alignment_measures_list$t7pa,
    #     t6_pelvic_angle = spine_alignment_measures_list$t6pa,
    #     t5_pelvic_angle = spine_alignment_measures_list$t5pa,
    #     t7_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T5 Segment Angle
    #   segment_angles_estimated_list$t5_segment_angle <- xray_t5_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t6_pelvic_angle = spine_alignment_measures_list$t6pa,
    #     t5_pelvic_angle = spine_alignment_measures_list$t5pa,
    #     t4_pelvic_angle = spine_alignment_measures_list$t4pa,
    #     t6_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T4 Segment Angle
    #   segment_angles_estimated_list$t4_segment_angle <- xray_t4_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t5_pelvic_angle = spine_alignment_measures_list$t5pa,
    #     t4_pelvic_angle = spine_alignment_measures_list$t4pa,
    #     t3_pelvic_angle = spine_alignment_measures_list$t3pa,
    #     t5_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T3 Segment Angle
    #   segment_angles_estimated_list$t3_segment_angle <- xray_t3_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t4_pelvic_angle = spine_alignment_measures_list$t4pa,
    #     t3_pelvic_angle = spine_alignment_measures_list$t3pa,
    #     t2_pelvic_angle = spine_alignment_measures_list$t2pa,
    #     t4_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    # 
    #   # T2 Segment Angle
    #   segment_angles_estimated_list$t2_segment_angle <- xray_t2_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t3_pelvic_angle = spine_alignment_measures_list$t3pa,
    #     t2_pelvic_angle = spine_alignment_measures_list$t2pa,
    #     t1_pelvic_angle = spine_alignment_measures_list$t1pa,
    #     t3_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # T1 Segment Angle
    #   segment_angles_estimated_list$t1_segment_angle <- xray_t1_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t2_pelvic_angle = spine_alignment_measures_list$t2pa,
    #     t1_pelvic_angle = spine_alignment_measures_list$t1pa,
    #     c7_pelvic_angle = spine_alignment_measures_list$c7pa,
    #     t2_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # C7 Segment Angle
    #   segment_angles_estimated_list$c7_segment_angle <- xray_c7_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     t1_pelvic_angle = spine_alignment_measures_list$t1pa,
    #     c7_pelvic_angle = spine_alignment_measures_list$c7pa,
    #     c6_pelvic_angle = spine_alignment_measures_list$c6pa,
    #     t1_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # C6 Segment Angle
    #   segment_angles_estimated_list$c6_segment_angle <- xray_c6_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     c7_pelvic_angle = spine_alignment_measures_list$c7pa,
    #     c6_pelvic_angle = spine_alignment_measures_list$c6pa,
    #     c5_pelvic_angle = spine_alignment_measures_list$c5pa,
    #     c7_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # C5 Segment Angle
    #   segment_angles_estimated_list$c5_segment_angle <- xray_c5_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     c6_pelvic_angle = spine_alignment_measures_list$c6pa,
    #     c5_pelvic_angle = spine_alignment_measures_list$c5pa,
    #     c4_pelvic_angle = spine_alignment_measures_list$c4pa,
    #     c6_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    #   # C4 Segment Angle
    #   segment_angles_estimated_list$c4_segment_angle <- xray_c4_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     c5_pelvic_angle = spine_alignment_measures_list$c5pa,
    #     c4_pelvic_angle = spine_alignment_measures_list$c4pa,
    #     c3_pelvic_angle = spine_alignment_measures_list$c3pa,
    #     c5_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # C3 Segment Angle
    #   segment_angles_estimated_list$c3_segment_angle <- xray_c3_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     c4_pelvic_angle = spine_alignment_measures_list$c4pa,
    #     c3_pelvic_angle = spine_alignment_measures_list$c3pa,
    #     c2_pelvic_angle = spine_alignment_measures_list$c2pa,
    #     c4_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   # C2 Segment Angle
    #   segment_angles_estimated_list$c2_segment_angle <- xray_c2_segment_angle_function(
    #     pelvic_incidence = pelvic_incidence,
    #     c3_pelvic_angle = spine_alignment_measures_list$c3pa,
    #     c2_pelvic_angle = spine_alignment_measures_list$c2pa,
    #     c3_s1 = sum(unlist(segment_angles_estimated_list))
    #   )
    # 
    #   segment_angles_estimated_list$c1_segment_angle <- segment_angles_estimated_list$c2_segment_angle
    # 
    #   }
    #   segment_angles_estimated_list
    # 
    # })
    
    output$alignment_parameters_df <- renderTable({

      enframe(reactiveValuesToList(alignment_parameters_reactivevalues_list)) %>%
        mutate(name = str_replace_all(name, "pelvic_tilt", "PT")) %>%
        mutate(name = str_replace_all(name, "pelvic_incidence", "PI")) %>%
        mutate(name = str_replace_all(name, "sacral_slope", "SS")) %>%
        mutate(name = str_to_upper(name))

    })
    
    # output$xray_plot_ui <- renderUI({
    #   
    #   # Ensure the image is uploaded
    #   req(input$xray_file)
    #   
    #   # Read the uploaded image to get its dimensions
    #   xray <- image_read(path = input$xray_file$datapath)
    #   
    #   # Extract the image's height and width
    #   xray_height <- image_info(xray)$height
    #   xray_width <- image_info(xray)$width
    #   
    #       # Use imageState reactive values to trigger reactivity
    #   zoomLevel <- isolate(imageState$zoomLevel)
    #   panX <- isolate(imageState$panX)
    #   panY <- isolate(imageState$panY)
    #   
    #   # Set up the dynamic UI for the plot, adjusting the height and width
    #   div(
    #     id = "image_container",
    #     plotOutput(
    #       outputId = "xray",
    #       click = "xray_click",
    #       height = paste0(xray_height, "px"),
    #       width = paste0(xray_width, "px")
    #     ),
    #     
    #     # Style for container and image
    #     tags$style(HTML(paste0("
    #     #image_container {
    #       overflow: hidden;
    #       width: 100%;
    #       height: ", xray_height, "px;
    #       position: relative;
    #       display: flex;
    #     }
    #     #image_container img {
    #       transition: transform 0.15s ease;
    #       cursor: crosshair;
    #     }
    #     #image_container img:active {
    #       cursor: grabbing;
    #     }
    #   "))),
    #     
    #     # JavaScript to handle zoom and pan
    #     tags$script(HTML(paste0("
    #     var zoomLevel = ", zoomLevel, ";
    #     var panX = ", panX, ";
    #     var panY = ", panY, ";
    #     var isPanning = false;
    #     var startX, startY;
    # 
    #     function applyTransform() {
    #       var imgElement = document.querySelector('#image_container img');
    #       if (imgElement) {
    #         imgElement.style.transform = 'scale(' + zoomLevel + ') translate(' + panX + 'px, ' + panY + 'px)';
    #       }
    #     }
    # 
    #     // Zoom in/out on scroll
    #     document.getElementById('image_container').addEventListener('wheel', function(event) {
    #       event.preventDefault();
    #       if (event.deltaY > 0) {
    #         zoomLevel *= 0.9;
    #       } else {
    #         zoomLevel *= 1.1;
    #       }
    #       applyTransform();
    #       Shiny.setInputValue('zoomLevel', zoomLevel, {priority: 'event'});
    #     });
    # 
    #     // Start panning on right-click (mousedown)
    #     document.getElementById('image_container').addEventListener('mousedown', function(event) {
    #       if (event.button === 2) {
    #         isPanning = true;
    #         startX = event.clientX;
    #         startY = event.clientY;
    #         var imgElement = document.querySelector('#image_container img');
    #         if (imgElement) {
    #           imgElement.style.cursor = 'grabbing';
    #         }
    #       }
    #     });
    # 
    #     // Stop panning on mouseup
    #     document.addEventListener('mouseup', function(event) {
    #       isPanning = false;
    #       var imgElement = document.querySelector('#image_container img');
    #       if (imgElement) {
    #         imgElement.style.cursor = 'crosshair';
    #       }
    #     });
    # 
    #     // Handle mouse movement for panning
    #     document.addEventListener('mousemove', function(event) {
    #       if (isPanning) {
    #         var deltaX = event.clientX - startX;
    #         var deltaY = event.clientY - startY;
    #         panX += deltaX;
    #         panY += deltaY;
    #         applyTransform();
    #         Shiny.setInputValue('panX', panX, {priority: 'event'});
    #         Shiny.setInputValue('panY', panY, {priority: 'event'});
    #         startX = event.clientX;
    #         startY = event.clientY;
    #       }
    #     });
    # 
    #     // Prevent the right-click menu from showing up
    #     document.addEventListener('contextmenu', function(event) {
    #       event.preventDefault();
    #     });
    # 
    #     // Restore zoom and pan state after the plot is clicked
    #     Shiny.addCustomMessageHandler('restoreTransform', function(message) {
    #       zoomLevel = message.zoomLevel;
    #       panX = message.panX;
    #       panY = message.panY;
    #       applyTransform();
    #     });
    #   ")))
    #   )
    # })

    # output$xray_plot_ui <- renderUI({
    #   # Ensure the image is uploaded
    #   req(input$xray_file)
    #   
    #   # Read the uploaded image to get its dimensions
    #   xray <- image_read(path = input$xray_file$datapath)
    #   
    #   # Extract the image's height and width
    #   xray_height <- image_info(xray)$height
    #   xray_width <- image_info(xray)$width
    #   
    #   # Set up the dynamic UI for the plot, adjusting the height and width
    #   div(
    #     id = "image_container",
    #     plotOutput(
    #       outputId = "xray",
    #       click = "xray_click",  # Clicks are still processed by Shiny
    #       height = paste0(xray_height, "px"),
    #       width = paste0(xray_width, "px")
    #     ),
    #     
    #     # Style for container and image
    #     tags$style(HTML(paste0("
    #   #image_container {
    #     overflow: hidden;
    #     width: 100%;
    #     height: ", xray_height, "px;
    #     position: relative;
    #     display: flex;
    #   }
    #   #image_container img {
    #     transition: transform 0.15s ease;
    #     cursor: crosshair;
    #   }
    #   #image_container img:active {
    #     cursor: grabbing;
    #   }
    # "))),
    #     
    #     # JavaScript to handle zoom and pan on the client side
    #     tags$script(HTML("
    #   var zoomLevel = 1;
    #   var panX = 0;
    #   var panY = 0;
    #   var isPanning = false;
    #   var startX, startY;
    # 
    #   function applyTransform() {
    #     var imgElement = document.querySelector('#image_container img');
    #     if (imgElement) {
    #       imgElement.style.transform = 'scale(' + zoomLevel + ') translate(' + panX + 'px, ' + panY + 'px)';
    #     }
    #   }
    # 
    #   // Zoom in/out on scroll
    #   document.getElementById('image_container').addEventListener('wheel', function(event) {
    #     event.preventDefault();
    #     if (event.deltaY > 0) {
    #       zoomLevel *= 0.9; // Zoom out
    #     } else {
    #       zoomLevel *= 1.1; // Zoom in
    #     }
    #     applyTransform(); // Apply the updated zoom level
    #   });
    # 
    #   // Start panning on right-click (mousedown)
    #   document.getElementById('image_container').addEventListener('mousedown', function(event) {
    #     if (event.button === 2) { // Right-click for panning
    #       isPanning = true;
    #       startX = event.clientX;
    #       startY = event.clientY;
    #       var imgElement = document.querySelector('#image_container img');
    #       if (imgElement) {
    #         imgElement.style.cursor = 'grabbing';
    #       }
    #     }
    #   });
    # 
    #   // Stop panning on mouseup
    #   document.addEventListener('mouseup', function(event) {
    #     isPanning = false;
    #     var imgElement = document.querySelector('#image_container img');
    #     if (imgElement) {
    #       imgElement.style.cursor = 'crosshair'; // Reset to crosshair
    #     }
    #   });
    # 
    #   // Handle mouse movement for panning
    #   document.addEventListener('mousemove', function(event) {
    #     if (isPanning) {
    #       var deltaX = event.clientX - startX;
    #       var deltaY = event.clientY - startY;
    #       panX += deltaX;
    #       panY += deltaY;
    #       applyTransform(); // Apply the updated pan
    #       startX = event.clientX;
    #       startY = event.clientY;
    #     }
    #   });
    # 
    #   // Prevent the right-click menu from showing up
    #   document.addEventListener('contextmenu', function(event) {
    #     event.preventDefault();
    #   });
    # "))
    #   )
    # })
    # 



    ##############################

    xray_reactive_plot <- reactive({
      # req(input$xray)
      req(input$xray_file)
  
      xray <-  image_read(path = input$xray_file$datapath)
      xray_height <- image_info(xray)$height
      xray_width <- image_info(xray)$width
      
      xlim_left <-0.5 - (xray_width/xray_height)/2 
      xlim_right <-0.5 + (xray_width/xray_height)/2 
      
      xray_plot <- ggdraw() +
        draw_image(
          xray,
          x = 0, 
          y = 0, 
          width = 1,
          height = 1
          # width = xray_width, 
          # height = xray_height
        ) 

      if(nrow(xray_click_coordinates_reactive_df()) > 0) {
        coord_df <- xray_click_coordinates_reactive_df()

        xray_plot <- xray_plot +
          geom_point(data = coord_df,
                     aes(x = x, y = y), color = "red", size = 2) 

      }
      
      if(any(names(click_coord_reactive_list$coords) == "c2_centroid")){

        spine_colors_df <- xray_centroid_coordinates_reactive_df() %>%
          mutate(spine_point = str_remove_all(spine_point, "_centroid|_center")) %>%
          filter(spine_point != "s1") %>%
          mutate(spine_color = case_when(
            str_detect(spine_point, "c") ~ "lightblue",
            str_detect(spine_point, "t") ~ "lightgreen",
            str_detect(spine_point, "l") ~ "darkblue"
            )
            )
        
        xray_plot <- xray_plot +
          geom_path(data = xray_centroid_coordinates_reactive_df(),
                     aes(x = x, y = y), color = "lightblue", size = 1) +
          geom_point(data = spine_colors_df,
        aes(x = x, y = y, color = spine_color, fill = spine_color),size = 2) + 
      scale_fill_identity() +
      scale_color_identity()
      }
      
      # xray_estimated_segment_angles_reactive_list
      # alignment_parameters_list <- alignment_parameters_reactive_list()
      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
      
       if(length(click_coord_reactive_list$coords)>2 & any(names(alignment_parameters_list) == "pelvic_incidence")){
         
         s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior[[1]] + click_coord_reactive_list$coords$s1_posterior_superior[[1]])/2,
                        (click_coord_reactive_list$coords$s1_anterior_superior[[2]] + click_coord_reactive_list$coords$s1_posterior_superior[[2]])/2)
         
         pi_df <- calculate_pelvic_incidence_line_coordinates(fem_head_center = click_coord_reactive_list$coords$fem_head_center,
                                                     s1_anterior = click_coord_reactive_list$coords$s1_anterior_superior, 
                                                     s1_posterior = click_coord_reactive_list$coords$s1_posterior_superior, 
                                                     spine_facing = if_else(input$xray_orientation, "left", "right"),
                                                     pelvic_tilt = alignment_parameters_list$pelvic_tilt, 
                                                     pelvic_incidence_value = alignment_parameters_list$pelvic_incidence
                                                     )
         
      
        xray_plot <- xray_plot +
          geom_path(data = pi_df,
                    aes(x = x, y = y), color = "darkgreen", size = 1.5)

      }
      
      if(any(names(click_coord_reactive_list$coords) == "l1_centroid") & any(names(alignment_parameters_list) == "l1pa")){
        spine_coordinates_df <- tibble(spine_point = "s1_center",
                                       x = s1_center[[1]],
                                       y = s1_center[[2]]) %>%
          union_all(xray_click_coordinates_reactive_df()) %>%
          filter(spine_point %in% c("fem_head_center", "s1_anterior_superior", "s1_posterior_superior") == FALSE)

        l1pa_df <- tibble(x = c(s1_center[[1]],
                                click_coord_reactive_list$coords$fem_head_center[[1]],
                                click_coord_reactive_list$coords$l1_centroid[[1]]),
                          y = c(s1_center[[2]],
                                click_coord_reactive_list$coords$fem_head_center[[2]],
                                click_coord_reactive_list$coords$l1_centroid[[2]]))

        xray_plot <- xray_plot +
          geom_path(data = l1pa_df,
                    aes(x = x, y = y),
                    color = "darkblue", size = 1)

      }
      
      if(any(names(click_coord_reactive_list$coords) == "t4_centroid") & any(names(alignment_parameters_list) == "t4pa")){
        t4pa_df <- tibble(x = c(s1_center[[1]],
                                click_coord_reactive_list$coords$fem_head_center[[1]],
                                click_coord_reactive_list$coords$t4_centroid[[1]]),
                          y = c(s1_center[[2]],
                                click_coord_reactive_list$coords$fem_head_center[[2]],
                                click_coord_reactive_list$coords$t4_centroid[[2]]))

        xray_plot <- xray_plot +
          geom_path(data = t4pa_df,
                    aes(x = x, y = y),
                    color = "purple", size = 1)
        
      }

      # Add coord_fixed to ensure a 1:1 aspect ratio
      xray_plot <- xray_plot + coord_fixed(xlim = c(xlim_left, xlim_right), 
                                           ylim = c(0, 1))
      xray_plot
      
    })
    
    output$xray <- renderPlot({
      xray_reactive_plot() 
    }
    )
    
    output$preop_spine_simulation_plot <- renderPlot({
      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
      if(any(names(alignment_parameters_list) == "c2pa")){
        
        if(any(names(click_coord_reactive_list$coords) == "c2_centroid")){
          

          
          spine_simulation_list <- build_full_spine_from_vertebral_pelvic_angles_function(pelv_inc_value = alignment_parameters_list$pelvic_incidence,
                                                                                          pt_value = alignment_parameters_list$pelvic_tilt,
                                                                                          l1pa_value_input = alignment_parameters_list$l1pa,
                                                                                          # l1s1_value_input = alignment_parameters_list$preop_l1s1,
                                                                                          # t10_l2_value_input = alignment_parameters_list$preop_t10_l2,
                                                                                          t9pa_value_input = alignment_parameters_list$t9pa,
                                                                                          t4pa_value_input = alignment_parameters_list$t4pa,
                                                                                          c2pa_value_input = alignment_parameters_list$c2pa,
                                                                                          # c2_c7_value_input = alignment_parameters_list$preop_c2c7,
                                                                                          # input_segment_angles = "yes",
                                                                                          # segment_angles_input = segment_angles_list,
                                                                                          spine_faces = if_else(input$xray_orientation, "left", "right")
          )

        spine_geoms_df <- spine_simulation_list$spine_df %>%
          select(object, geom, geom_alpha)

        ggplot() +
          geom_sf(data = spine_geoms_df,
                  color = "black",
                  aes(geometry = geom,
                      alpha = geom_alpha,
                      fill = "grey90")) +
          ylim(-20, 110) +
          theme_void() +
          labs(title = "Preop Alignment") +
          theme(
            axis.text = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(
              size = 16,
              hjust = 0.5,
              vjust = -0.5,
              face = "bold.italic"
            ),
            plot.background = element_rect(fill = "transparent", colour = NA),
            panel.background = element_rect(fill = "transparent", colour = NA)
          ) +
          # draw_text(text = measurements_df$label,
          #           x = measurements_df$x,
          #           y = measurements_df$y, size = 11) +
          scale_fill_identity() +
          scale_alpha_identity()
        } 
          # coord_fixed()

      }

    })
    
   
    
    output$preop_spine_simulation_xray_ui <- renderUI({
      
      # column(width = 3, 
      fluidRow(
             plotOutput(outputId = "preop_spine_simulation_plot",
                        height = "850px"),
             br(),
             hr(),
             h4("Click Coordinates:"),
             tableOutput(outputId = "xray_click_df"),
             h4("Centroid Coordinates:"),
             tableOutput(outputId = "centroid_coordinates_df"),
             # h4("Segment Angles"), 
             # tableOutput(outputId = "segment_angles_df")
      )
      

    })
    
    output$preop_xray_rigid_segments_ui <- renderUI({
      # preop_segment_angles_list <- preop_segment_angles_list_reactive()
      
      segment_angles_input_list <- rev(map(spinal_segments_labels_vector, function(label) {
        create_spine_rigid_level_input_function(segment_input_label = label)
      }))
      # create_spine_rigid_level_input_function
      
      # column(width = 5,
      fluidRow(
        h4("Select any Fused Levels:"),
             box(width = 12,title = "Cervical", 
                 collapsible = TRUE, 
                 collapsed = TRUE,
                 h5("Check box if Rigid Level"),
                 segment_angles_input_list[1:6] %>% tagList()
             ),
             box(width = 12,title = "Thoracic", 
                 collapsible = TRUE, 
                 collapsed = TRUE,
                 h5("Check box if Rigid Level"),
                 segment_angles_input_list[7:19] %>% tagList()
             ),
             box(width = 12,title = "Lumbar", 
                 collapsible = TRUE, 
                 collapsed = FALSE,
                 h5("Check box if Rigid Level"),
                 segment_angles_input_list[20:24]%>% tagList()
             )
      )
    })
    
    preop_rigid_levels_vector_reactive_xray <- reactive({
      # segment_id <- paste0("preop_", str_to_lower(str_replace_all(segment_input_label, pattern = "-", "_")), "_rigid")
      rigid_levels <- c("na")
      if(input$preop_l5_s1_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L5-S1")} 
      if(input$preop_l4_l5_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L4-L5")} 
      if(input$preop_l3_l4_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L3-L4")} 
      if(input$preop_l2_l3_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L2-L3")} 
      if(input$preop_l1_l2_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "L1-L2")} 
      if(input$preop_t12_l1_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T12-L1")} 
      if(input$preop_t11_t12_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T11-T12")} 
      if(input$preop_t10_t11_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T10-T11")} 
      if(input$preop_t9_t10_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T10-T9")} 
      if(input$preop_t8_t9_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T8-T9")} 
      if(input$preop_t7_t8_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T7-T8")} 
      if(input$preop_t6_t7_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T6-T7")} 
      if(input$preop_t5_t6_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T5-T6")} 
      if(input$preop_t4_t5_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T4-T5")} 
      if(input$preop_t3_t4_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T3-T4")} 
      if(input$preop_t2_t3_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T2-T3")} 
      if(input$preop_t1_t2_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "T1-T2")} 
      if(input$preop_c7_t1_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C7-T1")} 
      if(input$preop_c6_c7_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C6-C7")} 
      if(input$preop_c5_c6_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C5-C6")} 
      if(input$preop_c4_c5_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C4-C5")} 
      if(input$preop_c3_c4_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C3-C4")} 
      if(input$preop_c2_c3_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C2-C3")} 
      if(input$preop_c1_c2_rigid_xray == TRUE){rigid_levels <- append(rigid_levels, "C1-C2")} 
      # set_names(map_chr(spinal_segments_labels_vector, ~ paste0(str_to_lower(strsplit(.x, "-")[[1]][1]), "_segment_angle")))
      if(length(rigid_levels)>0){
        rigid_levels <- map_chr(rigid_levels, ~ paste0(str_to_lower(strsplit(.x, "-")[[1]][1]), "_segment"))
      }
      rigid_levels
    })
    
    
    spine_plan_uiv_t11_option_1_xray <- eventReactive(input$compute_plan_xray, {
      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
      
      build_t11_spine_plot_function(pso_option_number = 1, 
                                    preop_age = input$preop_age,
                                    preop_sex = input$preop_sex,
                                    preop_pelvic_incidence = alignment_parameters_list$pelvic_incidence,
                                    preop_pt = alignment_parameters_list$pelvic_tilt,
                                    preop_l1pa = alignment_parameters_list$l1pa,
                                    preop_t9pa = alignment_parameters_list$t9pa,
                                    preop_t4pa = alignment_parameters_list$t4pa,
                                    preop_c2pa = alignment_parameters_list$c2pa,
                                    l1pa_line_color = input$l1pa_line_color,
                                    t4pa_line_color = input$t4pa_line_color,
                                    c2pa_line_color = input$c2pa_line_color, 
                                    # preop_segment_angles_input_list_reactive = xray_estimated_segment_angles_reactive_list(),
                                    preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive_xray(),
                                    return_list_or_plot = "list"
      )
    })
    
    
    spine_plan_uiv_t11_option_2_xray <- eventReactive(input$compute_plan_xray, {
      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
      
      build_t11_spine_plot_function(pso_option_number = 2, 
                                    preop_age = input$preop_age,
                                    preop_sex = input$preop_sex,
                                    preop_pelvic_incidence = alignment_parameters_list$pelvic_incidence,
                                    preop_pt = alignment_parameters_list$pelvic_tilt,
                                    preop_l1pa = alignment_parameters_list$l1pa,
                                    preop_t9pa = alignment_parameters_list$t9pa,
                                    preop_t4pa = alignment_parameters_list$t4pa,
                                    preop_c2pa = alignment_parameters_list$c2pa,
                                    l1pa_line_color = input$l1pa_line_color,
                                    t4pa_line_color = input$t4pa_line_color,
                                    c2pa_line_color = input$c2pa_line_color, 
                                    # preop_segment_angles_input_list_reactive = xray_estimated_segment_angles_reactive_list(),
                                    preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive_xray(), 
                                    return_list_or_plot = "list"
      )
    })
    
    
    output$spine_plan_lower_t_xray <- renderPlot({
      
      if(spine_plan_uiv_t11_option_1_xray()$pso_level == "none"){

                plot_grid(NULL,
                  spine_plan_uiv_t11_option_1_xray()$prescribed_plot, 
                  nrow = 2, rel_heights = c(0.075, 0.925)
        ) + 
          draw_text(text = "Prescribed Alignment: Lower Thoracic UIV", x = 0.5, y = 0.975, size = 18, fontfact = "bold")
        
      }else{
        plot_grid(NULL, NULL, NULL,
                  spine_plan_uiv_t11_option_1_xray()$prescribed_plot, NULL, spine_plan_uiv_t11_option_2_xray()$prescribed_plot, 
                  nrow = 2, rel_heights = c(0.075, 0.925), 
                  rel_widths = c(1, -0.15, 1)
        ) + 
          draw_text(text = "Prescribed Alignment: Lower Thoracic UIV", x = 0.5, y = 0.975, size = 18, fontfact = "bold")
        
      }

      

    })
    
    spine_plan_uiv_t4_option_1_xray <- eventReactive(input$compute_plan_xray, {
      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
      build_upper_t_uiv_spine_plot_function(pso_option_number = 1, 
                                            preop_age = input$preop_age,
                                            preop_sex = input$preop_sex,
                                            preop_pelvic_incidence = alignment_parameters_list$pelvic_incidence,
                                            preop_pt = alignment_parameters_list$pelvic_tilt,
                                            preop_l1pa = alignment_parameters_list$l1pa,
                                            preop_t9pa = alignment_parameters_list$t9pa,
                                            preop_t4pa = alignment_parameters_list$t4pa,
                                            preop_c2pa = alignment_parameters_list$c2pa,
                                            l1pa_line_color = input$l1pa_line_color,
                                            t4pa_line_color = input$t4pa_line_color,
                                            c2pa_line_color = input$c2pa_line_color, 
                                            # preop_segment_angles_input_list_reactive = xray_estimated_segment_angles_reactive_list(),
                                            preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive_xray(),
                                            return_list_or_plot = "list"
      )
    })
    
    
    ############## PSO OPTION 2 UIV T4
    spine_plan_uiv_t4_option_2_xray <- eventReactive(input$compute_plan_xray, {
      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
      build_upper_t_uiv_spine_plot_function(pso_option_number = 2, 
                                            preop_age = input$preop_age,
                                            preop_sex = input$preop_sex,
                                            preop_pelvic_incidence = alignment_parameters_list$pelvic_incidence,
                                            preop_pt = alignment_parameters_list$pelvic_tilt,
                                            preop_l1pa = alignment_parameters_list$l1pa,
                                            preop_t9pa = alignment_parameters_list$t9pa,
                                            preop_t4pa = alignment_parameters_list$t4pa,
                                            preop_c2pa = alignment_parameters_list$c2pa,
                                            l1pa_line_color = input$l1pa_line_color,
                                            t4pa_line_color = input$t4pa_line_color,
                                            c2pa_line_color = input$c2pa_line_color, 
                                            # preop_segment_angles_input_list_reactive = xray_estimated_segment_angles_reactive_list(),
                                            preop_rigid_levels_vector_reactive = preop_rigid_levels_vector_reactive_xray(), #preop_rigid_levels_vector_reactive()
                                            return_list_or_plot = "list"
                                            )
    })
    
    output$spine_plan_upper_t_xray <- renderPlot({
      
      if(spine_plan_uiv_t4_option_1_xray()$pso_level == "none"){
        
        plot_grid(NULL,
                  spine_plan_uiv_t4_option_1_xray()$prescribed_plot, 
                  nrow = 2, rel_heights = c(0.075, 0.925)
        ) + 
          draw_text(text = "Prescribed Alignment: Upper Thoracic UIV", x = 0.5, y = 0.975, size = 18, fontfact = "bold")
        
      }else{
        plot_grid(NULL, NULL, NULL,
                  spine_plan_uiv_t4_option_1_xray()$prescribed_plot, NULL, spine_plan_uiv_t4_option_2_xray()$prescribed_plot, 
                  nrow = 2, rel_heights = c(0.075, 0.925), 
                  rel_widths = c(1, -0.15, 1)
        ) + 
          draw_text(text = "Prescribed Alignment: Upper Thoracic UIV", x = 0.5, y = 0.975, size = 18, fontfact = "bold")
        
      }
      
      
      # plot_grid(NULL, NULL, NULL,
      #           spine_plan_uiv_t4_option_1_xray(), NULL, spine_plan_uiv_t4_option_2_xray(), 
      #           nrow = 2, rel_heights = c(0.075, 0.925),
      #           rel_widths = c(1, -0.15, 1)) + 
      #   draw_text(text = "Prescribed Alignment: Upper Thoracic UIV", x = 0.5, y = 0.975, size = 18, fontfact = "bold")
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

