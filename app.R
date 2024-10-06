#library(colourpicker)
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

# all_possible_lumbar_segments_angles_with_lpa_df <- read_csv("all_possible_lumbar_segment_angles_for_lpa.csv")

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



ui <- dashboardPage(
  dashboardHeader(title = "SolaSpine"),
  dashboardSidebar(
    fileInput("image", "Upload an Image", accept = c('image/png', 'image/jpeg', 'image/jpg')), 
    br(), 
    h4("Xray Orientation:"),
    actionBttn(
      inputId = "spine_orientation_button",
      label = "Facing LEFT", 
      style = "material-flat",
      color = "primary",
      icon = icon("arrow-left")
    ) 
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(width = 3,
          fluidRow(
            column(width = 12, 
                   # h4("Click Instructions")
                   tags$div(
                     style = "font-size: 20px; 
                 font-weight: bold; 
                 color: yellow; 
                 font-family: arial; 
                 font-style: italic; 
                 text-align: center; 
                 background-color: black; 
                 padding: 3px; 
                 border-radius: 12px;  /* Rounded corners */
                 display: block;",
                     htmlOutput(outputId = "xray_click_instructions")
                   )
            )
          ),
          fluidRow(
            column(width = 2), 
            column(width = 8, 
                   tags$div(
                     id = "image-container",
                     style = "position: relative; width: 350px; height: 750px; overflow: hidden; border: 0px solid #ccc;",
                     tags$img(
                       id = "uploadedImage",
                       src = "",
                       style = "position: absolute; top: 0; left: 0; cursor: crosshair;"
                     )
                   ),
                   tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"),
                   tags$script(HTML("
       $(document).ready(function() {
  let scale = 1;
  let panX = 0, panY = 0;
  let isPanning = false;
  let startX, startY;

  function updateImageTransform() {
    $('#uploadedImage').css({
      'transform-origin': 'top left',
      'transform': `translate(${panX}px, ${panY}px) scale(${scale})`
    });

    // Update positions of all dots to match the transformation
    $('.dot').each(function() {
      const originalX = $(this).data('orig-x');
      const originalY = $(this).data('orig-y');
      const adjustedX = (originalX * scale) + panX;
      const adjustedY = ((imageHeight - originalY) * scale) + panY;

      $(this).css({
        left: adjustedX + 'px',
        top: adjustedY + 'px'
      });
    });
  }

  let imageHeight = null; // We'll determine the height once the image is loaded

  Shiny.addCustomMessageHandler('load-image', function(data) {
    var img = document.getElementById('uploadedImage');
    img.src = data.src;

    // Once the image loads, set the natural height
    img.onload = function() {
      imageHeight = img.naturalHeight;
      // Reset scaling and position when new image is loaded
      scale = 1;
      panX = 0;
      panY = 0;
      updateImageTransform();

      // Remove existing dots when a new image is loaded
      $('.dot').remove();
    };
  });

  Shiny.addCustomMessageHandler('plot-coordinates', function(data) {
    // Remove existing dots
    $('.dot').remove();

    if (!imageHeight) {
      console.error('Image height not set yet.');
      return;
    }

    // Plot all coordinates
    data.coords.forEach(function(coord, index) {
      console.log(`Plotting point ${index + 1}:`, coord);  // Debugging log for each coordinate

      // Create a new dot element
      const dot = $('<div class=\"dot\"></div>');

      // Adjust Y-coordinate for the Cartesian system
      const adjustedX = (coord.x * scale) + panX;
      const adjustedY = ((imageHeight - coord.y) * scale) + panY;  // Adjusting Y-coordinate

      // Debugging log for adjusted positions
      console.log('Adjusted position for dot:', { adjustedX, adjustedY });
      
      // Adjust the dot's CSS to center it on the point clicked
    const dotSize = 10; // This is the width and height of the dot (in pixels)
    const correctionOffset = 0.5; // Small adjustment if there’s still an offset issue

    dot.css({
      position: 'absolute',
      top: (adjustedY - (dotSize / 2)) + correctionOffset + 'px',
      left: (adjustedX - (dotSize / 2)) + correctionOffset + 'px',
      width: dotSize + 'px',
      height: dotSize + 'px',
      'background-color': 'red',
      'border-radius': '50%',
      'pointer-events': 'none', // Ensures dots don't interfere with panning/zooming
      'z-index': 10 // Ensures the dots are layered above the image
    });

      // Store original coordinates for reference during zoom and pan
      dot.data('orig-x', coord.x);
      dot.data('orig-y', coord.y);

      // Append dot to the image container
      $('#image-container').append(dot);
    });

    // Immediately update dot positions to reflect the current zoom and pan state
    updateImageTransform();
  });

  // Handle zoom with the mouse wheel
  $('#image-container').on('wheel', function(e) {
    e.preventDefault();
    const zoomIntensity = 0.1;
    const delta = e.originalEvent.deltaY > 0 ? -1 : 1;
    const previousScale = scale;

    // Update scale
    scale *= (1 + delta * zoomIntensity);
    scale = Math.min(Math.max(0.5, scale), 5);

    // Calculate new pan to keep the zoom centered at mouse position
    const mouseX = e.pageX - $(this).offset().left;
    const mouseY = e.pageY - $(this).offset().top;

    panX = mouseX - (mouseX - panX) * (scale / previousScale);
    panY = mouseY - (mouseY - panY) * (scale / previousScale);

    updateImageTransform();
  });

  // Handle panning with right-click only
  $('#image-container').on('mousedown', function(e) {
    if (e.which === 3) { // Right-click
      isPanning = true;
      startX = e.pageX - panX;
      startY = e.pageY - panY;
      $(this).css('cursor', 'grabbing');
      return false; // Prevent context menu
    }
  });

  $(document).on('mouseup', function() {
    isPanning = false;
    $('#image-container').css('cursor', 'crosshair');
  });

  $(document).on('mousemove', function(e) {
    if (!isPanning) return;
    panX = e.pageX - startX;
    panY = e.pageY - startY;

    updateImageTransform();
  });

  // Prevent the default context menu from appearing on right-click
  $('#image-container').on('contextmenu', function(e) {
    return false;
  });

  // Record click coordinates on left-click
  $('#image-container').on('click', function(e) {
    if (e.which === 1) { // Left-click
      var img = document.getElementById('uploadedImage');
      const rect = img.getBoundingClientRect(); // Get the image's bounding box relative to the viewport

      // Get the click coordinates relative to the image
      const clickX = e.clientX - rect.left;
      const clickY = e.clientY - rect.top;

      // Adjust the coordinates for the current pan and zoom level to get the original image reference frame
      const adjustedX = clickX / scale;
      const adjustedY = clickY / scale;

      // Correcting Y-coordinate (flipping the y-axis based on the height of the image)
      const correctedY = imageHeight - adjustedY;

      // Debugging log for adjusted click positions
      console.log('Adjusted click position:', { adjustedX, correctedY });

      // Send the corrected click coordinates to the Shiny server
      Shiny.setInputValue('xray_click', {x: adjustedX - 1, y: correctedY + 1}, {priority: 'event'});
    }
  });
});
      "))
            ),
            column(width = 2),
            fluidRow(
              column(width = 2),
              column(
                width = 3,
                actionBttn(
                  inputId = "xray_delete_last_point",
                  size = "sm",
                  label = "Delete Last",
                  style = "jelly",
                  color = "success",
                  icon = icon("delete-left")
                )
              ),
              column(width = 2),
              column(
                width = 3,
                actionBttn(
                  size = "sm",
                  inputId = "xray_reset_points",
                  label = "Reset",
                  style = "unite",
                  color = "danger",
                  icon = icon("trash-can")
                )
              ),
              column(width = 2)
            )
          )
      ),
      box(width = 3, 
          plotOutput(outputId = "xray_plot")
      ),
      box(width = 5,
          title = "Tables",
          h4("Click Coordinates:"),
          tableOutput(outputId = "click_coordinates_df"),
          br(),
          h4("Centroid Coordinates:"),
          tableOutput(outputId = "centroid_coordinates_df"), 
          br(), 
          textOutput("xray_centroid_tibble_text")
      ),
      box(title = "VPA Measures", 
          # textOutput(outputId = "alignment_parameters_df_text")
          tableOutput("alignment_parameters_df")
      )
    )
  )
)





# Server logic
server <- function(input, output, session) {
  
  spine_orientation <- reactiveVal("left")
  
  observeEvent(input$spine_orientation_button, {
    # Update the spine orientation value
    if (spine_orientation() == "left") {
      spine_orientation("right")
      updateActionButton(session, "spine_orientation_button", label = "Facing RIGHT", icon = icon("arrow-right"))
    } else {
      spine_orientation("left")
      updateActionButton(session, "spine_orientation_button", label = "Facing LEFT", icon = icon("arrow-left"))
    }
  })
  
  observeEvent(input$image, {
    req(input$image)
    
    # Create a base64-encoded URI for the uploaded image
    image_path <- input$image$datapath
    image_src <- base64enc::dataURI(file = image_path, mime = input$image$type)
    
    # Send the image URI to the UI
    session$sendCustomMessage('load-image', list(src = image_src))
  })
  
  click_coord_reactive_list <- reactiveValues(coords = list(), index = 1)
  
  
  
  # Reset button to clear all points
  observeEvent(input$xray_reset_points, ignoreInit = TRUE, {
    click_coord_reactive_list$coords <- list()
    click_coord_reactive_list$index <- 1
  })
  
  # Button to remove the last recorded point
  observeEvent(input$xray_delete_last_point, ignoreInit = TRUE, {
    if (click_coord_reactive_list$index > 1) {
      click_coord_reactive_list$coords[[click_coord_reactive_list$index - 1]] <- NULL
      click_coord_reactive_list$index <- click_coord_reactive_list$index - 1
    }
    
    if(length(plot_points_coordinates_reactiveval())>0){
      plot_points_list <- plot_points_coordinates_reactiveval()
      
      plot_points_list <- plot_points_list[-length(plot_points_list)]
      
      plot_points_coordinates_reactiveval(plot_points_list)
      
    }
  })
  
  
  plot_points_coordinates_reactiveval <- reactiveVal()
  
  # Store clicks and assign them to the correct label
  observeEvent(input$xray_click, {
    spine_input_labels <- get_spine_labels(FALSE)
    
    # Only proceed if there's a label available for the current index
    if (click_coord_reactive_list$index <= length(spine_input_labels)) {
      target_name <- spine_input_labels[click_coord_reactive_list$index]
      # Create a named list with the click coordinates
      new_click <- list(x = input$xray_click$x, y = input$xray_click$y)
      
      # Append the new click as a named list with the label
      click_coord_reactive_list$coords[[target_name]] <- new_click
      click_coord_reactive_list$index <- click_coord_reactive_list$index + 1
    }
    
    # Debugging step to print the structure of click_coord_reactive_list$coords
    print(str(click_coord_reactive_list$coords))
    
    # Convert the named list to a list of lists
    coords_for_js <- unname(lapply(click_coord_reactive_list$coords, function(coord) {
      list(x = coord$x, y = coord$y)
    }))
    
    # Debugging step to print coords_for_js
    print(coords_for_js)
    
    plot_points_coordinates_reactiveval(coords_for_js)
    
    # Send the coordinates to JavaScript
    # session$sendCustomMessage('plot-coordinates', list(coords = coords_for_js))
  })
  
  observeEvent(list(input$xray_click, input$xray_delete_last_point, input$xray_reset_points), {
    
    session$sendCustomMessage('plot-coordinates', list(coords = plot_points_coordinates_reactiveval()))
  })
  
  
  
  xray_instructions_reactiveval <- reactiveVal("x")
  
  # Render instructions dynamically based on the number of recorded clicks
  output$xray_click_instructions <- renderText({
    spine_input_labels <- get_spine_labels(FALSE)
    click_count <- length(click_coord_reactive_list$coords)
    
    if (click_count < length(spine_input_labels)) {
      instruction <- spine_input_labels[click_count + 1]
      
      instruction <- str_replace_all(instruction, "fem_head_center", "Center of Hips")
      instruction <- str_replace_all(instruction, "_superior", "_superior Corner")
      
      instruction <- str_to_title(str_replace_all(instruction, "_", " "))
      
      instruction <- glue("Click:<br>{instruction}")
      
    } else {
      instruction <- "All points recorded."
      xray_instructions_reactiveval("Completed")
    }
    HTML("<div>", instruction, "</div>")
  })
  
  
  # Create a reactive table to display coordinates
  click_coordinates_df_reactive <- reactive({
    if (length(click_coord_reactive_list$coords) > 0) {
      # Convert the list to a tibble
      tibble(
        spine_point = names(click_coord_reactive_list$coords),
        x = map_dbl(click_coord_reactive_list$coords, "x"),
        y = map_dbl(click_coord_reactive_list$coords, "y")
      )
    } else {
      tibble(spine_point = character(), x = double(), y = double())
    }
  })
  
  output$click_coordinates_df <- renderTable({
    # click_coordinates_df_reactive()
    
    plotting_coord_list <- plot_points_coordinates_reactiveval()
    
    if (length(plotting_coord_list) > 0) {
      # Convert the list to a tibble
      plotting_coord_df <- tibble(
        spine_point = names(click_coord_reactive_list$coords),
        x = map_dbl(plotting_coord_list, "x"),
        y = map_dbl(plotting_coord_list, "y")
      ) 
      
      plotting_coord_df 
      # mutate(y = abs(y - max(plotting_coord_df$y)))
      
    } else {
      tibble(spine_point = character(), x = double(), y = double())
    }
  })
  
  
  ############ COMPUTE CENTROIDS #################
  # Reactive tibble for centroid coordinates
  xray_centroid_coordinates_reactive_df <- reactive({
    # Get clicked coordinates dataframe
    xray_click_coordinates_df <- click_coordinates_df_reactive()
    
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
    
    if(any(xray_click_coordinates_df$spine_point == "c2_centroid")){
      
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
    
    
    final_coords_df%>%
      filter(!is.na(x)) 
    # mutate(y = abs(y - max(final_coords_df$y)))
  })
  
  output$centroid_coordinates_df <- renderTable({
    xray_centroid_coordinates_reactive_df()
  })
  
  output$xray_centroid_tibble_text <- renderText({
    # xray_centroid_coordinates_reactive_df()
    
    spine_point_labels <- glue_collapse(xray_centroid_coordinates_reactive_df()$spine_point, sep = "', '")
    
    x_values <- glue_collapse(xray_centroid_coordinates_reactive_df()$x, sep = ", ")
    y_values <- glue_collapse(xray_centroid_coordinates_reactive_df()$y, sep = ", ")
    
    glue("tibble(spine_point = c('{spine_point_labels}'), x = c({x_values}), y = c({y_values}))")
  })
  
  
  alignment_parameters_reactivevalues_list <- reactiveValues()
  
  observeEvent(list(input$xray_click,
                    spine_orientation()), {
                      
                      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
                      
                      if((any(names(alignment_parameters_list) == "pelvic_incidence") == FALSE) & any(xray_centroid_coordinates_reactive_df()$spine_point == "s1_center")){
                        fem_head_center <- c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y)
                        
                        s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
                                       (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)
                        
                        #   ### COMPUTE PT ###
                        fem_head_to_s1_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
                                                                                                 point_2 = s1_center) ## hypotenuse
                        
                        fem_head_to_s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
                                                                                                   point_2 = c(s1_center[[1]], fem_head_center[[2]])) ## opposite
                        
                        pt_orientation_modifier <- case_when(
                          spine_orientation() == "left" & fem_head_center[[1]] < s1_center[[1]] ~ 1,
                          spine_orientation() == "left" & fem_head_center[[1]] > s1_center[[1]] ~ -1,
                          spine_orientation() == "right" & fem_head_center[[1]] > s1_center[[1]] ~ 1,
                          spine_orientation() == "right" & fem_head_center[[1]] < s1_center[[1]] ~ -1
                        )
                        
                        alignment_parameters_reactivevalues_list$pelvic_tilt <- asin(fem_head_to_s1_x_length/fem_head_to_s1_length)*180/pi*pt_orientation_modifier
                        
                        ### COMPUTE SS ###
                        s1_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y),
                                                                                     point_2 = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y))
                        
                        s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior$x,
                                                                                                   click_coord_reactive_list$coords$s1_posterior_superior$y),
                                                                                       point_2 = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y))
                        
                        alignment_parameters_reactivevalues_list$sacral_slope <- acos(s1_x_length/s1_length)*180/pi
                        
                        ### COMPUTE PI ###
                        alignment_parameters_reactivevalues_list$pelvic_incidence <- alignment_parameters_reactivevalues_list$pelvic_tilt + alignment_parameters_reactivevalues_list$sacral_slope
                        
                      }
                      
                      ## COMPUTE ALL VPAs ##
                      if(any(xray_centroid_coordinates_reactive_df()$spine_point == "c2_centroid")){
                        
                        vpa_df <- xray_centroid_coordinates_reactive_df() %>%
                          filter(spine_point != "s1_center") %>%
                          mutate(vpa = map2(.x = x, .y = y, .f = ~ jh_compute_vpa_from_xray_data_function(fem_head_center = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y),
                                                                                                          vertebral_centroid = c(.x, .y),
                                                                                                          spine_facing = spine_orientation(),
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
                      
                    }
  )
  
  
  # observeEvent(input$xray_orientation, {
  #   
  #   alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
  #   
  #   if((any(names(alignment_parameters_list) == "pelvic_incidence") == TRUE)){
  #     fem_head_center <- click_coord_reactive_list$coords$fem_head_center
  #     
  #     s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior[[1]] + click_coord_reactive_list$coords$s1_posterior_superior[[1]])/2,
  #                    (click_coord_reactive_list$coords$s1_anterior_superior[[2]] + click_coord_reactive_list$coords$s1_posterior_superior[[2]])/2)
  #     
  #     ### COMPUTE PT ###
  #     fem_head_to_s1_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
  #                                                                              point_2 = s1_center) ## hypotenuse
  #     
  #     fem_head_to_s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = fem_head_center,
  #                                                                                point_2 = c(s1_center[[1]], fem_head_center[[2]])) ## opposite
  #     
  #     pt_orientation_modifier <- case_when(
  #       spine_orientation() == "left" & fem_head_center[[1]] < s1_center[[1]] ~ 1,
  #       spine_orientation() == "left" & fem_head_center[[1]] > s1_center[[1]] ~ -1,
  #       spine_orientation() == "right" & fem_head_center[[1]] > s1_center[[1]] ~ 1,
  #       spine_orientation() == "right" & fem_head_center[[1]] < s1_center[[1]] ~ -1
  #     )
  #     
  #     alignment_parameters_reactivevalues_list$pelvic_tilt <- asin(fem_head_to_s1_x_length/fem_head_to_s1_length)*180/pi*pt_orientation_modifier
  #     
  #     ### COMPUTE SS ###
  #     s1_length <- jh_calculate_distance_between_2_points_function(point_1 = click_coord_reactive_list$coords$s1_anterior_superior,
  #                                                                  point_2 = click_coord_reactive_list$coords$s1_posterior_superior)
  #     
  #     s1_x_length <- jh_calculate_distance_between_2_points_function(point_1 = c(click_coord_reactive_list$coords$s1_anterior_superior[[1]],
  #                                                                                click_coord_reactive_list$coords$s1_posterior_superior[[2]]),
  #                                                                    point_2 = click_coord_reactive_list$coords$s1_posterior_superior)
  #     
  #     alignment_parameters_reactivevalues_list$sacral_slope <- acos(s1_x_length/s1_length)*180/pi
  #     
  #     ### COMPUTE PI ###
  #     alignment_parameters_reactivevalues_list$pelvic_incidence <- alignment_parameters_reactivevalues_list$pelvic_tilt + alignment_parameters_reactivevalues_list$sacral_slope
  #   }
  #   
  #   ## COMPUTE ALL VPAs ##
  #   if(any(xray_centroid_coordinates_reactive_df()$spine_point == "c2_centroid")){
  #     
  #     vpa_df <- xray_centroid_coordinates_reactive_df() %>%
  #       filter(spine_point != "s1_center") %>%
  #       mutate(vpa = map2(.x = x, .y = y, .f = ~ jh_compute_vpa_from_xray_data_function(fem_head_center = click_coord_reactive_list$coords$fem_head_center,
  #                                                                                       vertebral_centroid = c(.x, .y),
  #                                                                                       spine_facing = spine_orientation(),
  #                                                                                       pelvic_tilt = alignment_parameters_reactivevalues_list$pelvic_tilt
  #       )
  #       )
  #       ) %>%
  #       unnest() %>%
  #       mutate(vpa_label = str_replace_all(spine_point, "_centroid", "pa")) %>%
  #       select(vpa_label, vpa)
  #     
  #     vpa_list <- as.list(vpa_df$vpa)
  #     names(vpa_list) <- vpa_df$vpa_label
  #     
  #     for (name in names(vpa_list)) {
  #       alignment_parameters_reactivevalues_list[[name]] <- vpa_list[[name]]
  #     }
  #   }
  # })
  
  # output$alignment_parameters_df_text <- renderText({
  #   
  #   glue_collapse(reactiveValuesToList(alignment_parameters_reactivevalues_list), sep = " \n")
  #   
  # })
  
  output$alignment_parameters_df <- renderTable({
    
    enframe(reactiveValuesToList(alignment_parameters_reactivevalues_list)) %>%
      mutate(name = str_replace_all(name, "pelvic_tilt", "PT")) %>%
      mutate(name = str_replace_all(name, "pelvic_incidence", "PI")) %>%
      mutate(name = str_replace_all(name, "sacral_slope", "SS")) %>%
      mutate(name = str_to_upper(name))
    
  })
  
  
  xray_reactive_plot <- reactive({
    
    
    # req(xray)  # Ensure there's a cached image
    
    
    if(xray_instructions_reactiveval() == "Completed"){
      xray <- image_read(path = input$image$datapath)
      
      xray_height <- image_info(xray)$height
      xray_width <- image_info(xray)$width
      
      xlim_left <-0.5 - (xray_width/xray_height)/2
      xlim_right <-0.5 + (xray_width/xray_height)/2
      
      spine_colors_df <- xray_centroid_coordinates_reactive_df() %>%
        mutate(spine_point = str_remove_all(spine_point, "_centroid|_center")) %>%
        filter(spine_point != "s1") %>%
        mutate(spine_color = case_when(
          str_detect(spine_point, "c") ~ "lightblue",
          str_detect(spine_point, "t") ~ "lightgreen",
          str_detect(spine_point, "l") ~ "darkblue"
        )
        )
      
      alignment_parameters_list <- reactiveValuesToList(alignment_parameters_reactivevalues_list)
      
      s1_center <- c((click_coord_reactive_list$coords$s1_anterior_superior$x + click_coord_reactive_list$coords$s1_posterior_superior$x)/2,
                     (click_coord_reactive_list$coords$s1_anterior_superior$y + click_coord_reactive_list$coords$s1_posterior_superior$y)/2)
      
      pi_df <- calculate_pelvic_incidence_line_coordinates(fem_head_center = c(click_coord_reactive_list$coords$fem_head_center$x, click_coord_reactive_list$coords$fem_head_center$y),
                                                           s1_anterior = c(click_coord_reactive_list$coords$s1_anterior_superior$x, click_coord_reactive_list$coords$s1_anterior_superior$y),
                                                           s1_posterior = c(click_coord_reactive_list$coords$s1_posterior_superior$x, click_coord_reactive_list$coords$s1_posterior_superior$y),
                                                           spine_facing = spine_orientation(),
                                                           pelvic_tilt = alignment_parameters_list$pelvic_tilt,
                                                           pelvic_incidence_value = alignment_parameters_list$pelvic_incidence
      )
      
      spine_coordinates_df <- tibble(spine_point = "s1_center",
                                     x = s1_center[[1]],
                                     y = s1_center[[2]]) %>%
        union_all(click_coordinates_df_reactive()) %>%
        filter(spine_point %in% c("fem_head_center", "s1_anterior_superior", "s1_posterior_superior") == FALSE)
      
      l1pa_df <- tibble(x = c(s1_center[[1]],
                              click_coord_reactive_list$coords$fem_head_center$x,
                              click_coord_reactive_list$coords$l1_centroid$x),
                        y = c(s1_center[[2]],
                              click_coord_reactive_list$coords$fem_head_center$y,
                              click_coord_reactive_list$coords$l1_centroid$y))
      
      t4pa_df <- tibble(x = c(s1_center[[1]],
                              click_coord_reactive_list$coords$fem_head_center$x,
                              click_coord_reactive_list$coords$t4_centroid$x),
                        y = c(s1_center[[2]],
                              click_coord_reactive_list$coords$fem_head_center$y,
                              click_coord_reactive_list$coords$t4_centroid$y))
      
      # xray_plot <- ggdraw()
      
      xray_plot <-  ggdraw(xlim = c(0, xray_width), ylim = c(0, xray_height)) +
        draw_image(scale = xray_height,
                   xray,
                   x = 0,
                   halign = 0, 
                   valign = 0,
                   y = 0,
                   width = 1,
                   height = 1,
                   clip = FALSE
        ) +
        geom_path(data = xray_centroid_coordinates_reactive_df(),
                  aes(x = x, y = y), color = "lightblue", size = 1) +
        geom_point(data = spine_colors_df,
                   aes(x = x, y = y, color = spine_color, fill = spine_color),size = 2) +
        scale_fill_identity() +
        scale_color_identity()+
        geom_path(data = pi_df,
                  aes(x = x, y = y), color = "darkgreen", size = 1.5)+
        geom_path(data = l1pa_df,
                  aes(x = x, y = y),
                  color = "darkblue", size = 1)+
        geom_path(data = t4pa_df,
                  aes(x = x, y = y),
                  color = "purple", size = 1)+ 
        # coord_fixed(xlim = c(xlim_left, xlim_right),
        #             ylim = c(0, 1))
        coord_fixed(xlim = c(0, xray_width), ylim = c(0, xray_height))
      
      
      # xray_plot <- xray_plot +
      #   draw_image(
      #     xray,
      #     x = 0,
      #     y = 0,
      #     width = 1,
      #     height = 1
      # )+
      # geom_path(data = xray_centroid_coordinates_reactive_df(),
      #           aes(x = x, y = y), color = "lightblue", size = 1) +
      # geom_point(data = spine_colors_df,
      #            aes(x = x, y = y, color = spine_color, fill = spine_color),size = 2) +
      # scale_fill_identity() +
      # scale_color_identity()+
      # geom_path(data = pi_df,
      #           aes(x = x, y = y), color = "darkgreen", size = 1.5)+
      # geom_path(data = l1pa_df,
      #           aes(x = x, y = y),
      #           color = "darkblue", size = 1)+
      # geom_path(data = t4pa_df,
      #           aes(x = x, y = y),
      #           color = "purple", size = 1)+ 
      # coord_fixed(xlim = c(xlim_left, xlim_right),
      #                                                    ylim = c(0, 1))
      
      
      xray_plot
      
    }
    
    
    
    
  })
  
  output$xray_plot <- renderPlot({
    xray_reactive_plot()
  }
  )
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
