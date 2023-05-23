## ATTEMPT WITH CLEANED CODE - FUNCTION FOR VERT BODIES
library(colourpicker)
library(shiny)
library(reactlog)
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



source("jh_functions.R", local = TRUE)

source("function_segment_angles_separated.R", local = TRUE)

source("jh_spine_build_NEW_function.R", local = TRUE)

source("jh_prescribing_alignment_functions.R", local = TRUE)

source("spinal_regional_alignment_analysis_by_vpa.R", local = TRUE)



all_possible_lumbar_segments_angles_with_lpa_df <- read_csv("all_possible_lumbar_segment_angles_for_lpa.csv")
# 
# getwd()
# setw

reactlog_enable()

# Define UI for application that draws a histogram
ui <- navbarPage(title = "Spine Plotting/Planning",
                 tabPanel("Spine Plotting",
                          # Application title
                          # titlePanel("Plotting Alignment"),
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
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
                              # conditionalPanel(condition = "input.plot_lumbar_distribution_using == 'L4-S1 Lordosis'", 
                              #                  sliderInput(
                              #                      "l1_s1_lordosis",
                              #                      "Spinal Lordosis:",
                              #                      min = 0,
                              #                      max = 90,
                              #                      value = 55
                              #                  ),
                              #                  sliderInput(
                              #                      "l4_s1_lordosis",
                              #                      "L4-S1 Lordosis",
                              #                      min = 0,
                              #                      max = 60,
                              #                      value = 40
                              #                  )
                              # ),
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
                              # prettySwitch("pt_input",
                              #             "Input PT (rather than predict)",
                              #             value = FALSE), 
                              # switchInput("pt_input",
                              #             "Input PT (rather than predict)",
                              #             labelWidth = "130px",  
                              #             value = FALSE), 
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
                            mainPanel(
                              column(
                                width = 8,
                                plotOutput("spine_base", height = 700),
                                span(textOutput("messages_ll"), style="color:red"),
                                downloadButton(outputId = "download_figure", label = "Download the Figure"),
                                downloadButton(outputId = "download_plain_figure", label = "Download Plain Spine Figure")
                              ),
                              column(
                                width = 4,
                                h5(strong("Plot Graphics")),
                                fixedRow(
                                  column(width = 5, 
                                         dropdownButton(
                                           circle = TRUE,
                                           icon = icon("gear"),
                                           h5(strong("Plot Settings")),
                                           colourpicker::colourInput(
                                             inputId = "plot_background_color",
                                             label = "Choose Plot Background Color",
                                             value = "white",
                                             allowTransparent = TRUE
                                           ),
                                           switchInput("spine_coloring",
                                                       "Change Spine Color with Values",
                                                       labelWidth = "130px", 
                                                       value = FALSE),
                                           colourpicker::colourInput(inputId = "c2pa_line_color", label = "Choose C2 Pelvic Angle Line Color", "darkgreen"),
                                           colourpicker::colourInput(inputId = "c2_tilt_line_color", label = "Choose C2 Tilt Line Color", "#F090D8"),
                                           colourpicker::colourInput(inputId = "t1pa_line_color", label = "Choose T1PA Line Color", value = "grey55"),
                                           colourpicker::colourInput(inputId = "t9pa_line_color", label = "Choose T9 Pelvic Angle Line Color", "#CC79A7"),
                                           colourpicker::colourInput(inputId = "t4pa_line_color", label = "Choose T4 Pelvic Angle Line Color", "purple"),
                                           colourpicker::colourInput(inputId = "l1_pelvic_angle_line_color", label = "Choose L1 Pelvic Angle Line Color", "blue"),
                                           colourpicker::colourInput(inputId = "t1_c2_ha_line_color", label = "Choose T1-C2-Hip Angle Line Color", "orange"),
                                           colourpicker::colourInput(inputId = "t9_c2_ha_line_color", label = "Choose T9-C2-Hip Angle Line Color", "orange"),
                                           colourpicker::colourInput(inputId = "tk_line_color", label = "Choose TK Line Color", "blue"),
                                           colourpicker::colourInput(inputId = "pi_line_color", label = "Choose PT Line Color", "darkred"),
                                           colourpicker::colourInput(inputId = "pt_line_color", label = "Choose PT Line Color", "red"),
                                           colourpicker::colourInput(inputId = "ss_line_color", label = "Choose SS Line Color", "blue"),
                                           br(),
                                           switchInput("lines_posterior",
                                                       "Show Lordosis Lines Posteriorly",
                                                       labelWidth = "130px", value = TRUE),
                                           colourpicker::colourInput(inputId = "l1s1_line_color", label = "Choose L1-S1 Color", "blue"),
                                           sliderInput(
                                             "l1l4_angle_line_length",
                                             "L1-L4 Angle Display Length",
                                             min = 0,
                                             max = 30,
                                             value = 10
                                           ),
                                           colourpicker::colourInput(inputId = "l1l4_line_color", label = "Choose L1-L4 Line Color", "blue"),
                                           br(),
                                           sliderInput(
                                             "l4s1_angle_line_length",
                                             "L4-S1 Angle Display Length",
                                             min = 0,
                                             max = 30,
                                             value = 10
                                           ),
                                           colourpicker::colourInput(inputId = "l4s1_line_color", label = "Choose L4-S1 Line Color", "red"),
                                           sliderInput(
                                             "posterior_lordosis_line_lengths",
                                             "Posterior Lordosis Line Lengths",
                                             min = 0,
                                             max = 70,
                                             value = 30
                                           )
                                         )
                                  ), 
                                  # column(width = 6, 
                                  #        switchInput("face_right",
                                  #                    "Face Right",
                                  #                    value = TRUE)
                                  # )
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
                                            labelWidth = "130px"),
                                h5(strong("Predict Postop PT:")),
                                dropdownButton(
                                  circle = TRUE,
                                  label = "Predict Postop PT", 
                                  icon = icon("calculator"),
                                  numericInput(inputId = "predict_pt_preop_pt", label = "Preop PT", value = 25),
                                  numericInput(inputId = "predict_pt_preop_c2pa", label = "Preop C2PA", value = 20),
                                  numericInput(inputId = "predict_pt_postop_c2pa", label = "Postop C2PA", value = 11),
                                  textOutput(outputId = "predicted_pt")
                                )
                              )
                            )
                          )
                 ), 
                 ###################################### SURGICAL PLANNING TAB #####################################
                 ###################################### SURGICAL PLANNING TAB #####################################
                 ###################################### SURGICAL PLANNING TAB #####################################
                 ###################################### SURGICAL PLANNING TAB #####################################
                 ###################################### SURGICAL PLANNING TAB #####################################
                 ###################################### SURGICAL PLANNING TAB #####################################
                 # pi_preop <- 43
                 # pt_preop <- 37
                 # lpa_preop <- 9
                 # l1s1_preop <- 17
                 # t4pa_preop <- 29
                 # t4_t12_preop <- 37
                 # cl_preop <- 5
                 # rigid_levels <- c("l3_l4", "l4_l5")
                 tabPanel("Preoperative Sagittal Alignment Planning", 
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              h3(strong("Pre-operative Alignment:")),
                              sliderInput(
                                "preop_pelvic_incidence",
                                "Pelvic Incidence:",
                                min = 25,
                                max = 90,
                                value = 57
                              ),
                              sliderInput(
                                "preop_pt",
                                "Preop Pelvic Tilt:",
                                min = -15,
                                max = 60,
                                value = 23
                              ),
                              # sliderInput(
                              #     "preop_c2pa",
                              #     "Preop C2 Pelvic Angle:",
                              #     min = -10,
                              #     max = 80,
                              #     value = 28
                              # ),
                              sliderInput(
                                "preop_l1pa",
                                "Preop L1 Pelvic Angle:",
                                min = -15,
                                max = 50,
                                value = 19
                              ),
                              sliderInput(
                                "preop_l1_s1",
                                "Preop L1-S1 Lordosis:",
                                min = -40,
                                max = 100,
                                value = 42
                              ),
                              sliderInput(
                                "preop_t4pa",
                                "Preop T4 Pelvic Angle:",
                                min = -15,
                                max = 70,
                                value = 16
                              ),
                              sliderInput(
                                "preop_t4_t12",
                                "Preop T4-T12 Kyphosis:",
                                min = -40,
                                max = 100,
                                value = 28
                              ),
                              sliderInput(
                                "preop_c2_c7",
                                "Preop C2-C7 Lordosis:",
                                min = -40,
                                max = 100,
                                value = 5
                              ),
                              # awesomeCheckboxGroup(
                              #     inputId = "rigid_lumbar_levels",
                              #     label = "Select any Rigid Lumbar Levels", 
                              #     choices = c("T10-T11", "T11-T12", "T12-L1", "L1-L2", "L2-L3", "L3-L4", "L4-L5", "L5-S1"),
                              #     status = "danger"
                              # ),
                              # conditionalPanel(condition = "$.inArray('L1-L2', input.rigid_lumbar_levels) > -1",
                              #                  sliderInput(inputId = "l1_l2_fixed_angle", label = "Select the amount of lordosis at L1-L2 (negative = kyphosis)", 
                              #                              min = -20, max = 50, value = 5, step = 1)),
                              # conditionalPanel(condition = "$.inArray('L2-L3', input.rigid_lumbar_levels) > -1",
                              #                  sliderInput(inputId = "l2_l3_fixed_angle", label = "Select the amount of lordosis at L2-L3 (negative = kyphosis)", 
                              #                              min = -20, max = 50, value = 5, step = 1)),
                              # conditionalPanel(condition = "$.inArray('L3-L4', input.rigid_lumbar_levels) > -1",
                              #                  sliderInput(inputId = "l3_l4_fixed_angle", label = "Select the amount of lordosis at L3-L4 (negative = kyphosis)", 
                              #                              min = -20, max = 50, value = 5, step = 1)),
                              # conditionalPanel(condition = "$.inArray('L4-L5', input.rigid_lumbar_levels) > -1",
                              #                  sliderInput(inputId = "l4_l5_fixed_angle", label = "Select the amount of lordosis at L4-L5 (negative = kyphosis)", 
                              #                              min = -20, max = 50, value = 5, step = 1)),
                              # conditionalPanel(condition = "$.inArray('L5-S1', input.rigid_lumbar_levels) > -1",
                              #                  sliderInput(inputId = "l5_s1_fixed_angle", label = "Select the amount of lordosis at L5-S1 (negative = kyphosis)", 
                              #                              min = -20, max = 50, value = 5, step = 1))
                            ),
                            mainPanel(
                              column(width = 6, 
                                     h3("Preop Lumbar Alignment:"),
                                     plotOutput(outputId = "preop_spine_alignment", height = 650),
                                     awesomeCheckboxGroup(
                                       inputId = "rigid_lumbar_levels",
                                       label = "Select any Rigid Lumbar Levels", 
                                       # choices = c("T10-T11", "T11-T12", "T12-L1", "L1-L2", "L2-L3", "L3-L4", "L4-L5", "L5-S1"),
                                       choices = c("L5-S1", "L4-L5", "L3-L4", "L2-L3", "L1-L2", "T12-L1", "T11-T12", "T10-T11"), 
                                       inline = TRUE,
                                       status = "danger"
                                     )
                              ),
                              column(width = 6, 
                                     actionBttn(
                                       inputId = "compute_plan",
                                       label = "Compute Plan",
                                       style = "unite", 
                                       color = "danger"
                                     ),
                                     h3("Prescribed Lumbar Alignment:"),
                                     # tableOutput(outputId = "prescribed_lumbar_segment_levels_df"),
                                     plotOutput(outputId = "spine_plan", height = 800)
                              )
                              
                              # h3("Possible Options"),
                              # tableOutput(outputId = "close_lumbar_segment_levels_df")
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
  ### this is just the function at the bottom right. 
  output$predicted_pt <- renderText({
    predicting_pt_function <- function(postop_c2pa = 21.298135,
                                       preop_c2pa = 28.33507,
                                       preop_pt = 26.092608) {1.4817532+0.77386313*postop_c2pa-0.31861799*preop_c2pa+0.49868069*preop_pt }
    
    predicted_pt <- predicting_pt_function(postop_c2pa = input$predict_pt_postop_c2pa, preop_c2pa = input$predict_pt_preop_c2pa, preop_pt = input$predict_pt_preop_pt)
    
    paste("Predicted PT = ", round(predicted_pt, 1), sep = "")
    
  })
  
  
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
  
  
  ######################################## BUILD SPINE FUNCTION ########################################
  ######################################## BUILD SPINE FUNCTION  ########################################
  
  ### first compute relevant pelvic angles and then determine predicted PT. MUST DO THIS FIRST. You first set PT = 0 so that pelvic angles can be measured, then predict PT, then build the real plot
  
  ## DEFINE PT PREDICTION FUNCTION
  pt_by_c2pa_pi_function <- function(rad_6w_c2pa = 19.706756,rad_6w_s1pi = 55.207959) {
    -0.023418739+0.83835713*rad_6w_c2pa+0.065802858*rad_6w_s1pi
  }
  
  # jh_compute_pelvic_angles_and_pt_function <- function(pelvic_incidence_start = 50,
  #                                                      segment_angle_list_start = segment_angle_list,
  #                                                      # l1pa_start = 10,
  #                                                      cervical_lordosis_start = 20
  # ){
  #     initial_spine_build_simulation_list <- build_full_spine_function_new(pelv_inc_value = pelvic_incidence_start,
  #                                                                          segment_angle_list = segment_angle_list_start, 
  #                                                                          # planned_l1_pelvic_angle = l1pa_start, 
  #                                                                          cervical_lordosis = cervical_lordosis_start, 
  #                                                                          pt_value = 0, spine_faces = "right")
  #     
  #     c2pa_value <- initial_spine_build_simulation_list$spine_list$c2pa_value
  #     
  #     pt_predicted <- round(-0.023418739+0.83835713*c2pa_value+0.065802858*pelvic_incidence_start)
  #     # pt_predicted <- round(1.531932+0.77877765*c2pa_value+0.067338772*pelvic_incidence_start, 0)
  #     
  #     t1pa_value <- initial_spine_build_simulation_list$spine_list$t1_pelvic_angle
  #     
  #     #T4PA
  #     t4pa_value <- initial_spine_build_simulation_list$spine_list$t4pa_value
  #     
  #     return(list(c2pa_value = c2pa_value,
  #                 predicted_pt = pt_predicted,
  #                 t1pa_value = t1pa_value,
  #                 t4pa_value = t4pa_value, 
  #                 l1pa_value = initial_spine_build_simulation_list$spine_list$l1pa_value))
  #     
  # }
  
  c2pa_pt_predicted_list_reactive <- reactive({
    jh_compute_pelvic_angles_and_pt_function(pelvic_incidence_start = input$pelvic_incidence,
                                             segment_angle_list_start = segment_angle_list_reactive(), 
                                             # l1pa_start = input$l1_pelvic_angle,
                                             cervical_lordosis_start = input$cervical_lordosis)
  })
  
  #################### NEW ATTEMPT
  
  build_spine_reactive_function <- function(){
    reactive({
      
      pelv_inc_value <- input$pelvic_incidence
      cervical_lordosis <- input$cervical_lordosis
      spinal_kyphosis <- input$spinal_kyphosis
      
      segment_angle_list <- segment_angle_list_reactive()
      
      if(input$pt_input == "Input PT"){
        pt_value <- input$pt_value_input
      }else{
        pt_value <- c2pa_pt_predicted_list_reactive()$predicted_pt
      }
      ss_value <- pelv_inc_value - pt_value
      
      spine_build_list <- build_full_spine_function_new(pelv_inc_value = pelv_inc_value,
                                                        segment_angle_list = segment_angle_list,  ### CURRENTLY THE SEGMENT ANGLE LIST DOESNT INCLUDE CERVICAL
                                                        # planned_l1_pelvic_angle = lpa_test, 
                                                        cervical_lordosis = cervical_lordosis, 
                                                        pt_value = pt_value,
                                                        spine_faces = "right", posterior_line_length = input$posterior_lordosis_line_lengths)
      
      
      l1_l4_lordosis <- segment_angle_list$l1_segment_angle + segment_angle_list$l2_segment_angle + segment_angle_list$l3_segment_angle
      l4_s1_lordosis <- segment_angle_list$l4_segment_angle + segment_angle_list$l5_segment_angle 
      l1_s1_lordosis <- l1_l4_lordosis + l4_s1_lordosis
      
      l1_s1 = l4_s1_lordosis + segment_angle_list$l1_segment_angle + segment_angle_list$l2_segment_angle + segment_angle_list$l3_segment_angle
      
      l4s1_angle_line_length <- input$l4s1_angle_line_length
      l1l4_angle_line_length <- input$l1l4_angle_line_length
      
      ### OFFSETS AND COLORS
      ### #### ####
      t1pa_value <- c2pa_pt_predicted_list_reactive()$t1pa_value
      
      l1_s1_lordosis_offset <- abs(l1_s1_lordosis - (33+ pelv_inc_value*0.6))
      
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
      lines_list$c2pa_line_sf <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$c2pa_line_sf,
                                         color = input$c2pa_line_color, 
                                         fill = input$c2pa_line_color, 
                                         size = line_size, 
                                         lineend="round", 
                                         linejoin="round")
    }
    
    if(input$c2_tilt_line_show == TRUE){
      lines_list$c2_tilt_line_sf <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$c2_tilt_line_sf, 
                                            color = input$c2_tilt_line_color, 
                                            fill = input$c2_tilt_line_color, 
                                            size = line_size,
                                            lineend="round", 
                                            linejoin="round",
                                            linetype = "dashed")
    }
    
    if(input$t1pa_line_show == TRUE){
      lines_list$t1pa_line_sf <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$t1pa_line_sf,
                                         color = input$t1pa_line_color, 
                                         fill = input$t1pa_line_color, 
                                         size = line_size,lineend="round", linejoin="round")
    }
    if(input$t9pa_line_show == TRUE){
      lines_list$t9pa_line_sf <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$t9pa_line_sf, 
                                         color = input$t9pa_line_color, 
                                         fill = input$t9pa_line_color, 
                                         size = line_size,lineend="round", linejoin="round")
    }
    if(input$t4pa_line_show == TRUE){
      lines_list$t4pa_line_sf <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$t4pa_line_sf, 
                                         color = input$t4pa_line_color, 
                                         fill = input$t4pa_line_color, 
                                         size = line_size,lineend="round", linejoin="round")
    }
    if(input$l1pa_line_show == TRUE){
      lines_list$l1pa_line <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$l1pa_line_sf, 
                                      color = input$l1_pelvic_angle_line_color, 
                                      fill = input$l1_pelvic_angle_line_color, 
                                      size = line_size,lineend="round", linejoin="round")
    }
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
    if(input$tk_line_show == TRUE){
      lines_list$t12_line <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$t12_line_sf,
                                     color = input$tk_line_color, 
                                     fill = input$tk_line_color, 
                                     size = line_size,lineend="round", linejoin="round")
    }
    if(input$l1s1_line_show == TRUE){
      lines_list$l1_s1_line <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$l1s1_line_sf_1, 
                                       color = input$l1s1_line_color, 
                                       fill = input$l1s1_line_color, 
                                       size = line_size,lineend="round", linejoin="round")
      lines_list$l1_s1_line2 <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$l1s1_line_sf_2, 
                                        color = input$l1s1_line_color, 
                                        fill = input$l1s1_line_color, 
                                        size = line_size,lineend="round", linejoin="round")
    }
    
    
    if(input$l4s1_line_show == TRUE){
      lines_list$l1_s1_line <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$l4s1_line_sf_1, 
                                       color = input$l1s1_line_color, 
                                       fill = input$l1s1_line_color, 
                                       size = line_size,lineend="round", linejoin="round")
      lines_list$l1_s1_line2 <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$l4s1_line_sf_2, 
                                        color = input$l4s1_line_color, 
                                        fill = input$l4s1_line_color, 
                                        size = line_size,
                                        lineend="round", 
                                        linejoin="round")
    }
    if(input$ss_line_show == TRUE){
      lines_list$ss_line <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$ss_line_sf,
                                    color = input$ss_line_color, 
                                    fill = input$ss_line_color, 
                                    size = line_size,lineend="round", linejoin="round")
    }
    if(input$pt_line_show == TRUE){
      lines_list$pt_line <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$pt_line_sf,
                                    color = input$pt_line_color, 
                                    fill = input$pt_line_color, 
                                    size = line_size,lineend="round", linejoin="round")
    }
    if(input$pi_line_show == TRUE){
      lines_list$pt_line <- geom_sf(data = reactive_spine()$spine_build_list$spine_list$pi_line_sf,
                                    color = input$pi_line_color, 
                                    fill = input$pi_line_color, 
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
      labs(title = "The Cone of Economy") +
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
  
  output$messages_ll <- renderText({
    ll_in_out_range_list <- reactive_spine()$ll_in_out_range_list
    
    ll_message <- case_when(
      between(ll_in_out_range_list$ll_diff_from_norm, 0, 17) ~ " ",
      ll_in_out_range_list$ll_diff_from_norm > 17 ~ "The Spinal Lordosis is outside the normal range for this Pelvic Incidence.")
    
    print(ll_message)
    
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
  
  preop_spine_figure_reactive <- reactive({
    
    segment_angles_preop <- segment_angle_function_using_lpa_tpa(l1_pelvic_angle_input = input$preop_l1pa, 
                                                                 pelvic_incidence_input = input$preop_pelvic_incidence,
                                                                 l1_s1_lordosis_input = input$preop_l1_s1,
                                                                 pt_input = input$preop_pt, 
                                                                 t4_t12_input = input$preop_t4_t12,
                                                                 t4_pelvic_angle_input = input$preop_t4pa)
    
    # seg_angles_preop
    
    
    spine_preop_list <- build_full_spine_function_new(pelv_inc_value = input$preop_pelvic_incidence,
                                                      segment_angle_list = segment_angles_preop, 
                                                      cervical_lordosis = input$preop_c2_c7, 
                                                      pt_value = input$preop_pt,
                                                      spine_faces = "right")
    
    measured_pelvic_angles_preop_list <- jh_compute_pelvic_angles_and_pt_function(pelvic_incidence_start = input$preop_pelvic_incidence,
                                                                                  segment_angle_list_start = segment_angles_preop, 
                                                                                  cervical_lordosis_start = input$preop_c2_c7)
    
    regional_analysis_list <- id_focal_deformity_function(pelvic_incidence = input$preop_pelvic_incidence,
                                                          l4_pelvic_angle = measured_pelvic_angles_preop_list$l4pa_value, 
                                                          l1_pelvic_angle = measured_pelvic_angles_preop_list$l1pa_value,
                                                          t9_pelvic_angle = measured_pelvic_angles_preop_list$t9pa_value, 
                                                          t4_pelvic_angle = measured_pelvic_angles_preop_list$t4pa_value)
    
    spine_geoms_df <- spine_preop_list$spine_df %>%
      select(object, geom)  %>%
      # mutate(fill_color = case_when(
      #     str_starts(string = object, "l") ~ l1_s1_lordosis_color,
      #     str_starts(string = object, "t") ~ spinal_kyphosis_color,
      #     str_starts(string = object, "c") ~ "grey90",
      #     str_starts(string = object, "head") ~ "grey90",
      #     str_starts(string = object, "fem") ~ sacral_slope_color,
      #     str_starts(string = object, "sac") ~ sacral_slope_color
      # )) %>%
      mutate(alpha = case_when(
        object == "head_geom" ~ 0.8, 
        str_detect(object, "c1") ~ 0.8, 
        TRUE ~ 1
      ))
    
    lines_list <- list()
    lines_list$t4pa_line_sf <- geom_sf(data = spine_preop_list$spine_list$t4pa_line_sf, 
                                       color = input$t4pa_line_color, 
                                       fill = input$t4pa_line_color, 
                                       size = 1,lineend="round", linejoin="round")
    lines_list$l1pa_line <- geom_sf(data = spine_preop_list$spine_list$l1pa_line_sf, 
                                    color = input$l1_pelvic_angle_line_color, 
                                    fill = input$l1_pelvic_angle_line_color, 
                                    size = 1,lineend="round", linejoin="round")
    
    
    # return_list$upper_thoracic_analyzed <- results_list$t4pa
    # 
    # return_list$tl_junction_analyzed <- results_list$t9pa
    # 
    # return_list$upper_lumbar_analyzed <- results_list$l1pa
    # 
    # return_list$lower_lumbar_analyzed <- results_list$l4pa
    
    ggplot() +
      # cone_economy_list_reactive() +
      geom_sf(data = spine_geoms_df, 
              color = "black", 
              aes(geometry = geom, 
                  alpha = alpha,
                  fill = "grey90")) +
      draw_text(text = regional_analysis_list$upper_thoracic_analyzed, x = -64, y = 65, size = 11, hjust = 0) + 
      draw_text(text = regional_analysis_list$tl_junction_analyzed, x = -64, y = 45, size = 11, hjust = 0) +
      draw_text(text = regional_analysis_list$upper_lumbar_analyzed, x = -64, y = 30, size = 11, hjust = 0) +
      draw_text(text = regional_analysis_list$lower_lumbar_analyzed, x = -64, y = 18, size = 11, hjust = 0) +
      draw_text(text = paste("Recommended UIV Region:", regional_analysis_list$recommended_uiv, "Spine"), x = -55, y = -2, size = 16, fontface = "bold", hjust = 0) +
      # geom_text(label = paste(regional_analysis_list$regional_analysis), aes(x = -35, y = 0), hjust = 0) +
      lines_list +
      # xlim(-40, 40) +
      xlim(-65, 40) +
      ylim(-6, 110) +
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
      scale_fill_identity() +
      scale_alpha_identity()
    
    
  })
  
  output$preop_spine_alignment <- renderPlot({
    preop_spine_figure_reactive()
  })
  
  
  
  
  #################### NEW VERSION SIMPLIFIED
  
  spine_plan_plot2 <- eventReactive(input$compute_plan, {
    
    starting_segment_angles <- segment_angle_function_using_lpa_tpa(l1_pelvic_angle_input = input$preop_l1pa, 
                                                                    pelvic_incidence_input = input$preop_pelvic_incidence,
                                                                    l1_s1_lordosis_input = input$preop_l1_s1,
                                                                    pt_input = input$preop_pt, 
                                                                    t4_t12_input = input$preop_t4_t12,
                                                                    t4_pelvic_angle_input = input$preop_t4pa)
    
    measured_pelvic_angles_preop_list <- jh_compute_pelvic_angles_and_pt_function(pelvic_incidence_start = input$preop_pelvic_incidence,
                                                                                  segment_angle_list_start = starting_segment_angles,
                                                                                  cervical_lordosis_start = input$preop_c2_c7)
    
    regional_analysis_list <- id_focal_deformity_function(pelvic_incidence = input$preop_pelvic_incidence,
                                                          l4_pelvic_angle = measured_pelvic_angles_preop_list$l4pa_value, 
                                                          l1_pelvic_angle = measured_pelvic_angles_preop_list$l1pa_value,
                                                          t9_pelvic_angle = measured_pelvic_angles_preop_list$t9pa_value, 
                                                          t4_pelvic_angle = measured_pelvic_angles_preop_list$t4pa_value)
    
    uiv_for_rod <- case_when(
      regional_analysis_list$recommended_uiv == "Upper Thoracic" ~ 11,
      regional_analysis_list$recommended_uiv == "Lower Thoracic" ~ 18, 
      regional_analysis_list$recommended_uiv == "Upper Lumbar" ~ 21,
      regional_analysis_list$recommended_uiv == "Lower Lumbar" ~ 23
    )
    
    uiv_level <- case_when(
      regional_analysis_list$recommended_uiv == "Upper Thoracic" ~ "T4",
      regional_analysis_list$recommended_uiv == "Lower Thoracic" ~ "T11",
      regional_analysis_list$recommended_uiv == "Upper Lumbar" ~ "L2",
      regional_analysis_list$recommended_uiv == "Lower Lumbar" ~ "L4"
    )
    
    
    
    # seg_angles_preop
    if(any(input$rigid_lumbar_levels == "L5-S1")){
      l5_sa <- starting_segment_angles$l5_segment_angle
    }else{
      l5_sa <-99
    }
    if(any(input$rigid_lumbar_levels == "L4-L5")){
      l4_sa <- starting_segment_angles$l4_segment_angle
    }else{
      l4_sa <-99
    }
    if(any(input$rigid_lumbar_levels == "L3-L4")){
      l3_sa <- starting_segment_angles$l3_segment_angle
    }else{
      l3_sa <-99
    }
    if(any(input$rigid_lumbar_levels == "L2-L3")){
      l2_sa <- starting_segment_angles$l2_segment_angle
    }else{
      l2_sa <-99
    }
    if(any(input$rigid_lumbar_levels == "L1-L2")){
      l1_sa <- starting_segment_angles$l1_segment_angle
    }else{
      l1_sa <-99
    }
    if(any(input$rigid_lumbar_levels == "T12-L1")){
      t12_sa <- starting_segment_angles$t12_segment_angle
    }else{
      t12_sa <-99
    }
    if(any(input$rigid_lumbar_levels == "T11-T12")){
      t11_sa <- starting_segment_angles$t11_segment_angle
    }else{
      t11_sa <-99
    }
    if(any(input$rigid_lumbar_levels == "T10-T11")){
      t10_sa <- starting_segment_angles$t10_segment_angle
    }else{
      t10_sa <-99
    }
    
    
    segment_angles_plan <-   segment_angle_function_targets(pelvic_incidence_input = input$preop_pelvic_incidence,
                                                            fixed_l5sa_input = l5_sa, 
                                                            fixed_l4sa_input = l4_sa, 
                                                            fixed_l3sa_input = l3_sa,
                                                            fixed_l2sa_input = l2_sa, 
                                                            fixed_l1sa_input = l1_sa, 
                                                            fixed_t12sa_input = t12_sa,
                                                            fixed_t11sa_input = t11_sa,
                                                            fixed_t10sa_input = t10_sa)
    
    starting_segment_angles_plan <- segment_angles_plan
    
    measured_pelvic_angles_predicted_pt_list <- jh_compute_pelvic_angles_and_pt_function(pelvic_incidence_start = input$preop_pelvic_incidence,
                                                                                         segment_angle_list_start = segment_angles_plan, 
                                                                                         cervical_lordosis_start = input$preop_c2_c7)
    
    
    #  Returns:
    # list(c2pa_value = c2pa_value,
    #      predicted_pt = pt_predicted,
    #      t1pa_value = t1pa_value,
    #      t4pa_value = t4pa_value))
    
    # if(abs(measured_pelvic_angles_predicted_pt_list$t4pa_value - initial_spine_build_simulation_list$spine_list$l1pa_value)>4){
    #     
    # }
    
    rigid_levels <- input$rigid_lumbar_levels
    
    l5_s1_mobile <- if_else(any(rigid_levels == "L5-S1"), "no", "yes")
    l4_l5_mobile <- if_else(any(rigid_levels == "L4-L5"), "no", "yes")
    l3_l4_mobile <- if_else(any(rigid_levels == "L3-L4"), "no", "yes")
    l2_l3_mobile <- if_else(any(rigid_levels == "L2-L3"), "no", "yes")
    l1_l2_mobile <- if_else(any(rigid_levels == "L1-L2"), "no", "yes")
    t12_l1_mobile <- if_else(any(rigid_levels == "T12-L1"), "no", "yes")
    t11_t12_mobile <- if_else(any(rigid_levels == "T11-T12"), "no", "yes")
    t10_t11_mobile <- if_else(any(rigid_levels == "T10-T11"), "no", "yes")
    
    target_l1pa <- input$preop_pelvic_incidence*0.5-19
    
    lpa_deviation <- abs(target_l1pa - measured_pelvic_angles_predicted_pt_list$l1pa_value) 
    pso_list <- list()
    
    pso_attempted_list <- list()
    pso_attempted_list$l5 <- FALSE
    pso_attempted_list$l4 <- FALSE
    pso_attempted_list$l3 <- FALSE
    pso_attempted_list$l2 <- FALSE
    pso_attempted_list$l1 <- FALSE
    
    while (lpa_deviation > 3) {
      if(l5_s1_mobile == "yes" & segment_angles_plan$l5_segment_angle < 30){
        segment_angles_plan$l5_segment_angle <- segment_angles_plan$l5_segment_angle + 3
        
      }else if(l4_l5_mobile == "yes" & segment_angles_plan$l4_segment_angle < 30){
        segment_angles_plan$l4_segment_angle <- segment_angles_plan$l4_segment_angle + 3
        
      }else if(l3_l4_mobile == "yes" & segment_angles_plan$l3_segment_angle < 30){
        segment_angles_plan$l3_segment_angle <- segment_angles_plan$l3_segment_angle + 3
      }else{ ### PERFORM PSO
        
        ideal_segment_angles_list <- segment_angle_function_targets(pelvic_incidence_input = input$preop_pelvic_incidence)
        segment_angle_targets_df <- enframe(ideal_segment_angles_list) %>%
          unnest() %>%
          rename(target = value)
        if(l1_l2_mobile == "yes"){
          segment_angles_plan$l1_segment_angle <- ideal_segment_angles_list$l1_segment_angle
        }
        if(l2_l3_mobile == "yes"){
          segment_angles_plan$l2_segment_angle <- ideal_segment_angles_list$l2_segment_angle
        }
        if(l3_l4_mobile == "yes"){
          segment_angles_plan$l3_segment_angle <- ideal_segment_angles_list$l3_segment_angle
        }
        if(l4_l5_mobile == "yes"){
          segment_angles_plan$l4_segment_angle <- ideal_segment_angles_list$l4_segment_angle
        }
        if(l5_s1_mobile == "yes"){
          segment_angles_plan$l5_segment_angle <- ideal_segment_angles_list$l5_segment_angle
        }
        
        pso_level_df <- enframe(segment_angles_plan) %>%
          unnest(value) %>%
          filter(str_starts(name, "l")) %>%
          left_join(segment_angle_targets_df) %>%
          mutate(level = fct_inorder(str_remove_all(name, "_segment_angle"))) %>%
          mutate(level = fct_rev(level)) %>%
          select(-name) %>%
          arrange(level) %>%
          mutate(mobile_segment = c(l5_s1_mobile, l4_l5_mobile, l3_l4_mobile, l2_l3_mobile, l1_l2_mobile)) %>%
          mutate(pso_attempted = c(pso_attempted_list$l5, pso_attempted_list$l4, pso_attempted_list$l3, pso_attempted_list$l2, pso_attempted_list$l1)) %>%
          filter(mobile_segment == "no") %>%
          # filter(pso_attempted == FALSE) %>%
          mutate(target_offset = target - value) %>%
          arrange(desc(target_offset)) %>%
          head(2) %>%
          arrange(level)
        
        if(diff(pso_level_df$target_offset) < 3 & sum(pso_level_df$target_offset) < 20){
          pso_level_df <- pso_level_df %>%
            head(1)
        }else{
          pso_level_df <- pso_level_df %>%
            tail(1)
        }
        
        pso_level_segment_angle <-  jh_pso_degree_calculator(pso_level = pso_level_df$level, 
                                                             current_lpa = measured_pelvic_angles_predicted_pt_list$l1pa_value, 
                                                             desired_lpa = target_l1pa)
        pso_list <- list(pso_level_df$level)
        names(pso_list) <- pso_level_df$level 
        
        if(pso_level_df$level == "l1"){
          if(pso_attempted_list$l1 == TRUE){
            segment_angles_plan$l1_segment_angle <- segment_angles_plan$l1_segment_angle +5
          }else{
            segment_angles_plan$l1_segment_angle <- pso_level_segment_angle
            pso_attempted_list$l1 <- TRUE 
          }
        }
        if(pso_level_df$level == "l2"){
          if(pso_attempted_list$l2 == TRUE){
            segment_angles_plan$l2_segment_angle <- segment_angles_plan$l2_segment_angle +5
          }else{
            segment_angles_plan$l2_segment_angle <- pso_level_segment_angle
            pso_attempted_list$l2 <- TRUE 
          }
        }
        if(pso_level_df$level == "l3"){
          if(pso_attempted_list$l3 == TRUE){
            segment_angles_plan$l3_segment_angle <- segment_angles_plan$l3_segment_angle +5
          }else{
            segment_angles_plan$l3_segment_angle <- pso_level_segment_angle
            pso_attempted_list$l3 <- TRUE 
          }
        }
        if(pso_level_df$level == "l4"){
          if(pso_attempted_list$l4 == TRUE){
            segment_angles_plan$l4_segment_angle <- segment_angles_plan$l4_segment_angle +5
          }else{
            segment_angles_plan$l4_segment_angle <- pso_level_segment_angle
            pso_attempted_list$l4 <- TRUE 
          }
        }
        if(pso_level_df$level == "l5"){
          if(pso_attempted_list$l5 == TRUE){
            segment_angles_plan$l5_segment_angle <- segment_angles_plan$l5_segment_angle +5
          }else{
            segment_angles_plan$l5_segment_angle <- pso_level_segment_angle
            pso_attempted_list$l5 <- TRUE 
          }
        }
        
      }
      
      measured_pelvic_angles_predicted_pt_list <- jh_compute_pelvic_angles_and_pt_function(pelvic_incidence_start = input$preop_pelvic_incidence,
                                                                                           segment_angle_list_start = segment_angles_plan, 
                                                                                           cervical_lordosis_start = input$preop_c2_c7, 
                                                                                           pso_input = as_vector(pso_list))
      
      lpa_deviation <- abs(target_l1pa - measured_pelvic_angles_predicted_pt_list$l1pa_value) 
    }
    
    
    t4pa_l1pa_mismatch <- (measured_pelvic_angles_predicted_pt_list$t4pa_value - measured_pelvic_angles_predicted_pt_list$l1pa_value)
    
    while (t4pa_l1pa_mismatch > 1) {
      # if(l5_s1_mobile == "yes" & segment_angles_plan$l5_segment_angle < 31){
      #     segment_angles_plan$l5_segment_angle <- segment_angles_plan$l5_segment_angle + 3
      # }else if(l4_l5_mobile == "yes" & segment_angles_plan$l4_segment_angle < 31){
      #     segment_angles_plan$l4_segment_angle <- segment_angles_plan$l4_segment_angle + 3
      # }else if(l3_l4_mobile == "yes" & segment_angles_plan$l3_segment_angle < 31){
      #     segment_angles_plan$l3_segment_angle <- segment_angles_plan$l3_segment_angle + 3
      # }else
      # if(l1_l2_mobile == "yes" & segment_angles_plan$l1_segment_angle < 20){
      #       segment_angles_plan$l1_segment_angle <- segment_angles_plan$l1_segment_angle + 3
      #   }else 
      if(t12_l1_mobile == "yes" & segment_angles_plan$t12_segment_angle < 10){
        segment_angles_plan$t12_segment_angle <- segment_angles_plan$t12_segment_angle + 3
      }else if(t11_t12_mobile == "yes" & segment_angles_plan$t11_segment_angle < 5){
        segment_angles_plan$t11_segment_angle <- segment_angles_plan$t11_segment_angle + 3
      }else if(t10_t11_mobile == "yes" & segment_angles_plan$t10_segment_angle < 3){
        segment_angles_plan$t10_segment_angle <- segment_angles_plan$t10_segment_angle + 2
      }else if(segment_angles_plan$t9_segment_angle < -5 ){
        segment_angles_plan$t9_segment_angle <- segment_angles_plan$t9_segment_angle + 2
      }else if(segment_angles_plan$t8_segment_angle < -5 ){
        segment_angles_plan$t8_segment_angle <- segment_angles_plan$t8_segment_angle + 3
      }else if(segment_angles_plan$t7_segment_angle < -5 ){
        segment_angles_plan$t7_segment_angle <- segment_angles_plan$t7_segment_angle + 3
      }else if(segment_angles_plan$t6_segment_angle < -5 ){
        segment_angles_plan$t6_segment_angle <- segment_angles_plan$t6_segment_angle + 3
      }else if(segment_angles_plan$t5_segment_angle < -5 ){
        segment_angles_plan$t5_segment_angle <- segment_angles_plan$t5_segment_angle + 3
      }else if(segment_angles_plan$t4_segment_angle < -5 ){
        segment_angles_plan$t4_segment_angle <- segment_angles_plan$t4_segment_angle + 3
      }else{
        segment_angles_plan$t10_segment_angle <- segment_angles_plan$t10_segment_angle + 1
        segment_angles_plan$t9_segment_angle <- segment_angles_plan$t9_segment_angle + 1
        segment_angles_plan$t8_segment_angle <- segment_angles_plan$t8_segment_angle + 1
        segment_angles_plan$t7_segment_angle <- segment_angles_plan$t7_segment_angle + 1
        segment_angles_plan$t6_segment_angle <- segment_angles_plan$t6_segment_angle + 1
        segment_angles_plan$t5_segment_angle <- segment_angles_plan$t5_segment_angle + 1
      }
      
      measured_pelvic_angles_predicted_pt_list <- jh_compute_pelvic_angles_and_pt_function(pelvic_incidence_start = input$preop_pelvic_incidence,
                                                                                           segment_angle_list_start = segment_angles_plan,
                                                                                           cervical_lordosis_start = 20,
                                                                                           pso_input = as_vector(pso_list))
      
      lpa_deviation <- abs(target_l1pa - measured_pelvic_angles_predicted_pt_list$l1pa_value)
      
      t4pa_l1pa_mismatch <- (measured_pelvic_angles_predicted_pt_list$t4pa_value - measured_pelvic_angles_predicted_pt_list$l1pa_value)
      
    }
    
    predict_pt_fun <- function(postop_c2pa = 20,
                               preop_c2pa = 25,
                               preop_pt = 25) {1.4817532+0.77386313*postop_c2pa - 0.31861799*preop_c2pa + 0.49868069*preop_pt }
    
    predicted_pt <- predict_pt_fun(postop_c2pa = measured_pelvic_angles_predicted_pt_list$c2pa_value, 
                                   preop_c2pa = measured_pelvic_angles_preop_list$c2pa_value, 
                                   preop_pt = input$preop_pt )
    
    
    spine_prescribed_list <- build_full_spine_function_new(pelv_inc_value = input$preop_pelvic_incidence,
                                                           segment_angle_list = segment_angles_plan, 
                                                           cervical_lordosis = input$preop_c2_c7, 
                                                           pt_value = predicted_pt,
                                                           spine_faces = "right", 
                                                           pso_levels = as_vector(pso_list))
    
    spine_geoms_df <- spine_prescribed_list$spine_df %>%
      select(object, geom)  %>%
      # mutate(fill_color = case_when(
      #     str_starts(string = object, "l") ~ l1_s1_lordosis_color,
      #     str_starts(string = object, "t") ~ spinal_kyphosis_color,
      #     str_starts(string = object, "c") ~ "grey90",
      #     str_starts(string = object, "head") ~ "grey90",
      #     str_starts(string = object, "fem") ~ sacral_slope_color,
      #     str_starts(string = object, "sac") ~ sacral_slope_color
      # )) %>%
      mutate(alpha = case_when(
        object == "head_geom" ~ 0.8, 
        str_detect(object, "c1") ~ 0.8, 
        TRUE ~ 1
      ))
    
    lines_list <- list()
    
    lines_list$t4pa_line_sf <- geom_sf(data = spine_prescribed_list$spine_list$t4pa_line_sf, 
                                       color = input$t4pa_line_color, 
                                       fill = input$t4pa_line_color, 
                                       size = 1,
                                       lineend="round", 
                                       linejoin="round")
    lines_list$l1pa_line <- geom_sf(data = spine_prescribed_list$spine_list$l1pa_line_sf,
                                    color = input$l1_pelvic_angle_line_color,
                                    fill = input$l1_pelvic_angle_line_color,
                                    size = 1,
                                    lineend="round",
                                    linejoin="round")
    lines_list$c2pa_line <- geom_sf(data = spine_prescribed_list$spine_list$c2pa_line_sf,
                                    color = input$c2pa_line_color,
                                    fill = input$c2pa_line_color,
                                    size = 1,
                                    lineend="round",
                                    linejoin="round")
    
    preop_t4pa_l1pa_mismatch = input$preop_t4pa - input$preop_l1pa
    
    # uiv_for_rod <- case_when(
    #   preop_t4pa_l1pa_mismatch > 6.5 ~ 11,
    #   preop_t4pa_l1pa_mismatch < 6.5 ~ 17
    # )
    # uiv_level <- case_when(
    #   preop_t4pa_l1pa_mismatch > 6.5 ~ "T4",
    #   preop_t4pa_l1pa_mismatch < 6.5 ~ "T10"
    # )
    
    # uiv_for_rod <- case_when(
    #     input$planned_uiv == "T2" ~ 9,
    #     input$planned_uiv == "T3" ~ 10,
    #     input$planned_uiv == "T4" ~ 11,
    #     input$planned_uiv == "T5" ~ 12,
    #     input$planned_uiv == "T6" ~ 13,
    #     input$planned_uiv == "T7" ~ 14,
    #     input$planned_uiv == "T8" ~ 15,
    #     input$planned_uiv == "T9" ~ 16,
    #     input$planned_uiv == "T10" ~ 17,
    #     input$planned_uiv == "T11" ~ 18,
    #     input$planned_uiv == "T12" ~ 19
    # )
    
    instrumented_vertebral_body_list <- spine_prescribed_list$vertebral_body_list[uiv_for_rod:length(spine_prescribed_list$vertebral_body_list)]
    
    sp_matrix_for_rod <- do.call(rbind, map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ rbind(instrumented_vertebral_body_list[[.x]]$sp)))
    
    rod_sf <- st_buffer(x = st_linestring(sp_matrix_for_rod), dist = 0.5, nQuadSegs = 5, joinStyle = "ROUND", endCapStyle = "ROUND")
    
    screw_list <- st_multipolygon(compact(map(.x = seq(1:(length(instrumented_vertebral_body_list))), .f = ~ instrumented_vertebral_body_list[[.x]]$screw_sf)))
    
    measurements_list <- list()
    measurements_list$"C2 Tilt" <- round(spine_prescribed_list$spine_list$c2_tilt_value)
    measurements_list$"C2PA" <- round(spine_prescribed_list$spine_list$c2pa_value, 0)
    measurements_list$"T1PA" <- round(spine_prescribed_list$spine_list$t1pa_value, 0)
    measurements_list$"T4PA" <- round(spine_prescribed_list$spine_list$t4pa_value, 0)
    measurements_list$"L1PA" <- round(spine_prescribed_list$spine_list$l1pa_value, 0)
    measurements_list$"LL" <-  round(spine_prescribed_list$spine_list$lumbar_lordosis, 0)
    measurements_list$"TK" <- round(spine_prescribed_list$spine_list$thoracic_kyphosis, 0)
    measurements_list$"PT" <- round(spine_prescribed_list$spine_list$pelvic_tilt, 0)
    measurements_list$"L1PA Deviation" <- round(lpa_deviation, 0)
    measurements_list$"T4PA-L1PA Mismatch" <- round(t4pa_l1pa_mismatch, 0)
    
    measurements_df <- enframe(measurements_list) %>%
      unnest() %>%
      mutate(x = if_else(name == "PI", 10, -26)) %>%
      mutate(y = c(seq(from = 43, by = -4, length = length(measurements_list)-2), 7, 3)) %>%
      mutate(label = paste(name, value, sep = " = ")) 
    
    segment_angles_list_for_plot <- list()
    segment_angles_list_for_plot$"L1-L2" <- as.character(round(segment_angles_plan$l1_segment_angle, 0))
    segment_angles_list_for_plot$"L2-L3" <- as.character(round(segment_angles_plan$l2_segment_angle, 0))
    segment_angles_list_for_plot$"L3-L4" <- as.character(round(segment_angles_plan$l3_segment_angle, 0))
    segment_angles_list_for_plot$"L4-L5" <- as.character(round(segment_angles_plan$l4_segment_angle, 0))
    segment_angles_list_for_plot$"L5-S1" <- as.character(round(segment_angles_plan$l5_segment_angle, 0))
    
    if(length(pso_list)>0){
      pso_label <- paste("PSO at", str_to_upper(pso_list[[1]]))
    }else{
      pso_label <- " "
    }
    
    
    segment_angles_df <- enframe(segment_angles_list_for_plot) %>%
      unnest() %>%
      mutate(x = 22) %>%
      mutate(y = c(seq(from = 35, by = -4, length = length(segment_angles_list_for_plot)))) %>%
      mutate(label = paste(name, value, sep = " = ")) 
    
    ggplot() +
      geom_sf(data = spine_geoms_df,
              aes(geometry = geom,
                  alpha = alpha
              ),
              color = "black",
              fill = "grey90") +
      geom_sf(data = st_multipolygon(x = screw_list), fill = "grey55") +
      geom_sf(data = rod_sf, fill = "grey55") +
      geom_text(data = measurements_df, aes(label = label, x = x, y = y), size = 5) +
      geom_text(data = segment_angles_df, aes(label = label, x = x, y = y), size = 5) +
      geom_text(label = paste("UIV:", uiv_level), aes(x = -30, y = 60), size = 6, fontface = "bold") +
      geom_text(label = pso_label, aes(x = 22, y = 5), size = 6, fontface = "bold") +
      # geom_text(label = paste(regional_analysis_list$regional_analysis), aes(x = -30, y = 0)) +
      lines_list +
      xlim(-35, 35) +
      # ylim(-6, 110) +
      theme_void() +
      labs(title = "Planned Alignment") +
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
      # scale_fill_identity() +
      scale_alpha_identity()
    
    
  })
  
  output$spine_plan <- renderPlot({
    spine_plan_plot2()
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

