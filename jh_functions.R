library(tidyverse)
library(janitor)
library(rms)
library(Hmisc)
library(cowplot)
library(assertr)
library(lubridate)
library(glue)
library(tableone)

jh_fit_model_function <- function(outcome_text, model_type, predictor_text, cohort_df){
  
  model_formula <- as.formula(paste(outcome_text, " ~ ", predictor_text))
  if(model_type == "lrm"){
    model_fit <- lrm(model_formula, cohort_df, x = TRUE, y = TRUE)
  }
  if(model_type == "ols"){
    model_fit <- ols(model_formula, cohort_df, x = TRUE, y = TRUE)
  }
  model_fit  
}

jh_get_regression_tibble_function <- function(imputed_model){
  tab_1 <- gtsummary::tbl_regression(imputed_model) %>%
    add_significance_stars() %>%
    as_tibble() %>%
    clean_names() 
  
  tab_2 <- gtsummary::tbl_regression(imputed_model) %>%
    as_tibble() %>%
    clean_names() %>%
    rename(ci_95 = x95_percent_ci) %>%
    select(-beta)%>%
    mutate(p_value_numeric = as.double(str_remove_all(p_value, "<")))
  
  tab_1 %>%
    left_join(tab_2)
}

jh_make_table_1_function <- function(df_with_measures_to_include, stratify_by = "none", round_digitis_to = 1){
  
  if(stratify_by == "none"){
    print(tableone::CreateTableOne(data = df_with_measures_to_include, addOverall = TRUE), 
          catDigits = round_digitis_to,
          contDigits = round_digitis_to,
          pDigits = 2, 
          quote = TRUE, 
          nonnormal = TRUE)
  }else{
    print(tableone::CreateTableOne(data = df_with_measures_to_include, addOverall = TRUE, 
                                   strata =  paste(stratify_by)),
          catDigits = round_digitis_to,
          contDigits = round_digitis_to,
          pDigits = 2, 
          quote = TRUE, 
          nonnormal = TRUE)
  }
}


jh_spine_levels_factors_df <- tibble(level = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "l1", "l2", "l3", "l4", "l5")) %>%
  mutate(level_label = str_to_upper(level)) %>% 
  mutate(level_tilt = paste0(level, "_tilt")) %>%
  mutate(level_pelvic_angle = paste0(level, "_pelvic_angle")) %>%
  mutate(level_cobb_angle = paste0(level, "_s1")) %>%
  mutate(level_tilt_label = paste0(level_label, " Tilt")) %>%
  mutate(level_pelvic_angle_label = paste0(level_label, " Pelvic Angle")) %>%
  mutate(level_cobb_label = paste0(level_label, "-S1")) %>%
  select(level, level_tilt, level_pelvic_angle, level_cobb_angle, level_label, level_tilt_label, level_pelvic_angle_label, level_cobb_label) %>%
  mutate(across(everything(), fct_inorder)) %>%
  mutate(across(everything(), fct_rev))

### THEME FUNCTION
jh_theme_function <- function(theme_template = theme_minimal(), 
                              axis_text_size = 8,
                              axis_title_size = 10,
                              axis_ticks_length = 1,
                              plot_title_size = 12,
                              axis_line_size = 0.5, 
                              top_margin = 5,
                              bottom_margin = 5, 
                              left_margin = 5, 
                              right_margin = 5){
  my_theme <- theme_template +
    theme(strip.text = element_text(face = "bold", size = 10, hjust = 0.5), 
          strip.placement = "outside",
          axis.text = element_text(size = axis_text_size),
          axis.title = element_text(face = "bold", size = axis_title_size, hjust = 0.5),
          axis.line = element_line(colour = "black", size = axis_line_size),
          axis.ticks = element_line(rel(axis_ticks_length)),
          plot.subtitle = element_text(size = axis_title_size, hjust = 0.5),
          plot.caption = element_text(size = 8, hjust = 1),
          plot.title = element_text(face = "bold", size = plot_title_size, hjust = 0.5),
          plot.margin = margin(top_margin,right_margin,bottom_margin,left_margin)
    )
  #   scale_color_manual(values = color_scale) +
  #   scale_x_continuous(expand = expansion(add = c(expand_x_left_by,expand_x_right_by))
  #                      ) +
  #   scale_y_continuous(expand = expansion(add = c(expand_y_bottom_by,expand_y_top_by))
  #                      ) 
  # 
  # if(convert_continuous_x_to_discrete == TRUE){
  #   
  # my_theme <- my_theme +
  #     scale_x_continuous(breaks = 1:nrow(labels_levels_df),
  #                    labels = labels_levels_df$measurement, expand = expansion(add = c(0.5,1))
  #                    ) 
  # }
  
  return(my_theme)
}

## SYMBOLS LIST
# https://cran.r-project.org/web/packages/utf8/vignettes/utf8.html
jh_symbols_list <- list(superscript_2 = "²",
                        degree_symbol = "º",
                        rho_label = sprintf('\u03c1'),
                        multiplication_symbol = "×", 
                        beta_symbol = "β", 
                        plus_minus_sign = "±")

# jh_symbols_list

# 0 1 2 3 4 5 6 7 8 9 a b c d e f
# 8                                
# 9                                
# a   ¡ ¢ £ ¤ ¥ ¦ § ¨ © ª « ¬   ® ¯
# b ° ± ² ³ ´ µ ¶ · ¸ ¹ º » ¼ ½ ¾ ¿
# c À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï
# d Ð Ñ Ò Ó Ô Õ Ö × Ø Ù Ú Û Ü Ý Þ ß
# e à á â ã ä å æ ç è é ê ë ì í î ï
# f ð ñ ò ó ô õ ö ÷ ø ù ú û ü ý þ ÿ

## COLOR PALETTES 

colorblind_palette <<- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
colorblind_palette_dark <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

color_palette_orange_red_9 <- RColorBrewer::brewer.pal(n = 9, name = "OrRd")
color_palette_paired_8 <- RColorBrewer::brewer.pal(12, name = "Paired")
color_palette_reds_9 <<- RColorBrewer::brewer.pal(n = 9, name = "Reds")
color_palette_blues_9 <<- RColorBrewer::brewer.pal(n = 9, name = "Blues")
color_palette_greens_9 <- RColorBrewer::brewer.pal(n = 9, name = "PuBuGn")
color_palette_dark_8 <- RColorBrewer::brewer.pal(n =8,name = "Dark2")

jh_colors_list <- list(
  # view_all_colors = colors_plot,
  colors_reds_4 = color_palette_reds_9[9:6],
  colors_reds_5 = color_palette_reds_9[9:5],
  colors_reds_6 = color_palette_reds_9[9:4],
  colors_blues_4 = color_palette_blues_9[9:6],
  colors_blues_5 = color_palette_blues_9[9:5],
  colors_blues_6 = color_palette_blues_9[9:4],
  colors_greens_4 = color_palette_greens_9[9:6],
  colors_greens_5 = color_palette_greens_9[9:5],
  colors_greens_6 = color_palette_greens_9[9:4], 
  colorblind_palette_light = colorblind_palette,
  colorblind_palette_dark = colorblind_palette_dark
)

jh_colors_list$view_all_colors_plot <- tibble(jh_colors_list) %>%
  mutate(group = names(jh_colors_list)) %>%
  unnest() %>%
  group_by(group) %>%
  mutate(count = row_number()) %>%
  ungroup() %>%
  ggplot(aes(x = 1, y = count, color = jh_colors_list)) +
  geom_point(size = 3) +
  geom_text(aes(label = jh_colors_list, x = 0.45, y = count)) +
  xlim(0, 1.1) +
  facet_wrap(facets = "group", scales = "free") +
  jh_theme_function() +
  scale_color_identity()



jh_view_color_palette_function <- function(color_palette){
  tibble(y = c(1:length(color_palette))) %>%
    mutate(color = color_palette) %>%
    ggplot() + 
    geom_hline(aes(yintercept = y, color = color), size = 3) + 
    scale_color_identity()
}

jh_text_list <- list(text_8 = element_text(size = 8), 
                     text_8_bold = element_text(face = "bold", size = 8), 
                     text_8_bold_centered = element_text(face = "bold", size = 8, hjust = 0.5), 
                     text_8_centered = element_text(size = 8, hjust = 0.5), 
                     text_9 = element_text(size = 9), 
                     text_9_bold = element_text(face = "bold", size = 9), 
                     text_9_bold_centered = element_text(face = "bold", size = 9, hjust = 0.5), 
                     text_9_centered = element_text(size = 9, hjust = 0.5), 
                     text_10 = element_text(size = 10), 
                     text_10_bold = element_text(face = "bold", size = 10), 
                     text_10_bold_centered = element_text(face = "bold", size = 10, hjust = 0.5), 
                     text_10_centered = element_text(size = 10, hjust = 0.5), 
                     text_11 = element_text(size = 11), 
                     text_11_bold = element_text(face = "bold", size = 11), 
                     text_11_bold_centered = element_text(face = "bold", size = 11, hjust = 0.5), 
                     text_11_centered = element_text(size = 11, hjust = 0.5), 
                     text_12 = element_text(size = 12), 
                     text_12_bold = element_text(face = "bold", size = 12), 
                     text_12_bold_centered = element_text(face = "bold", size = 12, hjust = 0.5), 
                     text_12_centered = element_text(size = 12, hjust = 0.5), 
                     text_13 = element_text(size = 13), 
                     text_13_bold = element_text(face = "bold", size = 13), 
                     text_13_bold_centered = element_text(face = "bold", size = 13, hjust = 0.5), 
                     text_13_centered = element_text(size = 13, hjust = 0.5), 
                     text_14 = element_text(size = 14), 
                     text_14_bold = element_text(face = "bold", size = 14), 
                     text_14_bold_centered = element_text(face = "bold", size = 14, hjust = 0.5), 
                     text_16 = element_text(size = 16), 
                     text_16_bold = element_text(face = "bold", size = 16), 
                     text_16_bold_centered = element_text(face = "bold", size = 16, hjust = 0.5) )


my_theme_for_arranged <<- theme_minimal() +
  theme(strip.text = jh_text_list$text_12_bold, strip.placement = "outside",
        axis.text = jh_text_list$text_10,
        axis.title = jh_text_list$text_12,
        plot.title = jh_text_list$text_14_bold_centered)

my_theme <<- theme_minimal() +
  theme(strip.text = jh_text_list$text_12_bold, strip.placement = "outside",
        axis.text = jh_text_list$text_12,
        axis.title = jh_text_list$text_14,
        plot.subtitle = jh_text_list$text_12_centered,
        plot.title = jh_text_list$text_16_bold_centered)

my_theme_xaxis_angled <<- theme_minimal() +
  theme(strip.text = jh_text_list$text_12_bold, strip.placement = "outside",
        axis.text = jh_text_list$text_12_bold,
        axis.text.x = element_text(face = "bold", size = 12, angle = 60),
        axis.title = jh_text_list$text_14_bold,
        plot.title = jh_text_list$text_16_bold_centered)



#FUNCTIONS

## ROUNDING FUNCTION
jh_round_function <- function(number_to_round,round_to_nearest){
  round_to_nearest*round(number_to_round/round_to_nearest)
}

## MODE FUNCTION
jh_modes_function <- function(d){
  i <- which(diff(sign(diff(d$y))) < 0) + 1
  data.frame(x = d$x[i], y = d$y[i])
}
## get R2 functions
jh_get_r2 <- function(model){
  r2 <- model$stats['R2']
}

jh_get_r2_statement <- function(model){
  r2 <- model$stats['R2']
  paste0("r² = ", round(r2,2))
}

jh_get_R2_statement <- function(model){
  r2 <- model$stats['R2']
  paste0("R² = ", round(r2,2))
}


## NOMOGRAM FUNCTION

jh_nomogram_function <- function(rms_model, predicted_variable_label, variable_order_vector_for_nomogram){
  predictor_vector <- rms_model$Design$name
  predictor_vector_labels <- rms_model$Design$label
  
  model_nomogram <- nomogram(rms_model)
  
  nomogram_df <- tibble(measure = character(), 
                        value = numeric(), 
                        points = numeric())
  
  for (i in 1:length(predictor_vector)) {
    add_df <- tibble(measure = predictor_vector_labels[i],
                     value = model_nomogram[[i]][[1]], 
                     points = model_nomogram[[i]]$points)
    
    nomogram_df <- nomogram_df %>%
      union_all(add_df)
    
  }
  
  nomogram_df <- nomogram_df %>%
    union_all(tibble(measure = "Points",
                     value = seq(0, 100, by = 10), 
                     points = seq(0, 100, by = 10)))
  
  nomo_scoring_df <- tibble(measure = predicted_variable_label,
                            value = model_nomogram$lp$x.real, 
                            points = model_nomogram$lp$x) %>%
    union_all(tibble(measure = "Total Points",
                     value = model_nomogram$total.points$x, 
                     points = model_nomogram$total.points$x))
  
  nomo_scoring_scaled_df <- nomo_scoring_df %>%
    mutate(points_raw = points) %>%
    mutate(points = points_raw *100/max(points_raw))
  
  variable_vector <- c("Points")
  
  variable_vector <- append(variable_vector, variable_order_vector_for_nomogram)
  
  variable_vector <- append(variable_vector, c("Total Points", predicted_variable_label))
  
  pre_nomo_plotting_df <- nomogram_df %>%
    union_all(nomo_scoring_scaled_df %>% select(-points_raw)) %>%
    mutate(measure = fct_inorder(measure)) %>%
    mutate(measure = fct_relevel(measure, variable_vector)) %>%
    mutate(measure = fct_rev(measure))
  
  nomo_plotting_df <- pre_nomo_plotting_df %>%
    select(measure) %>%
    distinct() %>%
    arrange(measure) %>%
    mutate(measure_number = row_number()) %>%
    left_join(pre_nomo_plotting_df) %>%
    mutate(measure = fct_relevel(measure, variable_vector)) 
  
  
  nomo_plotting_line_df <- nomo_plotting_df %>%
    filter(measure == predicted_variable_label) 
  
  
  nomo_plotting_labels <- nomo_plotting_df %>%
    select(measure) %>%
    distinct()
  
  ### make the plot
  
  nomogram_plot <- nomo_plotting_df %>%
    filter(measure != predicted_variable_label) %>%
    ggplot() + 
    geom_linerange(aes(xmin = 0, xmax = points, y = measure_number)) + 
    geom_linerange(data = nomo_plotting_line_df, aes(xmin = min(points), xmax = max(points), y = measure_number)) + 
    geom_linerange(aes(x = points, ymin = measure_number - 0.1, ymax = measure_number)) +
    geom_linerange(data = nomo_plotting_line_df, aes(x = points, ymin = measure_number - 0.1, ymax = measure_number)) +
    draw_text(text = nomo_plotting_df$value, y = nomo_plotting_df$measure_number - 0.15, x = nomo_plotting_df$points, size = 8, vjust = 1, check_overlap = TRUE) +
    scale_y_continuous(breaks = seq(min(as.numeric(nomo_plotting_labels$measure)), max(as.numeric(nomo_plotting_labels$measure)), by = 1), limits = c(0.5, length(variable_vector)), 
                       labels = nomo_plotting_labels$measure) +
    theme_void() + 
    theme(axis.text.y = element_text(size = 10, face = "bold", hjust = 1),
          plot.margin = margin(5, 5, 10, 5)) + 
    draw_line(x = c(2.5, 97.5), y = 2.3, linetype = "dashed")
  
  
  ## Plot with only Y labels 
  nomogram_plot_only_y_labels <- nomo_plotting_df %>%
    filter(measure != predicted_variable_label) %>%
    ggplot() + 
    geom_linerange(aes(xmin = 0, xmax = points, y = measure_number)) + 
    geom_linerange(data = nomo_plotting_line_df, aes(xmin = min(points), xmax = max(points), y = measure_number)) + 
    geom_linerange(aes(x = points, ymin = measure_number - 0.1, ymax = measure_number)) +  ## adds tick marks to the other lines
    geom_linerange(data = nomo_plotting_line_df, aes(x = points, ymin = measure_number - 0.1, ymax = measure_number)) +  ##  adds tick marks to the Total points Axis
    # draw_text(text = nomo_plotting_df$value, y = nomo_plotting_df$measure_number - 0.15, x = nomo_plotting_df$points, size = 8, vjust = 1, check_overlap = TRUE) +
    scale_y_continuous(breaks = seq(min(as.numeric(nomo_plotting_labels$measure)), max(as.numeric(nomo_plotting_labels$measure)), by = 1), limits = c(0.5, length(variable_vector)), 
                       labels = nomo_plotting_labels$measure) +
    theme_void() + 
    theme(axis.text.y = element_text(size = 10, face = "bold", hjust = 1),
          plot.margin = margin(5, 5, 10, 5))
  
  nomogram_plot_only_y_labels_no_main_axis_ticks <- nomo_plotting_df %>%
    filter(measure != predicted_variable_label) %>%
    ggplot() + 
    geom_linerange(aes(xmin = 0, xmax = points, y = measure_number)) + 
    geom_linerange(data = nomo_plotting_line_df, aes(xmin = min(points), xmax = max(points), y = measure_number)) + 
    geom_linerange(data = nomo_plotting_line_df, aes(x = points, ymin = measure_number - 0.1, ymax = measure_number)) +  ## adds tick marks to the Total points Axis
    scale_y_continuous(breaks = seq(min(as.numeric(nomo_plotting_labels$measure)), max(as.numeric(nomo_plotting_labels$measure)), by = 1), limits = c(0.5, length(variable_vector)), 
                       labels = nomo_plotting_labels$measure) +
    theme_void() + 
    theme(axis.text.y = element_text(size = 10, face = "bold", hjust = 1),
          plot.margin = margin(5, 5, 10, 5))
  
  
  nomogram_figure <- plot_grid(NULL, nomogram_plot, nrow = 2, rel_heights = c(0.1, 0.9)) +
    draw_label(label = glue("Nomogram for Estimating {predicted_variable_label}"), fontface = "bold.italic", size = 12, x = 0, y = 1, hjust = 0, vjust = 1) 
  
  
  full_nomogram_ggplot_text <- "  nomogram_plot <- nomo_plotting_df %>%
    filter(measure != predicted_variable_label) %>%
    ggplot() + 
    geom_linerange(aes(xmin = 0, xmax = points, y = measure_number)) + 
    geom_linerange(data = nomo_plotting_line_df, aes(xmin = min(points), xmax = max(points), y = measure_number)) + 
    geom_linerange(aes(x = points, ymin = measure_number - 0.1, ymax = measure_number)) +
    geom_linerange(data = nomo_plotting_line_df, aes(x = points, ymin = measure_number - 0.1, ymax = measure_number)) +
    draw_text(text = nomo_plotting_df$value, y = nomo_plotting_df$measure_number - 0.15, x = nomo_plotting_df$points, size = 8, vjust = 1, check_overlap = TRUE) +
    scale_y_continuous(breaks = seq(min(as.numeric(nomo_plotting_labels$measure)), max(as.numeric(nomo_plotting_labels$measure)), by = 1), limits = c(0.5, length(variable_vector)), 
                       labels = nomo_plotting_labels$measure) +
    theme_void() + 
    theme(axis.text.y = element_text(size = 10, face = 'bold', hjust = 1),
          plot.margin = margin(5, 5, 10, 5)) + 
    draw_line(x = c(2.5, 97.5), y = 2.3, linetype = 'dashed')"
  
  r2_statement <- glue("R² = {round(rms_model$stats[4], 2)}")
  
  return(list(
    nomogram_plot_only_y_labels_no_main_axis_ticks = nomogram_plot_only_y_labels_no_main_axis_ticks,
    nomogram_plot_only_y_labels = nomogram_plot_only_y_labels,
    nomogram_figure_with_title = nomogram_figure, 
    nomogram_plot_no_title = nomogram_plot,
    nomogram_plotting_df = nomo_plotting_df,
    nomogram_plotting_line_df = nomo_plotting_line_df, 
    nomogram_plotting_labels_df = nomo_plotting_labels,
    full_nomogram_ggplot_text = full_nomogram_ggplot_text,
    r2_statement = r2_statement
  ))
  
}


## GET MODEL EQUATION FUNCTION
jh_get_model_equation <- function(model, model_outcome_label = "Outcome", variable_labels_vector_in_order = c("variable_1"), round_coefficients_to = 1, round_intercept_to = 0){
  
  abs_value_intercept <- abs(round(model$coefficients[['Intercept']], round_intercept_to))
  
  right_hand_intercept_section <- if_else(round(model$coefficients[['Intercept']], round_intercept_to) < 0, 
                                          glue("- {abs_value_intercept}"), 
                                          glue("+ {abs_value_intercept}"))
  
  right_hand_df_no_intercept <- tibble(label = names(model$coefficients), value = model$coefficients)%>%
    mutate(label = str_to_lower(str_trim(label))) %>%
    filter(label != "intercept")
  
  if(length(names(model$coefficients)) == length(variable_labels_vector_in_order) + 1){
    
    right_hand_df <- tibble(label = variable_labels_vector_in_order, value = right_hand_df_no_intercept$value)%>%
      mutate(value = round(value, round_coefficients_to)) %>%
      mutate(row_count = row_number()) %>%
      mutate(sign = if_else(value < 0, "-", "+")) %>%
      mutate(sign = if_else(row_count == 1 & sign == "+", "", sign)) %>%
      mutate(variable_section = glue("{sign} {value}×{label} ")) %>%
      mutate(variable_section = as.character(variable_section))
    
    right_hand_equation <- str_squish(paste(glue_collapse(right_hand_df$variable_section), right_hand_intercept_section))
    
    full_equation <- str_replace_all(string = paste(model_outcome_label, "=", right_hand_equation), pattern = "- -", replacement = "+ ")
    
    
  }else{
    
    full_equation <- "Please enter labels in the correct order for the variables in the model."
    
  }
  
  
  return(full_equation)
  
}


## PARTIAL R2 PLOT
jh_partial_r2_plot_function <- function(model_with_labels){
  
  anova_plot <- plot(anova(model_with_labels), what = "proportion R2")
  
  anova_text <- anova(model_with_labels)
  
  p_values_df <- names(anova_text[,5]) %>%
    as_tibble() %>%
    mutate(result = anova_text[,5]) %>%
    mutate(p_value = if_else(result < 0.01, "<0.001", as.character(round(result,2)))) %>%
    filter(value != "ERROR") %>%
    filter(value != "TOTAL") %>%
    mutate(measure = fct_inorder(value)) %>%
    mutate(measure_label = str_to_title(str_replace_all(measure, pattern = "_", replacement = " "))) %>%
    select(measure, measure_label, p_value)
  
  proporital_r2_mod_df <- tibble("measure" = names(anova_plot), "values" = anova_plot) %>%
    mutate(measure = fct_inorder(measure)) %>%
    mutate(measure_label = str_to_title(str_replace_all(measure, pattern = "_", replacement = " "))) %>%
    left_join(p_values_df) %>%
    mutate(p_value = fct_inorder(p_value)) %>%
    mutate(measure_label = fct_inorder(measure_label))
  
  
  proportional_r2_plot <- proporital_r2_mod_df %>%
    ggplot() + 
    geom_point(aes(x = values, y = as.numeric(measure)), size = 3) +
    xlab(label = "Proportion of Overall R²") + 
    labs(title = "Variance Explained by Predictor") +
    build_theme_function(theme_template = theme_void(),
                         axis_text_size = 8,
                         axis_title_size = 10,
                         plot_title_size = 10,
                         axis_line_size = 0,
                         top_margin = 1,
                         bottom_margin = 10,
                         left_margin = 5,
                         right_margin = 5) +
    scale_y_continuous(breaks = 1:nrow(proporital_r2_mod_df), 
                       labels = proporital_r2_mod_df$measure_label, 
                       name = NULL, 
                       sec.axis = sec_axis(~.,breaks = 1:nrow(proporital_r2_mod_df), 
                                           labels = proporital_r2_mod_df$p_value, name = "P Value"), 
                       expand = expansion(add = c(0.5, 0.5))) +
    theme(
      panel.background = element_blank(),
      panel.grid.major.y = element_line(colour = "black", linetype = "dashed", size = 0.3),
      panel.border = element_rect(color = "black", size = 1.1, fill = NA),
      panel.grid.minor.y = element_line(colour = "grey", linetype = "solid", size = 0.2),
      axis.line = element_blank(),
      plot.title = element_text(size = 12, face = "bold.italic", margin = margin(b = 4)),
      axis.text.y = element_text(size = 8, face = "bold", hjust = 1, margin = margin(r = 4, l = 4)),
      axis.title.y.right = element_text(angle = 0, 
                                        vjust = 1.05, 
                                        margin = margin(l = -30), size = 8, face = "bold", lineheight = 0.9),
      axis.title.x = element_text(margin = margin(t = 4), face = "bold.italic"),
      axis.text.x = element_text(size = 8, face = "bold", margin = margin(t = 2)),
      plot.margin = margin(5,5,5,5)
    )  
  
  return(list(proportional_r2_ggplot = proportional_r2_plot, 
              proporital_r2_mod_df = proporital_r2_mod_df, 
              rms_anova_plot = anova_plot))
  
}


## GGTEXT Model Stats Table
jh_model_stats_summary_gg_table_function <- function(model_with_labels){
  fit_stats <- summary.lm(model_with_labels)
  
  predictor_stats_df <- fit_stats$coefficients %>%
    as_tibble(rownames = "term") %>%
    clean_names() %>%
    mutate(estimate = round(estimate, 2),
           std_error = round(std_error, 2), 
           t_value = round(t_value, 2), 
           pr_t = round(pr_t, 3)) %>%
    mutate(term = str_replace_all(string = term, pattern = "_", replacement = " ")) %>%
    mutate(term = str_to_title(term)) %>%
    select(term, estimate, std_error, p_value = pr_t) %>%
    mutate(p_value = if_else(p_value < 0.05, round(p_value, 3), round(p_value, 2))) %>%
    mutate(p_value = if_else(p_value < 0.001, "<0.001", as.character(p_value)))
  
  
  predictor_stats_table <- ggtexttable(x = predictor_stats_df %>% select(-term),
                                       rows = predictor_stats_df$term, 
                                       cols = c("β", "S.E.", "P Value"), 
                                       theme = ttheme("blank", 
                                                      base_size = 8, 
                                                      padding = unit(c(1.5,2), "mm"), 
                                                      rownames.style = rownames_style(hjust = 1, x = 0.97, size = 8, face = "plain")
                                       )
  ) %>%
    tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 1) %>%
    tab_add_hline(at.row = nrow(predictor_stats_df)+1, row.side = "bottom", linewidth = 1) %>%
    tab_add_vline(at.column = 1, from.row = 2, to.row = nrow(predictor_stats_df)+1, column.side = "right", linewidth = 0.25, linecolor = "grey") %>%
    tab_add_title(text = "Coefficients:", size = 10, face = "bold", padding = unit(2, "mm")) +
    theme(plot.margin = margin(5,5,5,5))
  
  return(list(predictor_stats_df = predictor_stats_df,
              stats_table = predictor_stats_table))
  
}




##### SPEARMAN RANK CORRELATION FUNCTION
jh_spearman_rank_correlation_plot_function <- function(df_with_variables, 
                                                       variable_to_correlate_to,
                                                       named_list_of_variables_and_labels, 
                                                       label_text_size = 9){
  rho_label <- sprintf('\u03c1')
  
  label_variable_df <- enframe(named_list_of_variables_and_labels) %>%
    unnest() %>%
    rename(label = name, measure = value)
  
  vector_of_variables <- df_with_variables %>%
    select(-variable_to_correlate_to) %>%
    names()
  
  equation <- paste(variable_to_correlate_to, glue_collapse(as.vector(named_list_of_variables_and_labels), sep = " + "), sep = " ~ ")
  
  spearman_pi_global <- spearman2(as.formula(equation),
                                  data = df_with_variables, p=2)
  
  
  global_spearman_iqr_skinny_df <- spearman_pi_global[,c(0, 6,5)] %>%
    as.data.frame() %>%
    as_tibble(rownames = "measure") %>%
    clean_names() %>%
    mutate(adjusted_rho2 = round(adjusted_rho2, 2), p = round(p, 2)) %>%
    mutate(p_label = if_else(p < 0.01, "P < 0.01", paste("P = ", p))) %>%
    mutate(p_asterisk = if_else(p < 0.01, "*", "")) %>%
    arrange((adjusted_rho2)) %>%
    left_join(label_variable_df) %>%
    # mutate(label = str_to_title(str_replace_all(string = measure, pattern = "_", replacement = " "))) %>%
    mutate(measure = fct_inorder(measure)) %>%
    mutate(label = fct_inorder(label))
  
  spearman_plot <- global_spearman_iqr_skinny_df %>%
    ggplot() +
    geom_point(aes(x = adjusted_rho2, y = as.numeric(label)), size = 2) +
    xlab(label = glue("Adjusted {rho_label}²")) +
    labs(title = glue("Correlation with {str_to_title(str_replace_all(string = variable_to_correlate_to, pattern = '_', replacement = ' ' ))}")) +
    ylab(NULL) +
    theme(panel.background = element_rect(colour = "black",size = 0.5, fill = NA),
          panel.grid.major.y = element_line(colour = "black", linetype = "dashed", size = 0.3),
          panel.grid.minor.y = element_line(colour = "grey", linetype = "solid", size = 0.2),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.ontop = FALSE,
          axis.text.x = text_10_bold,
          axis.text.y.left = element_text(size = label_text_size, face = "bold", hjust = 1),
          axis.text.y.right = element_text(size = label_text_size, hjust = 0),
          axis.title = text_10_bold_centered,
          plot.subtitle = text_8_centered,
          plot.caption = text_8,
          plot.title = element_text(size = 12, hjust = 0.5, face = "bold", vjust = 1),
          axis.line = element_line(colour = "black", size = 0.5),
          axis.line.x.top = element_line(colour = "black", size = 0.5),
          axis.line.y.right = element_line(colour = "black", size = 0.5),
          plot.margin = margin(1,2,1,1),
          axis.title.x = element_text(size = 10, face = "bold", hjust = 0.5, vjust = 2)
    ) +
    scale_y_continuous(breaks = c(1:length(global_spearman_iqr_skinny_df$label)), labels = global_spearman_iqr_skinny_df$label, sec.axis = dup_axis(labels = global_spearman_iqr_skinny_df$p_label))
  
  return(list(spearman_df = global_spearman_iqr_skinny_df, spearman_plot = spearman_plot))
}



############################### SURGIMAP 
jh_reformat_surgimap_export_function <- function(raw_df){
  
  ## filter the df to only have the measures of interest
  filtered_measures_long_df <- raw_df %>%
    filter(parameter == "PI" | 
             parameter == "PT" |
             parameter == "SS" |
             parameter == "SVA (C7S1)" |
             str_detect(string = parameter, pattern = "_pelvic_angle") | 
             str_detect(string = parameter, pattern = "_slope") | 
             str_detect(string = parameter, pattern = "_inferior_endplate_slope")) %>%
    mutate(parameter = if_else(parameter == "PI", "pelvic_incidence", parameter)) %>%
    mutate(parameter = if_else(parameter == "PT", "pelvic_tilt", parameter)) %>%
    mutate(parameter = if_else(parameter == "SS", "sacral_slope", parameter)) %>%
    mutate(parameter = if_else(parameter == "SVA (C7S1)", "sva", parameter)) %>%
    mutate(baseline = as.double(baseline))
  
  
  pelvic_parameters_wide_df <- filtered_measures_long_df %>%
    filter(parameter %in% c("pelvic_incidence", "sacral_slope", "pelvic_tilt", "sva")) %>%
    pivot_wider(names_from = "parameter", values_from = "baseline")
  
  
  slope_pelvic_angles_long_df <- filtered_measures_long_df %>%
    filter(parameter != "pelvic_incidence",
           parameter != "sacral_slope", 
           parameter != "pelvic_tilt", 
           parameter != "sva")
  
  
  prepared_df <- pelvic_parameters_wide_df %>%
    left_join(slope_pelvic_angles_long_df) %>%
    rename(measure = parameter, value = baseline)
  
  
  sagittal_cobb_pre_df <- prepared_df %>%
    filter(str_detect(measure, "_slope")) %>%
    filter(str_detect(measure, "inferior_endplate_slope") == FALSE) %>%
    mutate(sagittal_cobb_angle_label = str_replace_all(string = measure, 
                                                       pattern = "_slope",
                                                       replacement = "_s1")) %>%
    mutate(sagittal_cobb_angle_value = value - sacral_slope) %>%
    select(-measure, -value) %>%
    pivot_wider(names_from = sagittal_cobb_angle_label, values_from = sagittal_cobb_angle_value)
  
  sagittal_cobb_df <- order_columns_by_level_function(sagittal_cobb_pre_df)
  
  
  vertebral_tilt_pre_df <- prepared_df %>%
    filter(str_detect(measure, "_pelvic_angle")) %>%
    mutate(vertebral_tilt_label = str_replace_all(string = measure, 
                                                  pattern = "_pelvic_angle",
                                                  replacement = "_tilt")) %>%
    mutate(vertebral_tilt = value - pelvic_tilt) %>%
    select(-measure, -value) %>%
    pivot_wider(names_from = vertebral_tilt_label, values_from = vertebral_tilt)
  
  
  vertebral_tilt_df <- order_columns_by_level_function(vertebral_tilt_pre_df)
  
  pelvic_angles_pre_df <- prepared_df %>%
    filter(str_detect(measure, "pelvic_angle")) %>%
    pivot_wider(names_from = measure, values_from = value)
  
  pelvic_angles_df <- order_columns_by_level_function(pelvic_angles_pre_df)
  
  ## Now assemble all into 1 wide df
  alignment_measures_wide_df <- sagittal_cobb_df %>%
    left_join(pelvic_angles_df) %>%
    left_join(vertebral_tilt_df)
  
  return(alignment_measures_wide_df)
}

jh_symbols_list <- list(superscript_2 = "²",
                        degree_symbol = "º",
                        rho_label = sprintf('\u03c1'),
                        multiplication_symbol = "×", 
                        beta_symbol = "β", 
                        plus_minus_sign = "±")
