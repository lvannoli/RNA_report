library(readxl)
library(dplyr)
library(stringr)
library(knitr)
library(kableExtra)

generate_table_prop_select_multiple <- function(data, analysis_var_input, section_input, group_var_value_input) {
  # Generate a table for prop_select_multiple
  filtered_data <- data %>%
    filter(
      analysis_type == "prop_select_multiple",
      analysis_var == analysis_var_input,  # Filtra per analysis_var
      group_var_value == group_var_value_input  # Filtra per valore di group_var_value
    ) %>%
    mutate(stat = round(stat*100, 1)) %>%  # Moltiplica per 100 e arrotonda a 1 decimale
    select(analysis_var_value, label, stat, n, group_var_value)  # Mostra solo analysis_var_value e stat

  table_section <- section_input
  table_label <- unique(filtered_data$label)
  table_group_var_value <- unique(filtered_data$group_var_value)

  filtered_data <- filtered_data %>% select(-group_var_value, -label)  # remove the columns we don't want to display
  # Crea la tabella
  
  return(
    kable(
    filtered_data,
    caption = paste0(
      "Multi-choice question: <b>", table_label, 
      "</b> - Section: <b>", table_section,
      "</b> - District: <b>", table_group_var_value,"</b>"
    ),
    col.names = c("Options", "%", "N of Responses"),
    format = "html"
  ) %>%
  kable_minimal(
    html_font = "\"Raleway\", raleway, Segoe UI Light, sans-serif"  # Apply font styling
  )%>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  )%>%
  column_spec(1, width = "20em") %>%  # Adjust the width of the first column
  column_spec(2, width = "20em") %>%  # Adjust the width of the second column
  column_spec(3, width = "20em")      # Adjust the width of the third column
  )
}

# Funzione per prop_select_one
generate_table_prop_select_one <- function(data, group_var_value_input, section_input, label_input) {
  filtered_data <- data %>%
    filter(
      analysis_type == "prop_select_one",
      group_var_value == group_var_value_input,
      section == section_input,
      label == label_input
    ) %>%
    mutate(stat = round(stat*100, 1)) %>%  # Moltiplica per 100 e arrotonda a 1 decimale
    select(analysis_var_value, section, label, stat, n)  # Mostra solo analysis_var_value e stat

  table_section <- unique(filtered_data$section)
  table_label <- unique(filtered_data$label)

  filtered_data <- filtered_data %>% select(-section, -label)

  return(
    kable(
    filtered_data,
    caption = paste0(
          "Single-choice question: <b>", table_label, 
          "</b> - Section: <b>", table_section,
          "</b> - District: <b>", group_var_value_input
        ),
    col.names = c(table_label, "%", "N of Responses"),
    format = "html"
  ) %>%
  kable_minimal(
    html_font = "\"Raleway\", raleway, Segoe UI Light, sans-serif"  # Apply font styling
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  ) %>%
  column_spec(1, width = "20em") %>%  # Adjust the width of the first column
  column_spec(2, width = "20em") %>%  # Adjust the width of the second column
  column_spec(3, width = "20em")      # Adjust the width of the third column
  )
}

# function for mean analysis
generate_table_mean <- function(data, group_var_value_input, section_input, label_input) {
  filtered_data <- data %>%
    filter(
      analysis_type == "mean",
      group_var_value == group_var_value_input,
      section == section_input,
      label == label_input
    ) %>%
    mutate(stat = round(stat, 1)) %>%  # Arrotonda a 1 decimale
    select(analysis_var_value, section, label, stat, n)  # Mostra solo analysis_var_value e stat

  table_section <- unique(filtered_data$section)
  table_label <- unique(filtered_data$label)

  filtered_data <- filtered_data %>% select(-section, -label)

  return(
    kable(
    filtered_data,
    caption = paste0(
          "Mean analysis: <b>", table_label, 
          "</b> - Section: <b>", table_section,
          "</b> - District: <b>", group_var_value_input
        ),
    col.names = c(table_label, "Mean", "N of Responses"),
    format = "html"
  ) %>%
  kable_minimal(
    html_font = "\"Raleway\", raleway, Segoe UI Light, sans-serif"  # Apply font styling
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE
  ) %>%
  column_spec(1, width = "20em") %>%  # Adjust the width of the first column
  column_spec(2, width = "20em") %>%  # Adjust the width of the second column
  column_spec(3, width = "20em")      # Adjust the width of the third column
  )
}


# Helper function to generate and print plots
generate_and_print_plot <- function(data, plot_function, ...) {
  tryCatch({
    plot <- do.call(plot_function, c(list(data = data), list(...)))
    if (!is.null(plot)) print(plot)
  }, error = function(e) {
    message(sprintf("Error generating plot: %s", e$message))
    return(NULL)
  })
}

# Plotting Functions
plot_multi_choice <- function(data, label_input) {
  filtered_data <- data %>% filter(label == label_input)

  # Compute statistics
  total_responses <- filtered_data$n_total

  # # Determine if a legend is needed
  # use_legend <- nrow(filtered_data) > 6 || any(nchar(filtered_data$analysis_var_value) > 40)

  # ggplot(filtered_data, aes(x = factor(1), y = round(stat*100, 1), fill = str_wrap(analysis_var_value, width = 10))) +
  #   geom_bar(stat = "identity",  fill = "#75C376", position = "dodge", width = 0.7) +
  #   geom_text(
  #     aes(label = round(stat * 100, 1)), # Add the percentage values on top of the bars
  #     vjust = -0.5, # Position slightly above the bars
  #     size = 4, # Font size
  #     color = "black"
  #   ) +
  #   labs(
  #     title = str_wrap(paste("Multi-Choice: ", label_input), width=40),
  #     x = "Options",
  #     y = "Percentage [%]",
  #     fill = "Options" # legend titles
  #   ) +
  #   theme_minimal() +
  #   theme(
  #     axis.text.x = if (use_legend) element_blank() else element_text(angle = 45, hjust = 1),
  #     axis.ticks.x = if (use_legend) element_blank() else element_line(),
  #     plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  #     legend.position = if (use_legend) "right" else "none"
  #   ) +
  #   scale_y_continuous(
  #     limits = c(0, max(filtered_data$stat, na.rm = TRUE) + 10), # Extend y-axis
  #     expand = expansion(mult = c(0, 0.1))
  #   ) +
  #   # add the stat box
  #   annotate(
  #     "text", 
  #     x = 1,  # Position near the first bar
  #     y = max(filtered_data$stat * 100) + 10,  # Position above the highest bar
  #     label = paste0("Total Responses: ", round(total_responses, 1)),
  #     hjust = 0, # right alignment
  #     vjust = 0, # Top alignment
  #     size = 3,
  #     color = "black"
  #   )
  ggplot(filtered_data, aes(x = str_wrap(analysis_var_value, width = 10), y = round(stat * 100, 1))) +
    geom_bar(stat = "identity", fill = "#75C376") +
    geom_text(
      aes(label = round(stat * 100, 1)), # Add the percentage values on top of the bars
      vjust = -0.5, # Position slightly above the bars
      size = 4, # Font size
      color = "black"
    ) +
    labs(
      title = str_wrap(paste("Multi-Choice: ", label_input), width=40),
      x = "Options",
      y = "Percentage [%]"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 10)), # Adjust x-axis labels
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5) # Adjust title
    ) +
    scale_y_continuous(
      limits = c(0, max(filtered_data$stat * 100, na.rm = TRUE) + 20), # Extend y-axis
      expand = expansion(mult = c(0, 0.1)) # Add space above
    ) +
    # add the stat box
    annotate(
      "text", 
      x = 1,  # Position near the first bar
      y = max(filtered_data$stat * 100) + 10,  # Position above the highest bar
      label = paste0("Total Responses: ", round(total_responses, 1)),
      hjust = 0, # right alignment
      vjust = 0, # Top alignment
      size = 3,
      color = "black"
    )
}

plot_single_choice <- function(data, label_input) {
  filtered_data <- data %>% filter(label == label_input)

  # Compute statistics
  total_responses <- filtered_data$n_total
  
  ggplot(filtered_data, aes(x = str_wrap(analysis_var_value, width = 10), y = round(stat * 100, 1))) +
    geom_bar(stat = "identity", fill = "#EE5859") +
    geom_text(
      aes(label = round(stat * 100, 1)), # Add the percentage values on top of the bars
      vjust = -0.5, # Position slightly above the bars
      size = 4, # Font size
      color = "black"
    ) +
    labs(
      title = str_wrap(paste("Single-Choice: ", label_input), width=40),
      x = "Options",
      y = "Percentage [%]"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 10)), # Adjust x-axis labels
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5) # Adjust title
    ) +
    scale_y_continuous(
      limits = c(0, max(filtered_data$stat * 100, na.rm = TRUE) + 20), # Extend y-axis
      expand = expansion(mult = c(0, 0.1)) # Add space above
    ) +
    # add the stat box
    annotate(
    "text", 
    x = 1,  # Position near the first bar
    y = max(filtered_data$stat * 100) + 10,  # Position above the highest bar
    label = paste0("Total Responses: ", round(total_responses, 1)),
    hjust = 0, # Left alignment
    vjust = 0, # Top alignment
    size = 4,
    color = "black"
  )
}

plot_mean_analysis <- function(data, label_input) {
  filtered_data <- data %>% filter(label == label_input)

  # Compute statistics
  total_responses <- filtered_data$n_total

  # Adjust y-axis limits dynamically
  y_max <- max(filtered_data$stat, na.rm = TRUE)
  y_limit <- y_max + y_max * 0.2  # Add 20% extra space above the bars

  ggplot(filtered_data, aes(x = str_wrap(analysis_var_value, width = 10), y =stat)) +
    geom_bar(stat = "identity", fill = "#0C596B", width = 0.7) +
    geom_text(
      aes(label = round(stat, 2)), # Add the percentage values on top of the bars
      vjust = -0.5, # Position slightly above the bars
      size = 4, # Font size
      color = "black"
    ) +
    labs(
      title = str_wrap(paste("Mean Analysis: ", label_input), width=40),
      x = "Options",
      y = "Mean Value"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, margin = margin(t = 10)), # Adjust x-axis labels
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5), # Adjust title
      axis.title.y = element_text(margin = margin(r = 10)),                                 # Adjust y-axis title margin
      axis.title.x = element_text(margin = margin(t = 10))                             # Adjust x-axis title margin
    ) +
    scale_y_continuous(
      limits = c(0, y_limit), # Extend y-axis
      expand = expansion(mult = c(0, 0.1)) # Add space above
    ) +
    # add the stat box
    annotate(
    "text", 
    x = 1,  # Position near the first bar
    y = y_limit,     # Position above the highest bar
    label = paste0("Total Responses: ", round(total_responses, 1)),
    hjust = 0, # Left alignment
    vjust = 0, # Top alignment
    size = 4,
    color = "black"
  )
}
