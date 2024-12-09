library(readxl)
library(dplyr)
library(stringr)
library(knitr)
library(kableExtra)

generate_table_prop_select_multiple <- function(data, analysis_var_input, group_var_value_input) {
  # Generate a table for prop_select_multiple
  filtered_data <- data %>%
      filter(
        analysis_type == "prop_select_multiple",
        analysis_var == analysis_var_input,  # Filtra per analysis_var
        group_var_value == group_var_value_input  # Filtra per valore di group_var_value
      ) %>%
      mutate(stat = round(stat*100, 1)) %>%  # Moltiplica per 100 e arrotonda a 1 decimale
      select(analysis_var_value, label, stat, n, group_var_value)  # Mostra solo analysis_var_value e stat

    table_label <- unique(filtered_data$label)
    table_group_var_value <- unique(filtered_data$group_var_value)

    filtered_data <- filtered_data %>% select(-group_var_value, -label)
    # Crea la tabella
    return(
      kable(
        filtered_data,
        caption = paste0(
          "Multi-choice question: <b>", table_label, 
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