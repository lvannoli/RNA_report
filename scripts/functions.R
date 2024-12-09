library(readxl)
library(dplyr)
library(stringr)
library(knitr)
library(kableExtra)

generate_table_prop_select_multiple <- function(data, analysis_var_input, group_var_value_input) {
  # Filtra i dati per analysis_type e analysis_var
  filtered_data <- data %>%
    filter(
      analysis_type == "prop_select_multiple",
      analysis_var == analysis_var_input,  # Filtra per analysis_var
      group_var_value == group_var_value_input  # Filtra per valore di group_var_value
    ) %>%
    select(analysis_var_value, label, stat, n)  # Mostra solo analysis_var_value e stat
  
  # Crea la tabella
  table_output <- kable(
    filtered_data,
    caption = paste("Table for prop_select_multiple - Group Value:", group_var_value_input),
    col.names = c("Option", "Label", "Stat", "N"),
    format = "html"
  ) %>%
  kable_minimal(
    html_font = "\"Raleway\", raleway, Segoe UI Light, sans-serif"  # Apply font styling
  )
  return(table_output)
}

# Funzione per prop_select_one
generate_table_prop_select_one <- function(data, analysis_var_input, section_input, label_input) {
  filtered_data <- data %>%
    filter(
      analysis_type == "prop_select_one",
      analysis_var == analysis_var_input,
      section == section_input,
      label == label_input
    ) %>%
    select(group_var_value, label, stat, n)  # Mostra group_var_value e stat
  
  table_output <- kable(
    filtered_data,
    caption = paste("Table for prop_select_one - Section:", section_input, "- Label:", label_input),
    col.names = c("Group_var_value", "Label", "Stat", "N"),
    format = "html"
  ) %>%
  kable_minimal(
    html_font = "\"Raleway\", raleway, Segoe UI Light, sans-serif"  # Apply font styling
  )
  return(table_output)
}