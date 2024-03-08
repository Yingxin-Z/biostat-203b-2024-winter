## ---------------------------
##
## Script name: app.R
##
## Purpose of script: To create a shiny app for summaries and plots
##
## Author: Yingxin Zhang, UID:006140202
##
## Date Created: 2024-03-07
##
## Copyright (c) 2024 Yingxin Zhang
##
## ---------------------------

# Load the required libraries
library(shiny)
library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)

# Load the ICU cohort data
mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds") |>
  as_tibble() |>
  mutate(los_group = cut(los, breaks = c(0, 2, 5, 10, 15, 20, 115)),
         age_group = cut(age_intime, 
                         breaks = c(17, 30, 40, 50, 60, 70, 80, 90, 103)))

# Here I put the token in the same directory as the app.R file, 
# if you did not connect before and want to use the token, please uncomment the following lines
# satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
# bq_auth(path = satoken)

# connect to the BigQuery database `biostat-203b-2024-winter.mimic4_v2_2`
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)

# Import the hosp data
d_icd_procedures <- tbl(con_bq, "d_icd_procedures") |> 
  select(icd_code, long_title) |>
  as_tibble()
d_icd_diagnoses <- tbl(con_bq, "d_icd_diagnoses") |> 
  select(icd_code, long_title) |>
  as_tibble()
d_items <- tbl(con_bq, "d_items") |> 
  filter(abbreviation %in% c("HR", "NBPd", "NBPs", "Temperature F", "RR")) |> 
  select(itemid, abbreviation) |> as_tibble()

# Define demographic variables
demo_vars_ctg <- c("race", "insurance", "marital_status", "gender", 
                   "age_group", "first_careunit", "last_careunit", 
                   "admission_type", "admission_location")

# Define lab measurement variables
lab_vars <- c("Creatinine", "Potassium", "Sodium", "Chloride", "Bicarbonate", 
              "Hematocrit", "Glucose", "White Blood Cells")

# Define vital measurement variables
vital_vars <- c("Heart Rate", "Non Invasive Blood Pressure systolic", 
                "Non Invasive Blood Pressure diastolic", 
                "Temperature Fahrenheit", "Respiratory Rate")

## Lab measurement long format
long_lab <- pivot_longer(mimic_icu_cohort, cols = lab_vars, 
                         names_to = "Variable", values_to = "Value")

long_lab_filtered <- long_lab |>
  group_by(Variable) |>
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) |>
  filter(Value >= (Q1 - 1.5 * IQR) & Value <= (Q3 + 1.5 * IQR)) |>
  select(-c(Q1, Q3, IQR))

# Vital measurement long format
long_vital <- pivot_longer(mimic_icu_cohort, cols = vital_vars, 
                         names_to = "Variable", values_to = "Value")

long_vital_filtered <- long_vital |>
  group_by(Variable) |>
  mutate(
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) |>
  filter(Value >= (Q1 - 1.5 * IQR) & Value <= (Q3 + 1.5 * IQR)) |>
  select(-c(Q1, Q3, IQR))

################################################
################################################
# Define the UI for the Shiny app
ui <- fluidPage(
  # Application title
  titlePanel("ICU Cohort Explorer"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      tabPanel("Summary and Visualization of Characteristics", 
        # Input: drop-down list for variables ----
        selectInput(
          inputId = "char",
          label = "Characteristics of Interest", 
          choices = c(
            "Race" = "race", 
            "Insurance" = "insurance", 
            "Marital status" = "marital_status", 
            "Gender" = "gender", 
            "Age at intime" = "age_group", 
            "First care unit" = "first_careunit", 
            "Last care unit" = "last_careunit", 
            "Admission location" = "admission_location", 
            "Lab events", "Vital events")), 
        # add a "outlier_remover" checkbox
        checkboxInput(
          inputId = "remove_outliers", 
          label = "Remove outliers in the lab/vital measurement plots 
          based on the IQR method", 
          value = FALSE)),
       tabPanel("Patient Information",
         #Select patient ID
         selectInput(
           inputId = "patient_id", 
           label = "Select Patient ID",
           choices = unique(mimic_icu_cohort$subject_id)))),
    
    # Main panel for displaying tab content
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Summary and Visualization of Characteristics", 
          plotOutput("char_plot"), 
          gt_output("char_table")
        ), 
        tabPanel(
          title = "Patient ADT and ICU Information",
          # Display the patient information
          textOutput("patient_info_output"),
          plotOutput("ADTplot"), 
          plotOutput("ICUplot"))
      )
    )
  )
)

################################################
# Define the server logic
server <- function(input, output) {
  # Render the characteristic table and plot
  output$char_plot <- renderPlot(
    if (input$char %in% demo_vars_ctg) {
      inchar <- as.character(input$char)
      ggplot(mimic_icu_cohort) +
        geom_bar(mapping = aes(x = !!sym(inchar), fill = los_group)) +
        coord_flip() +
        labs(title = paste0("Distributions of ", as.character(inchar), 
                            " by length of ICU stay")) +
        theme(axis.title = element_text(size = 8))
    } else if (input$char == "Lab events") {
      if (input$remove_outliers) {
        ggplot(long_lab_filtered, aes(x = Variable, y = Value)) +
          geom_boxplot() +
          coord_flip() +
          theme_minimal() +
          labs(title = "Boxplot of lab measurements (outliers removed)")
      } else {
        ggplot(long_lab, aes(x = Variable, y = Value)) +
          geom_boxplot() +
          coord_flip() + 
          theme_minimal() + 
          labs(title = "Boxplot of lab measurements")
      }
    } else {
      if (input$remove_outliers) {
        ggplot(long_vital_filtered, aes(x = Variable, y = Value)) +
          geom_boxplot() +
          coord_flip() +
          theme_minimal() +
          labs(title = "Boxplot of vital measurements (outliers removed)")
      } else {
        ggplot(long_vital, aes(x = Variable, y = Value)) +
          geom_boxplot() +
          coord_flip() + 
          theme_minimal() + 
          labs(title = "Boxplot of vital measurements")
      }
    }
  )
  
  output$char_table <- render_gt(
    if (input$char %in% demo_vars_ctg) {
      mimic_icu_cohort |>
        select(all_of(as.character(input$char)), los_long) |>
        tbl_summary(by = los_long) |>
        as_gt() |>
        tab_header(
          title = paste("Summary of ", input$char, 
                        " by length of stay (if >= 2)")
        )
    } else if (input$char == "Lab events") {
      mimic_icu_cohort |>
        select(all_of(lab_vars), los_long) |>
        tbl_summary(by = los_long) |>
        as_gt() |> 
        tab_header(
          title = "Summary of lab events by length of stay (if >= 2)"
        )
    } else {
      mimic_icu_cohort |>
        select(all_of(vital_vars), los_long) |>
        tbl_summary(by = los_long) |>
        as_gt() |>
        tab_header(
          title = "Summary of vital events by length of stay (if >= 2)"
        )
    }
  )
  
  # Render the patient information output
  output$patient_info_output <- renderText({
    # Display patient information based on selected patient ID
    req(input$patient_id)
    
    # Filter the ICU cohort data for the selected patient ID
    patient_data <- mimic_icu_cohort |>
      filter(subject_id == input$patient_id) |>
      select(subject_id, gender, age_intime, race) |>
      slice(1)
    
    # Return patient information as a formatted text
    paste0("Patient ", patient_data$subject_id, ", ",
           patient_data$gender, ", ",
           patient_data$age_intime, " years old, ",
           patient_data$race)
  })
  
  # Render the ADT plot
  output$ADTplot <- renderPlot({
    patient_id <- input$patient_id
    transfers <- tbl(con_bq, "transfers") |> 
      select(subject_id, intime, outtime, careunit) |> 
      filter(subject_id == as.integer(!!input$patient_id)) |> as_tibble()
    labevents <- tbl(con_bq, "labevents") |> 
      select(subject_id, charttime, itemid, valuenum) |> 
      filter(subject_id == as.integer(!!input$patient_id)) |> as_tibble()
    procedures_icd <- tbl(con_bq, "procedures_icd") |> 
      select(subject_id, icd_code, chartdate) |> 
      filter(subject_id == as.integer(!!input$patient_id)) |> as_tibble()
    diagnoses_icd <- tbl(con_bq, "diagnoses_icd") |> 
      select(subject_id, icd_code) |> 
      filter(subject_id == as.integer(!!input$patient_id)) |> as_tibble()
    
    # add col unit to transfers, if careunit contains "ICU" or "CCU", then unit is 10, otherwise 8
    transfers <- transfers |>
      mutate(unit = ifelse(str_detect(careunit, "ICU|CCU"), 10, 8))
    
    # remove missing values in transfers
    transfers <- transfers |> drop_na()
    
    # remove missing values in labevents
    labevents <- labevents |> drop_na()
    
    # merge procedures_icd and d_icd_procedures
    procedures <- procedures_icd |>
      left_join(d_icd_procedures, by = "icd_code")
    
    # insert column Procedure = the content before the first comma in long_title, if long_title contains a comma
    procedures$Procedure <- sub(",.*", "", procedures$long_title)
    
    # change the format of chartdate to POSIXct
    procedures$chartdate <- as.POSIXct(procedures$chartdate)
    
    # merge diagnoses_icd_10013310 and d_icd_diagnoses
    diagnoses <- diagnoses_icd |>
      left_join(d_icd_diagnoses, by = "icd_code")
    
    # change the colname "long_title" to "long_title_diagnoses"
    colnames(diagnoses)[colnames(diagnoses) == "long_title"] <- "long_diag"
    
    # pull top 3 diagnoses
    top3_diagnoses <- diagnoses |>
      count(long_diag, sort = TRUE) |>
      slice(1:3) |>
      pull(long_diag)
    
    # number of procedure types
    num_proc <- length(unique(procedures$Procedure))
    
    # draw the ADT history for patient
    ggplot() +
      geom_segment(data = transfers, 
                   aes(x = intime, xend = outtime, 
                       y = 3, yend = 3, color = careunit, size = unit)) +
      geom_point(data = labevents, 
                 aes(x = charttime, y = 2), shape = 3, size = 5) +
      geom_point(data = procedures, 
                 aes(x = chartdate, y = 1, shape = Procedure), size = 5) +
      labs(subtitle = paste(top3_diagnoses, collapse = "\n")) +
      xlab("Calendar Time") +
      theme(axis.title.y = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(color = "gray", linewidth = 0.2),
            panel.border = element_rect(fill = NA, linewidth = 0.5)) +  
      theme(axis.title = element_text(size = 12), 
            legend.text = element_text(size = 8),
            legend.position = "bottom",  # Placing legend at the bottom
            legend.box = "vertical") +
      scale_y_continuous(breaks = 1:3, labels = c("Procedure", "Lab", "ADT")) +
      coord_cartesian(ylim = c(1, 3)) +
      guides(size = FALSE) +
      guides(shape = guide_legend(nrow = as.integer(num_proc/2) + 1)) +
      guides(color = guide_legend(title = "Care Unit"))
  })
  
  # Render the ICU plot
  output$ICUplot <- renderPlot({
    req(input$patient_id)
    chartevents <- tbl(con_bq, "chartevents") |> 
      select(subject_id, stay_id, charttime, itemid, valuenum) |> 
      filter(subject_id == as.integer(!!input$patient_id)) |>
      as_tibble() |> 
      semi_join(d_items, by = "itemid") |> 
      left_join(d_items, by = "itemid") |>
      as_tibble()

    # draw the ICU vitals and put different vitals in different facets
    ggplot(chartevents, 
           aes(x = charttime, y = valuenum, color = abbreviation)) +
      geom_line() +
      geom_point() +
      facet_grid(abbreviation ~ stay_id, scales = "free") +
      labs(title = "ICU stays - Vitals for Patient") +
      theme(axis.title.y = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(color = "gray", linewidth = 0.5),
            panel.border = element_rect(
              color = "grey", fill = NA, linewidth = 0.8),
            strip.text = element_text(size = 10)) + 
      scale_color_brewer(palette = "Set1") +
      theme(axis.title.x = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(color = FALSE)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
