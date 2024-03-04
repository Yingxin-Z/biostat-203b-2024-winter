library(shiny)
library(gtsummary)
library(dplyr)
library(tidyr)
library(bigrquery)
library(DBI)
library(gt)
library(tidyverse)

if (exists("con_bq")) {
  dbDisconnect(con_bq)
}
rm(list = ls())

# Load the ICU cohort data
mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds") |>
  as_tibble() |>
  mutate(los_group = cut(los, breaks = c(0, 2.5, 5, 10, 15, 20, 115)),
         age_group = cut(age_intime, 
                         breaks = c(17, 30, 40, 50, 60, 70, 80, 90, 103)))

# path to the service account token 
satoken <- "biostat-203b-2024-winter-313290ce47a6.json"
# BigQuery authentication using service account
bq_auth(path = satoken)

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
demo_vars <- c("race", "insurance", "marital_status", "gender", "age_intime")
demo_vars_ctg <- c("race", "insurance", "marital_status", "gender")

# Define lab measurement variables
lab_vars <- c("Creatinine", "Potassium", "Sodium", "Chloride", "Bicarbonate", 
              "Hematocrit", "Glucose", "White Blood Cells")

# Define vital measurement variables
vital_vars <- c("Heart Rate", "Non Invasive Blood Pressure systolic", 
                "Non Invasive Blood Pressure diastolic", 
                "Temperature Fahrenheit", "Respiratory Rate")

list(
  list(demo_vars, lab_vars, vital_vars), 
  list("Table 1. Demographic characteristics of MIMIC IV", 
       "Table 2. Lab measurements of MIMIC IV", 
       "Table 3. Vital measurements of MIMIC IV")
) %>% 
  pmap(function(key_vars, title){
    mimic_icu_cohort %>% 
      select(
        all_of(key_vars), los_group
      ) %>% 
      tbl_summary(
        by = los_group, missing = "no"
      ) %>% 
      add_n() %>% 
      add_overall() %>% 
      modify_header(label = "**Characteristics**") %>%
      modify_spanning_header(
        starts_with("stat_") ~ "**Length of ICU Stay**"
      ) %>%
      modify_caption(title) %>%
      bold_labels() %>% 
      as_gt()
  }) %>% 
  set_names("demo_table", "lab_table", "vital_table") %>% 
  list2env(envir = .GlobalEnv)

## Demographic plots
# for race, insurance, marital_status, gender
plots_demo <- list()

# Iterate over each demographic variable
for (var in demo_vars_ctg) {
  p <- ggplot(mimic_icu_cohort) +
    geom_bar(mapping = aes(x = !!sym(var), fill = los_group)) +
    labs(title = paste("Length of ICU Stay vs ", var)) +
    theme(axis.title = element_text(size = 8))
  
  # Add the plot to the list
  plots_demo[[var]] <- p
}

# for age_intime
demo_plots_age <- ggplot(data = mimic_icu_cohort) + 
  geom_bar(mapping = aes(x = age_group, fill = los_group)) +
  labs(title = "Length of ICU Stay vs age at intime") +
  labs(x = "Age at Intime (years)", 
       y = "Count",
       fill = "Length of ICU Stay (los)") + 
  theme_minimal()

# combine all plots
plots_demo[["age_intime"]] <- demo_plots_age

## Lab measurement plots
plots_lab <- list()

# Iterate over each demographic variable
for (var in lab_vars) {
  p <- ggplot(mimic_icu_cohort, aes(x = !!sym(var), y = los)) +
    # geom_point() +
    geom_smooth() +
    labs(title = paste("Length of ICU Stay vs ", var)) +
    theme(axis.title = element_text(size = 12), 
          legend.text = element_text(size = 8))
  theme_minimal()
  
  # Add the plot to the list
  plots_lab[[var]] <- p
}

## Vital measurement plots
plots_vital <- list()

# Iterate over each demographic variable
for (var in vital_vars) {
  p <- ggplot(mimic_icu_cohort, aes(x = !!sym(var), y = los)) +
    geom_smooth() +
    labs(title = paste("Length of ICU Stay vs ", var)) +
    theme_minimal()
  
  # Add the plot to the list
  plots_vital[[var]] <- p
}



# Define the UI for the Shiny app
ui <- fluidPage(
  
  # Application title
  titlePanel("ICU Cohort Explorer"),
  
  # Sidebar layout with tabset panel
  sidebarLayout(
    sidebarPanel(
      # Tabset panel with two tabs
      tabsetPanel(
        tabPanel("Summary and Visualization", 
          # Input: drop-down list for Demographic variables ----
          selectInput(
            inputId = "demo", 
            label = "Demographic characteristics", 
            choices = c(
              "race", "insurance", "marital status", "gender", "age at intime"
            )
          ),
          # Input: drop-down list for Lab measurements ----
          selectInput(
            inputId = "lab",
            label = "Lab measurements",
            choices = c(
              "Creatinine", "Potassium", "Sodium", "Chloride", "Bicarbonate", 
              "Hematocrit", "Glucose", "White Blood Cells"
            )
          ),
          # Input: drop-down list for Vital measurements ----
          selectInput(
            inputId = "vital",
            label = "Vital measurements",
            choices = c(
              "Heart Rate", "Non Invasive Blood Pressure systolic", 
              "Non Invasive Blood Pressure diastolic", 
              "Temperature Fahrenheit", "Respiratory Rate"
            )
          )
        ),
        tabPanel("Patient Information",
          # Select patient ID
            selectInput(
              inputId = "patient_id", 
              label = "Select Patient ID",
              choices = unique(mimic_icu_cohort$subject_id)
            )
         )
      )
    ),
    
    # Main panel for displaying tab content
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Demographics", 
          gt_output("demo_table"), 
          plotOutput("demo_plot")
        ),
        tabPanel(
          title = "Lab Measurements", 
          gt_output("lab_table"), 
          plotOutput("lab_plot")
        ),
        tabPanel(
          title = "Vital measurements", 
          gt_output("vital_table"), 
          plotOutput("vital_plot")
        ),
        tabPanel(
          title = "Patient Information",
          # Display the patient information
          textOutput("patient_info_output"),
          plotOutput("ADTplot"), 
          plotOutput("ICUplot"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  demo_plot_source <- reactive({
    switch(
      input$demo, 
      "race" = plots_demo[[1]], 
      "insurance" = plots_demo[[2]], 
      "marital status" = plots_demo[[3]], 
      "gender" = plots_demo[[4]], 
      "age at intime" = plots_demo[[5]]
    )
  })
  
  lab_plot_source <- reactive({
    switch(
      input$lab, 
      "Creatinine" = plots_lab[[1]],
      "Potassium" = plots_lab[[2]],
      "Sodium" = plots_lab[[3]],
      "Chloride" = plots_lab[[4]],
      "Bicarbonate" = plots_lab[[5]],
      "Hematocrit" = plots_lab[[6]],
      "Glucose" = plots_lab[[7]],
      "White Blood Cells" = plots_lab[[8]]
    )
  })
  
  vital_plot_source <- reactive({
    switch(
      input$vital, 
      "Heart Rate" = plots_vital[[1]],
      "Non Invasive Blood Pressure systolic" = plots_vital[[2]],
      "Non Invasive Blood Pressure diastolic" = plots_vital[[3]],
      "Temperature Fahrenheit" = plots_vital[[4]],
      "Respiratory Rate" = plots_vital[[5]]
    )
  })
  
  output$demo_table <- render_gt(demo_table)
  output$lab_table <- render_gt(lab_table)
  output$vital_table <- render_gt(vital_table)
  
  output$demo_plot <- renderPlot({demo_plot_source()})
  output$lab_plot <- renderPlot({lab_plot_source()})
  output$vital_plot <- renderPlot({vital_plot_source()})
  
  # Render the patient information output
  output$patient_info_output <- renderText({
    # Display patient information based on selected patient ID
    req(input$patient_id)
    
    # Filter the ICU cohort data for the selected patient ID
    patient_data <- mimic_icu_cohort %>%
      filter(subject_id == input$patient_id) %>%
      select(subject_id, gender, age_intime, race) %>%
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
    colnames(diagnoses)[colnames(diagnoses) == "long_title"] <- "long_diagnoses"
    
    # pull top 3 diagnoses
    top3_diagnoses <- diagnoses |>
      count(long_diagnoses, sort = TRUE) |>
      slice(1:3) |>
      pull(long_diagnoses)
    
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
      guides(shape = guide_legend(nrow = 5)) +
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
    ggplot(chartevents, aes(x = charttime, y = valuenum, 
                            color = abbreviation)) +
      geom_line() +
      geom_point() +
      facet_grid(abbreviation ~ stay_id, scales = "free") +
      labs(title = "ICU stays - Vitals for Patient") +
      theme(axis.title.y = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid = element_line(color = "gray", linewidth = 0.5),
            panel.border = element_rect(color = "grey", 
                                        fill = NA, linewidth = 0.8),
            strip.text = element_text(size = 10)) + 
      scale_color_brewer(palette = "Set1") +
      theme(axis.title.x = element_blank()) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(color = FALSE)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
