# ============================================================
# Outpatient General Summary Module - Updated Structure
# ============================================================


# -------------------------
# Registrations & Treatments Tab UI (Key Insights + Combined specialty graph)
# -------------------------
outpatientRegistrationsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Back to Clinical Conditions Button
    fluidRow(
      column(12,
             div(
               style = "text-align: right; margin-bottom: 10px;",
               actionButton(
                 ns("back_to_eligible"), 
                 HTML("← Reminder of QI Eligibility"),
                 class = "btn btn-primary fixed-button",
                 style = "
                   background-color: #3498db; 
                   border-color: #3498db; 
                   color: white; 
                   font-size: 12px !important;
                   font-weight: 500; 
                   border-radius: 6px; 
                   padding: 3px 8px !important;
                   min-width: auto; 
                   width: auto;
                   line-height: 1.2;
                 ",
                 onclick = "
                   Shiny.setInputValue('navigate_to_overview', Math.random(), {priority: 'event'});
                   window.scrollTo({top: 0, behavior: 'smooth'});
                 ")
             )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == false",
      fluidRow(
        column(10, offset = 1,
               box(
                 width = 12, 
                 title = "📤 Upload Required", 
                 status = "warning", 
                 solidHeader = TRUE,
                 p("Please upload your data files to view registrations and treatments summary.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      
      # Value boxes (key insights)
      fluidRow(
        column(10, offset = 1,
               valueBoxOutput(ns("total_registered"), width = 3),
               valueBoxOutput(ns("total_antimicrobials"), width = 3),
               valueBoxOutput(ns("total_antibiotics"), width = 3),
               valueBoxOutput(ns("proportion_antibiotics"), width = 3)
        )
      ),
      
      # Combined Chart (Registered and Treated)
      fluidRow(
        column(10, offset = 1,
               box(
                 title = "Patient Distribution by Specialty", 
                 status = "primary", 
                 solidHeader = TRUE,
                 width = 12,
                 plotlyOutput(ns("specialty_combined_chart"), height = "320px")
               )
        )
      )
    )
  )
}

# -------------------------
# Antimicrobial Prescription Tab UI (Like AWaRe summary boxes)
# -------------------------
outpatientAntimicrobialPrescriptionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Back to Clinical Conditions Button
    fluidRow(
      column(12,
             div(
               style = "text-align: right; margin-bottom: 10px;",
               actionButton(
                 ns("back_to_eligible"), 
                 HTML("← Reminder of QI Eligibility"),
                 class = "btn btn-primary fixed-button",
                 style = "
                   background-color: #3498db; 
                   border-color: #3498db; 
                   color: white; 
                   font-size: 12px !important;
                   font-weight: 500; 
                   border-radius: 6px; 
                   padding: 3px 8px !important;
                   min-width: auto; 
                   width: auto;
                   line-height: 1.2;
                 ",
                 onclick = "
                   Shiny.setInputValue('navigate_to_overview', Math.random(), {priority: 'event'});
                   window.scrollTo({top: 0, behavior: 'smooth'});
                 ")
             )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == false",
      fluidRow(
        column(10, offset = 1,
               box(
                 width = 12, 
                 title = "📤 Upload Required", 
                 status = "warning", 
                 solidHeader = TRUE,
                 p("Please upload your data files to view antimicrobial prescription overview.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      
      fluidRow(
        column(10, offset = 1,
               h3("💊 Overview of Antimicrobial Prescription", 
                  style = "text-align: center; margin-bottom: 10px;"),
               div(class = "note-box",
                   strong("💡 Note:"), "Eligible patients are adults presenting with one of seven predefined conditions."
               )
        )
      ),
      
      # Facility-Wide Summary Box
      fluidRow(
        uiOutput(ns("facility_wide_summary"))
      ),
      
      fluidRow(
        column(10, offset = 1,
               div(class = "note-box",
                   strong("💡 Note:"), "The AWaRe classification divides antibiotics into three categories: Access, Watch, and Reserve. Access antibiotics are first-line treatments with a low risk of resistance, typically recommended for common infections. Watch antibiotics are associated with a higher risk of resistance and require careful monitoring. Reserve antibiotics are reserved for critical cases involving resistant infections, typically used only when other options are ineffective."
               )
        )
      ),
      
      # Specialty Summary Boxes
      uiOutput(ns("specialty_summary_boxes"))
    )
  )
}

# -------------------------
# Antibiotics by Diagnosis Tab UI
# -------------------------
outpatientAntibioticsDiagnosisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Back to Clinical Conditions Button
    fluidRow(
      column(12,
             div(
               style = "text-align: right; margin-bottom: 10px;",
               actionButton(
                 ns("back_to_eligible"), 
                 HTML("← Reminder of QI Eligibility"),
                 class = "btn btn-primary fixed-button",
                 style = "
                   background-color: #3498db; 
                   border-color: #3498db; 
                   color: white; 
                   font-size: 12px !important;
                   font-weight: 500; 
                   border-radius: 6px; 
                   padding: 3px 8px !important;
                   min-width: auto; 
                   width: auto;
                   line-height: 1.2;
                 ",
                 onclick = "
                   Shiny.setInputValue('navigate_to_overview', Math.random(), {priority: 'event'});
                   window.scrollTo({top: 0, behavior: 'smooth'});
                 ")
             )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == false",
      fluidRow(
        column(10, offset = 1,
               box(
                 width = 12, 
                 title = "📤 Upload Required", 
                 status = "warning", 
                 solidHeader = TRUE,
                 p("Please upload your data files to view antibiotic prescriptions by diagnosis.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      
      fluidRow(
        column(10, offset = 1,
               box(
                 width = 12,
                 title = "📊 Antibiotic Prescriptions by Condition",
                 status = "primary",
                 solidHeader = TRUE,
                 p("This plot provides a general summary of antibiotic prescriptions across pre-defined diagnostic groups, regardless of age, indication, or type of treatment."),
                 div(style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                     withSpinner(
                       plotlyOutput(ns("antibiotics_diagnosis_plot"), height = "520px", width = "100%"),
                       type = 4
                     ))
               )
        )
      )
    )
  )
}

# -------------------------
# Antibiotics by Diagnosis & Specialty Tab UI
# -------------------------
outpatientAntibioticsDiagnosisSpecialtyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Back to Clinical Conditions Button
    fluidRow(
      column(12,
             div(
               style = "text-align: right; margin-bottom: 10px;",
               actionButton(
                 ns("back_to_eligible"), 
                 HTML("← Reminder of QI Eligibility"),
                 class = "btn btn-primary fixed-button",
                 style = "
                   background-color: #3498db; 
                   border-color: #3498db; 
                   color: white; 
                   font-size: 12px !important;
                   font-weight: 500; 
                   border-radius: 6px; 
                   padding: 3px 8px !important;
                   min-width: auto; 
                   width: auto;
                   line-height: 1.2;
                 ",
                 onclick = "
                   Shiny.setInputValue('navigate_to_overview', Math.random(), {priority: 'event'});
                   window.scrollTo({top: 0, behavior: 'smooth'});
                 ")
             )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == false",
      fluidRow(
        column(10, offset = 1,
               box(
                 width = 12, 
                 title = "📤 Upload Required", 
                 status = "warning", 
                 solidHeader = TRUE,
                 p("Please upload your data files to view antibiotic prescriptions by diagnosis and specialty.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      
      fluidRow(
        column(10, offset = 1,
               box(
                 width = 12,
                 title = "📊 Patient Antibiotic Prescriptions by Diagnosis & Specialty",
                 status = "primary",
                 solidHeader = TRUE,
                 p("This plot provides a general summary of patient-level antibiotic prescriptions for pre-defined diagnostic groups and across different specialty, regardless of age."),
                 div(style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                     withSpinner(
                       plotlyOutput(ns("antibiotics_heatmap_plot"), height = "520px", width = "100%"),
                       type = 4
                     ))
                 
                 
                 
               )
        )
      )
    )
  )
}

# -------------------------
# Module Server Function
# -------------------------
outpatientGeneralSummaryServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Constants - matching baseline
    diagnostic_labels <- c(
      RTI = "Respiratory tract infections",
      ENT = "Ear, Nose, Throat infections",
      DENT = "Dental infections",
      B_DIA = "Bloody diarrhoea",
      Non_B_DIA = "Non-bloody diarrhoea",
      LYMPH = "lymph nodes Infections",
      SSTI = "Skin/Soft Tissue Infections",
      UTI = "Lower urinary tract infections"
    )
    
    infection_types <- c("RTI", "ENT", "DENT", "B_DIA", "Non_B_DIA", "LYMPH", "SSTI", "UTI")
    AWaRe_abx <- c("ACCESS", "WATCH", "RESERVE", "NOT RECOMMENDED", "UNCLASSIFIED")
    
    # Check if data is available
    check_data <- function() {
      data <- data_reactive()
      return(!is.null(data) && !is.null(data$data_patients))
    }
    
    # Output to control conditional panels
    output$dataUploaded <- reactive({ check_data() })
    outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
    
    # -------------------------
    # Registrations & Treatments Outputs
    # -------------------------
    
    # Value Box 1: Total Registered Patients
    output$total_registered <- renderValueBox({
      data <- data_reactive(); req(data)
      total <- n_distinct(data$data_patients$`Unique Patient ID`)
      valueBox(
        value = formatC(total, format = "d", big.mark = ","),
        subtitle = HTML("Total Registered<br>Patients"),
        icon = icon("hospital"),
        color = "blue",
        width = 3
      )
    })
    
    # Value Box 2: Total Patients on Antimicrobials (MODIFIED)
    output$total_antimicrobials <- renderValueBox({
      data <- data_reactive(); req(data)
      
      total_antimicrobials <- data$data_patients %>%
        filter(`Antimicrobial prescribed/administered` == "YES") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      valueBox(
        value = formatC(total_antimicrobials, format = "d", big.mark = ","),
        subtitle = HTML("Total Patients<br>on Antimicrobials"),
        icon = icon("syringe"),
        color = "green",
        width = 3
      )
    })
    
    # Value Box 3: Total Patients on Antibiotics (MODIFIED - moved from position 4)
    output$total_antibiotics <- renderValueBox({
      data <- data_reactive(); req(data)
      
      total_abx <- data$data_patients %>%
        filter(
          `Antimicrobial prescribed/administered` == "YES",
          AWaRe %in% AWaRe_abx
        ) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      valueBox(
        value = formatC(total_abx, format = "d", big.mark = ","),
        subtitle = HTML("Total Patients on Antibiotics<br><small style='font-size: 0.8em;'>&nbsp;</small>"),
        icon = icon("capsules"),
        color = "red",
        width = 3
      )
    })
    
    # Value Box 4: Proportion of Patient on Antibiotics (MODIFIED - moved from position 3)
    output$proportion_antibiotics <- renderValueBox({
      data <- data_reactive(); req(data)
      
      total_registered <- n_distinct(data$data_patients$`Unique Patient ID`)
      
      total_abx <- data$data_patients %>%
        filter(
          `Antimicrobial prescribed/administered` == "YES",
          AWaRe %in% AWaRe_abx
        ) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      rate <- if(total_registered > 0) round(100 * total_abx / total_registered, 1) else 0
      
      valueBox(
        value = paste0(rate, "%"),
        subtitle = HTML("Proportion of Patient<br>on Antibiotics"),
        icon = icon("percentage"),
        color = "yellow",
        width = 3
      )
    })
    
    # Helper function for specialty abbreviations
    create_abbreviation <- function(specialty_name) {
      # Remove common words and create abbreviation
      specialty_clean <- specialty_name %>%
        str_remove_all("(?i)\\b(or|and|the|of|in|at|to|for|with|by|department)\\b") %>%
        str_replace_all("-", " ") %>%
        str_split("\\s+") %>%
        unlist() %>%
        .[. != ""] %>%
        str_sub(1, 1) %>%
        toupper() %>%
        paste0(collapse = "")
      
      # If abbreviation is too short, use different strategy
      if (nchar(specialty_clean) < 2) {
        main_word <- str_extract(specialty_name, "\\b\\w{3,}\\b")
        if (!is.na(main_word)) {
          specialty_clean <- toupper(str_sub(main_word, 1, 4))
        } else {
          specialty_clean <- toupper(str_sub(specialty_name, 1, 4))
        }
      }
      
      # Limit to maximum 6 characters for readability
      if (nchar(specialty_clean) > 6) {
        specialty_clean <- str_sub(specialty_clean, 1, 6)
      }
      
      return(specialty_clean)
    }
    
    # Combined Chart: Registered and Treated Patients by Specialty (COMBINED VERSION)
    output$specialty_combined_chart <- renderPlotly({
      data <- data_reactive(); req(data)
      
      # 1️⃣ Summarize registered patients per specialty
      registered_summary <- data$data_patients %>%
        group_by(`Type of specialty`) %>%
        summarise(
          Registered = n_distinct(`Unique Patient ID`),
          .groups = "drop"
        )
      
      # 2️⃣ Summarize treated patients (on antimicrobials) per specialty
      treated_summary <- data$data_patients %>%
        filter(`Antimicrobial prescribed/administered` == "YES") %>%
        group_by(`Type of specialty`) %>%
        summarise(
          Treated = n_distinct(`Unique Patient ID`),
          .groups = "drop"
        )
      
      # 3️⃣ Combine both summaries
      specialty_summary <- registered_summary %>%
        left_join(treated_summary, by = "Type of specialty") %>%
        mutate(
          Treated = replace_na(Treated, 0)
        ) %>%
        pivot_longer(
          cols = c(Registered, Treated),
          names_to = "Type",
          values_to = "Count"
        ) %>%
        mutate(
          # Rename for cleaner display
          Type = case_when(
            Type == "Registered" ~ "Registered",
            Type == "Treated" ~ "Treated With Antimicrobials",
            TRUE ~ Type
          )
        )
      
      # 4️⃣ Create specialty abbreviations
      unique_specialties <- unique(specialty_summary$`Type of specialty`)
      specialty_abbreviations <- tibble(
        full_name = unique_specialties,
        abbreviated = map_chr(unique_specialties, create_abbreviation)
      )
      
      specialty_summary <- specialty_summary %>%
        left_join(specialty_abbreviations, by = c("Type of specialty" = "full_name")) %>%
        mutate(
          abbreviated = ifelse(is.na(abbreviated), `Type of specialty`, abbreviated),
          specialty_display = abbreviated,
          specialty_original = `Type of specialty`
        )
      
      # Main plot
      p <- ggplot(specialty_summary, aes(
        x = specialty_display, y = Count, fill = Type,
        text = paste0("Specialty: ", specialty_original, "\n",
                      "Type: ", Type, "\n",
                      "Patients: ", scales::comma(Count))
      )) +
        geom_bar(stat = "identity", position = "dodge", width = 0.68) +
        geom_text(aes(label = scales::comma(Count)),
                  position = position_dodge(width = 0.68),
                  vjust = -0.5, size = 2.2) +
        scale_fill_manual(values = c("Registered" = "#3498db", "Treated With Antimicrobials" = "#D35442")) +
        labs(
          x = NULL,
          y = "Patients",
          fill = NULL
        ) +
        theme_minimal(base_size = 8.5) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6),
          axis.text.y = element_text(size = 7),
          axis.title = element_text(size = 8),
          legend.position = "bottom"
        )
      
      # Abbreviation legend
      specialty_legend_data <- specialty_summary %>%
        select(specialty_original, abbreviated) %>%
        distinct() %>%
        arrange(specialty_original) %>%
        mutate(display_item = paste0("<b>", abbreviated, ":</b> ", specialty_original))
      
      legend_items <- specialty_legend_data$display_item
      legend_rows <- split(legend_items, ceiling(seq_along(legend_items) / 2))
      legend_text <- paste(sapply(legend_rows, function(row) paste(row, collapse = "     ")), collapse = "<br>")
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Specialty Distribution</b><br><span style='font-size:11px;'>Patients registered vs. patients on antimicrobials</span>",
            x = 0.5, 
            xanchor = "center",
            y = 0.92,
            yanchor = "top",
            font = list(size = 15)
          ),
          height = 320,
          margin = list(t = 70, r = 8, b = 100, l = 45),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.22,
            font = list(size = 7.5)
          ),
          xaxis = list(title = list(text = "")),
          yaxis = list(
            automargin = TRUE,
            title = list(font = list(size = 8))
          ),
          annotations = list(
            list(
              text = paste0("<b>Abbreviations:</b><br>", legend_text),
              showarrow = FALSE,
              xref = "paper", yref = "paper",
              x = 0.5, y = -0.38,
              xanchor = "center", yanchor = "top",
              font = list(size = 7)
            )
          )
        )
    })
    
    # -------------------------
    # Antimicrobial Prescription Outputs (AWaRe summary boxes)
    # -------------------------
    
    output$facility_wide_summary <- renderUI({
      data <- data_reactive(); req(data)
      data_patients <- data$data_patients
      
      # Filter for eligible patients - EXACT baseline logic
      filtered_data_patients <- data_patients %>%
        filter(
          `Diagnosis code` %in% infection_types,
          `Patient age group` == "ADULT",
          `Antimicrobial prescribed/administered` == "YES"
        )
      
      # Facility-wide data
      facility_data_full <- data_patients %>%
        filter(AWaRe %in% AWaRe_abx)
      
      facility_data_filtered <- filtered_data_patients %>%
        filter(AWaRe %in% AWaRe_abx)
      
      total_patients <- n_distinct(facility_data_full$`Unique Patient ID`)
      qi_patients <- n_distinct(facility_data_filtered$`Unique Patient ID`)
      total_prescriptions <- facility_data_full %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      qi_prescriptions <- facility_data_filtered %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      
      # AWaRe counts - full dataset - EXACT baseline logic
      access_patients_full <- facility_data_full %>%
        filter(grepl("Access", AWaRe, ignore.case = TRUE)) %>%
        summarise(n = n_distinct(`Unique Patient ID`)) %>%
        pull(n)
      watch_patients_full <- facility_data_full %>%
        filter(grepl("Watch", AWaRe, ignore.case = TRUE)) %>%
        summarise(n = n_distinct(`Unique Patient ID`)) %>%
        pull(n)
      reserve_patients_full <- facility_data_full %>%
        filter(grepl("Reserve", AWaRe, ignore.case = TRUE)) %>%
        summarise(n = n_distinct(`Unique Patient ID`)) %>%
        pull(n)
      not_rec_patients_full <- facility_data_full %>%
        filter(grepl("Not recommended", AWaRe, ignore.case = TRUE)) %>%
        summarise(n = n_distinct(`Unique Patient ID`)) %>%
        pull(n)
      unclass_patients_full <- facility_data_full %>%
        filter(AWaRe == "UNCLASSIFIED" | is.na(AWaRe)) %>%
        summarise(n = n_distinct(`Unique Patient ID`)) %>%
        pull(n)
      
      # AWaRe counts - eligible dataset
      access_patients_qi <- facility_data_filtered %>%
        filter(grepl("Access", AWaRe, ignore.case = TRUE)) %>%
        summarise(n = n_distinct(`Unique Patient ID`)) %>%
        pull(n)
      watch_patients_qi <- facility_data_filtered %>%
        filter(grepl("Watch", AWaRe, ignore.case = TRUE)) %>%
        summarise(n = n_distinct(`Unique Patient ID`)) %>%
        pull(n)
      reserve_patients_qi <- facility_data_filtered %>%
        filter(grepl("Reserve", AWaRe, ignore.case = TRUE)) %>%
        summarise(n = n_distinct(`Unique Patient ID`)) %>%
        pull(n)
      not_rec_patients_qi <- facility_data_filtered %>%
        filter(grepl("Not recommended", AWaRe, ignore.case = TRUE)) %>%
        summarise(n = n_distinct(`Unique Patient ID`)) %>%
        pull(n)
      unclass_patients_qi <- facility_data_filtered %>%
        filter(AWaRe == "UNCLASSIFIED" | is.na(AWaRe)) %>%
        summarise(n = n_distinct(`Unique Patient ID`)) %>%
        pull(n)
      
      # Prescription counts - full dataset
      access_prescriptions_full <- facility_data_full %>%
        filter(grepl("Access", AWaRe, ignore.case = TRUE)) %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      watch_prescriptions_full <- facility_data_full %>%
        filter(grepl("Watch", AWaRe, ignore.case = TRUE)) %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      reserve_prescriptions_full <- facility_data_full %>%
        filter(grepl("Reserve", AWaRe, ignore.case = TRUE)) %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      not_rec_prescriptions_full <- facility_data_full %>%
        filter(grepl("Not recommended", AWaRe, ignore.case = TRUE)) %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      unclass_prescriptions_full <- facility_data_full %>%
        filter(AWaRe == "UNCLASSIFIED" | is.na(AWaRe)) %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      
      # Prescription counts - eligible dataset
      access_prescriptions_qi <- facility_data_filtered %>%
        filter(grepl("Access", AWaRe, ignore.case = TRUE)) %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      watch_prescriptions_qi <- facility_data_filtered %>%
        filter(grepl("Watch", AWaRe, ignore.case = TRUE)) %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      reserve_prescriptions_qi <- facility_data_filtered %>%
        filter(grepl("Reserve", AWaRe, ignore.case = TRUE)) %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      not_rec_prescriptions_qi <- facility_data_filtered %>%
        filter(grepl("Not recommended", AWaRe, ignore.case = TRUE)) %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      unclass_prescriptions_qi <- facility_data_filtered %>%
        filter(AWaRe == "UNCLASSIFIED" | is.na(AWaRe)) %>%
        distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
        nrow()
      
      column(width = 10, offset = 1,
             box(
               title = "Facility-Wide",
               status = "primary",
               solidHeader = TRUE,
               width = 12,
               class = "ward-summary-box",
               div(style = "padding: 8px;",
                   div(style = "margin-bottom: 8px;",
                       div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
                           span(strong("Patients on antibiotics:"), style = "font-size: 12px;"),
                           span(total_patients, style = "font-size: 13px; font-weight: bold;")
                       ),
                       div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
                           span(strong("QI-Eligible patients on antibiotics:"), style = "font-size: 12px;"),
                           span(qi_patients, style = "font-size: 13px; font-weight: bold; color: #e74c3c;")
                       ),
                       div(style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
                           span(strong("Total antibiotic prescriptions (Rx):"), style = "font-size: 12px;"),
                           span(total_prescriptions, style = "font-size: 13px; font-weight: bold;")
                       ),
                       div(style = "display: flex; justify-content: space-between;",
                           span(strong("Total QI-Eligible antibiotic prescriptions (QI-Eligible Rx):"), style = "font-size: 12px;"),
                           span(qi_prescriptions, style = "font-size: 13px; font-weight: bold; color: #e74c3c;")
                       )
                   ),
                   hr(style = "margin: 6px 0;"),
                   div(style = "font-size: 11px;",
                       div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
                           span(strong("Category"), style = "width: 20%;"),
                           span(strong("Total Patients"), style = "width: 20%; text-align: center;"),
                           span(strong("QI-Eligible Patients"), style = "width: 20%; text-align: center;"),
                           span(strong("Total Rx"), style = "width: 20%; text-align: center;"),
                           span(strong("QI-Eligible Rx"), style = "width: 20%; text-align: center;")
                       ),
                       div(style = "display: flex; justify-content: space-between; margin-bottom: 2px; padding: 3px; background-color: #1b9e77; border-radius: 3px;",
                           span("Access", style = "width: 20%; color: #fff; font-size: 11px; font-weight: bold;"),
                           span(access_patients_full, style = "width: 20%; text-align: center; color: #fff;"),
                           span(access_patients_qi, style = "width: 20%; text-align: center; font-weight: bold; color: #fff;"),
                           span(access_prescriptions_full, style = "width: 20%; text-align: center; color: #fff;"),
                           span(access_prescriptions_qi, style = "width: 20%; text-align: center; font-weight: bold; color: #fff;")
                       ),
                       div(style = "display: flex; justify-content: space-between; margin-bottom: 2px; padding: 3px; background-color: #ff7f00; border-radius: 3px;",
                           span("Watch", style = "width: 20%; color: #fff; font-size: 11px; font-weight: bold;"),
                           span(watch_patients_full, style = "width: 20%; text-align: center; color: #fff;"),
                           span(watch_patients_qi, style = "width: 20%; text-align: center; font-weight: bold; color: #fff;"),
                           span(watch_prescriptions_full, style = "width: 20%; text-align: center; color: #fff;"),
                           span(watch_prescriptions_qi, style = "width: 20%; text-align: center; font-weight: bold; color: #fff;")
                       ),
                       div(style = "display: flex; justify-content: space-between; margin-bottom: 2px; padding: 3px; background-color: #e41a1c; border-radius: 3px;",
                           span("Reserve", style = "width: 20%; color: #fff; font-size: 11px; font-weight: bold;"),
                           span(reserve_patients_full, style = "width: 20%; text-align: center; color: #fff;"),
                           span(reserve_patients_qi, style = "width: 20%; text-align: center; font-weight: bold; color: #fff;"),
                           span(reserve_prescriptions_full, style = "width: 20%; text-align: center; color: #fff;"),
                           span(reserve_prescriptions_qi, style = "width: 20%; text-align: center; font-weight: bold; color: #fff;")
                       ),
                       div(style = "display: flex; justify-content: space-between; margin-bottom: 2px; padding: 3px; background-color: #8c510a; border-radius: 3px;",
                           span("Not Recommended", style = "width: 20%; color: #fff; font-size: 11px; font-weight: bold;"),
                           span(not_rec_patients_full, style = "width: 20%; text-align: center; color: #fff;"),
                           span(not_rec_patients_qi, style = "width: 20%; text-align: center; font-weight: bold; color: #fff;"),
                           span(not_rec_prescriptions_full, style = "width: 20%; text-align: center; color: #fff;"),
                           span(not_rec_prescriptions_qi, style = "width: 20%; text-align: center; font-weight: bold; color: #fff;")
                       ),
                       div(style = "display: flex; justify-content: space-between; padding: 3px; background-color: #b3b3b3; border-radius: 3px;",
                           span("Unclassified", style = "width: 20%; color: #000; font-size: 11px; font-weight: bold;"),
                           span(unclass_patients_full, style = "width: 20%; text-align: center;"),
                           span(unclass_patients_qi, style = "width: 20%; text-align: center; font-weight: bold;"),
                           span(unclass_prescriptions_full, style = "width: 20%; text-align: center;"),
                           span(unclass_prescriptions_qi, style = "width: 20%; text-align: center; font-weight: bold;")
                       )
                   )
               )
             )
      )
    })
    
    # Specialty Summary Boxes
    output$specialty_summary_boxes <- renderUI({
      data <- data_reactive(); req(data)
      data_patients <- data$data_patients
      
      filtered_data_patients <- data_patients %>%
        filter(
          `Diagnosis code` %in% infection_types,
          `Patient age group` == "ADULT",
          `Antimicrobial prescribed/administered` == "YES"
        )
      
      all_specialties <- unique(data_patients$`Type of specialty`[!is.na(data_patients$`Type of specialty`)])
      box_list <- list()
      
      for (specialty in all_specialties) {
        specialty_data_full <- data_patients %>%
          filter(`Type of specialty` == specialty, AWaRe %in% AWaRe_abx)
        
        specialty_data_filtered <- filtered_data_patients %>%
          filter(`Type of specialty` == specialty, AWaRe %in% AWaRe_abx)
        
        total_patients <- n_distinct(specialty_data_full$`Unique Patient ID`)
        qi_patients <- n_distinct(specialty_data_filtered$`Unique Patient ID`)
        total_prescriptions <- specialty_data_full %>%
          distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
          nrow()
        qi_prescriptions <- specialty_data_filtered %>%
          distinct(`Unique Patient ID`, `Antimicrobial name`) %>%
          nrow()
        
        # Calculate AWaRe counts - EXACT baseline logic
        access_patients_full  <- specialty_data_full  %>% filter(grepl("Access", AWaRe, ignore.case = TRUE))  %>% summarise(n = n_distinct(`Unique Patient ID`)) %>% pull(n)
        watch_patients_full   <- specialty_data_full  %>% filter(grepl("Watch",  AWaRe, ignore.case = TRUE))  %>% summarise(n = n_distinct(`Unique Patient ID`)) %>% pull(n)
        reserve_patients_full <- specialty_data_full  %>% filter(grepl("Reserve",AWaRe, ignore.case = TRUE))  %>% summarise(n = n_distinct(`Unique Patient ID`)) %>% pull(n)
        not_rec_patients_full <- specialty_data_full  %>% filter(grepl("Not recommended", AWaRe, ignore.case = TRUE))%>% summarise(n = n_distinct(`Unique Patient ID`)) %>% pull(n)
        unclass_patients_full <- specialty_data_full  %>% filter(AWaRe == "UNCLASSIFIED" | is.na(AWaRe)) %>% summarise(n = n_distinct(`Unique Patient ID`)) %>% pull(n)
        
        access_patients_qi  <- specialty_data_filtered %>% filter(grepl("Access", AWaRe, ignore.case = TRUE))  %>% summarise(n = n_distinct(`Unique Patient ID`)) %>% pull(n)
        watch_patients_qi   <- specialty_data_filtered %>% filter(grepl("Watch",  AWaRe, ignore.case = TRUE))  %>% summarise(n = n_distinct(`Unique Patient ID`)) %>% pull(n)
        reserve_patients_qi <- specialty_data_filtered %>% filter(grepl("Reserve",AWaRe, ignore.case = TRUE))  %>% summarise(n = n_distinct(`Unique Patient ID`)) %>% pull(n)
        not_rec_patients_qi <- specialty_data_filtered %>% filter(grepl("Not recommended", AWaRe, ignore.case = TRUE)) %>% summarise(n = n_distinct(`Unique Patient ID`)) %>% pull(n)
        unclass_patients_qi <- specialty_data_filtered %>% filter(AWaRe == "UNCLASSIFIED" | is.na(AWaRe)) %>% summarise(n = n_distinct(`Unique Patient ID`)) %>% pull(n)
        
        access_prescriptions_full  <- specialty_data_full  %>% filter(grepl("Access", AWaRe, ignore.case = TRUE))  %>% distinct(`Unique Patient ID`, `Antimicrobial name`) %>% nrow()
        watch_prescriptions_full   <- specialty_data_full  %>% filter(grepl("Watch",  AWaRe, ignore.case = TRUE))  %>% distinct(`Unique Patient ID`, `Antimicrobial name`) %>% nrow()
        reserve_prescriptions_full <- specialty_data_full  %>% filter(grepl("Reserve",AWaRe, ignore.case = TRUE))  %>% distinct(`Unique Patient ID`, `Antimicrobial name`) %>% nrow()
        not_rec_prescriptions_full <- specialty_data_full  %>% filter(grepl("Not recommended", AWaRe, ignore.case = TRUE)) %>% distinct(`Unique Patient ID`, `Antimicrobial name`) %>% nrow()
        unclass_prescriptions_full <- specialty_data_full  %>% filter(AWaRe == "UNCLASSIFIED" | is.na(AWaRe)) %>% distinct(`Unique Patient ID`, `Antimicrobial name`) %>% nrow()
        
        access_prescriptions_qi  <- specialty_data_filtered %>% filter(grepl("Access", AWaRe, ignore.case = TRUE))  %>% distinct(`Unique Patient ID`, `Antimicrobial name`) %>% nrow()
        watch_prescriptions_qi   <- specialty_data_filtered %>% filter(grepl("Watch",  AWaRe, ignore.case = TRUE))  %>% distinct(`Unique Patient ID`, `Antimicrobial name`) %>% nrow()
        reserve_prescriptions_qi <- specialty_data_filtered %>% filter(grepl("Reserve",AWaRe, ignore.case = TRUE))  %>% distinct(`Unique Patient ID`, `Antimicrobial name`) %>% nrow()
        not_rec_prescriptions_qi <- specialty_data_filtered %>% filter(grepl("Not recommended", AWaRe, ignore.case = TRUE)) %>% distinct(`Unique Patient ID`, `Antimicrobial name`) %>% nrow()
        unclass_prescriptions_qi <- specialty_data_filtered %>% filter(AWaRe == "UNCLASSIFIED" | is.na(AWaRe)) %>% distinct(`Unique Patient ID`, `Antimicrobial name`) %>% nrow()
        
        box_list[[specialty]] <- box(
          title = specialty, status = "primary", solidHeader = TRUE,
          width = 6, collapsible = TRUE, collapsed = TRUE,
          class = "ward-summary-box",
          div(style = "padding: 8px;",
              div(style = "margin-bottom: 6px;",
                  div(style = "display: flex; justify-content: space-between; margin-bottom: 3px;",
                      span(strong("Patients on antibiotics:"), style = "font-size: 11px;"),
                      span(total_patients, style = "font-size: 12px; font-weight: bold;")
                  ),
                  div(style = "display: flex; justify-content: space-between; margin-bottom: 3px;",
                      span(strong("QI-Eligible patients on antibiotics:"), style = "font-size: 11px;"),
                      span(qi_patients, style = "font-size: 12px; font-weight: bold; color: #e74c3c;")
                  ),
                  div(style = "display: flex; justify-content: space-between; margin-bottom: 3px;",
                      span(strong("Total antibiotic prescriptions (Rx):"), style = "font-size: 11px;"),
                      span(total_prescriptions, style = "font-size: 12px; font-weight: bold;")
                  ),
                  div(style = "display: flex; justify-content: space-between;",
                      span(strong("Total QI-Eligible antibiotic prescriptions (QI-Eligible Rx):"), style = "font-size: 11px;"),
                      span(qi_prescriptions, style = "font-size: 12px; font-weight: bold; color: #e74c3c;")
                  )
              ),
              hr(style = "margin: 6px 0;"),
              div(style = "font-size: 10.5px;",
                  div(style = "display: flex; justify-content: space-between; margin-bottom: 3px;",
                      span(strong("Category"), style = "width: 20%;"),
                      span(strong("Total Patients"), style = "width: 20%; text-align: center;"),
                      span(strong("QI-Eligible Patients"), style = "width: 20%; text-align: center;"),
                      span(strong("Total Rx"), style = "width: 20%; text-align: center;"),
                      span(strong("QI-Eligible Rx"), style = "width: 20%; text-align: center;")
                  ),
                  div(style = "display: flex; justify-content: space-between; margin-bottom: 2px; padding: 2px; background-color: #1b9e77;",
                      span("Access", style = "width: 20%; color: #fff;"),
                      span(access_patients_full, style = "width: 20%; text-align: center; color:#fff;"),
                      span(access_patients_qi, style = "width: 20%; text-align: center; font-weight: bold; color:#fff;"),
                      span(access_prescriptions_full, style = "width: 20%; text-align: center; color:#fff;"),
                      span(access_prescriptions_qi, style = "width: 20%; text-align: center; font-weight: bold; color:#fff;")
                  ),
                  div(style = "display: flex; justify-content: space-between; margin-bottom: 2px; padding: 2px; background-color: #ff7f00;",
                      span("Watch", style = "width: 20%; color: #fff;"),
                      span(watch_patients_full, style = "width: 20%; text-align: center; color:#fff;"),
                      span(watch_patients_qi, style = "width: 20%; text-align: center; font-weight: bold; color:#fff;"),
                      span(watch_prescriptions_full, style = "width: 20%; text-align: center; color:#fff;"),
                      span(watch_prescriptions_qi, style = "width: 20%; text-align: center; font-weight: bold; color:#fff;")
                  ),
                  div(style = "display: flex; justify-content: space-between; margin-bottom: 2px; padding: 2px; background-color: #e41a1c;",
                      span("Reserve", style = "width: 20%; color: #fff;"),
                      span(reserve_patients_full, style = "width: 20%; text-align: center; color:#fff;"),
                      span(reserve_patients_qi, style = "width: 20%; text-align: center; font-weight: bold; color:#fff;"),
                      span(reserve_prescriptions_full, style = "width: 20%; text-align: center; color:#fff;"),
                      span(reserve_prescriptions_qi, style = "width: 20%; text-align: center; font-weight: bold; color:#fff;")
                  ),
                  div(style = "display: flex; justify-content: space-between; margin-bottom: 2px; padding: 2px; background-color: #8c510a;",
                      span("Not Recommended", style = "width: 20%; color: #fff;"),
                      span(not_rec_patients_full, style = "width: 20%; text-align: center; color:#fff;"),
                      span(not_rec_patients_qi, style = "width: 20%; text-align: center; font-weight: bold; color:#fff;"),
                      span(not_rec_prescriptions_full, style = "width: 20%; text-align: center; color:#fff;"),
                      span(not_rec_prescriptions_qi, style = "width: 20%; text-align: center; font-weight: bold; color:#fff;")
                  ),
                  div(style = "display: flex; justify-content: space-between; padding: 2px; background-color: #b3b3b3;",
                      span("Unclassified", style = "width: 20%; color: #000;"),
                      span(unclass_patients_full, style = "width: 20%; text-align: center;"),
                      span(unclass_patients_qi, style = "width: 20%; text-align: center; font-weight: bold;"),
                      span(unclass_prescriptions_full, style = "width: 20%; text-align: center;"),
                      span(unclass_prescriptions_qi, style = "width: 20%; text-align: center; font-weight: bold;")
                  )
              )
          )
        )
      }
      
      do.call(tagList, lapply(seq(1, length(box_list), by = 2), function(i) {
        fluidRow(box_list[i:min(i+1, length(box_list))])
      }))
    })
    
    # -------------------------
    # Antibiotics by Condition Plot (EXACT R MARKDOWN MATCH)
    # -------------------------
    output$antibiotics_diagnosis_plot <- renderPlotly({
      data <- data_reactive(); req(data)
      data_patients <- data$data_patients
      
      # AWaRe stacking and legend order - EXACT baseline
      aware_levels_stack <- c("UNCLASSIFIED", "NOT RECOMMENDED", "RESERVE", "WATCH", "ACCESS")
      aware_levels_legend <- c("ACCESS", "WATCH", "RESERVE", "NOT RECOMMENDED", "UNCLASSIFIED")
      
      # All combinations
      all_combos <- expand_grid(
        `Diagnosis code` = names(diagnostic_labels),
        AWaRe = aware_levels_stack
      )
      
      # Summarise prescriptions - EXACT R MARKDOWN LOGIC
      # This counts ROWS (prescriptions), not unique patients
      aware_summary <- data_patients %>%
        filter(`Diagnosis code` %in% names(diagnostic_labels)) %>%
        filter(!is.na(AWaRe), !is.na(`Diagnosis code`)) %>%
        group_by(`Diagnosis code`, AWaRe) %>%
        summarise(Prescriptions = n(), .groups = "drop") %>%  # Count rows = prescriptions
        right_join(all_combos, by = c("Diagnosis code", "AWaRe")) %>%
        mutate(Prescriptions = replace_na(Prescriptions, 0)) %>%
        group_by(`Diagnosis code`) %>%
        mutate(
          Total = sum(Prescriptions),
          Percentage = if_else(Total > 0, 100 * Prescriptions / Total, 0)
        ) %>%
        ungroup() %>%
        mutate(
          FriendlyLabel = diagnostic_labels[`Diagnosis code`],
          FriendlyLabel = factor(FriendlyLabel, levels = diagnostic_labels)
        )
      
      # Set factor levels for stacking
      aware_summary$AWaRe <- factor(aware_summary$AWaRe, levels = aware_levels_stack)
      
      # Integer counts per diagnosis - EXACT R Markdown logic
      aware_summary <- aware_summary %>%
        group_by(`Diagnosis code`) %>%
        group_modify(~ {
          df <- .x
          dt <- max(df$Total, na.rm = TRUE)
          raw <- df$Percentage / 100 * dt
          base <- floor(raw)
          remainder <- dt - sum(base)
          if (remainder > 0) {
            frac_order <- order(-(raw - base))
            base[frac_order[seq_len(remainder)]] <- base[frac_order[seq_len(remainder)]] + 1L
          }
          df$Count <- as.integer(base)
          df$diag_total <- dt
          df
        }) %>%
        ungroup()
      
      # Labels and hover
      aware_summary <- aware_summary %>%
        mutate(
          PlotLabel = FriendlyLabel,
          PercentagePct = Percentage,
          hover_text = paste0(
            "<b>Condition:</b> ", as.character(PlotLabel), "<br>",
            "<b>AWaRe Category:</b> ", as.character(AWaRe), "<br>",
            "<b>Count:</b> ", Count, "<br>",
            "<b>Percentage:</b> ", round(PercentagePct, 1), "%"
          )
        )
      
      # Plot order - matching R Markdown
      plot_order <- diagnostic_labels
      aware_summary$PlotLabel <- factor(aware_summary$PlotLabel, levels = plot_order)
      
      # n= labels (total prescriptions per condition)
      label_data <- aware_summary %>%
        distinct(`Diagnosis code`, PlotLabel, diag_total)
      
      # Dynamic buffer
      max_digits <- max(nchar(as.character(label_data$diag_total)), na.rm = TRUE)
      y_buffer <- max(0.06, 0.03 + 0.035 * max_digits)
      ylim_max <- min(1 + y_buffer, 1.5)
      label_y <- 1 + y_buffer * 0.48
      
      # ggplot - theme matches second code exactly
      p <- ggplot(
        aware_summary,
        aes(x = fct_rev(PlotLabel), y = PercentagePct / 100, fill = AWaRe, text = hover_text)
      ) +
        geom_bar(stat = "identity", position = "fill", width = 0.85) +
        geom_text(
          aes(label = ifelse(Percentage >= 5, paste0(round(PercentagePct), "%"), "")),
          position = position_fill(vjust = 0.5),
          size = 3,
          color = "black"
        ) +
        geom_text(
          data = label_data,
          aes(x = PlotLabel, label = paste0("n=", formatC(diag_total, format = "d", big.mark = ","))),
          y = label_y,
          inherit.aes = FALSE,
          size = 3,
          color = "gray30",
          vjust = 0.5,
          hjust = 0
        ) +
        scale_fill_manual(
          breaks = aware_levels_legend,
          values = c(
            "ACCESS" = "#1b9e77",
            "WATCH" = "#ff7f00",
            "RESERVE" = "#e41a1c",
            "NOT RECOMMENDED" = "#8c510a",
            "UNCLASSIFIED" = "gray70"
          ),
          drop = FALSE
        ) +
        coord_flip(ylim = c(0, ylim_max), expand = FALSE) +
        labs(
          x = "Condition",
          y = "Proportion of Prescriptions",
          fill = "AWaRe Category"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = 7),
          axis.title = element_text(face = "bold", size = 10),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(size = 8),
          plot.margin = margin(6, 18, 6, 8)
        ) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "top",
                                   override.aes = list(fill = c("#1b9e77", "#ff7f00", "#e41a1c", "#8c510a", "gray70"))))
      
      # Convert to plotly - layout matches second code; only standoff added for y-axis title
      x_margin <- 40 + round(300 * y_buffer)
      
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Use of Antibiotics by Condition (AWaRe Classification)</b>",
            x = 0.5,
            xanchor = "center",
            y = 0.98,
            yanchor = "top",
            font = list(size = 11)
          ),
          height = 450,
          width = 680,
          margin = list(l = 30, r = x_margin, t = 60, b = 170),  # same as second code
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.40,
            yanchor = "top",
            font = list(size = 10),
            title = list(text = "<b>AWaRe Category</b>", font = list(size = 10)),
            traceorder = "normal"
          ),
          bargap = 0,
          bargroupgap = 0
        ) %>%
        layout(
          xaxis = list(automargin = TRUE, categoryorder = "array", categoryarray = rev(plot_order)),
          yaxis = list(
            automargin = TRUE,
            title = list(
              text = "<b>Condition</b>",
              standoff = 10,                 # pushes "Condition" away from tick labels
              font = list(size = 14)         # same size/bold as second code's axis.title
            )
          )
        )
      
      # Reorder legend traces
      legend_order_map <- setNames(seq_along(aware_levels_legend), aware_levels_legend)
      plt$x$data <- plt$x$data[order(sapply(plt$x$data, function(trace) {
        legend_order_map[trace$name]
      }))]
      
      plt
    })
    
    # -------------------------
    # Antibiotics by Diagnosis & Specialty Heatmap (WITHOUT Facility-Wide)
    # -------------------------
    output$antibiotics_heatmap_plot <- renderPlotly({
      data <- data_reactive(); req(data)
      data_patients <- data$data_patients
      
      # AWaRe levels and colors - matching baseline
      aware_levels <- c("Access", "Watch", "Reserve")
      color_mapping <- c("Access" = "#1b9e77", "Watch" = "#ff7f00", "Reserve" = "#e41a1c")
      
      diagnostic_lookup <- tibble(
        `Diagnosis code` = names(diagnostic_labels),
        FriendlyLabel = unname(diagnostic_labels)
      )
      
      # Total patients per diagnosis - EXACT baseline
      infection_totals <- data_patients %>%
        filter(`Diagnosis code` %in% names(diagnostic_labels)) %>%
        group_by(`Diagnosis code`) %>%
        summarise(Total_Infection_Patients = n_distinct(`Unique Patient ID`), .groups = "drop") %>%
        right_join(diagnostic_lookup, by = "Diagnosis code") %>%
        mutate(Total_Infection_Patients = replace_na(Total_Infection_Patients, 0))
      
      # Specialty-level data only (no Facility-Wide)
      specialty_data <- data_patients %>%
        filter(`Diagnosis code` %in% names(diagnostic_labels)) %>%
        mutate(
          Antibiotic_Type = case_when(
            AWaRe == "ACCESS" ~ "Access",
            AWaRe == "WATCH" ~ "Watch",
            AWaRe == "RESERVE" ~ "Reserve",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(Antibiotic_Type)) %>%
        group_by(`Type of specialty`, `Diagnosis code`, Antibiotic_Type) %>%
        summarise(Count = n_distinct(`Unique Patient ID`), .groups = "drop")
      
      # Use only specialty data (no Facility-Wide)
      heatmap_data <- specialty_data
      
      # All combinations
      all_combos <- expand_grid(
        `Type of specialty` = unique(heatmap_data$`Type of specialty`),
        `Diagnosis code` = names(diagnostic_labels),
        Antibiotic_Type = aware_levels
      )
      
      # Merge and fill 0s
      heatmap_data <- all_combos %>%
        left_join(heatmap_data, by = c("Type of specialty", "Diagnosis code", "Antibiotic_Type")) %>%
        mutate(Count = replace_na(Count, 0))
      
      # Add friendly labels
      heatmap_data <- heatmap_data %>%
        left_join(infection_totals, by = "Diagnosis code") %>%
        mutate(
          FriendlyLabel_n = paste0(FriendlyLabel, "\n(n=", Total_Infection_Patients, ")"),
          FriendlyLabel_n = factor(
            FriendlyLabel_n,
            levels = paste0(
              unname(diagnostic_labels),
              "\n(n=", infection_totals$Total_Infection_Patients[
                match(names(diagnostic_labels), infection_totals$`Diagnosis code`)
              ], ")"
            )
          ),
          Antibiotic_Type = factor(Antibiotic_Type, levels = aware_levels),
          Label = ifelse(Count > 0, as.character(Count), "")
        )
      
      # GET UNIQUE SPECIALTIES AND CREATE ABBREVIATIONS
      unique_specialties <- unique(heatmap_data$`Type of specialty`)
      specialty_abbreviations <- tibble(
        full_name = unique_specialties,
        abbreviated = map_chr(unique_specialties, create_abbreviation)
      )
      
      # Apply abbreviations - simplified without Facility-Wide special handling
      heatmap_data <- heatmap_data %>%
        left_join(specialty_abbreviations, by = c("Type of specialty" = "full_name")) %>%
        mutate(
          abbreviated = ifelse(is.na(abbreviated), `Type of specialty`, abbreviated),
          specialty_display = abbreviated,
          specialty_original = `Type of specialty`,
          specialty_display = factor(specialty_display, levels = unique(specialty_display))
        )
      
      max_count <- max(heatmap_data$Count, na.rm = TRUE)
      
      # Plot
      p <- ggplot(
        heatmap_data,
        aes(x = specialty_display, y = FriendlyLabel_n,
            fill = Antibiotic_Type,
            text = paste0(
              "Specialty: ", specialty_original, "\n",
              "Infection: ", gsub("\n.*", "", FriendlyLabel_n), "\n",
              "AWaRe: ", Antibiotic_Type, "\n",
              "Patients: ", Count
            ))
      ) +
        geom_tile(aes(alpha = Count), color = "white", linewidth = 0.55) +
        geom_text(aes(label = Label), size = 2.2, fontface = "bold", color = "black") +
        facet_wrap(~Antibiotic_Type, ncol = 3) +
        scale_fill_manual(
          values = color_mapping, 
          name = "AWaRe Category", 
          drop = FALSE,
          guide = "none"
        ) +
        scale_alpha_continuous(limits = c(0, max_count), range = c(0.12, 1), guide = "none") +
        labs(x = "Specialty", y = "Diagnosis Group") +
        theme_minimal(base_size = 9) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6.5),
          axis.text.y = element_text(size = 6.5, face = "bold"),
          strip.background = element_rect(fill = "black", color = "black"),
          strip.text = element_text(color = "white", face = "bold", size = 9.5),
          panel.grid = element_blank(),
          legend.position = "none"
        )
      
      # CREATE SPECIALTY LEGEND TABLE - 3 ITEMS PER ROW FORMAT
      specialty_legend_data <- heatmap_data %>%
        select(specialty_original, abbreviated) %>%
        distinct() %>%
        arrange(specialty_original) %>%
        mutate(display_item = paste0("<b>", abbreviated, ":</b> ", specialty_original))
      
      # Split into groups of 3 for better visibility
      legend_items <- specialty_legend_data$display_item
      legend_rows <- split(legend_items, ceiling(seq_along(legend_items) / 3))
      legend_text <- paste(sapply(legend_rows, function(row) paste(row, collapse = " | ")), collapse = "<br>")
      
      # CREATE AWaRe CATEGORIES LEGEND with colored boxes
      aware_legend_html <- paste0(
        "<b>AWaRe Categories:</b>  ",
        "<span style='font-size:25px; color:#1b9e77'>&#9632;</span> Access  ",
        "<span style='font-size:25px; color:#ff7f00'>&#9632;</span> Watch  ",
        "<span style='font-size:25px; color:#e41a1c'>&#9632;</span> Reserve"
      )
      
      # Convert to plotly
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Patient Distribution by Diagnostic Group and Specialty</b>",
            x = 0.5,
            xanchor = "center",
            y = 0.95,
            yanchor = "top",
            font = list(size = 11)
          ),
          height = 520,
          margin = list(t = 80, r = 10, b = 220, l = 60),
          xaxis = list(automargin = TRUE),
          yaxis = list(automargin = TRUE),
          annotations = list(
            # AWaRe Categories legend
            list(
              text = aware_legend_html,
              showarrow = FALSE,
              xref = "paper", yref = "paper",
              x = 0.5, y = -0.22,
              xanchor = "center", yanchor = "top",
              font = list(size = 11)
            ),
            # Specialty abbreviations
            list(
              text = paste0("<b>Specialty Abbreviations:</b><br>", legend_text),
              showarrow = FALSE,
              xref = "paper", yref = "paper",
              x = 0.5, y = -0.38,
              xanchor = "center", yanchor = "top",
              font = list(size = 8)
            )
          )
        )
      
      plt
    })
    
  })
}