# ENT Analysis Shiny Module - Complete Implementation
# This module contains all ENT-specific analysis functionality

# Overview Tab UI
entOutpatientOverviewUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Back to Clinical Conditions Button
    fluidRow(
      column(12,
             div(
               style = "text-align: right; margin-bottom: 10px;",
               actionButton(
                 ns("back_to_eligible"), 
                 HTML("← Back to Conditions"),
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
            Shiny.setInputValue('navigate_to_eligibility', Math.random(), {priority: 'event'});
            window.scrollTo({top: 0, behavior: 'smooth'});
          ")
             )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == false",
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📤 Upload Required", status = "warning", solidHeader = TRUE,
                   div(class = "error-box",
                       h4("⚠️ No Data Uploaded"),
                       p("Please upload your patient data file to begin the WHO AWaRe QIs analysis."),
                       p("Go to the 'Data Upload' tab to upload your Excel file.")
                   )
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      # Initial Eligible Cases Check
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "🔍 Initial Eligible Cases Check", status = "primary", solidHeader = TRUE,
                   htmlOutput(ns("overview_eligibility_feedback"))
               )
        )
      ),
      
      # Key Insights
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📊 Key Insights", status = "warning", solidHeader = TRUE,
                   htmlOutput(ns("summary_insights_cards"))
               )
        )
      ),
      
      # WHO AWaRe QIs sections
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "🎯 AWaRe Quality Indicators for ENT Infections", status = "info", solidHeader = TRUE,
                   h5("Within the gPPS data structure, the following antibiotic use quality indicator has been identified:"),
                   p("1) Proportion of patients with any ear/sinus/throat infection (not pneumonia) given an oral antibiotic."),
                   div(class = "note-box",
                       style = "background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 12px; margin-top: 10px;",
                       strong("🔍 WHO AWaRe Book Recommendation:"), br(), br(),
                       strong("For acute otitis media:"),
                       tags$ul(
                         tags$li("Amoxicillin 500 mg every 8 hours (ORAL)"),
                         tags$li("Amoxicillin+clavulanic acid 500 mg+125 mg every 8 hours (ORAL)")
                       ),
                       strong("For Acute sinusitis:"),
                       tags$ul(
                         tags$li("Amoxicillin 1 g every 8 hours (ORAL)"),
                         tags$li("Amoxicillin+clavulanic acid 500 mg+125 mg every 8 hours (ORAL)")
                       ),
                       strong("For Pharyngitis:"),
                       tags$ul(
                         tags$li("Amoxicillin 1 g every 8 hours (ORAL)"),
                         tags$li("Phenoxymethylpenicillin 500 mg (800 000 IU) every 6 hours (ORAL)"),
                         tags$li("Cefalexin 500 mg every 8 hours (ORAL)"),
                         tags$li("Clarithromycin 500 mg every 12 hours (ORAL)")
                       ),
                       strong("Duration:"),
                       tags$ul(
                         tags$li("5 days")
                       )
                   )
               )
        )
      ),
      
      # Patient-Level and Prescription-Level Insights
      fluidRow(
        box(width = 6, title = "👥 Patient-Level Insights", status = "info", solidHeader = TRUE, collapsed = TRUE, collapsible = TRUE,
            DT::dataTableOutput(ns("patient_level_table"))
        ),
        box(width = 6, title = "📑 Prescription-Level Insights", status = "success", solidHeader = TRUE, collapsed = TRUE, collapsible = TRUE,
            DT::dataTableOutput(ns("prescription_level_table"))
        )
      )
    )
  )
}

# Oral Antibiotic Use Tab UI
entOutpatientOralUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Back Button
    fluidRow(
      column(12,
             div(
               style = "text-align: right; margin-bottom: 10px;",
               actionButton(
                 ns("back_to_eligible"), 
                 HTML("← Back to Conditions"),
                 class = "btn btn-primary fixed-button",
                 style = "background-color: #3498db; border-color: #3498db; color: white; font-size: 12px !important; font-weight: 500; border-radius: 6px; padding: 3px 8px !important; min-width: auto; width: auto; line-height: 1.2;",
                 onclick = "Shiny.setInputValue('navigate_to_eligibility', Math.random(), {priority: 'event'}); window.scrollTo({top: 0, behavior: 'smooth'});"
               )
             )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == false",
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📤 Upload Required", status = "warning", solidHeader = TRUE,
                   p("Please upload your data files to view oral antibiotic use analysis.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📌 Indicator 1 - Oral Antibiotic Use", status = "primary", solidHeader = TRUE,
                   p("Proportion of patients with any ear/sinus/throat infection (not pneumonia) given an oral antibiotic.")
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📈 Antibiotic Use by Route of Administration", status = "primary", solidHeader = TRUE,
                   p("This visual summarises the proportion of adult outpatients with ENT infections given antibiotics by route of administration across specialties."),
                   div(style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                       withSpinner(plotlyOutput(ns("oral_plot"), height = "450px", width = "100%"), type = 4))
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📝 Summary of Antibiotic Use for ENT Infections by Route of Administration", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   htmlOutput(ns("oral_summary"))
               )
        )
      )
    )
  )
}

# Module Server - Handles all ENT tabs
entOutpatientAnalysisServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Constants
    AWaRe_abx <- c("ACCESS", "WATCH", "RESERVE", "NOT RECOMMENDED", "UNCLASSIFIED")
    
    # Helper function to check if data is available
    check_data <- function() {
      data <- data_reactive()
      return(!is.null(data) && !is.null(data$data_patients))
    }
    
    # Output to control conditional panels
    output$dataUploaded <- reactive({
      check_data()
    })
    outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
    
    # Eligibility feedback
    generate_eligibility_feedback <- function() {
      tryCatch({
        if (!check_data()) {
          return(HTML("<div style='background-color: #fff3cd; border: 1px solid #ffeeba; padding: 15px; border-radius: 5px;'><p><strong>⚠️ No data available</strong></p><p>Please upload your data files to check eligible cases.</p></div>"))
        }
        
        data <- data_reactive()
        data_patients <- data$data_patients
        
        # Filter eligible ENT infections patients
        data_ENT <- data_patients %>%
          filter(`Diagnosis code` == "ENT") %>%
          mutate(
            Route = toupper(as.character(Route)),
            AWaRe_compatible = (`Patient age group` == "ADULT")
          ) 
        
        # Count eligible unique ENT cases
        eligible_ENT_n <- data_ENT %>%
          filter(AWaRe_compatible) %>%
          distinct(`Unique Patient ID`) %>%
          nrow()
        
        # Build status message
        status_message <- if(eligible_ENT_n == 0) {
          "<div style='background-color:#fff3cd; border: 1px solid #ffeeba; padding: 10px; border-radius: 3px; margin-top: 10px;'>
            <strong>🚫 No eligible cases found:</strong> There were no eligible cases for evaluation during this survey period. Please verify data availability.
          </div>"
        } else if(eligible_ENT_n < 10) {
          "<div style='background-color:#ffe0e0; border: 1px solid #ffb3b3; padding: 10px; border-radius: 3px; margin-top: 10px;'>
            <strong>⚠️ Caution:</strong> Few eligible cases detected. Interpret results with caution.
          </div>"
        } else {
          "<div style='background-color:#e0ffe0; border: 1px solid #b3ffb3; padding: 10px; border-radius: 3px; margin-top: 10px;'>
            <strong>✅ Good to go!</strong> Sufficient eligible cases available to proceed with full evaluation.
          </div>"
        }
        
        # Build HTML feedback
        html_feedback <- HTML(paste0(
          "<div style='background-color: #f0f8ff; border: 1px solid #add8e6; padding: 15px; border-radius: 5px; font-family: sans-serif;'>",
          "<p style='margin-bottom: 10px;'>",
          "This script applies <strong>WHO AWaRe Quality Indicators</strong> to adult outpatients with ENT infections.",
          "</p>",
          "<ul>",
          "<li><strong>gPPS recorded symptoms:</strong> Ear pain OR Ear discharge OR Sore throat OR Sneezing/nasal congestion/runny or stuffy nose</li>",
          "<li><strong>Total eligible cases:</strong> ", eligible_ENT_n, "</li>",
          "</ul>",
          status_message,
          "</div>"
        ))
        
        return(html_feedback)
        
      }, error = function(e) {
        return(HTML(paste0("<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 15px; border-radius: 5px;'><p><strong>❌ Error loading eligibility information:</strong></p><p>", as.character(e$message), "</p></div>")))
      })
    }
    
    # Eligibility feedback output
    output$overview_eligibility_feedback <- renderUI({
      generate_eligibility_feedback()
    })
    
    # Summary Insights Cards
    output$summary_insights_cards <- renderUI({
      if (!check_data()) {
        return(HTML("<p>No data available for insights</p>"))
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      
      # Calculate totals
      total_ENT <- data_patients %>%
        filter(`Diagnosis code` == "ENT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_ENT_adults <- data_patients %>%
        filter(`Diagnosis code` == "ENT", `Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_ENT_adults_abx <- data_patients %>%
        filter(`Diagnosis code` == "ENT", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_ENT_adults_prescriptions <- data_patients %>%
        filter(`Diagnosis code` == "ENT", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        nrow()
      
      insight_cards <- HTML(glue::glue(
        "<div style='display: flex; gap: 12px; margin-top: 15px; flex-wrap: wrap;'>
        
        <div style='flex: 1; min-width: 250px; background-color: #e6f4ea; border-left: 6px solid #009E73; padding: 12px; border-radius: 5px;'>
          <strong>Eligible ENT Patients (Adults):</strong><br>
          <span style='font-size: 1.3em;'>{total_ENT_adults}</span><br>
          <small>{round(100 * total_ENT_adults / total_ENT, 1)}% (of {total_ENT} ENT patients)</small>
        </div>
        
        <div style='flex: 1; min-width: 250px; background-color: #fff3cd; border-left: 6px solid #D55E00; padding: 12px; border-radius: 5px;'>
          <strong>Treatment Rate (Abx):</strong><br>
          <span style='font-size: 1.3em;'>{round(100 * total_ENT_adults_abx / total_ENT_adults, 1)}%</span><br>
          <small>({total_ENT_adults_abx} of {total_ENT_adults} eligible ENT patients)</small>
        </div>
        
        <div style='flex: 1; min-width: 250px; background-color: #f0f8ff; border-left: 6px solid #0072B2; padding: 12px; border-radius: 5px;'>
          <strong>Total Antibiotic Prescriptions:</strong><br>
          <span style='font-size: 1.3em;'>{total_ENT_adults_prescriptions}</span>
        </div>
        
        </div>"
      ))
      
      return(insight_cards)
    })
    
    # Patient Level Table
    output$patient_level_table <- DT::renderDataTable({
      if (!check_data()) {
        return(DT::datatable(data.frame(Message = "No data available"), options = list(dom = 't'), rownames = FALSE))
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      
      total_patients <- data_patients %>%
        filter(`Diagnosis code` == "ENT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      patient_summary <- data.frame(
        Category = c(
          "Number of all patients with a diagnosis of ENT infections",
          "Number of adult patients (≥18 years) with ENT infections",
          "Number of eligible patients: Adult patients with ENT infections who received antibiotics"
        ),
        Count = c(
          total_patients,
          data_patients %>%
            filter(`Diagnosis code` == "ENT", `Patient age group` == "ADULT") %>%
            distinct(`Unique Patient ID`) %>%
            nrow(),
          data_patients %>%
            filter(`Diagnosis code` == "ENT", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
            distinct(`Unique Patient ID`) %>%
            nrow()
        )
      ) %>%
        mutate(
          Percent = sprintf("%.1f%%", 100 * Count / total_patients)
        )
      
      DT::datatable(
        patient_summary,
        colnames = c("Patient Category", "Number of Patients", "Proportion of All Patients"),
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      )
    })
    
    # Prescription Level Table
    output$prescription_level_table <- DT::renderDataTable({
      if (!check_data()) {
        return(DT::datatable(data.frame(Message = "No data available"), options = list(dom = 't'), rownames = FALSE))
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      
      total_prescriptions <- data_patients %>%
        filter(`Diagnosis code` == "ENT") %>%
        nrow()
      
      prescription_summary <- data.frame(
        Category = c(
          "Number of all antibiotic prescriptions for patients diagnosed with ENT infections",
          "Number of antibiotic prescriptions for adult patients with ENT infections",
          "Number of eligible antibiotic prescriptions"
        ),
        Count = c(
          total_prescriptions,
          data_patients %>% filter(`Diagnosis code` == "ENT", `Patient age group` == "ADULT") %>% nrow(),
          data_patients %>% filter(`Diagnosis code` == "ENT", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>% nrow()
        )
      ) %>%
        mutate(
          Percent = sprintf("%.1f%%", 100 * Count / total_prescriptions)
        )
      
      DT::datatable(
        prescription_summary,
        colnames = c("Prescription Category", "Number of Prescriptions", "Proportion of All Prescriptions"),
        options = list(pageLength = 10, dom = 't'),
        rownames = FALSE
      )
    })
    
    # Oral antibiotic use data
    oral_data_reactive <- reactive({
      if (!check_data()) {
        return(list())
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      
      # Pre-clean data
      data_ENT <- data_patients %>%
        filter(`Diagnosis code` == "ENT", `Patient age group` == "ADULT") %>%
        mutate(
          Route = toupper(Route),
          `Antimicrobial name` = trimws(toupper(`Antimicrobial name`)),
          AWaRe = toupper(AWaRe),
          `Type of specialty` = trimws(`Type of specialty`)
        )
      
      # Patient-level summary
      ENT_oral_summary <- data_ENT %>%
        group_by(`Unique Patient ID`, `Type of specialty`) %>%
        summarise(
          Any_Oral = any(Route == "O"),
          Any_IV_IM = any(Route %in% c("P", "IM")),
          Any_Other = any(!Route %in% c("O", "P", "IM")),
          Eligible_AWaRe = any(`Patient age group` == "ADULT" & AWaRe %in% AWaRe_abx),
          .groups = "drop"
        ) %>%
        filter(Eligible_AWaRe) %>%
        select(-Eligible_AWaRe)
      
      # Summary by specialty
      oral_summary_by_spec <- ENT_oral_summary %>%
        group_by(`Type of specialty`) %>%
        summarise(
          Patients = n(),
          Oral_Abx = sum(Any_Oral),
          IV_IM_Abx = sum(Any_IV_IM),
          Other_Abx = sum(Any_Other),
          Pct_Oral_Abx = round(100 * Oral_Abx / Patients, 1),
          Pct_IV_IM_Abx = round(100 * IV_IM_Abx / Patients, 1),
          Pct_Other_Abx = round(100 * Other_Abx / Patients, 1),
          .groups = "drop"
        )
      
      # Facility-Wide totals
      oral_summary_total <- ENT_oral_summary %>%
        summarise(
          `Type of specialty` = "Facility-Wide",
          Patients = n(),
          Oral_Abx = sum(Any_Oral),
          IV_IM_Abx = sum(Any_IV_IM),
          Other_Abx = sum(Any_Other),
          Pct_Oral_Abx = round(100 * Oral_Abx / Patients, 1),
          Pct_IV_IM_Abx = round(100 * IV_IM_Abx / Patients, 1),
          Pct_Other_Abx = round(100 * Other_Abx / Patients, 1)
        )
      
      # Combine
      oral_summary_final <- bind_rows(oral_summary_total, oral_summary_by_spec)
      
      list(
        data_ENT = data_ENT,
        oral_summary_final = oral_summary_final
      )
    })
    
    # Oral summary
    output$oral_summary <- renderUI({
      oral_data <- oral_data_reactive()
      if(length(oral_data) == 0) {
        return(HTML("<p>No data available</p>"))
      }
      
      oral_summary_final <- oral_data$oral_summary_final
      data_ENT <- oral_data$data_ENT
      
      # Denominator
      total_eligible <- data_ENT %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_eligible_with_abx <- data_ENT %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      intro_text <- glue::glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received any antibiotic for ENT infections (<strong>{total_eligible_with_abx}</strong> out of {total_eligible}).
        </div><br><br>
        <strong>Summary:</strong><br><br>"
      )
      
      # Format blocks
      oral_blocks <- oral_summary_final %>%
        mutate(
          block = pmap_chr(list(
            `Type of specialty`, Patients,
            Oral_Abx, Pct_Oral_Abx,
            IV_IM_Abx, Pct_IV_IM_Abx,
            Other_Abx, Pct_Other_Abx
          ), function(dept, n, abx_n, abx_pct, iv_n, iv_pct, other_n, other_pct) {
            color <- if (dept == "Facility-Wide") "#0072B2" else "#6c757d"
            bg <- if (dept == "Facility-Wide") "#f0f0f0" else "#ffffff"
            
            glue::glue(
              "<div style='background-color: {bg}; border-left: 5px solid {color}; padding: 14px; margin-bottom: 20px;'>
              
              <strong>🏥 {dept}</strong> <span style='color: #888;'>(n = {n} patients)</span><br><br>
              
              <ul style='margin-left: 1.2em; line-height: 1.8; padding-left: 0; list-style-type: none;'>
                <li>
                  <span style='display: inline-block; background-color: #007bff; color: white; border-radius: 50%; width: 22px; height: 22px; text-align: center; line-height: 22px; font-size: 12px; margin-right: 8px;'>1</span>
                  <strong>Given any oral antibiotic:</strong> {abx_pct}% ({abx_n} of {n})
                </li>
                <li>
                  <span style='display: inline-block; background-color: #dc3545; color: white; border-radius: 50%; width: 22px; height: 22px; text-align: center; line-height: 22px; font-size: 12px; margin-right: 8px;'>2</span>
                  <strong>Given any IV/IM antibiotic:</strong> {iv_pct}% ({iv_n} of {n})
                </li>
                <li>
                  <span style='display: inline-block; background-color: #6f42c1; color: white; border-radius: 50%; width: 22px; height: 22px; text-align: center; line-height: 22px; font-size: 12px; margin-right: 8px;'>3</span>
                  <strong>Given any antibiotic via other routes:</strong> {other_pct}% ({other_n} of {n})
                </li>
              </ul>
              </div>"
            )
          })
        )
      
      final_html <- htmltools::HTML(paste0(intro_text, paste(oral_blocks$block, collapse = "\n")))
      htmltools::browsable(final_html)
    })
    
    # Oral plot - NO GRAY HIGHLIGHT
    output$oral_plot <- renderPlotly({
      oral_data <- oral_data_reactive()
      if(length(oral_data) == 0) {
        return(plotly_empty())
      }
      
      oral_summary_final <- oral_data$oral_summary_final
      
      # Prepare long-format data
      oral_summary_long <- oral_summary_final %>%
        select(`Type of specialty`, Patients, Oral_Abx, IV_IM_Abx, Other_Abx) %>%
        pivot_longer(
          cols = c(Oral_Abx, IV_IM_Abx, Other_Abx),
          names_to = "Antibiotic_Type",
          values_to = "Count"
        ) %>%
        mutate(
          Antibiotic_Type = recode(Antibiotic_Type,
                                   Oral_Abx = "Oral",
                                   IV_IM_Abx = "IV/IM",
                                   Other_Abx = "Others"
          ),
          Percent = round(100 * Count / Patients, 1),
          Antibiotic_Type = factor(Antibiotic_Type, levels = c("Oral", "IV/IM", "Others")),
          hover_text = paste0(
            "<b>Specialty:</b> ", `Type of specialty`, "<br>",
            "<b>Route:</b> ", Antibiotic_Type, "<br>",
            "<b>Patients:</b> ", Count, "<br>",
            "<b>Proportion:</b> ", Percent, "%"
          )
        )
      
      # Reorder specialty
      oral_summary_long <- oral_summary_long %>%
        mutate(
          `Type of specialty` = factor(
            `Type of specialty`,
            levels = c("Facility-Wide", sort(setdiff(unique(`Type of specialty`), "Facility-Wide")))
          ),
          LabelColor = ifelse(`Type of specialty` == "Facility-Wide", "#0072B2", "black")
        )
      
      # Colors
      custom_palette <- c(
        "Oral" = "#0072B2",
        "IV/IM" = "#3399ff",
        "Others" = "#6F42C1"
      )
      
      label_colors <- setNames(oral_summary_long$LabelColor, oral_summary_long$`Type of specialty`)
      
      # Total counts
      total_counts <- oral_summary_long %>%
        group_by(`Type of specialty`) %>%
        summarise(Total = sum(Count), .groups = "drop")
      
      # Bar plot
      p <- ggplot(oral_summary_long, aes(x = `Type of specialty`, y = Percent, fill = Antibiotic_Type, text = hover_text)) +
        geom_bar(stat = "identity", position = "stack", width = 0.7) +
        geom_text(
          aes(label = ifelse(Percent > 3, paste0(Percent, "%"), "")),
          position = position_stack(vjust = 0.5),
          color = "white",
          size = 2.6,
          fontface = "bold"
        ) +
        geom_text(
          data = total_counts,
          aes(x = `Type of specialty`, y = 103, label = paste0("n = ", Total)),
          inherit.aes = FALSE,
          size = 3,
          fontface = "bold",
          color = "gray30"
        ) +
        scale_fill_manual(values = custom_palette) +
        scale_y_continuous(
          labels = function(x) paste0(x, "%"),
          limits = c(0, 108),
          expand = expansion(mult = c(0, 0))
        ) +
        labs(
          title = NULL,
          x = NULL,
          y = "Percentage of Patients",
          fill = "Antibiotic Type"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.x = element_text(
            angle = 30,
            hjust = 1,
            face = "bold",
            size = 7,
            color = label_colors[levels(oral_summary_long$`Type of specialty`)]
          ),
          axis.text.y = element_text(size = 7),
          axis.title.y = element_text(face = "bold", size = 10),
          panel.grid.major.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(size = 8),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          plot.margin = margin(6, 8, 6, 8)
        ) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "top"))
      
      # Convert to plotly - NO GRAY HIGHLIGHT SHAPE
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Proportion of Patients with ENT Infections by Antibiotic Route</b>",
            x = 0.5,
            xanchor = "center",
            y = 0.98,
            yanchor = "top",
            font = list(size = 12)
          ),
          height = 450,
          width = 680,
          margin = list(l = 30, r = 30, t = 60, b = 180),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.40,
            yanchor = "top",
            font = list(size = 10),
            title = list(text = "<b>Antibiotic Type</b>", font = list(size = 10))
          ),
          bargap = 0,
          bargroupgap = 0
        )
      
      plt
    })
    
  })
}