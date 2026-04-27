# Bloody Diarrhoea Analysis Shiny Module - Complete Implementation
# File: diarrhoea_module.R

# ========== TAB 1: OVERVIEW UI ==========
bloodyDiarrhoeaOverviewUI <- function(id) {
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
      
      # WHO AWaRe QIs section
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "🎯 AWaRe Quality Indicators for Bloody Diarrhoea", status = "info", solidHeader = TRUE,
                   h5("Within the gPPS data structure, the following antibiotic use quality indicators have been identified:"),
                   p("1) Proportion of all patients presenting with acute bloody infectious diarrhoea given oral antibiotics."),
                   p("2) Proportion of patients with acute bloody infectious diarrhoea given oral antibiotics by AWaRe category (Access or Watch).")
                   
                     )
                   ) 
                
               ),
      
      # WHO AWaRe book Recommendations Box
      fluidRow(
        column(10, offset = 1,
               box(
                 width = 12, 
                 title = "🔍 WHO AWaRe book Recommendation:",
                 strong("For Significant acute bloody diarrhoea:"),
                 status = "primary", 
                 solidHeader = TRUE,
                 tags$ul(
                   tags$li("Ciprofloxacin 500 mg every 12 hours (ORAL) for 3 days"),
                   tags$li("Azithromycin Day 1: 500 mg once a day (ORAL) OR Day 2-4: 250 mg once a day for 4 days"),
                   tags$li("Cefixime 400 mg once a day (ORAL) for 3 days"),
                   tags$li("Sulfamethoxazole+trimethoprim 800 mg +160 mg every 12 hours (ORAL) for 5 days"),
                   tags$li("Ceftriaxone 1 g once a day (IV/IM) for 3 days")
                 )
               )
        )
      )
    )
  )
}

# ========== TAB 2: ORAL ANTIBIOTIC USE UI ==========
bloodyDiarrhoeaOralUI <- function(id) {
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
                   p("Please upload your data files to view bloody diarrhoea oral antibiotic analysis.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      
      # Indicator Title
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📌 Indicator 1 - Oral Antibiotic Use for Acute Bloody Diarrhoea", 
                   status = "primary", solidHeader = TRUE,
                   p("Proportion of all patients presenting with acute infectious bloody diarrhoea given an oral antibiotic.")
                   
               )
        )
      ),
      
      # Plot
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📈 Antibiotic Use by Route of Administration", 
                   status = "primary", solidHeader = TRUE,
                   p("This visual summarises the proportion of adult outpatients with acute infectious bloody diarrhoea given antibiotics by route of administration across specialties."),
                   div(
                     style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden; background-color: white;",
                     withSpinner(plotlyOutput(ns("bloody_oral_plot"), height = "450px", width = "100%"), type = 4)
                   )
               )
        )
      ),
      
      # Summary
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📝 Summary of Antibiotic Use for Bloody Diarrhoea by Route of Administration", status = "success", solidHeader = TRUE, 
                   collapsible = TRUE, collapsed = TRUE,
                   htmlOutput(ns("bloody_oral_summary"))
               )
        )
      )
    )
  )
}

# ========== TAB 3: AWaRe CATEGORY UI ==========
bloodyDiarrhoeaAWaReUI <- function(id) {
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
                   p("Please upload your data files to view bloody diarrhoea AWaRe category analysis.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      
      # Indicator Title
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📌 Indicator 2 - Oral Antibiotic Use by AWaRe for Acute Bloody Diarrhoea", 
                   status = "primary", solidHeader = TRUE,
                   p("Proportion of patients with acute bloody infectious diarrhoea given oral antibiotics by AWaRe category (Access or Watch).")
                   
               )
        )
      ),
      
      # Plot
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📈 Antibiotic Prescription by AWaRe Classification", 
                   status = "primary", solidHeader = TRUE,
                   p("This visual summarises the proportion of adult outpatients with acute infectious bloody diarrhoea given antibiotics by AWaRe classification across specialties."),
                   div(
                     style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden; background-color: white;",
                     withSpinner(plotlyOutput(ns("bloody_aware_plot"), height = "450px", width = "100%"), type = 4)
                   )
               )
        )
      ),
      
      # Note Box
      fluidRow(
        column(10, offset = 1,
               div(class = "info-box",
                   style = "background-color: #f8f9fa; padding: 15px; border-left: 5px solid #17a2b8; border-radius: 5px; margin-top: 15px;",
                   strong("💡 Note:"),
                   tags$ul(
                     tags$li("This count in this visual represents the number of unique patients who were prescribed at least one antibiotic within a specific WHO AWaRe category (Access, Watch, or Reserve) during their encounter. A patient is counted once for each distinct AWaRe category they received an antibiotic from."),
                     tags$li("The AWaRe classification divides antibiotics into three categories: Access, Watch, and Reserve. Access antibiotics are first-line treatments with a low risk of resistance, typically recommended for common infections. Watch antibiotics are associated with a higher risk of resistance and require careful monitoring. Reserve antibiotics are reserved for critical cases involving resistant infections, typically used only when other options are ineffective.")
                   )
               )
        )
      ),
      # Summary
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📝 Summary of Antibiotic Use for Acute Bloody Diarrhoea by AWaRe classification", status = "success", solidHeader = TRUE, 
                   collapsible = TRUE, collapsed = TRUE,
                   htmlOutput(ns("bloody_aware_summary"))
               )
        )
      )
      
      
    )
  )
}

# ========== MODULE SERVER ==========
bloodyDiarrhoeaModuleServer <- function(id, data_reactive) {
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
    
    # ========== OVERVIEW TAB OUTPUTS ==========
    
    # Eligibility feedback
    output$overview_eligibility_feedback <- renderUI({
      tryCatch({
        if (!check_data()) {
          return(HTML("<div style='background-color: #fff3cd; border: 1px solid #ffeeba; padding: 15px; border-radius: 5px;'><p><strong>⚠️ No data available</strong></p><p>Please upload your data files to check eligible cases.</p></div>"))
        }
        
        data <- data_reactive()
        data_patients <- data$data_patients
        
        # Filter eligible cases
        eligible_counts <- data_patients %>%
          filter(`Diagnosis code` == "B_DIA") %>%
          mutate(AWaRe_compatible = (`Patient age group` == "ADULT")) %>%
          filter(AWaRe_compatible) %>%
          summarise(Eligible_n = n_distinct(`Unique Patient ID`))
        
        n_cases <- eligible_counts$Eligible_n
        
        status_block <- if (n_cases == 0) {
          "<div style='background-color:#fff3cd; border: 1px solid #ffeeba; padding: 10px; border-radius: 3px; margin-top: 10px;'>
            <strong>🚫 No data available:</strong> There were no eligible cases for evaluation during this survey period.
          </div>"
        } else if (n_cases < 10) {
          "<div style='background-color:#ffe0e0; border: 1px solid #ffb3b3; padding: 10px; border-radius: 3px; margin-top: 10px;'>
            <strong>⚠️ Caution:</strong> Few eligible cases detected. Interpret results with caution.
          </div>"
        } else {
          "<div style='background-color:#e0ffe0; border: 1px solid #b3ffb3; padding: 10px; border-radius: 3px; margin-top: 10px;'>
            <strong>✅ Good to go!</strong> Sufficient eligible cases available to proceed with full evaluation.
          </div>"
        }
        
        feedback <- glue::glue("
          <div style='background-color: #f0f8ff; border: 1px solid #add8e6; padding: 15px; border-radius: 5px; font-family: sans-serif;'>
            <p>This script applies <strong>WHO AWaRe Quality Indicators</strong> to adult outpatients with <strong>Bloody diarrhoea</strong></p>
            <ul><li><strong>Total eligible cases:</strong> {n_cases}</li></ul>
            {status_block}
          </div>
        ")
        
        return(HTML(feedback))
        
      }, error = function(e) {
        return(HTML(paste0("<div style='background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 15px; border-radius: 5px;'><p><strong>❌ Error loading eligibility information:</strong></p><p>", as.character(e$message), "</p></div>")))
      })
    })
    
    # Summary Insights Cards
    output$summary_insights_cards <- renderUI({
      if (!check_data()) {
        return(HTML("<p>No data available for insights</p>"))
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      
      total_diag <- data_patients %>%
        filter(`Diagnosis code` == "B_DIA") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_diag_adults <- data_patients %>%
        filter(`Diagnosis code` == "B_DIA", `Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_diag_adults_abx <- data_patients %>%
        filter(`Diagnosis code` == "B_DIA", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_diag_adults_prescriptions <- data_patients %>%
        filter(`Diagnosis code` == "B_DIA", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        nrow()
      
      pct_adults_of_all <- if (total_diag > 0) round(100 * total_diag_adults / total_diag, 1) else 0
      pct_treated_rate <- if (total_diag_adults > 0) round(100 * total_diag_adults_abx / total_diag_adults, 1) else 0
      
      html_summary <- glue::glue("
        <div style='display:flex; flex-direction:column; gap: 12px; margin-top: 15px;'>
          <div style='display: flex; gap: 12px;'>
            <div style='flex: 1; background-color: #e6f4ea; border-left: 6px solid #009E73; padding: 12px; border-radius: 5px;'>
              <strong>Eligible Patients (Adults):</strong><br>
              <span style='font-size: 1.25em;'>{total_diag_adults}</span><br>
              <small>{pct_adults_of_all}% (of {total_diag} patients with bloody diarrhoea)</small>
            </div>
            <div style='flex: 1; background-color: #fff3cd; border-left: 6px solid #D55E00; padding: 12px; border-radius: 5px;'>
              <strong>Treatment Rate (Abx):</strong><br>
              <span style='font-size: 1.25em;'>{pct_treated_rate}%</span><br>
              <small>({total_diag_adults_abx} of {total_diag_adults} eligible adults)</small>
            </div>
            <div style='flex: 1; background-color: #f0f8ff; border-left: 6px solid #0072B2; padding: 12px; border-radius: 5px;'>
              <strong>Total Antibiotic Prescriptions:</strong><br>
              <span style='font-size: 1.25em;'>{total_diag_adults_prescriptions}</span>
            </div>
          </div>
        </div>
      ")
      
      return(HTML(html_summary))
    })
    
    # ========== ORAL ANTIBIOTIC USE TAB ==========
    
    # Bloody Oral data
    bloody_oral_data_reactive <- reactive({
      if (!check_data()) {
        return(list())
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      
      # Pre-clean data
      data_DIARRHEA_B <- data_patients %>%
        filter(`Diagnosis code` == "B_DIA", `Patient age group` == "ADULT") %>%
        mutate(
          Route = toupper(Route),
          `Antimicrobial name` = trimws(toupper(`Antimicrobial name`)),
          AWaRe = toupper(AWaRe),
          `Type of specialty` = trimws(`Type of specialty`)
        )
      
      # Patient-level summary
      oral_summary <- data_DIARRHEA_B %>%
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
      oral_summary_by_spec <- oral_summary %>%
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
      oral_summary_total <- oral_summary %>%
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
        data_DIARRHEA_B = data_DIARRHEA_B,
        oral_summary_final = oral_summary_final
      )
    })
    
    # Bloody Oral summary
    output$bloody_oral_summary <- renderUI({
      oral_data <- bloody_oral_data_reactive()
      if(length(oral_data) == 0) {
        return(HTML("<p>No data available</p>"))
      }
      
      oral_summary_final <- oral_data$oral_summary_final
      data_DIARRHEA_B <- oral_data$data_DIARRHEA_B
      
      # Denominator
      total_eligible <- data_DIARRHEA_B %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_eligible_with_abx <- data_DIARRHEA_B %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      intro_text <- glue::glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients presenting with acute infectious bloody diarrhoea (<strong>{total_eligible_with_abx}</strong> out of {total_eligible}).
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
    
    # Bloody Oral plot - NO GRAY HIGHLIGHT
    output$bloody_oral_plot <- renderPlotly({
      oral_data <- bloody_oral_data_reactive()
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
            text = "<b>Proportion of Patients with Acute Bloody Diarrhoea by Antibiotic Route</b>",
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
    
    
    # ========== AWaRe CATEGORY TAB ==========
    
    # Bloody AWaRe data
    bloody_aware_data_reactive <- reactive({
      if (!check_data()) {
        return(list())
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      
      data_DIARRHEA_B <- data_patients %>%
        filter(`Diagnosis code` == "B_DIA", `Patient age group` == "ADULT") %>%
        mutate(
          Route = toupper(Route),
          `Antimicrobial name` = trimws(toupper(`Antimicrobial name`)),
          AWaRe = toupper(AWaRe),
          `Type of specialty` = trimws(`Type of specialty`)
        )
      
      # Expected AWaRe categories
      expected_aware <- c("Access", "Watch", "Reserve", "Not Recommended", "Unclassified")
      
      # Widen data
      aware_summary <- data_DIARRHEA_B %>%
        filter(!is.na(Route), Route == "O") %>%
        mutate(
          AWaRe_Cat = case_when(
            AWaRe == "ACCESS" ~ "Access",
            AWaRe == "WATCH" ~ "Watch",
            AWaRe == "RESERVE" ~ "Reserve",
            AWaRe == "NOT RECOMMENDED" ~ "Not Recommended",
            AWaRe == "UNCLASSIFIED" ~ "Unclassified",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(AWaRe_Cat)) %>%
        group_by(`Unique Patient ID`, `Type of specialty`, AWaRe_Cat) %>%
        summarise(.groups = "drop") %>%
        mutate(flag = TRUE) %>%
        pivot_wider(
          names_from = AWaRe_Cat,
          values_from = flag,
          values_fill = FALSE
        )
      
      # Ensure all columns exist
      for (col in expected_aware) {
        if (!col %in% colnames(aware_summary)) {
          aware_summary[[col]] <- FALSE
        }
      }
      
      # Summary by specialty
      aware_by_spec <- aware_summary %>%
        group_by(`Type of specialty`) %>%
        summarise(
          Patients = n(),
          Access = sum(Access),
          Watch = sum(Watch),
          Reserve = sum(Reserve),
          `Not Recommended` = sum(`Not Recommended`),
          Unclassified = sum(Unclassified),
          Pct_Access = round(100 * Access / Patients, 1),
          Pct_Watch = round(100 * Watch / Patients, 1),
          Pct_Reserve = round(100 * Reserve / Patients, 1),
          Pct_Not_Recommended = round(100 * `Not Recommended` / Patients, 1),
          Pct_Unclassified = round(100 * Unclassified / Patients, 1),
          .groups = "drop"
        )
      
      # Facility-Wide
      aware_total <- aware_summary %>%
        summarise(
          `Type of specialty` = "Facility-Wide",
          Patients = n(),
          Access = sum(Access),
          Watch = sum(Watch),
          Reserve = sum(Reserve),
          `Not Recommended` = sum(`Not Recommended`),
          Unclassified = sum(Unclassified),
          Pct_Access = round(100 * Access / Patients, 1),
          Pct_Watch = round(100 * Watch / Patients, 1),
          Pct_Reserve = round(100 * Reserve / Patients, 1),
          Pct_Not_Recommended = round(100 * `Not Recommended` / Patients, 1),
          Pct_Unclassified = round(100 * Unclassified / Patients, 1)
        )
      
      aware_final <- bind_rows(aware_total, aware_by_spec)
      
      list(
        data_DIARRHEA_B = data_DIARRHEA_B,
        aware_final = aware_final,
        aware_summary = aware_summary
      )
    })
    
    # Bloody AWaRe summary
    output$bloody_aware_summary <- renderUI({
      aware_data <- bloody_aware_data_reactive()
      if(length(aware_data) == 0) {
        return(HTML("<p>No data available</p>"))
      }
      
      aware_final <- aware_data$aware_final
      data_DIARRHEA_B <- aware_data$data_DIARRHEA_B
      
      # Denominator
      total_eligible2 <- data_DIARRHEA_B %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_eligible_with_abx2 <- data_DIARRHEA_B %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx, Route == "O") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      intro_text2 <- glue::glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received any oral antibiotic for acute infectious bloody diarrhoea (<strong>{total_eligible_with_abx2}</strong> out of {total_eligible2}).
        </div><br><br>
        <strong>Summary:</strong><br><br>"
      )
      
      # Build blocks
      aware_blocks <- aware_final %>%
        mutate(
          block = pmap_chr(list(
            `Type of specialty`, Patients,
            Access, Pct_Access,
            Watch, Pct_Watch,
            Reserve, Pct_Reserve,
            `Not Recommended`, Pct_Not_Recommended,
            Unclassified, Pct_Unclassified
          ), function(dept, n,
                      a_n, a_p,
                      w_n, w_p,
                      r_n, r_p,
                      nr_n, nr_p,
                      u_n, u_p) {
            
            color <- if (dept == "Facility-Wide") "#0072B2" else "#6c757d"
            bg <- if (dept == "Facility-Wide") "#f0f0f0" else "#ffffff"
            
            glue::glue(
              "<div style='background-color: {bg}; border-left: 5px solid {color}; padding: 14px; margin-bottom: 20px;'>
              <strong>🏥 {dept}</strong> <span style='color: #888;'>(n = {n} patients)</span><br><br>
              
              <ul style='margin-left: 1.2em; line-height: 1.8; padding-left: 0; list-style-type: none;'>
                <li><span style='display: inline-block; background-color: #1b9e77; color: white; border-radius: 50%; width: 22px; height: 22px; text-align: center;'>A</span>
                <strong>Access oral antibiotic:</strong> {a_p}% ({a_n} of {n})</li>
                
                <li><span style='display: inline-block; background-color: #ffc107; color: black; border-radius: 50%; width: 22px; height: 22px; text-align: center;'>Wa</span>
                <strong>Watch oral antibiotic:</strong> {w_p}% ({w_n} of {n})</li>
                
                <li><span style='display: inline-block; background-color: #dc3545; color: white; border-radius: 50%; width: 22px; height: 22px; text-align: center;'>Re</span>
                <strong>Reserve oral antibiotic:</strong> {r_p}% ({r_n} of {n})</li>
                
                <li><span style='display: inline-block; background-color: #8c510a; color: white; border-radius: 50%; width: 22px; height: 22px; text-align: center;'>N</span>
                <strong>Not Recommended oral antibiotic:</strong> {nr_p}% ({nr_n} of {n})</li>
                
                <li><span style='display: inline-block; background-color: #343a40; color: white; border-radius: 50%; width: 22px; height: 22px; text-align: center;'>U</span>
                <strong>Unclassified oral antibiotic:</strong> {u_p}% ({u_n} of {n})</li>
                
              </ul>
              </div>"
            )
          })
        )
      
      final2_html <- htmltools::HTML(paste0(intro_text2, paste(aware_blocks$block, collapse = "\n")))
      htmltools::browsable(final2_html)
    })
    
    
    # Bloody AWaRe plot - with visible rectangular border (matching provided image)
    output$bloody_aware_plot <- renderPlotly({
      aware_data <- bloody_aware_data_reactive()
      if (length(aware_data) == 0) {
        return(plotly_empty())
      }
      
      aware_final <- aware_data$aware_final
      
      aware_plot_data <- aware_final %>%
        select(`Type of specialty`, Access, Watch, Reserve, `Not Recommended`, Unclassified) %>%
        pivot_longer(
          cols = -`Type of specialty`,
          names_to = "AWaRe_Cat",
          values_to = "Count"
        ) %>%
        group_by(`Type of specialty`) %>%
        mutate(
          Total = sum(Count),
          Proportion = Count / Total,
          hover_text = paste0(
            "<b>Specialty:</b> ", `Type of specialty`, "<br>",
            "<b>AWaRe Category:</b> ", AWaRe_Cat, "<br>",
            "<b>Patients:</b> ", Count, "<br>",
            "<b>Proportion:</b> ", round(Proportion * 100, 1), "%"
          )
        ) %>%
        ungroup()
      
      aware_levels <- c("Access", "Watch", "Reserve", "Not Recommended", "Unclassified")
      aware_plot_data$AWaRe_Cat <- factor(aware_plot_data$AWaRe_Cat, levels = aware_levels)
      
      aware_plot_data <- aware_plot_data %>%
        mutate(
          PlotLabel = ifelse(
            `Type of specialty` == "Facility-Wide",
            "<b style='color:#0072B2;'>Facility-Wide</b>",
            `Type of specialty`
          )
        )
      
      aware_plot_data$PlotLabel <- factor(
        aware_plot_data$PlotLabel,
        levels = rev(c(
          "<b style='color:#0072B2;'>Facility-Wide</b>",
          sort(setdiff(unique(aware_plot_data$PlotLabel), "<b style='color:#0072B2;'>Facility-Wide</b>"))
        ))
      )
      
      aware_palette <- c(
        "Access" = "#1b9e77",
        "Watch" = "#ff7f00",
        "Reserve" = "#e41a1c",
        "Not Recommended" = "#8c510a",
        "Unclassified" = "gray70"
      )
      
      label_data <- aware_plot_data %>%
        group_by(PlotLabel) %>%
        summarise(Total = sum(Count), .groups = "drop")
      
      max_digits <- max(nchar(as.character(label_data$Total)), na.rm = TRUE)
      x_buffer <- max(0.06, 0.03 + 0.035 * max_digits)
      xlim_max <- min(1 + x_buffer, 1.5)
      label_x <- 1 + x_buffer * 0.48
      
      p <- ggplot(aware_plot_data, aes(y = PlotLabel, x = Proportion, fill = AWaRe_Cat, text = hover_text)) +
        geom_bar(stat = "identity", position = "fill", width = 0.85) +
        geom_text(
          aes(label = ifelse(Proportion > 0.05, paste0(round(Proportion * 100, 1), "%"), "")),
          position = position_fill(vjust = 0.5),
          size = 3, color = "black"
        ) +
        geom_text(
          data = label_data,
          aes(y = PlotLabel, label = paste0("n=", formatC(Total, format = "d", big.mark = ","))),
          x = label_x,
          inherit.aes = FALSE, size = 3, color = "gray30", vjust = 0.5, hjust = 0
        ) +
        scale_fill_manual(values = aware_palette, breaks = aware_levels, name = "AWaRe Category") +
        coord_cartesian(xlim = c(0, xlim_max), expand = FALSE) +
        labs(
          x = "Proportion of Patients",
          y = "Department"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.y = ggtext::element_markdown(size = 7),
          axis.text.x = element_text(size = 7),
          axis.title  = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text  = element_text(size = 8),
          plot.margin  = margin(6, 18, 6, 8)
        ) +
        guides(fill = guide_legend(nrow = 5, byrow = TRUE, title.position = "top")) +
        scale_y_discrete(limits = rev(levels(aware_plot_data$PlotLabel)))
      
      r_margin <- 40 + round(300 * x_buffer)
      
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Distribution of Oral Antibiotic Use in Acute Bloody Diarrhoea by AWaRe Category</b>",
            x = 0.5, xanchor = "center",
            y = 0.98, yanchor = "top",
            font = list(size = 11)
          ),
          height = 450,
          width  = 680,
          margin = list(l = 30, r = r_margin, t = 60, b = 220),
          legend = list(
            orientation = "h", x = 0.5, xanchor = "center",
            y = -0.50, yanchor = "top",
            font = list(size = 10),
            title = list(text = "<b>AWaRe Category</b>", font = list(size = 10))
          ),
          bargap = 0,
          bargroupgap = 0
        ) %>%
        layout(
          yaxis = list(automargin = TRUE, categoryorder = "array", categoryarray = rev(levels(aware_plot_data$PlotLabel))),
          xaxis = list(automargin = TRUE)
        ) %>%
        # --- This shape draws the full rectangular border ---
        layout(
          shapes = list(list(
            type = "rect",
            xref = "paper", yref = "paper",
            x0 = 0, x1 = 1, y0 = 0, y1 = 1,
            line = list(color = "black", width = 1.5),
            fillcolor = "rgba(0,0,0,0)"
          ))
        )
      
      plt
    })
    
    
    
    
    
  })
}