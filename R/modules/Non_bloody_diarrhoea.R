# Non-bloody Diarrhoea Analysis Shiny Module - Complete Implementation
# File: Non_bloody_diarrhoea.R

# ========== TAB 1: OVERVIEW UI ==========
nonBloodyDiarrhoeaOverviewUI <- function(id) {
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
               box(width = 12, title = "🎯 AWaRe Quality Indicators for Non-bloody Diarrhoea", status = "info", solidHeader = TRUE,
                   h5("Within the gPPS data structure, the following antibiotic use quality indicator has been identified:"),
                   p("1) Proportion of all patients presenting with acute infectious non-bloody diarrhoea given oral antibiotics."),
                   
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
                   tags$li("No antibiotics are recommended")
                 )
               )
        )
      )
    
    )
  )
}

# ========== TAB 2: ORAL ANTIBIOTIC USE UI ==========
nonBloodyDiarrhoeaOralUI <- function(id) {
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
                   p("Please upload your data files to view non-bloody diarrhoea oral antibiotic analysis.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      
      # Indicator Title with Warning
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📌 Indicator - Oral Antibiotic Use for Non-bloody Diarrhoea", 
                   status = "primary", solidHeader = TRUE,
                   p("Proportion of all patients presenting with acute infectious non-bloody diarrhoea given an oral antibiotic.")
                  
               )
        )
      ),
      
      # Plot
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📈 Antibiotic Use by Route of Administration", 
                   status = "primary", solidHeader = TRUE,
                   p("This visual summarises the proportion of adult outpatients with non-bloody diarrhoea given antibiotics by route of administration across specialties."),
                   div(
                     style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                     withSpinner(plotlyOutput(ns("nonbloody_oral_plot"), height = "450px", width = "100%"), type = 4)
                   )
               )
        )
      ),
      
      # Summary
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📝 Summary of Antibiotic Use for Non-bloody Diarrhoea by Route of Administration", 
                   status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   htmlOutput(ns("nonbloody_oral_summary"))
               )
        )
      )
    )
  )
}

# ========== MODULE SERVER ==========
nonBloodyDiarrhoeaModuleServer <- function(id, data_reactive) {
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
          filter(`Diagnosis code` == "Non_B_DIA") %>%
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
            <p>This script applies <strong>WHO AWaRe Quality Indicators</strong> to adult outpatients with <strong>Non-bloody diarrhoea</strong></p>
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
        filter(`Diagnosis code` == "Non_B_DIA") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_diag_adults <- data_patients %>%
        filter(`Diagnosis code` == "Non_B_DIA", `Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_diag_adults_abx <- data_patients %>%
        filter(`Diagnosis code` == "Non_B_DIA", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_diag_adults_prescriptions <- data_patients %>%
        filter(`Diagnosis code` == "Non_B_DIA", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        nrow()
      
      pct_adults_of_all <- if (total_diag > 0) round(100 * total_diag_adults / total_diag, 1) else 0
      pct_treated_rate <- if (total_diag_adults > 0) round(100 * total_diag_adults_abx / total_diag_adults, 1) else 0
      
      html_summary <- glue::glue("
        <div style='display:flex; flex-direction:column; gap: 12px; margin-top: 15px;'>
          <div style='display: flex; gap: 12px;'>
            <div style='flex: 1; background-color: #e6f4ea; border-left: 6px solid #009E73; padding: 12px; border-radius: 5px;'>
              <strong>Eligible Patients (Adults):</strong><br>
              <span style='font-size: 1.25em;'>{total_diag_adults}</span><br>
              <small>{pct_adults_of_all}% (of {total_diag} patients with non-bloody diarrhoea)</small>
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
    
    # Non-bloody Oral data
    nonbloody_oral_data_reactive <- reactive({
      if (!check_data()) {
        return(list())
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      
      # Pre-clean data
      data_DIARRHEA_NB <- data_patients %>%
        filter(`Diagnosis code` == "Non_B_DIA", `Patient age group` == "ADULT") %>%
        mutate(
          Route = toupper(Route),
          `Antimicrobial name` = trimws(toupper(`Antimicrobial name`)),
          AWaRe = toupper(AWaRe),
          `Type of specialty` = trimws(`Type of specialty`)
        )
      
      # Patient-level summary
      oral_summary <- data_DIARRHEA_NB %>%
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
        data_DIARRHEA_NB = data_DIARRHEA_NB,
        oral_summary_final = oral_summary_final
      )
    })
    
    # Non-bloody Oral summary
    output$nonbloody_oral_summary <- renderUI({
      oral_data <- nonbloody_oral_data_reactive()
      if(length(oral_data) == 0) {
        return(HTML("<p>No data available</p>"))
      }
      
      oral_summary_final <- oral_data$oral_summary_final
      data_DIARRHEA_NB <- oral_data$data_DIARRHEA_NB
      
      # Denominator
      total_eligible <- data_DIARRHEA_NB %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_eligible_with_abx <- data_DIARRHEA_NB %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      intro_text <- glue::glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received any antibiotic for non-bloody diarrhoea (<strong>{total_eligible_with_abx}</strong> out of {total_eligible}).
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
    
    # Non-bloody Oral plot - NO GRAY HIGHLIGHT
    output$nonbloody_oral_plot <- renderPlotly({
      oral_data <- nonbloody_oral_data_reactive()
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
        "IV/IM" = "#D55E00",
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
            text = "<b>Proportion of Patients with Acute Non-bloody Diarrhoea by Antibiotic Route</b>",
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