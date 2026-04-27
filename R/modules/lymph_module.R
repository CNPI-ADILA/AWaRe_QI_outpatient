# ========================================================================
# LYMPH NODE INFECTIONS ANALYSIS MODULE
# ========================================================================
# Module for analyzing WHO AWaRe Quality Indicators for Lymph Node Infections
# Based on Global-PPS outpatient data
# ========================================================================

# Required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidyverse)
library(glue)
library(scales)
library(htmltools)
library(ggtext)

# ========================================================================
# 1. OVERVIEW TAB
# ========================================================================

lymphOverviewUI <- function(id) {
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
    
    # Eligibility Check Box
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "🔍 Initial Eligible Cases Check",
               status = "primary",
               solidHeader = TRUE,
               uiOutput(ns("eligibility_check"))
             )
      )
    ),
    
    # Key Insights Box
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📊 Key Insights",
               status = "warning",
               solidHeader = TRUE,
               uiOutput(ns("key_insights"))
             )
      )
    ),
    
    # WHO AWaRe QIs Overview Box
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "🎯 AWaRe Quality Indicators for Lymph Node Infections",
               status = "info",
               solidHeader = TRUE,
               h5("Within the gPPS data structure, the following antibiotic use quality indicators have been identified:"),
               p("1) Proportion of patients presenting with acute lymphadenitis given an oral antibiotic."),
               p("2) Proportion of patients with acute lymph nodes infections given oral antibiotics by AWaRe category (Access or Watch)."),
               p("3) Proportion of patients presenting with acute lymphadenitis prescribed the total daily dose of oral antibiotics recommended in the WHO AWaRe Antibiotic Book."),
               p("4) Proportion of patients presenting with acute lymphadenitis given the duration in days of oral antibiotics recommended in the WHO AWaRe Antibiotic Book.")
             )
      )
    ),
    
    # Patient-Level and Prescription-Level Insights
    fluidRow(
      column(10, offset = 1,
             box(
               width = 6,
               title = "👥 Patient-Level Insights",
               status = "info",
               solidHeader = TRUE,
               collapsed = TRUE,
               collapsible = TRUE,
               DTOutput(ns("patient_table"))
             ),
             box(
               width = 6,
               title = "📑 Prescription-Level Insights",
               status = "success",
               solidHeader = TRUE,
               collapsed = TRUE,
               collapsible = TRUE,
               DTOutput(ns("prescription_table"))
             )
      )
    )
  )
}

# ========================================================================
# 2. ORAL ANTIBIOTIC USE TAB
# ========================================================================

lymphOralUI <- function(id) {
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
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📌 Indicator 1 - Oral Antibiotic Use",
               status = "primary",
               solidHeader = TRUE,
               p("Proportion of patients presenting with acute lymphadenitis given an oral antibiotic.")
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📈 Antibiotic Use by Route of Administration",
               status = "primary",
               solidHeader = TRUE,
               p("This visual summarises the proportion of adult outpatients with lymph node infections given antibiotics by route of administration across specialties."),
               div(style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                   withSpinner(plotlyOutput(ns("oral_plot"), height = "450px", width = "100%"), type = 4))
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📝 Summary of Antibiotic Use by Route of Administration",
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               uiOutput(ns("oral_summary"))
             )
      )
    )
  )
}

# ========================================================================
# 3. AWaRe CATEGORY TAB
# ========================================================================

lymphAWaReUI <- function(id) {
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
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📌 Indicator 2 - Oral Antibiotic Use by AWaRe",
               status = "primary",
               solidHeader = TRUE,
               p("Proportion of patients with acute lymph nodes infections given oral antibiotics by AWaRe category (Access or Watch).")
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📈 Antibiotic Prescription by AWaRe Classification",
               status = "primary",
               solidHeader = TRUE,
               p("This visual summarises the proportion of adult outpatients with lymph node infections given antibiotics by AWaRe classification across specialties."),
               div(style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                   withSpinner(plotlyOutput(ns("aware_plot"), height = "450px", width = "100%"), type = 4))
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             div(class = "info-box",
                 style = "background-color: #f8f9fa; padding: 15px; border-left: 5px solid #17a2b8; border-radius: 5px; margin-top: 15px;",
                 strong("💡 Note:"),
                 tags$ul(
                   tags$li("This count represents the number of unique patients who were prescribed at least one antibiotic within a specific WHO AWaRe category (Access, Watch, or Reserve) during their encounter. A patient is counted once for each distinct AWaRe category they received an antibiotic from."),
                   tags$li("The AWaRe classification divides antibiotics into three categories: Access, Watch, and Reserve. Access antibiotics are first-line treatments with a low risk of resistance, typically recommended for common infections. Watch antibiotics are associated with a higher risk of resistance and require careful monitoring. Reserve antibiotics are reserved for critical cases involving resistant infections, typically used only when other options are ineffective.")
                 )
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📝 Summary of Antibiotic Use by AWaRe Classification",
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               uiOutput(ns("aware_summary"))
             )
      )
    )
  )
}

# ========================================================================
# 4. CHOICE ALIGNMENT TAB
# ========================================================================

lymphChoiceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Custom CSS for white backgrounds
    tags$head(
      tags$style(HTML("
        .box {
          background-color: white !important;
        }
        .box-body {
          background-color: white !important;
        }
        .tab-content {
          background-color: white !important;
        }
        .nav-tabs-custom {
          background-color: white !important;
        }
        .tab-pane {
          background-color: white !important;
        }
      "))
    ),
    
    # Back Button
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
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📌 Antibiotic Choice Alignment with AWaRe Book Recommendations",
               status = "primary",
               solidHeader = TRUE,
               p("Proportion of patients with acute lymph nodes infections prescribed the recommended oral antibiotic choice in the WHO AWaRe Antibiotic Book.")
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "🔍 WHO AWaRe Book Recommendation:",
               status = "primary",
               solidHeader = TRUE,
               tags$p(strong("For localized acute bacterial lymphadenitis cases:")),
               tags$ul(
                 tags$li(strong("Amoxicillin+clavulanic acid"), " 500 mg+125 mg every 8 hours (ORAL) OR 1 g+200 mg every 8 hours (IV)"),
                 tags$li(strong("Cefalexin"), " 500 mg every 8 hours (ORAL)"),
                 tags$li(strong("Cloxacillin"), " 500 mg every 6 hours (ORAL) OR 2 g every 6 hours (IV)")
               ),
               tags$p(strong("Duration:")),
               tags$ul(
                 tags$li(strong("5 days"))
               )
             )
      )
    ),
    
    # Visual Analytics with Tabbed Interface
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = tagList(
                 icon("chart-bar"),
                 " Antibiotic Choice Alignment Analysis"
               ),
               status = "primary",
               solidHeader = TRUE,
               tabsetPanel(
                 id = ns("choice_viz_tabs"),
                 type = "tabs",
                 
                 # Tab 1: Choice Alignment
                 tabPanel(
                   tagList(icon("check-square"), " Choice Alignment"),
                   div(
                     style = "background-color: white; padding: 15px;",
                     br(),
                     p("This visual summarises the proportion of adult outpatients with lymph node infections prescribed the recommended oral antibiotic choice in the WHO AWaRe Antibiotic Book across specialties."),
                     div(
                       style = "
                         display: flex; 
                         justify-content: center; 
                         align-items: center; 
                         max-width: 100%; 
                         overflow: hidden;
                         background-color: white;
                       ",
                       withSpinner(
                         plotlyOutput(ns("choice_plot"), height = "450px", width = "100%"),
                         type = 4
                       )
                     )
                   )
                 ),
                 
                 # Tab 2: AWaRe Classification
                 tabPanel(
                   tagList(icon("layer-group"), " AWaRe Classification"),
                   div(
                     style = "background-color: white; padding: 15px;",
                     br(),
                     p("This visual shows the distribution of recommended oral antibiotics by WHO AWaRe classification."),
                     div(
                       style = "
                         display: flex; 
                         justify-content: center; 
                         align-items: center; 
                         max-width: 100%; 
                         overflow: hidden;
                         background-color: white;
                       ",
                       withSpinner(
                         plotlyOutput(ns("choice_aware_plot"), height = "450px", width = "100%"),
                         type = 4
                       )
                     )
                   )
                 )
               )
             )
      )
    ),
    
    # Note BEFORE Summary
    fluidRow(
      column(10, offset = 1,
             div(class = "info-box",
                 style = "background-color: #f8f9fa; padding: 15px; border-left: 5px solid #17a2b8; border-radius: 5px; margin-top: 15px;",
                 strong("💡 Note:"),
                 tags$ul(
                   tags$li(strong("Partially recommended:"), " Refers to cases where patients received a WHO AWaRe-recommended antibiotic in combination with one or more additional antibiotics, rather than as monotherapy as specified in the guideline."),
                   tags$li(strong("No antibiotics given:"), " Refers to cases where no antibiotics were prescribed. This may include patients who did not receive any antimicrobial treatment at all, or those who were given other antimicrobials that are not classified as antibiotics.")
                 )
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📝 Summary of Antibiotic Choice Appropriateness",
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               uiOutput(ns("choice_summary"))
             )
      )
    )
  )
}

# ========================================================================
# 5. DOSAGE ALIGNMENT TAB
# ========================================================================

lymphDosageUI <- function(id) {
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
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📌 Indicator 3 – Oral Antibiotic Choice & Dosage Appropriateness",
               status = "primary",
               solidHeader = TRUE,
               p("Proportion of patients with acute lymph nodes infections prescribed the total daily dose of oral antibiotics recommended in the WHO AWaRe Antibiotic Book.")
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "🔍 WHO AWaRe Book Recommendation:",
               status = "primary",
               solidHeader = TRUE,
               tags$p(strong("For localized acute bacterial lymphadenitis cases:")),
               tags$ul(
                 tags$li(strong("Amoxicillin+clavulanic acid"), " 500 mg+125 mg every 8 hours (ORAL) OR 1 g+200 mg every 8 hours (IV)"),
                 tags$li(strong("Cefalexin"), " 500 mg every 8 hours (ORAL)"),
                 tags$li(strong("Cloxacillin"), " 500 mg every 6 hours (ORAL) OR 2 g every 6 hours (IV)")
               ),
               tags$p(strong("Duration:")),
               tags$ul(
                 tags$li(strong("5 days"))
               )
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📈 Antibiotic Choice & Dosage Appropriateness",
               status = "primary",
               solidHeader = TRUE,
               p("This visual summarises the proportion of adult outpatients with lymph node infections prescribed the total daily dose of oral antibiotics recommended in the WHO AWaRe Antibiotic Book across specialties."),
               div(style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                   withSpinner(plotlyOutput(ns("dosage_plot"), height = "450px", width = "100%"), type = 4))
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📝 Summary of Antibiotic Choice & Dosage Appropriateness",
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               uiOutput(ns("dosage_summary"))
             )
      )
    )
  )
}

# ========================================================================
# 6. DURATION ALIGNMENT TAB
# ========================================================================

lymphDurationUI <- function(id) {
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
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📌 Indicator 4 – Oral Antibiotic Duration Appropriateness",
               status = "primary",
               solidHeader = TRUE,
               p("Proportion of patients presenting with acute lymphadenitis given the duration in days of oral antibiotics recommended in the WHO AWaRe Antibiotic Book.")
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📈 Antibiotic Duration Appropriateness",
               status = "primary",
               solidHeader = TRUE,
               p("This visual summarises the proportion of adult outpatients with lymph node infections given the duration in days of oral antibiotics recommended in the WHO AWaRe Antibiotic Book across specialties."),
               div(style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                   withSpinner(plotlyOutput(ns("duration_plot"), height = "450px", width = "100%"), type = 4))
             )
      )
    ),
    
    fluidRow(
      column(10, offset = 1,
             box(
               width = 12,
               title = "📝 Summary of Antibiotic Duration Appropriateness",
               status = "success",
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               uiOutput(ns("duration_summary"))
             )
      )
    )
  )
}

# ========================================================================
# SERVER FUNCTION
# ========================================================================

lymphAnalysisServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # AWaRe categories
    AWaRe_abx <- c("ACCESS", "WATCH", "RESERVE", "NOT RECOMMENDED", "UNCLASSIFIED")
    
    # ====================================================================
    # REACTIVE DATA PREPARATION
    # ====================================================================
    
    # Base filtered data
    data_lymph_base <- reactive({
      req(data_reactive())
      data <- data_reactive()
      
      data$data_patients %>%
        filter(`Diagnosis code` == "LYMPH") %>%
        mutate(
          Route = toupper(Route),
          ATC5 = trimws(toupper(ATC5)),
          AWaRe = toupper(AWaRe),
          `Type of specialty` = trimws(`Type of specialty`),
          AWaRe_compatible = (`Patient age group` == "ADULT" & AWaRe %in% AWaRe_abx)
        )
    })
    
    # Eligible patient count
    eligible_count <- reactive({
      data_lymph_base() %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
    })
    
    # ====================================================================
    # 1. OVERVIEW TAB OUTPUTS
    # ====================================================================
    
    output$eligibility_check <- renderUI({
      eligible_n <- eligible_count()
      
      feedback_class <- if (eligible_n == 0) {
        "error-box"
      } else if (eligible_n < 10) {
        "warning-box"
      } else {
        "success-box"
      }
      
      feedback_icon <- if (eligible_n == 0) {
        "🚫"
      } else if (eligible_n < 10) {
        "⚠️"
      } else {
        "✅"
      }
      
      feedback_message <- if (eligible_n == 0) {
        "No data available: There were no eligible cases for evaluation during this survey period."
      } else if (eligible_n < 10) {
        "Caution: Few eligible cases detected. Interpret results with caution."
      } else {
        "Good to go! Sufficient eligible cases available to proceed with full evaluation."
      }
      
      div(class = feedback_class,
          p("This script applies WHO AWaRe Quality Indicators to adult outpatients with/without antibiotics for lymph nodes infections"),
          tags$ul(
            tags$li(strong("Total eligible cases:"), eligible_n)
          ),
          p(strong(feedback_icon), feedback_message)
      )
    })
    
    output$key_insights <- renderUI({
      data_base <- data_lymph_base()
      
      total_lymph <- data_base %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_lymph_adults <- data_base %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_lymph_adults_abx <- data_base %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_lymph_adults_prescriptions <- data_base %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        nrow()
      
      HTML(glue(
        "<div style='display: flex; gap: 12px; margin-top: 15px; flex-wrap: wrap;'>
          <div style='flex: 1; min-width: 250px; background-color: #e6f4ea; border-left: 6px solid #009E73; padding: 12px; border-radius: 5px;'>
            <strong>Eligible Patients with Lymph Nodes Infections (Adults):</strong><br>
            <span style='font-size: 1.3em;'>{total_lymph_adults}</span><br>
            <small>{round(100 * total_lymph_adults / total_lymph, 1)}% (of {total_lymph} patients with lymph nodes infections)</small>
          </div>
          
          <div style='flex: 1; min-width: 250px; background-color: #fff3cd; border-left: 6px solid #D55E00; padding: 12px; border-radius: 5px;'>
            <strong>Treatment Rate (Abx):</strong><br>
            <span style='font-size: 1.3em;'>{round(100 * total_lymph_adults_abx / total_lymph_adults, 1)}%</span><br>
            <small>({total_lymph_adults_abx} of {total_lymph_adults} eligible patients with lymph nodes infections)</small>
          </div>
          
          <div style='flex: 1; min-width: 250px; background-color: #f0f8ff; border-left: 6px solid #0072B2; padding: 12px; border-radius: 5px;'>
            <strong>Total Antibiotic Prescriptions (eligible adults):</strong><br>
            <span style='font-size: 1.3em;'>{total_lymph_adults_prescriptions}</span>
          </div>
        </div>"
      ))
    })
    
    output$patient_table <- renderDT({
      data_base <- data_lymph_base()
      
      patient_summary <- data_base %>%
        filter(`Patient age group` == "ADULT") %>%
        group_by(`Unique Patient ID`, `Type of specialty`, `Patient age group`) %>%
        summarise(
          `Number of Prescriptions` = n(),
          `Antibiotics Prescribed` = paste(unique(`Antimicrobial name`[!is.na(`Antimicrobial name`)]), collapse = "; "),
          .groups = "drop"
        )
      
      datatable(
        patient_summary,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
    })
    
    output$prescription_table <- renderDT({
      data_base <- data_lymph_base()
      
      prescription_data <- data_base %>%
        filter(`Patient age group` == "ADULT", !is.na(`Antimicrobial name`)) %>%
        select(`Unique Patient ID`, `Type of specialty`, `Antimicrobial name`, 
               ATC5, AWaRe, Route, `Single unit dose`, Unit, `N Doses/day`, 
               `Prescribed/intended duration (days)`)
      
      datatable(
        prescription_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
    })
    
    # Navigation handler
    observeEvent(input$back_to_eligibility, {
      updateTabItems(session = session$parent, "sidebar", selected = "conditions_eligibility")
    })
    
    # ====================================================================
    # 2. ORAL ANTIBIOTIC USE - REACTIVE DATA & OUTPUTS
    # ====================================================================
    
    oral_data_reactive <- reactive({
      data_base <- data_lymph_base()
      
      # Patient-level summary
      patient_summary <- data_base %>%
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
      oral_by_spec <- patient_summary %>%
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
      oral_total <- patient_summary %>%
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
      oral_final <- bind_rows(oral_total, oral_by_spec)
      
      return(oral_final)
    })
    
    output$oral_summary <- renderUI({
      oral_data <- oral_data_reactive()
      data_base <- data_lymph_base()
      
      total_eligible <- data_base %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_eligible_with_abx <- data_base %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      intro_text <- glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received any antibiotic for lymph nodes infections (<strong>{total_eligible_with_abx}</strong> out of {total_eligible}).
        </div><br><br>
        <strong>Summary:</strong><br><br>"
      )
      
      blocks <- oral_data %>%
        mutate(block = pmap_chr(list(
          `Type of specialty`, Patients,
          Oral_Abx, Pct_Oral_Abx,
          IV_IM_Abx, Pct_IV_IM_Abx,
          Other_Abx, Pct_Other_Abx
        ), function(dept, n, oral_n, oral_pct, iv_n, iv_pct, other_n, other_pct) {
          color <- if (dept == "Facility-Wide") "#0072B2" else "#6c757d"
          bg <- if (dept == "Facility-Wide") "#f0f0f0" else "#ffffff"
          
          glue(
            "<div style='background-color: {bg}; border-left: 5px solid {color}; padding: 14px; margin-bottom: 20px;'>
            <strong>🏥 {dept}</strong> <span style='color: #888;'>(n = {n} patients)</span><br><br>
            <ul style='margin-left: 1.2em; line-height: 1.8; padding-left: 0; list-style-type: none;'>
              <li>
                <span style='display: inline-block; background-color: #007bff; color: white; border-radius: 50%; width: 22px; height: 22px; text-align: center; line-height: 22px; font-size: 12px; margin-right: 8px;'>1</span>
                <strong>Given any oral antibiotic:</strong> {oral_pct}% ({oral_n} of {n})
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
        }))
      
      HTML(paste0(intro_text, paste(blocks$block, collapse = "\n")))
    })
    
    output$oral_plot <- renderPlotly({
      oral_data <- oral_data_reactive()
      
      # Prepare long format
      oral_long <- oral_data %>%
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
                                   Other_Abx = "Others"),
          Percent = round(100 * Count / Patients, 1),
          Antibiotic_Type = factor(Antibiotic_Type, levels = c("Oral", "IV/IM", "Others")),
          hover_text = paste0(
            "<b>Specialty:</b> ", `Type of specialty`, "<br>",
            "<b>Route:</b> ", Antibiotic_Type, "<br>",
            "<b>Patients:</b> ", Count, "<br>",
            "<b>Proportion:</b> ", Percent, "%"
          )
        )
      
      # Reorder specialties
      oral_long <- oral_long %>%
        mutate(
          `Type of specialty` = factor(
            `Type of specialty`,
            levels = c("Facility-Wide", sort(setdiff(unique(`Type of specialty`), "Facility-Wide")))
          ),
          LabelColor = ifelse(`Type of specialty` == "Facility-Wide", "#0072B2", "black")
        )
      
      # Color palette
      custom_palette <- c(
        "Oral" = "#0072B2",
        "IV/IM" = "#D55E00",
        "Others" = "#6F42C1"
      )
      
      # Label colors
      label_colors <- setNames(oral_long$LabelColor, oral_long$`Type of specialty`)
      
      # Total counts
      total_counts <- oral_long %>%
        group_by(`Type of specialty`) %>%
        summarise(Total = sum(Count), .groups = "drop")
      
      # Create plot
      p <- ggplot(oral_long, aes(x = `Type of specialty`, y = Percent, fill = Antibiotic_Type, text = hover_text)) +
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
            color = label_colors[levels(oral_long$`Type of specialty`)]
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
      
      # Convert to plotly
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Proportion of Patients with Lymph Node Infections by Antibiotic Route</b>",
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
    
    # ====================================================================
    # 3. AWaRe CLASSIFICATION - REACTIVE DATA & OUTPUTS
    # ====================================================================
    
    aware_data_reactive <- reactive({
      data_base <- data_lymph_base()
      
      expected_aware <- c("Access", "Watch", "Reserve", "Not Recommended", "Unclassified")
      
      # Patient-level AWaRe flags
      aware_summary <- data_base %>%
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
      
      return(aware_final)
    })
    
    output$aware_summary <- renderUI({
      aware_data <- aware_data_reactive()
      data_base <- data_lymph_base()
      
      total_eligible <- data_base %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_eligible_with_abx <- data_base %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx, Route == "O") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      intro_text <- glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received any oral antibiotic for lymph nodes infections (<strong>{total_eligible_with_abx}</strong> out of {total_eligible}).
        </div><br><br>
        <strong>Summary:</strong><br><br>"
      )
      
      blocks <- aware_data %>%
        mutate(block = pmap_chr(list(
          `Type of specialty`, Patients,
          Access, Pct_Access,
          Watch, Pct_Watch,
          Reserve, Pct_Reserve,
          `Not Recommended`, Pct_Not_Recommended,
          Unclassified, Pct_Unclassified
        ), function(dept, n, a_n, a_p, w_n, w_p, r_n, r_p, nr_n, nr_p, u_n, u_p) {
          color <- if (dept == "Facility-Wide") "#0072B2" else "#6c757d"
          bg <- if (dept == "Facility-Wide") "#f0f0f0" else "#ffffff"
          
          glue(
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
        }))
      
      HTML(paste0(intro_text, paste(blocks$block, collapse = "\n")))
    })
    
    output$aware_plot <- renderPlotly({
      aware_data <- aware_data_reactive()
      
      # Prepare plot data
      aware_plot_data <- aware_data %>%
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
      
      # Factor levels
      aware_levels <- c("Access", "Watch", "Reserve", "Not Recommended", "Unclassified")
      aware_plot_data$AWaRe_Cat <- factor(aware_plot_data$AWaRe_Cat, levels = aware_levels)
      
      # Format specialty labels
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
      
      # Colors
      aware_palette <- c(
        "Access" = "#1b9e77",
        "Watch" = "#ff7f00",
        "Reserve" = "#e41a1c",
        "Not Recommended" = "#8c510a",
        "Unclassified" = "#666666"
      )
      
      # Label data
      label_data <- aware_plot_data %>%
        group_by(PlotLabel) %>%
        summarise(Total = sum(Count), .groups = "drop")
      
      # Dynamic buffer
      max_digits <- max(nchar(as.character(label_data$Total)), na.rm = TRUE)
      x_buffer <- max(0.06, 0.03 + 0.035 * max_digits)
      xlim_max <- min(1 + x_buffer, 1.5)
      label_x <- 1 + x_buffer * 0.48
      
      # Create plot
      p <- ggplot(aware_plot_data, aes(y = PlotLabel, x = Proportion, fill = AWaRe_Cat, text = hover_text)) +
        geom_bar(stat = "identity", position = "fill", width = 0.85) +
        geom_text(
          aes(label = ifelse(Proportion > 0.05, percent(Proportion, accuracy = 1), "")),
          position = position_fill(vjust = 0.5),
          size = 3, color = "black"
        ) +
        geom_text(
          data = label_data,
          aes(y = PlotLabel, label = paste0("n=", formatC(Total, format = "d", big.mark = ","))),
          x = label_x,
          inherit.aes = FALSE, size = 3, color = "gray30", vjust = 0.5, hjust = 0
        ) +
        scale_fill_manual(values = aware_palette, breaks = aware_levels) +
        coord_cartesian(xlim = c(0, xlim_max), expand = FALSE) +
        labs(
          x = "Proportion of Patients",
          y = "Specialty",
          fill = "AWaRe Category"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.y = element_markdown(size = 7),
          axis.text.x = element_text(size = 7),
          axis.title = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(size = 8),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          plot.margin = margin(6, 18, 6, 8)
        ) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top"))
      
      # Convert to plotly
      r_margin <- 40 + round(300 * x_buffer)
      
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Distribution of Oral Antibiotic Use in Lymph Node Infections by AWaRe Category</b>",
            x = 0.5,
            xanchor = "center",
            y = 0.98,
            yanchor = "top",
            font = list(size = 11)
          ),
          height = 450,
          width = 680,
          margin = list(l = 30, r = r_margin, t = 60, b = 170),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.40,
            yanchor = "top",
            font = list(size = 10),
            title = list(text = "<b>AWaRe Category</b>", font = list(size = 10))
          ),
          bargap = 0,
          bargroupgap = 0
        ) %>%
        layout(
          yaxis = list(automargin = TRUE, categoryorder = "array", categoryarray = rev(levels(aware_plot_data$PlotLabel))),
          xaxis = list(automargin = TRUE)
        )
      
      plt
    })
    
    # ====================================================================
    # 4. CHOICE ALIGNMENT - REACTIVE DATA & OUTPUTS
    # ====================================================================
    
    choice_data_reactive <- reactive({
      req(data_reactive())
      data <- data_reactive()
      data_base <- data_lymph_base()
      
      # Lookup recommended drugs
      lookup_lymph <- data$data_lookup %>%
        filter(Code == "PC_LYMPH_APPROP_DOSAGE_ORAL_ABX")
      
      lookup_names <- lookup_lymph %>%
        select(starts_with("ABX-ATC")) %>%
        unlist(use.names = FALSE) %>%
        na.omit()
      
      # Expected categories
      expected_categories <- c(
        "Received recommended oral antibiotics",
        "Partially received recommended oral antibiotics",
        "Received oral antibiotics not among recommended options",
        "Received other non-oral antibiotics",
        "No antibiotics given",
        "Not eligible for AWaRe QIs"
      )
      
      # Patient-level classification
      patient_classification <- data_base %>%
        group_by(`Unique Patient ID`, `Type of specialty`) %>%
        summarise(
          Is_Adult = all(`Patient age group` == "ADULT"),
          Has_AWaRe_ABX = any(AWaRe %in% AWaRe_abx),
          Any_Oral = any(Route == "O" & AWaRe %in% AWaRe_abx),
          Match_1_O = any(ATC5 == lookup_names[1] & Route == "O"),
          Match_2_O = any(ATC5 == lookup_names[2] & Route == "O"),
          Match_3_O = any(ATC5 == lookup_names[3] & Route == "O"),
          N_ABX = n_distinct(ATC5[AWaRe %in% AWaRe_abx]),
          N_Oral_ABX = n_distinct(ATC5[Route == "O" & AWaRe %in% AWaRe_abx]),
          .groups = "drop"
        ) %>%
        mutate(
          Num_Recommended_O = Match_1_O + Match_2_O + Match_3_O,
          Category = case_when(
            !Is_Adult ~ "Not eligible for AWaRe QIs",
            !Has_AWaRe_ABX ~ "No antibiotics given",
            Num_Recommended_O == 1 & N_ABX == 1 ~ "Received recommended oral antibiotics",
            Num_Recommended_O >= 1 & N_ABX > 1 ~ "Partially received recommended oral antibiotics",
            Any_Oral ~ "Received oral antibiotics not among recommended options",
            TRUE ~ "Received other non-oral antibiotics"
          )
        )
      
      # Pivot to wide format
      patient_wide <- patient_classification %>%
        select(`Unique Patient ID`, `Type of specialty`, Category) %>%
        mutate(flag = TRUE) %>%
        pivot_wider(
          names_from = Category,
          values_from = flag,
          values_fill = FALSE
        )
      
      # Ensure all expected columns exist
      for (col in expected_categories) {
        if (!col %in% colnames(patient_wide)) {
          patient_wide[[col]] <- FALSE
        }
      }
      
      # Summary by specialty
      choice_by_spec <- patient_wide %>%
        group_by(`Type of specialty`) %>%
        summarise(
          Total = n(),
          `Received recommended oral antibiotics` = sum(`Received recommended oral antibiotics`),
          `Partially received recommended oral antibiotics` = sum(`Partially received recommended oral antibiotics`),
          `Received oral antibiotics not among recommended options` = sum(`Received oral antibiotics not among recommended options`),
          `Received other non-oral antibiotics` = sum(`Received other non-oral antibiotics`),
          `No antibiotics given` = sum(`No antibiotics given`),
          `Not eligible for AWaRe QIs` = sum(`Not eligible for AWaRe QIs`),
          .groups = "drop"
        ) %>%
        pivot_longer(
          cols = -c(`Type of specialty`, Total),
          names_to = "Indicator",
          values_to = "Patients"
        ) %>%
        mutate(Proportion = Patients / Total)
      
      # Facility-Wide summary
      choice_total <- patient_wide %>%
        summarise(
          `Type of specialty` = "Facility-Wide",
          Total = n(),
          `Received recommended oral antibiotics` = sum(`Received recommended oral antibiotics`),
          `Partially received recommended oral antibiotics` = sum(`Partially received recommended oral antibiotics`),
          `Received oral antibiotics not among recommended options` = sum(`Received oral antibiotics not among recommended options`),
          `Received other non-oral antibiotics` = sum(`Received other non-oral antibiotics`),
          `No antibiotics given` = sum(`No antibiotics given`),
          `Not eligible for AWaRe QIs` = sum(`Not eligible for AWaRe QIs`)
        ) %>%
        pivot_longer(
          cols = -c(`Type of specialty`, Total),
          names_to = "Indicator",
          values_to = "Patients"
        ) %>%
        mutate(Proportion = Patients / Total)
      
      # Combine
      choice_final <- bind_rows(choice_total, choice_by_spec)
      
      return(choice_final)
    })
    
    output$choice_plot <- renderPlotly({
      choice_data <- choice_data_reactive()
      
      # Define colors
      drug_choice_colors <- c(
        "Received recommended oral antibiotics" = "#1F77B4",
        "Partially received recommended oral antibiotics" = "#4FA9DC",
        "Received oral antibiotics not among recommended options" = "#EF476F",
        "Received other non-oral antibiotics" = "#D3D3D3",
        "No antibiotics given" = "#F9D99E",
        "Not eligible for AWaRe QIs" = "#A9A9A9"
      )
      
      # Create labeled specialty field
      plot_data <- choice_data %>%
        mutate(
          PlotLabel = ifelse(
            `Type of specialty` == "Facility-Wide",
            "<b style='color:#0072B2;'>Facility-Wide</b>",
            `Type of specialty`
          ),
          PlotLabel = factor(
            PlotLabel,
            levels = c(
              "<b style='color:#0072B2;'>Facility-Wide</b>",
              sort(setdiff(unique(PlotLabel), "<b style='color:#0072B2;'>Facility-Wide</b>"))
            )
          ),
          Indicator = factor(
            Indicator,
            levels = c(
              "Received recommended oral antibiotics",
              "Partially received recommended oral antibiotics",
              "Received oral antibiotics not among recommended options",
              "Received other non-oral antibiotics",
              "No antibiotics given",
              "Not eligible for AWaRe QIs"
            )
          ),
          hover_text = paste0(
            "<b>Specialty:</b> ", gsub("<.*?>", "", as.character(PlotLabel)), "<br>",
            "<b>Category:</b> ", Indicator, "<br>",
            "<b>Patients:</b> ", Patients, "<br>",
            "<b>Total:</b> ", Total, "<br>",
            "<b>Proportion:</b> ", round(Proportion * 100, 1), "%"
          )
        )
      
      # Create plot
      p <- ggplot(plot_data, aes(x = PlotLabel, y = Proportion, fill = Indicator, text = hover_text)) +
        geom_bar(stat = "identity", position = "fill", width = 0.7) +
        geom_text(
          aes(label = ifelse(Patients > 0, Patients, "")),
          position = position_fill(vjust = 0.5),
          size = 2.6, color = "black"
        ) +
        geom_text(
          data = plot_data %>% distinct(PlotLabel, Total),
          aes(x = PlotLabel, y = 1.02, label = paste0("n=", Total)),
          inherit.aes = FALSE,
          size = 3, color = "gray30", hjust = 0.5
        ) +
        scale_fill_manual(values = drug_choice_colors, drop = FALSE) +
        scale_y_continuous(limits = c(0, 1.08), expand = c(0, 0),
                           labels = percent_format(accuracy = 1)) +
        scale_x_discrete(expand = c(0.01, 0.01)) +
        labs(
          x = "Specialty",
          y = "Proportional Stacked Bars",
          fill = "Treatment Appropriateness Category"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.x = element_markdown(size = 7, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 7),
          axis.title  = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text  = element_text(size = 8, margin = margin(b = 4)),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          plot.margin  = margin(6, 8, 6, 8)
        ) +
        guides(
          fill = guide_legend(nrow = 3, byrow = TRUE, title.position = "top")
        )
      
      # Convert to plotly
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Antibiotic Choice Alignment for Lymph Node Infections</b>",
            x = 0.5, xanchor = "center",
            y = 0.97, yanchor = "top",
            font = list(size = 12)
          ),
          height = 450,
          width  = 680,
          margin = list(l = 30, r = 30, t = 60, b = 200),
          legend = list(
            orientation = "h",
            x = 0.5, xanchor = "center",
            y = -0.75, yanchor = "top",
            font = list(size = 9),
            title = list(text = "<b>Treatment Alignment Category</b>", font = list(size = 9))
          ),
          bargap = 0,
          bargroupgap = 0
        )
      
      plt
    })
    
    output$choice_aware_plot <- renderPlotly({
      req(data_reactive())
      data <- data_reactive()
      data_base <- data_lymph_base()
      
      # Lookup names
      lookup_lymph <- data$data_lookup %>%
        filter(Code == "PC_LYMPH_APPROP_DOSAGE_ORAL_ABX")
      
      lookup_names <- lookup_lymph %>%
        select(starts_with("ABX-ATC")) %>%
        unlist(use.names = FALSE)
      
      # Patient summary with AWaRe
      patient_summary_aware <- data_base %>%
        filter(AWaRe_compatible) %>%
        group_by(`Unique Patient ID`, `Type of specialty`, AWaRe) %>%
        summarise(
          N_total = n(),
          N_match = sum(ATC5 %in% lookup_names),
          N_O = sum(Route == "O"),
          N_O_match = sum(Route == "O" & ATC5 %in% lookup_names),
          Match_1_O = any(ATC5 == lookup_names[1] & Route == "O"),
          Match_2_O = any(ATC5 == lookup_names[2] & Route == "O"),
          Match_3_O = any(ATC5 == lookup_names[3] & Route == "O"),
          .groups = "drop"
        ) %>%
        mutate(
          Appropriate_oral = (Match_1_O | Match_2_O | Match_3_O)
        )
      
      # Create summary
      aware_long <- patient_summary_aware %>%
        filter(Appropriate_oral) %>%
        group_by(`Type of specialty`, AWaRe) %>%
        summarise(Patients = n(), .groups = "drop")
      
      # Add Facility-Wide
      aware_hospital <- aware_long %>%
        group_by(AWaRe) %>%
        summarise(Patients = sum(Patients), .groups = "drop") %>%
        mutate(`Type of specialty` = "Facility-Wide")
      
      aware_long <- bind_rows(aware_long, aware_hospital)
      
      # Set factor levels
      aware_levels_stack <- c("WATCH", "ACCESS")
      aware_long$AWaRe <- factor(aware_long$AWaRe, levels = aware_levels_stack)
      
      # Ensure all combinations
      all_combos <- expand_grid(
        `Type of specialty` = unique(aware_long$`Type of specialty`),
        AWaRe = aware_levels_stack
      )
      
      aware_long <- all_combos %>%
        left_join(aware_long, by = c("Type of specialty", "AWaRe")) %>%
        mutate(Patients = replace_na(Patients, 0))
      
      # Calculate totals
      aware_long <- aware_long %>%
        group_by(`Type of specialty`) %>%
        mutate(
          Total = sum(Patients),
          Proportion = ifelse(Total > 0, Patients / Total, 0),
          hover_text = paste0(
            "<b>Specialty:</b> ", `Type of specialty`, "<br>",
            "<b>AWaRe:</b> ", AWaRe, "<br>",
            "<b>Patients:</b> ", Patients, "<br>",
            "<b>Total:</b> ", Total, "<br>",
            "<b>Proportion:</b> ", round(Proportion * 100, 1), "%"
          )
        ) %>%
        ungroup()
      
      # Format labels
      aware_long <- aware_long %>%
        mutate(
          PlotLabel = ifelse(
            `Type of specialty` == "Facility-Wide",
            "<b style='color:#0072B2;'>Facility-Wide</b>",
            `Type of specialty`
          ),
          PlotLabel = factor(
            PlotLabel,
            levels = rev(c(
              "<b style='color:#0072B2;'>Facility-Wide</b>",
              sort(setdiff(unique(`Type of specialty`), "Facility-Wide"))
            ))
          )
        )
      
      # Create plot
      p <- ggplot(aware_long, aes(x = PlotLabel, y = Proportion, fill = AWaRe, text = hover_text)) +
        geom_bar(stat = "identity", position = "fill", width = 0.85) +
        geom_text(
          aes(label = ifelse(Proportion > 0.05, percent(Proportion, accuracy = 1), "")),
          position = position_fill(vjust = 0.5),
          size = 3, color = "black"
        ) +
        geom_text(
          data = aware_long %>% distinct(PlotLabel, Total),
          aes(x = PlotLabel, y = 1.015, label = paste0("n=", Total)),
          inherit.aes = FALSE,
          size = 3, color = "gray30", hjust = 0
        ) +
        scale_fill_manual(
          breaks = c("ACCESS", "WATCH"),
          values = c("ACCESS" = "#1b9e77", "WATCH" = "#ff7f00"),
          drop = FALSE
        ) +
        coord_flip(ylim = c(0, 1.04)) +
        labs(
          x = "Specialty",
          y = "Proportion of Patients",
          fill = "AWaRe Classification"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.y = element_markdown(size = 7),
          axis.text.x = element_text(size = 7),
          axis.title = element_text(face = "bold", size = 10),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(size = 8),
          plot.margin = margin(6, 8, 6, 8)
        ) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "top"))
      
      # Convert to plotly
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Recommended Oral Antibiotics by AWaRe Classification</b>",
            x = 0.5,
            xanchor = "center",
            y = 0.98,
            yanchor = "top",
            font = list(size = 11)
          ),
          height = 450,
          width = 680,
          margin = list(l = 30, r = 40, t = 60, b = 170),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.40,
            yanchor = "top",
            font = list(size = 10),
            title = list(text = "<b>AWaRe Classification</b>", font = list(size = 10))
          ),
          bargap = 0,
          bargroupgap = 0
        ) %>%
        layout(
          yaxis = list(automargin = TRUE, categoryorder = "array", categoryarray = rev(levels(aware_long$PlotLabel))),
          xaxis = list(automargin = TRUE)
        )
      
      plt
    })
    
    output$choice_summary <- renderUI({
      choice_data <- choice_data_reactive()
      
      # Filter relevant categories
      relevant_data <- choice_data %>%
        filter(Indicator %in% c(
          "Received recommended oral antibiotics",
          "Partially received recommended oral antibiotics",
          "Received oral antibiotics not among recommended options"
        ))
      
      if (nrow(relevant_data) == 0 || sum(relevant_data$Patients) == 0) {
        return(HTML(glue(
          "<div style='background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 14px; margin-top: 10px;'>
          ⚠️ <strong>No summary available</strong> — there are currently no eligible patients for assessing oral antibiotic choice appropriateness for lymph nodes infections.<br><br>
          <em>This may indicate that no patients received oral antibiotics, or that none met the inclusion criteria during the reporting period.</em>
          </div>"
        )))
      }
      
      # Get total oral from Facility-Wide
      total_oral <- relevant_data %>%
        filter(`Type of specialty` == "Facility-Wide") %>%
        summarise(Total = sum(Patients)) %>%
        pull(Total)
      
      total_eligible <- choice_data %>%
        filter(`Type of specialty` == "Facility-Wide", 
               Indicator != "Not eligible for AWaRe QIs") %>%
        summarise(Total = sum(Patients)) %>%
        pull(Total)
      
      # Intro text
      intro_text <- glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received any oral antibiotics for lymph nodes infections (<strong>{total_oral}</strong> out of {total_eligible}).
        </div><br><br>
        <strong>Summary:</strong><br><br>"
      )
      
      # Format blocks
      formatted_blocks <- relevant_data %>%
        pivot_wider(
          names_from = Indicator,
          values_from = c(Patients, Proportion),
          values_fill = 0
        ) %>%
        mutate(block = pmap_chr(
          list(
            `Type of specialty`,
            Total,
            `Patients_Received recommended oral antibiotics`,
            `Proportion_Received recommended oral antibiotics`,
            `Patients_Partially received recommended oral antibiotics`,
            `Proportion_Partially received recommended oral antibiotics`,
            `Patients_Received oral antibiotics not among recommended options`,
            `Proportion_Received oral antibiotics not among recommended options`
          ),
          function(dept, total, full_n, full_p, part_n, part_p, none_n, none_p) {
            color <- if (dept == "Facility-Wide") "#0072B2" else "#6c757d"
            bg <- if (dept == "Facility-Wide") "#f0f0f0" else "#ffffff"
            
            glue(
              "<div style='background-color: {bg}; border-left: 5px solid {color}; padding: 14px; margin-bottom: 20px;'>
              <strong>🏥 {dept}</strong> <span style='color: #888;'>(n = {total} patients)</span><br><br>
              <ul style='margin-left: 1.2em; line-height: 1.7; padding-left: 0; list-style-type: none;'>
                <li>✅ <strong>Received recommended oral antibiotics</strong> (as per WHO AWaRe Book): <strong>{percent(full_p, accuracy = 0.1)}</strong> ({full_n} out of {total})</li>
                <li>⚠️ <strong>Partially received recommended oral antibiotics</strong> (as per WHO AWaRe Book): <strong>{percent(part_p, accuracy = 0.1)}</strong> ({part_n} out of {total})</li>
                <li>❌ <strong>Received oral antibiotics not among recommended options</strong> (as per WHO AWaRe Book): <strong>{percent(none_p, accuracy = 0.1)}</strong> ({none_n} out of {total})</li>
              </ul>
              </div>"
            )
          }
        )) %>%
        mutate(order = ifelse(`Type of specialty` == "Facility-Wide", 0, 1)) %>%
        arrange(order, `Type of specialty`)
      
      HTML(paste0(intro_text, paste(formatted_blocks$block, collapse = "\n")))
    })
    
    # ====================================================================
    # 5. DOSAGE ALIGNMENT - REACTIVE DATA & OUTPUTS
    # ====================================================================
    
    dosage_data_reactive <- reactive({
      req(data_reactive())
      data <- data_reactive()
      data_base <- data_lymph_base()
      
      # Get lookup info
      lookup2 <- data$data_lookup %>%
        filter(Code == "PC_LYMPH_APPROP_DOSAGE_ORAL_ABX")
      
      lookup_names2 <- toupper(trimws(unlist(lookup2[1, c("ABX-ATC-1", "ABX-ATC-2", "ABX-ATC-3")], use.names = FALSE)))
      
      # Compute Total Daily Dose with MU → IU conversion
      data_lymph_dose <- data_base %>%
        mutate(
          Unit_Factor = case_when(
            Unit == "mg" ~ 1,
            Unit == "g" ~ 1000,
            Unit == "IU" ~ 1,
            Unit == "MU" ~ 1e6,
            TRUE ~ NA_real_
          ),
          Total_Daily_Dose = as.numeric(`Single unit dose`) * as.numeric(`N Doses/day`) * Unit_Factor
        )
      
      # Helper functions
      convert_unit <- function(unit) {
        case_when(
          unit == "mg" ~ 1,
          unit == "g" ~ 1000,
          unit == "IU" ~ 1,
          unit == "MU" ~ 1e6,
          TRUE ~ NA_real_
        )
      }
      
      get_expected_dose <- function(i) {
        dose <- as.numeric(lookup2[[paste0("ABX-DOSE-", i)]][1])
        freq <- as.numeric(lookup2[[paste0("ABX-DAY-DOSE-", i)]][1])
        unit <- lookup2[[paste0("ABX-UNIT-", i)]][1]
        dose * freq * convert_unit(unit)
      }
      
      # Extract info
      drug_names <- sapply(1:3, function(i) toupper(trimws(lookup2[[paste0("ABX-ATC-", i)]][1])))
      routes <- sapply(1:3, function(i) toupper(trimws(lookup2[[paste0("ABX-ROUTE-", i)]][1])))
      expected_doses <- sapply(1:3, get_expected_dose)
      
      # Match drug + dose
      data_lymph_dose <- data_lymph_dose %>%
        mutate(
          Match_Drug_Dose_1 = ATC5 == drug_names[1] & Route == "O" &
            abs(Total_Daily_Dose - expected_doses[1]) < 1,
          Match_Drug_Dose_2 = ATC5 == drug_names[2] & Route == "O" &
            abs(Total_Daily_Dose - expected_doses[2]) < 1,
          Match_Drug_Dose_3 = ATC5 == drug_names[3] & Route == "O" &
            abs(Total_Daily_Dose - expected_doses[3]) < 1
        )
      
      # Patient summary
      patient_summary <- data_lymph_dose %>%
        filter(AWaRe_compatible) %>%
        group_by(`Unique Patient ID`, `Type of specialty`) %>%
        summarise(
          Match_1_O = any(ATC5 == lookup_names2[1] & Route == "O"),
          Match_2_O = any(ATC5 == lookup_names2[2] & Route == "O"),
          Match_3_O = any(ATC5 == lookup_names2[3] & Route == "O"),
          Dose_1_OK = any(Match_Drug_Dose_1),
          Dose_2_OK = any(Match_Drug_Dose_2),
          Dose_3_OK = any(Match_Drug_Dose_3),
          Any_O = any(Route == "O"),
          .groups = "drop"
        ) %>%
        mutate(
          Any_Match = Match_1_O | Match_2_O | Match_3_O,
          Any_Correct_Dose = 
            (Match_1_O & Dose_1_OK) |
            (Match_2_O & Dose_2_OK) |
            (Match_3_O & Dose_3_OK),
          Dose_Result = case_when(
            Any_Correct_Dose ~ "Received recommended oral antibiotic with recommended dosage",
            Any_Match & !Any_Correct_Dose ~ "Received recommended oral antibiotic without recommended dosage",
            Any_O & !Any_Match ~ "Received oral antibiotics not among recommended options",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(Dose_Result))
      
      # Summary by specialty
      dose_counts <- patient_summary %>%
        count(`Type of specialty`, Dose_Result, name = "Patients")
      
      # Complete combinations
      all_combos <- expand_grid(
        `Type of specialty` = unique(patient_summary$`Type of specialty`),
        Dose_Result = c("Received recommended oral antibiotic with recommended dosage",
                        "Received recommended oral antibiotic without recommended dosage",
                        "Received oral antibiotics not among recommended options")
      )
      
      dose_summary <- all_combos %>%
        left_join(dose_counts, by = c("Type of specialty", "Dose_Result")) %>%
        mutate(Patients = replace_na(Patients, 0)) %>%
        group_by(`Type of specialty`) %>%
        mutate(Total = sum(Patients),
               Proportion = round(100 * Patients / Total, 1)) %>%
        ungroup()
      
      # Facility-Wide
      hospital_row <- dose_summary %>%
        group_by(Dose_Result) %>%
        summarise(Patients = sum(Patients), .groups = "drop") %>%
        mutate(`Type of specialty` = "Facility-Wide") %>%
        group_by(`Type of specialty`) %>%
        mutate(Total = sum(Patients),
               Proportion = round(100 * Patients / Total, 1)) %>%
        ungroup()
      
      final_summary <- bind_rows(dose_summary, hospital_row) %>%
        arrange(`Type of specialty`, Dose_Result)
      
      return(final_summary)
    })
    
    output$dosage_plot <- renderPlotly({
      dosage_data <- dosage_data_reactive()
      
      # Order factor for stacking
      dosage_data$Dose_Result <- factor(
        dosage_data$Dose_Result,
        levels = c("Received recommended oral antibiotic with recommended dosage",
                   "Received recommended oral antibiotic without recommended dosage",
                   "Received oral antibiotics not among recommended options")
      )
      
      # Create styled labels
      dosage_data <- dosage_data %>%
        mutate(
          PlotLabel = ifelse(
            `Type of specialty` == "Facility-Wide",
            "<b style='color:#0072B2;'>Facility-Wide</b>",
            `Type of specialty`
          ),
          hover_text = paste0(
            "<b>Specialty:</b> ", gsub("<.*?>", "", as.character(PlotLabel)), "<br>",
            "<b>Category:</b> ", Dose_Result, "<br>",
            "<b>Patients:</b> ", Patients, "<br>",
            "<b>Total:</b> ", Total, "<br>",
            "<b>Proportion:</b> ", Proportion, "%"
          )
        )
      
      # Define factor order
      ordered_labels <- c(
        "<b style='color:#0072B2;'>Facility-Wide</b>",
        sort(unique(dosage_data$PlotLabel[dosage_data$PlotLabel != "<b style='color:#0072B2;'>Facility-Wide</b>"]))
      )
      dosage_data$PlotLabel <- factor(dosage_data$PlotLabel, levels = rev(ordered_labels))
      
      # Extract label data
      label_data <- dosage_data %>%
        distinct(`Type of specialty`, Total) %>%
        mutate(
          PlotLabel = ifelse(
            `Type of specialty` == "Facility-Wide",
            "<b style='color:#0072B2;'>Facility-Wide</b>",
            `Type of specialty`
          ),
          PlotLabel = factor(PlotLabel, levels = levels(dosage_data$PlotLabel))
        )
      
      # Create plot
      p <- ggplot(dosage_data, aes(x = PlotLabel, y = Proportion, fill = Dose_Result, text = hover_text)) +
        geom_bar(stat = "identity", position = "fill", width = 0.85) +
        geom_text(
          aes(label = ifelse(Proportion > 5, paste0(round(Proportion, 1), "%"), "")),
          position = position_fill(vjust = 0.5),
          size = 3, color = "black"
        ) +
        geom_text(
          data = label_data,
          aes(x = PlotLabel, y = 1.015, label = paste0("n=", Total)),
          inherit.aes = FALSE,
          size = 3, color = "gray30", hjust = 0
        ) +
        scale_fill_manual(
          breaks = c("Received recommended oral antibiotic with recommended dosage",
                     "Received recommended oral antibiotic without recommended dosage",
                     "Received oral antibiotics not among recommended options"),
          values = c("Received recommended oral antibiotic with recommended dosage" = "#084594",
                     "Received recommended oral antibiotic without recommended dosage" = "#FC9272",
                     "Received oral antibiotics not among recommended options" = "#D3D3D3")
        ) +
        coord_flip(ylim = c(0, 1.05)) +
        labs(
          x = "Specialty",
          y = "Proportional Stacked Bars",
          fill = "Treatment Appropriateness Category"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.y = element_markdown(size = 7),
          axis.text.x = element_text(size = 7),
          axis.title = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(size = 8, margin = margin(r = 2)),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          plot.margin = margin(6, 8, 6, 8)
        ) +
        guides(
          fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top")
        )
      
      # Convert to plotly
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Antibiotic Choice & Dosage for Lymph Node Infections</b>",
            x = 0.5,
            xanchor = "center",
            y = 0.98,
            yanchor = "top",
            font = list(size = 11)
          ),
          height = 450,
          width = 680,
          margin = list(l = 30, r = 80, t = 60, b = 200),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.70,
            yanchor = "top",
            font = list(size = 9),
            title = list(text = "<b>Treatment Classification</b>", font = list(size = 9))
          ),
          bargap = 0,
          bargroupgap = 0
        ) %>%
        layout(
          yaxis = list(automargin = TRUE, categoryorder = "array", categoryarray = rev(ordered_labels)),
          xaxis = list(automargin = TRUE)
        )
      
      plt
    })
    
    output$dosage_summary <- renderUI({
      dosage_data <- dosage_data_reactive()
      data_base <- data_lymph_base()
      
      # Exclude Facility-Wide
      dosage_filtered <- dosage_data %>%
        filter(`Type of specialty` != "Facility-Wide")
      
      # Define relevant categories
      dose_categories <- c(
        "Received recommended oral antibiotic with recommended dosage",
        "Received recommended oral antibiotic without recommended dosage"
      )
      
      # Total eligible
      total_eligible <- data_base %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      # Total with appropriate choice
      total_approp <- dosage_filtered %>%
        filter(Dose_Result %in% dose_categories) %>%
        summarise(Total = sum(Patients, na.rm = TRUE)) %>%
        pull(Total)
      
      # Extract relevant data
      relevant_data <- dosage_filtered %>%
        filter(Dose_Result %in% dose_categories)
      
      if (nrow(relevant_data) == 0) {
        return(HTML(glue(
          "<div style='background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 14px; margin-top: 10px;'>
          ⚠️ <strong>No summary available</strong> — there are currently no eligible patients with lymph nodes infections for assessing oral antibiotic dosage appropriateness.<br><br>
          <em>This may indicate that no patients met the inclusion criteria, or no patients received recommended oral antibiotics during the reporting period.</em>
          </div>"
        )))
      }
      
      # Build intro
      intro_text <- glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients with lymph nodes infections who received any recommended (<em>or partially recommended</em>) oral antibiotic choice based on WHO AWaRe Book (<strong>{total_approp}</strong> out of {total_eligible}).
        </div><br><br>
        <strong>Summary:</strong><br><br>"
      )
      
      # Ensure all combinations
      complete_summary <- expand_grid(
        `Type of specialty` = unique(dosage_data$`Type of specialty`),
        Dose_Result = dose_categories
      ) %>%
        left_join(dosage_data, by = c("Type of specialty", "Dose_Result")) %>%
        mutate(
          Patients = replace_na(Patients, 0),
          Total = ave(Patients, `Type of specialty`, FUN = sum),
          Proportion = ifelse(Total == 0, 0, Patients / Total)
        )
      
      # Filter out zero totals
      complete_summary <- complete_summary %>%
        filter(Total > 0)
      
      if (nrow(complete_summary) == 0) {
        return(HTML(glue(
          "<div style='background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 14px; margin-top: 10px;'>
          ⚠️ <strong>No summary available</strong> — there are currently no eligible patients for evaluating the <strong>appropriateness of oral antibiotic dosage</strong> for lymph nodes infections<br><br>
          <em>This may indicate that no patients met the inclusion criteria, or no patients received recommended oral antibiotics during the reporting period.</em>
          </div>"
        )))
      }
      
      # Format blocks
      formatted_blocks <- complete_summary %>%
        group_by(`Type of specialty`) %>%
        summarise(
          block = {
            dept <- first(`Type of specialty`)
            color <- if (dept == "Facility-Wide") "#0072B2" else "#6c757d"
            bg <- if (dept == "Facility-Wide") "#f0f0f0" else "#ffffff"
            
            glue(
              "<div style='background-color: {bg}; border-left: 5px solid {color}; padding: 14px; margin-bottom: 20px;'>
              <strong>🏥 {dept}</strong> <span style='color: #888;'>(n = {first(Total)} patients)</span><br><br>
              <ul style='margin-left: 1.2em; line-height: 1.7; padding-left: 0; list-style-type: none;'>
                <li>✅ <strong>Received recommended oral antibiotic with recommended dosage</strong> (as per WHO AWaRe Book): <strong>{percent(Proportion[Dose_Result == 'Received recommended oral antibiotic with recommended dosage'], accuracy = 0.1)}</strong> ({Patients[Dose_Result == 'Received recommended oral antibiotic with recommended dosage']} out of {first(Total)})</li>
                <li>❌ <strong>Received recommended oral antibiotic without recommended dosage</strong> (as per WHO AWaRe Book): <strong>{percent(Proportion[Dose_Result == 'Received recommended oral antibiotic without recommended dosage'], accuracy = 0.1)}</strong> ({Patients[Dose_Result == 'Received recommended oral antibiotic without recommended dosage']} out of {first(Total)})</li>
              </ul>
              </div>"
            )
          },
          .groups = "drop"
        )
      
      # Reorder to show Facility-Wide first
      formatted_blocks <- formatted_blocks %>%
        mutate(order = ifelse(`Type of specialty` == "Facility-Wide", 0, 1)) %>%
        arrange(order, `Type of specialty`) %>%
        select(-order)
      
      HTML(paste0(intro_text, paste(formatted_blocks$block, collapse = "\n")))
    })
    
    # ====================================================================
    # 6. DURATION ALIGNMENT - REACTIVE DATA & OUTPUTS
    # ====================================================================
    
    duration_data_reactive <- reactive({
      req(data_reactive())
      data <- data_reactive()
      data_base <- data_lymph_base()
      
      # Get lookup
      lookup2 <- data$data_lookup %>%
        filter(Code == "PC_LYMPH_APPROP_DOSAGE_ORAL_ABX")
      
      # Extract durations
      lookup_duration <- tibble(
        Drug = toupper(trimws(unlist(lookup2 %>% select(starts_with("ABX-ATC"))))),
        Duration_Raw = trimws(unlist(lookup2 %>% select(starts_with("ABX-DURATION-"))))
      ) %>%
        filter(!is.na(Drug), !is.na(Duration_Raw)) %>%
        mutate(
          Duration_Min = as.numeric(gsub("-.*", "", Duration_Raw)),
          Duration_Max = as.numeric(gsub(".*-", "", Duration_Raw)),
          Duration_Max = ifelse(is.na(Duration_Max), Duration_Min, Duration_Max)
        )
      
      # Merge into patient data
      data_lymph_dur <- data_base %>%
        left_join(lookup_duration, by = c("ATC5" = "Drug"))
      
      # Flag duration match
      data_lymph_dur <- data_lymph_dur %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        mutate(
          Prescribed_Duration = as.numeric(`Prescribed/intended duration (days)`),
          Duration_Match = case_when(
            is.na(Prescribed_Duration) | is.na(Duration_Min) ~ NA_character_,
            Prescribed_Duration >= Duration_Min & Prescribed_Duration <= Duration_Max ~ "Yes",
            TRUE ~ "No"
          )
        )
      
      # Patient summary
      duration_summary <- data_lymph_dur %>%
        filter(Route == "O") %>%
        group_by(`Unique Patient ID`, `Type of specialty`) %>%
        summarise(
          Duration_Match_Status = case_when(
            all(is.na(Duration_Raw)) ~ "Given oral antibiotic not among recommended options",
            any(Duration_Match == "Yes") ~ "Given recommended oral antibiotic with recommended duration",
            all(Duration_Match == "No") ~ "Given recommended oral antibiotic but with non-recommended duration",
            TRUE ~ "Given recommended oral antibiotic with unknown duration"
          ),
          .groups = "drop"
        )
      
      # Summary by specialty
      dept_duration <- duration_summary %>%
        count(`Type of specialty`, Duration_Match_Status, name = "Patients") %>%
        group_by(`Type of specialty`) %>%
        mutate(
          Total = sum(Patients),
          Proportion = round(Patients / Total, 3)
        ) %>%
        ungroup()
      
      # Facility-Wide
      hospital_duration <- dept_duration %>%
        group_by(Duration_Match_Status) %>%
        summarise(Patients = sum(Patients), .groups = "drop") %>%
        mutate(`Type of specialty` = "Facility-Wide") %>%
        group_by(`Type of specialty`) %>%
        mutate(
          Total = sum(Patients),
          Proportion = round(Patients / Total, 3)
        ) %>%
        ungroup()
      
      # Combine
      duration_viz_data <- bind_rows(dept_duration, hospital_duration)
      
      # Format labels
      duration_viz_data <- duration_viz_data %>%
        mutate(
          PlotLabel = ifelse(
            `Type of specialty` == "Facility-Wide",
            "<b style='color:#0072B2;'>Facility-Wide</b>",
            `Type of specialty`
          )
        )
      
      # Complete combinations
      duration_expected_categories <- c(
        "Given recommended oral antibiotic with recommended duration",
        "Given recommended oral antibiotic but with non-recommended duration",
        "Given recommended oral antibiotic with unknown duration",
        "Given oral antibiotic not among recommended options"
      )
      
      all_combos <- expand_grid(
        PlotLabel = unique(duration_viz_data$PlotLabel),
        Duration_Match_Status = duration_expected_categories
      )
      
      duration_viz_data <- all_combos %>%
        left_join(duration_viz_data, by = c("PlotLabel", "Duration_Match_Status")) %>%
        mutate(
          Patients = replace_na(Patients, 0),
          Total = replace_na(Total, 0),
          Proportion = ifelse(Total > 0, Patients / Total, 0),
          hover_text = paste0(
            "<b>Specialty:</b> ", gsub("<.*?>", "", as.character(PlotLabel)), "<br>",
            "<b>Category:</b> ", Duration_Match_Status, "<br>",
            "<b>Patients:</b> ", Patients, "<br>",
            "<b>Total:</b> ", Total, "<br>",
            "<b>Proportion:</b> ", round(Proportion * 100, 1), "%"
          )
        )
      
      # Set factor levels
      ordered_labels <- c(
        "<b style='color:#0072B2;'>Facility-Wide</b>",
        sort(setdiff(unique(duration_viz_data$PlotLabel), "<b style='color:#0072B2;'>Facility-Wide</b>"))
      )
      
      duration_viz_data$PlotLabel <- factor(duration_viz_data$PlotLabel, levels = rev(ordered_labels))
      duration_viz_data$Duration_Match_Status <- factor(
        duration_viz_data$Duration_Match_Status,
        levels = duration_expected_categories
      )
      
      return(duration_viz_data)
    })
    
    output$duration_plot <- renderPlotly({
      duration_data <- duration_data_reactive()
      
      # Define colors
      duration_colors <- c(
        "Given recommended oral antibiotic with recommended duration" = "#2c7bb6",
        "Given recommended oral antibiotic but with non-recommended duration" = "#f46d43",
        "Given recommended oral antibiotic with unknown duration" = "#F9D99E",
        "Given oral antibiotic not among recommended options" = "gray80"
      )
      
      # Label data
      label_data <- duration_data %>%
        filter(Proportion > 0) %>%
        distinct(PlotLabel, Total)
      
      # Create plot
      p <- ggplot(duration_data, aes(x = PlotLabel, y = Proportion, fill = Duration_Match_Status, text = hover_text)) +
        geom_bar(stat = "identity", position = "fill", width = 0.85) +
        geom_text(
          aes(label = ifelse(Proportion > 0.05, paste0(round(Proportion * 100), "%"), "")),
          position = position_fill(vjust = 0.5),
          size = 3, color = "black"
        ) +
        geom_text(
          data = label_data,
          aes(x = PlotLabel, y = 1.05, label = paste0("n = ", Total)),
          inherit.aes = FALSE,
          size = 3.2, color = "gray30", hjust = 0
        ) +
        scale_fill_manual(values = duration_colors) +
        coord_flip(ylim = c(0, 1.07)) +
        labs(
          x = "Specialty",
          y = "Proportion of Patients",
          fill = "Duration Match"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.y = element_markdown(size = 7),
          axis.text.x = element_text(size = 7),
          axis.title = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(size = 8),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          plot.margin = margin(6, 8, 6, 8)
        ) +
        guides(
          fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top")
        )
      
      # Convert to plotly
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Duration Appropriateness for Lymph Node Infections</b>",
            x = 0.5,
            xanchor = "center",
            y = 0.98,
            yanchor = "top",
            font = list(size = 11)
          ),
          height = 450,
          width = 680,
          margin = list(l = 30, r = 40, t = 60, b = 200),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.70,
            yanchor = "top",
            font = list(size = 10),
            title = list(text = "<b>Duration Match</b>", font = list(size = 10))
          ),
          bargap = 0,
          bargroupgap = 0
        ) %>%
        layout(
          yaxis = list(automargin = TRUE, categoryorder = "array", categoryarray = rev(levels(duration_data$PlotLabel))),
          xaxis = list(automargin = TRUE)
        )
      
      plt
    })
    
    output$duration_summary <- renderUI({
      duration_data <- duration_data_reactive()
      data_base <- data_lymph_base()
      
      # Define categories
      duration_categories <- c(
        "Given recommended oral antibiotic with recommended duration",
        "Given recommended oral antibiotic but with non-recommended duration",
        "Given recommended oral antibiotic with unknown duration"
      )
      
      # Filter data
      duration_filtered <- duration_data %>%
        filter(Duration_Match_Status %in% duration_categories)
      
      # Check for data
      if (nrow(duration_filtered) == 0 || sum(duration_filtered$Patients, na.rm = TRUE) == 0) {
        return(HTML(glue(
          "<div style='background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 14px; margin-top: 10px;'>
          ⚠️ <strong>No summary available</strong> — there are currently no eligible patients with lymph nodes infections for assessing oral antibiotic duration compliance.<br><br>
          <em>This may indicate that no patients received recommended oral antibiotics with available duration data during the reporting period.</em>
          </div>"
        )))
      }
      
      # Ensure all combinations
      dept_labels <- unique(duration_data$PlotLabel)
      
      all_combos <- expand_grid(
        PlotLabel = dept_labels,
        Duration_Match_Status = duration_categories
      )
      
      duration_summary_all <- all_combos %>%
        left_join(duration_data, by = c("PlotLabel", "Duration_Match_Status")) %>%
        mutate(
          Patients = replace_na(Patients, 0),
          Total = ave(Patients, PlotLabel, FUN = function(x) max(x, na.rm = TRUE)),
          Proportion = ifelse(Total > 0, Patients / Total, 0)
        )
      
      # Calculate Facility-Wide total
      hospital_total_patients <- duration_summary_all %>%
        filter(PlotLabel == "<b style='color:#0072B2;'>Facility-Wide</b>") %>%
        summarise(Total = sum(Patients)) %>%
        pull(Total)
      
      # Intro
      intro_text <- glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients with lymph nodes infections who received recommended (<em>or partially recommended</em>) oral antibiotic choice (based on WHO AWaRe Book): 
        <strong>{hospital_total_patients}</strong>.
        </div><br><br>
        <strong>Summary:</strong><br><br>"
      )
      
      # Format blocks
      formatted_blocks <- duration_summary_all %>%
        filter(Duration_Match_Status %in% duration_categories) %>%
        group_by(PlotLabel) %>%
        summarise(
          block = {
            dept <- unique(PlotLabel)
            total <- unique(Total)
            color <- if (dept == "<b style='color:#0072B2;'>Facility-Wide</b>") "#0072B2" else "#6c757d"
            bg <- if (dept == "<b style='color:#0072B2;'>Facility-Wide</b>") "#f0f0f0" else "#ffffff"
            
            glue(
              "<div style='background-color: {bg}; border-left: 5px solid {color}; padding: 14px; margin-bottom: 20px;'>
              <strong>🏥 {dept}</strong> <span style='color: #888;'>(n = {total} patients)</span><br><br>
              <ul style='margin-left: 1.2em; line-height: 1.7; padding-left: 0; list-style-type: none;'>
                <li>✅ <strong>{duration_categories[1]}</strong>: 
                  <strong>{percent(Proportion[Duration_Match_Status == duration_categories[1]], accuracy = 0.1)}</strong> 
                  ({Patients[Duration_Match_Status == duration_categories[1]]} of {total})</li>
                <li>❌ <strong>{duration_categories[2]}</strong>: 
                  <strong>{percent(Proportion[Duration_Match_Status == duration_categories[2]], accuracy = 0.1)}</strong> 
                  ({Patients[Duration_Match_Status == duration_categories[2]]} of {total})</li>
                <li>❓ <strong>{duration_categories[3]}</strong>: 
                  <strong>{percent(Proportion[Duration_Match_Status == duration_categories[3]], accuracy = 0.1)}</strong> 
                  ({Patients[Duration_Match_Status == duration_categories[3]]} of {total})</li>
              </ul>
              </div>"
            )
          },
          .groups = "drop"
        )
      
      # Reorder to show Facility-Wide first
      formatted_blocks <- formatted_blocks %>%
        mutate(order = ifelse(PlotLabel == "<b style='color:#0072B2;'>Facility-Wide</b>", 0, 1)) %>%
        arrange(order, PlotLabel) %>%
        select(-order)
      
      HTML(paste0(intro_text, paste(formatted_blocks$block, collapse = "\n")))
    })
    
  })
}