# Lower UTI Analysis Shiny Module - Complete Implementation (Corrected to match baseline)
# This module contains all the lower UTI-specific analysis functionality split into separate tabs

# Overview Tab UI
utiOutpatientOverviewUI <- function(id) {
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
               box(width = 12, title = "🎯 AWaRe Quality Indicators for Lower UTI", status = "info", solidHeader = TRUE,
                   h5("Within the gPPS data structure, the following antibiotic use quality indicators have been identified:"),
                   p("1) Proportion of patients presenting with lower UTI given an oral antibiotic."),
                   p("2) Proportion of patients presenting with lower UTI given oral antibiotics by AWaRe category (Access or Watch)."),
                   p("3) Proportion of patients presenting with lower UTI prescribed the total daily dose of oral antibiotics recommended in the WHO AWaRe Antibiotic Book."),
                   p("4) Proportion of patients presenting with lower UTI given the duration in days of oral antibiotics recommended in the WHO AWaRe Antibiotic Book.")
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
utiOutpatientOralUI <- function(id) {
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
                   p("Proportion of patients presenting with lower UTI given an oral antibiotic.")
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📈 Antibiotic Use by Route of Administration", status = "primary", solidHeader = TRUE,
                   p("This visual summarises the proportion of adult outpatients with lower UTIs given antibiotics by route of administration across specialties."),
                   div(style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                       withSpinner(plotlyOutput(ns("oral_plot"), height = "450px", width = "100%"), type = 4))
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📝 Summary of Antibiotic Use for Lower UTI by Route of Administration", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   htmlOutput(ns("oral_summary"))
               )
        )
      )
    )
  )
}

# AWaRe Category Tab UI
utiOutpatientAWaReUI <- function(id) {
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
                   p("Please upload your data files to view AWaRe category analysis.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📌 Indicator 2 - Oral Antibiotic Use by AWaRe", status = "primary", solidHeader = TRUE,
                   p("Proportion of patients presenting with lower UTI given oral antibiotics by AWaRe category (Access or Watch).")
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📈 Antibiotic Prescription by AWaRe Classification", status = "primary", solidHeader = TRUE,
                   p("This visual summarises the proportion of adult outpatients with lower UTI given antibiotics by AWaRe classification across specialties."),
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
                     tags$li("This count represents the number of unique patients who were prescribed at least one antibiotic within a specific WHO AWaRe category."),
                     tags$li("A patient is counted once for each distinct AWaRe category they received an antibiotic from."),
                     tags$li("The AWaRe classification divides antibiotics into three categories: Access (first-line, low resistance risk), Watch (higher resistance risk, requires monitoring), and Reserve (last-resort for resistant infections).")
                   )
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📝 Summary of Antibiotic Use for Lower UTI by AWaRe Classification", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   htmlOutput(ns("aware_summary"))
               )
        )
      )
    )
  )
}

# Choice Alignment Tab UI
utiOutpatientChoiceUI <- function(id) {
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
                   p("Please upload your data files to view choice appropriateness analysis.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📌 Antibiotic Choice Alignment with AWaRe Book Recommendations for Lower UTI", status = "primary", solidHeader = TRUE,
                   p("Proportion of patients presenting with lower UTI prescribed the recommended oral antibiotic choice in the WHO AWaRe Antibiotic Book.")
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "🔍 WHO AWaRe Book Recommendation:", status = "primary", solidHeader = TRUE,
                   tags$ul(
                     tags$li(strong("Amoxicillin+clavulanic acid"), " 500 mg+125 mg every 8 hours (ORAL) for 3-5 days"),
                     tags$li(strong("Nitrofurantoin"), " 100 mg every 12 hours OR 50 mg every 6 hours (ORAL) for 5 days"),
                     tags$li(strong("Sulfamethoxazole+trimethoprim"), " 800 mg+160 mg every 12 hours (ORAL) for 3 days"),
                     tags$li(strong("Trimethoprim"), " 200 mg every 12 hours (ORAL) for 3 days")
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
                   " Antibiotic Choice Alignment Analysis for Lower UTI"
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
                       p("This visual summarises the proportion of adult outpatients with lower UTIs prescribed the recommended oral antibiotic choice in the WHO AWaRe Antibiotic Book across specialties."),
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
                       p("This visual shows the distribution of recommended oral antibiotics for lower UTIs by WHO AWaRe classification."),
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
               box(width = 12, title = "📝 Summary of Antibiotic Choice Appropriateness for Lower UTI", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   htmlOutput(ns("choice_summary"))
               )
        )
      )
    )
  )
}

# Dosage Alignment Tab UI
utiOutpatientDosageUI <- function(id) {
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
                   p("Please upload your data files to view dosage appropriateness analysis.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📌 Indicator 3 – Oral Antibiotic Choice & Dosage Appropriateness", status = "primary", solidHeader = TRUE,
                   p("Proportion of patients presenting with lower UTI prescribed the total daily dose of oral antibiotics recommended in the WHO AWaRe Antibiotic Book.")
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "🔍 WHO AWaRe Book Recommendation:", status = "primary", solidHeader = TRUE,
                   tags$ul(
                     tags$li(strong("Amoxicillin+clavulanic acid"), " 500 mg+125 mg every 8 hours (ORAL) for 3-5 days"),
                     tags$li(strong("Nitrofurantoin"), " 100 mg every 12 hours OR 50 mg every 6 hours (ORAL) for 5 days"),
                     tags$li(strong("Sulfamethoxazole+trimethoprim"), " 800 mg+160 mg every 12 hours (ORAL) for 3 days"),
                     tags$li(strong("Trimethoprim"), " 200 mg every 12 hours (ORAL) for 3 days")
                   )
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📈 Antibiotic Choice & Dosage Appropriateness for Lower UTI", status = "primary", solidHeader = TRUE,
                   p("This visual summarises the proportion of adult outpatients with lower UTIs prescribed the total daily dose of oral antibiotics recommended in the WHO AWaRe Antibiotic Book across specialties."),
                   div(style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                       withSpinner(plotlyOutput(ns("dosage_plot"), height = "450px", width = "100%"), type = 4))
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📝 Summary of Antibiotic Choice & Dosage Appropriateness for Lower UTI", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   htmlOutput(ns("dosage_summary"))
               )
        )
      )
    )
  )
}

# Duration Alignment Tab UI
utiOutpatientDurationUI <- function(id) {
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
                   p("Please upload your data files to view duration appropriateness analysis.")
               )
        )
      )
    ),
    
    conditionalPanel(
      condition = "output.dataUploaded == true",
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📌 Indicator 4 – Oral Antibiotic Duration Appropriateness", status = "primary", solidHeader = TRUE,
                   p("Proportion of patients presenting with lower UTI given the duration in days of oral antibiotics recommended in the WHO AWaRe Antibiotic Book.")
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "🔍 WHO AWaRe Book Recommendation:", status = "primary", solidHeader = TRUE,
                   tags$ul(
                     tags$li(strong("Amoxicillin+clavulanic acid"), " 500 mg+125 mg every 8 hours (ORAL) for ", strong("3-5 days")),
                     tags$li(strong("Nitrofurantoin"), " 100 mg every 12 hours OR 50 mg every 6 hours (ORAL) for ", strong("5 days")),
                     tags$li(strong("Sulfamethoxazole+trimethoprim"), " 800 mg+160 mg every 12 hours (ORAL) for ", strong("3 days")),
                     tags$li(strong("Trimethoprim"), " 200 mg every 12 hours (ORAL) for ", strong("3 days"))
                   )
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📈 Antibiotic Duration Appropriateness for Lower UTI", status = "primary", solidHeader = TRUE,
                   p("This visual summarises the proportion of adult outpatients with lower UTIs given the duration in days of oral antibiotics recommended in the WHO AWaRe Antibiotic Book across specialties."),
                   div(style = "display: flex; justify-content: center; align-items: center; max-width: 100%; overflow: hidden;",
                       withSpinner(plotlyOutput(ns("duration_plot"), height = "450px", width = "100%"), type = 4))
               )
        )
      ),
      
      # Note BEFORE Summary
      fluidRow(
        column(10, offset = 1,
               div(class = "info-box",
                   style = "background-color: #f8f9fa; padding: 15px; border-left: 5px solid #17a2b8; border-radius: 5px; margin-top: 15px;",
                   strong("💡 Note:"),
                   " Duration ranges indicate the acceptable treatment periods as specified in the WHO AWaRe Antibiotic Book. Treatment durations falling within these ranges are considered appropriate."
               )
        )
      ),
      
      fluidRow(
        column(10, offset = 1,
               box(width = 12, title = "📝 Summary of Antibiotic Duration Appropriateness for Lower UTI", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   htmlOutput(ns("duration_summary"))
               )
        )
      )
    )
  )
}

# Module Server - Handles all lower UTI tabs
utiOutpatientAnalysisServer <- function(id, data_reactive) {
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
        
        # Filter eligible lower UTI patients
        data_UTI <- data_patients %>%
          filter(`Diagnosis code` == "UTI") %>%
          mutate(
            Route = toupper(as.character(Route)),
            AWaRe_compatible = (`Patient age group` == "ADULT")
          ) 
        
        # Count eligible unique lower UTI cases
        eligible_UTI_n <- data_UTI %>%
          filter(AWaRe_compatible) %>%
          distinct(`Unique Patient ID`) %>%
          nrow()
        
        # Build status message
        status_message <- if(eligible_UTI_n == 0) {
          "<div style='background-color:#fff3cd; border: 1px solid #ffeeba; padding: 10px; border-radius: 3px; margin-top: 10px;'>
            <strong>🚫 No eligible cases found:</strong> There were no eligible cases for evaluation during this survey period. Please verify data availability.
          </div>"
        } else if(eligible_UTI_n < 10) {
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
          "This script applies <strong>WHO AWaRe Quality Indicators</strong> to adult outpatients with lower UTI.",
          "</p>",
          "<ul>",
          "<li><strong>gPPS recorded symptoms:</strong> Painful/frequent urination</li>",
          "<li><strong>Total eligible cases:</strong> ", eligible_UTI_n, "</li>",
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
      total_UTI <- data_patients %>%
        filter(`Diagnosis code` == "UTI") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_UTI_adults <- data_patients %>%
        filter(`Diagnosis code` == "UTI", `Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_UTI_adults_abx <- data_patients %>%
        filter(`Diagnosis code` == "UTI", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_UTI_adults_prescriptions <- data_patients %>%
        filter(`Diagnosis code` == "UTI", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        nrow()
      
      insight_cards <- HTML(glue::glue(
        "<div style='display: flex; gap: 12px; margin-top: 15px; flex-wrap: wrap;'>
        
        <div style='flex: 1; min-width: 250px; background-color: #e6f4ea; border-left: 6px solid #009E73; padding: 12px; border-radius: 5px;'>
          <strong>Eligible Patients with Lower UTI (Adults):</strong><br>
          <span style='font-size: 1.3em;'>{total_UTI_adults}</span><br>
          <small>{round(100 * total_UTI_adults / total_UTI, 1)}% (of {total_UTI} patients with lower UTI)</small>
        </div>
        
        <div style='flex: 1; min-width: 250px; background-color: #fff3cd; border-left: 6px solid #D55E00; padding: 12px; border-radius: 5px;'>
          <strong>Treatment Rate (Abx):</strong><br>
          <span style='font-size: 1.3em;'>{round(100 * total_UTI_adults_abx / total_UTI_adults, 1)}%</span><br>
          <small>({total_UTI_adults_abx} of {total_UTI_adults} eligible patients)</small>
        </div>
        
        <div style='flex: 1; min-width: 250px; background-color: #f0f8ff; border-left: 6px solid #0072B2; padding: 12px; border-radius: 5px;'>
          <strong>Total Antibiotic Prescriptions:</strong><br>
          <span style='font-size: 1.3em;'>{total_UTI_adults_prescriptions}</span>
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
        filter(`Diagnosis code` == "UTI") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      patient_summary <- data.frame(
        Category = c(
          "Number of all patients with a diagnosis of lower UTI",
          "Number of adult patients (≥18 years) with lower UTI",
          "Number of eligible patients: Adult patients with lower UTI who received antibiotics"
        ),
        Count = c(
          total_patients,
          data_patients %>%
            filter(`Diagnosis code` == "UTI", `Patient age group` == "ADULT") %>%
            distinct(`Unique Patient ID`) %>%
            nrow(),
          data_patients %>%
            filter(`Diagnosis code` == "UTI", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
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
        filter(`Diagnosis code` == "UTI") %>%
        nrow()
      
      prescription_summary <- data.frame(
        Category = c(
          "Number of all antibiotic prescriptions for patients diagnosed with lower UTI",
          "Number of antibiotic prescriptions for adult patients with lower UTI",
          "Number of eligible antibiotic prescriptions"
        ),
        Count = c(
          total_prescriptions,
          data_patients %>% filter(`Diagnosis code` == "UTI", `Patient age group` == "ADULT") %>% nrow(),
          data_patients %>% filter(`Diagnosis code` == "UTI", `Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>% nrow()
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
      data_UTI <- data_patients %>%
        filter(`Diagnosis code` == "UTI", `Patient age group` == "ADULT") %>%
        mutate(
          Route = toupper(Route),
          ATC5 = trimws(toupper(ATC5)),
          AWaRe = toupper(AWaRe),
          `Type of specialty` = trimws(`Type of specialty`)
        )
      
      # Patient-level summary
      rti_oral_summary <- data_UTI %>%
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
      oral_summary_by_spec <- rti_oral_summary %>%
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
      oral_summary_total <- rti_oral_summary %>%
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
        data_UTI = data_UTI,
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
      data_UTI <- oral_data$data_UTI
      
      # Denominator
      total_eligible <- data_UTI %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_eligible_with_abx <- data_UTI %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      intro_text <- glue::glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received any antibiotic for lower UTIs (<strong>{total_eligible_with_abx}</strong> out of {total_eligible}).
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
    
    # Oral plot - NO GRAY HIGHLIGHT, WITH HOVER TEXT
    output$oral_plot <- renderPlotly({
      oral_data <- oral_data_reactive()
      if(length(oral_data) == 0) {
        return(plotly_empty())
      }
      
      oral_summary_final <- oral_data$oral_summary_final
      
      # Prepare long-format data with hover text
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
      
      # Bar plot with hover text
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
      
      # Convert to plotly with hover text
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Proportion of Patients with Lower UTI by Antibiotic Route</b>",
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
    
    # AWaRe data
    aware_data_reactive <- reactive({
      if (!check_data()) {
        return(list())
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      
      data_UTI <- data_patients %>%
        filter(`Diagnosis code` == "UTI", `Patient age group` == "ADULT") %>%
        mutate(
          Route = toupper(Route),
          ATC5 = trimws(toupper(ATC5)),
          AWaRe = toupper(AWaRe),
          `Type of specialty` = trimws(`Type of specialty`)
        )
      
      # Expected AWaRe categories
      expected_aware <- c("Access", "Watch", "Reserve", "Not Recommended", "Unclassified")
      
      # Widen data
      aware_summary <- data_UTI %>%
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
        data_UTI = data_UTI,
        aware_final = aware_final,
        aware_summary = aware_summary
      )
    })
    
    # AWaRe summary
    output$aware_summary <- renderUI({
      aware_data <- aware_data_reactive()
      if(length(aware_data) == 0) {
        return(HTML("<p>No data available</p>"))
      }
      
      aware_final <- aware_data$aware_final
      data_UTI <- aware_data$data_UTI
      
      # Denominator
      total_eligible2 <- data_UTI %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      total_eligible_with_abx2 <- data_UTI %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx, Route == "O") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      intro_text2 <- glue::glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received any oral antibiotic for lower UTIs (<strong>{total_eligible_with_abx2}</strong> out of {total_eligible2}).
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
    
    # AWaRe plot - NO GRAY HIGHLIGHT, WITH HOVER TEXT
    output$aware_plot <- renderPlotly({
      aware_data <- aware_data_reactive()
      if(length(aware_data) == 0) {
        return(plotly_empty())
      }
      
      aware_final <- aware_data$aware_final
      
      # Prepare plot data with hover text
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
      
      # Format labels
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
      
      # Plot with hover text
      p <- ggplot(aware_plot_data, aes(y = PlotLabel, x = Proportion, fill = AWaRe_Cat, text = hover_text)) +
        geom_bar(stat = "identity", position = "fill", width = 0.85) +
        geom_text(
          aes(label = ifelse(Proportion > 0.05, scales::percent(Proportion, accuracy = 1), "")),
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
          axis.text.y = ggtext::element_markdown(size = 7),
          axis.text.x = element_text(size = 7),
          axis.title = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(size = 8),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          plot.margin = margin(6, 18, 6, 8)
        ) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE, title.position = "top"))
      
      # Convert to plotly with hover text
      r_margin <- 40 + round(300 * x_buffer)
      
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Distribution of Oral Antibiotic Use in Lower UTI by AWaRe Category</b>",
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
    
    # Choice data reactive - matching R Markdown exactly with ATC5
    choice_data_reactive <- reactive({
      if (!check_data()) {
        return(list())
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      data_lookup <- data$data_lookup
      
      # Prepare the data - use ATC5 instead of Antimicrobial name
      data_UTI_choice <- data_patients %>%
        filter(`Diagnosis code` == "UTI") %>%
        mutate(
          Route = toupper(Route),
          ATC5 = trimws(toupper(ATC5)),
          AWaRe_compatible = ifelse(
            `Patient age group` == "ADULT" & AWaRe %in% AWaRe_abx,
            TRUE, FALSE
          )
        )
      
      # Lookup ABX names using ATC5
      lookup_names <- data_lookup %>%
        filter(Code == "PC_UTI_APPROP_DOSAGE_ORAL_ABX") %>%
        select(starts_with("ABX-ATC")) %>%
        unlist(use.names = FALSE)
      
      # Flag matched prescriptions
      data_UTI_choice <- data_UTI_choice %>%
        mutate(
          Drug_Match = ATC5 %in% lookup_names
        )
      
      # Extract lookup info
      lookup_UTI <- data_lookup %>%
        filter(Code == "PC_UTI_APPROP_DOSAGE_ORAL_ABX")
      
      # Create long format from lookup
      lookup_long <- tibble(
        Drug = unlist(lookup_UTI %>% select(starts_with("ABX-ATC")), use.names = FALSE),
        Choice = unlist(lookup_UTI %>% select(starts_with("ABX-CHOICE")), use.names = FALSE)
      ) %>%
        filter(!is.na(Drug))
      
      # Merge choice info with patient-level data
      data_UTI_choice <- data_UTI_choice %>%
        left_join(lookup_long, by = c("ATC5" = "Drug"))
      
      # Patient Summary
      patient_summary <- data_UTI_choice %>%
        filter(AWaRe_compatible) %>%
        group_by(`Unique Patient ID`, `Type of specialty`) %>%
        summarise(
          All_ABX = list(unique(ATC5)),
          
          # Match flags
          Match_1 = any(ATC5 == lookup_names[1]),
          Match_2 = any(ATC5 == lookup_names[2]),
          Match_3 = any(ATC5 == lookup_names[3]),
          Match_4 = any(ATC5 == lookup_names[4]),
          
          Match_1_O = any(ATC5 == lookup_names[1] & Route == "O"),
          Match_2_O = any(ATC5 == lookup_names[2] & Route == "O"),
          Match_3_O = any(ATC5 == lookup_names[3] & Route == "O"),
          Match_4_O = any(ATC5 == lookup_names[4] & Route == "O"),
          
          N_ABX = n_distinct(ATC5),
          Any_O = any(Route == "O"),
          .groups = "drop"
        ) %>%
        rowwise() %>%
        mutate(
          Num_Recommended_O = sum(c_across(Match_1_O:Match_4_O)),
          
          # Fully appropriate: only one antibiotic, and it's recommended
          Appropriate = Num_Recommended_O == 1 & N_ABX == 1,
          
          # Partial appropriate: ≥1 recommended drug, but given with others
          Partial_Appropriate = Num_Recommended_O >= 1 & N_ABX > 1,
          
          # Inappropriate Oral use (no recommended drugs via Oral)
          No_Appropriate = Any_O & !Appropriate & !Partial_Appropriate,
          
          # All other inappropriate cases
          No_Appropriate_others = !Appropriate & !Partial_Appropriate & !No_Appropriate
        ) %>%
        ungroup()
      
      # Get not eligible patients
      no_abx_patients <- data_UTI_choice %>%
        group_by(`Unique Patient ID`, `Type of specialty`) %>%
        summarise(
          Is_Adult = all(`Patient age group` == "ADULT"),
          Any_AWaRe_ABX = all(AWaRe %in% AWaRe_abx),
          .groups = "drop"
        ) %>%
        filter(Is_Adult, !Any_AWaRe_ABX)
      
      not_eligible_patients <- data_UTI_choice %>%
        group_by(`Unique Patient ID`, `Type of specialty`) %>%
        summarise(Ineligible = all(`Patient age group` != "ADULT"), .groups = "drop") %>%
        filter(Ineligible)
      
      # Create summary table with Type of specialty
      eligible_long <- patient_summary %>%
        select(`Unique Patient ID`, `Type of specialty`, Appropriate, Partial_Appropriate, No_Appropriate, No_Appropriate_others) %>%
        pivot_longer(
          cols = -c(`Unique Patient ID`, `Type of specialty`),
          names_to = "Indicator",
          values_to = "Value"
        ) %>%
        mutate(Value = as.logical(Value)) %>%
        filter(Value) %>%
        group_by(`Type of specialty`, Indicator) %>%
        summarise(Patients = n(), .groups = "drop")
      
      # Get all possible combinations
      all_combos <- expand_grid(
        `Type of specialty` = unique(patient_summary$`Type of specialty`),
        Indicator = c("Appropriate", "Partial_Appropriate", "No_Appropriate", "No_Appropriate_others", "Not_Eligible", "No_ABX")
      )
      
      # Left join and replace NAs
      eligible_long <- all_combos %>%
        left_join(eligible_long, by = c("Type of specialty", "Indicator")) %>%
        mutate(Patients = replace_na(Patients, 0))
      
      # Add not eligible
      ineligible_summary <- not_eligible_patients %>%
        count(`Type of specialty`) %>%
        mutate(Indicator = "Not_Eligible") %>%
        rename(Patients = n)
      
      # Add no patients
      no_abx_summary <- no_abx_patients %>%
        count(`Type of specialty`) %>%
        mutate(Indicator = "No_ABX") %>%
        rename(Patients = n)
      
      # Combine
      qi_long <- bind_rows(eligible_long, ineligible_summary, no_abx_summary)
      
      # Calculate total per Specialty
      dept_totals <- qi_long %>%
        group_by(`Type of specialty`) %>%
        summarise(Total = sum(Patients), .groups = "drop")
      
      # Final summary table
      qi_summary_UTI <- qi_long %>%
        left_join(dept_totals, by = "Type of specialty") %>%
        mutate(
          Indicator = case_when(
            Indicator == "Appropriate" ~ "Received recommended oral antibiotics",
            Indicator == "Partial_Appropriate" ~ "Partially received recommended oral antibiotics",
            Indicator == "No_Appropriate" ~ "Received oral antibiotics not among recommended options",
            Indicator == "No_Appropriate_others" ~ "Received other non-oral antibiotics",
            Indicator == "No_ABX" ~ "No antibiotics given",
            Indicator == "Not_Eligible" ~ "Not eligible for AWaRe QIs",
            TRUE ~ Indicator
          ), 
          Proportion = round(100 * Patients / Total, 1)
        ) %>%
        select(`Type of specialty`, Indicator, Patients, Total, Proportion)
      
      # Add Facility-Wide row
      hospital_data <- qi_summary_UTI %>%
        group_by(Indicator) %>%
        summarise(Patients = sum(Patients), .groups = "drop") %>%
        mutate(`Type of specialty` = "Facility-Wide") %>%
        select(`Type of specialty`, Indicator, Patients)
      
      # Combine
      qi_summary_UTI <- bind_rows(qi_summary_UTI, hospital_data)
      
      # Recalculate totals
      qi_summary_UTI <- qi_summary_UTI %>%
        group_by(`Type of specialty`) %>%
        mutate(
          Total = sum(Patients),
          Proportion = Patients / Total
        ) %>%
        ungroup()
      
      # Format labels
      qi_summary_UTI <- qi_summary_UTI %>%
        mutate(`Type of specialty` = ifelse(`Type of specialty` == "Facility-Wide", "Facility-Wide", `Type of specialty`))
      
      list(
        data_UTI_choice = data_UTI_choice,
        patient_summary = patient_summary,
        qi_summary_UTI = qi_summary_UTI
      )
    })
    
    # Choice plot (main alignment) - NO GRAY HIGHLIGHT, WITH HOVER TEXT
    output$choice_plot <- renderPlotly({
      choice_data <- choice_data_reactive()
      if (length(choice_data) == 0) {
        return(plotly_empty() %>% layout(title = "No eligible data available"))
      }
      
      qi_summary_UTI <- choice_data$qi_summary_UTI
      
      # Define all expected categories
      all_expected_categories <- c(
        "Received recommended oral antibiotics",
        "Partially received recommended oral antibiotics",
        "Received oral antibiotics not among recommended options",
        "Received other non-oral antibiotics",
        "No antibiotics given",
        "Not eligible for AWaRe QIs"
      )
      
      # Get all unique specialties
      all_specialties <- unique(qi_summary_UTI$`Type of specialty`)
      
      # Create complete grid of all combinations
      complete_grid <- expand_grid(
        `Type of specialty` = all_specialties,
        Indicator = all_expected_categories
      )
      
      # Join with actual data and fill missing combinations with zeros
      qi_summary_UTI <- complete_grid %>%
        left_join(qi_summary_UTI, by = c("Type of specialty", "Indicator")) %>%
        group_by(`Type of specialty`) %>%
        mutate(
          Patients = replace_na(Patients, 0),
          Total = sum(Patients, na.rm = TRUE),
          Proportion = if_else(Total > 0, Patients / Total, 0)
        ) %>%
        ungroup()
      
      # Define colors
      drug_choice_colors <- c(
        "Received recommended oral antibiotics" = "#1F77B4",
        "Partially received recommended oral antibiotics" = "#4FA9DC",
        "Received oral antibiotics not among recommended options" = "#EF476F",
        "Received other non-oral antibiotics" = "#D3D3D3",
        "No antibiotics given" = "#F9D99E",
        "Not eligible for AWaRe QIs" = "#A9A9A9"
      )
      
      # Create styled labels and hover text
      qi_summary_UTI <- qi_summary_UTI %>%
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
              sort(setdiff(unique(`Type of specialty`), "Facility-Wide"))
            )
          ),
          Indicator = factor(
            Indicator,
            levels = all_expected_categories
          ),
          hover_text = paste0(
            "<b>Specialty:</b> ", gsub("<.*?>", "", as.character(PlotLabel)), "<br>",
            "<b>Category:</b> ", Indicator, "<br>",
            "<b>Patients:</b> ", Patients, "<br>",
            "<b>Total:</b> ", Total, "<br>",
            "<b>Proportion:</b> ", round(Proportion * 100, 1), "%"
          )
        )
      
      # Plot with hover text
      p <- ggplot(qi_summary_UTI, aes(x = PlotLabel, y = Proportion, fill = Indicator, text = hover_text)) +
        geom_bar(stat = "identity", position = "fill", width = 0.7) +
        geom_text(
          aes(label = ifelse(Patients > 0, Patients, "")),
          position = position_fill(vjust = 0.5),
          size = 2.6, color = "black"
        ) +
        geom_text(
          data = qi_summary_UTI %>% distinct(PlotLabel, Total),
          aes(x = PlotLabel, y = 1.02, label = paste0("n=", Total)),
          inherit.aes = FALSE,
          size = 3, color = "gray30", hjust = 0.5
        ) +
        scale_fill_manual(values = drug_choice_colors, drop = FALSE) +
        scale_y_continuous(limits = c(0, 1.08), expand = c(0, 0),
                           labels = scales::percent_format(accuracy = 1)) +
        scale_x_discrete(expand = c(0.01, 0.01)) +
        labs(
          x = "Specialty",
          y = "Proportional Stacked Bars",
          fill = "Treatment Appropriateness Category"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.x = ggtext::element_markdown(size = 7, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 7),
          axis.title = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(size = 8, margin = margin(b = 4)),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          plot.margin = margin(6, 8, 6, 8)
        ) +
        guides(
          fill = guide_legend(nrow = 3, byrow = TRUE, title.position = "top")
        )
      
      # Convert to plotly with hover text
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Antibiotic Choice Alignment for Lower UTI</b>",
            x = 0.5,
            xanchor = "center",
            y = 0.97,
            yanchor = "top",
            font = list(size = 12)
          ),
          height = 450,
          width = 680,
          margin = list(l = 30, r = 30, t = 60, b = 200),
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center",
            y = -0.75,
            yanchor = "top",
            font = list(size = 9),
            title = list(text = "<b>Treatment Alignment Category</b>", font = list(size = 9))
          ),
          bargap = 0,
          bargroupgap = 0
        )
      
      plt
    })
    
    # Choice AWaRe plot - NO GRAY HIGHLIGHT, WITH HOVER TEXT
    output$choice_aware_plot <- renderPlotly({
      choice_data <- choice_data_reactive()
      if (length(choice_data) == 0) {
        return(plotly_empty())
      }
      
      data_UTI_choice <- choice_data$data_UTI_choice
      patient_summary <- choice_data$patient_summary
      
      data <- data_reactive()
      data_lookup <- data$data_lookup
      
      # Lookup names using ATC5
      lookup_names <- data_lookup %>%
        filter(Code == "PC_UTI_APPROP_DOSAGE_ORAL_ABX") %>%
        select(starts_with("ABX-ATC")) %>%
        unlist(use.names = FALSE)
      
      # Patient summary by AWaRe
      patient_summary_AWARE <- data_UTI_choice %>%
        filter(`Patient age group` == "ADULT", AWaRe %in% AWaRe_abx) %>%
        group_by(`Unique Patient ID`, `Type of specialty`, AWaRe) %>%
        summarise(
          N_total = n(),
          N_match = sum(Drug_Match),
          N_O = sum(Route == "O"),
          N_O_match = sum(Route == "O" & Drug_Match),
          
          Match_1_O = any(ATC5 == lookup_names[1] & Route == "O"),
          Match_2_O = any(ATC5 == lookup_names[2] & Route == "O"),
          Match_3_O = any(ATC5 == lookup_names[3] & Route == "O"),
          Match_4_O = any(ATC5 == lookup_names[4] & Route == "O"),
          
          .groups = "drop"
        ) %>%
        mutate(
          Appropriate_oral = (Match_1_O | Match_2_O | Match_3_O | Match_4_O)
        )
      
      # Summary by specialty and AWaRe
      AWaRe_long <- patient_summary_AWARE %>%
        filter(Appropriate_oral) %>%
        group_by(`Type of specialty`, AWaRe) %>%
        summarise(Patients = n(), .groups = "drop")
      
      # Calculate totals
      AWaRe_dept_totals <- AWaRe_long %>%
        group_by(`Type of specialty`) %>%
        summarise(Total = sum(Patients), .groups = "drop")
      
      AWaRe_long <- AWaRe_long %>%
        left_join(AWaRe_dept_totals, by = "Type of specialty") %>%
        mutate(Proportion = round(100 * Patients / Total, 1))
      
      # Add Facility-Wide
      AWaRe_hospital_data <- AWaRe_long %>%
        group_by(AWaRe) %>%
        summarise(Patients = sum(Patients), .groups = "drop") %>%
        mutate(`Type of specialty` = "Facility-Wide") %>%
        select(`Type of specialty`, AWaRe, Patients)
      
      AWaRe_long <- bind_rows(AWaRe_long, AWaRe_hospital_data)
      
      # Set stacking order
      aware_levels_stack <- c("WATCH", "ACCESS")
      AWaRe_long$AWaRe <- factor(AWaRe_long$AWaRe, levels = aware_levels_stack)
      
      # Ensure all combos exist
      all_combos3 <- expand_grid(
        `Type of specialty` = unique(AWaRe_long$`Type of specialty`),
        AWaRe = aware_levels_stack
      )
      
      AWaRe_long <- all_combos3 %>%
        left_join(AWaRe_long, by = c("Type of specialty", "AWaRe")) %>%
        mutate(
          Patients = replace_na(Patients, 0),
          Total = replace_na(Total, 0),
          Proportion = replace_na(Proportion, 0)
        )
      
      # Recalculate
      AWaRe_long <- AWaRe_long %>%
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
      
      # Create styled labels
      AWaRe_long <- AWaRe_long %>%
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
      
      # Plot with hover text
      p <- ggplot(AWaRe_long, aes(x = PlotLabel, y = Proportion, fill = AWaRe, text = hover_text)) +
        geom_bar(stat = "identity", position = "fill", width = 0.85) +
        geom_text(
          aes(label = ifelse(Proportion > 0.05, scales::percent(Proportion, accuracy = 1), "")),
          position = position_fill(vjust = 0.5),
          size = 3, color = "black"
        ) +
        geom_text(
          data = AWaRe_long %>% distinct(PlotLabel, Total),
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
          axis.text.y = ggtext::element_markdown(size = 7),
          axis.text.x = element_text(size = 7),
          axis.title = element_text(face = "bold", size = 10),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(size = 8),
          plot.margin = margin(6, 8, 6, 8)
        ) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE, title.position = "top"))
      
      # Convert to plotly with hover text
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
          yaxis = list(automargin = TRUE, categoryorder = "array", categoryarray = rev(levels(AWaRe_long$PlotLabel))),
          xaxis = list(automargin = TRUE)
        )
      
      plt
    })
    
    # Choice summary
    output$choice_summary <- renderUI({
      choice_data <- choice_data_reactive()
      if(length(choice_data) == 0) {
        return(HTML("<p>No data available for choice summary</p>"))
      }
      
      qi_summary_UTI <- choice_data$qi_summary_UTI
      data_UTI_choice <- choice_data$data_UTI_choice
      
      # Filter Facility-Wide
      qi_summary_UTI_filtered <- qi_summary_UTI %>%
        filter(`Type of specialty` != "Facility-Wide")
      
      # Denominator
      total_eligible <- qi_summary_UTI_filtered %>%
        filter(!Indicator %in% c("Not eligible for AWaRe QIs")) %>%
        summarise(Total = sum(Patients, na.rm = TRUE)) %>%
        pull(Total)
      
      # Total receiving oral antibiotics
      total_iv <- qi_summary_UTI_filtered %>%
        filter(Indicator %in% c(
          "Received recommended oral antibiotics",
          "Partially received recommended oral antibiotics",
          "Received oral antibiotics not among recommended options"
        )) %>%
        summarise(Total = sum(Patients, na.rm = TRUE)) %>%
        pull(Total)
      
      # Extract relevant data
      relevant_data <- qi_summary_UTI_filtered %>%
        filter(Indicator %in% c(
          "Received recommended oral antibiotics",
          "Partially received recommended oral antibiotics",
          "Received oral antibiotics not among recommended options"
        ))
      
      # No data check
      if (nrow(relevant_data) == 0) {
        return(HTML(
          "<div style='background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 14px; margin-top: 10px;'>
          ⚠️ <strong>No summary available</strong> — there are currently no eligible patients for assessing oral antibiotic choice appropriateness for lower UTI.<br><br>
          <em>This may indicate that no patients received oral antibiotics, or none met the inclusion criteria during the reporting period.</em>
          </div>"
        ))
      }
      
      # Summarise data
      summary_data_UTI <- relevant_data %>%
        group_by(`Type of specialty`, Indicator) %>%
        summarise(Patients = sum(Patients), .groups = "drop") %>%
        pivot_wider(
          names_from = Indicator,
          values_from = Patients,
          values_fill = 0
        ) %>%
        mutate(
          Total = `Received recommended oral antibiotics` +
            `Partially received recommended oral antibiotics` +
            `Received oral antibiotics not among recommended options`,
          Prop_Full = `Received recommended oral antibiotics` / Total,
          Prop_Partial = `Partially received recommended oral antibiotics` / Total,
          Prop_None = `Received oral antibiotics not among recommended options` / Total
        ) %>%
        filter(Total > 0)
      
      # No data after filtering
      if (nrow(summary_data_UTI) == 0) {
        return(HTML(
          "<div style='background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 14px; margin-top: 10px;'>
          ⚠️ <strong>No summary available</strong> — there are currently no eligible patients for assessing oral antibiotic choice appropriateness for lower UTI.<br><br>
          <em>This may indicate that no patients received oral antibiotics, or none met the inclusion criteria during the reporting period.</em>
          </div>"
        ))
      }
      
      # Create Facility-Wide row
      hospital_row <- summary_data_UTI %>%
        summarise(
          `Type of specialty` = "Facility-Wide",
          `Received recommended oral antibiotics` = sum(`Received recommended oral antibiotics`),
          `Partially received recommended oral antibiotics` = sum(`Partially received recommended oral antibiotics`),
          `Received oral antibiotics not among recommended options` = sum(`Received oral antibiotics not among recommended options`)
        ) %>%
        mutate(
          Total = `Received recommended oral antibiotics` +
            `Partially received recommended oral antibiotics` +
            `Received oral antibiotics not among recommended options`,
          Prop_Full = `Received recommended oral antibiotics` / Total,
          Prop_Partial = `Partially received recommended oral antibiotics` / Total,
          Prop_None = `Received oral antibiotics not among recommended options` / Total
        )
      
      # Combine
      summary_data_combined <- bind_rows(hospital_row, summary_data_UTI)
      
      # Intro
      intro_text <- glue::glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received any oral antibiotics for lower UTI (<strong>{total_iv}</strong> out of {total_eligible}).
        </div><br><br>
        <strong>Summary:</strong><br><br>"
      )
      
      # Format blocks
      formatted_blocks <- summary_data_combined %>%
        mutate(block = pmap_chr(
          list(
            `Type of specialty`,
            Prop_Full, `Received recommended oral antibiotics`,
            Prop_Partial, `Partially received recommended oral antibiotics`,
            Prop_None, `Received oral antibiotics not among recommended options`,
            Total
          ),
          function(dept, full_p, full_n, part_p, part_n, none_p, none_n, total_n) {
            color <- if (dept == "Facility-Wide") "#0072B2" else "#6c757d"
            bg <- if (dept == "Facility-Wide") "#f0f0f0" else "#ffffff"
            
            glue::glue(
              "<div style='background-color: {bg}; border-left: 5px solid {color}; padding: 14px; margin-bottom: 20px;'>
              <strong>🏥 {dept}</strong> <span style='color: #888;'>(n = {total_n} patients)</span><br><br>
              <ul style='margin-left: 1.2em; line-height: 1.7; padding-left: 0; list-style-type: none;'>
                <li>✅ <strong>Received recommended oral antibiotics</strong> (as per WHO AWaRe Book): <strong>{scales::percent(full_p, accuracy = 0.1)}</strong> ({full_n} out of {total_n})</li>
                <li>⚠️ <strong>Partially received recommended oral antibiotics</strong> (as per WHO AWaRe Book): <strong>{scales::percent(part_p, accuracy = 0.1)}</strong> ({part_n} out of {total_n})</li>
                <li>❌ <strong>Received oral antibiotics not among recommended options</strong> (as per WHO AWaRe Book): <strong>{scales::percent(none_p, accuracy = 0.1)}</strong> ({none_n} out of {total_n})</li>
              </ul>
              </div>"
            )
          }
        ))
      
      # Reorder to show Total first
      formatted_blocks <- formatted_blocks %>%
        mutate(order = ifelse(`Type of specialty` == "Facility-Wide", 0, 1)) %>%
        arrange(order, `Type of specialty`) %>%
        select(-order)
      
      # Final output
      final_summary_html <- HTML(paste0(intro_text, paste(formatted_blocks$block, collapse = "\n")))
      htmltools::browsable(final_summary_html)
    })
    
    # Dosage data reactive - CORRECTED TO MATCH BASELINE (2-tier system using ATC5)
    dosage_data_reactive <- reactive({
      if (!check_data()) {
        return(list())
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      data_lookup <- data$data_lookup
      
      # Filter and prepare - use ATC5
      data_UTI2 <- data_patients %>%
        filter(`Diagnosis code` == "UTI") %>%
        mutate(
          Route = toupper(Route),
          ATC5 = trimws(toupper(ATC5)),
          AWaRe_compatible = ifelse(
            `Patient age group` == "ADULT" & AWaRe %in% AWaRe_abx, 
            TRUE, FALSE
          )
        )
      
      # Lookup - use ABX-ATC instead of ABX-NAME
      lookup2 <- data_lookup %>% filter(Code == "PC_UTI_APPROP_DOSAGE_ORAL_ABX")
      lookup_names2 <- unlist(lookup2[1, c("ABX-ATC-1", "ABX-ATC-2", "ABX-ATC-3", "ABX-ATC-4")], use.names = FALSE)
      lookup_names2 <- toupper(trimws(lookup_names2))
      
      # Compute Total Daily Dose
      data_UTI2 <- data_UTI2 %>%
        mutate(
          Unit_Factor = case_when(
            Unit == "mg" ~ 1,
            Unit == "g"  ~ 1000,
            Unit == "IU" ~ 1,
            Unit == "MU" ~ 1e6,
            TRUE ~ NA_real_
          ),
          Total_Daily_Dose = as.numeric(`Single unit dose`) * as.numeric(`N Doses/day`) * Unit_Factor
        )
      
      # Match Drug + Dose + Route
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
      
      drug_names <- sapply(1:4, function(i) toupper(trimws(lookup2[[paste0("ABX-ATC-", i)]][1])))
      routes <- sapply(1:4, function(i) toupper(trimws(lookup2[[paste0("ABX-ROUTE-", i)]][1])))
      expected_doses <- sapply(1:4, get_expected_dose)
      
      # Special logic for Drug 2 (Nitrofurantoin - has alternative dosing)
      expected_2_main <- expected_doses[2]
      expected_2_alt <- as.numeric(lookup2[["ABX-DOSE-2a"]][1]) *
        as.numeric(lookup2[["ABX-DAY-DOSE-2a"]][1]) *
        convert_unit(lookup2[["ABX-UNIT-2"]][1])
      
      # Perform matching
      data_UTI2 <- data_UTI2 %>%
        mutate(
          Match_Drug_Dose_1 = ATC5 == drug_names[1] & Route == "O" &
            abs(Total_Daily_Dose - expected_doses[1]) < 1,
          
          Match_Drug_Dose_2 = ATC5 == drug_names[2] & Route == "O" &
            (abs(Total_Daily_Dose - expected_2_main) < 1 | abs(Total_Daily_Dose - expected_2_alt) < 1),
          
          Match_Drug_Dose_3 = ATC5 == drug_names[3] & Route == "O" &
            abs(Total_Daily_Dose - expected_doses[3]) < 1,
          
          Match_Drug_Dose_4 = ATC5 == drug_names[4] & Route == "O" &
            abs(Total_Daily_Dose - expected_doses[4]) < 1
        )
      
      # Patient summary - SIMPLIFIED 2-TIER LOGIC TO MATCH BASELINE
      patient_summary2 <- data_UTI2 %>%
        filter(AWaRe_compatible) %>%
        group_by(`Unique Patient ID`, `Type of specialty`) %>%
        summarise(
          Match_1_O = any(ATC5 == lookup_names2[1] & Route == "O"),
          Match_2_O = any(ATC5 == lookup_names2[2] & Route == "O"),
          Match_3_O = any(ATC5 == lookup_names2[3] & Route == "O"),
          Match_4_O = any(ATC5 == lookup_names2[4] & Route == "O"),
          
          Dose_1_OK = any(Match_Drug_Dose_1),
          Dose_2_OK = any(Match_Drug_Dose_2),
          Dose_3_OK = any(Match_Drug_Dose_3),
          Dose_4_OK = any(Match_Drug_Dose_4),
          
          Any_O = any(Route == "O"),
          .groups = "drop"
        ) %>%
        mutate(
          Any_Match = Match_1_O | Match_2_O | Match_3_O | Match_4_O,
          Any_Correct_Dose = 
            (Match_1_O & Dose_1_OK) |
            (Match_2_O & Dose_2_OK) |
            (Match_3_O & Dose_3_OK) |
            (Match_4_O & Dose_4_OK),
          
          # SIMPLIFIED 2-TIER LOGIC MATCHING BASELINE
          Dose_Result = case_when(
            Any_Correct_Dose ~ "Received recommended oral antibiotic with recommended dosage",
            Any_Match & !Any_Correct_Dose ~ "Received recommended oral antibiotic without recommended dosage",
            Any_O & !Any_Match ~ "Received oral antibiotics not among recommended options",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(Dose_Result))
      
      # Summary by specialty
      iv_dose_counts <- patient_summary2 %>%
        count(`Type of specialty`, Dose_Result, name = "Patients")
      
      # Define expected categories (2-tier system)
      expected_categories <- c(
        "Received recommended oral antibiotic with recommended dosage",
        "Received recommended oral antibiotic without recommended dosage",
        "Received oral antibiotics not among recommended options"
      )
      
      all_combos2 <- expand_grid(
        `Type of specialty` = unique(patient_summary2$`Type of specialty`),
        Dose_Result = expected_categories
      )
      
      oral_dose_summary2 <- all_combos2 %>%
        left_join(iv_dose_counts, by = c("Type of specialty", "Dose_Result")) %>%
        mutate(Patients = replace_na(Patients, 0)) %>%
        group_by(`Type of specialty`) %>%
        mutate(Total = sum(Patients),
               Proportion = round(100 * Patients / Total, 1)) %>%
        ungroup()
      
      # Facility-Wide row
      hospital_row2 <- oral_dose_summary2 %>%
        group_by(Dose_Result) %>%
        summarise(Patients = sum(Patients), .groups = "drop") %>%
        mutate(`Type of specialty` = "Facility-Wide") %>%
        group_by(`Type of specialty`) %>%
        mutate(Total = sum(Patients),
               Proportion = round(100 * Patients / Total, 1)) %>%
        ungroup()
      
      final_summary2 <- bind_rows(oral_dose_summary2, hospital_row2) %>%
        arrange(`Type of specialty`, Dose_Result)
      
      list(
        data_UTI2 = data_UTI2,
        patient_summary2 = patient_summary2,
        final_summary2 = final_summary2
      )
    })
    
    # Dosage plot - NO GRAY HIGHLIGHT, WITH HOVER TEXT (2-tier system)
    output$dosage_plot <- renderPlotly({
      dosage_data <- dosage_data_reactive()
      if(length(dosage_data) == 0) {
        return(plotly_empty())
      }
      
      final_summary2 <- dosage_data$final_summary2
      
      if(nrow(final_summary2) == 0) {
        return(plotly_empty())
      }
      
      # Order factor with 2-tier categories
      expected_categories <- c(
        "Received recommended oral antibiotic with recommended dosage",
        "Received recommended oral antibiotic without recommended dosage",
        "Received oral antibiotics not among recommended options"
      )
      
      final_summary2$Dose_Result <- factor(
        final_summary2$Dose_Result,
        levels = expected_categories
      )
      
      # Create styled labels and hover text
      final_summary2 <- final_summary2 %>%
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
      
      ordered_labels <- c(
        "<b style='color:#0072B2;'>Facility-Wide</b>",
        sort(unique(final_summary2$PlotLabel[final_summary2$PlotLabel != "<b style='color:#0072B2;'>Facility-Wide</b>"]))
      )
      final_summary2$PlotLabel <- factor(final_summary2$PlotLabel, levels = rev(ordered_labels))
      
      # Label data
      label_data <- final_summary2 %>%
        distinct(`Type of specialty`, Total) %>%
        mutate(
          PlotLabel = ifelse(
            `Type of specialty` == "Facility-Wide",
            "<b style='color:#0072B2;'>Facility-Wide</b>",
            `Type of specialty`
          ),
          PlotLabel = factor(PlotLabel, levels = levels(final_summary2$PlotLabel))
        )
      
      # Color palette for 2-tier system
      dosage_colors <- c(
        "Received recommended oral antibiotic with recommended dosage" = "#084594",
        "Received recommended oral antibiotic without recommended dosage" = "#FC9272",
        "Received oral antibiotics not among recommended options" = "#D3D3D3"
      )
      
      # Plot with hover text
      p <- ggplot(final_summary2, aes(x = PlotLabel, y = Proportion, fill = Dose_Result, text = hover_text)) +
        geom_bar(stat = "identity", position = "fill", width = 0.85) +
        geom_text(
          aes(label = ifelse(Proportion > 3, paste0(round(Proportion * 1, 1), "%"), "")),
          position = position_fill(vjust = 0.5),
          size = 2.8, color = "black"
        ) +
        geom_text(
          data = label_data,
          aes(x = PlotLabel, y = 1.015, label = paste0("n=", Total)),
          inherit.aes = FALSE,
          size = 3, color = "gray30", hjust = 0
        ) +
        scale_fill_manual(
          breaks = expected_categories,
          values = dosage_colors
        ) +
        coord_flip(ylim = c(0, 1.05)) +
        labs(
          x = "Specialty",
          y = "Proportional Stacked Bars",
          fill = "Treatment Appropriateness Category"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.y = ggtext::element_markdown(size = 7),
          axis.text.x = element_text(size = 7),
          axis.title = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 9),
          legend.text = element_text(margin = margin(r = 2), size = 8),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = .6),
          plot.margin = margin(6, 8, 6, 8)
        ) +
        guides(
          fill = guide_legend(nrow = 3, byrow = TRUE, title.position = "top")
        )
      
      # Convert to plotly with hover text
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Antibiotic Choice & Dosage for Lower UTI</b>",
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
    
    # Dosage summary - CORRECTED TO MATCH BASELINE (2-tier system)
    output$dosage_summary <- renderUI({
      dosage_data <- dosage_data_reactive()
      if(length(dosage_data) == 0) {
        return(HTML("<p>No data available for dosage summary</p>"))
      }
      
      final_summary2 <- dosage_data$final_summary2
      data_UTI2 <- dosage_data$data_UTI2
      
      # Filter
      final_summary2_UTI_filtered <- final_summary2 %>%
        filter(`Type of specialty` != "Facility-Wide")
      
      # Define 2-tier categories
      dose_categories <- c(
        "Received recommended oral antibiotic with recommended dosage",
        "Received recommended oral antibiotic without recommended dosage"
      )
      
      total_approp_iv_given <- final_summary2_UTI_filtered %>%
        filter(Dose_Result %in% dose_categories) %>%
        summarise(Total = sum(Patients, na.rm = TRUE)) %>%
        pull(Total)
      
      relevant_dose_data <- final_summary2_UTI_filtered %>%
        filter(Dose_Result %in% dose_categories)
      
      if (nrow(relevant_dose_data) == 0) {
        return(HTML(
          "<div style='background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 14px; margin-top: 10px;'>
          ⚠️ <strong>No summary available</strong> — there are currently no eligible patients for assessing oral antibiotic dosage appropriateness.<br><br>
          <em>This may indicate that no patients met the inclusion criteria, or no patients received recommended oral antibiotics during the reporting period.</em>
          </div>"
        ))
      }
      
      # Total eligible
      total_eligible <- data_UTI2 %>%
        filter(`Patient age group` == "ADULT") %>%
        distinct(`Unique Patient ID`) %>%
        nrow()
      
      # Intro
      intro_text2 <- glue::glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received any recommended (<em>or partially recommended</em>) oral antibiotic choice based on WHO AWaRe Book (<strong>{total_approp_iv_given}</strong> out of {total_eligible}).
        </div><br><br>
        <strong>Summary:</strong><br><br>"
      )
      
      # Ensure all combos
      dept_list <- unique(final_summary2$`Type of specialty`)
      complete_summary <- expand_grid(
        `Type of specialty` = dept_list,
        Dose_Result = dose_categories
      ) %>%
        left_join(final_summary2, by = c("Type of specialty", "Dose_Result")) %>%
        mutate(
          Patients = replace_na(Patients, 0),
          Total = ave(Patients, `Type of specialty`, FUN = sum),
          Proportion = ifelse(Total == 0, 0, Patients / Total)
        )
      
      complete_summary <- complete_summary %>%
        filter(Total > 0)
      
      if (nrow(complete_summary) == 0) {
        return(HTML(
          "<div style='background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 14px; margin-top: 10px;'>
          ⚠️ <strong>No summary available</strong> — there are currently no eligible patients for evaluating the <strong>appropriateness of oral antibiotic dosage</strong> for lower UTI.<br><br>
          <em>This may indicate that no patients met the inclusion criteria, or no patients received recommended oral antibiotics during the reporting period.</em>
          </div>"
        ))
      }
      
      # Format blocks with 2-tier categories
      formatted_blocks2 <- complete_summary %>%
        group_by(`Type of specialty`) %>%
        summarise(
          block = {
            dept <- first(`Type of specialty`)
            color <- if (dept == "Facility-Wide") "#0072B2" else "#6c757d"
            bg <- if (dept == "Facility-Wide") "#f0f0f0" else "#ffffff"
            
            glue::glue(
              "<div style='background-color: {bg}; border-left: 5px solid {color}; padding: 14px; margin-bottom: 20px;'>
              <strong>🏥 {dept}</strong> <span style='color: #888;'>(n = {first(Total)} patients)</span><br><br>
              <ul style='margin-left: 1.2em; line-height: 1.7; padding-left: 0; list-style-type: none;'>
                <li>✅ <strong>Received recommended oral antibiotic with recommended dosage</strong> (as per WHO AWaRe Book): <strong>{scales::percent(Proportion[Dose_Result == 'Received recommended oral antibiotic with recommended dosage'], accuracy = 0.1)}</strong> ({Patients[Dose_Result == 'Received recommended oral antibiotic with recommended dosage']} out of {first(Total)})</li>
                <li>❌ <strong>Received recommended oral antibiotic without recommended dosage</strong> (as per WHO AWaRe Book): <strong>{scales::percent(Proportion[Dose_Result == 'Received recommended oral antibiotic without recommended dosage'], accuracy = 0.1)}</strong> ({Patients[Dose_Result == 'Received recommended oral antibiotic without recommended dosage']} out of {first(Total)})</li>
              </ul>
              </div>"
            )
          },
          .groups = "drop"
        )
      
      # Reorder to show Total first
      formatted_blocks2 <- formatted_blocks2 %>%
        mutate(order = ifelse(`Type of specialty` == "Facility-Wide", 0, 1)) %>%
        arrange(order, `Type of specialty`) %>%
        select(-order)
      
      # Final output
      final_summary_html2 <- HTML(paste0(intro_text2, paste(formatted_blocks2$block, collapse = "\n")))
      htmltools::browsable(final_summary_html2)
    })
    
    # Duration data reactive - use ATC5
    duration_data_reactive <- reactive({
      if (!check_data()) {
        return(list())
      }
      
      data <- data_reactive()
      data_patients <- data$data_patients
      data_lookup <- data$data_lookup
      
      # Get lookup - use ABX-ATC
      lookup2 <- data_lookup %>% filter(Code == "PC_UTI_APPROP_DOSAGE_ORAL_ABX")
      
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
      
      # Filter data
      data_UTI4 <- data_patients %>%
        filter(`Diagnosis code` == "UTI") %>%
        mutate(
          Route = toupper(Route),
          ATC5 = trimws(toupper(ATC5)),
          AWaRe_compatible = ifelse(
            `Patient age group` == "ADULT" & AWaRe %in% AWaRe_abx,
            TRUE, FALSE
          )
        )
      
      # Merge duration info
      data_UTI4 <- data_UTI4 %>%
        left_join(lookup_duration, by = c("ATC5" = "Drug"))
      
      # Flag duration match
      data_UTI4 <- data_UTI4 %>%
        filter(AWaRe_compatible) %>%
        mutate(
          Prescribed_Duration = as.numeric(`Prescribed/intended duration (days)`),
          Duration_Match = case_when(
            is.na(Prescribed_Duration) | is.na(Duration_Min) ~ NA_character_,
            Prescribed_Duration >= Duration_Min & Prescribed_Duration <= Duration_Max ~ "Yes",
            TRUE ~ "No"
          )
        )
      
      # Summarise duration match per patient
      duration_summary <- data_UTI4 %>%
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
      
      # Create summary by specialty
      dept_duration_summary <- duration_summary %>%
        count(`Type of specialty`, Duration_Match_Status, name = "Patients") %>%
        group_by(`Type of specialty`) %>%
        mutate(
          Total = sum(Patients),
          Proportion = round(Patients / Total, 3)
        ) %>%
        ungroup()
      
      # Add Facility-Wide
      hospital_duration_row <- dept_duration_summary %>%
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
      duration_viz_data <- bind_rows(dept_duration_summary, hospital_duration_row)
      
      # Create styled labels
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
      
      duration_plot_labels <- unique(duration_viz_data$PlotLabel)
      
      all_duration_combos <- expand_grid(
        PlotLabel = duration_plot_labels,
        Duration_Match_Status = duration_expected_categories
      )
      
      duration_viz_data <- all_duration_combos %>%
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
      
      list(
        data_UTI4 = data_UTI4,
        duration_viz_data = duration_viz_data
      )
    })
    
    # Duration plot - NO GRAY HIGHLIGHT, WITH HOVER TEXT
    output$duration_plot <- renderPlotly({
      duration_data <- duration_data_reactive()
      if(length(duration_data) == 0) {
        return(plotly_empty())
      }
      
      duration_viz_data <- duration_data$duration_viz_data
      
      # Colors
      duration_colors <- c(
        "Given recommended oral antibiotic with recommended duration" = "#2c7bb6",
        "Given recommended oral antibiotic but with non-recommended duration" = "#f46d43",
        "Given recommended oral antibiotic with unknown duration"= "#F9D99E",
        "Given oral antibiotic not among recommended options" = "gray80"
      )
      
      # Label data
      label_data <- duration_viz_data %>%
        filter(Proportion > 0) %>%
        distinct(PlotLabel, Total)
      
      # Plot with hover text
      p <- ggplot(duration_viz_data, aes(x = PlotLabel, y = Proportion, fill = Duration_Match_Status, text = hover_text)) +
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
          axis.text.y = ggtext::element_markdown(size = 7),
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
      
      # Convert to plotly with hover text
      plt <- ggplotly(p, tooltip = "text") %>%
        layout(
          title = list(
            text = "<b>Duration Appropriateness for Lower UTI</b>",
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
          yaxis = list(automargin = TRUE, categoryorder = "array", categoryarray = rev(levels(duration_viz_data$PlotLabel))),
          xaxis = list(automargin = TRUE)
        )
      
      plt
    })
    
    # Duration summary
    output$duration_summary <- renderUI({
      duration_data <- duration_data_reactive()
      if(length(duration_data) == 0) {
        return(HTML("<p>No data available</p>"))
      }
      
      duration_viz_data <- duration_data$duration_viz_data
      data_UTI4 <- duration_data$data_UTI4
      
      # Define relevant categories
      duration_categories <- c(
        "Given recommended oral antibiotic with recommended duration",
        "Given recommended oral antibiotic but with non-recommended duration",
        "Given recommended oral antibiotic with unknown duration"
      )
      
      # Filter
      duration_filtered <- duration_viz_data %>%
        filter(Duration_Match_Status %in% duration_categories)
      
      # Check for absence
      if (nrow(duration_filtered) == 0 || sum(duration_filtered$Patients, na.rm = TRUE) == 0) {
        return(HTML(
          "<div style='background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 14px; margin-top: 10px;'>
          ⚠️ <strong>No summary available</strong> — there are currently no eligible patients for assessing oral antibiotic duration compliance.<br><br>
          <em>This may indicate that no patients received recommended oral antibiotics with available duration data during the reporting period.</em>
          </div>"
        ))
      }
      
      # Ensure all combinations
      dept_labels <- unique(duration_viz_data$PlotLabel)
      
      all_duration_combos <- expand_grid(
        PlotLabel = dept_labels,
        Duration_Match_Status = duration_categories
      )
      
      duration_summary_all <- all_duration_combos %>%
        left_join(duration_viz_data, by = c("PlotLabel", "Duration_Match_Status")) %>%
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
      intro_text <- glue::glue(
        "<div style='background-color: #f8f9fa; border-left: 5px solid #17a2b8; padding: 14px; margin-top: 10px; margin-bottom: 10px;'>
        💊 <strong>Denominator:</strong> Number of eligible patients who received recommended (<em>or partially recommended</em>) oral antibiotic choice (based on WHO AWaRe Book): 
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
            
            glue::glue(
              "<div style='background-color: {bg}; border-left: 5px solid {color}; padding: 14px; margin-bottom: 20px;'>
              <strong>🏥 {dept}</strong> <span style='color: #888;'>(n = {total} patients)</span><br><br>
              <ul style='margin-left: 1.2em; line-height: 1.7; padding-left: 0; list-style-type: none;'>
                <li>✅ <strong>{duration_categories[1]}</strong>: 
                  <strong>{scales::percent(Proportion[Duration_Match_Status == duration_categories[1]], accuracy = 0.1)}</strong> 
                  ({Patients[Duration_Match_Status == duration_categories[1]]} of {total})</li>
                <li>❌ <strong>{duration_categories[2]}</strong>: 
                  <strong>{scales::percent(Proportion[Duration_Match_Status == duration_categories[2]], accuracy = 0.1)}</strong> 
                  ({Patients[Duration_Match_Status == duration_categories[2]]} of {total})</li>
                <li>❓ <strong>{duration_categories[3]}</strong>: 
                  <strong>{scales::percent(Proportion[Duration_Match_Status == duration_categories[3]], accuracy = 0.1)}</strong> 
                  ({Patients[Duration_Match_Status == duration_categories[3]]} of {total})</li>
              </ul>
              </div>"
            )
          },
          .groups = "drop"
        ) %>%
        mutate(order = ifelse(PlotLabel == "<b style='color:#0072B2;'>Facility-Wide</b>", 0, 1)) %>%
        arrange(order, PlotLabel) %>%
        select(-order)
      
      # Final output
      final_summary_html_duration <- HTML(paste0(intro_text, paste(formatted_blocks$block, collapse = "\n")))
      htmltools::browsable(final_summary_html_duration)
    })
    
  })
}