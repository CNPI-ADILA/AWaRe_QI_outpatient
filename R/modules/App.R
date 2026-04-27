# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidyverse)
library(readxl)
library(glue)
library(purrr)
library(tidyr)
library(scales)
library(htmltools)
library(ggtext)
library(forcats)
library(kableExtra)
library(shinythemes)
library(shinyjs)
library(readr)
library(writexl)
library(openxlsx)
library(shinycssloaders)  

# Source all modules
source("outpatient_general_summary_module.R")
source("rti_module.R")
source("ent_module.R")
source("dent_module.R")
source("diarrhoea_module.R")           # Bloody Diarrhoea Module
source("Non_bloody_diarrhoea.R")       # Non-bloody Diarrhoea Module
source("lymph_module.R")
source("ssti_outpatient_module.R")
source("uti_outpatient_module.R")

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = div(
      style = "display: flex; align-items: center; padding: 5px 15px; height: 100px;",
      tags$a(href = "https://antibioticpolicy.org/", target = "_blank", style = "display: flex; align-items: center;",
             img(src = "https://github.com/CNPI-ADILA/AWaRe_QI_outpatient/raw/main/www/img/APG_logo_primary.jpg", 
                 height = "60px", 
                 style = "margin-right: 15px;",
                 class = "header-logo")
      ),
      span("AWaRe Quality Indicators Tool for Outpatient Antibiotic Use", 
           style = "font-size: 18px; font-weight: bold; color: white; white-space: nowrap; overflow: visible; line-height: 1.2;",
           class = "header-title")
    ),
    titleWidth = 700
  ),
  
  dashboardSidebar(
    width = 280,
    useShinyjs(),
    tags$div(
      id = "sidebar-close-btn",
      class = "sidebar-close-btn",
      onclick = "toggleSidebar()",
      icon("times")
    ),
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Upload", tabName = "upload", icon = icon("upload"), selected = TRUE),
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("General Summary", tabName = "general_summary", icon = icon("chart-line"),
               menuSubItem("Registrations & Treatments", tabName = "registrations_treatments"),
               menuSubItem("Antimicrobial Prescription Overview", tabName = "antimicrobial_overview"),
               menuSubItem("Prescriptions by Diagnosis", tabName = "prescriptions_diagnosis"),
               menuSubItem("Prescriptions by Diagnosis & Specialty", tabName = "prescriptions_specialty")
      ),
      
      menuItem("Clinical Conditions", tabName = "conditions_eligibility", icon = icon("check-circle")),
      
      # Dynamically shown/hidden menu items for each condition
      conditionalPanel(
        condition = "output.showRtiMenu",
        menuItem("RTI Analysis", tabName = "rti", icon = icon("lungs-virus"),
                 menuSubItem("Overview", tabName = "rti_overview"),
                 menuSubItem("Oral Antibiotic Use", tabName = "rti_oral"),
                 menuSubItem("AWaRe Category Analysis", tabName = "rti_aware"),
                 menuSubItem("Choice Alignment", tabName = "rti_choice"),
                 menuSubItem("Dosage Alignment", tabName = "rti_dosage"),
                 menuSubItem("Duration Alignment", tabName = "rti_duration")
        )
      ),
      conditionalPanel(
        condition = "output.showEntMenu",
        menuItem("ENT Infections", tabName = "ent", icon = icon("head-side-virus"),
                 menuSubItem("Overview", tabName = "ent_overview",icon = icon("chart-bar")),
                 menuSubItem("Oral Antibiotic Use", tabName = "ent_oral", icon = icon("pills"))
        )
      ),
      conditionalPanel(
        condition = "output.showDentMenu",
        menuItem("Dental Infections", tabName = "dent", icon = icon("tooth"),
                 menuSubItem("Overview", tabName = "dent_overview", icon = icon("chart-bar")),
                 menuSubItem("Oral Antibiotic Use", tabName = "dent_oral", icon = icon("pills")),
                 menuSubItem("AWaRe Category", tabName = "dent_aware", icon = icon("layer-group")),
                 menuSubItem("Choice Alignment", tabName = "dent_choice", icon = icon("check-circle")),
                 menuSubItem("Dosage Alignment", tabName = "dent_dosage", icon = icon("syringe")),
                 menuSubItem("Duration Alignment", tabName = "dent_duration", icon = icon("clock"))
        )
      ),
      conditionalPanel(
        condition = "output.showBloodyDiarrhoeaMenu",
        menuItem("Bloody Diarrhoea", tabName = "bloody_diarrhoea_main", icon = icon("poop"),
                 menuSubItem("Overview", tabName = "bloody_diarrhoea_overview", icon = icon("chart-bar")),
                 menuSubItem("Oral Antibiotic Use", tabName = "bloody_diarrhoea_oral", icon = icon("pills")),
                 menuSubItem("AWaRe Category", tabName = "bloody_diarrhoea_aware", icon = icon("layer-group"))
        )
      ),
      conditionalPanel(
        condition = "output.showNonBloodyDiarrhoeaMenu",
        menuItem("Non-bloody Diarrhoea", tabName = "non_bloody_diarrhoea_main", icon = icon("toilet"),
                 menuSubItem("Overview", tabName = "non_bloody_diarrhoea_overview", icon = icon("chart-bar")),
                 menuSubItem("Oral Antibiotic Use", tabName = "non_bloody_diarrhoea_oral", icon = icon("pills"))
        )
      ),
      conditionalPanel(
        condition = "output.showLymphMenu",
        menuItem("Lymph Node Infections", tabName = "lymph", icon = icon("circle-nodes"),
                 menuSubItem("Overview", tabName = "lymph_overview", icon = icon("chart-bar")),
                 menuSubItem("Oral Antibiotic Use", tabName = "lymph_oral", icon = icon("pills")),
                 menuSubItem("AWaRe Category", tabName = "lymph_aware", icon = icon("layer-group")),
                 menuSubItem("Choice Alignment", tabName = "lymph_choice", icon = icon("check-circle")),
                 menuSubItem("Dosage Alignment", tabName = "lymph_dosage", icon = icon("syringe")),
                 menuSubItem("Duration Alignment", tabName = "lymph_duration", icon = icon("clock"))
        )
      ),
      conditionalPanel(
        condition = "output.showSstiMenu",
        menuItem("SSTI", tabName = "ssti_outpatient", icon = icon("bandage"),
                 menuSubItem("Overview", tabName = "ssti_op_overview", icon = icon("chart-bar")),
                 menuSubItem("Oral Antibiotic Use", tabName = "ssti_op_oral", icon = icon("pills")),
                 menuSubItem("AWaRe Category", tabName = "ssti_op_aware", icon = icon("layer-group")),
                 menuSubItem("Choice Alignment", tabName = "ssti_op_choice", icon = icon("check-circle")),
                 menuSubItem("Dosage Alignment", tabName = "ssti_op_dosage", icon = icon("syringe")),
                 menuSubItem("Duration Alignment", tabName = "ssti_op_duration", icon = icon("clock"))
        )
      ),
      conditionalPanel(
        condition = "output.showUtiMenu",
        menuItem("Lower UTI", tabName = "uti_outpatient", icon = icon("droplet"),
                 menuSubItem("Overview", tabName = "uti_op_overview",icon = icon("chart-bar")),
                 menuSubItem("Oral Antibiotic Use", tabName = "uti_op_oral", icon = icon("pills")),
                 menuSubItem("AWaRe Category Analysis", tabName = "uti_op_aware", icon = icon("layer-group")),
                 menuSubItem("Choice Alignment", tabName = "uti_op_choice", icon = icon("check-circle")),
                 menuSubItem("Dosage Alignment", tabName = "uti_op_dosage", icon = icon("syringe")),
                 menuSubItem("Duration Alignment", tabName = "uti_op_duration", icon = icon("clock"))
        )
      ),
      
      menuItem("Download Raw Data", tabName = "download_rawdata")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$title("AWaRe QI Dashboard - Outpatient"),
      
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          document.title = 'AWaRe QI Dashboard - Outpatient';
          var observer = new MutationObserver(function(mutations) {
            mutations.forEach(function(mutation) {
              if (mutation.type === 'childList' && document.title !== 'AWaRe QI Dashboard - Outpatient') {
                document.title = 'AWaRe QI Dashboard - Outpatient';
              }
            });
          });
          observer.observe(document.querySelector('title'), { childList: true });
        });
      ")),
      
      tags$link(rel = "icon", 
                type = "image/png",
                href = "https://raw.githubusercontent.com/CNPI-ADILA/Global_PPS_Rshinny/logos/APG%20logo_primary.jpg"),
      tags$link(rel = "shortcut icon", 
                type = "image/png",
                href = "https://raw.githubusercontent.com/CNPI-ADILA/Global_PPS_Rshinny/logos/APG%20logo_primary.jpg"),
      tags$link(rel = "apple-touch-icon", 
                type = "image/png",
                href = "https://raw.githubusercontent.com/CNPI-ADILA/Global_PPS_Rshinny/logos/APG%20logo_primary.jpg"),
      
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"),
      tags$meta(charset = "UTF-8"),
      
      tags$style(HTML("
        /* Base Styles */
        .main-header {
          position: fixed !important;
          width: 100% !important;
          z-index: 1000 !important;
        }
        
        .content-wrapper, .right-side {
          padding-top: 100px !important;
        }
        
        .main-sidebar {
          position: fixed !important;
          top: 100px !important;
          height: calc(100vh - 100px) !important;
          overflow-y: auto !important;
        }
        
        body, .content-wrapper, .right-side, .tab-content {
          background-color: #f5f5f5 !important;
        }
        
        .main-sidebar {
          width: 280px !important;
          background-color: #2c3e50 !important;
        }
        
        .content-wrapper, .right-side, .main-footer {
          margin-left: 280px !important;
          transition: margin-left 0.3s ease;
        }
        
        .main-header {
          background-color: #2c3e50 !important;
          height: 100px;
          min-height: 100px;
        }
        
        .main-header .navbar {
          background-color: #2c3e50 !important;
          height: 100px;
          min-height: 100px;
          display: flex;
          align-items: center;
        }
        
        .box {
          width: 100% !important;
        }
        
        .main-header .logo {
          background-color: #2c3e50 !important;
          width: 750px !important;
          height: 100px;
          line-height: 100px;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 0;
          max-width: none !important;
        }
        
        .main-header .logo > span {
          display: flex;
          align-items: center;
          height: 100%;
        }
        
        /* Hamburger Menu Button - Hidden on Desktop */
        .navbar-toggle, .sidebar-toggle {
          display: none !important;
          background-color: transparent !important;
          border: none !important;
          color: white !important;
          font-size: 24px;
          padding: 10px 15px;
          cursor: pointer;
        }
        
        /* Sidebar Close Button - Hidden on Desktop */
        .sidebar-close-btn {
          display: none;
          position: absolute;
          top: 10px;
          right: 10px;
          font-size: 24px;
          color: white;
          cursor: pointer;
          z-index: 1002;
          padding: 5px 10px;
          background: rgba(255,255,255,0.1);
          border-radius: 4px;
        }
        
        .main-header .logo .title {
          white-space: nowrap !important;
          overflow: visible !important;
        }
        
        .sidebar-menu > li {
          display: block !important;
        }
        
        .sidebar-menu > li > a {
          color: white !important;
          transition: background-color 0.3s ease, color 0.3s ease;
          padding: 12px 15px !important;
          display: block !important;
        }
        
        .sidebar-menu > li > a:hover {
          background-color: #3498db !important;
          color: white !important;
        }
        
        .sidebar-menu > li.active > a {
          background-color: #34495e !important;
          color: white !important;
        }
        
        .sidebar-menu .treeview > a > .fa-angle-left {
          position: absolute !important;
          right: 15px !important;
          margin-top: -8px !important;
        }
        
        .sidebar-menu .treeview > a {
          padding-right: 40px !important;
        }
        
        .sidebar-menu .treeview-menu {
          background-color: #34495e !important;
          padding-left: 0px !important;
        }
        
        .sidebar-menu .treeview-menu > li > a {
          color: #b8c7ce !important;
          background-color: #34495e !important;
          padding: 8px 15px 8px 35px !important;
        }
        
        .sidebar-menu .treeview-menu > li > a:hover {
          background-color: #3498db !important;
          color: white !important;
        }
        
        .sidebar-menu .treeview-menu > li.active > a {
          background-color: #3498db !important;
          color: white !important;
        }
        
        .box {
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          border: 1px solid #e3e6f0;
          margin-bottom: 25px;
          transition: transform 0.2s ease, box-shadow 0.2s ease;
          background-color: #ffffff !important;
        }
        
        .box:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(0,0,0,0.15);
        }
        
        .box.box-primary > .box-header,
        .box.box-info > .box-header,
        .box.box-success > .box-header,
        .box.box-warning > .box-header,
        .box.box-danger > .box-header {
          background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%) !important;
          border-bottom: 3px solid #3498db !important;
          border-radius: 8px 8px 0 0;
          padding: 8px 20px;
        }
        
        .box-header h3, .box-header h4, .box-header .box-title {
          color: white !important;
          font-weight: 600 !important;
          font-size: 14px !important;
          margin: 0 !important;
          letter-spacing: 0.5px;
        }
        
        .box-body {
          padding: 25px;
          background-color: #ffffff;
          border-radius: 0 0 8px 8px;
        }
        
        .box-content {
          background-color: #ffffff !important;
        }

        .nav-tabs-custom {
          background-color: white !important;
        }
        
        .info-box {
          margin-bottom: 25px;
          padding: 20px;
          background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
          border-left: 6px solid #3498db;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          transition: transform 0.2s ease;
        }
        
        .fixed-button {
          position: fixed;
          top: 100px;
          right: 20px;
          z-index: 9999;
        }
        
        .sidebar-menu .treeview-menu > li > a {
          font-size: 10px !important;
        }

        .sidebar-menu .treeview-menu > li > a {
          padding: 6px 15px 6px 35px !important;
        }
        
        .info-box:hover { transform: translateX(2px); }
        .condition-box { margin-bottom: 20px; padding: 18px; background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%); border-left: 6px solid #2c3e50; border-radius: 6px; box-shadow: 0 2px 6px rgba(0,0,0,0.06); transition: transform 0.2s ease; }
        .condition-box:hover { transform: translateX(2px); box-shadow: 0 4px 12px rgba(0,0,0,0.12); }
        .note-box { background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%); padding: 15px 20px; border-left: 5px solid #3498db; margin: 15px 0; border-radius: 6px; box-shadow: 0 2px 6px rgba(0,0,0,0.06); }
        .error-box { background: linear-gradient(135deg, #fdf2f2 0%, #ffffff 100%); padding: 15px 20px; border-left: 5px solid #e74c3c; margin: 15px 0; border-radius: 6px; box-shadow: 0 2px 6px rgba(231,76,60,0.1); }
        .warning-box { background: linear-gradient(135deg, #fffdf2 0%, #ffffff 100%); border-left: 5px solid #f39c12; padding: 15px 20px; margin: 15px 0 22px; border-radius: 6px; box-shadow: 0 2px 6px rgba(243,156,18,0.1); }
        .success-box { background: linear-gradient(135deg, #f2f8f2 0%, #ffffff 100%); border-left: 5px solid #27ae60; padding: 15px 20px; margin: 15px 0; border-radius: 6px; box-shadow: 0 2px 6px rgba(39,174,96,0.1); }
        .ward-summary-box { min-height: 450px; background: #ffffff; border-radius: 8px; overflow: hidden; }
        .ward-summary-box .box-body { padding: 20px; }
        .ward-summary-box hr { border-color: #e3e6f0; margin: 15px 0; }

        .collapsible-header { cursor: pointer; background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); color: white; padding: 15px 20px; border-radius: 8px; margin-bottom: 10px; transition: all 0.3s ease; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
        .collapsible-header:hover { background: linear-gradient(135deg, #2980b9 0%, #21618c 100%); transform: translateY(-1px); box-shadow: 0 4px 12px rgba(0,0,0,0.15); }
        .collapsible-header h4 { margin: 0; font-weight: 600; display: flex; align-items: center; justify-content: space-between; }
        .collapsible-content { display: none; padding: 20px; background: white; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 8px rgba(0,0,0,0.05); border: 1px solid #e3e6f0; }
        .collapsible-content.show { display: block; animation: slideDown 0.3s ease; }
        @keyframes slideDown { from { opacity: 0; transform: translateY(-10px); } to { opacity: 1; transform: translateY(0); } }
        .toggle-icon { transition: transform 0.3s ease; }
        .toggle-icon.rotated { transform: rotate(180deg); }

        .custom-value-box { 
          background: linear-gradient(135deg, #4CAF50 0%, #45a049 100%); 
          border-radius: 12px; 
          padding: 20px; 
          margin: 10px 0; 
          box-shadow: 0 4px 15px rgba(76, 175, 80, 0.3); 
          position: relative; 
          overflow: hidden; 
          transition: all 0.3s ease; 
          border: none; 
          height: 180px;
          display: flex; 
          flex-direction: column; 
          justify-content: space-between; 
          color: white; 
          cursor: pointer; 
        }
        .custom-value-box:hover { 
          transform: translateY(-2px); 
          box-shadow: 0 6px 20px rgba(76, 175, 80, 0.4); 
        }
        .custom-value-box.red { 
          background: linear-gradient(135deg, #f44336 0%, #d32f2f 100%); 
          box-shadow: 0 4px 15px rgba(244, 67, 54, 0.3); 
          cursor: default; 
          height: 180px;
        }
        .custom-value-box.red:hover { 
          box-shadow: 0 6px 20px rgba(244, 67, 54, 0.4); 
          transform: none; 
        }
        .custom-value-box::before { content: ''; position: absolute; top: 0; right: 0; width: 0; height: 0; border-style: solid; border-width: 0 60px 60px 0; border-color: transparent rgba(255, 255, 255, 0.1) transparent transparent; }
        .custom-value-number { font-size: 48px; font-weight: 700; line-height: 1; margin-bottom: 5px; text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.3); }
        .custom-value-title { font-size: 16px; font-weight: 600; margin-bottom: 8px; line-height: 1.2; }
        .custom-value-status { font-size: 14px; font-weight: 500; display: flex; align-items: center; gap: 5px; }
        .custom-value-status i { font-size: 16px; }
        .custom-value-icon { position: absolute; bottom: 15px; right: 15px; font-size: 40px; opacity: 0.3; }

        .table { border-radius: 8px; overflow: hidden; box-shadow: 0 2px 8px rgba(0,0,0,0.06); background-color: white; }
        .table thead th { background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%); color: white; font-weight: 600; letter-spacing: 0.5px; border: none; padding: 15px; }
        .table tbody tr { transition: background-color 0.2s ease; }
        .table tbody tr:hover { background-color: #f8f9fa; }
        .btn { border-radius: 6px; font-weight: 600; letter-spacing: 0.3px; transition: all 0.3s ease; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .btn:hover { transform: translateY(-1px); box-shadow: 0 4px 8px rgba(0,0,0,0.15); }
        .form-control { border-radius: 6px; border: 2px solid #e3e6f0; transition: border-color 0.3s ease, box-shadow 0.3s ease; background-color: white; }
        .form-control:focus { border-color: #3498db; box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25); }
        @keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
        .box, .info-box, .condition-box { animation: fadeIn 0.5s ease-out; }
        body { padding-bottom: 180px; }
        .tab-content { padding-bottom: 50px; min-height: calc(100vh - 250px); }
        
        /* Tables Responsive */
        .dataTables_wrapper {
          overflow-x: auto;
          -webkit-overflow-scrolling: touch;
        }
        
        /* MOBILE STYLES */
        @media (max-width: 768px) {
          /* Header adjustments */
          .main-header {
            height: 70px !important;
            min-height: 70px !important;
          }
          
          .main-header .navbar {
            height: 70px !important;
            min-height: 70px !important;
          }
          
          .main-header .logo {
            width: calc(100% - 60px) !important;
            height: 70px !important;
            line-height: 70px !important;
            padding: 0 10px !important;
          }
          
          .header-logo {
            height: 40px !important;
            margin-right: 10px !important;
          }
          
          .header-title {
            font-size: 14px !important;
            white-space: normal !important;
            line-height: 1.3 !important;
          }
          
          /* Show hamburger menu */
          .navbar-toggle, .sidebar-toggle {
            display: block !important;
            position: absolute;
            right: 15px;
            top: 50%;
            transform: translateY(-50%);
            z-index: 1001;
          }
          
          /* Sidebar mobile behavior */
          .main-sidebar {
            left: -280px !important;
            top: 70px !important;
            height: calc(100vh - 70px) !important;
            transition: left 0.3s ease;
            z-index: 1001;
            box-shadow: 2px 0 8px rgba(0,0,0,0.2);
          }
          
          .main-sidebar.sidebar-open {
            left: 0 !important;
          }
          
          .sidebar-close-btn {
            display: block !important;
          }
          
          /* Content adjustments */
          .content-wrapper, .right-side, .main-footer {
            margin-left: 0 !important;
            padding-top: 70px !important;
          }
          
          /* Overlay when sidebar is open */
          body.sidebar-open::before {
            content: '';
            position: fixed;
            top: 70px;
            left: 0;
            right: 0;
            bottom: 0;
            background: rgba(0,0,0,0.5);
            z-index: 999;
          }
          
          /* Value boxes */
          .custom-value-box {
            height: 150px !important;
            padding: 15px !important;
            margin: 10px 0 !important;
          }
          
          .custom-value-box.red {
            height: 150px !important;
          }
          
          .custom-value-number {
            font-size: 36px !important;
          }
          
          .custom-value-title {
            font-size: 14px !important;
          }
          
          .custom-value-icon {
            font-size: 30px !important;
          }
          
          /* Boxes and content */
          .box {
            margin-bottom: 15px !important;
          }
          
          .box-body {
            padding: 15px !important;
          }
          
          /* Info boxes */
          .info-box, .condition-box {
            padding: 15px !important;
            margin-bottom: 15px !important;
          }
          
          /* Tables */
          .table {
            font-size: 12px !important;
          }
          
          .table thead th {
            padding: 10px 5px !important;
            font-size: 11px !important;
          }
          
          .table tbody td {
            padding: 8px 5px !important;
          }
          
          /* Collapsible sections */
          .collapsible-header {
            padding: 12px 15px !important;
            font-size: 14px !important;
          }
          
          .collapsible-content {
            padding: 15px !important;
          }
          
          /* Column adjustments */
          [class*='col-'] {
            padding-left: 10px !important;
            padding-right: 10px !important;
          }
          
          /* Buttons */
          .btn {
            padding: 8px 12px !important;
            font-size: 13px !important;
            width: 100%;
            margin: 5px 0;
          }
          
          /* Download section */
          .download-button {
            width: 100% !important;
            margin: 10px 0 !important;
          }
          
          /* Fixed button */
          .fixed-button {
            top: 80px;
            right: 10px;
          }
        }
        
        @media (max-width: 576px) {
          /* Extra small devices */
          .header-title {
            font-size: 12px !important;
          }
          
          .header-logo {
            height: 35px !important;
          }
          
          .custom-value-box {
            height: 140px !important;
            padding: 12px !important;
          }
          
          .custom-value-box.red {
            height: 140px !important;
          }
          
          .custom-value-number {
            font-size: 32px !important;
          }
          
          .custom-value-title {
            font-size: 13px !important;
          }
          
          .table {
            font-size: 11px !important;
          }
          
          .sidebar-menu .treeview-menu > li > a {
            font-size: 11px !important;
            padding: 6px 10px 6px 25px !important;
          }
          
          .box-body {
            padding: 10px !important;
          }
          
          .info-box, .condition-box {
            padding: 12px !important;
          }
        }
        
        /* Landscape phone mode */
        @media (max-width: 768px) and (orientation: landscape) {
          .main-header {
            height: 60px !important;
          }
          
          .main-header .navbar {
            height: 60px !important;
          }
          
          .main-header .logo {
            height: 60px !important;
          }
          
          .main-sidebar {
            top: 60px !important;
            height: calc(100vh - 60px) !important;
          }
          
          .content-wrapper, .right-side {
            padding-top: 60px !important;
          }
          
          body.sidebar-open::before {
            top: 60px;
          }
        }
        
        /* Ensure touch targets are large enough */
        @media (max-width: 768px) {
          a, button, .clickable {
            min-height: 44px;
            display: inline-flex;
            align-items: center;
            justify-content: center;
          }
          
          .sidebar-menu > li > a {
            min-height: 44px;
            padding: 12px 15px !important;
          }
        }
      "))
    ),
    
    tags$script(HTML("
      $(document).ready(function() {
        // Collapsible sections
        $('.collapsible-header').click(function() {
          var content = $(this).next('.collapsible-content');
          var icon = $(this).find('.toggle-icon');
          content.slideToggle(300);
          icon.toggleClass('rotated');
          if (content.hasClass('show')) { 
            content.removeClass('show'); 
          } else { 
            content.addClass('show'); 
          }
        });
        
        // Scroll to top when sidebar menu item clicked
        $('.sidebar-menu a').click(function() {
          window.scrollTo({top: 0, behavior: 'smooth'});
        });
        
        // MOBILE MENU TOGGLE
        $('.navbar-toggle, .sidebar-toggle').click(function(e) {
          e.preventDefault();
          e.stopPropagation();
          toggleSidebar();
        });
        
        // Close sidebar when clicking overlay
        $(document).on('click', function(e) {
          if ($(window).width() <= 768) {
            if ($('body').hasClass('sidebar-open') && 
                !$(e.target).closest('.main-sidebar').length && 
                !$(e.target).closest('.navbar-toggle, .sidebar-toggle').length) {
              toggleSidebar();
            }
          }
        });
        
        // Close sidebar when menu item clicked on mobile
        $('.sidebar-menu a').click(function() {
          if ($(window).width() <= 768) {
            setTimeout(function() {
              if ($('body').hasClass('sidebar-open')) {
                toggleSidebar();
              }
            }, 300);
          }
        });
        
        // Prevent body scroll when sidebar open on mobile
        function updateBodyScroll() {
          if ($(window).width() <= 768 && $('body').hasClass('sidebar-open')) {
            $('body').css('overflow', 'hidden');
          } else {
            $('body').css('overflow', '');
          }
        }
        
        // Handle window resize
        $(window).resize(function() {
          if ($(window).width() > 768) {
            $('.main-sidebar').removeClass('sidebar-open');
            $('body').removeClass('sidebar-open').css('overflow', '');
          }
        });
      });

      // Toggle sidebar function
      function toggleSidebar() {
        $('.main-sidebar').toggleClass('sidebar-open');
        $('body').toggleClass('sidebar-open');
        
        // Update body scroll
        if ($(window).width() <= 768 && $('body').hasClass('sidebar-open')) {
          $('body').css('overflow', 'hidden');
        } else {
          $('body').css('overflow', '');
        }
      }

      function openMenuForTab(tabName) {
        var $link = $('.sidebar-menu a[href=\"#shiny-tab-' + tabName + '\"]');
        if ($link.length === 0) return;

        var $tree = $link.closest('li.treeview');
        if ($tree.length === 0) { 
          $tree = $link.closest('ul.treeview-menu').closest('li.treeview'); 
        }
        if ($tree.length) {
          $('.sidebar-menu li.treeview').removeClass('menu-open active');
          $('.sidebar-menu li.treeview > ul.treeview-menu').css('display', 'none');

          $tree.addClass('menu-open active');
          $tree.children('ul.treeview-menu').css('display', 'block');

          $tree.find('ul.treeview-menu > li').removeClass('active');
          var $targetLi = $link.closest('li');
          $targetLi.addClass('active');
        }

        var $topLi = $tree;
        if ($topLi.length) {
          $('.sidebar-menu > li').removeClass('active');
          $topLi.addClass('active');
        }
        
        window.scrollTo({top: 0, behavior: 'smooth'});
      }

      Shiny.addCustomMessageHandler('openMenu', function(tabName) {
        setTimeout(function(){ 
          openMenuForTab(tabName); 
        }, 100);
      });
    ")),
    
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                column(10, offset = 1,
                       div(class = "success-box",
                           h3("Welcome to AWaRe Antibiotic Use Quality Indicators Tool - Outpatient Analysis"),
                           p("This dashboard provides analysis of AWaRe quality indicators (QIs) based on condition-specific Global-PPS prescribing data according to the AWaRe book recommendations in outpatient settings."),
                           p(em("This dashboard is a supplementary tool to the Global PPS dashboard and will provide additional insights to your PPS data particularly around the AWaRe book.")),
                           h4("Getting Started"), 
                           
                           p("1.", strong("Download Firstline app:"), " ",
                             tags$a("https://app.firstline.org/", 
                                    href = "https://app.firstline.org/", 
                                    target = "_blank"),
                             " to familiarise yourself with the conditions and antibiotic recommendations listed in the WHO AWaRe book."),
                           p("2. Upload your data files below"),
                           p("3. Check eligible cases in the 'Clinical Conditions' tab"),
                           p("4. Click on condition value boxes to unlock specific analyses"),
                           p("5. Review quality indicators and recommendations")
                       )
                )
              ),
              
              fluidRow(
                column(10, offset = 1,
                       box(
                         width = 12,
                         title = "📁 Upload Your Data",
                         status = "primary",
                         solidHeader = TRUE,
                         p("Please upload your Global Point Prevalence Survey (gPPS) outpatient data file in Excel format (.xlsx)."),
                         fileInput("datafile", 
                                   label = "Choose Excel File",
                                   accept = c(".xlsx", ".xls"),
                                   buttonLabel = "Browse...",
                                   placeholder = "No file selected"),
                         conditionalPanel(
                           condition = "output.dataUploaded == true",
                           div(class = "success-box",
                               h4(icon("check-circle"), " Data Successfully Loaded"),
                               p("Your data has been uploaded successfully. Navigate to 'Clinical Conditions' to begin analysis.")
                           )
                         )
                       )
                )
              )
      ),
      
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                column(10, offset = 1,
                       div(class = "collapsible-header",
                           h4("Overview")
                       ),
                       
                       div(class = "info-box",
                           h4("AWaRe Quality Indicator Analysis Report:"),
                           p("This report provides a summary analysis of the Global PPS (Global-PPS) outpatient dataset based on the WHO AWaRe system. The analysis specifically focuses on antibiotic prescribing practices in outpatient settings for common community infections covered by the WHO AWaRe Antibiotic Book."),
                           h4("General Notes:"),
                           tags$ul(
                             tags$li("This tool uses standardised data from the Global-PPS, specifically extracted from the Excel-based outputs received from Global-PPS by participating facilities."),
                             tags$li("Quality indicators (QIs) were mapped and interpreted based on clinical assumptions aligning WHO guidance with available Global-PPS symptom codes."),
                             tags$li("Oral administration is the primary route for outpatient antibiotics as per Global-PPS data collection standards.")
                           ),
                           h4("Disclaimer:"),  
                           p(tags$span(
                             style = "color: red;",
                             "This version of the tool has not yet been formally validated. Outputs are intended to support antimicrobial stewardship, but should not be used in isolation to make clinical decisions or facility policy changes. Results must always be interpreted in conjunction with local guidelines, clinical expertise, and institutional approval processes."
                           ))
                       ),
                       
                       div(class = "collapsible-header",
                           h4("Condition Definitions", 
                              tags$i(class = "fa fa-chevron-down toggle-icon", style = "float: right;"))
                       ),
                       div(class = "collapsible-content",
                           div(class = "condition-box",
                               strong("1- Respiratory Tract Infections (RTI)"), " (Based on WHO AWaRe book)", br(),
                               strong("gPPS recorded symptoms: "), "Acute cough OR Sore throat OR Sneezing / nasal congestion / runny or stuffy nose OR Ear pain OR Ear discharge"
                           ),
                           div(class = "condition-box",
                               strong("2- Ear, Nose, Throat (ENT) Infections"), " (Based on WHO AWaRe book)", br(),
                               strong("gPPS recorded symptoms: "), "Ear pain OR Ear discharge OR Sore throat OR OR Sneezing / nasal congestion / runny or stuffy nose"
                           ),
                           div(class = "condition-box",
                               strong("3- Dental Infections"), " (Based on WHO AWaRe book)", br(),
                               strong("gPPS recorded symptoms: "), "Toothache/gum swelling"
                           ),
                           div(class = "condition-box",
                               strong("4- Diarrhoea"), " (Based on WHO AWaRe book)", br(),
                               strong("gPPS recorded symptoms: "), "Diarrhea OR Bloody diarrhea"
                           ),
                           div(class = "condition-box",
                               strong("5- Lymph Node Infections"), " (Based on WHO AWaRe book)", br(),
                               strong("gPPS recorded symptoms: "), "Limb swelling/warmth erythema"
                           ),
                           div(class = "condition-box",
                               strong("6- Skin/Soft Tissue Infections (SSTI)"), " (Based on WHO AWaRe book)", br(),
                               strong("gPPS recorded symptoms: "), "Skin lesions/spots"
                           ),
                           div(class = "condition-box",
                               strong("7- Lower Urinary Tract Infections (UTI)"), " (Based on WHO AWaRe book)", br(),
                               strong("gPPS recorded symptoms: "), " Painful/frequent urination"
                           )
                       ),
                       div(class = "collapsible-header",
                           h4("Frequently Asked Questions", 
                              tags$i(class = "fa fa-chevron-down toggle-icon", style = "float: right;"))
                       ),
                       div(class = "collapsible-content show",
                           
                           # FAQ 1
                           div(class = "collapsible-header", style = "margin-bottom: 5px; background: linear-gradient(135deg, #e8e8e8 0%, #d0d0d0 100%); color: #2c3e50;",
                               h4(style = "font-size: 16px; margin: 0;", "What are AWaRe Quality Indicators (QIs)?", 
                                  tags$i(class = "fa fa-chevron-down toggle-icon", style = "float: right;"))
                           ),
                           div(class = "collapsible-content", style = "margin-bottom: 15px;",
                               p("AWaRe QIs are a set of evidence-based, quantifiable measures developed to assess whether antibiotic prescribing in health care (in both hospital and primary care settings) complies with the guidance provided by the WHO under its AWaRe Antibiotic Book. The QIs were created following a rigorous process (a scoping review plus two rounds of consensus via the Delphi and RAND/UCLA methods) that evaluated both the clinical appropriateness and feasibility of each indicator globally."),
                               p("Each QI reflects the proportion of patients with a defined infection who receive an antibiotic regimen consistent with AWaRe guidance (e.g., recommended agent, route, and duration). Together, these indicators provide a standardised, globally applicable framework for monitoring and benchmarking antibiotic prescribing quality. They help antimicrobial stewardship programmes identify inappropriate use, track changes over time, and guide targeted improvement efforts."),
                               p("The AWaRe QIs used in this dashboard correspond to hospital-care indicators from the WHO-developed QI set."),
                               p("More details are available in the AWaRe QI development paper: ",
                                 tags$a("https://www.medrxiv.org/content/10.1101/2025.10.24.25338539v1", 
                                        href = "https://www.medrxiv.org/content/10.1101/2025.10.24.25338539v1",
                                        target = "_blank",
                                        style = "color: #3498db; text-decoration: underline; font-weight: 600;"))
                           ),
                           
                           # FAQ 2
                           div(class = "collapsible-header", style = "margin-bottom: 5px; background: linear-gradient(135deg, #e8e8e8 0%, #d0d0d0 100%); color: #2c3e50;",
                               h4(style = "font-size: 16px; margin: 0;", "What is the Global Point Prevalence Survey (Global-PPS)?", 
                                  tags$i(class = "fa fa-chevron-down toggle-icon", style = "float: right;"))
                           ),
                           div(class = "collapsible-content", style = "margin-bottom: 15px;",
                               p("The Global-PPS is an international, standardised surveillance programme used by hospitals worldwide to measure antimicrobial prescribing and healthcare-associated infections on a single survey day. It collects a comprehensive set of variables—including patient demographics, clinical diagnoses, infection type, indication for antimicrobial use, antibiotic choice and AWaRe category, route, dose, duration, and adherence to guidelines—all using a uniform methodology."),
                               p("This structured and harmonised approach allows facilities to generate reliable, comparable antimicrobial use data across wards, hospitals, and countries. Because the Global-PPS dataset contains all variables needed to apply the AWaRe Quality Indicator definitions accurately, it provides the ideal input for the automated analyses performed by the AWaRe QI dashboard."),
                               p("For more information about the Global-PPS methodology and participation, please visit: ",
                                 tags$a("https://www.global-pps.com/", 
                                        href = "https://www.global-pps.com/",
                                        target = "_blank",
                                        style = "color: #3498db; text-decoration: underline; font-weight: 600;"))
                           ),
                           
                           # FAQ 3
                           div(class = "collapsible-header", style = "margin-bottom: 5px; background: linear-gradient(135deg, #e8e8e8 0%, #d0d0d0 100%); color: #2c3e50;",
                               h4(style = "font-size: 16px; margin: 0;", "What is the purpose of the AWaRe QI Dashboard, and what supporting documents are available?", 
                                  tags$i(class = "fa fa-chevron-down toggle-icon", style = "float: right;"))
                           ),
                           div(class = "collapsible-content", style = "margin-bottom: 15px;",
                               p("The AWaRe QI Dashboard was developed to automatically apply the definitions of the AWaRe QIs and calculate their proportions by linking facility data to the WHO AWaRe Antibiotic Book recommendations for inpatient care. It uses existing Global-PPS exports to streamline this process."),
                               p("The main goals of the dashboard are to:"),
                               tags$ul(
                                 tags$li("Accelerate and automate the calculation of AWaRe QIs"),
                                 tags$li("Provide a practical, user-friendly tool for visualising antibiotic prescribing patterns"),
                                 tags$li("Deliver high-quality, diverse outputs that support antimicrobial stewardship (AMS)"),
                                 tags$li("Facilitate meaningful interpretation of results through supporting materials, examples, and guidance so that outputs can be appropriately understood in local contexts")
                               ),
                               p("A set of supporting documents accompanies the dashboard to guide users through operation, interpretation, and application of results:"),
                               tags$ul(
                                 tags$li(
                                   tags$a("User Manual:", 
                                          href = "https://github.com/CNPI-ADILA/AWaRe_QI_inpatient/blob/main/docs/01%20User%20Manual%20for%20AWaRe%20QI%20Inpatient%20Dashboard%20(V.1.0).pdf",
                                          target = "_blank",
                                          style = "color: #3498db; text-decoration: underline; font-weight: 600;")
                                 ),
                                 tags$li(
                                   tags$a("Interpretation Guide:", 
                                          href = "https://github.com/CNPI-ADILA/AWaRe_QI_inpatient/blob/main/docs/02%20Graph%20Interpretations%20for%20AWaRe%20QI%20Inpatient%20Dashboard%20(V.1.0).pdf",
                                          target = "_blank",
                                          style = "color: #3498db; text-decoration: underline; font-weight: 600;")
                                 ),
                                 tags$li(
                                   tags$a("Antimicrobial stewardship Guide:", 
                                          href = "https://github.com/CNPI-ADILA/AWaRe_QI_inpatient/blob/main/docs/03%20Antimicrobial%20Stewarship%20Interpretations%20for%20AWaRe%20QI%20Inpatient%20Dashboard%20(V.1.0).pdf",
                                          target = "_blank",
                                          style = "color: #3498db; text-decoration: underline; font-weight: 600;")
                                 )
                               )
                           ),
                           
                           # FAQ 4
                           div(class = "collapsible-header", style = "margin-bottom: 5px; background: linear-gradient(135deg, #e8e8e8 0%, #d0d0d0 100%); color: #2c3e50;",
                               h4(style = "font-size: 16px; margin: 0;", "What if my facility has a small number of eligible cases for calculating Quality Indicators?", 
                                  tags$i(class = "fa fa-chevron-down toggle-icon", style = "float: right;"))
                           ),
                           div(class = "collapsible-content", style = "margin-bottom: 15px;",
                               p("If your facility has only a small number of eligible cases (<10 cases), the QI–specific outputs will not be displayed, as the sample size may be too small to generate meaningful or reliable indicators. However, the general summary outputs will still be available and remain useful. These summaries provide an overview of antibiotic prescribing patterns, infection prevalence, AWaRe category distribution, and potential areas for antimicrobial stewardship focus, even when QIs cannot be calculated.")
                           ),
                           
                           # FAQ 5
                           div(class = "collapsible-header", style = "margin-bottom: 5px; background: linear-gradient(135deg, #e8e8e8 0%, #d0d0d0 100%); color: #2c3e50;",
                               h4(style = "font-size: 16px; margin: 0;", "What if your local or national guidelines differ from the WHO AWaRe Antibiotic Book?", 
                                  tags$i(class = "fa fa-chevron-down toggle-icon", style = "float: right;"))
                           ),
                           div(class = "collapsible-content", style = "margin-bottom: 15px;",
                               p("If your local or national guidelines are not mostly or fully aligned with the recommendations in the WHO AWaRe antibiotic book, then using the QI-specific outputs from the dashboard is not advisable, as they may not accurately reflect the appropriateness of prescribing in your context."),
                               p("However, the general summary outputs remain useful, as they still provide an overview of antibiotic prescribing patterns, infection prevalence, AWaRe category distribution, and potential areas for stewardship focus, even when guideline recommendations differ."),
                               p("If your facility has the technical capacity, you may adapt the underlying dashboard code to incorporate your own local guideline recommendations. All code used in the dashboard is openly accessible here:", 
                                 tags$a("https://github.com/CNPI-ADILA/AWaRe_QI_inpatient/tree/main", 
                                        href = "https://github.com/CNPI-ADILA/AWaRe_QI_inpatient/tree/main",
                                        target = "_blank",
                                        style = "color: #3498db; text-decoration: underline; font-weight: 600;"))
                           ),
                           
                           # FAQ 6
                           div(class = "collapsible-header", style = "margin-bottom: 5px; background: linear-gradient(135deg, #e8e8e8 0%, #d0d0d0 100%); color: #2c3e50;",
                               h4(style = "font-size: 16px; margin: 0;", "What if you do not use Global-PPS to collect data from your facilities?", 
                                  tags$i(class = "fa fa-chevron-down toggle-icon", style = "float: right;"))
                           ),
                           div(class = "collapsible-content", style = "margin-bottom: 15px;",
                               p("If your facilities use other antimicrobial surveillance methodologies (such as the WHO PPS or national data collection tools), it is still possible to use the AWaRe QI dashboard. You may be able to restructure your dataset to align with the Global-PPS data structure. Doing so allows the dashboard to correctly identify the required variables and generate valid outputs."),
                               tags$a("04 PPS Data Input Guide for AWaRe QI Inpatient Dashboard (V.1.0)", 
                                      href = "https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fraw.githubusercontent.com%2FCNPI-ADILA%2FAWaRe_QI_inpatient%2Frefs%2Fheads%2Fmain%2Fdocs%2F04%2520PPS%2520Data%2520Input%2520Guide%2520for%2520AWaRe%2520QI%2520Inpatient%2520Dashboard%2520(V.1.0).xlsx&wdOrigin=BROWSELINK",
                                      target = "_blank",
                                      style = "color: #3498db; text-decoration: underline; font-weight: 600;"))
                       )
                       
                )
              )
      ),
      
      # General Summary Tabs
      tabItem(tabName = "registrations_treatments", outpatientRegistrationsUI("general_summary")),
      tabItem(tabName = "antimicrobial_overview", outpatientAntimicrobialPrescriptionUI("general_summary")),
      tabItem(tabName = "prescriptions_diagnosis", outpatientAntibioticsDiagnosisUI("general_summary")),
      tabItem(tabName = "prescriptions_specialty", outpatientAntibioticsDiagnosisSpecialtyUI("general_summary")),
      
      # Clinical Conditions Eligibility Page
      tabItem(tabName = "conditions_eligibility",
              tagList(
                conditionalPanel(
                  condition = "output.dataUploaded == false",
                  fluidRow(
                    column(10, offset = 1,
                           box(
                             width = 12, 
                             title = "📤 Upload Required", 
                             status = "warning", 
                             solidHeader = TRUE,
                             p("Please upload your data files to view Clinical Conditions.")
                           )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "output.dataUploaded == true",
                  # Value Boxes Section - 4 per row (ALPHABETICAL ORDER by Diagnosis code)
                  fluidRow(
                    column(10, offset = 1,
                           fluidRow(
                             column(3, uiOutput("b_dia_eligibility")),     # 1. B_DIA - Bloody diarrhoea
                             column(3, uiOutput("dent_eligibility")),      # 2. DENT - Dental infections
                             column(3, uiOutput("ent_eligibility")),       # 3. ENT - ENT infections
                             column(3, uiOutput("lymph_eligibility"))      # 4. LYMPH - Lymph nodes
                           )
                    )
                  ),
                  fluidRow(
                    column(10, offset = 1,
                           fluidRow(
                             column(3, uiOutput("non_b_dia_eligibility")), # 5. Non_B_DIA - Non-bloody diarrhoea
                             column(3, uiOutput("rti_eligibility")),       # 6. RTI - Respiratory tract
                             column(3, uiOutput("ssti_eligibility")),      # 7. SSTI - Skin/Soft Tissue
                             column(3, uiOutput("uti_eligibility"))        # 8. UTI - Lower UTI
                           )
                    )
                  ),
                  # Information Box Below Value Boxes
                  fluidRow(
                    column(10, offset = 1,
                           box(width = 12, 
                               title = "Patient Eligibility Check", 
                               status = "primary", 
                               solidHeader = TRUE,
                               tags$div(style = "font-size: 16px;",
                                        strong("Note:"), 
                                        tags$ul(
                                          tags$li(strong("Fewer than 10 patients cannot be accessed")),
                                          tags$li(strong("If you do not have a lot of eligible conditions – go back to general summary, or overall diagnoses table for more insights on available conditions"))
                                        )
                               )
                           )
                    )
                  )
                )
              )
      ),
      
      # Condition-specific tabs
      # RTI
      tabItem(tabName = "rti_overview", rtiOverviewUI("rti_module")),
      tabItem(tabName = "rti_oral", rtiOralUI("rti_module")),
      tabItem(tabName = "rti_aware", rtiAWaReUI("rti_module")),
      tabItem(tabName = "rti_choice", rtiChoiceUI("rti_module")),
      tabItem(tabName = "rti_dosage", rtiDosageUI("rti_module")),
      tabItem(tabName = "rti_duration", rtiDurationUI("rti_module")),
      
      # DENT 
      tabItem(tabName = "dent_overview", dentOverviewUI("dent")),
      tabItem(tabName = "dent_oral", dentOralUI("dent")),
      tabItem(tabName = "dent_aware", dentAWaReUI("dent")),
      tabItem(tabName = "dent_choice", dentChoiceUI("dent")),
      tabItem(tabName = "dent_dosage", dentDosageUI("dent")),
      tabItem(tabName = "dent_duration", dentDurationUI("dent")),
      
      # UTI
      tabItem(tabName = "uti_op_overview", utiOutpatientOverviewUI("uti_outpatient_module")),
      tabItem(tabName = "uti_op_oral", utiOutpatientOralUI("uti_outpatient_module")),
      tabItem(tabName = "uti_op_aware", utiOutpatientAWaReUI("uti_outpatient_module")),
      tabItem(tabName = "uti_op_choice", utiOutpatientChoiceUI("uti_outpatient_module")),
      tabItem(tabName = "uti_op_dosage", utiOutpatientDosageUI("uti_outpatient_module")),
      tabItem(tabName = "uti_op_duration", utiOutpatientDurationUI("uti_outpatient_module")),
      
      # SSTI
      tabItem(tabName = "ssti_op_overview", sstiOutpatientOverviewUI("ssti_outpatient_module")),
      tabItem(tabName = "ssti_op_oral", sstiOutpatientOralUI("ssti_outpatient_module")),
      tabItem(tabName = "ssti_op_aware", sstiOutpatientAWaReUI("ssti_outpatient_module")),
      tabItem(tabName = "ssti_op_choice", sstiOutpatientChoiceUI("ssti_outpatient_module")),
      tabItem(tabName = "ssti_op_dosage", sstiOutpatientDosageUI("ssti_outpatient_module")),
      tabItem(tabName = "ssti_op_duration", sstiOutpatientDurationUI("ssti_outpatient_module")),
      
      # ENT
      tabItem(tabName = "ent_overview", entOutpatientOverviewUI("ent_module")),
      tabItem(tabName = "ent_oral", entOutpatientOralUI("ent_module")),
      
      # BLOODY DIARRHOEA - All content in single UI
      tabItem(tabName = "bloody_diarrhoea_overview", bloodyDiarrhoeaOverviewUI("bloody_diarrhoea_module")),
      tabItem(tabName = "bloody_diarrhoea_oral", bloodyDiarrhoeaOralUI("bloody_diarrhoea_module")),
      tabItem(tabName = "bloody_diarrhoea_aware", bloodyDiarrhoeaAWaReUI("bloody_diarrhoea_module")),
      
      # Non-bloody Diarrhoea tabs - Two separate UI functions
      tabItem(tabName = "non_bloody_diarrhoea_overview", nonBloodyDiarrhoeaOverviewUI("non_bloody_diarrhoea_module")),
      tabItem(tabName = "non_bloody_diarrhoea_oral", nonBloodyDiarrhoeaOralUI("non_bloody_diarrhoea_module")),
      
      # LYMPH
      tabItem(tabName = "lymph_overview", lymphOverviewUI("lymph")),
      tabItem(tabName = "lymph_oral", lymphOralUI("lymph")),
      tabItem(tabName = "lymph_aware", lymphAWaReUI("lymph")),
      tabItem(tabName = "lymph_choice", lymphChoiceUI("lymph")),
      tabItem(tabName = "lymph_dosage", lymphDosageUI("lymph")),
      tabItem(tabName = "lymph_duration", lymphDurationUI("lymph")),
      
      # Download Raw Data Tab
      tabItem(tabName = "download_rawdata",
              tagList(
                conditionalPanel(
                  condition = "output.dataUploaded == false",
                  fluidRow(
                    column(10, offset = 1,
                           box(
                             width = 12, 
                             title = "📤 Upload Required", 
                             status = "warning", 
                             solidHeader = TRUE,
                             p("Please upload your data file")
                           )
                    )
                  )
                ),
                
                
                conditionalPanel(
                  condition = "output.dataUploaded == true",
                  fluidRow(
                    column(10, offset = 1,
                           box(
                             title = "📥 Download Complete Dataset",
                             status = "success",
                             solidHeader = TRUE,
                             width = 12,
                             p(strong("Download the complete patient dataset with QI-eligibility markers")),
                             hr(),
                             div(style = "text-align: center; margin: 30px 0;",
                                 downloadButton("download_combined", 
                                                "Download Complete Data with QI Eligibility", 
                                                class = "btn-success btn-lg",
                                                style = "margin-top: 10px;")
                             )
                           )
                    )
                  )
                )
              )
      )
    ),
    
    div(class = "footer",
        div(class = "footer-content",
            div(class = "footer-background"),
            
            div(class = "footer-logos-section",
                h5("Our Partners & Supporters", 
                   style = "text-align: center; color: #2c3e50; margin-bottom: 20px; font-weight: 600;"),
                div(class = "footer-logos",
                    div(class = "logo-item",
                        tags$a(href = "https://cnpi-amr.org/research/adila/", target = "_blank", class = "logo-link",
                               img(src = "https://github.com/CNPI-ADILA/AWaRe_QI_outpatient/raw/main/www/img/ADILA%20Logo.png", 
                                   class = "footer-logo", alt = "ADILA Logo"),
                               div(class = "logo-label", "ADILA")
                        )
                    ),
                    div(class = "logo-item",
                        img(src = "https://github.com/CNPI-ADILA/AWaRe_QI_outpatient/raw/main/www/img/APG_logo_primary.jpg", 
                            class = "footer-logo", alt = "APG Logo"),
                        div(class = "logo-label", "APG")
                    ),
                    div(class = "logo-item",
                        tags$a(href = "https://cnpi.org.uk/", target = "_blank", class = "logo-link",
                               img(src = "https://github.com/CNPI-ADILA/AWaRe_QI_outpatient/raw/main/www/img/CNPI%20Logo_Horz_RGB.jpg", 
                                   class = "footer-logo", alt = "CNPI Logo"),
                               div(class = "logo-label", "CNPI")
                        )
                    ),
                    div(class = "logo-item city-st-georges",
                        tags$a(href = "https://www.citystgeorges.ac.uk/", target = "_blank", class = "logo-link",
                               img(src = "https://github.com/CNPI-ADILA/AWaRe_QI_outpatient/raw/main/www/img/city_logo.png",
                                   class = "footer-logo city-logo", alt = "City St Georges Logo"),
                               div(class = "logo-label", "City St Georges")
                        )
                    ),
                    div(class = "logo-item",
                        tags$a(href = "https://www.flemingfund.org/", target = "_blank", class = "logo-link",
                               img(src = "https://github.com/CNPI-ADILA/AWaRe_QI_outpatient/raw/main/www/img/Fleming%20Fund%20Logo.png", 
                                   class = "footer-logo", alt = "Fleming Fund Logo"),
                               div(class = "logo-label", "Fleming Fund")
                        )
                    ),
                    div(class = "logo-item",
                        tags$a(href = "https://wellcome.org/", target = "_blank", class = "logo-link",
                               img(src = "https://github.com/CNPI-ADILA/AWaRe_QI_outpatient/raw/main/www/img/Wellcome%20trust%20logo.png", 
                                   class = "footer-logo", alt = "Wellcome Trust Logo"),
                               div(class = "logo-label", "Wellcome Trust")
                        )
                    ),
                   
                    div(class = "logo-item",
                        tags$a(href = "https://www.global-pps.com/", target = "_blank", class = "logo-link",
                               img(src = "https://github.com/CNPI-ADILA/AWaRe_QI_outpatient/raw/main/www/img/logo%20Global%20PPS%20DEF.png", 
                                   class = "footer-logo", alt = "Global PPS Logo"),
                               div(class = "logo-label", "Global PPS")
                        )
                    )
                )
            ),
            
            div(class = "privacy-disclaimer",
                p(HTML("<b>Data Privacy Disclaimer:</b> <i>All data processing occurs locally in your browser session - no patient data or uploaded files are stored on our servers. Once you close the browser or refresh the page, all uploaded data is permanently deleted. We do not collect, store, or transmit any sensitive information.</i>"),
                  style = "text-align: center; color: #6c757d; margin: 10px 0 10px 0; font-size: 14px; font-weight: 500; max-width: 900px; margin-left: auto; margin-right: auto; padding: 0 20px;")
            ),
            
            div(class = "footer-developed-by",
                p("Developed by the CNPI AMR Team",
                  style = "text-align: center; color: #6c757d; margin: 10px 0 0 0; font-size: 14px; font-weight: 500;"),
                p("Version 1.0",
                  style = "text-align: center; color: #6c757d; margin: 10px 0 0 0; font-size: 14px; font-weight: 500;")
            )
        ),
        
        tags$style(HTML("
        .footer { background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%); border-top: 1px solid #e3e6f0; padding: 40px 0 20px 0; position: relative; margin-top: 50px; box-shadow: 0 -4px 20px rgba(0,0,0,0.1); overflow: hidden; }
        .footer-background { position: absolute; top: 0; left: 0; right: 0; bottom: 0; background: radial-gradient(circle at 20% 20%, rgba(52, 152, 219, 0.05) 0%, transparent 50%), radial-gradient(circle at 80% 80%, rgba(39, 174, 96, 0.05) 0%, transparent 50%); animation: footerFloat 8s ease-in-out infinite; }
        @keyframes footerFloat { 0%, 100% { transform: translateY(0px); } 50% { transform: translateY(-10px); } }
        .footer-content { max-width: 1200px; margin: 0 auto; padding: 0 20px; position: relative; z-index: 2; }
        .footer-logos-section { margin-bottom: 20px; }
        .footer-logos { display: flex; justify-content: center; align-items: center; flex-wrap: wrap; gap: 20px; padding: 20px; }
        .logo-item { display: flex; flex-direction: column; align-items: center; transition: all 0.3s ease; padding: 15px; border-radius: 10px; background: rgba(255, 255, 255, 0.8); box-shadow: 0 2px 8px rgba(0,0,0,0.05); position: relative; overflow: hidden; }
        .logo-item::before { content: ''; position: absolute; top: 0; left: -100%; width: 100%; height: 100%; background: linear-gradient(90deg, transparent, rgba(52, 152, 219, 0.1), transparent); transition: left 0.5s ease; }
        .logo-item:hover { transform: translateY(-5px) scale(1.05); box-shadow: 0 8px 25px rgba(0,0,0,0.15); }
        .logo-item:hover::before { left: 100%; }
        .footer-logo { height: 80px; max-width: 120px; object-fit: contain; filter: grayscale(20%); transition: all 0.3s ease; border-radius: 6px; margin-bottom: 8px; }
        .footer-logo.city-logo { height: 80px; max-width: 300px; filter: grayscale(0%); }
        .logo-item:hover .footer-logo { filter: grayscale(0%); transform: scale(1.1); }
        .logo-link { text-decoration: none; display: flex; flex-direction: column; align-items: center; }
        .logo-label { font-size: 11px; color: #6c757d; font-weight: 600; text-align: center; margin-top: 5px; transition: color 0.3s ease; }
        .logo-item:hover .logo-label { color: #2c3e50; }
        .privacy-disclaimer { margin: 10px 0 15px 0; padding: 15px 0; border-top: 1px solid #e3e6f0; }
        
        @media (max-width: 768px) {
          .footer { padding: 30px 0 15px 0; }
          .footer-logos { gap: 15px; padding: 15px; }
          .logo-item { padding: 10px; width: calc(50% - 15px); }
          .footer-logo { height: 50px !important; max-width: 80px !important; }
          .footer-logo.city-logo { height: 55px !important; max-width: 120px !important; }
          .logo-label { font-size: 10px !important; }
        }
        
        @media (max-width: 576px) {
          .footer-logos { gap: 10px; }
          .logo-item { padding: 8px; width: calc(50% - 10px); }
          .footer-logo { height: 40px !important; max-width: 70px !important; }
          .footer-logo.city-logo { height: 45px !important; max-width: 100px !important; }
          .logo-label { font-size: 9px !important; }
          .footer-developed-by p, .privacy-disclaimer p { font-size: 12px !important; }
        }
        
        body { padding-bottom: 200px; }
        .tab-content { padding-bottom: 50px; min-height: calc(100vh - 250px); }
    "))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  observeEvent(input$navigate_to_eligibility, ignoreInit = TRUE, {
    updateTabItems(session, "sidebar", selected = "eligibility")
    session$sendCustomMessage("openMenu", "eligibility")
  })
  
  observeEvent(input$navigate_to_overview, ignoreInit = TRUE, {
    updateTabItems(session, "sidebar", selected = "overview")
    session$sendCustomMessage("openMenu", "overview")
  })
  
  # Reactive values for menu visibility
  menu_visibility <- reactiveValues(
    rti = FALSE,
    ent = FALSE,
    dent = FALSE,
    bloody_diarrhoea = FALSE,
    non_bloody_diarrhoea = FALSE,
    lymph = FALSE,
    ssti = FALSE,
    uti = FALSE
  )
  
  # Output for conditional menu visibility
  output$showRtiMenu <- reactive({ menu_visibility$rti })
  output$showEntMenu <- reactive({ menu_visibility$ent })
  output$showDentMenu <- reactive({ menu_visibility$dent })
  output$showBloodyDiarrhoeaMenu <- reactive({ menu_visibility$bloody_diarrhoea })
  output$showNonBloodyDiarrhoeaMenu <- reactive({ menu_visibility$non_bloody_diarrhoea })
  output$showLymphMenu <- reactive({ menu_visibility$lymph })
  output$showSstiMenu <- reactive({ menu_visibility$ssti })
  output$showUtiMenu <- reactive({ menu_visibility$uti })
  
  outputOptions(output, "showRtiMenu", suspendWhenHidden = FALSE)
  outputOptions(output, "showEntMenu", suspendWhenHidden = FALSE)
  outputOptions(output, "showDentMenu", suspendWhenHidden = FALSE)
  outputOptions(output, "showBloodyDiarrhoeaMenu", suspendWhenHidden = FALSE)
  outputOptions(output, "showNonBloodyDiarrhoeaMenu", suspendWhenHidden = FALSE)
  outputOptions(output, "showLymphMenu", suspendWhenHidden = FALSE)
  outputOptions(output, "showSstiMenu", suspendWhenHidden = FALSE)
  outputOptions(output, "showUtiMenu", suspendWhenHidden = FALSE)
  
  # Reactive data storage
  data_reactive <- reactiveVal(NULL)
  
  # Data upload indicator
  output$dataUploaded <- reactive({ !is.null(data_reactive()) })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  # File upload handler
  observeEvent(input$datafile, {
    req(input$datafile)
    
    tryCatch({
      file_path <- input$datafile$datapath
      
      # Load Survey sheet
      data_info <- tryCatch({
        read_excel(file_path, sheet = "Survey", col_types = c("guess", "date", "guess", "date"))
      }, error = function(e) { 
        data.frame()
      })
      
      # Load Department forms sheet
      data_deps <- read_excel(file_path, sheet = "Department forms")
      
      # Load Outpatient registrations sheet
      data_patients_all <- read_excel(file_path, sheet = "Outpatient registrations")
      
      # Load lookup data from GitHub
      lookup_url <- "https://github.com/CNPI-ADILA/AWaRe_QI_outpatient/raw/main/data/QIs-Lookup-OP.csv"
      data_lookup <- read_csv(lookup_url, show_col_types = FALSE)
      
      # Store the complete patient data for download
      data_patients_complete <- data_patients_all
      
      # Create subset for analysis
      data_patients <- data_patients_all %>%
        select("Type of specialty", "Unique Patient ID", "Patient age group", 
               "Presenting symptom 1", "Presenting symptom 2", "Presenting symptom 3", 
               "Presenting symptom 4", "Presenting symptom 5", "Presenting symptom 6", 
               "Antimicrobial prescribed/administered", "Antimicrobial name", "ATC5", "AWaRe", 
               "Single unit dose", "Unit", "N Doses/day", "Route", "Prescribed/intended duration (days)")
      
      # Clean and standardize data
      data_patients$AWaRe <- toupper(data_patients$AWaRe)
      data_patients[data_patients == ""] <- NA
      
      # CORRECTED: Use tribble with left_join (EXACT R MARKDOWN LOGIC)
      symptom_map <- tribble(
        ~symptom,                 ~`Diagnosis code`,
        "NASAL_CONGEST_NOSE",     "RTI",
        "ACUTE_COUGH",            "RTI",
        "THROAT_SORE",            "RTI",
        "EAR_PAIN",               "RTI",
        "EAR_DISCHARGE",          "RTI",
        
        "EAR_PAIN",               "ENT",
        "EAR_DISCHARGE",          "ENT",
        "THROAT_SORE",            "ENT",
        "NASAL_CONGEST_NOSE",     "ENT",
        
        "TOOTHACHE_GUM_SWELLING", "DENT",
        "DIARRHEA_BLOOD",         "B_DIA",
        "DIARRHEA",               "Non_B_DIA",
        "LIMB_SWELLING_ERYTHEMA", "LYMPH",
        "SKIN_SYMPTOMS",          "SSTI",
        "URINATION_PAIN",         "UTI"
      )
      
      # CORRECTED: Use left_join instead of case_when
      data_patients <- data_patients %>%
        pivot_longer(
          cols = starts_with("Presenting symptom"),
          names_to = "symptom_col",
          values_to = "symptom"
        ) %>%
        left_join(symptom_map, by = "symptom") %>%  # This allows multiple diagnoses per symptom
        distinct(across(-symptom_col))  # drop duplicate rows per patient/diagnosis
      
      # Store all data in reactive
      data_reactive(list(
        data_info = data_info,
        data_deps = data_deps,
        data_patients_all = data_patients_all,
        data_patients = data_patients,
        data_lookup = data_lookup
      ))
      
      showNotification(paste("Data loaded successfully!", n_distinct(data_patients_all$`Unique Patient ID`), "patients found. Lookup data loaded from GitHub."), 
                       type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
    })
  })
  
  # Upload summary
  output$upload_summary <- renderUI({
    req(data_reactive())
    data <- data_reactive()
    
    n_patients <- n_distinct(data$data_patients$`Unique Patient ID`)
    n_prescriptions <- nrow(data$data_patients %>% filter(!is.na(AWaRe)))
    
    HTML(glue(
      "<div style='padding: 15px;'>
        <h4>✅ File uploaded successfully!</h4>
        <ul>
          <li><strong>Total Patients:</strong> {n_patients}</li>
          <li><strong>Total Prescriptions:</strong> {n_prescriptions}</li>
        </ul>
        <p>You can now explore the data using the navigation menu.</p>
      </div>"
    ))
  })
  
  # Get eligibility data
  get_eligibility_data <- reactive({
    req(data_reactive())
    data <- data_reactive()
    
    diagnostic_labels <- c(
      RTI = "Respiratory tract infections",
      ENT = "Ear, Nose, Throat infections",
      DENT = "Dental infections",
      B_DIA = "Bloody diarrhoea",
      Non_B_DIA = "Non-bloody diarrhoea",
      LYMPH = "Lymph nodes Infections",
      SSTI = "Skin/Soft Tissue Infections",
      UTI = "Lower urinary tract infections"
    )
    
    # Calculate eligible counts
    eligible_counts <- data$data_patients %>%
      filter(`Diagnosis code` %in% names(diagnostic_labels)) %>%
      mutate(
        Route = toupper(Route),
        AWaRe_compatible = (`Patient age group` == "ADULT")
      ) %>%
      filter(AWaRe_compatible) %>%
      distinct(`Diagnosis code`, `Unique Patient ID`) %>%
      count(`Diagnosis code`, name = "n_eligible")
    
    # Calculate total counts for each diagnosis
    total_counts <- data$data_patients %>%
      filter(`Diagnosis code` %in% names(diagnostic_labels)) %>%
      distinct(`Diagnosis code`, `Unique Patient ID`) %>%
      count(`Diagnosis code`, name = "n_total")
    
    full_diagnostics <- tibble(`Diagnosis code` = names(diagnostic_labels))
    
    eligible_summary <- full_diagnostics %>%
      left_join(eligible_counts, by = "Diagnosis code") %>%
      left_join(total_counts, by = "Diagnosis code") %>%
      mutate(
        `N of Eligible Patients` = replace_na(n_eligible, 0),
        `Total Patients` = replace_na(n_total, 0),
        Condition = diagnostic_labels[`Diagnosis code`],
        Status = ifelse(`N of Eligible Patients` > 0, "Sufficient", "No Data"),
        BgColor = ifelse(`N of Eligible Patients` > 0, "#28a745", "#fd7e14"),
        Emoji = ifelse(`N of Eligible Patients` > 0, "✅", "⚠️")
      ) %>%
      arrange(`Diagnosis code`) %>%
      select(Condition, `N of Eligible Patients`, `Total Patients`, Status, BgColor, Emoji, `Diagnosis code`)
    
    return(eligible_summary)
  })
  
  # Individual value box outputs with updated icons
  output$rti_eligibility <- renderUI({
    eligibility_data <- get_eligibility_data()
    rti_data <- eligibility_data %>% filter(`Diagnosis code` == "RTI")
    
    box_class <- ifelse(rti_data$`N of Eligible Patients` > 0, 
                        "custom-value-box", 
                        "custom-value-box red")
    
    status_text <- ifelse(rti_data$`N of Eligible Patients` > 0,
                          "✅ Sufficient",
                          "⚠️ No Data")
    
    div(class = box_class,
        onclick = "Shiny.setInputValue('condition_rti_clicked', Math.random());",
        div(class = "custom-value-number", formatC(rti_data$`N of Eligible Patients`, format = "d", big.mark = ",")),
        div(class = "custom-value-title", "Respiratory Tract Infections"),
        div(style = "font-size: 14px; margin: 5px 0; font-weight: 500;", 
            paste0("out of ", formatC(rti_data$`Total Patients`, format = "d", big.mark = ","), " patients")),
        div(class = "custom-value-status", status_text),
        div(class = "custom-value-icon", icon("lungs-virus"))
    )
  })
  
  output$ent_eligibility <- renderUI({
    eligibility_data <- get_eligibility_data()
    ent_data <- eligibility_data %>% filter(`Diagnosis code` == "ENT")
    
    box_class <- ifelse(ent_data$`N of Eligible Patients` > 0, 
                        "custom-value-box", 
                        "custom-value-box red")
    
    status_text <- ifelse(ent_data$`N of Eligible Patients` > 0,
                          "✅ Sufficient",
                          "⚠️ No Data")
    
    div(class = box_class,
        onclick = "Shiny.setInputValue('condition_ent_clicked', Math.random());",
        div(class = "custom-value-number", formatC(ent_data$`N of Eligible Patients`, format = "d", big.mark = ",")),
        div(class = "custom-value-title", "Ear, Nose, Throat infections"),
        div(style = "font-size: 14px; margin: 5px 0; font-weight: 500;", 
            paste0("out of ", formatC(ent_data$`Total Patients`, format = "d", big.mark = ","), " patients")),
        div(class = "custom-value-status", status_text),
        div(class = "custom-value-icon", icon("head-side-virus"))
    )
  })
  
  output$dent_eligibility <- renderUI({
    eligibility_data <- get_eligibility_data()
    dent_data <- eligibility_data %>% filter(`Diagnosis code` == "DENT")
    
    box_class <- ifelse(dent_data$`N of Eligible Patients` > 0, 
                        "custom-value-box", 
                        "custom-value-box red")
    
    status_text <- ifelse(dent_data$`N of Eligible Patients` > 0,
                          "✅ Sufficient",
                          "⚠️ No Data")
    
    div(class = box_class,
        onclick = "Shiny.setInputValue('condition_dent_clicked', Math.random());",
        div(class = "custom-value-number", formatC(dent_data$`N of Eligible Patients`, format = "d", big.mark = ",")),
        div(class = "custom-value-title", "Dental Infections"),
        div(style = "font-size: 14px; margin: 5px 0; font-weight: 500;", 
            paste0("out of ", formatC(dent_data$`Total Patients`, format = "d", big.mark = ","), " patients")),
        div(class = "custom-value-status", status_text),
        div(class = "custom-value-icon", icon("tooth"))
    )
  })
  
  # BLOODY DIARRHOEA - Updated icon
  output$b_dia_eligibility <- renderUI({
    eligibility_data <- get_eligibility_data()
    b_dia_data <- eligibility_data %>% filter(`Diagnosis code` == "B_DIA")
    
    box_class <- ifelse(b_dia_data$`N of Eligible Patients` > 0, 
                        "custom-value-box", 
                        "custom-value-box red")
    
    status_text <- ifelse(b_dia_data$`N of Eligible Patients` > 0,
                          "✅ Sufficient",
                          "⚠️ No Data")
    
    div(class = box_class,
        onclick = "Shiny.setInputValue('condition_b_dia_clicked', Math.random());",
        div(class = "custom-value-number", formatC(b_dia_data$`N of Eligible Patients`, format = "d", big.mark = ",")),
        div(class = "custom-value-title", "Bloody Diarrhoea"),
        div(style = "font-size: 14px; margin: 5px 0; font-weight: 500;", 
            paste0("out of ", formatC(b_dia_data$`Total Patients`, format = "d", big.mark = ","), " patients")),
        div(class = "custom-value-status", status_text),
        div(class = "custom-value-icon", icon("poop"))
    )
  })
  
  # NON-BLOODY DIARRHOEA - Updated icon
  output$non_b_dia_eligibility <- renderUI({
    eligibility_data <- get_eligibility_data()
    non_b_dia_data <- eligibility_data %>% filter(`Diagnosis code` == "Non_B_DIA")
    
    box_class <- ifelse(non_b_dia_data$`N of Eligible Patients` > 0, 
                        "custom-value-box", 
                        "custom-value-box red")
    
    status_text <- ifelse(non_b_dia_data$`N of Eligible Patients` > 0,
                          "✅ Sufficient",
                          "⚠️ No Data")
    
    div(class = box_class,
        onclick = "Shiny.setInputValue('condition_non_b_dia_clicked', Math.random());",
        div(class = "custom-value-number", formatC(non_b_dia_data$`N of Eligible Patients`, format = "d", big.mark = ",")),
        div(class = "custom-value-title", "Non-bloody Diarrhoea"),
        div(style = "font-size: 14px; margin: 5px 0; font-weight: 500;", 
            paste0("out of ", formatC(non_b_dia_data$`Total Patients`, format = "d", big.mark = ","), " patients")),
        div(class = "custom-value-status", status_text),
        div(class = "custom-value-icon", icon("toilet"))
    )
  })
  
  # LYMPH - Updated icon
  output$lymph_eligibility <- renderUI({
    eligibility_data <- get_eligibility_data()
    lymph_data <- eligibility_data %>% filter(`Diagnosis code` == "LYMPH")
    
    box_class <- ifelse(lymph_data$`N of Eligible Patients` > 0, 
                        "custom-value-box", 
                        "custom-value-box red")
    
    status_text <- ifelse(lymph_data$`N of Eligible Patients` > 0,
                          "✅ Sufficient",
                          "⚠️ No Data")
    
    div(class = box_class,
        onclick = "Shiny.setInputValue('condition_lymph_clicked', Math.random());",
        div(class = "custom-value-number", formatC(lymph_data$`N of Eligible Patients`, format = "d", big.mark = ",")),
        div(class = "custom-value-title", "Lymph Node Infections"),
        div(style = "font-size: 14px; margin: 5px 0; font-weight: 500;", 
            paste0("out of ", formatC(lymph_data$`Total Patients`, format = "d", big.mark = ","), " patients")),
        div(class = "custom-value-status", status_text),
        div(class = "custom-value-icon", icon("circle-nodes"))
    )
  })
  
  # SSTI - Updated icon
  output$ssti_eligibility <- renderUI({
    eligibility_data <- get_eligibility_data()
    ssti_data <- eligibility_data %>% filter(`Diagnosis code` == "SSTI")
    
    box_class <- ifelse(ssti_data$`N of Eligible Patients` > 0, 
                        "custom-value-box", 
                        "custom-value-box red")
    
    status_text <- ifelse(ssti_data$`N of Eligible Patients` > 0,
                          "✅ Sufficient",
                          "⚠️ No Data")
    
    div(class = box_class,
        onclick = "Shiny.setInputValue('condition_ssti_clicked', Math.random());",
        div(class = "custom-value-number", formatC(ssti_data$`N of Eligible Patients`, format = "d", big.mark = ",")),
        div(class = "custom-value-title", "Skin/Soft Tissue Infections"),
        div(style = "font-size: 14px; margin: 5px 0; font-weight: 500;", 
            paste0("out of ", formatC(ssti_data$`Total Patients`, format = "d", big.mark = ","), " patients")),
        div(class = "custom-value-status", status_text),
        div(class = "custom-value-icon", icon("bandage"))
    )
  })
  
  output$uti_eligibility <- renderUI({
    eligibility_data <- get_eligibility_data()
    uti_data <- eligibility_data %>% filter(`Diagnosis code` == "UTI")
    
    box_class <- ifelse(uti_data$`N of Eligible Patients` > 0, 
                        "custom-value-box", 
                        "custom-value-box red")
    
    status_text <- ifelse(uti_data$`N of Eligible Patients` > 0,
                          "✅ Sufficient",
                          "⚠️ No Data")
    
    div(class = box_class,
        onclick = "Shiny.setInputValue('condition_uti_clicked', Math.random());",
        div(class = "custom-value-number", formatC(uti_data$`N of Eligible Patients`, format = "d", big.mark = ",")),
        div(class = "custom-value-title", "Lower urinary tract infections"),
        div(style = "font-size: 14px; margin: 5px 0; font-weight: 500;", 
            paste0("out of ", formatC(uti_data$`Total Patients`, format = "d", big.mark = ","), " patients")),
        div(class = "custom-value-status", status_text),
        div(class = "custom-value-icon", icon("droplet"))
    )
  })
  
  # Hide all condition menus
  hide_all_menus <- function() {
    menu_visibility$rti <- FALSE
    menu_visibility$ent <- FALSE
    menu_visibility$dent <- FALSE
    menu_visibility$bloody_diarrhoea <- FALSE
    menu_visibility$non_bloody_diarrhoea <- FALSE
    menu_visibility$lymph <- FALSE
    menu_visibility$ssti <- FALSE
    menu_visibility$uti <- FALSE
  }
  
  # Navigation back to eligibility page
  observeEvent(input$navigate_to_eligibility, {
    updateTabItems(session, "sidebar", selected = "conditions_eligibility")
  })
  
  observeEvent(input$navigate_to_overview, ignoreInit = TRUE, {
    updateTabItems(session, "sidebar", selected = "overview")
    session$sendCustomMessage("openMenu", "overview")
  })
  
  # Condition card click handlers
  observeEvent(input$condition_rti_clicked, {
    eligibility_data <- get_eligibility_data()
    rti_count <- eligibility_data %>% filter(`Diagnosis code` == "RTI") %>% pull(`N of Eligible Patients`)
    
    if (rti_count >= 0) {
      hide_all_menus()
      menu_visibility$rti <- TRUE
      updateTabItems(session, "sidebar", selected = "rti_overview")
      session$sendCustomMessage("openMenu", "rti_overview")
      showNotification("Navigating to RTI Analysis Overview.", type = "message", duration = 3)
    } else {
      showNotification("Insufficient data for RTI analysis.", type = "warning", duration = 3)
    }
  })
  
  observeEvent(input$condition_ent_clicked, {
    eligibility_data <- get_eligibility_data()
    ent_count <- eligibility_data %>% filter(`Diagnosis code` == "ENT") %>% pull(`N of Eligible Patients`)
    
    if (ent_count >= 0) {
      hide_all_menus()
      menu_visibility$ent <- TRUE
      updateTabItems(session, "sidebar", selected = "ent_overview")
      session$sendCustomMessage("openMenu", "ent_overview")
      showNotification("Navigating to ENT Infections Analysis Overview.", type = "message", duration = 3)
    } else {
      showNotification("Insufficient data for ENT analysis.", type = "warning", duration = 3)
    }
  })
  
  observeEvent(input$condition_dent_clicked, {
    eligibility_data <- get_eligibility_data()
    dent_count <- eligibility_data %>% filter(`Diagnosis code` == "DENT") %>% pull(`N of Eligible Patients`)
    
    if (dent_count >= 0) {
      hide_all_menus()
      menu_visibility$dent <- TRUE
      updateTabItems(session, "sidebar", selected = "dent_overview")
      session$sendCustomMessage("openMenu", "dent_overview")
      showNotification("Navigating to Dental Infections Analysis Overview.", type = "message", duration = 3)
    } else {
      showNotification("Insufficient data for Dental Infections analysis.", type = "warning", duration = 3)
    }
  })
  
  # Bloody Diarrhoea Click Handler
  observeEvent(input$condition_b_dia_clicked, {
    eligibility_data <- get_eligibility_data()
    b_dia_count <- eligibility_data %>% filter(`Diagnosis code` == "B_DIA") %>% pull(`N of Eligible Patients`)
    
    if (b_dia_count >= 0) {
      hide_all_menus()
      menu_visibility$bloody_diarrhoea <- TRUE
      updateTabItems(session, "sidebar", selected = "bloody_diarrhoea_overview")
      session$sendCustomMessage("openMenu", "bloody_diarrhoea_overview")
      showNotification("Navigating to Bloody Diarrhoea Analysis Overview.", type = "message", duration = 3)
    } else {
      showNotification("Insufficient data for Bloody Diarrhoea analysis.", type = "warning", duration = 3)
    }
  })
  
  # Non-bloody Diarrhoea Click Handler
  observeEvent(input$condition_non_b_dia_clicked, {
    eligibility_data <- get_eligibility_data()
    non_b_dia_count <- eligibility_data %>% filter(`Diagnosis code` == "Non_B_DIA") %>% pull(`N of Eligible Patients`)
    
    if (non_b_dia_count >= 0) {
      hide_all_menus()
      menu_visibility$non_bloody_diarrhoea <- TRUE
      updateTabItems(session, "sidebar", selected = "non_bloody_diarrhoea_overview")
      session$sendCustomMessage("openMenu", "non_bloody_diarrhoea_overview")
      showNotification("Navigating to Non-bloody Diarrhoea Analysis Overview.", type = "message", duration = 3)
    } else {
      showNotification("Insufficient data for Non-bloody Diarrhoea analysis.", type = "warning", duration = 3)
    }
  })
  
  observeEvent(input$condition_lymph_clicked, {
    eligibility_data <- get_eligibility_data()
    lymph_count <- eligibility_data %>% filter(`Diagnosis code` == "LYMPH") %>% pull(`N of Eligible Patients`)
    
    if (lymph_count >= 0) {
      hide_all_menus()
      menu_visibility$lymph <- TRUE
      updateTabItems(session, "sidebar", selected = "lymph_overview")
      session$sendCustomMessage("openMenu", "lymph_overview")
      showNotification("Navigating to Lymph Node Infections Analysis Overview.", type = "message", duration = 3)
    } else {
      showNotification("Insufficient data for Lymph Node Infections analysis.", type = "warning", duration = 3)
    }
  })
  
  observeEvent(input$condition_ssti_clicked, {
    eligibility_data <- get_eligibility_data()
    ssti_count <- eligibility_data %>% filter(`Diagnosis code` == "SSTI") %>% pull(`N of Eligible Patients`)
    
    if (ssti_count >= 0) {
      hide_all_menus()
      menu_visibility$ssti <- TRUE
      updateTabItems(session, "sidebar", selected = "ssti_op_overview")
      session$sendCustomMessage("openMenu", "ssti_op_overview")
      showNotification("Navigating to SSTI Analysis Overview.", type = "message", duration = 3)
    } else {
      showNotification("Insufficient data for SSTI analysis.", type = "warning", duration = 3)
    }
  })
  
  observeEvent(input$condition_uti_clicked, {
    eligibility_data <- get_eligibility_data()
    uti_count <- eligibility_data %>% filter(`Diagnosis code` == "UTI") %>% pull(`N of Eligible Patients`)
    
    if (uti_count >= 0) {
      hide_all_menus()
      menu_visibility$uti <- TRUE
      updateTabItems(session, "sidebar", selected = "uti_op_overview")
      session$sendCustomMessage("openMenu", "uti_op_overview")
      showNotification("Navigating to Lower UTI Analysis Overview.", type = "message", duration = 3)
    } else {
      showNotification("Insufficient data for Lower UTI analysis.", type = "warning", duration = 3)
    }
  })
  
  # Call the module servers
  outpatientGeneralSummaryServer("general_summary", data_reactive)
  dentAnalysisServer("dent", data_reactive)
  utiOutpatientAnalysisServer("uti_outpatient_module", data_reactive)
  sstiOutpatientAnalysisServer("ssti_outpatient_module", data_reactive)
  rtiAnalysisServer("rti_module", data_reactive)
  entOutpatientAnalysisServer("ent_module", data_reactive)
  bloodyDiarrhoeaModuleServer("bloody_diarrhoea_module", data_reactive)
  nonBloodyDiarrhoeaModuleServer("non_bloody_diarrhoea_module", data_reactive)
  lymphAnalysisServer("lymph", data_reactive)
  
  # Download handler
  output$download_combined <- downloadHandler(
    filename = function() {
      paste0("Outpatient_Complete_Data_with_QI_Eligibility_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      data <- data_reactive()
      req(data)
      
      data_patients_all <- data$data_patients_all
      
      infection_types <- c("RTI", "ENT", "DENT", "B_DIA", "Non_B_DIA", "LYMPH", "SSTI", "UTI")
      
      data_with_eligibility <- data_patients_all %>%
        mutate(
          # Add diagnosis code
          `Diagnosis code` = NA_character_,
          AWaRe_compatible = FALSE
        )
      
      # Mark QI-eligible patients
      for (i in 1:nrow(data_with_eligibility)) {
        symptoms <- c(
          data_with_eligibility$`Presenting symptom 1`[i],
          data_with_eligibility$`Presenting symptom 2`[i],
          data_with_eligibility$`Presenting symptom 3`[i],
          data_with_eligibility$`Presenting symptom 4`[i],
          data_with_eligibility$`Presenting symptom 5`[i],
          data_with_eligibility$`Presenting symptom 6`[i]
        )
        
        if (any(symptoms %in% c("NASAL_CONGEST_NOSE", "ACUTE_COUGH", "THROAT_SORE", "EAR_PAIN", "EAR_DISCHARGE"))) {
          data_with_eligibility$`Diagnosis code`[i] <- "RTI"
        } else if (any(symptoms %in% c("EAR_PAIN", "EAR_DISCHARGE", "THROAT_SORE", "NASAL_CONGEST_NOSE"))) {
          data_with_eligibility$`Diagnosis code`[i] <- "ENT"
        } else if ("TOOTHACHE_GUM_SWELLING" %in% symptoms) {
          data_with_eligibility$`Diagnosis code`[i] <- "DENT"
        } else if ("DIARRHEA_BLOOD" %in% symptoms) {
          data_with_eligibility$`Diagnosis code`[i] <- "B_DIA"
        } else if ("DIARRHEA" %in% symptoms) {
          data_with_eligibility$`Diagnosis code`[i] <- "Non_B_DIA"
        } else if ("LIMB_SWELLING_ERYTHEMA" %in% symptoms) {
          data_with_eligibility$`Diagnosis code`[i] <- "LYMPH"
        } else if ("SKIN_SYMPTOMS" %in% symptoms) {
          data_with_eligibility$`Diagnosis code`[i] <- "SSTI"
        } else if ("URINATION_PAIN" %in% symptoms) {
          data_with_eligibility$`Diagnosis code`[i] <- "UTI"
        }
        
        if (data_with_eligibility$`Patient age group`[i] == "ADULT" && 
            !is.na(data_with_eligibility$`Diagnosis code`[i])) {
          data_with_eligibility$AWaRe_compatible[i] <- TRUE
        }
      }
      
      data_with_eligibility <- data_with_eligibility %>%
        mutate(
          `QI-Eligible` = ifelse(AWaRe_compatible, "YES", "NO")
        ) %>%
        select(-AWaRe_compatible)
      
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Patient Data")
      openxlsx::writeData(wb, "Patient Data", data_with_eligibility)
      
      qi_col_index <- which(names(data_with_eligibility) == "QI-Eligible")
      yes_rows <- which(data_with_eligibility$`QI-Eligible` == "YES") + 1
      
      yellow_style <- openxlsx::createStyle(fgFill = "#FFFF00", fontColour = "#000000", textDecoration = "bold")
      header_style <- openxlsx::createStyle(fgFill = "#4472C4", fontColour = "#FFFFFF", textDecoration = "bold", 
                                            border = "TopBottomLeftRight", borderColour = "#000000")
      
      openxlsx::addStyle(wb, "Patient Data", header_style, rows = 1, cols = 1:ncol(data_with_eligibility), 
                         gridExpand = TRUE)
      
      if(length(yes_rows) > 0) {
        openxlsx::addStyle(wb, "Patient Data", yellow_style, rows = yes_rows, cols = qi_col_index, 
                           gridExpand = TRUE)
      }
      
      openxlsx::setColWidths(wb, "Patient Data", cols = 1:ncol(data_with_eligibility), widths = "auto")
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      
      eligible_count <- sum(data_with_eligibility$`QI-Eligible` == "YES")
      total_count <- nrow(data_with_eligibility)
      
      showNotification(
        paste0("Complete dataset downloaded successfully! ", 
               eligible_count, " of ", total_count, " records are QI-eligible (highlighted in yellow)."), 
        type = "message", 
        duration = 5
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)