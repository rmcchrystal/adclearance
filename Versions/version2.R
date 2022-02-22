
###########################################################################
###########################################################################
###                                                                     ###
###                      ADCLEARANCE | VERSION TWO                      ###
###                      RYAN MCCHRYSTAL | 40205088                     ###
###                                                                     ###
###########################################################################
###########################################################################

# Required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(readxl)
library(lubridate)
library(hms)


##================================================================
##                        USER INTERFACE                         =
##================================================================

ui <- dashboardPage(
    
  ####################################
  #              LOGO                #
  ####################################
  
    # Header for AdCLEARance
    dashboardHeader(
      
        # HTML to edit height of header
        tags$li(class = "dropdown",
              tags$style(".main-header {max-height: 70px}"),
              tags$style(".main-header .logo {height: 70px}")),
        
        # Adding logo to header
        title = tags$img(src="AdCLEARance.png",
                         height = '70', 
                         width = '200')),
  
  ####################################
  #             SIDEBAR              #
  ####################################
  
  # Sidebar menus
  dashboardSidebar(
    # HTML to adjust sidebar dimensions with expanded header
    tags$style(".left-side, .main-sidebar {padding-top: 70px}"),
    # Creating items
    sidebarMenu(
      # Tabs
      menuItem("Dashboard", tabName = "main", icon = icon("dashboard")),
      menuItem("Calculate", tabName = "calculate", icon = icon("calculator"))
    )
  ),

  ####################################
  #              BODY                #
  ####################################
  
    # Content of dashboard
    dashboardBody(
      
      # Extend page and add scrollbar
      tags$head(tags$style(HTML('.content-wrapper { height: 2500px !important; }'))),
        
        tabItems(
            
            tabItem(
              
              # Dashboard page
              tabName = "main",
              h1(HTML("<h1><b>Patient HTS 6% Adherence Dashboard</b></h1>")),
              
              # Row 1 - Upload data and select patient
              fluidRow(

                  # Upload data
                  box(title = HTML("<b>Upload eFlow Dataset</b>"),
                      fileInput("upload", "File must be in Excel worksheet (.xlsx) format"),
                      height = 140),
                  
                  # Patient selection
                  box(title = HTML("<b>Select a Patient</b>"),
                      HTML("<b>Choose a Patient ID from the dropdown list</b>"),
                      selectInput("patient", "", choices = c("")),
                      height = 140)
                  
                  ),
              
              # Row 2 - Summary information on adherence
              fluidRow(
                
                # Column 1 - Adherence summary
                column(width = 6,
                       box(title = HTML("<b> Adherence Overview & Feedback </b>"),
                           # Overall
                           valueBoxOutput("overall"),
                           # Mean number of daily attempts
                           valueBoxOutput("attempts"),
                           # Mean number of valid daily nebulisations
                           valueBoxOutput("valid"),
                           # Recommendation for intervention
                           valueBoxOutput("recommend", width = 12),
                           width = 13,
                           height = 350)),
                
                # Column 2 - Errors and recommendation
                column(width = 6,
                       box(title = HTML("<b> Overview of Nebuliser Usage </b>"),
                           # Most common nebulisation error
                           valueBoxOutput("cleaning", width = 3),
                           # Most common cleaning error
                           valueBoxOutput("error", width = 9),
                           # Daily
                           valueBoxOutput("daily"),
                           # Weekly
                           valueBoxOutput("weekly"),
                           # Monthly
                           valueBoxOutput("monthly"),
                           width = 13,
                           height = 350))
                
                ),
              
            
              # Row 3 - Visualisation of cumulative adherence
              fluidRow(
                
                # Cumulative adherence
                box(title = HTML("<b>Cumulative Medication Adherence Throughout Study Enrollment</b>"),
                    id = "plottab1",
                    height = "600px",
                    width = 12,
                    plotOutput("plot1", height = 500))
                
                ),
              
              # Row 4 - Visualisations of adherence
              fluidRow(
                
                # Nebulisation plots
                box(title = HTML("<b>Number of Valid Daily Nebulisations Performed</b>"),
                    id = "plottab2",
                    height = "600px",
                    width = 12,
                    plotOutput("plot2", height = 500))
                
                ),
              
              # Row 5 - Performance vs Lung Function
              fluidRow(
                
                # Nebulisation plots
                box(title = HTML("<b>Changes In Lung Function and Adherence Throughout Study Enrollment</b>"),
                    id = "plottab3",
                    height = "600px",
                    width = 12,
                    plotOutput("plot3", height = 500))
                
                )),
            
            tabItem(
              
              # Calculate tab
              tabName = "calculate",
              h1(HTML("<h1><b>Respiratory and Secondary Calculators</b></h1>")),
              
              # Row 1 - BMI
              fluidRow(
                
                # BMI
                box(title = HTML("<b> Body Mass Index (BMI) </b>"),
                    
                    column(width = 4,
                           numericInput("weight", "Weight (KG)", min = 1, max = 999, value = 0, width = 100),
                           numericInput("height", "Height (CM)", min = 1, max = 999, value = 0, width = 100),
                           actionButton("bmiCalc", "Calculate")),
                    
                    column(width = 8,
                           valueBoxOutput("bmi", width = 100)),
                    
                    width = 3),
                
                # Predictive
                box(title = HTML("<b> Predictive Spirometry </b>"),
                    selectInput("gender", "Gender", choices = c("Male", "Female"), width = 100),
                    numericInput("height2", "Height (CM)", min = 1, max = 999, value = 0, width = 100),
                    actionButton("predCalc", "Calculate"),
                    width = 8)
                
              )
              
              # Row 2 - BSI and FACED
              
            )
            
            )))


##================================================================
##                     APP FUNCTIONALITY                         =
##================================================================

# Adding functionality
server <- function(input, output, session) {
    
    # Assign user input as data
    inputGrab <- reactive({req(input$upload)})
    
    ####################################
    #           DATA TIDYING           #
    ####################################
    
    # Creates tidied dataset of adherence data
    inputData <- reactive({
      
        # Read input data as excel spreadsheet
        dat <- read_excel(inputGrab()$datapath)
        
        # Removing NAs for VALID_INHAL from pre-trial dates
        dat <- dat[!is.na(dat$VALID_INHAL),]
        
        # Removing NA patient data
        raw_data <- dat[!is.na(dat$PATIENTNUMBER),]
        
        # Identifying test IDs
        unique(raw_data$PATIENTNUMBER)
        
        # Removing remaining test patient data
        # Fixing incorrectly formatted IDs
        data_IDs_fixed <- 
          raw_data %>% 
          filter(!(PATIENTNUMBER %in% c("999", "a1b2c3", "DummyPatient111", "3"))) %>%
          mutate(PATIENTNUMBER = 
                   ifelse(nchar(PATIENTNUMBER) == 5, paste0("R", PATIENTNUMBER),
                          ifelse(grepl("r", PATIENTNUMBER), gsub("r", "R", PATIENTNUMBER), PATIENTNUMBER)))
        
        # Extracting datetime
        # Filtering to trial duration
        data_datetime <- 
          data_IDs_fixed %>%
          mutate(
            DATE = strtrim(INHAL_ID, 8),
            TIME = substr(INHAL_ID, 9, 14),
            DATE = as_date(ymd(DATE)),
            TIME = gsub('(..)(?=.)', '\\1:', TIME, perl=TRUE),
            TIME = as_hms(TIME)) %>%
          filter(!is.na(DATE) & DATE > "2018-06-27") %>%
          select(-c(INHAL_ID, SERIALNO))
        
        # Sequencing missing dates between start/end dates
        # Adding week number
        data_completeDates <- 
          data_datetime %>% 
          group_by(PATIENTNUMBER) %>%
          complete(DATE = seq.Date(min(DATE), max(DATE), "days")) %>%
          mutate(WEEK = week(DATE),
                 MONTH = month(DATE)) %>%
          select(PATIENTNUMBER, WEEK, MONTH, DATE, TIME, everything(.)) %>%
          arrange(PATIENTNUMBER, DATE, TIME) %>%
          ungroup()
        
        })
    
    ####################################
    #        ADHERENCE DATASET         #
    ####################################
    
    # Main dataset
    adhData <- reactive({
      
      dat <- inputData()
      
      # Subset and coding
      subset_HTS <- 
        dat %>%
        filter(INHAL_INTERRUPTION %in% c(NA, 4, 104)) %>%
        filter(!(INHAL_INTERRUPTION == 104 & VALID_INHAL == 0)) %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(TIMEDIFF = round((unclass(TIME) - unclass(lag(TIME)))/3600,1)) %>%
        mutate(TYPE = case_when(n() == 2 & TIMEDIFF >= 1 & VALID_INHAL != 0 ~ 1,
                                n() == 1 | TIMEDIFF < 1 | VALID_INHAL == 0 ~ 0)) %>%
        ungroup()
    })
    
    # Daily adherence
    dailyAdh <- reactive({
      
      dat <- adhData()
      
      # Daily adherence
      # Number of daily nebulisations performed
      HTS_dailyNebs <- 
        dat %>%
        group_by(PATIENTNUMBER) %>%
        mutate(DAY = row_number()) %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(NEBS = n()) %>%
        mutate(NEBS = ifelse(is.na(TIME), 0, ifelse(duplicated(NEBS), NA_real_, NEBS))) %>%
        filter(!is.na(NEBS)) %>%
        ungroup()
      
    })
    
    # Cumulative adherence
    dailyCumul <- reactive({
      
      dat <- adhData()
      
      HTS_daily <- 
        dat %>%
        filter(!is.na(TYPE)) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(DAY = row_number(),
               CUMUL = cumsum(TYPE)/DAY * 100)
      
    })
    
    # Weekly adherence/cumulative
    weeklyAdh <- reactive({
      
      dat <- dailyCumul()
      
      HTS_weekly <- 
        dat %>%
        group_by(PATIENTNUMBER, WEEK) %>%
        mutate(WEEK_CUMUL = mean(CUMUL),
               WEEK_ADH = case_when(sum(TYPE)/length(WEEK) > 0.7 ~ 1,
                                    TRUE ~ 0),
               WEEKLY = mean(TYPE)*100) %>%
        filter(!duplicated(WEEK)) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(WEEKS = row_number()) %>%
        select(-c(CUMUL, DAY)) %>%
        ungroup()
      
    })
    
    # Monthly adherence/cumulative
    monthlyAdh <- reactive({
      
      dat <- weeklyAdh()
      
      HTS_monthly <- 
        dat %>%
        group_by(PATIENTNUMBER, MONTH) %>%
        mutate(MONTH_CUMUL = mean(WEEK_CUMUL),
               MONTH_ADH = mean(WEEK_ADH)*100) %>%
        filter(!duplicated(MONTH)) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(MONTHS = row_number()) %>%
        select(-c(WEEK_CUMUL)) %>%
        ungroup()
    })

    ####################################
    #         ATTEMPTS DATASET         #
    ####################################
    
    adhAttempts <- reactive({
      
      dat <- inputData()
      
      # Subset
      subset_attempts <- 
        dat %>%
        filter(INHAL_INTERRUPTION %in% c(1:8))
      
      # Coding and count
      attempts_code <- 
        subset_attempts %>%
        mutate(TYPE = case_when(INHAL_INTERRUPTION == 4 & VALID_INHAL != 0 ~ 1,
                                TRUE ~ 0)) %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(ATTEMPTS = n(),
               ATTEMPTS = ifelse(duplicated(ATTEMPTS), NA, ATTEMPTS)) %>%
        add_count(name = "VALID", wt = TYPE == 1) %>%
        mutate(VALID = ifelse(duplicated(VALID), NA, VALID))
    })
    
    ####################################
    #         ERRORS DATASETS          #
    ####################################
    
    # Nebulisation errors
    errors <- reactive({
      
      dat <- inputData()
      
      # Subset
      subset_errors <- 
        dat %>%
        filter(INHAL_INTERRUPTION %in% c(1:3, 5:8, 101:103, 105:107) & !is.na(VALID_INHAL))
      
      # Adding index to code errors
      errors <- 
        subset_errors %>%
        mutate(ERROR = case_when(INHAL_INTERRUPTION == 1 ~ "Lost supply voltage",
                                 INHAL_INTERRUPTION == 2 ~ "Aerosol head disconnected",
                                 INHAL_INTERRUPTION == 3 ~ "Medication missing",
                                 INHAL_INTERRUPTION == 5 ~ "Manual shutdown",
                                 INHAL_INTERRUPTION == 6 ~ "Battery empty",
                                 INHAL_INTERRUPTION == 7 ~ "Timeout",
                                 INHAL_INTERRUPTION == 8 ~ "Paused Timeout",
                                 INHAL_INTERRUPTION == 101 ~ "Lost supply voltage",
                                 INHAL_INTERRUPTION == 102 ~ "Aerosol head disconnected",
                                 INHAL_INTERRUPTION == 103 ~ "Medication missing",
                                 INHAL_INTERRUPTION == 105 ~ "Manual shutdown",
                                 INHAL_INTERRUPTION == 106 ~ "Battery empty",
                                 INHAL_INTERRUPTION == 107 ~ "Timeout"))
      
    })
    
    # Attempts dataset for thresholds of errors occurring
    all <- reactive({
      
      dat <- inputData()
      
      subset_all <- 
        dat %>%
        filter(INHAL_INTERRUPTION %in% c(1:8, 101:107)) %>%
        group_by(PATIENTNUMBER) %>%
        count() %>%
        rename(All = n)
      
    }) 

    ####################################
    #        CLEANING DATASETS         #
    ####################################
    
    clean <- reactive({
      
      dat <- inputData()
      
      cleandat <- 
        dat %>%
        filter(INHAL_INTERRUPTION %in% c(NA, 104)) %>%
        filter(!(INHAL_INTERRUPTION == 104 & VALID_INHAL %in% c(1,2)))
      
      cleancount <- 
        cleandat %>%
        mutate(CLEANED = case_when(!is.na(VALID_INHAL) ~ 1, TRUE ~ NA_real_)) %>%
        group_by(PATIENTNUMBER, WEEK) %>%
        add_count(name = "WEEK_CLEAN", wt = CLEANED)
    })
    
    ####################################
    #        DASHBOARD DATASET         #
    ####################################
    
    dashData <- reactive({
      
      # Summary variables
      # Total number of adherent days
      dt_HTS_total <- dailyCumul() %>% group_by(PATIENTNUMBER) %>% count(length(DAY)) %>% select(PATIENTNUMBER, TOTAL = n)
      dt_HTS_adherent <- 
        as.data.frame(table(dailyCumul()$PATIENTNUMBER, dailyCumul()$TYPE == 1)) %>% 
        filter(Var2 == TRUE) %>% 
        select(PATIENTNUMBER = Var1, ADH = Freq) %>%
        merge(., dt_HTS_total) %>%
        mutate(DAILY = round(ADH/TOTAL*100, 1))
      
      # Mean weekly adherence
      dt_HTS_totalWk <- weeklyAdh() %>% group_by(PATIENTNUMBER) %>% count(length(WEEK)) %>% select(PATIENTNUMBER, TOTAL_WK = n)
      dt_HTS_adherentWk <- 
        as.data.frame(table(weeklyAdh()$PATIENTNUMBER, weeklyAdh()$WEEK_ADH == 1)) %>%
        filter(Var2 == TRUE) %>%
        select(PATIENTNUMBER = Var1, ADH = Freq) %>%
        merge(., dt_HTS_totalWk) %>%
        mutate(WEEKLY = round(ADH/TOTAL_WK*100, 1))
      
      # Mean monthly adherence
      dt_HTS_totalM <- monthlyAdh() %>% group_by(PATIENTNUMBER) %>% count(length(MONTH)) %>% select(PATIENTNUMBER, TOTAL_M = n)
      dt_HTS_adherentM <- 
        monthlyAdh() %>%
        group_by(PATIENTNUMBER) %>%
        summarise(MONTHLY = round(mean(MONTH_ADH),2))
      
      # Mean attempts at HTS (valid and invalid)
      dt_HTS_attempts <- adhAttempts() %>% group_by(PATIENTNUMBER) %>% summarise(MEAN_ATTEMPTS = round(mean(ATTEMPTS, na.rm = T),2))
      
      # Mean daily valid attempts
      dt_HTS_valid <- adhAttempts() %>% group_by(PATIENTNUMBER) %>% summarise(MEAN_VALID = round(mean(VALID, na.rm = T),2))
      
      # Most common error
      dt_error <- errors() %>% group_by(PATIENTNUMBER) %>% count(ERROR) %>% slice(which.max(n)) %>% ungroup()
      
      # Cleaning frequency weekly
      dt_clean <- clean() %>% group_by(PATIENTNUMBER) %>% summarise(count = sum(WEEK_CLEAN, na.rm = T))
      
      # All attempts
      dt_all <- all()

      # Overall adherence dataset
      dt_HTS_overall <- 
        data.frame(PATIENTNUMBER = dt_HTS_adherent$PATIENTNUMBER,
                   DAILY = dt_HTS_adherent$DAILY,
                   WEEKLY = dt_HTS_adherentWk$WEEKLY,
                   MONTHLY = dt_HTS_adherentM$MONTHLY) %>%
        mutate(OVERALL = round((DAILY + WEEKLY + MONTHLY)/3,1),
               CATEG = case_when(OVERALL >= 80 ~ "Good",
                                 OVERALL < 80 & OVERALL > 50 ~ "Moderate",
                                 OVERALL <= 50 ~ "Low")) %>%
        merge(., dt_HTS_attempts) %>%
        merge(., dt_HTS_valid) %>%
        mutate(VALID_PCNT = round((MEAN_VALID/MEAN_ATTEMPTS)*100,2)) %>%
        left_join(., dt_error) %>%
        left_join(., dt_clean) %>%
        merge(., dt_all) %>%
        mutate(ERROR = ifelse(is.na(ERROR), "None", ERROR),
               count = ifelse(is.na(count), 0, count),
               occur = round(n/All*100,2),
               VALID_CATEG = case_when(VALID_PCNT >= 80 ~ "Good",
                                       VALID_PCNT < 80 & VALID_PCNT > 50 ~ "Moderate",
                                       VALID_PCNT <= 50 ~ "Low"),
               FEEDBACK = case_when(# Best possible feedback
                                    CATEG == "Good" 
                                    & VALID_CATEG == "Good" 
                                    & (MEAN_ATTEMPTS >= 1.6 & MEAN_ATTEMPTS <= 2) ~ "Overall good adherence and performance, keep it up!",
                                    
                                    # Poorest possible feedback
                                    CATEG == "Low"
                                    & VALID_CATEG == "Low"
                                    & (MEAN_ATTEMPTS < 1.6 | MEAN_ATTEMPTS >= 2) ~ "Overall poor adherence and performance, let's discuss?",
                                    
                                    
                                    
                                    
                                    # Good but too few or too many
                                    CATEG == "Good" 
                                    & VALID_CATEG == "Good" 
                                    & (MEAN_ATTEMPTS >= 2) ~ "Good adherence and performance, but too many daily attempts",
                                    
                                    CATEG == "Good" 
                                    & VALID_CATEG == "Good" 
                                    & (MEAN_ATTEMPTS < 1.6) ~ "Good adherence and performance, but too few daily attempts",
                                    
                                    # Good adherence, moderate performance
                                    CATEG == "Good" 
                                    & VALID_CATEG == "Moderate" 
                                    & (MEAN_ATTEMPTS >= 2) ~ "Good adherence, moderate performance and too many daily attempts",
                                    
                                    CATEG == "Good" 
                                    & VALID_CATEG == "Moderate" 
                                    & (MEAN_ATTEMPTS < 1.6) ~ "Good adherence, moderate performance and too few daily attempts",
                                    
                                    # Overall good but moderate
                                    CATEG == "Good" 
                                    & VALID_CATEG == "Moderate" 
                                    & (MEAN_ATTEMPTS >= 1.6 & MEAN_ATTEMPTS <= 2) ~ "Overall good adherence but moderate performance, getting there!",
                                    
                                    # Good adherence, low performance
                                    CATEG == "Good" 
                                    & VALID_CATEG == "Low" 
                                    & (MEAN_ATTEMPTS >= 2) ~ "Good adherence but low performance and too many daily attempts",
                                    
                                    CATEG == "Good" 
                                    & VALID_CATEG == "Low" 
                                    & (MEAN_ATTEMPTS < 1.6) ~ "Good adherence but low performance and too few daily attempts",
                                    
                                    # Overall good but low
                                    CATEG == "Good" 
                                    & VALID_CATEG == "Low" 
                                    & (MEAN_ATTEMPTS >= 1.6 & MEAN_ATTEMPTS <= 2) ~ "Overall good adherence but low performance, let's discuss?",
                                    
                                    
                                    
                                    # Moderate but too few or too many
                                    CATEG == "Moderate" 
                                    & VALID_CATEG == "Moderate" 
                                    & (MEAN_ATTEMPTS >= 2) ~ "Moderate adherence and performance, too many daily attempts",
                                    
                                    CATEG == "Moderate" 
                                    & VALID_CATEG == "Moderate" 
                                    & (MEAN_ATTEMPTS < 1.6) ~ "Moderate adherence and performance, too few daily attempts",
                                    
                                    CATEG == "Moderate" 
                                    & VALID_CATEG == "Moderate" 
                                    & (MEAN_ATTEMPTS >= 1.6 & MEAN_ATTEMPTS <= 2) ~ "Overall moderate adherence and performance, let's discuss?",
                                    
                                    # Moderate adherence, good performance
                                    CATEG == "Moderate" 
                                    & VALID_CATEG == "Good" 
                                    & (MEAN_ATTEMPTS >= 2) ~ "Moderate adherence, good performance but too many daily attempts",
                                    
                                    CATEG == "Moderate" 
                                    & VALID_CATEG == "Good" 
                                    & (MEAN_ATTEMPTS < 1.6) ~ "Moderate adherence, good performance but too few daily attempts",
                                    
                                    CATEG == "Moderate" 
                                    & VALID_CATEG == "Good" 
                                    & (MEAN_ATTEMPTS >= 1.6 & MEAN_ATTEMPTS <= 2) ~ "Overall moderate adherence but good performance, let's discuss?",
                                    
                                    # Moderate and low
                                    CATEG == "Moderate" 
                                    & VALID_CATEG == "Low" 
                                    & (MEAN_ATTEMPTS >= 2) ~ "Moderate adherence, low performance and too many daily attempts",
                                    
                                    CATEG == "Moderate" 
                                    & VALID_CATEG == "Low" 
                                    & (MEAN_ATTEMPTS < 1.6) ~ "Moderate adherence, low performance and too few daily attempts",
                                    
                                    CATEG == "Moderate" 
                                    & VALID_CATEG == "Low" 
                                    & (MEAN_ATTEMPTS >= 1.6 & MEAN_ATTEMPTS <= 2) ~ "Overall moderate adherence and low performance, let's discuss?",
                                    
                                    
                                    
                                    # Poor but too few or too many
                                    CATEG == "Low" 
                                    & VALID_CATEG == "Low" 
                                    & (MEAN_ATTEMPTS >= 2) ~ "Poor adherence and performance, too many daily attempts",
                                    
                                    CATEG == "Low" 
                                    & VALID_CATEG == "Low" 
                                    & (MEAN_ATTEMPTS < 1.6) ~ "Poor adherence and performance, too few daily attempts",
                                    
                                    CATEG == "Low" 
                                    & VALID_CATEG == "Low" 
                                    & (MEAN_ATTEMPTS >= 1.6 & MEAN_ATTEMPTS <= 2) ~ "Overall poor adherence and performance, let's discuss?",
                                    
                                    # Moderate adherence, good performance
                                    CATEG == "Low" 
                                    & VALID_CATEG == "Good" 
                                    & (MEAN_ATTEMPTS >= 2) ~ "Poor adherence but good performance, too many daily attempts",
                                    
                                    CATEG == "Low" 
                                    & VALID_CATEG == "Good" 
                                    & (MEAN_ATTEMPTS < 1.6) ~ "Poor adherence but good performance, too few daily attempts",
                                    
                                    CATEG == "Low" 
                                    & VALID_CATEG == "Good" 
                                    & (MEAN_ATTEMPTS >= 1.6 & MEAN_ATTEMPTS <= 2) ~ "Overall poor adherence but good performance, let's discuss?",
                                    
                                    # Moderate and low
                                    CATEG == "Low" 
                                    & VALID_CATEG == "Moderate" 
                                    & (MEAN_ATTEMPTS >= 2) ~ "Poor adherence, moderate performance and too many daily attempts",
                                    
                                    CATEG == "Low" 
                                    & VALID_CATEG == "Moderate" 
                                    & (MEAN_ATTEMPTS < 1.6) ~ "Poor adherence, moderate performance and too few daily attempts",
                                    
                                    CATEG == "Low" 
                                    & VALID_CATEG == "Moderate" 
                                    & (MEAN_ATTEMPTS >= 1.6 & MEAN_ATTEMPTS <= 2) ~ "Overall poor adherence and moderate performance, let's discuss?",
                                    
                                    
                                    TRUE ~ "Test"))
    })
    
    ####################################
    #             UPLOAD               #
    ####################################
    
    # Observed changes to upload prompts dropdown list
    observe({
        updateSelectInput(
            session, "patient",
            choices = unique(inputData()$PATIENTNUMBER))
      })
    
    # Subsets dataset by user choice
    df_subset <- reactive({
        if(input$patient == ""){
            a <- inputData()
        } else{
        a <- subset(inputData(), inputData()$PATIENTNUMBER == input$patient)
        }
        return(a)
    })
    
    ####################################
    #            ADHERENCE             #
    ####################################
    
    # Subsetting dashboard data by patient selection
    sumData <- reactive({
      dashData() %>%
        filter(PATIENTNUMBER %in% input$patient)
    })
    
    # Overall adherence
    output$overall <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(sumData()$OVERALL, "%"), style = "font-size: 100%;height:60px;"), HTML("<b><h4>Overall Adherence</h4></b>"), 
          color = ifelse(sumData()$OVERALL >= 80, "green", 
                         ifelse(sumData()$OVERALL < 80 & sumData()$OVERALL > 50, "orange",
                                ifelse(sumData()$OVERALL <= 50, "red")))
        )
      })
    
    # Daily adherence
    output$daily <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(sumData()$DAILY, "%"), style = "font-size: 100%;height:60px;"), HTML("<b><h4>Daily Adherence</h4></b>"),
          color = ifelse(sumData()$DAILY >= 80, "green", 
                         ifelse(sumData()$DAILY < 80 & sumData()$DAILY > 50, "orange",
                                ifelse(sumData()$DAILY <= 50, "red")))
        )
      })
    
    output$weekly <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(sumData()$WEEKLY, "%"), style = "font-size:100%;height:60px;"), HTML("<b><h4>Weekly Adherence</h4></b>"),
          color = ifelse(sumData()$WEEKLY >= 70, "green", 
                         ifelse(sumData()$WEEKLY < 70 & sumData()$WEEKLY > 50, "orange",
                                ifelse(sumData()$WEEKLY <= 50, "red")))
        )
      })
    
    # Monthly adherence
    output$monthly <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(sumData()$MONTHLY, "%"), style = "font-size: 100%;height:60px;"), HTML("<b><h4>Monthly Adherence</h4></b>"),
          color = ifelse(sumData()$MONTHLY >= 70, "green", 
                         ifelse(sumData()$MONTHLY < 70 & sumData()$MONTHLY > 50, "orange",
                                ifelse(sumData()$MONTHLY <= 50, "red")))
        )
      })
    
    # Average daily attempts
    output$attempts <- 
      renderInfoBox({
        valueBox(
          sumData()$MEAN_ATTEMPTS, HTML("<b>Average Number of Daily Attempts At Nebulisation</b>"), color = "blue"
        )
      })
    
    # Average valid daily nebulisations
    output$valid <- 
      renderInfoBox({
        valueBox(
          paste0(sumData()$VALID_PCNT, "%"), HTML("<b>Proportion of Average Daily Attempts That Were Valid</b>"), 
          color = ifelse(sumData()$VALID_PCNT >= 80, "green", 
                         ifelse(sumData()$VALID_PCNT < 80 & sumData()$VALID_PCNT > 50, "orange",
                                ifelse(sumData()$VALID_PCNT <= 50, "red")))
          )
      })
    
    # Most common nebulisation error
    output$error <- 
      renderInfoBox({
        valueBox(
          tags$p(ifelse(sumData()$occur < 10, "None",
                        ifelse(sumData()$occur >= 10, paste0(sumData()$ERROR, " - ", sumData()$occur, "%"))), style = "font-size: 90%; height:60px;"), HTML("<b>Most Common eFlow Error and Proportion of Total Usage</b>"), 
          color = ifelse(sumData()$occur < 10, "green",
                         ifelse(sumData()$occur >= 10 & sumData()$occur < 50, "orange",
                                ifelse(sumData()$occur >= 50, "red")))
        )
      })
    
    # Most common cleaning error
    output$cleaning <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(sumData()$count), style = "font-size: 100%; height:60px;"), HTML("<b>Total Easycare Usage</b>"), 
          color = ifelse(sumData()$count >= 25 | sumData()$count == 0, "red", "blue")
        )
      })
    
    # Patient recommendation
    output$recommend <- 
      renderInfoBox({
        valueBox(
          tags$p(sumData()$FEEDBACK, style = "font-size: 60%; height:60px;"), HTML("<b>Feedback On Performance</b>"), 
          color = ifelse(grepl("oor", sumData()$FEEDBACK), "red",
                         ifelse(grepl("oderate", sumData()$FEEDBACK), "orange",
                                ifelse(grepl("ood", sumData()$FEEDBACK), "green", "blue"))))
      })
    
    ####################################
    #        NEBULISATION PLOTS        #
    ####################################
    
    # Cumulative adherence plot
    output$plot1 <- renderPlot({
      
      dailyCumul() %>%
        filter(PATIENTNUMBER %in% input$patient) %>%
        ggplot(aes(DAY, CUMUL)) +
        geom_line(size = 1.5) +
        labs(x = "Enrolled Days", y = "Cumulative Adherence Rate (%)") +
        scale_y_continuous(limits = c(0,100), n.breaks = 10) +
        scale_x_continuous(breaks = seq(0, max(dailyCumul()$DAY), by = 20)) +
        geom_hline(aes(yintercept = 80, colour = "Good"), linetype = "dashed", size = 2) +
        geom_hline(aes(yintercept = 50, colour = "Poor"), linetype = "dashed", size = 2) +
        theme_bw(base_size = 20) +
        scale_colour_manual(name = NULL, values = c("green", "red"))
    
    })
    
    # Daily nebulisations plot
    output$plot2 <- renderPlot({
      
      dailyAdh() %>%
        filter(PATIENTNUMBER %in% input$patient) %>%
        ggplot(aes(DAY, NEBS)) +
        geom_area(fill = "skyblue", colour = "black") +
        labs(x = "Enrolled days", y = "Daily nebulisations performed") +
        scale_x_continuous(breaks = seq(0, max(dailyAdh()$DAY), by = 20)) +
        scale_y_continuous(breaks = seq(0, max(dailyAdh()$NEBS))) +
        geom_hline(aes(yintercept = 2, colour = "Target"), linetype = "dashed", size = 2) +
        theme_bw(base_size = 20) +
        scale_colour_manual(name = NULL, values = c("green"))
    })


    
    ####################################
    #           CALCULATORS            #
    ####################################
    
    # BMI
    output$bmi <- 
      renderInfoBox({
        # If button has been pressed, generate the value
        if(input$bmiCalc > 0)
          {valueBox(paste0(round(input$weight / ((input$height/100)*(input$height/100)), 2)), HTML("<b>Body Mass Index</b>"))} 
        else(valueBox("0", HTML("<b>Body Mass Index</b>")))
        })
    
}
    


# Execute code
shinyApp(ui, server)




