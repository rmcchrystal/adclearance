
###########################################################################
###########################################################################
###                                                                     ###
###                    ADCLEARANCE | WORKING VERSION 1                  ###
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
              tags$style(".main-header .logo {height: 70px}")
              ),
        
        # Adding logo to header
        title = tags$img(src="AdCLEARance.png", 
                       height = '70', width = '200')
        ),
    
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
            menuItem("Save", tabName = "save", icon = icon("save")),
            menuItem("Calculate", tabName = "calculate", icon = icon("calculator"))
            
        )
        ),
    
  ####################################
  #              BODY                #
  ####################################
  
    # Content of dashboard
    dashboardBody(
      
      # Extend page and add scrollbar
      tags$head(tags$style(HTML('.content-wrapper { height: 1750px !important; }'))),
        
        tabItems(
            
            tabItem(
              # Dashboard page
              tabName = "main",
              # Header
              h2(HTML("<b>Adherence Dashboard</b>")),
              
              # Row 1 - Upload data and select patient
              fluidRow(
                  
                  # Upload data
                  box(
                    title = HTML("<b>Upload PARI eFlow Data</b>"), solidHeader = TRUE,
                    fileInput("upload", "Ensure the spreadsheet is saved in Excel (.xlsx) format"),
                    height = 140
                  ),
                  
                  # Choose a patient
                  box(
                    title = HTML("<b>Select A Patient</b>"), solidHeader = TRUE,
                    HTML("<b>Choose a Patient ID from the dropdown list</b>"),
                    selectInput("patient", "", choices = c("")),
                    height = 140
                    )
                  ),
              
              # Row 2 - Summary information on adherence
              fluidRow(
                # Overall
                valueBoxOutput("overall", width = 3),
                # Daily
                valueBoxOutput("daily", width = 3),
                # Weekly
                valueBoxOutput("weekly", width = 3),
                # Monthly
                valueBoxOutput("monthly", width = 3)
                ),
              
              # Row 3 - Summary information on nebulisations
              fluidRow(
                # Mean number of daily attempts
                valueBoxOutput("attempts", width = 3),
                # Mean number of valid daily nebulisations
                valueBoxOutput("valid", width = 3),
                # Most common nebulisation error
                valueBoxOutput("HTSerror", width = 3),
                # Most common cleaning error
                valueBoxOutput("cleanerror", width = 3)
                ),
              
              # Row 4 - Category and recommendation
              fluidRow(
                # Adherence categorisation
                valueBoxOutput("category", width = 6),
                # Recommendation for intervention
                valueBoxOutput("recommend", width = 6)
              ),
              
              # Row 5 - Visualisations of adherence
              fluidRow(
                # Nebulisation plots
                tabBox(
                  title = HTML("<b>Adherence To HTS Nebulisations</b>"),
                  id = "plottab1",
                  height = "400px",
                  width = 12,
                  tabPanel("Daily", plotOutput("plot1", height = 500)),
                  tabPanel("Weekly", plotOutput("plot2", height = 500)),
                  tabPanel("Monthly", plotOutput("plot3", height = 500))
                  ),
                div(style = 'padding:290px;'),
                ),
              
              # Row 6 - Visualisation of cumulative adherence
              fluidRow(
                # Cumulative adherence
                tabBox(
                  title = HTML("<b>Cumulative Adherence Rate</b>"),
                  id = "plottab2",
                  height = "400px",
                  width = 12,
                  tabPanel("Daily", plotOutput("plot4", height = 500)),
                  tabPanel("Weekly", plotOutput("plot5", height = 500)),
                  tabPanel("Monthly", plotOutput("plot6", height = 500))
                )
                )
              ),
            
            # Save
            tabItem(
                tabName = "save",
                h2(HTML("<b>Save Adherence Data/Plots</b>"))
                ),
            
            # Calculate
            tabItem(
                tabName = "calculate",
                h2(HTML("<b>Calculate Patient Adherence</b>")),
                box(
                  title = HTML("<b>Overall Adherence</b>"), solidHeader = TRUE,
                  numericInput("adherent", "Number of Days Adherent To HTS Nebulisations", min = 0, max = 1000, value = 0),
                  numericInput("enrolled", "Number of Days Enrolled To Study", min = 0, max = 1000, value = 0),
                  textOutput("calculatoroutput1")
                  ),
                box(
                  title = HTML("<b>Cumulative Adherence</b>"), solidHeader = TRUE,
                  numericInput("adherent2", "Total Daily Adherence To HTS Nebulisations", min = 0, max = 1000, value = 0),
                  numericInput("enrolled2", "Number of Days Enrolled To Study", min = 0, max = 1000, value = 0),
                  textOutput("calculatoroutput2")
                )
                )
            )
        )
    )


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
        filter(INHAL_INTERRUPTION %in% c(NA, 4)) %>%
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
    HTSerrors <- reactive({
      
      dat <- inputData()
      
      # Subset
      subset_HTSerrors <- 
        dat %>%
        filter(INHAL_INTERRUPTION %in% c(1:3, 5:8) & !is.na(VALID_INHAL))
      
      # Adding index to code errors
      errors_HTS <- 
        subset_HTSerrors %>%
        mutate(ERROR = case_when(INHAL_INTERRUPTION == 1 ~ "Lost supply voltage",
                                 INHAL_INTERRUPTION == 2 ~ "Aerosol head disconnected",
                                 INHAL_INTERRUPTION == 3 ~ "Medication missing",
                                 INHAL_INTERRUPTION == 5 ~ "Manual shutdown",
                                 INHAL_INTERRUPTION == 6 ~ "Battery empty",
                                 INHAL_INTERRUPTION == 7 ~ "Timeout",
                                 INHAL_INTERRUPTION == 8 ~ "Timeout, Paused"))
      
    })
    
    # Cleaning errors
    cleanerrors <- reactive({
      
      dat <- inputData()
      
      # Subset
      subset_cleanerrors <- 
        dat %>%
        filter(INHAL_INTERRUPTION %in% c(101:103, 105:107))
      
      # Adding index for code errors
      errors_clean <- 
        subset_cleanerrors %>%
        mutate(CLEAN_ERROR = case_when(INHAL_INTERRUPTION == 101 ~ "Lost supply voltage",
                                       INHAL_INTERRUPTION == 102 ~ "Aerosol head disconnected",
                                       INHAL_INTERRUPTION == 103 ~ "Medication missing",
                                       INHAL_INTERRUPTION == 105 ~ "Manual shutdown",
                                       INHAL_INTERRUPTION == 106 ~ "Battery empty",
                                       INHAL_INTERRUPTION == 107 ~ "Timeout"))
      
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
      
      # Most common HTS error
      dt_HTS_error <- HTSerrors() %>% group_by(PATIENTNUMBER) %>% count(ERROR) %>% slice(which.max(n)) %>% ungroup() %>% select(-c(n))
      
      # Most common cleaning error
      dt_clean_error <- cleanerrors() %>% group_by(PATIENTNUMBER) %>% count(CLEAN_ERROR) %>% slice(which.max(n)) %>% ungroup() %>% select(-c(n))
      
      # Overall adherence rate
      dt_HTS_overall <- 
        data.frame(PATIENTNUMBER = dt_HTS_adherent$PATIENTNUMBER,
                   DAILY = dt_HTS_adherent$DAILY,
                   WEEKLY = dt_HTS_adherentWk$WEEKLY,
                   MONTHLY = dt_HTS_adherentM$MONTHLY) %>%
        mutate(OVERALL = round((DAILY + WEEKLY + MONTHLY)/3,1),
               CATEG = case_when(OVERALL >= 80 ~ "Good",
                                 OVERALL < 80 & OVERALL >= 50 ~ "Moderate",
                                 OVERALL < 50 ~ "Low")) %>%
        merge(., dt_HTS_attempts) %>%
        merge(., dt_HTS_valid) %>%
        mutate(VALID_PCNT = round((MEAN_VALID/MEAN_ATTEMPTS)*100,2)) %>%
        left_join(., dt_HTS_error) %>%
        left_join(., dt_clean_error) %>%
        mutate(ERROR = ifelse(is.na(ERROR), "None", ERROR),
               CLEAN_ERROR = ifelse(is.na(CLEAN_ERROR), "None", CLEAN_ERROR))
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
    
    sumData <- reactive({
      dashData() %>%
        filter(PATIENTNUMBER %in% input$patient)
    })
    
    output$overall <- 
      renderInfoBox({
        valueBox(
          paste0(sumData()$OVERALL, "%"), HTML("<b>Overall Adherence</b>"), color = "green"
        )
      })
    
    output$daily <- 
      renderInfoBox({
        valueBox(
          paste0(sumData()$DAILY, "%"), HTML("<b>Daily Adherence</b>"), color = "blue"
        )
      })
    
    output$weekly <- 
      renderInfoBox({
        valueBox(
          paste0(sumData()$WEEKLY, "%"), HTML("<b>Weekly Adherence</b>"), color = "yellow"
        )
      })
    
    output$monthly <- 
      renderInfoBox({
        valueBox(
          paste0(sumData()$MONTHLY, "%"), HTML("<b>Monthly Adherence</b>"), color = "red"
        )
      })
    
    output$attempts <- 
      renderInfoBox({
        valueBox(
          sumData()$MEAN_ATTEMPTS, HTML("<b>Mean Attempts At Daily Nebulisations</b>"), color = "green"
        )
      })
    
    output$valid <- 
      renderInfoBox({
        valueBox(
          sumData()$MEAN_VALID, HTML("<b>Mean Number of Valid Daily Nebulisation</b>"), color = "blue"
        )
      })
    
    output$HTSerror <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(sumData()$ERROR), style = "font-size: 75%; height:40px;"), HTML("<b>Most Common Nebulisation Error</b>"), color = "yellow"
        )
      })
    
    output$cleanerror <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(sumData()$CLEAN_ERROR), style = "font-size: 75%; height:40px;"), HTML("<b>Most Common Cleaning Error</b>"), color = "red"
        )
      })
    
    ####################################
    #        NEBULISATION PLOTS        #
    ####################################
    
    # Daily adherence plot
    output$plot1 <- renderPlot({
      
      dailyAdh() %>%
        filter(PATIENTNUMBER %in% input$patient) %>%
        ggplot(aes(DAY, NEBS)) +
        geom_area(fill = "skyblue", colour = "black") +
        labs(x = "Enrolled days", y = "Daily nebulisations performed") +
        scale_x_continuous(breaks = seq(0, max(dailyAdh()$DAY), by = 20)) +
        scale_y_continuous(breaks = seq(0, max(dailyAdh()$NEBS))) +
        geom_hline(aes(yintercept = 2), colour = "red")
    })
    
    # Daily adherence plot
    output$plot2 <- renderPlot({
      
      weeklyAdh() %>%
        filter(PATIENTNUMBER %in% input$patient) %>%
        ggplot(aes(DATE, WEEKLY)) +
        geom_area(fill = "skyblue", colour = "black") +
        labs(x = "Date", y = "Weekly adherence (%)") +
        scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
        scale_x_date(date_breaks = "2 weeks") +
        geom_hline(aes(yintercept = 80), colour = "red")
    })
    
    # Daily adherence plot
    output$plot3 <- renderPlot({
      
      monthlyAdh() %>%
        filter(PATIENTNUMBER %in% input$patient) %>%
        ggplot(aes(DATE, MONTH_ADH)) +
        geom_area(fill = "skyblue", colour = "black") +
        labs(x = "Enrolled months", y = "Monthly adherence (%)") +
        scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
        scale_x_date(date_breaks = "1 month") +
        geom_hline(aes(yintercept = 80), colour = "red")
    })
    
    ####################################
    #        CUMULATIVE PLOTS          #
    ####################################
    
    # Daily cumulative adherence plot
    output$plot4 <- renderPlot({
      
      dailyCumul() %>%
        filter(PATIENTNUMBER %in% input$patient) %>%
        ggplot(aes(DAY, CUMUL)) +
        geom_line() +
        labs(x = "Enrolled Days", y = "Cumulative Adherence Rate (%)") +
        scale_y_continuous(limits = c(0,100)) +
        scale_x_continuous(breaks = seq(0, max(dailyCumul()$DAY), by = 10)) +
        geom_hline(aes(yintercept = 80), colour = "red")
      
    })
    
    # Weekly cumulative adherence plot
    output$plot5 <- renderPlot({
      
      weeklyAdh() %>%
        filter(PATIENTNUMBER %in% input$patient) %>%
        ggplot(aes(WEEKS, WEEK_CUMUL)) +
        geom_line() +
        labs(x = "Enrolled Weeks", y = "Weekly Adherence Rate (%)") +
        scale_y_continuous(limits = c(0,100)) +
        scale_x_continuous(breaks = seq(0, max(weeklyAdh()$WEEKS), by = 10)) +
        geom_hline(aes(yintercept = 80), colour = "red")
      
    })
    
    # Monthly cumulative adherence plot
    output$plot6 <- renderPlot({
      
      monthlyAdh() %>%
        filter(PATIENTNUMBER %in% input$patient) %>%
        ggplot(aes(MONTHS, MONTH_CUMUL)) +
        geom_line() +
        labs(x = "Enrolled Months", y = "Monthly Adherence Rate (%)") +
        scale_y_continuous(limits = c(0,100)) +
        scale_x_continuous(breaks = seq(0, max(monthlyAdh()$MONTHS), by = 10)) +
        geom_hline(aes(yintercept = 80), colour = "red")
      
    })
    
    ####################################
    #           CALCULATORS            #
    ####################################
    
    # Calculator output
    output$calculatoroutput1 <- renderText(input$adherent/input$enrolled*100)
    }


# Execute code
shinyApp(ui, server)
