

###########################################################################
###########################################################################
###                                                                     ###
###                     ADCLEARANCE | PUBLIC VERSION                    ###
###                      RYAN MCCHRYSTAL | 40205088                     ###
###                                                                     ###
###########################################################################
###########################################################################


# Required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(lubridate)
library(hms)


##================================================================
##                         USER INTERFACE                        =
##================================================================

ui <- 
  
  dashboardPage(
    
    # Header
    dashboardHeader(
      
      # Set height
      tags$li(class = "dropdown",
              tags$style(".main-header {max-height: 100px}"),
              tags$style(".main-header .logo {height: 100px}")),
      
      # Adding logo to header
      title = HTML("<h3><b>Medication Dashboard</b></h3>"),
      
      titleWidth = 280
      
      ),
    
    dashboardSidebar(
      disable = TRUE,
      sidebarMenu()
    ),
    
    # Dashboard interface
    dashboardBody(
      
      # Extends page height and adds scroll bar
      tags$head(tags$style(HTML('.content-wrapper { height: 1800px !important; }'))),
      
      # Row 1 - Upload data and select patient
      fluidRow(
        
        # Upload data
        box(title = HTML("<b>Upload eFlow Data Export</b>"),
            fileInput("upload", "File must be in Excel worksheet (.xlsx) format"),
            height = 140),
        
        # Patient selection
        box(title = HTML("<b>Select a Patient</b>"),
            HTML("<b>Choose a Patient ID from the dropdown list</b>"),
            selectInput("patient", "", choices = c("")),
            height = 140)
        
      ),
      
      # Row 2 - Summary information on adherence and nebuliser usage
      fluidRow(
        
        # Column 1 - Adherence summary
        column(width = 6,
               box(title = HTML("<b> Adherence To Medication </b>"),
                   # Overall adherence
                   valueBoxOutput("overall"),
                   # Average number of daily nebulisations performed
                   valueBoxOutput("average"),
                   # Percentage of enrolled nebuliser days that were missed
                   valueBoxOutput("missed"),
                   # Daily adherence
                   valueBoxOutput("daily"),
                   # Weekly adherence
                   valueBoxOutput("weekly"),
                   # Monthly adherence
                   valueBoxOutput("monthly"),
                   width = 13,
                   height = 350)),
        
        # Column 2 - Errors and recommendation
        column(width = 6,
               box(title = HTML("<b> Medication Device Usage & Feedback </b>"),
                   # Total times easycare was used
                   valueBoxOutput("cleaning", width = 3),
                   # Most common error
                   valueBoxOutput("error", width = 9),
                   # Recommendation for intervention
                   valueBoxOutput("recommend", width = 12),
                   width = 13,
                   height = 350))
        
      ),
      
      
      # Row 3 - Visualisation of cumulative adherence rate
      fluidRow(
        
        # Cumulative adherence
        box(title = HTML("<b>Changes In Daily Medication Intake</b>"),
            id = "plottab1",
            height = "600px",
            width = 12,
            plotOutput("plot1", height = 500))
        
      ),
      
      # Row 4 - Visualisations of daily nebulisations
      fluidRow(
        
        # Nebulisation plots
        box(title = HTML("<b>Changes In Daily Medication Adherence</b>"),
            id = "plottab2",
            height = "600px",
            width = 12,
            plotOutput("plot2", height = 500))
        
      )
    )
    )


##================================================================
##                           BACK-END                            =
##================================================================

server <- 
  
  function(input, output, session){
    
    ####################################
    #             DATASETS             #
    ####################################
    
    # Grabs uploaded data as a reactive object
    inputGrab <- reactive({req(input$upload)})
    
    # Tidied data
    tidiedData <- reactive({
      
      # Reads uploaded data into R
      dat <- read_excel(inputGrab()$datapath)
      
      # Removing NA and test entries
      # Extracting datetime
      # Filtering to trial dates (from 27th June 2018 onward)
      # Adding missing dates for individual patient history
      # Arranging by ID, date and time for easier reading
      # Indexing entries for subsets
      data <- 
        dat %>% 
        filter(!is.na(PATIENTNUMBER) & grepl("R", PATIENTNUMBER)) %>%
        mutate(
          DATE = strtrim(INHAL_ID, 8),
          TIME = substr(INHAL_ID, 9, 14),
          DATE = as_date(ymd(DATE)),
          TIME = gsub('(..)(?=.)', '\\1:', TIME, perl=TRUE),
          TIME = as_hms(TIME)) %>%
        filter(!is.na(DATE) & DATE > "2018-06-27") %>%
        select(-c(INHAL_ID, SERIALNO)) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(WEEK = week(DATE),
               MONTH = month(DATE),
               YEAR = year(DATE)) %>% ungroup() %>%
        select(PATIENTNUMBER, YEAR, WEEK, MONTH, DATE, TIME, everything(.)) %>%
        arrange(PATIENTNUMBER, DATE, TIME) %>%
        mutate(TYPE = case_when(INHAL_INTERRUPTION == 4 ~ "Nebulisation",
                                
                                INHAL_INTERRUPTION == 104
                                & VALID_INHAL != 0 ~ "Easycare Neb",
                                
                                INHAL_INTERRUPTION == 104
                                & VALID_INHAL == 0 ~ "Easycare",
                                
                                INHAL_INTERRUPTION %in% c(1:3, 5:8) ~ "Errors",
                                
                                INHAL_INTERRUPTION %in% c(101:103, 105:107) ~ "Easycare Error"))
    })
    
    
    # Adherence subset
    adherence <- reactive({
      
      dat <- tidiedData()
      
      subset_HTS <- 
        dat %>%
        filter(TYPE %in% c("Nebulisation", "Easycare Neb")) %>%
        group_by(PATIENTNUMBER) %>%
        complete(DATE = seq.Date(min(DATE), max(DATE), "days")) %>%
        mutate(TYPE = ifelse(is.na(VALID_INHAL), "Missed Day", TYPE)) %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(TIMEDIFF = round((unclass(TIME) - unclass(lag(TIME)))/3600,1),
               ADHERENT = case_when(n() == 2
                                    & INHAL_PERIOD >= 40
                                    & TIMEDIFF >= 1
                                    & VALID_INHAL %in% c(1,2) ~ 1,
                                    TRUE ~ 0)) %>% ungroup()
    })
    
    
    # Cleaning data
    # Counting usage for each patient
    cleaning <- reactive({
      
      dat <- tidiedData()
      
      subset_clean <- 
        dat %>%
        filter(TYPE == "Easycare") %>%
        group_by(PATIENTNUMBER) %>%
        summarise(EASYCARE = n()) %>% ungroup()
    })
    
    
    # Error data
    errors <- reactive({
      
      dat <- tidiedData()
      
      subset_errors <- 
        dat %>%
        filter(TYPE %in% c("Errors")) %>%
        mutate(CODE = case_when(INHAL_INTERRUPTION == 1 ~ "Lost supply voltage",
                                INHAL_INTERRUPTION == 2 ~ "Aerosol head disconnected",
                                INHAL_INTERRUPTION == 3 ~ "Medication missing",
                                INHAL_INTERRUPTION == 5 ~ "Manual shutdown",
                                INHAL_INTERRUPTION == 6 ~ "Battery empty",
                                INHAL_INTERRUPTION == 7 ~ "Timeout",
                                INHAL_INTERRUPTION == 8 ~ "Paused Timeout")) %>%
        group_by(PATIENTNUMBER) %>% 
        count(CODE, name = "OCCURRENCE") %>% 
        slice(which.max(OCCURRENCE)) %>% ungroup()
    })
    
    
    # Mean daily nebulisations data
    adherenceMean <- reactive({
      
      dat <- adherence()
      
      adherence_dailyMean <- 
        dat %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(DAILY_N = n()) %>%
        group_by(PATIENTNUMBER) %>%
        summarise(DAILY_MEAN = round(mean(DAILY_N, na.rm = T),2)) %>% ungroup()
      
    })
    
    # Cumulative adherence
    adherenceCumulative <- reactive({
      
      dat <- adherence()
      
      adherenceCumul <- 
        dat %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(ADHERENT = ifelse(TYPE == "Missed Day", NA_real_, ADHERENT)) %>%
        filter(ADHERENT %in% c(1, NA_real_)) %>%
        mutate(ADHERENT = ifelse(is.na(ADHERENT), 0, ADHERENT)) %>%
        group_by(PATIENTNUMBER) %>%
        complete(DATE = seq.Date(min(DATE), max(DATE), "days")) %>%
        mutate(ADHERENT = ifelse(is.na(ADHERENT), 0, ADHERENT),
               DAY = row_number(),
               CUMUL = cumsum(ADHERENT)/DAY * 100)
    })
    
    
    # Number of daily nebulisations performed
    adherenceDaily <- reactive({
      
      dat <- adherence()
      
      adherence_daily <- 
        dat %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(DAILY_N = n()) %>%
        filter(!duplicated(DAILY_N)) %>%
        mutate(DAILY_N = ifelse(is.na(VALID_INHAL), 0, DAILY_N)) %>%
        select(PATIENTNUMBER, DATE, DAILY_N) %>% ungroup()
    })
    
    
    # Weekly adherence
    adherenceWeekly <- reactive({
      
      dat <- adherence()
      
      adherence_weekly <- 
        dat %>%
        group_by(PATIENTNUMBER, WEEK, YEAR) %>%
        mutate(WEEK_N = sum(ADHERENT)) %>%
        filter(!duplicated(DATE)) %>%
        mutate(WEEK_ADH = ifelse(WEEK_N/length(WEEK) > 0.7, 1, 0)) %>%
        group_by(PATIENTNUMBER, YEAR) %>%
        filter(!duplicated(WEEK)) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(WEEKS = row_number(),
               WEEKLY = round(sum(WEEK_ADH)/length(WEEKS)*100,2)) %>%
        filter(!duplicated(WEEKLY)) %>%
        select(PATIENTNUMBER, WEEKLY) %>% ungroup()
    })
    
    
    # Monthly adherence
    adherenceMonthly <- reactive({
      
      dat <- adherence()
      
      adherence_monthly <- 
        dat %>%
        group_by(PATIENTNUMBER, WEEK, YEAR) %>%
        mutate(WEEK_N = sum(ADHERENT)) %>%
        filter(!duplicated(DATE)) %>%
        mutate(WEEK_ADH = ifelse(WEEK_N/length(WEEK) > 0.7, 1, 0)) %>%
        group_by(PATIENTNUMBER, YEAR) %>%
        filter(!duplicated(WEEK)) %>%
        group_by(PATIENTNUMBER, MONTH) %>%
        mutate(MONTHLY = round(sum(WEEK_ADH)/length(MONTH)*100,2)) %>%
        filter(!duplicated(MONTHLY)) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(MONTHLY = round(mean(MONTHLY),2)) %>%
        filter(!duplicated(MONTHLY)) %>%
        select(PATIENTNUMBER, MONTHLY) %>% ungroup()
    })
    
    
    # Dashboard data
    dashboardData <- reactive({
      
      # Patient IDs
      dashboard_IDs <- unique(tidiedData()$PATIENTNUMBER)
      
      # Total enrolled days
      dashboard_overall <- 
        adherence() %>%
        group_by(PATIENTNUMBER) %>%
        filter(!duplicated(DATE)) %>%
        mutate(ENROLLED_DAYS = length(PATIENTNUMBER)) %>%
        filter(!duplicated(ENROLLED_DAYS)) %>%
        select(PATIENTNUMBER, ENROLLED_DAYS) %>% ungroup()
      
      # Total adherent days
      dashboard_days <- 
        adherence() %>%
        group_by(PATIENTNUMBER) %>%
        filter(ADHERENT == 1) %>%
        mutate(ADHERENT_DAYS = n()) %>%
        filter(!duplicated(ADHERENT_DAYS)) %>%
        select(PATIENTNUMBER, ADHERENT_DAYS) %>% ungroup()
      
      # Missed days
      dashboard_missed <- 
        adherence() %>%
        group_by(PATIENTNUMBER) %>%
        filter(TYPE == "Missed Day") %>%
        count(name = "MISSED_DAYS") %>% ungroup()
      
      # All statistics for patients
      dashboard_data <- 
        data.frame(PATIENTNUMBER = dashboard_IDs) %>%
        full_join(., dashboard_overall) %>%
        full_join(., dashboard_missed) %>%
        full_join(., dashboard_days) %>%
        mutate(PCNT_MISSED = round(MISSED_DAYS/ENROLLED_DAYS *100,2),
               DAILY_ADH = round(ADHERENT_DAYS/ENROLLED_DAYS*100,2)) %>%
        full_join(., adherenceWeekly()) %>%
        full_join(., adherenceMonthly()) %>%
        full_join(., adherenceMean()) %>%
        full_join(., cleaning()) %>%
        full_join(., errors()) %>%
        replace(is.na(.), 0) %>%
        mutate(OVERALL = round((DAILY_ADH + WEEKLY + MONTHLY)/3,2)) %>%
        filter(ENROLLED_DAYS > 365 & OVERALL > 10) %>%
        mutate(FEEDBACK = case_when(OVERALL >= 80
                                    & PCNT_MISSED <= 10
                                    & OCCURRENCE <= 50
                                    ~ "Overall, your adherence and performance are great - keep it up!",
                                    
                                    OVERALL >= 80
                                    & PCNT_MISSED <= 10
                                    & OCCURRENCE > 50
                                    ~ "Your adherence is great, but let's work on your performance!",
                                    
                                    OVERALL < 80 & OVERALL >= 50
                                    & PCNT_MISSED < 10
                                    & OCCURRENCE <= 50
                                    ~ "Good performance but adherence could be better, let's discuss?",
                                    
                                    OVERALL < 80 & OVERALL >= 50
                                    & OCCURRENCE >= 25
                                    ~ "Let's figure out how we can improve your adherence/performance!",
                                    
                                    OVERALL < 50
                                    & PCNT_MISSED > 10
                                    & OCCURRENCE <= 50
                                    ~ "We need to work on your adherence, let's discuss?",
                                    
                                    OVERALL < 50
                                    & OCCURRENCE > 50
                                    ~ "We need to work on your adherence/performance, let's discuss?",
                                    TRUE ~ "Test"))
          
    })
    
    
    ####################################
    #       PATIENT DROPDOWN LIST      #
    ####################################
    
    # Observe data upload and set listing as patient numbers
    observe({
      updateSelectInput(
        session, "patient",
        choices = unique(dashboardData()$PATIENTNUMBER))
    })
    
    # Subsets dataset by user choice
    df_subset <- reactive({
      if(input$patient == ""){
        a <- dashboardData()
      } else{
        a <- subset(dashboardData(), dashboardData()$PATIENTNUMBER == input$patient)
      }
      return(a)
    })
    
    
    # Subset dashboard data by patient selection
    subsetData <- reactive({
      dashboardData() %>%
        filter(PATIENTNUMBER %in% input$patient)
    })
    
    
    ####################################
    #        SUMMARY STATISTICS        #
    ####################################
    
    # Overall adherence
    output$overall <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(subsetData()$OVERALL, "%"), style = "font-size: 100%;height:60px;"), HTML("<b><h4>Overall Adherence</h4></b>"), 
          color = ifelse(subsetData()$OVERALL >= 80, "green", 
                         ifelse(subsetData()$OVERALL < 80 & subsetData()$OVERALL > 50, "orange",
                                ifelse(subsetData()$OVERALL <= 50, "red")))
        )
      })
    
    # Daily adherence
    output$daily <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(subsetData()$DAILY_ADH, "%"), style = "font-size: 100%;height:60px;"), HTML("<b><h4>Daily Adherence</h4></b>"),
          color = ifelse(subsetData()$DAILY_ADH >= 80, "green", 
                         ifelse(subsetData()$DAILY_ADH < 80 & subsetData()$DAILY_ADH > 50, "orange",
                                ifelse(subsetData()$DAILY_ADH <= 50, "red")))
        )
      })
    
    # Weekly adherence
    output$weekly <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(subsetData()$WEEKLY, "%"), style = "font-size:100%;height:60px;"), HTML("<b><h4>Weekly Adherence</h4></b>"),
          color = ifelse(subsetData()$WEEKLY >= 80, "green", 
                         ifelse(subsetData()$WEEKLY < 80 & subsetData()$WEEKLY > 50, "orange",
                                ifelse(subsetData()$WEEKLY <= 50, "red")))
        )
      })
    
    # Monthly adherence
    output$monthly <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(subsetData()$MONTHLY, "%"), style = "font-size: 100%;height:60px;"), HTML("<b><h4>Monthly Adherence</h4></b>"),
          color = ifelse(subsetData()$MONTHLY >= 80, "green", 
                         ifelse(subsetData()$MONTHLY < 80 & subsetData()$MONTHLY > 50, "orange",
                                ifelse(subsetData()$MONTHLY <= 50, "red")))
        )
      })
    
    # Average number of daily nebulisations
    output$average <- 
      renderInfoBox({
        valueBox(
          subsetData()$DAILY_MEAN, HTML("<b><h4>Average Number of Medications Taken Daily</b></h4>"), color = "blue"
        )
      })
    
    # Percentage of enrolled days that were missed
    output$missed <- 
      renderInfoBox({
        valueBox(
          paste0(subsetData()$PCNT_MISSED, "%"), HTML("<b><h4>Proportion of Medication Days That Were Missed</h4></b>"), 
          color = ifelse(subsetData()$PCNT_MISSED >= 50, "red", 
                         ifelse(subsetData()$PCNT_MISSED < 50 & subsetData()$PCNT_MISSED > 10, "orange",
                                ifelse(subsetData()$PCNT_MISSED <= 10, "green")))
        )
      })
    
    # Most common nebulisation error
    output$error <- 
      renderInfoBox({
        valueBox(
          tags$p(ifelse(subsetData()$OCCURRENCE < 50, "None",
                        ifelse(subsetData()$OCCURRENCE >= 50, paste0(subsetData()$CODE, " - ", subsetData()$OCCURRENCE, " times"))), 
                 style = "font-size: 90%; height:60px;"), HTML("<b><h4>Most Frequent Error Occurring When Using Device<h4></b>"), 
          color = ifelse(subsetData()$OCCURRENCE < 50, "green",
                         ifelse(subsetData()$OCCURRENCE >= 50 & subsetData()$OCCURRENCE < 100, "orange",
                                ifelse(subsetData()$OCCURRENCE > 100, "red")))
        )
      })
    
    # Most common cleaning error
    output$cleaning <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(subsetData()$EASYCARE), style = "font-size: 100%; height:60px;"), HTML("<b><h4>Total Device Cleaning</h4></b>"), 
          color = ifelse(subsetData()$EASYCARE > 25 | subsetData()$EASYCARE == 0, "red", "blue")
        )
      })
    
    # Patient recommendation
    output$recommend <- 
      renderInfoBox({
        valueBox(
          tags$p(subsetData()$FEEDBACK, style = "font-size: 75%; height:60px;"), HTML("<b><h4>Feedback On Performance</h4></b>"), 
          color = ifelse(grepl("to work", subsetData()$FEEDBACK), "red",
                         ifelse(grepl("figure", subsetData()$FEEDBACK), "orange",
                                ifelse(grepl("better", subsetData()$FEEDBACK), "orange",
                                       ifelse(subsetData()$FEEDBACK == "Your adherence is great, but let's work on your performance!", "orange",
                                              ifelse(grepl("reat", subsetData()$FEEDBACK), "green", "blue"))))))
      })
    
    ####################################
    #        NEBULISATION PLOTS        #
    ####################################
    
    # Daily nebulisations plot
    output$plot1 <- renderPlot({
      
      adherenceDaily() %>%
        filter(PATIENTNUMBER %in% input$patient) %>%
        ggplot(aes(DATE, DAILY_N)) +
        geom_area(fill = "skyblue", colour = "black") +
        labs(x = "Date", y = "Number of Medications Taken") +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d %Y", expand = c(0,0)) +
        scale_y_continuous(breaks = seq(0, max(adherenceDaily()$DAILY_N))) +
        geom_hline(aes(yintercept = 2, colour = "Target"), linetype = "dashed", size = 2) +
        theme_bw(base_size = 20) +
        theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
        scale_colour_manual(name = NULL, values = c("green"))
    })
    
    # Cumulative adherence plot
    output$plot2 <- renderPlot({
      
      adherenceCumulative() %>%
        filter(PATIENTNUMBER %in% input$patient) %>%
        ggplot(aes(DATE, CUMUL)) +
        geom_line(size = 1.5) +
        labs(x = "Date", y = "Daily Adherence Rate (%)") +
        scale_y_continuous(limits = c(0,100), n.breaks = 10) +
        scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d %Y", expand = c(0,0)) +
        geom_hline(aes(yintercept = 80, colour = "Target"), linetype = "dashed", size = 2) +
        theme_bw(base_size = 20) +
        theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
        scale_colour_manual(name = NULL, values = c("green", "red"))
      
    })
    
  }


# Run code
shinyApp(ui, server)
