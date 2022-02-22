

###########################################################################
###########################################################################
###                                                                     ###
###                ADCLEARANCE | FINAL VERSION OF DASHBOARD             ###
###                      RYAN MCCHRYSTAL | 40205088                     ###
###                                                                     ###
###########################################################################
###########################################################################


# Required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
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
      title = tags$img(src="AdCLEARance.png",
                       height = '100', 
                       width = '250'),
      
      titleWidth = 280
      
      ),
    
    dashboardSidebar(
      disable = TRUE,
      sidebarMenu()
    ),
    
    # Dashboard interface
    dashboardBody(

      # Adding shinyjs for hide button feature
      useShinyjs(),
      
      # Disable progress bar for uploading file (saves space)
      tags$style(".shiny-file-input-progress {display: none}"),
      
      # Extends page height and adds scroll bar
      tags$head(tags$style(HTML('.content-wrapper { height: 1790px !important; }'))),
      
      # Row 1 - Upload data and select patient
      fluidRow(
        
        # Upload data
        box(title = HTML("<b>1. Upload eFlow Data Export</b>"),
            fileInput("upload", HTML("<b>File must be in Excel worksheet (.xlsx) format</b>")),
            height = 120,
            width = 3),
        
        # Patient selection
        box(title = HTML("<b>2. Select a Patient</b>"),
            selectInput("patient", HTML("<b>Choose a Patient ID from the dropdown list</b>"), choices = c("")),
            height = 120,
            width = 3),
        
        # Filter selection
        box(title = HTML("<b>3. Select an Enrollment Period</b>"),
            checkboxGroupInput("visit", HTML("<b>View performance around one visit, multiple visits or overall enrollment</b>"), 
                               c("Overall", "Visit 1", "Visit 2", "Visit 3", "Visit 4", "Visit 5", "Visit 6"), inline = TRUE),
            height = 120,
            width = 4),
        
        box(title = HTML("<b>4. Start Analysing eFlow Data</b>"),
            actionButton("analyse", "Start", class = "btn-success", 
                         style = "position: absolute; right: 160px; padding: 5px; font-size:200%; font-weight: bold; color:white", width = "100px"),
            actionButton("reset", "Reset", class = "btn-warning", 
                         style = "position: absolute; right: 30px; padding: 5px; font-size:200%; font-weight: bold; color:white", width = "100px"),
            height = 120,
            width = 2)
        

        ),
      
      # Row 2 - Summary information on adherence and nebuliser usage
      fluidRow(
        
        # Column 1 - Adherence summary
        column(width = 6,
               box(title = HTML("<b> Adherence To Hypertonic Saline </b>"),
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
               box(title = HTML("<b> Nebuliser Usage & Feedback </b>"),
                   # Total times easycare was used
                   valueBoxOutput("cleaning", width = 4),
                   # Most common error
                   valueBoxOutput("error", width = 8),
                   # Recommendation for intervention
                   valueBoxOutput("recommend", width = 12),
                   width = 13,
                   height = 350)),
        
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }")
        
      ),
      
      
      # Row 3 - Visualisation of cumulative adherence rate
      fluidRow(
        
        # Cumulative adherence
        box(title = HTML("<b>Changes In Daily Usage of Nebuliser</b>"),
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
    
    # IDs of interest who have at least one year of data
    IDs <- c("R01008", "R01011", "R01012", "R03003", "R03005", "R06001", "R06002", "R06008", "R06009",
             "R06011", "R06012", "R06013", "R06015", "R06016", "R07003", "R08005", "R08007", "R11004",
             "R11014", "R11015", "R11016", "R11018", "R15005", "R15009")
    
    ####################################
    #           DATA UPLOAD            #
    ####################################
    
    # Grabs uploaded data as a reactive object
    inputGrab <- reactive({req(input$upload)})
    
    # Reads in uploaded data and filters IDs
    uploadData <- reactive({
      
      dat <- 
        read_excel(inputGrab()$datapath)
      
      dat2 <- 
        dat %>%
        filter(grepl(paste0(IDs, collapse = "|"), PATIENTNUMBER)) %>%
        arrange(PATIENTNUMBER)
      
    })
    
    ####################################
    #           USER INPUTS            #
    ####################################
    
    # Observe data upload and set listing as patient numbers
    observe({
      updateSelectInput(
        session, "patient",
        choices = unique(uploadData()$PATIENTNUMBER))
    })
    
    
    # Subset dashboard data by ID selection
    subsetData <- reactive({
      uploadData() %>%
        filter(!is.na(PATIENTNUMBER) & grepl(paste0(IDs, collapse = "|"), PATIENTNUMBER))
    })
    
    # Reset selections for visit filters when reset is pressed
    observeEvent(input$reset, {
      updateCheckboxGroupInput(session, "visit", 
                               choices = c("Overall", "Visit 1", "Visit 2", "Visit 3", "Visit 4", "Visit 5", "Visit 6"),
                               inline = TRUE,
                               selected = NULL)
    })
    
    # Create tidied data for specified ID
    analysisData <- 
      eventReactive(input$analyse, {
        
        dat <- 
          subsetData() %>%
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
        
        dat2 <- 
          dat %>%
          group_by(PATIENTNUMBER, YEAR) %>%
          filter(!duplicated(WEEK)) %>%
          group_by(PATIENTNUMBER) %>%
          mutate(ENROLLED_WEEKS = row_number(),
                 WINDOW = case_when(ENROLLED_WEEKS == 1 ~ "Visit 1",
                                    ENROLLED_WEEKS == 2 ~ "Visit 2",
                                    ENROLLED_WEEKS > 2 & ENROLLED_WEEKS <= 8 ~ "Visit 3",
                                    ENROLLED_WEEKS > 8 & ENROLLED_WEEKS <= 26 ~ "Visit 4",
                                    ENROLLED_WEEKS > 26 & ENROLLED_WEEKS <= 52 ~ "Visit 5",
                                    ENROLLED_WEEKS > 52 ~ "Visit 6")) %>%
          select(PATIENTNUMBER, YEAR, WEEK, WINDOW) %>%
          right_join(., dat)
        
      })
    
    
    ####################################
    #          ADHERENCE DATA          #
    ####################################

    # Adherence subset
    adherence <- reactive({
      
      subset_HTS <- 
        analysisData() %>%
        filter(TYPE %in% c("Nebulisation", "Easycare Neb")) %>%
        group_by(PATIENTNUMBER) %>%
        complete(DATE = seq.Date(min(DATE), max(DATE), "days")) %>%
        mutate(TYPE = ifelse(is.na(VALID_INHAL), "Missed Day", TYPE)) %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(WEEK = week(DATE),
               MONTH = month(DATE),
               YEAR = year(DATE),
               TIMEDIFF = round((unclass(TIME) - unclass(lag(TIME)))/3600,1),
               ADHERENT = case_when(n() == 2
                                    & INHAL_PERIOD >= 40
                                    & TIMEDIFF >= 1
                                    & VALID_INHAL %in% c(1,2) ~ 1,
                                    TRUE ~ 0)) %>% ungroup()
      
      subset_fix <- 
        subset_HTS %>%
        group_by(PATIENTNUMBER, YEAR) %>%
        filter(!duplicated(WEEK)) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(ENROLLED_WEEK = row_number(),
               WINDOW = case_when(ENROLLED_WEEK == 1 ~ "Visit 1",
                                  ENROLLED_WEEK == 2 ~ "Visit 2",
                                  ENROLLED_WEEK > 2 & ENROLLED_WEEK <= 8 ~ "Visit 3",
                                  ENROLLED_WEEK > 8 & ENROLLED_WEEK <= 26 ~ "Visit 4",
                                  ENROLLED_WEEK > 26 & ENROLLED_WEEK <= 52 ~ "Visit 5",
                                  ENROLLED_WEEK > 52 ~ "Visit 6")) %>%
        select(PATIENTNUMBER, YEAR, WEEK, WINDOW2 = WINDOW) %>%
        right_join(., subset_HTS) %>%
        select(-c(WINDOW)) %>%
        rename(WINDOW = WINDOW2) %>%
        arrange(PATIENTNUMBER, DATE)
      
    })
    
    
    # Filtered adherence data by check boxes selected
    adherenceFilter <- reactive({
      
      if(input$visit == "Overall"){
        adherence()
        }
      else if(input$visit != "Overall"){
        adherence() %>% filter(WINDOW %in% input$visit)
      }
      
    })
    
    
    ####################################
    #          CLEANING DATA           #
    ####################################
    
    # Cleaning data
    # Counting usage for each patient
    cleaning <- reactive({
      
      subset_clean <- 
        analysisData() %>%
        group_by(PATIENTNUMBER, DATE) %>%
        filter(TYPE == "Easycare") %>%
        filter(n() < 2) %>%
        group_by(PATIENTNUMBER, MONTH) %>%
        mutate(COUNT = n()) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(MEAN = round(mean(COUNT))) %>%
        filter(!duplicated(MEAN)) %>% ungroup()
      
    })
    
    
    # Filtered cleaning data by check boxes selected
    cleaningFilter <- reactive({
      
      if(input$visit == "Overall"){
        cleaning()
      }
      else if(input$visit != "Overall"){
        cleaning() %>% filter(WINDOW %in% input$visit)
      }
      
    })
    
    
    ####################################
    #          HTS ERROR DATA          #
    ####################################
    
    # Error data
    errors <- reactive({
      
      subset_errors <- 
        analysisData() %>%
        filter(TYPE %in% c("Errors")) %>%
        mutate(CODE = case_when(INHAL_INTERRUPTION == 1 ~ "Lost supply voltage",
                                INHAL_INTERRUPTION == 2 ~ "Aerosol head disconnected",
                                INHAL_INTERRUPTION == 3 ~ "Medication missing",
                                INHAL_INTERRUPTION == 5 ~ "Manual shutdown",
                                INHAL_INTERRUPTION == 6 ~ "Battery empty",
                                INHAL_INTERRUPTION == 7 ~ "Timeout",
                                INHAL_INTERRUPTION == 8 ~ "Paused Timeout"))
    })
    
    # Filtered error data by check boxes selected
    errorFilter <- reactive({
      
      if(input$visit == "Overall"){
        errors()
      }
      else if(input$visit != "Overall"){
        errors() %>% filter(WINDOW %in% input$visit)
      }
      
    })
    
    
    ####################################
    #          DASHBOARD PREP          #
    ####################################
    
    # Mean daily nebulisations data
    adherenceMean <- reactive({
      
      adherence_dailyMean <- 
        adherenceFilter() %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(DAILY_N = n()) %>%
        group_by(PATIENTNUMBER) %>%
        summarise(DAILY_MEAN = round(mean(DAILY_N, na.rm = T),1)) %>% ungroup()
      
    })
    
    # Number of daily nebulisations performed
    adherenceDaily <- reactive({
      
      adherence_daily <- 
        adherenceFilter() %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(DAILY_N = n()) %>%
        filter(!duplicated(DAILY_N)) %>%
        mutate(DAILY_N = ifelse(is.na(VALID_INHAL), 0, DAILY_N)) %>%
        select(PATIENTNUMBER, DATE, DAILY_N, WINDOW) %>% ungroup()
      
    })
    
    # Cumulative adherence
    adherenceCumulative <- reactive({
      
      adherence_merge <- adherenceDaily() %>% select(PATIENTNUMBER, DATE)
      adherence_merge2 <- adherenceFilter() %>% select(PATIENTNUMBER, DATE, WINDOW2 = WINDOW)
      
      adherenceCumul <- 
        adherenceFilter() %>%
        group_by(PATIENTNUMBER, DATE) %>%
        mutate(ADHERENT = ifelse(TYPE == "Missed Day", NA_real_, ADHERENT)) %>%
        filter(ADHERENT %in% c(1, NA_real_) | sum(ADHERENT) == 0) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(ADHERENT = ifelse(is.na(ADHERENT), 0, ADHERENT),
               DAY = row_number(),
               CUMUL = cumsum(ADHERENT)/DAY * 100)
      
    })
    
    
    # Weekly adherence
    adherenceWeekly <- reactive({
      
      adherence_weekly <- 
        adherenceFilter() %>%
        group_by(PATIENTNUMBER, WEEK, YEAR) %>%
        mutate(WEEK_N = sum(ADHERENT)) %>%
        filter(!duplicated(DATE)) %>%
        mutate(WEEK_ADH = ifelse(WEEK_N/length(WEEK) > 0.7, 1, 0)) %>%
        group_by(PATIENTNUMBER, YEAR) %>%
        filter(!duplicated(WEEK)) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(WEEKS = row_number(),
               WEEKLY = round(sum(WEEK_ADH)/length(WEEKS)*100)) %>%
        filter(!duplicated(WEEKLY)) %>%
        select(PATIENTNUMBER, WEEKLY) %>% ungroup()
    })
    
    
    # Monthly adherence
    adherenceMonthly <- reactive({
      
      adherence_monthly <- 
        adherenceFilter() %>%
        group_by(PATIENTNUMBER, WEEK, YEAR) %>%
        mutate(WEEK_N = sum(ADHERENT)) %>%
        filter(!duplicated(DATE)) %>%
        mutate(WEEK_ADH = ifelse(WEEK_N/length(WEEK) > 0.7, 1, 0)) %>%
        group_by(PATIENTNUMBER, YEAR) %>%
        filter(!duplicated(WEEK)) %>%
        group_by(PATIENTNUMBER, MONTH) %>%
        mutate(MONTHLY = round(sum(WEEK_ADH)/length(MONTH)*100)) %>%
        filter(!duplicated(MONTHLY)) %>%
        group_by(PATIENTNUMBER) %>%
        mutate(MONTHLY = round(mean(MONTHLY))) %>%
        filter(!duplicated(MONTHLY)) %>%
        select(PATIENTNUMBER, MONTHLY) %>% ungroup()
    })
    
    
    ####################################
    #         ASSEMBLING DATA          #
    ####################################
    
    # Dashboard data
    dashboardData <- reactive({
      
      # Patient IDs
      dashboard_IDs <- IDs
      
      # Cleaning count
      cleaning <- cleaningFilter() %>% select(PATIENTNUMBER, MEAN)
      
      # Error count
      error <- 
        errorFilter() %>%
        group_by(PATIENTNUMBER) %>% 
        count(CODE, name = "OCCURRENCE") %>% 
        slice(which.max(OCCURRENCE)) %>% ungroup()
      
      # Total enrolled days
      dashboard_overall <- 
        adherenceCumulative() %>%
        group_by(PATIENTNUMBER) %>%
        filter(!duplicated(DATE)) %>%
        mutate(ENROLLED_DAYS = length(PATIENTNUMBER)) %>%
        filter(!duplicated(ENROLLED_DAYS)) %>%
        select(PATIENTNUMBER, ENROLLED_DAYS) %>% ungroup()
      
      # Total adherent days
      dashboard_days <- 
        adherenceFilter() %>%
        group_by(PATIENTNUMBER) %>%
        filter(ADHERENT == 1) %>%
        mutate(ADHERENT_DAYS = n()) %>%
        filter(!duplicated(ADHERENT_DAYS)) %>%
        select(PATIENTNUMBER, ADHERENT_DAYS) %>% ungroup()
      
      # Missed days
      dashboard_missed <- 
        adherenceFilter() %>%
        group_by(PATIENTNUMBER, DATE) %>%
        filter(sum(ADHERENT) == 0) %>%
        group_by(PATIENTNUMBER) %>%
        count(name = "MISSED_DAYS") %>% ungroup()
      
      # All statistics for patients
      dashboard_data <- 
        data.frame(PATIENTNUMBER = dashboard_IDs) %>%
        full_join(., dashboard_overall) %>%
        full_join(., dashboard_days) %>%
        mutate(DAILY_ADH = round(ADHERENT_DAYS/ENROLLED_DAYS*100),
               PCNT_MISSED = round(100 - DAILY_ADH)) %>%
        full_join(., adherenceWeekly()) %>%
        full_join(., adherenceMonthly()) %>%
        full_join(., adherenceMean()) %>%
        full_join(., cleaning) %>%
        full_join(., error) %>%
        replace(is.na(.), 0) %>%
        mutate(OVERALL = round((DAILY_ADH + WEEKLY + MONTHLY)/3)) %>%
        mutate(FEEDBACK = case_when(OVERALL >= 80
                                    & PCNT_MISSED <= 20
                                    & OCCURRENCE <= 50
                                    ~ "Overall, your adherence and performance are great - keep it up!",
                                    
                                    OVERALL >= 80
                                    & PCNT_MISSED <= 20
                                    & OCCURRENCE > 50
                                    ~ "Your adherence is great, but let's work on your performance!",
                                    
                                    OVERALL < 80 & OVERALL >= 50
                                    & PCNT_MISSED > 20
                                    & OCCURRENCE <= 50
                                    ~ "Good performance but adherence could be better, let's discuss?",
                                    
                                    OVERALL < 80 & OVERALL >= 50
                                    & OCCURRENCE >= 50
                                    ~ "Let's figure out how we can improve your adherence/performance!",
                                    
                                    OVERALL < 50
                                    & PCNT_MISSED > 20
                                    & OCCURRENCE <= 50
                                    ~ "We need to work on your adherence, let's discuss?",
                                    
                                    OVERALL < 50
                                    & OCCURRENCE > 50
                                    ~ "We need to work on your adherence/performance, let's discuss?",
                                    TRUE ~ "Not Enough Data Available")) %>%
        filter(PATIENTNUMBER == input$patient)
      
    })
    
    
    ####################################
    #        SUMMARY STATISTICS        #
    ####################################
    
    # Overall adherence
    output$overall <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(dashboardData()$OVERALL, "%"), style = "font-size: 100%;height:60px;"), HTML("<b><h4>Overall Adherence</h4></b>"), 
          color = ifelse(dashboardData()$OVERALL >= 80, "green", 
                         ifelse(dashboardData()$OVERALL < 80 & dashboardData()$OVERALL > 50, "orange",
                                ifelse(dashboardData()$OVERALL <= 50, "red")))
        ) 
      })
    
    # Daily adherence
    output$daily <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(dashboardData()$DAILY_ADH, "%"), style = "font-size: 100%;height:60px;"), HTML("<b><h4>Daily Adherence</h4></b>"),
          color = ifelse(dashboardData()$DAILY_ADH >= 80, "green", 
                         ifelse(dashboardData()$DAILY_ADH < 80 & dashboardData()$DAILY_ADH > 50, "orange",
                                ifelse(dashboardData()$DAILY_ADH <= 50, "red")))
        )
      })
    
    # Weekly adherence
    output$weekly <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(dashboardData()$WEEKLY, "%"), style = "font-size:100%;height:60px;"), HTML("<b><h4>Weekly Adherence</h4></b>"),
          color = ifelse(dashboardData()$WEEKLY >= 80, "green", 
                         ifelse(dashboardData()$WEEKLY < 80 & dashboardData()$WEEKLY > 50, "orange",
                                ifelse(dashboardData()$WEEKLY <= 50, "red")))
        )
      })
    
    # Monthly adherence
    output$monthly <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(dashboardData()$MONTHLY, "%"), style = "font-size: 100%;height:60px;"), HTML("<b><h4>Monthly Adherence</h4></b>"),
          color = ifelse(dashboardData()$MONTHLY >= 80, "green", 
                         ifelse(dashboardData()$MONTHLY < 80 & dashboardData()$MONTHLY > 50, "orange",
                                ifelse(dashboardData()$MONTHLY <= 50, "red")))
        )
      })
    
    # Average number of daily nebulisations
    output$average <- 
      renderInfoBox({
        valueBox(
          dashboardData()$DAILY_MEAN, HTML("<b><h4>Average Number of Daily Nebulisations Performed</b></h4>"), 
          color = ifelse(dashboardData()$DAILY_MEAN <= 2 & dashboardData()$DAILY_MEAN >= 1.6, "green",
                         ifelse(dashboardData()$DAILY_MEAN <= 1.59 & dashboardData()$DAILY_MEAN >= 1
                                | dashboardData()$DAILY_MEAN > 2, "orange",
                                ifelse(dashboardData()$DAILY_MEAN < 1, "red")))
        )
      })
    
    # Percentage of enrolled days that were missed
    output$missed <- 
      renderInfoBox({
        valueBox(
          paste0(dashboardData()$PCNT_MISSED, "%"), HTML("<b><h4>Amount of Nonadherent Days During Enrollment</h4></b>"), 
          color = ifelse(dashboardData()$PCNT_MISSED >= 50, "red", 
                         ifelse(dashboardData()$PCNT_MISSED < 50 & dashboardData()$PCNT_MISSED > 10, "orange",
                                ifelse(dashboardData()$PCNT_MISSED <= 10, "green")))
        )
      })
    
    # Most common nebulisation error
    output$error <- 
      renderInfoBox({
        valueBox(
          tags$p(ifelse(dashboardData()$OCCURRENCE < 50, "None",
                        ifelse(dashboardData()$OCCURRENCE >= 50, paste0(dashboardData()$CODE, " - ", dashboardData()$OCCURRENCE, " times"))), 
                 style = "font-size: 80%; height:60px;"), HTML("<b><h4>Most Frequent Error Occurring When Peforming Nebulisation<h4></b>"), 
          color = ifelse(dashboardData()$OCCURRENCE < 50, "green",
                         ifelse(dashboardData()$OCCURRENCE >= 50 & dashboardData()$OCCURRENCE < 100, "orange",
                                ifelse(dashboardData()$OCCURRENCE > 100, "red")))
        )
      })
    
    # Most common cleaning error
    output$cleaning <- 
      renderInfoBox({
        valueBox(
          tags$p(paste0(dashboardData()$MEAN), style = "font-size: 100%; height:60px;"), HTML("<b><h4>Monthly Easycare Usage</h4></b>"), 
          color = ifelse(dashboardData()$MEAN == 1, "green",
                         ifelse(dashboardData()$MEAN < 1, "red", 
                                ifelse(dashboardData()$MEAN > 1, "orange", "blue"))))
      })
    
    # Patient recommendation
    output$recommend <- 
      renderInfoBox({
        valueBox(
          tags$p(dashboardData()$FEEDBACK, style = "font-size: 75%; height:60px;"), HTML("<b><h4>Feedback On Performance</h4></b>"), 
          color = ifelse(grepl("to work", dashboardData()$FEEDBACK), "red",
                         ifelse(grepl("figure", dashboardData()$FEEDBACK), "orange",
                                ifelse(grepl("better", dashboardData()$FEEDBACK), "orange",
                                       ifelse(dashboardData()$FEEDBACK == "Your adherence is great, but let's work on your performance!", "orange",
                                              ifelse(grepl("reat", dashboardData()$FEEDBACK), "green", "blue"))))))
      })
    
    
    ####################################
    #        NEBULISATION PLOTS        #
    ####################################
    
    # Daily nebulisations plot
    output$plot1 <- renderPlot({
      
      if(input$visit == "Overall"){
        
        adherenceDaily() %>%
          filter(PATIENTNUMBER == input$patient) %>%
          ggplot(aes(DATE, DAILY_N)) +
          geom_area(fill = "skyblue", colour = "black") +
          labs(x = "Date", y = "Number of Nebulisations Performed") +
          scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d %Y", expand = c(0.01,0.01)) +
          scale_y_continuous(breaks = seq(0, max(adherenceDaily()$DAILY_N))) +
          geom_hline(aes(yintercept = 2, colour = "Target"), linetype = "solid", size = 1.3) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 1"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 1")) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 2"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 2")) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 3"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 3")) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 4"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 4")) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 5"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 5")) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 6"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 6")) +
          theme_bw(base_size = 20) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_colour_manual(name = NULL, values = c("green", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))
        
      }
      
      else if(c("Visit 6") %in% input$visit){
        
        adherenceDaily() %>%
          filter(PATIENTNUMBER == input$patient) %>%
          ggplot(aes(DATE, DAILY_N)) +
          geom_area(fill = "skyblue", colour = "black") +
          labs(x = "Date", y = "Number of Nebulisations Performed") +
          scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d %Y", expand = c(0.01,0.01)) +
          scale_y_continuous(breaks = seq(0, max(adherenceDaily()$DAILY_N))) +
          geom_hline(aes(yintercept = 2, colour = "Target"), linetype = "solid", size = 1.3) +
          theme_bw(base_size = 20) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_colour_manual(name = NULL, values = c("green"))
        
        
      }
      
      else if(c("Visit 4") %in% input$visit | c("Visit 5") %in% input$visit){
        
        adherenceDaily() %>%
          filter(PATIENTNUMBER == input$patient) %>%
          ggplot(aes(DATE, DAILY_N)) +
          geom_area(fill = "skyblue", colour = "black") +
          labs(x = "Date", y = "Number of Nebulisations Performed") +
          scale_x_date(date_breaks = "1 week", date_labels = "%b %d %Y", expand = c(0.01,0.01)) +
          scale_y_continuous(breaks = seq(0, max(adherenceDaily()$DAILY_N))) +
          geom_hline(aes(yintercept = 2, colour = "Target"), linetype = "solid", size = 1.3) +
          theme_bw(base_size = 20) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_colour_manual(name = NULL, values = c("green"))
        
        
      }
      
      else if((c("Visit 1", "Visit 2", "Visit 3") %in% input$visit) & !(c("Visit 4", "Visit 5", "Visit 6") %in% input$visit)){
        
        adherenceDaily() %>%
          filter(PATIENTNUMBER == input$patient) %>%
          ggplot(aes(DATE, DAILY_N)) +
          geom_area(fill = "skyblue", colour = "black") +
          labs(x = "Date", y = "Number of Nebulisations Performed") +
          scale_x_date(date_breaks = "1 day", date_labels = "%b %d %Y", expand = c(0.01,0.01)) +
          scale_y_continuous(breaks = seq(0, max(adherenceDaily()$DAILY_N))) +
          geom_hline(aes(yintercept = 2, colour = "Target"), linetype = "solid", size = 1.3) +
          theme_bw(base_size = 20) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_colour_manual(name = NULL, values = c("green"))
        
        
      }
      
      else if(input$visit == "Visit 1" | input$visit == "Visit 2" | input$visit == "Visit 3"){
        
        adherenceDaily() %>%
          filter(PATIENTNUMBER == input$patient) %>%
          ggplot(aes(DATE, DAILY_N)) +
          geom_area(fill = "skyblue", colour = "black") +
          labs(x = "Date", y = "Number of Nebulisations Performed") +
          scale_x_date(date_breaks = "1 day", date_labels = "%b %d %Y", expand = c(0.01,0.01)) +
          scale_y_continuous(breaks = seq(0, max(adherenceDaily()$DAILY_N))) +
          geom_hline(aes(yintercept = 2, colour = "Target"), linetype = "solid", size = 1.3) +
          theme_bw(base_size = 20) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_colour_manual(name = NULL, values = c("green"))
        
        
      }
      
    })
    
    # Adherence rate plot
    output$plot2 <- renderPlot({
      
      if(input$visit == "Overall"){
        
        adherenceCumulative() %>%
          filter(PATIENTNUMBER == input$patient) %>%
          ggplot(aes(DATE, CUMUL)) +
          geom_line(size = 1.5) +
          labs(x = "Date", y = "Daily Adherence Rate (%)") +
          scale_y_continuous(limits = c(0,100), n.breaks = 10) +
          scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d %Y", expand = c(0.01,0.01)) +
          geom_hline(aes(yintercept = 80, colour = "Target"), linetype = "solid", size = 1.3) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 1"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 1")) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 2"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 2")) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 3"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 3")) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 4"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 4")) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 5"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 5")) +
          geom_vline(aes(xintercept = mean(DATE), colour = "Visit 6"), linetype = "dashed", size = 1.3, . %>% filter(WINDOW == "Visit 6")) +
          theme_bw(base_size = 20) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_colour_manual(name = NULL, values = c("green", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))
        
      }
      
      else if(c("Visit 6") %in% input$visit){
        
        adherenceCumulative() %>%
          filter(PATIENTNUMBER == input$patient) %>%
          ggplot(aes(DATE, CUMUL)) +
          geom_line(size = 1.5) +
          labs(x = "Date", y = "Daily Adherence Rate (%)") +
          scale_y_continuous(limits = c(0,100), n.breaks = 10) +
          scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d %Y", expand = c(0.01,0.01)) +
          geom_hline(aes(yintercept = 80, colour = "Target"), linetype = "solid", size = 1.3) +
          theme_bw(base_size = 20) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_colour_manual(name = NULL, values = c("green"))
        
        
      }
      
      else if(c("Visit 4") %in% input$visit | c("Visit 5") %in% input$visit){
        
        adherenceCumulative() %>%
          filter(PATIENTNUMBER == input$patient) %>%
          ggplot(aes(DATE, CUMUL)) +
          geom_line(size = 1.5) +
          labs(x = "Date", y = "Daily Adherence Rate (%)") +
          scale_y_continuous(limits = c(0,100), n.breaks = 10) +
          scale_x_date(date_breaks = "1 week", date_labels = "%b %d %Y", expand = c(0.01,0.01)) +
          geom_hline(aes(yintercept = 80, colour = "Target"), linetype = "solid", size = 1.3) +
          theme_bw(base_size = 20) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_colour_manual(name = NULL, values = c("green"))
        
        
      }
      
      else if((c("Visit 1", "Visit 2", "Visit 3") %in% input$visit) & !(c("Visit 4", "Visit 5", "Visit 6") %in% input$visit)){
        
        adherenceCumulative() %>%
          filter(PATIENTNUMBER == input$patient) %>%
          ggplot(aes(DATE, CUMUL)) +
          geom_line(size = 1.5) +
          labs(x = "Date", y = "Daily Adherence Rate (%)") +
          scale_y_continuous(limits = c(0,100), n.breaks = 10) +
          scale_x_date(date_breaks = "1 day", date_labels = "%b %d %Y", expand = c(0.01,0.01)) +
          geom_hline(aes(yintercept = 80, colour = "Target"), linetype = "solid", size = 1.3) +
          theme_bw(base_size = 20) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_colour_manual(name = NULL, values = c("green"))
        
        
      }
      
      else if(input$visit == "Visit 1" | input$visit == "Visit 2" | input$visit == "Visit 3"){
        
        adherenceCumulative() %>%
          filter(PATIENTNUMBER == input$patient) %>%
          ggplot(aes(DATE, CUMUL)) +
          geom_line(size = 1.5) +
          labs(x = "Date", y = "Daily Adherence Rate (%)") +
          scale_y_continuous(limits = c(0,100), n.breaks = 10) +
          scale_x_date(date_breaks = "1 day", date_labels = "%b %d %Y", expand = c(0.01,0.01)) +
          geom_hline(aes(yintercept = 80, colour = "Target"), linetype = "solid", size = 1.3) +
          theme_bw(base_size = 20) +
          theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
          scale_colour_manual(name = NULL, values = c("green"))
        
        
      }
      
    })  
    
    
    # Observe start input, change to "Done" and disable button
    observeEvent(input$analyse, {
      
      isolate(updateActionButton(session, "analyse", label = "Done"))
      
      disable("analyse")
      
    })
    
  }
    

# Run code
shinyApp(ui, server)