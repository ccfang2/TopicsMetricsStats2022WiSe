rm(list = ls())
library(shiny)
library(shinyWidgets)
library(HonestDiD)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(latex2exp)
# library(shinyBS)
options(warn = -1)

# user interface
ui <- fluidPage(
  # bsTooltip(id = "lvec", title = "Examples: 1 means parameter at the first period post treatment; 
  #           1,2 means average over the first two periods post treatment;Leave it blank if you are interested in average causal effect over all post periods", 
  #           placement = "right", trigger = "hover"),
  # Background color
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  # Title
  titlePanel("Rambachan and Roth (2022): Sensitivity Analysis"),
  sidebarLayout(
    sidebarPanel(
      # design all widgets
      p(h4("Sensitivity analysis for papers discussed in Rambachan and Roth (2022)")),
      p(strong(span("Step 1: ", style="color:blue"))),
      selectInput("paper",label="Select a paper to analyze",
                  choices = list("Bailey and Goodman-Bacon (2015)"=1,
                                 "Bosch and Campos-Vazquez (2014)"=2,
                                 # "Deryugina (2017)"=3,
                                 "Deschenes et al. (2017)"=4,
                                 "Fitzpatrick and Lovenheim (2014)"=5,
                                 "Gallagher (2014)"=6,
                                 "He and Wang (2017)"=7,
                                 "Kuziemko et al. (2018)"=8,
                                 "Lafortune et al. (2017)"=9,
                                 "Markevich and Zhuravskaya (2018)"=10,
                                 "Tewari (2014)"=11,
                                 "Ujhelyi (2014)"=12),
                  selected = 1),
      p(strong(span("Step 2: ", style="color:blue"))),
      radioButtons("method","Select a method to construct confidence interval",
                   choices = list("Conditional"="Conditional",
                                  "Conditional hybrid"="C-LF"),
                   selected = "C-LF"),
      p(strong(span("Step 3: ", style="color:blue"))),
      checkboxGroupInput("delta","Select base choice of Delta",
                         choices = list("Smoothness restriction"=1,
                                        "Bounding Relative Magnitudes"=2),
                         selected = 1),
      p(strong("Click the button below to select more restrictions on Delta (optional).")),
      dropdownButton(
        # tags$h3("List of Input"),
        radioButtons("sign","Sign restriction",
                     choices = list("None"=1,
                                    "Positive"=2,
                                    "Negative"=3),
                     selected = 1),
        radioButtons("monotonicity","Shape restriction",
                     choices = list("None"=1,
                                    "Increasing"=2,
                                    "Decreasing"=3),
                     selected = 1),
        circle = TRUE,
        status = "info", 
        icon = icon("gear"), width = "50px",
        tooltip = tooltipOptions(title = "If Bounding Relative Magnitudes is selected for the base Delta, it is not allowed to select both shape and sign restrictions here. Otherwise, it is allowed.")
      ),
      p(strong(span("Step 4: ", style="color:blue"))),
      tags$div(title=" Examples: \n 1 means parameter at the first period post treatment; \n
            1,2 means average over the first two periods post treatment;\n
               Leave it blank if you are interested in average causal effect over all post periods",
      textInput("lvec", "Enter the vector to determine parameter of interest", 
                                               value = "")),
      p(strong(span("Step 5: ", style="color:blue"))),
      sliderInput("alpha", "Select alpha, the level of significance", min = 0, 
                  max = 1, value = 0.05),
      actionButton("start", "Start"),
      actionButton("reset", "Clear"),
      br(),
      br(),
      p("Reference: Rambachan, A. and Roth, J. (2022), A more credible approach to parallel trends, Forthcoming, Review of Economic Studies"),
      p("Contact: Chencheng Fang, ccfang@uni-bonn.de")
    ),
  # main panel to display outputs
  mainPanel(
    h3(textOutput("currentTime")),
    p("The clock is frozen after 'Start' is clicked, but it will return to correct current time after outputs pop out. 
      So, the freezing of clock is a signal that code is running.
      Usually it takes 2 min to 10 min to get outputs depending on the selected paper, specific settings and your machine capacity."),
    tabsetPanel(type = "tabs",
                tabPanel("Sensitivity Results", 
                         fluidRow(
                                  column(12,plotOutput("ssplot"))),
                         br(),
                         br(),
                         fluidRow(
                                  column(12,verbatimTextOutput("conclusion")))),
                tabPanel("Original Data", 
                         fluidRow(
                                  column(12,plotOutput("betaplot"))),
                         fluidRow(
                                  column(12,plotOutput("covplot"))))
                )
    )
  )
)

# server function
server <- function(input, output, session) {
  # import dataset
  dataset <- readRDS("data/ResultsObjectList.rds")
  v <- reactiveValues(result = NULL)
  
  # observe events with a click of start
  observeEvent(input$start,{
    
    data <- dataset[[as.numeric(input$paper)]]
    
    if (input$lvec=="") {
      lvec <- as.numeric(strsplit(input$lvec,",")[[1]])
      weight <- rep(1,length(data$postPeriodIndices))/length(data$postPeriodIndices)
      } else {
        lvec <- as.numeric(strsplit(input$lvec,",")[[1]])
        weight <- rep(0,length(data$postPeriodIndices))
        weight[lvec] <- 1
        weight <- weight/length(lvec)
      }
    
    sign <- if (input$sign=="2") "positive" else {if(input$sign =="3") "negative"}
    monotonicity <- if (input$monotonicity == "2") "increasing" else {if(input$monotonicity =="3") "decreasing"}
    
    # original OLS betahat
    originalResults <- constructOriginalCS(
      betahat = data$beta, #coefficients
      sigma = data$sigma, #covariance matrix
      numPrePeriods = length(data$prePeriodIndices), #num of pre-treatment coefs
      numPostPeriods = length(data$postPeriodIndices), # num of post-treatment coefs
      l_vec = weight, #l_vec
      alpha = input$alpha
    )
    
    if (all(length(input$delta)==1 & input$delta == "2")) {
      delta_rm_results <- createSensitivityResults_relativeMagnitudes(
        betahat = data$beta, #coefficients
        sigma = data$sigma, #covariance matrix
        numPrePeriods = length(data$prePeriodIndices), #num. of pre-treatment coefs
        numPostPeriods = length(data$postPeriodIndices), #num. of post-treatment coefs
        bound = "deviation from parallel trends", #the base choice of Delta
        method = input$method, #conditional least-favorable hybrid
        l_vec = weight, #l_vec
        biasDirection = sign, #sign restriction
        monotonicityDirection = monotonicity, #monotonicity restriction
        alpha = input$alpha #desired size of robust confidence set
      )
      v$result<-list(data=data,lvec=lvec,delta_rm_results=delta_rm_results, originalResults=originalResults)
    } else {
      if (all(length(input$delta)==1 & input$delta == "1")) {
        delta_sd_results <- createSensitivityResults(
          betahat = data$beta, #coefficients
          sigma = data$sigma, #covariance matrix
          numPrePeriods = length(data$prePeriodIndices), #num of pre-treatment coefs
          numPostPeriods = length(data$postPeriodIndices), # num of post-treatment coefs
          method = input$method, #conditional least-favorable hybrid
          Mvec = NULL, # values of M
          l_vec = weight, #l_vec
          monotonicityDirection = monotonicity, #monotonicity restriction
          biasDirection = sign, #sign restriction
          alpha = input$alpha #desired size of robust confidence set
        )
        v$result<-list(data=data,lvec=lvec,delta_sd_results=delta_sd_results, originalResults=originalResults)
      } else {
        delta_rm_results <- createSensitivityResults_relativeMagnitudes(
          betahat = data$beta, #coefficients
          sigma = data$sigma, #covariance matrix
          numPrePeriods = length(data$prePeriodIndices), #num. of pre-treatment coefs
          numPostPeriods = length(data$postPeriodIndices), #num. of post-treatment coefs
          bound = "deviation from linear trend", #the base choice of Delta
          method = input$method, #conditional least-favorable hybrid
          l_vec = weight, #l_vec
          biasDirection = sign, #sign restriction
          monotonicityDirection = monotonicity, #monotonicity restriction
          alpha = input$alpha #desired size of robust confidence set
        )
        v$result<-list(data=data,lvec=lvec,delta_rm_results=delta_rm_results, originalResults=originalResults)
      }
    }
  })
  
  # observe event with a click of reset
  observeEvent(input$reset, {
    v$result <- NULL
  })
  
  # sensitivity plot
  output$ssplot <- renderPlot({
    if (is.null(v$result)) return()
    
    if(all(length(input$delta)==1 & input$delta == "1")) {
      createSensitivityPlot(v$result$delta_sd_results, v$result$originalResults)
    } else {
      createSensitivityPlot_relativeMagnitudes(v$result$delta_rm_results, v$result$originalResults)
    }
  })
  
  # conclusion
  output$conclusion <- renderText({
    if (is.null(v$result)) return()
    
    target <- if (length(v$result$lvec) == 0) {
      paste(" Parameter of Interest: Average Causal Effect over all",
                      length(v$result$data$postPeriodIndices) ,"Post-Treatment Periods. \n")
    } else {
      if (length(v$result$lvec) == 1) {
        paste(" Parameter of Interest: Causal Effect at the",
                        case_when(v$result$lvec == 1 ~ "1st",
                                  v$result$lvec == 2 ~"2nd",
                                  v$result$lvec == 3 ~ "3rd",
                                  TRUE ~ paste(v$result$lvec,"th",sep="")),
              "Period after treatment. \n")
      } else {
        paste(" Parameter of Interest: Average Causal Effect over Periods",
              paste(v$result$lvec, collapse = ",") ,"after treatment. \n")
      }
    }
    

    if(all(length(input$delta)==1 & input$delta == "1")) {
      m<-as.numeric(v$result$delta_sd_results[sum(v$result$delta_sd_results$lb*v$result$delta_sd_results$ub>=0),ncol(v$result$delta_sd_results)])
      ifelse(is.na(m), paste(target, "Conclusion: We cannot reject a null effect."), 
             paste(target, "Conclusion: We can reject a null effect unless we are willing to allow for the linear extrapolation across consecutive periods to be off by more than", round(m,2), "percentage points."))
    } else{
      mbar <- as.numeric(v$result$delta_rm_results[sum(v$result$delta_rm_results$lb*v$result$delta_rm_results$ub>=0),ncol(v$result$delta_rm_results)])
      ifelse(is.na(mbar), paste(target, "Conclusion: The result is insignificant with any restriction of Bounding Relative Magnitudes"),
             paste(target, "Conclusion: The significant result is robust to allowing for violations of parallel trends up to", round(mbar,2), "as big as the max violation in the pre-treatment period"))
    }
  })
  
  # beta plot
  output$betaplot <- renderPlot({
    if (is.null(v$result)) return()
    
    data.frame(beta=v$result$data$beta, num=1:length(v$result$data$beta)) %>%
    ggplot()+
      geom_point(aes(num,beta))+
      geom_vline(xintercept = length(v$result$data$prePeriodIndices)+0.5, 
                 colour="red",linetype="dashed")+
      labs(y=expression(hat(beta)),x="", title = "Scatter Plot")
  })
  
  # sigma plot
  output$covplot <- renderPlot({
    if (is.null(v$result)) return()
    
    v$result$data$sigma %>%
      cov2cor %>%
      ggcorrplot(method = "circle", type = "lower", show.diag = T,
                 legend.title = "", title = "Correlation Plot")
  })
  
  # time
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
}


shinyApp(ui, server)