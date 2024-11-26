library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinymeta)
library(bslib)
library(tidyverse)
library(bnlearn)
library(Rgraphviz)

theme_set(theme_classic())
# options(shiny.autoreload = FALSE)

source(
  here::here("functions.R")
)

# START SHINY APP

ui <- page_navbar(
  theme = bs_theme(preset = "minty"),
  title = "Building a Discrete Bayesian Network for Inference",
  sidebar = sidebar(
    title = "Parameters to change for Bayes Net Structure, Model, & Network Score",
    width = 350,
    fileInput("file", "Choose CSV File. Example dataset can be found at LINK", accept = c("text/csv", "text/comma-separated-values, .csv")),
    fileInput("user_wl", "A matrix with desired edges. Referred to as a whitelist. EXAMPLE CAN BE SEEN AT LINK", accept = c("text", "text, .txt")),
    fileInput("user_bl", "A matrix with unwanted edges. Referred to as a blacklist. EXAMPLE CAN BE SEEN AT LINK", accept = c("text", "text, .txt")),

    textInput("event", "Include event of interest. Write out the event as: ({Variable of Interest} == '1'), (e.g., (carb == '4'))"),
    textInput("evidence", "Include conditions of evidence in prediction. Write out the evidence as: ({Variable of Interest} == '0'), (e.g., (cyl == '6' | cyl == '4') & (hp != 'high_hp'))"),

    #radioButtons("dag_choice", "Do you know the structure of your DAG (Only Supporting Structure Learning Currently)", c("Yes", "No"), selected = "No")
    selectInput("struct_learn", "Structural learning algorithm to create DAG", c("hc", "tabu", "mmhc", "h2pc", "rsmax2"), selected = "hc"),
    selectInput("max_type", "What score-based algorithm to maximize for hybrid learning algorithm", c("hc", "tabu"), selected = "hc"),
    numericInput("restart_par", "Restart parameter for Hill-Climb algorithm", min = 0, max = 100000, value = 0),
    numericInput("perturb_par", "Perturb parameter for TABU algorithm", min = 0, max = 100000, value = 1),
    numericInput("max_iter", "Maximum number of iterations", min = 0, max = 100000, value = 100000),
    numericInput("max_parents", "Maximum number of parents allowed for a node", min = 0, max = 1000, value = 1000),
    numericInput("tabu_par", "TABU parameter for TABU algorithm", min = 0, max = 10000, value = 10),
    numericInput("alpha_par", "Type I error rate for independence tests", min = 0, max = 1, value = .05),
    selectInput("test_type", "Type of test for conditional tests", c("mi", "sp-mi", "mc-x2", "x2"), selected = "x2"),
    selectInput("debug_type", "Used for debugging the model", c(TRUE, FALSE), selected = FALSE),
    selectInput("fit_method", "Method for fitting Bayes Net", c("mle", "bayes", "hdir", "hard-em", "mle-cg", "hard-em-cg"), selected = "mle"),
    numericInput("iss_par", "Imaginary sample size", min = 0, max = 100000, value = 1),
    selectInput("imputation_type", "Method for imputing missing data when using 'hard-em' or 'harm-em-cg' estimation method", c("parents", "bayes-lw", "exact"), selected = "parents"),
    numericInput("fit_max_iter", "Maximum number of iterations for fitting Bayes Net", min = 0, max = 100000, value = 5),
    selectInput("score_type", "The network score type", c("loglik", "aic", "bic", "ebic", "bde", "bds", "bdj", "mbde", "bdla", "k2", "fnml", "qnml", "nal", "pnal"), selected = "loglik"),
    numericInput("network_iss", "Imaginary sample size for network score", min = 0, max = 100000, value = 1),
    numericInput("l_par", "Number of scores to average in bdla network type", min = 0, max = 100000, value = 5),
    numericInput("gamma_par", "Additional penalty in ebic score", min = 0, max = 1, value = .5),
    selectInput("prior_type", "Prior for bde, bds, bdj, mbde, or bdla network score types ", c(NULL, "uniform", "vsp", "marginal", "cs"), selected = NULL)
  ),
  nav_panel(
    title = "Creating the Structure of a Bayes Net Model",
    value_box(
      title = "Network Score",
      value = verbatimTextOutput("network")
      ),
    plotOutput("bn_dag")    
    ),
  nav_panel(
    title = "Fitting a Bayes Net Model",
    verbatimTextOutput("cond_prob")    
  ),
  nav_panel(
    title = "Queries for Events of Interest Based on Evidence",
    verbatimTextOutput("query_code"),
    value_box(
      title = "Query of Event Probability Based on Evidence",
      value = textOutput("query_result")
      )
  )
)

server <- function(input, output) {
  data <- reactive({
    infile <- input$file
      if (is.null(infile)) {return(NULL)}
      readr::read_csv(
        infile$datapath
        ) |>
        mutate(
          across(
            everything(),
            ~as.factor(.x)
            )
          ) |>
        as.data.frame()

      # if(train_test == "Yes"){
# 
      # } else{
      #   
      # }
    })

  wl <- reactive({
    infile <- input$user_wl

    if (is.null(infile)) return(NULL)
    read.table(infile$datapath)
  })

  bl <- reactive({
    infile <- input$user_bl

    if (is.null(infile)) return(NULL)
    read.table(infile$datapath)
  })

  # max_type <- reactive({
  #   if (input$max_type == "") {
  #     NULL 
  #   } else {
  #     input$max_type
  #   }
  # })

  # test_type <- reactive({
  #   if (input$test_type == "") {
  #     NULL 
  #   } else {
  #     input$test_type
  #   }
  # })

  bn <- reactive({
     req(data())

      sl_alg(
      data = data(),
      algorithm = input$struct_learn, 
      whitelist = wl(),
      blacklist = bl(),
      maximize =  input$max_type,
      restart = input$restart_par,
      perturb = input$perturb_par,
      max.iter = input$max_iter,
      maxp = input$max_parents,
      tabu = input$tabu_par,
      max.tabu = tabu,
      alpha = input$alpha_par,
      test = input$test_type,
      debug = as.logical(input$debug_type),
      seed = input$seed
      )
  })

  output$bn_dag <- renderPlot({
    req(bn()) 
    
    graphviz.plot(
      bn()
      )
  })

  dag <- reactive({
      req(data())
      req(bn())

      graph <- empty.graph(colnames(data()))
      arcs(graph) <- bn()$arcs
      graph
    })

  output$network <- reactive({
    req(dag())
    req(data())

    round(
      network_score(
        dag = dag(),
        data = data(),
        type = input$score_type,
        iss = input$network_iss,
        l = input$l_par,
        gamma = input$gamma_par,
        prior = input$prior_type,
        seed = input$seed
      ),
      2
    )
  })

  fit <- reactive({
    req(dag())
    req(data())
    
    cat_fit(
      dag = dag(),
      data = data(),
      method = input$fit_method,
      iss = input$iss_par,
      impute = input$imputation_type,
      max.iter = input$fit_max_iter,
      seed = input$seed
    )
  })

  output$cond_prob <- renderPrint({
    req(fit())

    print(fit())
  })

  output$query_result <- renderPrint({
    req(fit())
    
    set.seed(input$seed)
    round(
      query_fun(
      fit = fit(),
      event = input$event,
      evi = input$evidence
      ),
      3
    )*100
  })

  output$query_code <- renderText({
    req(input$event)
    req(input$evidence)
    
    paste0(
      "cpquery(fit = fit, \n",
      "        event = ", input$event, ",\n",
      "        evidence = ", input$evidence, ")\n"
    )
  })

}

# Run the application
shinyApp(ui, server)