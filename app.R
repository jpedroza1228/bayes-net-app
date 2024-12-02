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

sl_alg <- function(
  data,
  algorithm = c("hc", "tabu", "rsmax2", "mmhc", "h2pc"), 
  whitelist = NULL,
  blacklist = NULL,
  maximize =  NULL, 
  restart = 0,
  perturb = 1,
  max.iter = Inf,
  maxp = Inf,
  tabu = 10,
  max.tabu = tabu,
  alpha = .05,
  test = NULL, 
  restrict.args = list(),
  maximize.args = list(),
  debug = FALSE,
  seed = NULL
  ){
    set.seed(seed)

    if(algorithm == "hc"){
      bn <- hc(data, whitelist = whitelist, blacklist = blacklist, restart = restart, perturb = perturb, max.iter = max.iter, maxp = maxp, debug = debug)
    } else
    if(algorithm == "tabu"){
      bn <- tabu(data, whitelist = whitelist, blacklist = blacklist, tabu = tabu, max.tabu = tabu, max.iter = max.iter, maxp = maxp, debug = debug)
    } else
    if(algorithm == "mmhc"){
      bn <- mmhc(data, whitelist = whitelist, blacklist = blacklist, restrict.args = list(alpha = alpha, test = test), maximize.args = list(), debug = debug)
    } else
    if(algorithm == "h2pc"){
      bn <- h2pc(data, whitelist = whitelist, blacklist = blacklist, restrict.args = list(alpha = alpha, test = test), maximize.args = list(), debug = debug)
    } else
    if(algorithm == "rsmax2"){
      if(maximize == "hc"){
        bn <- rsmax2(data, maximize = "hc", whitelist = whitelist, blacklist = blacklist, restrict.args = list(alpha = alpha, test = test), maximize.args = list(restart = restart), debug = debug)
      } else
      if(maximize == "tabu"){
        bn <- rsmax2(data, maximize = "tabu", whitelist = whitelist, blacklist = blacklist, restrict.args = list(alpha = alpha, test = test), maximize.args = list(tabu = tabu), debug = debug)
      } else{
        bn <- rsmax2(data, whitelist = whitelist, blacklist = blacklist, restrict.args = list(alpha = alpha, test = test), maximize.args = list(restart = restart), debug = debug)
      }
    }

    return(bn)
}

cat_fit <- function(
  dag,
  data,
  method = c("mle", "bayes", "hdir", "hard-em", "mle-cg", "hard-em-cg"),
  iss = 1,
  impute = c("parents", "bayes-lw", "exact"),
  max.iter = 5,
  seed = NULL
){
  set.seed(seed)

  method <- match.arg(method)

  # if (!method %in% c("hard-em", "hard-em-cg") && !is.null(impute)) {
  #   stop("The 'impute' argument is only supported for 'hard-em' and 'hard-em-cg' methods.")
  # }

  if(method == "mle"){
    fit <- bn.fit(dag, data = data, method = method)
  } else
  if(method == "bayes"){
    fit <- bn.fit(dag, data = data, method = method, iss = iss)
  } else
  if(method == "hdir"){
    fit <- bn.fit(dag, data = data, method = method)
  } else
  if(method == "hard-em"){
    fit <- bn.fit(dag, data = data, method = method, impute = impute, max.iter = max.iter)
  } else
  if(method == "mle-cg"){
    fit <- bn.fit(dag, data = data, method = method)
  } else
  if(method == "hard-em-cg"){
    fit <- bn.fit(dag, data = data, method = method, impute = impute, max.iter = max.iter)
  }
  return(fit)
}

network_score <- function(
  dag,
  data,
  type = c("loglik", "aic", "bic", "ebic", "bde", "bds", "bdj", "mbde", "bdla", "k2", "fnml", "qnml", "nal", "pnal"),
  iss = 1,
  l = 5,
  gamma = .5,
  prior = c("uniform", "vsp", "marginal", "cs"),
  seed = NULL
){
  set.seed(seed)

  type <- match.arg(type)

  # if (!type %in% c("bde", "bds", "bdj", "mbde", "bdla") && !is.null(prior)) {
  #   stop("The 'prior' argument is only supported for 'bde', 'bds', 'bdj', 'mbde', 'bdla' network score types.")
  # }

  # if(type != "ebic" && !is.null(gamma)){
  #   stop("The 'gamma' argument is only supported for the 'ebic' network score type.")
  # }

  if(type == "loglik"){
    bn_score <- bnlearn::score(dag, data, type = type)
  } else
  if(type == "aic"){
    bn_score <- bnlearn::score(dag, data, type = type)
  } else
  if(type == "bic"){
    bn_score <- bnlearn::score(dag, data, type = type)
  } else
  if(type == "ebic"){
    bn_score <- bnlearn::score(dag, data, type = type, gamma = gamma)
  } else
  if(type == "bde"){
    bn_score <- bnlearn::score(dag, data, type = type, iss = iss, prior = prior)
  } else
  if(type == "bds"){
    bn_score <- bnlearn::score(dag, data, type = type, iss = iss, prior = prior)
  } else
  if(type == "bdj"){
    bn_score <- bnlearn::score(dag, data, type = type, iss = iss, prior = prior)
  } else
  if(type == "mbde"){
    bn_score <- bnlearn::score(dag, data, type = type, iss = iss, prior = prior)
  } else
  if(type == "bdla"){
    bn_score <- bnlearn::score(dag, data, type = type, l = l, prior = prior)
  } else
  if(type == "k2"){
    bn_score <- bnlearn::score(dag, data, type = type)
  } else
  if(type == "fnml"){
    bn_score <- bnlearn::score(dag, data, type = type)
  } else
  if(type == "qnml"){
    bn_score <- bnlearn::score(dag, data, type = type)
  } else
  if(type == "nal"){
    bn_score <- bnlearn::score(dag, data, type = type)
  } else
  if(type == "pnal"){
    bn_score <- bnlearn::score(dag, data, type = type)
  }
  return(bn_score)
}

query_fun <- function(fit, event, evi){
  fit <- fit
  event <- event
  evi <- evi

  prob <- eval(parse(text = paste("cpquery(fit,",event,",",evi,")")))

  return(prob)
}
# START SHINY APP

ui <- page_navbar(
  theme = bs_theme(preset = "minty"),
  title = "Building a Discrete Bayesian Network for Inference",
  sidebar = sidebar(
    title = "Parameters to change for Bayes Net Structure, Model, & Network Score",
    width = 400,
    "All analyses for the Bayes Net models can be found on the bnlearn website (https://www.bnlearn.com/)",
    numericInput("seed", "Set a seed value for reproducibility", value = 12345),
    hr(),
    radioButtons("train_test", "Do you want your data split into a training and testing set?", c("Yes", "No"), selected = "No"),
    fileInput("file", "Choose CSV File. Example dataset can be found at https://github.com/jpedroza1228/bayes-net-app/blob/main/R/cat_cars.csv", accept = c("text/csv", "text/comma-separated-values, .csv")),
    fileInput("user_wl", "A matrix with desired edges. Referred to as a whitelist. Example code can be found at https://github.com/jpedroza1228/bayes-net-app/blob/main/R/lists_creation.r", accept = c("text", "text, .txt")),
    fileInput("user_bl", "A matrix with unwanted edges. Referred to as a blacklist. Example code can be found at https://github.com/jpedroza1228/bayes-net-app/blob/main/R/lists_creation.r", accept = c("text", "text, .txt")),
    numericInput("train_prop", "Proportion of data for your training set", min = .5, max = .99, value = .8),
    hr(),
    textInput("event", "Include event of interest. Write out the event as: ({Variable of Interest} == '1'), (e.g., (loan_status == '1'))"),
    textInput("evidence", "Include conditions of evidence in prediction. Write out the evidence as: ({Variable of Interest} == '0'), (e.g., (loan_intent %in% c('EDUCATION', 'MEDICAL'))"),
    hr(),
    selectInput("struct_learn", "Structural learning algorithm to create DAG", c("hc", "tabu", "mmhc", "h2pc", "rsmax2"), selected = "hc"),
    selectInput("max_type", "What score-based algorithm to maximize for hybrid learning algorithm", c("hc", "tabu"), selected = "hc"),
    numericInput("restart_par", "Restart parameter for Hill-Climb algorithm", min = 0, max = 100000, value = 0),
    numericInput("perturb_par", "Perturb parameter for TABU algorithm", min = 0, max = 100000, value = 1),
    numericInput("max_iter", "Maximum number of iterations", min = 0, max = 100000, value = 1000),
    numericInput("max_parents", "Maximum number of parents allowed for a node", min = 0, max = 1000, value = 100),
    numericInput("tabu_par", "TABU parameter for TABU algorithm", min = 0, max = 10000, value = 10),
    numericInput("alpha_par", "Type I error rate for independence tests", min = 0, max = 1, value = .05),
    selectInput("test_type", "Type of test for conditional tests", c("mi", "sp-mi", "mc-x2", "x2"), selected = "x2"),
    selectInput("debug_type", "Used for debugging the model", c(TRUE, FALSE), selected = FALSE),
    hr(),
    selectInput("fit_method", "Method for fitting Bayes Net", c("mle", "bayes", "hdir", "hard-em", "mle-cg", "hard-em-cg"), selected = "mle"),
    numericInput("iss_par", "Imaginary sample size", min = 0, max = 100000, value = 1),
    selectInput("imputation_type", "Method for imputing missing data when using 'hard-em' or 'harm-em-cg' estimation method", c("parents", "bayes-lw", "exact"), selected = "parents"),
    numericInput("fit_max_iter", "Maximum number of iterations for fitting Bayes Net", min = 0, max = 100000, value = 5),
    hr(),
    selectInput("score_type", "The network score type", c("loglik", "aic", "bic", "ebic", "bde", "bds", "bdj", "mbde", "bdla", "k2", "fnml", "qnml", "nal", "pnal"), selected = "loglik"),
    numericInput("network_iss", "Imaginary sample size for network score", min = 0, max = 100000, value = 1),
    numericInput("l_par", "Number of scores to average in bdla network type", min = 0, max = 100000, value = 5),
    numericInput("gamma_par", "Additional penalty in ebic score", min = 0, max = 1, value = .5),
    selectInput("prior_type", "Prior for bde, bds, bdj, mbde, or bdla network score types ", c("uniform", "vsp", "marginal", "cs")),
    hr(),
    selectInput("predict_node", "Node/Variable of interest to predict (Currently supporting binary outcomes)", ""),
    selectInput("predict_method", "Method for predictions", c("parents", "bayes-lw", "exact"))
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
        ),
      value_box(
        title = "Accuracy of the Model",
        value = verbatimTextOutput("accuracy")
      ),
      verbatimTextOutput("confusion_matrix")
  )
)

server <- function(input, output, session) {

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
    })

  observeEvent(input$file, {
    req(data())

    # data <- reactive({
    # infile <- input$file
    #   if (is.null(infile)) {return(NULL)}
    #   readr::read_csv(
    #     infile$datapath
    #     ) |>
    #     mutate(
    #       across(
    #         everything(),
    #         ~as.factor(.x)
    #         )
    #       ) |>
    #     as.data.frame()
    # })

    updateSelectInput(
      session,
      inputId = "predict_node",
      label = "Node/Variable of interest to predict (Currently supporting binary outcomes)",
      choices  = colnames(data()))
  })

  data_train <- reactive({
    req(data())

    data() |>
      slice_sample(prop = input$train_prop)
  })

  data_test <- reactive({
    req(data())
    req(data_train())

    anti_join(data(), data_train())
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

  bn <- reactive({
     
     if(input$train_test == "Yes"){
      req(data_train())

      sl_alg(
      data = data_train(),
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
     } else{
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
     }
  })

  output$bn_dag <- renderPlot({
    req(bn()) 
    
    graphviz.plot(
      bn()
      )
  })

  dag <- reactive({

    if(input$train_test == "Yes"){
      req(data_train())
      req(bn())

      graph <- empty.graph(colnames(data_train()))
      arcs(graph) <- bn()$arcs
      graph
    } else{
      req(data())
      req(bn())

      graph <- empty.graph(colnames(data()))
      arcs(graph) <- bn()$arcs
      graph
    }
    })

  output$network <- reactive({

    if(input$train_test == "Yes"){
      req(dag())
      req(data_train())

      round(
      network_score(
        dag = dag(),
        data = data_train(),
        type = input$score_type,
        iss = input$network_iss,
        l = input$l_par,
        gamma = input$gamma_par,
        prior = input$prior_type,
        seed = input$seed
      ),
      2
    )
    } else{
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
    }
  })

  fit <- reactive({

    if(input$train_test == "Yes"){
      req(dag())
      req(data_train())

      cat_fit(
      dag = dag(),
      data = data_train(),
      method = input$fit_method,
      iss = input$iss_par,
      impute = input$imputation_type,
      max.iter = input$fit_max_iter,
      seed = input$seed
    )
    } else{
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
    }    
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


  predictions <- reactive({
    req(fit())
    node <- input$predict_node

    if(input$train_test == "Yes"){
      req(data_test())

      set.seed(input$seed)
      predict(
        fit(),
        node = as.character(node),
        data = data_test(),
        method = input$predict_method
      )
    } else{
      req(data())

      warning("Full data was used for these predictions, not new data.")
      set.seed(input$seed)
      predict(
        fit(),
        node = as.character(node),
        data = data(),
        method = input$predict_method
      )
    }
  })


  output$confusion_matrix <- renderPrint({
    
    if(input$train_test == "Yes"){
      req(data_test())
      req(predictions())

      round(
      prop.table(
        table(
          data_test()[[input$predict_node]],
          predictions()
        )
      ),
      2
    )
    } else{
        req(data())
        req(predictions())

        round(
            prop.table(
              table(
                data()[[input$predict_node]],
                predictions()
              )
            ),
            2
          )
        }
  })

  output$accuracy <- reactive({
    
    if(input$train_test == "Yes"){
      req(data_test())
      req(predictions())

      correct_pred <- sum(predictions() == data_test()[[input$predict_node]])
      total_pred <- length(predictions())

      round(correct_pred/total_pred, 2)
    } else{
      req(data())
      req(predictions())

      correct_pred <- sum(predictions() == data()[[input$predict_node]])
      total_pred <- length(predictions())

      round(correct_pred/total_pred, 2)
    }
})

}

# Run the application
shinyApp(ui, server)