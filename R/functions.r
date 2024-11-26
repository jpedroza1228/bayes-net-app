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