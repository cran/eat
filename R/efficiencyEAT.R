#' @title Banker, Charnes and Cooper Programming Model with Output Orientation for an Efficiency Analysis Trees model
#'
#' @description Banker, Charnes and Cooper programming model with output orientation for an Efficiency Analysis Trees model.
#'
#' @param j Number of DMUs.
#' @param scores \code{matrix}. Empty matrix for scores.
#' @param x_k \code{data.frame}. Set of input variables.
#' @param y_k \code{data.frame} Set of output variables.
#' @param atreeTk \code{matrix} Set of "a" Pareto-coordinates.
#' @param ytreeTk \code{matrix} Set of predictions.
#' @param nX Number of inputs.
#' @param nY Number of outputs.
#' @param N_leaves Number of leaf nodes. 
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with efficiency scores.
EAT_BCC_out <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + 1, nrow = 1)
    objVal[1] <- 1
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + 1)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      add.constraint(lps, xt = c(0, atreeTk[, xi]), "<=",  rhs = x_k[d, xi])
    }
    for(yi in 1:nY)
    {
      add.constraint(lps, xt = c(- y_k[d, yi], ytreeTk[, yi]), ">=", rhs = 0)
    }
    
    # Constrain 2.3 - phi = 1
    add.constraint(lprec = lps, xt = c(0, rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + 1, type = c("binary"))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
}

#' @title Banker, Charnes and Cooper Programming Model with Input Orientation for an Efficiency Analysis Trees model
#'
#' @description Banker, Charnes and Cooper programming model with input orientation for an Efficiency Analysis Trees model.
#'
#' @param j Number of DMUs.
#' @param scores \code{matrix}. Empty matrix for scores.
#' @param x_k \code{data.frame}. Set of input variables.
#' @param y_k \code{data.frame} Set of output variables.
#' @param atreeTk \code{matrix} Set of "a" Pareto-coordinates.
#' @param ytreeTk \code{matrix} Set of predictions.
#' @param nX Number of inputs.
#' @param nY Number of outputs.
#' @param N_leaves Number of leaf nodes. 
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with efficiency scores.
EAT_BCC_in <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + 1, nrow = 1)
    objVal[1] <- 1
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + 1)
    lp.control(lps, sense = 'min')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      add.constraint(lps, xt = c(- x_k[d, xi], atreeTk[, xi]), "<=",  rhs = 0)
    }
    for(yi in 1:nY)
    {
      add.constraint(lps, xt = c(0, ytreeTk[, yi]), ">=", rhs = y_k[d, yi])
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(0, rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + 1, type = c("binary"))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Directional Distance Function Programming Model for an Efficiency Analysis Trees model
#'
#' @description Directional Distance Function for an Efficiency Analysis Trees model.
#'
#' @param j Number of DMUs.
#' @param scores \code{matrix}. Empty matrix for scores.
#' @param x_k \code{data.frame}. Set of input variables.
#' @param y_k \code{data.frame} Set of output variables.
#' @param atreeTk \code{matrix} Set of "a" Pareto-coordinates.
#' @param ytreeTk \code{matrix} Set of predictions.
#' @param nX Number of inputs.
#' @param nY Number of outputs.
#' @param N_leaves Number of leaf nodes. 
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with efficiency scores.
EAT_DDF <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  for(d in 1:j){
    
    objVal <- matrix(ncol = 1 + N_leaves, nrow = 1) # beta + lambdas
    objVal[1] <- 1 # beta
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + 1)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    { # beta[g-], a, <=, x
      add.constraint(lps, xt = c(x_k[d, xi], atreeTk[, xi]), "<=",  rhs = x_k[d, xi])
    }
    for(yi in 1:nY)
    { # - y, d(a), >=, beta[g+]
      add.constraint(lps, xt = c(- y_k[d, yi], ytreeTk[, yi]), ">=", rhs = y_k[d, yi])
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(0, rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + 1, type = c("binary"))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
}

#' @title Russell Model with Input Orientation for an Efficiency Analysis Trees model
#'
#' @description Russell Model with input orientation for an Efficiency Analysis Trees model.
#'
#' @param j Number of DMUs.
#' @param scores \code{matrix}. Empty matrix for scores.
#' @param x_k \code{data.frame}. Set of input variables.
#' @param y_k \code{data.frame} Set of output variables.
#' @param atreeTk \code{matrix} Set of "a" Pareto-coordinates.
#' @param ytreeTk \code{matrix} Set of predictions.
#' @param nX Number of inputs.
#' @param nY Number of outputs.
#' @param N_leaves Number of leaf nodes. 
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with efficiency scores.
EAT_RSL_in <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + nX, nrow = 1)
    objVal[1:nX] <- 1 / nX
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + nX)
    lp.control(lps, sense = 'min')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      vec <- c()
      vec[xi] <- - x_k[d, xi]
      vec[(1:nX)[- xi]] <- 0
      vec[(nX + 1):(nX + N_leaves)] <- atreeTk[, xi]
      
      add.constraint(lps, xt = vec, "<=",  rhs = 0)
    }
    
    for(yi in 1:nY)
    {
      add.constraint(lps, xt = c(rep(0, nX), ytreeTk[, yi]), ">=", rhs = y_k[d, yi])
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(rep(0, nX), rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + nX, type = c("binary"))
    
    # Constraint 2.5 -phi_m >= 1
    set.bounds(lps, columns = 1:nY, lower = rep(0, nY), upper=rep(1, nY))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Russell Model with Output Orientation for an Efficiency Analysis Trees model
#'
#' @description Russell Model with output orientation for an Efficiency Analysis Trees model.
#'
#' @param j Number of DMUs.
#' @param scores \code{matrix}. Empty matrix for scores.
#' @param x_k \code{data.frame}. Set of input variables.
#' @param y_k \code{data.frame} Set of output variables.
#' @param atreeTk \code{matrix} Set of "a" Pareto-coordinates.
#' @param ytreeTk \code{matrix} Set of predictions.
#' @param nX Number of inputs.
#' @param nY Number of outputs.
#' @param N_leaves Number of leaf nodes. 
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with efficiency scores.
EAT_RSL_out <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves) {
  for(d in 1:j){
    
    objVal <- matrix(ncol = N_leaves + nY, nrow = 1)
    objVal[1:nY] <- 1 / nY
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = N_leaves + nY)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      add.constraint(lps, xt = c(rep(0, nY), atreeTk[, xi]), "<=",  rhs = x_k[d, xi])
    }
    
    for(yi in 1:nY)
    {
      vec <- c()
      vec[yi] <- - y_k[d, yi]
      vec[(1:nY)[- yi]] <- 0
      vec[(nY + 1):(nY + N_leaves)] <- ytreeTk[, yi]
      
      add.constraint(lps, xt = vec, ">=", rhs = 0)
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(rep(0, nY), rep(1, N_leaves)), type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + nY, type = c("binary"))
    
    
    # Constraint 2.5 -phi_m >= 1
    set.bounds(lps, columns = 1:nY, lower = rep(1,nY), upper=rep(Inf, nY))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Weighted Additive Model for an Efficiency Analysis Trees model
#'
#' @description Weighted Additive Model for an Efficiency Analysis Trees model.
#'
#' @param j Number of DMUs.
#' @param scores \code{matrix}. Empty matrix for scores.
#' @param x_k \code{data.frame}. Set of input variables.
#' @param y_k \code{data.frame} Set of output variables.
#' @param atreeTk \code{matrix} Set of "a" Pareto-coordinates.
#' @param ytreeTk \code{matrix} Set of predictions.
#' @param nX Number of inputs.
#' @param nY Number of outputs.
#' @param N_leaves Number of leaf nodes. 
#' @param weights Character. \code{"MIP"} for Measure of Inefficiency Proportion or \code{"RAM"} for Range Adjusted Measure of Inefficiency.
#'
#' @importFrom lpSolveAPI make.lp lp.control set.objfn add.constraint set.type set.bounds get.objective
#'
#' @return A numerical vector with efficiency scores.
EAT_WAM <- function(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves, weights) {
  
  # Range for RAM measures
  if (weights == "RAM") {
    InputRanges <- apply(x_k, 2, max) - apply(x_k, 2, min)
    OutputRanges <- apply(y_k, 2, max) - apply(y_k, 2, min)
    
    ranges <- c(InputRanges, OutputRanges) / (nX + nY)
  }
  
  for(d in 1:j){
    
    objVal <- matrix(ncol = nX + nY + N_leaves, nrow = 1)
    
    if (weights == "MIP") {
      objVal[1:(nX + nY)] <- c(1 / x_k[d, ], 1 / y_k[d, ])
      
    } else if (weights == "RAM"){
      objVal[1:(nX + nY)] <- ranges

    }
    
    # structure for lpSolve
    lps <- make.lp(nrow = nX + nY, ncol = nX + nY + N_leaves)
    lp.control(lps, sense = 'max')
    set.objfn(lps, objVal)
    
    # constrain 2.1 and 2.2
    for(xi in 1:nX)
    {
      vec <- c()
      vec[xi] <- 1
      vec[(1:nX)[- xi]] <- 0
      vec[(nX + 1):(nX + nY)] <- 0
      vec[(nX + nY + 1):(nY + nX + N_leaves)] <- atreeTk[, xi]
      
      add.constraint(lps, xt = vec, "=",  rhs = x_k[d, xi])
    }
    
    for(yi in 1:nY)
    {
      vec <- c()
      vec[1:nX] <- 0
      vec[nX + yi] <- - 1
      vec[((nX + 1):(nX + nY))[- yi]] <- 0
      vec[(nX + nY + 1):(nY + nX + N_leaves)] <- ytreeTk[, yi]
      
      add.constraint(lps, xt = vec, "=", rhs = y_k[d, yi])
    }
    
    # Constrain 2.3 - lambda = 1
    add.constraint(lprec = lps, xt = c(rep(0, nY + nX), rep(1, N_leaves)), 
                   type = "=", rhs = 1)
    
    # Constrain 2.4
    set.type(lps, columns = 1:N_leaves + (nX + nY), type = c("binary"))
    
    solve(lps)
    scores[d, ] <- get.objective(lps)
  }
  
  return(scores)
  
}

#' @title Efficiency Scores computed through an Efficiency Analysis Trees model.
#'
#' @description This function computes the efficiency scores for each DMU through an Efficiency Analysis Trees model.
#' 
#' @param data \code{data.frame} or \code{matrix} containing the variables in the model.
#' @param x Column input indexes in \code{data}.
#' @param y Column output indexes in \code{data}.
#' @param object An \code{EAT} object.
#' @param scores_model Mathematical programming model to calculate scores. 
#' \itemize{
#' \item{\code{BCC.OUT} BCC model. Output-oriented. Efficiency level at 1.}
#' \item{\code{BCC.INP}  BCC model. Input-oriented. Efficiency level at 1.}
#' \item{\code{DDF}     Directional Distance Function. Efficiency level at 0.}
#' \item{\code{RSL.OUT} Russell model. Output-oriented. Efficiency level at 1.}
#' \item{\code{RSL.INP}  Russell model. Input-oriented. Efficiency level at 1.}
#' \item{\code{WAM.MIP} Weighted Additive Model. Measure of Inefficiency Proportions. Efficiency level at 0.}
#' \item{\code{WAM.RAM} Weighted Additive Model. Range Adjusted Measure of Inefficiency. Efficiency level at 0.}
#' }
#' @param digits Decimal units for scores.
#' @param FDH \code{logical}. If \code{TRUE}, FDH scores are also computed with the programming model selected in \code{scores_model}.
#' @param print.table \code{logical}. If \code{TRUE}, a summary descriptive table of the efficiency scores is displayed. 
#' @param na.rm \code{logical}. If \code{TRUE}, \code{NA} rows are omitted.
#'  
#' @importFrom dplyr summarise %>%
#' @importFrom stats median quantile sd
#' 
#' @export
#' 
#' @examples
#' 
#' \donttest{
#' simulated <- X2Y2.sim(N = 50, border = 0.2)
#' EAT_model <- EAT(data = simulated, x = c(1,2), y = c(3, 4))
#'
#' efficiencyEAT(data = simulated, x = c(1, 2), y = c(3, 4), object = EAT_model, 
#'               scores_model = "BCC.OUT", digits = 2, FDH = TRUE, print.table = TRUE,
#'               na.rm = TRUE)
#' }
#' 
#' @return A \code{data.frame} with the efficiency scores computed through an Efficiency Analysis Trees model. Optionally, a summary descriptive table of the efficiency scores can be displayed.
efficiencyEAT <- function(data, x, y, object, 
                          scores_model, digits = 3, FDH = TRUE,
                          print.table = FALSE, na.rm = TRUE) {
  
  # Possible errors
  
  if (!is(object, "EAT")) {
    stop(paste(deparse(substitute(object)), "must be an EAT object."))
    
  } else if (digits < 0) {
    stop(paste('digits =', digits, 'must be greater than 0.'))
    
  } else if (!scores_model %in% c("BCC.OUT", "BCC.INP", "DDF", "RSL.OUT", "RSL.INP", 
                                  "WAM.MIP","WAM.RAM")) {
    stop(paste(scores_model, "is not available. Please, check help(\"efficiencyEAT\")"))
  }
  
  data <- preProcess(data, x, y, na.rm = na.rm)
  
  x <- 1:(ncol(data) - length(y))
  y <- (length(x) + 1):ncol(data)
  
  train_names <- c(object[["data"]][["input_names"]], object[["data"]][["output_names"]])
  
  # Possible errors
  
  if (!identical(sort(train_names), sort(names(data)))) {
    stop("Different variable names in training and data.")
  }
  
  j <- nrow(data)
  scores <- matrix(nrow = j, ncol = 1)
  x_k <- as.matrix(data[, x])
  y_k <- as.matrix(data[, y])
  nX <- length(x)
  nY <- length(y)
  
  atreeTk <- object[["model"]][["a"]]
  ytreeTk <- object[["model"]][["y"]]
  N_leaves <- object[["model"]][["leaf_nodes"]]
  
  if (scores_model == "BCC.OUT"){
    scores <- EAT_BCC_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_BCC_OUT"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_BCC_out(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_BCC_OUT"
    }

  } else if (scores_model == "BCC.INP"){
    scores <- EAT_BCC_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_BCC_INP"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_BCC_in(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_BCC_INP"
    }

  } else if (scores_model == "DDF"){
    scores <- EAT_DDF(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_DDF"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_DDF(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_DDF"
    }

  } else if (scores_model == "RSL.OUT"){
    scores <- EAT_RSL_out(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_RSL_OUT"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_RSL_out(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_RSL_OUT"
    }

  } else if (scores_model == "RSL.INP"){
    scores <- EAT_RSL_in(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves)
    EAT_model <- "EAT_RSL_INP"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_RSL_in(j, scores, x_k, y_k, x_k, y_k, nX, nY, j)
      FDH_model <- "FDH_RSL_INP"
    }

  } else if (scores_model == "WAM.MIP"){
    scores <- EAT_WAM(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves, "MIP")
    EAT_model <- "EAT_WAM_MIP"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_WAM(j, scores, x_k, y_k, x_k, y_k, nX, nY, j, "MIP")
      FDH_model <- "FDH_WAM_MIP"
    }
    
  } else if (scores_model == "WAM.RAM") {
    scores <- EAT_WAM(j, scores, x_k, y_k, atreeTk, ytreeTk, nX, nY, N_leaves, "RAM")
    EAT_model <- "EAT_WAM_RAM"
    
    if (FDH == TRUE){
      scores_FDH <- EAT_WAM(j, scores, x_k, y_k, x_k, y_k, nX, nY, j, "RAM")
      FDH_model <- "FDH_WAM_RAM"
    }
  }

  scores <- as.data.frame(scores)
  names(scores) <- EAT_model
  rownames(scores) <- row.names(data)
  
  descriptive <- scores %>%
    summarise("Model" = "EAT",
              "Mean" = round(mean(scores[, 1]), digits),
              "Std. Dev." = round(sd(scores[, 1]), digits),
              "Min" = round(min(scores[, 1]), digits),
              "Q1" = round(quantile(scores[, 1])[[2]], digits),
              "Median" = round(median(scores[, 1]), digits),
              "Q3" = round(quantile(scores[, 1])[[3]], digits),
              "Max" = round(max(scores[, 1]), digits)
              )
  
  if (FDH == TRUE){
    
    scores_FDH <- as.data.frame(scores_FDH)
    names(scores_FDH) <- FDH_model
    rownames(scores_FDH) <- row.names(data)
    
    descriptive[2, ] <- scores_FDH %>%
      summarise("Model" = "FDH",
                "Mean" = round(mean(scores_FDH[, 1]), digits),
                "Std. Dev." = round(sd(scores_FDH[, 1]), digits),
                "Min" = round(min(scores_FDH[, 1]), digits),
                "Q1" = round(quantile(scores_FDH[, 1])[[2]], digits),
                "Median" = round(median(scores_FDH[, 1]), digits),
                "Q3" = round(quantile(scores_FDH[, 1])[[3]], digits),
                "Max" = round(max(scores_FDH[, 1]), digits)
      )
    
    scores_df <- cbind(data, round(scores, digits), round(scores_FDH, digits))
    
    if (print.table == TRUE) {
      print(descriptive, row.names = FALSE)
      cat("\n")
    }
    
    return(scores_df[, c(ncol(scores_df) - 1, ncol(scores_df))])
    
  } else {
    
    scores_df <- cbind(data, round(scores, digits))
    
    if (print.table == TRUE) {
      print(descriptive, row.names = FALSE)
      cat("\n") 
    }
    
    return(round(scores_df[, ncol(scores_df)], digits))
  }
}