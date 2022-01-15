## ---- include = FALSE---------------------------------------------------------
is_check <- ("CheckExEnv" %in% search()) || any(c("_R_CHECK_TIMINGS_",
             "_R_CHECK_LICENSE_") %in% names(Sys.getenv()))

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  warning = FALSE,
  message = FALSE,
  eval = !is_check
)

## ----table, echo = FALSE------------------------------------------------------
library(dplyr)

functions <- data.frame("Purpose" = c(rep("Model", 2),
                                      rep("Summarize", 5),
                                      rep("Tune", 2), 
                                      rep("Graph", 3),
                                      rep("Calculate efficiency scores", 3), 
                                      rep("Graph efficiency scores", 2),
                                      rep("Predict", 1), 
                                      rep("Rank", 2),
                                      rep("Simulation", 2)), 
                        "Function name" = c("EAT", "RFEAT",
                                            "print", "summary", "EAT_size", "EAT_frontier_levels", "EAT_leaf_stats",
                                            "bestEAT", "bestRFEAT", 
                                            "frontier", "plotEAT", "plotRFEAT",
                                            "efficiencyEAT", "efficiencyCEAT", "efficiencyRFEAT",
                                            "efficiencyDensity", "efficiencyJitter",
                                            "predict",
                                            "rankingEAT", "rankingRFEAT",
                                            "Y1.sim", "X2Y2.sim"), 
                        "Usage" = c("It generates a pruned Efficiency Analysis Trees model and returns an `EAT` object.",
                                    "It generates a Random Forest for Efficiency Analysis Trees model and returns a `RFEAT` object",
                                    "Print method for an `EAT` or a `RFEAT` object.",
                                    "Summary method for an `EAT` object.",
                                    "For an `EAT` object. It returns the number of leaf nodes.",
                                    "For an `EAT` object. It returns the frontier output levels at the leaf nodes.",
                                    "For an `EAT` object. It returns a descriptive summary statistics table for each output variable calculated from the leaf nodes observations.",
                                    "For an EAT model. Hyperparameter tuning.",
                                    "For an RFEAT model Hyperparameter tuning.",
                                    "For an `EAT` object. It plots the estimated frontier in a two-dimensional scenario (1 input and 1 output).",
                                    "For an `EAT` object. It plots the tree structure.",
                                    "For an `RFEAT` object. It plots a line plot graph with the Out-of-Bag (OOB) error for a forest consisting of k trees.",
                                    "It calculates the efficiency scores through an EAT (and FDH) model.",
                                    "It calculates the efficiency scores through a convexified EAT (and DEA) model.",
                                    "It calculates the efficiency scores through a RFEAT (and FDH) model.",
                                    "Density plot for a `data.frame` of efficiency scores (EAT, FDH, CEAT, DEA and RFEAT are available).",
                                    "For an `EAT` object. Jitter plot for a vector of efficiency scores calculated through an EAT /CEAT model. ",
                                    "Predict method for an `EAT` or a `RFEAT` object.",
                                    "For an `EAT` object. It calculates variable importance scores.",
                                    "For an `RFEAT` object. It calculates variable importance scores.",
                                    "It simulates a data set in 1 output scenario. 1, 3, 6, 9, 12 and 15 inputs can be generated.",
                                    "It simulates a data set in 2 outputs and 2 inputs scenario.")
)

kableExtra::kable(functions) %>%
  kableExtra::kable_styling("striped", full_width = F) %>%
  kableExtra::collapse_rows(columns = 1, valign = "middle")

## ----seed---------------------------------------------------------------------
# We save the seed for reproducibility of the results
set.seed(120)

## ----library------------------------------------------------------------------
library(eat)
data("PISAindex")

## ----EAT, eval = FALSE--------------------------------------------------------
#  EAT(data, x, y,
#      fold = 5,
#      numStop = 5,
#      max.depth = NULL,
#      max.leaves = NULL,
#      na.rm = TRUE)

## ----single.output, collapse = FALSE------------------------------------------
single_model <- EAT(data = PISAindex, 
                    x = 15, # input 
                    y = 3) # output

## ----print.single.output, collapse = FALSE------------------------------------
print(single_model)

## ----summary.single.output, collapse = FALSE----------------------------------
summary(single_model)

## ----size.single.output, collapse = FALSE-------------------------------------
EAT_size(single_model)

## ----frt.single.output, collapse = FALSE--------------------------------------
EAT_frontier_levels(single_model)

## ----perf.single.output, collapse = FALSE-------------------------------------
EAT_leaf_stats(single_model)

## ----node.charac, collapse = FALSE--------------------------------------------
single_model[["tree"]][[5]]

## ----table2, echo = FALSE-----------------------------------------------------
types <- data.frame("Variable" = c("Independent variables (inputs)", "Dependent variables (outputs)"),
                    "Integer" = c("x", "x"),
                    "Numeric" = c("x", "x"),
                    "Factor" = c("", ""),
                    "Ordered factor" = c("x", ""))

kableExtra::kable(types, align = rep("c", 5)) %>%
  kableExtra::kable_styling("striped", full_width = F)

## ----continent----------------------------------------------------------------
# Transform Continent to Factor
PISAindex_factor_Continent <- PISAindex
PISAindex_factor_Continent$Continent <- as.factor(PISAindex_factor_Continent$Continent)

## ----GDP_PPP_category, collapse = FALSE---------------------------------------
# Cateogirze GDP_PPP into 4 groups: Low, Medium, High, Very High.  
PISAindex_GDP_PPP_cat <- PISAindex

PISAindex_GDP_PPP_cat$GDP_PPP_cat <- cut(PISAindex_GDP_PPP_cat$GDP_PPP,
                                         breaks = c(0, 16.686, 31.419, 47.745, Inf),
                                         include.lowest = T,
                                         labels = c("Low", "Medium", "High", "Very high"))

class(PISAindex_GDP_PPP_cat$GDP_PPP_cat) # "factor" --> error

# It is necessary to indicate order = TRUE, before applying the EAT function

PISAindex_GDP_PPP_cat$GDP_PPP_cat <- factor(PISAindex_GDP_PPP_cat$GDP_PPP_cat, 
                                            order = TRUE)

class(PISAindex_GDP_PPP_cat$GDP_PPP_cat) # "ordered" "factor" --> correct

## ----categorized_model--------------------------------------------------------
categorized_model <- EAT(data = PISAindex_GDP_PPP_cat, 
                         x = c(15, 19), 
                         y = 3) 

## ----frontier, eval = FALSE---------------------------------------------------
#  frontier(object,
#           FDH = FALSE,
#           observed.data = FALSE,
#           observed.color = "black",
#           pch = 19,
#           size = 1,
#           rwn = FALSE,
#           max.overlaps = 10)

## ----single.output.frontier, fig.width = 7.2, fig.height = 6------------------
frontier <- frontier(object = single_model,
                     FDH = TRUE, 
                     observed.data = TRUE,
                     rwn = TRUE)

plot(frontier)

## ----single.output.max.depth, collapse = FALSE--------------------------------
single_model_md <- EAT(data = PISAindex, 
                       x = 15,  
                       y = 3,
                       max.leaves = 5) 

## ----size.single.output_md, collapse = FALSE----------------------------------
EAT_size(single_model_md)

## ----pred.single.output_md, collapse = FALSE----------------------------------
single_model_md[["model"]][["y"]]

## ----single.output.frontier_md, fig.width = 7.2, fig.height = 6---------------
frontier_md <- frontier(object = single_model_md,
                        observed.data = TRUE)

plot(frontier_md)

## ----multioutput.scenario, collapse = FALSE-----------------------------------
multioutput_model <- EAT(data = PISAindex, 
                         x = 6:18,  
                         y = 3:5
                         ) 

## ----ranking, eval = FALSE----------------------------------------------------
#  rankingEAT(object,
#             barplot = TRUE,
#             threshold = 70,
#             digits = 2)

## ----multioutput.importance, fig.width = 7.2, fig.height = 6------------------
rankingEAT(object = multioutput_model,
           barplot = TRUE,
           threshold = 70,
           digits = 2)

## ----plotEAT, eval = FALSE----------------------------------------------------
#  plotEAT(object)

## ----model.graph1, collapse = FALSE-------------------------------------------
reduced_model1 <- EAT(data = PISAindex, 
                      x = c(6, 7, 8, 12, 17), 
                      y = 3:5, 
                      numStop = 9)

## ----graph1, fig.dim = c(8.4, 7.5)--------------------------------------------
plotEAT(object = reduced_model1)

# Leaf nodes: 8
# Depth: 6

## ----model.graph2, collapse = FALSE-------------------------------------------
reduced_model2 <- EAT(data = PISAindex, 
                      x = c(6, 7, 8, 12, 17), 
                      y = 3:5, 
                      numStop = 9,
                      max.depth = 5)

## ----graph2, fig.dim = c(8.4, 7.5)--------------------------------------------
plotEAT(object = reduced_model2)

# Leaf nodes: 6
# Depth: 5

## ----model.graph3, collapse = FALSE-------------------------------------------
reduced_model3 <- EAT(data = PISAindex, 
                      x = c(6, 7, 8, 12, 17), 
                      y = 3:5, 
                      numStop = 9,
                      max.leaves = 4)

## ----graph3, fig.dim = c(8.4, 7.5)--------------------------------------------
plotEAT(object = reduced_model3)

# Leaf nodes: 4
# Depth: 3

## ----training_test------------------------------------------------------------
n <- nrow(PISAindex) # Observations in the dataset
selected <- sample(1:n, n * 0.7) # Training indexes
training <- PISAindex[selected, ] # Training set
test <- PISAindex[- selected, ] # Test set

## ----bestEAT, eval = FALSE----------------------------------------------------
#  bestEAT(training, test,
#          x, y,
#          numStop = 5,
#          fold = 5,
#          max.depth = NULL,
#          max.leaves = NULL,
#          na.rm = TRUE)

## ----eat.tuning, collapse = FALSE---------------------------------------------
bestEAT(training = training, 
        test = test,
        x = c(6, 7, 8, 12, 17),
        y = 3:5,
        numStop = c(3, 5, 7),
        fold = c(5, 7))

## ----bestEAT_model, collapse = FALSE------------------------------------------
bestEAT_model <- EAT(data = PISAindex,
                     x = c(6, 7, 8, 12, 17),
                     y = 3:5,
                     numStop = 7,
                     fold = 5)

## ----summary.bestEAT_model, collapse = FALSE----------------------------------
summary(bestEAT_model)

## ----efficiencyEAT, eval = FALSE----------------------------------------------
#  efficiencyEAT(data, x, y,
#                object,
#                score_model,
#                digits = 3,
#                FDH = TRUE,
#                print.table = FALSE,
#                na.rm = TRUE)

## ----scoresEAT, collapse = FALSE----------------------------------------------
# single_model <- EAT(data = PISAindex, x = 15, y = 3)

scores_EAT <- efficiencyEAT(data = PISAindex,
                            x = 15, 
                            y = 3,
                            object = single_model, 
                            scores_model = "BCC.OUT",
                            digits = 3,
                            FDH = TRUE,
                            print.table = TRUE,
                            na.rm = TRUE)

scores_EAT

## ----scoresEAT2, collapse = FALSE---------------------------------------------
scores_EAT2 <- efficiencyEAT(data = PISAindex,
                             x = 15, 
                             y = 3,
                             object = single_model, 
                             scores_model = "BCC.INP",
                             digits = 3,
                             FDH = TRUE,
                             print.table = FALSE,
                             na.rm = TRUE)

scores_EAT2

## ----efficiencyCEAT, eval = FALSE---------------------------------------------
#  efficiencyCEAT(data, x, y,
#                 object,
#                 score_model,
#                 digits = 3,
#                 DEA = TRUE,
#                 print.table = FALSE,
#                 na.rm = TRUE)

## ----scoresCEAT, collapse = FALSE---------------------------------------------
scores_CEAT <- efficiencyCEAT(data = PISAindex,
                              x = 15, 
                              y = 3,
                              object = single_model, 
                              scores_model = "BCC.INP",
                              digits = 3,
                              DEA = TRUE,
                              print.table = TRUE,
                              na.rm = TRUE)

scores_CEAT

## ----efficiency_jitter, eval = FALSE------------------------------------------
#  efficiencyJitter(object, df_scores,
#                   scores_model,
#                   lwb = NULL, upb = NULL)

## ----jitter_single, collapse = FALSE, fig.width = 7.2, fig.height = 5---------
efficiencyJitter(object = single_model,
                 df_scores = scores_EAT$EAT_BCC_OUT,
                 scores_model = "BCC.OUT",
                 lwb = 1.2)

## ----jitter_single2, collapse = FALSE, fig.width = 7.2, fig.height = 5--------
efficiencyJitter(object = single_model,
                 df_scores = scores_EAT2$EAT_BCC_INP,
                 scores_model = "BCC.INP",
                 upb = 0.65)

## ----frontier_comparar, fig.width = 7.2, fig.height = 6, fig.align = 'center'----
# frontier <- frontier(object = single_model, FDH = TRUE, 
                     # observed.data = TRUE, rwn = TRUE)

plot(frontier)

## ----efficiency_density, eval = FALSE-----------------------------------------
#  efficiencyDensity(df_scores,
#                    model = c("EAT", "FDH"))
#  

## ----density_single, collapse = FALSE, fig.width = 7.2, fig.height = 6, fig.align = 'center'----
efficiencyDensity(df_scores = scores_EAT,
                  model = c("EAT", "FDH"))

efficiencyDensity(df_scores = scores_CEAT,
                  model = c("CEAT", "DEA"))


## ----cursed.scores, collapse = FALSE------------------------------------------
# multioutput_model <- EAT(data = PISAindex, x = 6:18, y = 3:5) 

cursed_scores <- efficiencyEAT(data = PISAindex,
                               x = 6:18, 
                               y = 3:5,
                               object = multioutput_model,
                               scores_model = "BCC.OUT",
                               digits = 3,
                               print.table = TRUE,
                               FDH = TRUE)

## ----cursed.density, collapse = FALSE, fig.width = 7.2, fig.height = 6, fig.align = 'center'----
efficiencyDensity(df_scores = cursed_scores, model = c("EAT", "FDH"))

## ----RF, eval = FALSE---------------------------------------------------------
#  RFEAT(data, x, y,
#        numStop = 5, m = 50,
#        s_mtry = "BRM",
#        na.rm = TRUE)

## ----RFmodel------------------------------------------------------------------
forest <- RFEAT(data = PISAindex, 
                x = 6:18,
                y = 3:5,
                numStop = 5, 
                m = 30,
                s_mtry = "BRM",
                na.rm = TRUE)

## ----print.RFEAT, collapse = FALSE--------------------------------------------
print(forest)

## ----plot.RFEAT, collapse = FALSE, fig.width = 7.2, fig.height = 6------------
plotRFEAT(forest)

## ----rankingRFEAT, eval = FALSE-----------------------------------------------
#  rankingRFEAT(object,
#               barplot = TRUE,
#               digits = 2)

## ----RFmodel2-----------------------------------------------------------------
forestReduced <- RFEAT(data = PISAindex, 
                       x = c(6, 7, 8, 12, 17), 
                       y = 3:5,
                       numStop = 5, 
                       m = 30,
                       s_mtry = "BRM",
                       na.rm = TRUE)

## ----rankingRFEAT_forestReduced, fig.width = 7.2, fig.height = 6--------------
rankingRFEAT(object = forestReduced, 
             barplot = TRUE,
             digits = 2)

## ----bestRFEAT, eval = FALSE--------------------------------------------------
#  bestRFEAT(training, test,
#            x, y,
#            numStop = 5,
#            m = 50,
#            s_mtry = c("5", "BRM"),
#            na.rm = TRUE)

## ----tuning.bestRFEAT, collapse = FALSE---------------------------------------
# n <- nrow(PISAindex)
# selected <- sample(1:n, n * 0.7)
# training <- PISAindex[selected, ]
# test <- PISAindex[- selected, ]

bestRFEAT(training = training,
          test = test,
          x = c(6, 7, 8, 12, 17),
          y = 3:5,
          numStop = c(5, 10), # set of possible numStop
          m = c(20, 30), # set of possible m
          s_mtry = c("1", "BRM")) # set of possible s_mtry 

## ----bestModelRFEAT, collapse = FALSE-----------------------------------------
bestRFEAT_model <- RFEAT(data = PISAindex,
                         x = c(6, 7, 8, 12, 17),
                         y = 3:5,
                         numStop = 5,
                         m = 20,
                         s_mtry = "BRM")

## ----eff_scores, eval = FALSE-------------------------------------------------
#  efficiencyRFEAT(data, x, y,
#                  object,
#                  digits = 2,
#                  FDH = TRUE,
#                  print.table = FALSE,
#                  na.rm = TRUE)

## ----scores_RF----------------------------------------------------------------
scoresRF <- efficiencyRFEAT(data = PISAindex,
                            x = c(6, 7, 8, 12, 17),
                            y = 3:5,
                            object = bestRFEAT_model,
                            FDH = TRUE,
                            print.table = TRUE)

## ----predict, eval = FALSE----------------------------------------------------
#  predict(object, newdata, x, ...)

## ----predictions, collapse = FALSE--------------------------------------------
# bestEAT_model <- EAT(data = PISAindex, x = c(6, 7, 8, 12, 17), y = 3:5, 
                     # numStop = 5, fold = 5)

# bestRFEAT_model <- RFEAT(data = PISAindex, x = c(6, 7, 8, 12, 17), y = 3:5,
                         # numStop = 3, m = 30, s_mtry = 'BRM')

predictions_EAT <- predict(object = bestEAT_model,
                           newdata = PISAindex,
                           x = c(6, 7, 8, 12, 17))

predictions_RFEAT <- predict(object = bestRFEAT_model,
                             newdata = PISAindex,
                             x = c(6, 7, 8, 12, 17))

## ----EAT_vs_RFEAT, collapse = FALSE, echo = FALSE-----------------------------

predictions <- data.frame(
  "S_PISA" = PISAindex[, 3],
  "R_PISA" = PISAindex[, 4],
  "M_PISA" = PISAindex[, 5],
  "S_EAT" = predictions_EAT[, 1],
  "R_EAT" = predictions_EAT[, 2],
  "M_EAT" = predictions_EAT[, 3],
  "S_RFEAT" = predictions_RFEAT[, 1],
  "R_RFEAT" = predictions_RFEAT[, 2],
  "M_RFEAT" = predictions_RFEAT[, 3]
  ) 

kableExtra::kable(predictions) %>%
  kableExtra::kable_styling("striped", full_width = F)


## ----newDF, collapse = FALSE--------------------------------------------------
new <- data.frame(WS = c(87, 92, 99), S = c(93, 90, 90), NBMC = c(90, 95, 93),
                  HW = c(90, 91, 92), AAE = c(88, 91, 89))

predictions_EAT <- predict(object = bestEAT_model,
                           newdata = new,
                           x = 1:5)

