# R/functions.R
get_data <- function(file, language) {
  # read data
  data <- as.data.table(read_xlsx(file), sheet = "behavior_raw")
  # add column with id 
  data$id <- 1:nrow(data)
  # melt data to long format
  data <- melt(data,
               measure.vars = c("L1_SEM_CAT",
                                "L2_SEM_CAT",
                                "L1_PHONO",
                                "L2_PHONO"),
               variable.name = "condition",
               value.name = "accuracy")
  # create new columns with factors for lang and task separately 
  data$lang <- fifelse(grepl("L1", data$condition), "polish", "PJM")
  data$task <- fifelse(grepl("PHONO", data$condition), "PHONO", "SEM_CAT")
  # subset data of given language
  data <- data[lang == language]
  # remove column "condition" since we won't use it
  data <- data[, !("condition")]
  # change "grupa" to "group" to have a more coherent naming 
  setnames(data, "grupa", "group")
  data
}

# function for fitting poisson model
fit_poisson_model <- function (data) {
  formula <- accuracy ~ group * task + (1|id)
  model <- glmer(formula, data = data, family = poisson(link = "log"), nAGQ = 100)
  model
}

# function for plotting model diagnostics using DHARMa package
plot_model_diagnostics <- function (model) {
  simulationOutput <- simulateResiduals(fittedModel = model, plot = T)
}

# function for estimating p-values using bootstrap
# this step takes a lot of time and needs 
get_pvalues <- function(model, .data, .nsim = 10) {
  set.seed(2137)
  model_pvals <- mixed(model,
                       method = "PB",
                       family = poisson(link = "log"),
                       args_test = list(nsim = .nsim, details = 2),
                       data = .data,
                       progress = FALSE)
  model_pvals
}
