# Hello, world!


kappalate <- function(given_formula, data, zmodel = NULL, vce = NULL, std = NULL, which = NULL, subset = NULL) {

  # Ensure required packages are loaded
  if (!requireNamespace("formula.tools", quietly = TRUE)) {
    stop("Package 'formula.tools' is required. Install it using install.packages('formula.tools').")
  }
  if (!requireNamespace("geex", quietly = TRUE)) {
    stop("Package 'geex' is required. Install it using install.packages('geex').")
  }
  if (!requireNamespace("Formula", quietly = TRUE)) {
    stop("Package 'Formula' is required. Install it using install.packages('Formula').")
  }
  # Load libraries
  library(formula.tools)
  library(geex)
  library(Formula)

  # Check whether the formula is valid and raise an error if not.
  is.formula <- function(x){
    inherits(x,"formula")
  }

  is_formula <- is.formula(given_formula)
  if (!is_formula) {
    stop("The input must be a valid IV formula of the form: 'outcome ~ exogenous variables | endogenous variables | instrumental variables'. E.g.: 'y ~ x1 + x2 | t1 + t2 | z1 + z2'.")
  }
  length_correct <- all(length(Formula(given_formula))== c(1,3))
  if (!length_correct) {
    stop("The input must be a valid IV formula of the form: 'outcome ~ exogenous variables | endogenous variables | instrumental variables'. E.g.: 'y ~ x1 + x2 | t1 + t2 | z1 + z2'.")
  }

  # We parse the given input arguments using the formula.tools and Formula packages.
  # The parsed IV arguments are saved as characters to access the necessary data in the df.
  yvar <- as.character(lhs(given_formula))
  xvarsips <- all.vars(rhs(formula(Formula(given_formula), rhs = 1)))
  tvar <- all.vars(rhs(formula(Formula(given_formula), rhs = 2)))
  zvar <- all.vars(rhs(formula(Formula(given_formula), rhs = 3)))

  # We check if only a subset of the data should be used
  # First we assert what input constitutes a valid index vector
  is_valid_index <- function(index, data) {
    if (is.null(index)) return(FALSE)
    n <- nrow(data)
    if (all(is.numeric(index))) {
      return(all(index > 0 & index <= n & index == as.integer(index)))
    } else if (is.logical(index)) {
      return(length(index) == n)
    } else {
      return(FALSE)
    }
  }

  # Then we check if a subset is given and if it is a valid index vector and locally assign the subset
  if (!is.null(subset)){
    if (is_valid_index(subset, data)){
      data <- data[subset, ]
    } else {
      stop("The specified subset is not a valid index vector. It should either be numeric e.g. subset = c(1,2,3,4) where all specified index values must be part of the dataframe or logical e.g. subset = c(TRUE, FALSE, FALSE), with length = length of the dataframe.")
    }
  }

  # Next we locally remove rows with missing data
  data <- na.omit(data[, c(yvar,xvarsips, tvar, zvar)])
  #rownames(data) <- NULL

  # Then we assign some standard values for options that were not set.
  if (is.null(zmodel)) {
    zmodel <- "cbps"}
  if (is.null(vce)) {
    vce <- "robust"}
  if (is.null(std)) {
    std <- "on"}
  if (is.null(which)) {
    which <- "norm"}

  # Next we check some input conditions and raise errors if they are not fulfilled.
  # First for input strings
  if (!zmodel %in% c("cbps", "logit", "probit")) {
    stop(sprintf("zmodel = '%s'  not allowed. Allowed values are 'cbps', 'logit', or 'probit'.", zmodel))
  }
  if (!std %in% c("on", "off")) {
    stop(sprintf("std = '%s' not allowed. Allowed values are 'on' or 'off'.", std))
  }
  if (!which %in% c("norm", "all")) {
    stop(sprintf("which = '%s'= not allowed. Allowed values are 'norm' or 'all'.", which))
  }

  # Then for some requirements on the treatment and instrument data:
  tvar_values <- unique(data[[tvar]])
  if (length(tvar_values) < 2) {
    stop("Treatment variable must take on at least two distinct values.")
  }
  if (length(tvar_values) > 2) {
    bintreat <- 0
  } else if (length(tvar_values) == 2) {
    if (all(tvar_values %in% c(0,1))) {
      bintreat <- 1
    } else {
      bintreat <- 0
    }
  }

  zvar_values <- unique(data[[zvar]])
  if (length(zvar_values) != 2) {
    stop("Instrument  must be binary.")
  }
  if (!all(zvar_values %in% c(0,1))) {
    stop("Instrument must only take on values zero or one")
  }

  # Next we standardize the non-binary exogenous variables if std == "on" and assign the scaled values to the data frame.
  if (std == "on"){
    for (col in xvarsips){
      unique_values <- unique(data[[col]])
      if (length(unique_values) != 2){
        data[[col]] <- scale(data[[col]])
      }
    }
  }

  # We then examine compliance with the instrument.
  dmeanz1 <- mean(data[data[, zvar] == 1, tvar])
  dmeanz0 <- mean(data[data[, zvar] == 0, tvar])

  if (bintreat == 1) {
    if (dmeanz0 == dmeanz1) {
      stop("Error: zero denominator - LATE not defined.")
    } else if (dmeanz1 == 1 & dmeanz0 == 0) {
      stop("Error: instrument identical to treatment.")
    } else if (dmeanz1 == 0 & dmeanz0 == 1) {
      stop("Error: instrument identical to treatment.")
    }
  }

  # We can now begin with the main estimation procedure
  # Start with estimation of the instrument propensity score when no cbps is selected
  # Note that pz <- exp(zhat) / (1 + exp(zhat)) with the z estimates in the link function is equal to using the predict function
  formula_z <- reformulate(termlabels = xvarsips, response = zvar)
  if (zmodel != "cbps") {
    if (zmodel == "logit") {
      logit_model <- glm(formula_z, data = data, family = binomial())
      bips <- coef(logit_model)
      ips <- predict(logit_model, type = "response")
    } else if (zmodel == "probit") {
      probit_model <- glm(formula_z, data = data, family = binomial(link = "probit"))
      bips <- coef(probit_model)
      ips <- predict(probit_model, type = "response")
    }

  # Now for the case with cbps
  # Determine starting values from logit
  } else {
    logit_model <- glm(formula_z, data = data, family = binomial())
    initial <- coef(logit_model)

  # Then set up the moment condition in geex following covariate balancing by (Imai & Ratkovic, 2013). Here the moment condition is E[((z - pz)/(pz(1-pz)))X] = 0, with pz <- exp(zhat) / (1 + exp(zhat)), and zhat <- X*theta.
    cbps_moment <- function(data){
      X <- model.matrix(formula_z, data = data)
      Z <- data[[zvar]]
      function(theta){
        zhat <- X %*% theta
        pz <- plogis(zhat)
        c(((Z - pz)/pz*(1-pz))%*%X)
      }
    }
    # The moment estimation in geex is then done via m_estimate(). If convergence fails we continue with the logistic regression estimates.
    tryCatch({
      cbps_results <- m_estimate(
      estFUN = cbps_moment,
      data   = data,
      root_control = setup_root_control(FUN = rootSolve::multiroot, start = initial)
      )
    bips <- coef(cbps_results)
    ips <- plogis(model.matrix(formula_z, data = data) %*% bips)
    }, error = function(e) {
      message(sprintf("An error occurred using M-Estimation for the covariate balancing of treatment propensities: %s Did not acheive convergence. Instead logistic regression is used to estimate treatment propensity.", e$message))
      bips <<- coef(logit_model)
      ips <<- predict(logit_model, type = "response")
      })
    }

  # LATE Estimation
  # Since until now we stored only the variable names we create the matrices containing the actual data points
  y_data <- data[[yvar]]
  z_data <- data[[zvar]]
  t_data <- data[[tvar]]
  # We then create the variables identifying the LATE. See Abadie's Kappa and Weighting Estimators of the Local Average Treatment Effect" by Tymon Słoczyński, Derya Uysal, & Jeffrey M. Wooldridge for the detailed theory.
  numhat <- (z_data / ips) * y_data - ((1 - z_data) / (1 - ips)) * y_data
  kappa_1 <- (z_data / ips) * t_data - ((1 - z_data) / (1 - ips)) * t_data
  kappa_0 <- (1 - t_data) * ((1 - z_data) - (1 - ips)) / (ips * (1 - ips))
  kappaw <- 1 - (t_data * (1 - z_data)) / (1 - ips) - ((1 - t_data) * z_data) / ips
  num1hat <- kappa_1 * y_data
  num0hat <- kappa_0 * y_data

  nums <- mean(numhat)
  kappa_1s <- mean(kappa_1)
  kappa_0s <- mean(kappa_0)
  kappas <- mean(kappaw)
  num1hats <- mean(num1hat)
  num0hats <- mean(num0hat)







  print((kappa_0*(1-ips)+kappa_1*ips)[1:3])
  print(kappaw[1:3])
  } #Ends the function code











### TESTING
library(geex)

SB1_estfun <- function(data){
  X <- model.matrix(z ~ x1 + x2, data = data)
  Z <- data[["z"]]
  function(theta){
  zhat <- X %*% theta
  pz <- plogis(zhat)
  c(((Z - pz)/pz*(1-pz))%*%X)
  }
}


results <- m_estimate(
  estFUN = SB1_estfun,
  data   = df,
  root_control = setup_root_control(FUN = rootSolve::multiroot, start = coefslog))
print(coef(results))
print(vcov(results))


logit_model <- glm(z ~ x1 + x2, data = df, family = binomial())
predictions <- predict(logit_model, type = "response")
coefslog <- coef(logit_model)
coefslog
predictions[1:10]














kappalate(y ~ x1 + x2 | t | z  , data = df, zmodel = "cbps", std = "off", subset = c(1:1000))


set.seed(123)
n <- 1000
x1 <- rbinom(n, size = 1, prob = 0.5)
x2 <- rnorm(n, mean = -1, sd = 1)
z <- pmax(rbinom(n, size = 1, prob = 0.5)*x1, rbinom(n, size = 1, prob = 0.5))
u <- rbinom(n, size = 1, prob = 0.5)
t <- pmax(z,u)
y <- 1.5 * t + 0.8 * x1 - 0.5 * x2 + u + rnorm(n, mean = 0, sd = 1)
df <- data.frame(y = y, t = t, z = z, x1 = x1, x2 = x2)


cov(y,z)/cov(t,z)
lm(y ~ x1 + x2:x1 + t, data = df)
lm(y ~ x1 + x2 + z + t, data = df)
lm(t ~ x1 + x2 + z, data = df)
library(ivreg)
ivreg(y ~ x1 + x2 | t | z , data = df)
lm(y ~ x1 +x2 +t, data = df)


# Need to add a way to add various instruments or only allow single instrument
# Same for treatment
# Add further preprocessing using formulas
