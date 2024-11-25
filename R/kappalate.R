# Hello, world!


kappalate <- function(given_formula, data, zmodel = NULL, vce = NULL, std = NULL, which = NULL) {

  # Ensure required packages are loaded
  if (!requireNamespace("formula.tools", quietly = TRUE)) {
    stop("Package 'formula.tools' is required. Install it using install.packages('formula.tools').")
  }
  if (!requireNamespace("gmm", quietly = TRUE)) {
    stop("Package 'gmm' is required. Install it using install.packages('gmm').")
  }
  if (!requireNamespace("Formula", quietly = TRUE)) {
    stop("Package 'Formula' is required. Install it using install.packages('Formula').")
  }
  # Load libraries
  library(formula.tools)
  library(gmm)
  library(Formula)


  # Check whether the formula is valid and raise an error if not.
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


  # Next we remove rows with missing data
  print(c(yvar,xvarsips, tvar, zvar))
  data <- na.omit(data[, c(yvar,xvarsips, tvar, zvar)])
  rownames(data) <- NULL

  # Then we assign some standard values for options that were not set.
  if (is.null(zmodel)) {
    zmodel <- "cbps"}
  if (is.null(vce)) {
    vce <- "robust"}
  if (is.null(std)) {
    std <- "on"}
  if (is.null(which)) {
    which <- "norm"}


  print(c(zmodel,vce, std, which))
  } #Ends the function code






set.seed(123)
n <- 100
df <- data.frame(
  y = rnorm(n),
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  z1 = rnorm(n),
  z2 = rnorm(n)
)




# Testing
df$y[sample(1:n, size = 10)] <- NA  # 10 random missing values in y
df$x1[sample(1:n, size = 15)] <- NA  # 15 random missing values in x1
df$z1[sample(1:n, size = 5)] <- NA  # 5 random missing values in z1
df$x3[sample(1:n, size = 8)] <- NA  # 8 random missing values in x3


kappalate(y ~ x3 | x1 + x2 | z1 + z2 , data = df)


