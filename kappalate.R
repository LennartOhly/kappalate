# Hello, world!
kappalate <- function(formula, data, zmodel = NULL, vce = NULL, std = NULL, which = NULL) {

  # Ensure required packages are loaded
  if (!requireNamespace("formula.tools", quietly = TRUE)) {
    stop("Package 'formula.tools' is required. Install it using install.packages('formula.tools').")
  }
  if (!requireNamespace("gmm", quietly = TRUE)) {
    stop("Package 'gmm' is required. Install it using install.packages('gmm').")
  }
  library(formula.tools)
  library(formula.tools)


  # Check whether the formula is valid and raise an error if not.
  is.formula <- function(x){
    inherits(x,"formula")
  }
  if (!is.formula(formula)) {
    stop("The input must be a valid formula. To lear how to write valid IV formulas see: https://cran.r-project.org/web/packages/ivreg/vignettes/ivreg.html#instrumental-variables-regression ")
  }

  # We parse the given input arguments
  yvar <- lhs(formula)
  #yvar <- all.vars(lhs(formula))

  print(data[[yvar]])
  #print("Hello, online3 world my friend!")
  }


set.seed(123)
n <- 100
df <- data.frame(
  y = rnorm(n),
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),  # Exogenous variable
  z1 = rnorm(n),
  z2 = rnorm(n)
)


kappalate(y ~ x1 + x2 + x3 | z1 + z2, data = df)


df$y[10]
print(df[["y"]])

