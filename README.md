
# DBModelSelect

<!-- badges: start -->
<!-- badges: end -->

The goal of DBModelSelect is to provide a package for using various distribution-based model selection techniques.

## Installation

You can install the development version of DBModelSelect from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shkoeneman/DBModelSelect")
```

## Example

This is a basic example which shows you how to use various features of the package:

``` r
library(DBModelSelect)

# generate some data
data <- data.frame(s = rnorm(200), t = rnorm(200))
data$y <- data$s + rnorm(200)

# perform all subsets regression
model_list <- DBModelSelect::FitLMSubsets(response = "y", data = data, intercept = TRUE, force_intercept = TRUE)

# perform model selection
model_select <- DBModelSelect::StandICModelSelect(model_list, IC = "AIC")

# plot results of model selection
DBModelSelect::StandICPlot(model_select)
```

