
# DBModelSelect

<!-- badges: start -->
[![R-CMD-check](https://github.com/shkoeneman/DBModelSelect/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shkoeneman/DBModelSelect/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of DBModelSelect is to provide a package for using various distribution-based model selection techniques.

## Installation

You can install the latest CRAN version of DBModelSelect with:

```r
install.packages("DBModelSelect")
```

Or you can install the development version of DBModelSelect from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shkoeneman/DBModelSelect")
```

## Example

This is a basic example which shows you how to use various features of the package:

``` r
library(DBModelSelect)

# generate some data
set.seed(5122023)
data <- data.frame(s = rnorm(200), t = rnorm(200))
data$y <- data$s + rnorm(200)

# perform all subsets regression
model_list <- FitLMSubsets(response = "y", data = data, intercept = TRUE, force_intercept = FALSE)

#determine whether largest candidate model shows lack of fit
BootGOFTestLM(model_list[[length(model_list)]], data = data)

# perform model selection
model_select <- StandICModelSelect(model_list, IC = "AIC")

# print and plot results of model selection
print(model_select)
plot(model_select)
```

