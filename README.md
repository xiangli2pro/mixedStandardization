
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mixedStandardization

<!-- badges: start -->
<!-- badges: end -->

It’s always recommended to standardized the input data beforehand in
sparse penalized regressions (e.g, lasso, elastic-net) so that variables
can be selected fairly. However, there have been constant confusions
regarding the choice of standardization method, especially when the
input variables are of different types (i.e. continuous, binary,
categorical).

The R package `MixedStandardization` provides commonly implemented
standardization methods (Z-score, Gelman, Minmax, and Bring) for users
to standardize their data. Meanwhile, we offer a novel Mixed
standardization method proposed in our paper *Standardization of
Continuous and Categorical Covariates in Sparse Penalized Regressions*.
The new method targets at generating comparable selection probabilities
in sparse penalized regressions for continuous, binary or categorical
covariates with the same impact. The idea is to eliminate the essential
difference among covariates of different types by converting all the
covariates into the same simple format. For example, when a data has
both continuous and binary covariates, we convert continuous variable to
binary variable using either random or empirical thresholds such that
all variables are of the same format, i.e. binary of similar
probabilities, after standardization. Simulation and real data analysis
have shown that the Mixed standardization method has better performance
from variable selection aspect. If coefficient estimate is of concern,
we recommend to fit the model using the original forms (before
standardization) of the covariates that are selected from the sparse
penalized model applied on standardized inputs.

## Installation

You can install the development version of `mixedStandardization` like
so:

``` r
# install.packages("devtools")
devtools::install_github("xiangli2pro/mixedStandardization")

# load package
library('mixedStandardization')
```

## Example

``` r
# load packages
library(mixedStandardization)
library(dplyr)

# check the help page for more information of the arguments
?mixedStandardization::mixedStand
```

``` r
# example data
data(mtcars)

# set the categorical variables as factor
x <- mtcars %>%
  mutate(cyl = as.factor(cyl),
         gear = as.factor(gear),
         carb = as.factor(carb))

# set the binary outcome variable
y <- sample(c(0,1), size = nrow(x), replace = TRUE)

# standardize x using the Gelman method
# specify the continuous/binary/categorical variables
x_Gelman <- mixedStand(
  x,
  y,
  standardization = "Gelman",
  continuousVars = c("mpg", "disp"),
  binaryVars = c("vs", "am"),
  categoricalVars = c("cyl"),
  pDichoto = 0.5,
  family = "binomial"
)

head(x_Gelman)
#>                        mpg      disp vs am cyl.6 cyl.8
#> Mazda RX4         1.742175 0.6454804  0  1     1     0
#> Mazda RX4 Wag     1.742175 0.6454804  0  1     1     0
#> Datsun 710        1.891505 0.4356993  1  1     0     0
#> Hornet 4 Drive    1.775360 1.0408372  1  0     1     0
#> Hornet Sportabout 1.551366 1.4523309  0  0     0     1
#> Valiant           1.501589 0.9077068  1  0     1     0

# standardize x using the Mixed method
# specify the continuous/binary/categorical variables
x_Mixed <- mixedStand(
  x,
  y,
  standardization = "Mixed",
  continuousVars = c("mpg", "disp"),
  binaryVars = c("vs", "am"),
  categoricalVars = c("cyl", "gear"),
  pDichoto = 0.5,
  family = "binomial"
)

head(x_Mixed)
#>                        mpg     disp        vs       am       cyl    gear
#> Mazda RX4         0.000000 0.984251 0.0000000 0.000000 0.0000000 0.00000
#> Mazda RX4 Wag     0.000000 0.984251 0.0000000 0.000000 0.0000000 0.00000
#> Datsun 710        0.000000 0.984251 0.9920317 0.000000 0.0000000 0.00000
#> Hornet 4 Drive    0.000000 0.000000 0.9920317 1.002022 0.0000000 1.01653
#> Hornet Sportabout 0.986179 0.000000 0.0000000 1.002022 0.9920317 1.01653
#> Valiant           0.986179 0.000000 0.9920317 1.002022 0.0000000 1.01653
```
