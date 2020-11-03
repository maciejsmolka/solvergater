
<!-- README.md is generated from README.Rmd. Please edit that file -->

# solvergater

<!-- badges: start -->

[![R build
status](https://github.com/maciejsmolka/solvergater/workflows/R-CMD-check/badge.svg)](https://github.com/maciejsmolka/solvergater/actions)
<!-- badges: end -->

It is a common task to solve an optimization problem when the objective
is computed using an external application, e.g. an adaptive
finite-element PDE solver, that gets its input from the command line and
writes its output to a number of files. The `solvergater` package
provides a simple R gateway to such external apps.

## Installation

<!--
You can install the released version of solvergater from 
[CRAN](https://CRAN.R-project.org) with:
-->

<!--
``` r
install.packages("solvergater")
```
-->

<!--
And 
-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("maciejsmolka/solvergater")
```

## Example

This is a basic example which shows you how to run an external solver to
compute the objective value and (possibly) gradient. The example uses
`r_solver` which in fact does not perform any external calls. In actual
computations solver classes like `shell_solver` are more useful.

``` r
library(solvergater)
solver <- r_solver(objective = function(x) sum(x^2), 
                         gradient = function(x) 2 * x)
# A more realistic code would be:
# solver <- shell_solver("/a/path/to/solver/exec")
obj <- run(solver, c(1.2, 3.4, 0.2), precision = 2.3)
obj$value
#> [1] 13.04
obj$gradient
#> [1] 2.4 6.8 0.4
## basic example code
```
