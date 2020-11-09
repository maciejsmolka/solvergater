
<!-- README.md is generated from README.Rmd. Please edit that file -->

# solvergater

<!-- badges: start -->

[![R build
status](https://github.com/maciejsmolka/solvergater/workflows/R-CMD-check/badge.svg)](https://github.com/maciejsmolka/solvergater/actions)
[![Travis build
status](https://travis-ci.org/maciejsmolka/solvergater.svg?branch=master)](https://travis-ci.org/maciejsmolka/solvergater)
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

## Usage

Let us start with a basic example which shows you how to run an external
solver to compute the quantity of interest and its Jacobian matrix with
respect to the parameters. The example uses a mock solver provided with
the package.

``` r
library(solvergater)
rscript_path <- file.path(R.home(), "bin", "Rscript")
solver_path <- file.path(find.package("solvergater"), "exec", "fake_simple.R")
nparams <- 4
nqoi <- 5
solver_cmd <- paste(rscript_path, solver_path, nparams, nqoi)
s <- shell_solver(solver_cmd, nparams = nparams, qoi_file = "output_qoi",
                  jacobian_file = "output_jacobian", wd = tempdir())
run(s, c(20.11, 5, -1.2, 10.4), 10)
#> Solver command: /Library/Frameworks/R.framework/Resources/bin/Rscript /Library/Frameworks/R.framework/Versions/4.0/Resources/library/solvergater/exec/fake_simple.R 4 5 20.11 5 -1.2 10.4 10
#> Entering /var/folders/4g/7jf88w2d0wv30c08pl1kwcgh0000gn/T//RtmpcC0g17
#> Exiting /var/folders/4g/7jf88w2d0wv30c08pl1kwcgh0000gn/T//RtmpcC0g17
#> Solver exited normally
#> $qoi
#> [1]      34.3100     539.0121    9380.8630  175874.8000 3413761.0000
#> 
#> $jacobian
#>            [,1] [,2]   [,3]      [,4]
#> [1,]      1.000    1  1.000     1.000
#> [2,]     40.220   10 -2.400    20.800
#> [3,]   1213.236   75  4.320   324.480
#> [4,]  32530.910  500 -6.912  4499.456
#> [5,] 817745.700 3125 10.368 58492.930
```

The basic use case for `solvergater` is the support of the optimization
problems with special attention paid for inverse problems. A simulated
problem of the latter type can be as follows.

First, we prepare some ‘observed data’ by running the solver at a high
accuracy level.

``` r
observed_data <- run(s, c(10, 10, 10, 10), precision = 5.0)$qoi
#> Solver command: /Library/Frameworks/R.framework/Resources/bin/Rscript /Library/Frameworks/R.framework/Versions/4.0/Resources/library/solvergater/exec/fake_simple.R 4 5 10 10 10 10 5
#> Entering /var/folders/4g/7jf88w2d0wv30c08pl1kwcgh0000gn/T//RtmpcC0g17
#> Exiting /var/folders/4g/7jf88w2d0wv30c08pl1kwcgh0000gn/T//RtmpcC0g17
#> Solver exited normally
```

Then, as the objective we use the misfit between observed data and a
current proposed set of parameters.

``` r
x <- c(10.5, 9.44, 10.21, 8.14)
solver_obj <- objective(s, observed_data)
solver_obj$value(x, precision = 30.0)
#> Solver command: /Library/Frameworks/R.framework/Resources/bin/Rscript /Library/Frameworks/R.framework/Versions/4.0/Resources/library/solvergater/exec/fake_simple.R 4 5 10.5 9.44 10.21 8.14 30
#> Entering /var/folders/4g/7jf88w2d0wv30c08pl1kwcgh0000gn/T//RtmpcC0g17
#> Exiting /var/folders/4g/7jf88w2d0wv30c08pl1kwcgh0000gn/T//RtmpcC0g17
#> Solver exited normally
#> [1] 2594156034
solver_obj$gradient(x, precision = 30.0)
#> Solver command: /Library/Frameworks/R.framework/Resources/bin/Rscript /Library/Frameworks/R.framework/Versions/4.0/Resources/library/solvergater/exec/fake_simple.R 4 5 10.5 9.44 10.21 8.14 30
#> Entering /var/folders/4g/7jf88w2d0wv30c08pl1kwcgh0000gn/T//RtmpcC0g17
#> Exiting /var/folders/4g/7jf88w2d0wv30c08pl1kwcgh0000gn/T//RtmpcC0g17
#> Solver exited normally
#> [1] -6208209534 -4059190749 -5551351184 -2246937119
```
