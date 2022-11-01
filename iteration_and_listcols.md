iteration_and_listcols
================
2022-11-01

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(rvest) 
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Lists

You can put anything in a list

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.44453 -0.69325  0.01822  0.02749  0.77803  2.94546

``` r
l$vec_numeric 
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

Create a new list.

``` r
list_norm= 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = 0.2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 2.7124645 3.2265400 2.4501087 3.7403240 3.0036021 3.5505837 1.4434846
    ##  [8] 3.1446978 0.8709583 4.6308443 2.2392497 2.2865881 2.7998058 1.5661522
    ## [15] 5.4713660 2.3514528 4.6709162 2.7708445 4.2728493 2.6244390
    ## 
    ## $b
    ##  [1] -3.5759792  2.7612176  2.3038691 -4.9711501 -7.7341098 -3.8216239
    ##  [7] -0.7292078 12.4177148  0.9639878 -9.4809825 -1.3625363 -2.3226446
    ## [13] -2.0903333 -2.9313953  9.4909063  3.2436322 -3.1982266  5.6893860
    ## [19] -3.2335300 -2.8470233  1.1234228 -5.7951856  0.8265252 -1.2125836
    ## [25] -4.1849767 -3.4727285 -0.3477915  0.1257332 -3.4542337 -6.5705890
    ## 
    ## $c
    ##  [1] 10.201276  9.814757 10.159313 10.279803  9.832018  9.896427 10.023390
    ##  [8] 10.246702  9.727734  9.911039 10.138910  9.862442 10.076113  9.939447
    ## [15]  9.955822  9.965509  9.951641  9.888408 10.068135  9.816076 10.030242
    ## [22] 10.050316  9.788980  9.866836 10.045835 10.033866  9.727422 10.180801
    ## [29]  9.457812 10.337707  9.986138 10.005108 10.064131 10.232604  9.626060
    ## [36]  9.998514 10.145477  9.962747  9.575335  9.923283
    ## 
    ## $d
    ##  [1] -2.281480 -4.577322 -2.146570 -2.553536 -3.025120 -3.901274 -2.612346
    ##  [8] -1.326187 -1.608225 -2.920628 -4.266940 -3.184494 -3.536531 -2.563552
    ## [15] -1.636749 -2.339674 -2.615636 -2.952241 -2.727789 -2.528999

Pause and get my old function.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } 
  
  if (length(x) < 3) {
    stop("Input must have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.99  1.15

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.15  4.68

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97 0.191

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.77 0.840

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])

}
```
