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
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -2.366482 -0.495918  0.056486 -0.001876  0.524427  2.392864

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
    ##  [1] 3.705103 2.800328 3.355958 2.396724 1.653904 2.523779 2.340295 1.167254
    ##  [9] 1.830280 2.890790 1.474090 3.341358 4.529069 4.008447 2.861398 3.109150
    ## [17] 2.559654 2.235425 3.136550 3.074960
    ## 
    ## $b
    ##  [1] -2.47621781  4.15974374  1.01567207  1.24525639 -0.08130278 -3.89387920
    ##  [7] -0.58089067 -5.04429784  0.20838890 -2.20681622  0.82741865 -0.54992796
    ## [13]  3.40626131 -0.88412219  0.57418404  8.22618277 -1.37578412 -5.33501835
    ## [19]  1.09494962  1.03361360  4.04248491 14.00114164 14.49919255 -3.89209970
    ## [25]  2.98681380  8.53668593  2.03972501  5.68926536 -3.28243706  2.14137567
    ## 
    ## $c
    ##  [1]  9.933711  9.691680 10.000799 10.398412  9.653940 10.116558  9.985278
    ##  [8] 10.161802  9.856344  9.818377  9.987029  9.883832 10.056487 10.053501
    ## [15] 10.424513 10.129571 10.350683  9.839670  9.795440  9.911606 10.051151
    ## [22] 10.055619 10.154992  9.737102 10.005120  9.851715 10.329059 10.130178
    ## [29] 10.028323  9.848689  9.775761  9.802057  9.808774  9.690623 10.420512
    ## [36]  9.870797  9.822777  9.781339  9.876328 10.065904
    ## 
    ## $d
    ##  [1] -2.4262145 -2.9567241 -3.3741626 -3.9338775 -2.8348246 -4.2604709
    ##  [7] -1.3637931 -3.4909813 -4.2319999 -2.4322879 -3.4840197 -2.0014640
    ## [13] -0.9083253 -3.2510220 -3.5283452 -4.3416836 -4.2797273 -2.3531580
    ## [19] -3.0617480 -2.6722907

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
    ## 1  2.75 0.847

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.54  4.87

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.206

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.06 0.961

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])

}
```

## Let’s try map!

``` r
output = map(list_norm, mean_and_sd)
```

what if you want a different function..?

``` r
output = map(list_norm, median)
```

``` r
output = map_dbl(list_norm, median)
```

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

## List columns!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 3.705103 2.800328 3.355958 2.396724 1.653904 2.523779 2.340295 1.167254
    ##  [9] 1.830280 2.890790 1.474090 3.341358 4.529069 4.008447 2.861398 3.109150
    ## [17] 2.559654 2.235425 3.136550 3.074960
    ## 
    ## $b
    ##  [1] -2.47621781  4.15974374  1.01567207  1.24525639 -0.08130278 -3.89387920
    ##  [7] -0.58089067 -5.04429784  0.20838890 -2.20681622  0.82741865 -0.54992796
    ## [13]  3.40626131 -0.88412219  0.57418404  8.22618277 -1.37578412 -5.33501835
    ## [19]  1.09494962  1.03361360  4.04248491 14.00114164 14.49919255 -3.89209970
    ## [25]  2.98681380  8.53668593  2.03972501  5.68926536 -3.28243706  2.14137567
    ## 
    ## $c
    ##  [1]  9.933711  9.691680 10.000799 10.398412  9.653940 10.116558  9.985278
    ##  [8] 10.161802  9.856344  9.818377  9.987029  9.883832 10.056487 10.053501
    ## [15] 10.424513 10.129571 10.350683  9.839670  9.795440  9.911606 10.051151
    ## [22] 10.055619 10.154992  9.737102 10.005120  9.851715 10.329059 10.130178
    ## [29] 10.028323  9.848689  9.775761  9.802057  9.808774  9.690623 10.420512
    ## [36]  9.870797  9.822777  9.781339  9.876328 10.065904
    ## 
    ## $d
    ##  [1] -2.4262145 -2.9567241 -3.3741626 -3.9338775 -2.8348246 -4.2604709
    ##  [7] -1.3637931 -3.4909813 -4.2319999 -2.4322879 -3.4840197 -2.0014640
    ## [13] -0.9083253 -3.2510220 -3.5283452 -4.3416836 -4.2797273 -2.3531580
    ## [19] -3.0617480 -2.6722907

``` r
listcol_df %>%
  filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations…

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.75 0.847

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.54  4.87

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.75 0.847
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.54  4.87
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.206
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.06 0.961

So … can I add a list column??

``` r
listcol_df = 
  listcol_df %>%
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median))
```
