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
    ## -2.94459 -0.77599 -0.01330 -0.04596  0.71811  2.39311

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
    ##  [1] 2.548787 3.152817 3.407165 3.880655 2.645147 3.933843 3.013198 3.444367
    ##  [9] 4.808510 0.721354 2.578955 2.992574 2.661714 1.620537 2.338302 4.326343
    ## [17] 1.940002 2.677610 2.727503 3.598749
    ## 
    ## $b
    ##  [1] -6.6577171  6.4079761 -0.1101492 -2.6763601  2.2996578 10.2300362
    ##  [7] -6.5112837  1.3614497 -1.4679979  0.8997690  2.1886110 -1.7808791
    ## [13] -3.7853717  1.8042695  4.9992938 -5.5136911 -1.1866209 -2.6624836
    ## [19]  3.4067021  2.8431088  8.0907792 -0.5169545  6.5520517  4.8030133
    ## [25] 11.7402104  4.2530152  3.8868227 -1.8915512  5.4262173 -1.0807273
    ## 
    ## $c
    ##  [1]  9.608760 10.279935 10.353132 10.051611  9.925672  9.829433  9.817622
    ##  [8]  9.927520  9.845090 10.147790  9.983358 10.014380 10.049064 10.098543
    ## [15] 10.047534  9.851173  9.967695  9.891065  9.924524 10.353337  9.743315
    ## [22]  9.722856  9.580273 10.163239 10.096491 10.078944 10.122004  9.968106
    ## [29]  9.967828 10.000733  9.537406  9.899125 10.413672  9.917804 10.192464
    ## [36]  9.894529 10.192741 10.018087  9.907510  9.758200
    ## 
    ## $d
    ##  [1] -3.798129 -3.078577 -4.388395 -2.066153 -2.686523 -3.113908 -2.422900
    ##  [8] -3.167863 -4.093309 -2.804976 -2.343421 -2.121928 -5.094252 -3.198152
    ## [15] -1.498024 -4.530764 -3.861085 -1.521409 -4.641817 -2.502943

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
    ## 1  2.95 0.941

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.51  4.65

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.201

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.15  1.05

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
