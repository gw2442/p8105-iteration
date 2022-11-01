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
    ## -2.97985 -0.52243  0.04981  0.07147  0.71693  2.26131

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
    ##  [1] 1.552287 2.289901 2.988699 4.354961 4.807910 2.013666 4.945363 2.332641
    ##  [9] 2.950827 2.174711 3.195169 3.010686 2.895949 3.265216 1.853554 5.051886
    ## [17] 2.032402 1.281524 2.845260 2.096689
    ## 
    ## $b
    ##  [1]  9.47678983  6.62796933 -0.35433714 -2.88701523  1.68808795 -0.33348743
    ##  [7] -1.40706109  2.36508906 -0.52180938  0.77057824 -4.62244870 -7.69918627
    ## [13] -7.24222589 -0.55907007  3.06592920  4.56147351 -0.35180402 -1.17813836
    ## [19] -1.22410124 -1.72885280  2.26359473  3.89319254 -4.74340073  3.48401722
    ## [25] -2.37633741 -0.72579147 -0.09541609 -5.00838201  6.48213232 -5.91027362
    ## 
    ## $c
    ##  [1]  9.883150  9.900692 10.043190 10.206661  9.850081  9.847564 10.065959
    ##  [8]  9.856570 10.237228 10.044699  9.415462 10.069315 10.085508 10.061592
    ## [15] 10.258260 10.291728  9.823502 10.057608  9.798414 10.277364  9.850876
    ## [22] 10.086083 10.006329 10.041296 10.180862 10.053022 10.205411 10.001333
    ## [29] 10.125129 10.073781 10.350518 10.046468  9.855629 10.168722 10.121130
    ## [36]  9.994454  9.954963 10.076126 10.099150  9.950557
    ## 
    ## $d
    ##  [1] -4.971236 -4.075863 -2.772748 -4.230148 -1.421933 -3.437242 -3.397935
    ##  [8] -4.375842 -2.617198 -1.973482 -3.225131 -1.899215 -2.315836 -3.409392
    ## [15] -4.049016 -1.999065 -3.031221 -3.777682 -3.312069 -2.486269

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
    ## 1  2.90  1.12

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.143  4.09

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.172

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.14 0.949

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
    ##  [1] 1.552287 2.289901 2.988699 4.354961 4.807910 2.013666 4.945363 2.332641
    ##  [9] 2.950827 2.174711 3.195169 3.010686 2.895949 3.265216 1.853554 5.051886
    ## [17] 2.032402 1.281524 2.845260 2.096689
    ## 
    ## $b
    ##  [1]  9.47678983  6.62796933 -0.35433714 -2.88701523  1.68808795 -0.33348743
    ##  [7] -1.40706109  2.36508906 -0.52180938  0.77057824 -4.62244870 -7.69918627
    ## [13] -7.24222589 -0.55907007  3.06592920  4.56147351 -0.35180402 -1.17813836
    ## [19] -1.22410124 -1.72885280  2.26359473  3.89319254 -4.74340073  3.48401722
    ## [25] -2.37633741 -0.72579147 -0.09541609 -5.00838201  6.48213232 -5.91027362
    ## 
    ## $c
    ##  [1]  9.883150  9.900692 10.043190 10.206661  9.850081  9.847564 10.065959
    ##  [8]  9.856570 10.237228 10.044699  9.415462 10.069315 10.085508 10.061592
    ## [15] 10.258260 10.291728  9.823502 10.057608  9.798414 10.277364  9.850876
    ## [22] 10.086083 10.006329 10.041296 10.180862 10.053022 10.205411 10.001333
    ## [29] 10.125129 10.073781 10.350518 10.046468  9.855629 10.168722 10.121130
    ## [36]  9.994454  9.954963 10.076126 10.099150  9.950557
    ## 
    ## $d
    ##  [1] -4.971236 -4.075863 -2.772748 -4.230148 -1.421933 -3.437242 -3.397935
    ##  [8] -4.375842 -2.617198 -1.973482 -3.225131 -1.899215 -2.315836 -3.409392
    ## [15] -4.049016 -1.999065 -3.031221 -3.777682 -3.312069 -2.486269

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
    ## 1  2.90  1.12

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.143  4.09

Can I just … map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.90  1.12
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.143  4.09
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.172
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.14 0.949

So … can I add a list column??

``` r
listcol_df = 
  listcol_df %>%
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median))
```

## Weather Data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2022-09-29 10:34:25 (8.401)

    ## file min/max dates: 1869-01-01 / 2022-09-30

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2022-09-29 10:34:30 (1.699)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2022-09-29 10:34:33 (0.95)

    ## file min/max dates: 1999-09-01 / 2022-09-30

Get our list columns…

``` r
weather_nest =
  weather_df %>%
  nest(data = date:tmin)
```

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

Suppose I want to regress `tmax` on `tmin` for each station.

This works …

``` r
lm(tmax ~ tmin, data = weather_nest$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

Let’s write a function.

``` r
weather_lm = function(df) {
  
  lm(tmax ~ tmin, data = df)
  
}

output = vector("list", 3)

for (i in 1:3) {
  
  output[[i]] = weather_nest$data[[i]]
  
  }
```

What about a map …!?

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

What about a map in a list column !!!!??

``` r
weather_nest = 
  weather_nest %>%
  mutate(models = map(data, weather_lm))

weather_nest$models
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221
