writing_functions
================
2022-10-29

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.40116971  1.04454771 -0.01335214 -1.13513998 -0.82052883  0.89791469
    ##  [7]  0.19153425 -1.25104054 -0.15432781 -0.35478296 -1.85671709  0.19860940
    ## [13]  0.42110794  0.04924150 -0.31227071 -0.02828018  0.95033026  0.14511090
    ## [19] -1.74273305  1.00327844 -0.12984049 -0.22260473  2.29601829 -0.47905786
    ## [25]  0.66077026 -1.32254707  1.64628758 -1.06246543  0.08016573  1.70194162

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)){
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1] -0.40116971  1.04454771 -0.01335214 -1.13513998 -0.82052883  0.89791469
    ##  [7]  0.19153425 -1.25104054 -0.15432781 -0.35478296 -1.85671709  0.19860940
    ## [13]  0.42110794  0.04924150 -0.31227071 -0.02828018  0.95033026  0.14511090
    ## [19] -1.74273305  1.00327844 -0.12984049 -0.22260473  2.29601829 -0.47905786
    ## [25]  0.66077026 -1.32254707  1.64628758 -1.06246543  0.08016573  1.70194162

Try my function on some other things. These should give errors.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least three numbers

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): Input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric
