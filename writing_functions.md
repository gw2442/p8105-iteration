writing_functions
================
2022-10-29

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.403716717  0.444214900 -1.663844817  0.920853208 -1.055930197
    ##  [6] -0.246969727 -0.713873743 -1.312957163  0.062360471  1.555819566
    ## [11] -1.024040180  1.269606258  0.009541334 -1.209133423 -1.391476004
    ## [16]  0.841556625  0.152499311 -1.353927756  0.374528638  1.508949572
    ## [21] -0.044019161 -0.644732748 -0.365944657  0.280536779  1.724684968
    ## [26]  1.702061387  0.534053559  0.622869855 -0.922909104  0.349338966

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

    ##  [1] -0.403716717  0.444214900 -1.663844817  0.920853208 -1.055930197
    ##  [6] -0.246969727 -0.713873743 -1.312957163  0.062360471  1.555819566
    ## [11] -1.024040180  1.269606258  0.009541334 -1.209133423 -1.391476004
    ## [16]  0.841556625  0.152499311 -1.353927756  0.374528638  1.508949572
    ## [21] -0.044019161 -0.644732748 -0.365944657  0.280536779  1.724684968
    ## [26]  1.702061387  0.534053559  0.622869855 -0.922909104  0.349338966

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

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)){
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x) 
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

Check that the function works.

``` r
x_vec = rnorm(100, mean = 3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.91  4.45

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>%
  summarise(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.35  2.87

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 4) {
  
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

  sim_data %>%
    summarise(
      mean = mean(x),
      sd = sd(x)
  )
  
}

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.05  2.91

``` r
sim_mean_sd(mu = 6, samp_size = 100, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.30  2.96

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.30  4.23

## Let’s review Napolean Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of reviews…

Let’s turn that code into a function

``` r
read_page_reviews = function(url) {
  
  html = read_html(url)

  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()

  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() 
    str_extract("^\\d") %>%
    as.numeric()

  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() 

  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  reviews
}
```

Let me try my function.

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

read_page_reviews(dynamite_url)
```

Let’s read a few pages of reviews.

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

all_reviews =
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5])
    )
```

## Mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

## Functions as arguments

``` r
my_summary = function(x, sum_func) {
  
  sum_func(x)
  
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 3.549368

``` r
median(x_vec)
```

    ## [1] 4.400311

``` r
my_summary(x_vec, IQR)
```

    ## [1] 8.318797
