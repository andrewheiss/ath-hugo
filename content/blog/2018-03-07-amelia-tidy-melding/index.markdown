---
title: Meld regression output from multiple imputations with tidyverse
date: 2018-03-07
year: "2018"
month: "2018/03"
description: Use tidyverse functions to correctly meld and pool multiply imputed model output.
tags: 
  - r
  - imputation
  - tidyverse
slug: amelia-tidy-melding
math: True
---



<span class="small">([See this notebook on GitHub](https://github.com/andrewheiss/amelia-tidy-melding))</span>

---

Missing data can significantly influence the results of normal regression models, since the default in R and most other statistical packages is to throw away any rows with missing variables. To avoid unnecessarily throwing out data, it's helpful to impute missing values. One of the best ways to do this is to build a separate regression model to make predictions that fill in the gaps in data. This isn't always accurate, so it's best to make many iterations of predictions (in imputation parlance, `\(m\)` is the number of imputations done to a dataset). After making `\(m\)` datasets, you can use this data by (1) running statistical tests on each imputation individually and then (2) pooling those results into a single number. The [excellent Amelia vignette](https://cran.r-project.org/web/packages/Amelia/vignettes/amelia.pdf) details the theory and mechanics of how to use multiple imputation, and it's a fantastic resource.

There are several packages for dealing with missing data in R, including [`mi`](https://cran.r-project.org/package=mi), [`mice`](https://cran.r-project.org/package=mice), and [`Amelia`](https://cran.r-project.org/package=Amelia), and Thomas Leeper has [a short overview of how to use all three](http://thomasleeper.com/Rcourse/Tutorials/mi.html). I'm partial to [Amelia](https://gking.harvard.edu/amelia), since it's designed to work well with time series-cross sectional data and can deal with complicated features like country-year observations. 

Because Amelia is written by Gary King, et al., it works with [Zelig](https://zeligproject.org/), a separate framework that's designed to simplify modeling in R. With Zelig + Amelia, you can combine all of the `\(m\)` imputations automatically with whatever Zelig uses for printing model results. I'm not a huge fan of Zelig, though, and I prefer using `lm()`, `glm()`, `stan_glm()`, and gang on my own, thank you very much.

However, doing it on my own means there's a little more work involved with combining coefficients and parameters across imputations. Fortunately, the [tidyverse](https://www.tidyverse.org/)—specifically its ability to store models within data frames—makes it really easy to deal with models based on imputed data. Here's how to do it using tidy functions. The code for this whole process can be greatly simplified in real life. You technically don't need all these intermediate steps, though they're helpful for seeing what's going on behind the scenes. 

We'll start by working with some basic example imputed data frame from Amelia's built-in data. We create 5 imputed datasets defining countries and years as cross sections and time series, and we log GDP per capita in the predictive model:


```r
library(tidyverse)
library(Amelia)
library(broom)

set.seed(1234)
data(africa)
imp_amelia <- amelia(x = africa, m = 5, cs = "country", ts = "year", 
                     logs = "gdp_pc", p2s = 0)
```

The resulting object contains a list of data frames, and each imputed dataset is stored in a list slot named "imputations" or `imp_amelia$imputations`. We can combine these all into one big data frame with `bind_rows()`, group by the imputation number ($m$), and nest them into imputation-specific rows:


```r
# unclass() is necessary because bind_rows() will complain when dealing with
# lists with the "amelia" class, which is what amelia() returns
all_imputations <- bind_rows(unclass(imp_amelia$imputations), .id = "m") %>%
  group_by(m) %>%
  nest()

all_imputations
## # A tibble: 5 x 2
## # Groups:   m [5]
##   m     data              
##   <chr> <list>            
## 1 imp1  <tibble [120 × 7]>
## 2 imp2  <tibble [120 × 7]>
## 3 imp3  <tibble [120 × 7]>
## 4 imp4  <tibble [120 × 7]>
## 5 imp5  <tibble [120 × 7]>
```

With this nested data, we can use `purrr::map()` to run models and return tidy summaries of those models directly in the data frame:


```r
models_imputations <- all_imputations %>%
  mutate(model = data %>% map(~ lm(gdp_pc ~ trade + civlib, data = .)),
         tidied = model %>% map(~ tidy(., conf.int = TRUE)),
         glance = model %>% map(~ glance(.)))

models_imputations
## # A tibble: 5 x 5
## # Groups:   m [5]
##   m     data               model  tidied           glance           
##   <chr> <list>             <list> <list>           <list>           
## 1 imp1  <tibble [120 × 7]> <lm>   <tibble [3 × 7]> <tibble [1 × 12]>
## 2 imp2  <tibble [120 × 7]> <lm>   <tibble [3 × 7]> <tibble [1 × 12]>
## 3 imp3  <tibble [120 × 7]> <lm>   <tibble [3 × 7]> <tibble [1 × 12]>
## 4 imp4  <tibble [120 × 7]> <lm>   <tibble [3 × 7]> <tibble [1 × 12]>
## 5 imp5  <tibble [120 × 7]> <lm>   <tibble [3 × 7]> <tibble [1 × 12]>
```

Having the models structured like this makes it easy to access coefficients for models from individual imputations, like so:


```r
models_imputations %>%
  filter(m == "imp1") %>%
  unnest(tidied)
## # A tibble: 3 x 11
## # Groups:   m [1]
##   m     data   model term  estimate std.error statistic  p.value conf.low conf.high glance
##   <chr> <list> <lis> <chr>    <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl> <list>
## 1 imp1  <tibb… <lm>  (Int…    114.      97.7       1.17 2.44e- 1    -79.0     308.  <tibb…
## 2 imp1  <tibb… <lm>  trade     18.1      1.25     14.4  9.65e-28     15.6      20.6 <tibb…
## 3 imp1  <tibb… <lm>  civl…   -631.     182.       -3.46 7.47e- 4   -993.     -270.  <tibb…
```

More importantly, we can access the coefficients for all the models, which is essential for combining and averaging the coefficients across all five imputations.

Pooling or melding coefficients from many models is a little trickier than just averaging them all together (as delightfully easy as that would be). [Donald Rubin (1987)](https://doi.org/10.1002/9780470316696) outlines an algorithm/set of rules for combining the results from multiply imputed datasets that reflects the averages and accounts for differences in standard errors. Rubin's rules are essentially a fancier, more robust way of averaging coefficients and other quantities of interest across imputations.

Amelia has a built-in function for using Rubin's rules named `mi.meld()` that accepts two m-by-k matrices (one for coefficients and one for standard errors) like so:

```text
      coef1  coef2  coefn
imp1  x      x      x
imp2  x      x      x
impn  x      x      x
```

We can use some dplyr/tidyr magic to wrangle the regression results into this form:


```r
# Create a wide data frame of just the coefficients and standard errors
params <- models_imputations %>%
  unnest(tidied) %>%
  select(m, term, estimate, std.error) %>%
  gather(key, value, estimate, std.error) %>%
  spread(term, value) %>% 
  ungroup()
params
## # A tibble: 10 x 5
##    m     key       `(Intercept)` civlib trade
##    <chr> <chr>             <dbl>  <dbl> <dbl>
##  1 imp1  estimate          114.   -631. 18.1 
##  2 imp1  std.error          97.7   182.  1.25
##  3 imp2  estimate          123.   -626. 18.0 
##  4 imp2  std.error          96.8   181.  1.24
##  5 imp3  estimate          114.   -633. 18.2 
##  6 imp3  std.error          96.5   181.  1.24
##  7 imp4  estimate          119.   -651. 18.2 
##  8 imp4  std.error          95.4   180.  1.22
##  9 imp5  estimate          132.   -648. 18.0 
## 10 imp5  std.error          95.2   180.  1.22
```


```r
# Extract just the coefficients
just_coefs <- params %>%
  filter(key == "estimate") %>%
  select(-m, -key)
just_coefs
## # A tibble: 5 x 3
##   `(Intercept)` civlib trade
##           <dbl>  <dbl> <dbl>
## 1          114.  -631.  18.1
## 2          123.  -626.  18.0
## 3          114.  -633.  18.2
## 4          119.  -651.  18.2
## 5          132.  -648.  18.0
```


```r
# Extract just the standard errors
just_ses <- params %>%
  filter(key == "std.error") %>%
  select(-m, -key)
just_ses
## # A tibble: 5 x 3
##   `(Intercept)` civlib trade
##           <dbl>  <dbl> <dbl>
## 1          97.7   182.  1.25
## 2          96.8   181.  1.24
## 3          96.5   181.  1.24
## 4          95.4   180.  1.22
## 5          95.2   180.  1.22
```

We can then use these matrices in `mi.meld()`, which returns a list with two slots—`q.mi` and `se.mi`:


```r
coefs_melded <- mi.meld(just_coefs, just_ses)
coefs_melded
## $q.mi
##      (Intercept) civlib trade
## [1,]         121   -638  18.1
## 
## $se.mi
##      (Intercept) civlib trade
## [1,]        96.7    181  1.24
```

Armed with these, we can create our regression summary table with some more dplyr wizardry. To calculate the p-value and confidence intervals, we need to extract the degrees of freedom from one of the imputed models


```r
model_degree_freedom <- models_imputations %>%
  unnest(glance) %>%
  filter(m == "imp1") %>%
  pull(df.residual)

melded_summary <- as.data.frame(cbind(t(coefs_melded$q.mi),
                                      t(coefs_melded$se.mi))) %>%
  magrittr::set_colnames(c("estimate", "std.error")) %>%
  mutate(term = rownames(.)) %>%
  select(term, everything()) %>%
  mutate(statistic = estimate / std.error,
         conf.low = estimate + std.error * qt(0.025, model_degree_freedom),
         conf.high = estimate + std.error * qt(0.975, model_degree_freedom),
         p.value = 2 * pt(abs(statistic), model_degree_freedom, lower.tail = FALSE))

melded_summary
##          term estimate std.error statistic conf.low conf.high  p.value
## 1 (Intercept)    120.6     96.67      1.25    -70.9     312.0 2.15e-01
## 2      civlib   -637.8    181.13     -3.52   -996.6    -279.1 6.13e-04
## 3       trade     18.1      1.24     14.63     15.6      20.5 3.45e-28
```

Hooray! Correctly melded coefficients and standard errors!

But what do we do about the other model details, like `\(R^2\)` and the F-statistic? How do we report those?

According to [a post on the Amelia mailing list](https://lists.gking.harvard.edu/pipermail/amelia/2016-July/001249.html), there are two ways. First, we can use a fancy method for combining `\(R^2\)` and adjusted `\(R^2\)` described by [Ofer Harel (2009)](https://doi.org/10.1080/02664760802553000). Second, we can just take the average of the `\(R^2\)`s from all the imputed models. The results should be roughly the same.

Harel's method involves two steps:

1. In each complete data set, calculate the `\(R^2\)`, take its square root ($R$), transform `\(R\)` with a Fisher z-transformation ($Q = \frac{1}{2} \log_{e}(\frac{1 + R}{1 - R})$), and calculate the variance of `\(R^2\)` (which is `\(\frac{1}{\text{degrees of freedom}}\)`)
2. Meld the resulting `\(Q\)` and variance using Rubin's rules (`mi.meld()`; this creates `\(Q_a\)`), undo the z-transformation ($R_a = (\frac{-1 + \exp(2Q_a)}{1 + \exp(2Q_a)})^2$), and square it ($R_a^2$)

That looks complicated, but it's fairly easy with some dplyr magic. Here's how to do it for adjusted `\(R^2\)` (the same process works for regular `\(R^2\)` too):


```r
# Step 1: in each complete data set, calculate R2, take its square root,
# transform it with Fisher z-transformation, and calculate the variance of R2\
r2s <- models_imputations %>%
  unnest(glance) %>%
  select(m, adj.r.squared, df.residual) %>%
  mutate(R = sqrt(adj.r.squared),  # Regular R
         Q = 0.5 * log((R + 1) / (1 - R)),  # Fisher z-transformation
         se = 1 / df.residual)  # R2 variance
r2s
## # A tibble: 5 x 6
## # Groups:   m [5]
##   m     adj.r.squared df.residual     R     Q      se
##   <chr>         <dbl>       <int> <dbl> <dbl>   <dbl>
## 1 imp1          0.643         117 0.802  1.10 0.00855
## 2 imp2          0.648         117 0.805  1.11 0.00855
## 3 imp3          0.652         117 0.807  1.12 0.00855
## 4 imp4          0.660         117 0.812  1.13 0.00855
## 5 imp5          0.654         117 0.808  1.12 0.00855
```


```r
# Step 2: combine the results using Rubin's rules (mi.meld()), inverse transform
# the value, and square it

# Meld the R2 values with mi.meld()
Q_melded <- mi.meld(as.matrix(r2s$Q), as.matrix(r2s$se))

# Inverse transform Q to R and square it
r2_melded <- ((exp(2 * Q_melded$q.mi) - 1) / (1 + exp(2 * Q_melded$q.mi)))^2
r2_melded
##       [,1]
## [1,] 0.651
```

The correctly pooled/melded `\(R^2\)` is thus 0.651. Neat.

How does this compare to just the average of all the `\(R^2\)`s from all the imputations?


```r
r2s_avg <- models_imputations %>%
  ungroup() %>% 
  unnest(glance) %>%
  summarize(adj.r.squared_avg = mean(adj.r.squared)) %>%
  pull(adj.r.squared_avg)
r2s_avg
## [1] 0.651
```

The incorrectly averaged `\(R^2\)` is 0.651, which is basically identical to the correctly melded 0.651. This is probably because the models from the five imputed models are already fairly similar—there might be more variance in `\(R^2\)` in data that's less neat. But for this situation, the two approaches are essentially the same. Other model diagnostics like the F-statistic can probably be pooled just with averages as well. I haven't found any specific algorithms for melding them with fancy math. 

So, in summary, combine the coefficients and standard errors from multiply imputed models with `mi.meld()` and combine other model parameters like `\(R^2\)` either with Harel's fancy method or by simply averaging them.
