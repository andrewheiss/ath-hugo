---
title: Show multiply imputed results in a side-by-side regression table with broom and huxtable
date: 2018-03-08
year: "2018"
month: "2018/03"
description: Extend broom's tidy() and glance() to work with lists of multiply imputed regression models
tags: 
  - r
  - imputation
  - tidyverse
  - markdown
slug: amelia-broom-huxtable
math: True
---



<span class="small">([See this notebook on GitHub](https://github.com/andrewheiss/amelia-broom-huxtable))</span>

---

*tl;dr*: Use the functions in [`broomify-amelia.R`](broomify-amelia.R) to use `broom::tidy()`, `broom::glance()`, and `huxtable::huxreg()` on lists of multiply imputed models.

---

The whole reason I went into the rabbit hole of the mechanics of merging imputed regression results [in the previous post](https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/) was so I could easily report these results in papers and writeups. In political science and economics (and probably other social science disciplines), it's fairly standard to report many regression models in a side-by-side table, with a column for each model and rows for each coefficient. R packages like [`stargazer`](https://cran.r-project.org/package=stargazer) and [`huxtable`](https://cran.r-project.org/package=huxtable) make this fairly straightforward.



```r
library(tidyverse)
library(Amelia)
library(stargazer)
library(huxtable)
library(broom)

# Use the africa dataset from Ameila
data(africa)

# Build some example models
model_original1 <- lm(gdp_pc ~ trade + civlib, data = africa)
model_original2 <- lm(gdp_pc ~ trade + civlib + infl, data = africa)
```

Stargazer takes a list of models:


```r
stargazer(model_original1, model_original2, type = "text")
## 
## ====================================================================
##                                   Dependent variable:               
##                     ------------------------------------------------
##                                          gdp_pc                     
##                               (1)                      (2)          
## --------------------------------------------------------------------
## trade                      18.000***                18.500***       
##                             (1.270)                  (1.200)        
##                                                                     
## civlib                    -665.000***              -589.000***      
##                            (185.000)                (176.000)       
##                                                                     
## infl                                                -6.340***       
##                                                      (1.620)        
##                                                                     
## Constant                    136.000                 166.000*        
##                            (100.000)                (94.900)        
##                                                                     
## --------------------------------------------------------------------
## Observations                  115                      115          
## R2                           0.653                    0.695         
## Adjusted R2                  0.647                    0.687         
## Residual Std. Error    352.000 (df = 112)      332.000 (df = 111)   
## F Statistic         106.000*** (df = 2; 112) 84.500*** (df = 3; 111)
## ====================================================================
## Note:                                    *p<0.1; **p<0.05; ***p<0.01
```

As does huxtable's `huxreg()`:


```r
huxreg(model_original1, model_original2) %>%
  print_screen()
##                    ────────────────────────────────────────────────────
##                                                  (1)              (2)  
##                                     ───────────────────────────────────
##                      (Intercept)         136.063          166.435      
##                                         (100.409)         (94.871)     
##                      trade                18.027 ***       18.494 ***  
##                                           (1.272)          (1.204)     
##                      civlib             -665.428 ***     -588.722 **   
##                                         (185.436)        (175.717)     
##                      infl                                  -6.336 ***  
##                                                            (1.620)     
##                                     ───────────────────────────────────
##                      N                   115              115          
##                      R2                    0.653            0.695      
##                      logLik             -836.136         -828.709      
##                      AIC                1680.272         1667.418      
##                    ────────────────────────────────────────────────────
##                      *** p < 0.001; ** p < 0.01; * p < 0.05.           
## 
## Column names: names, model1, model2
```

[Stargazer has support for a ton of different model types](https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf) (see `?stargazer` for details), but they're all hardcoded into stargazer's internal code and adding more is tricky. [Huxtable](https://hughjonesd.github.io/huxtable/), on the other hand, doesn't rely on hardcoded model processing, but instead will display any model that works with `broom::tidy()` and `broom::glance()`. The [`broom` package](https://cran.r-project.org/package=broom) supports way more models than stargazer (including models created with [`rstan`](https://cran.r-project.org/package=rstan) and [`rstanarm`](https://cran.r-project.org/package=rstanarm)!), and because of this, huxtable is far more extensible—if you can create a `tidy()` and a `glance()` function for a type of model, huxtable can use it. 

Also, stargazer was written before [R Markdown](https://rmarkdown.rstudio.com/) was really a thing, so it has excellent support for HTML and LaTeX output, but that's it. Including stargazer tables in an R Markdown document is a hassle, especially if you want to be able to convert it to Word ([I've written a Python script for doing this](https://github.com/andrewheiss/edb-social-pressure/blob/master/bin/stargazer2docx.py)—that's how much extra work it takes). Huxtable, though, was written after the R Markdown and tidyverse revolutions, so it supports piping *and* can output to HTML, LaTeX, *and* Markdown (with `huxtable::print_md()`). 

This history is important because it means that models based on multiple imputation **will not work with stargazer.** Melding all the coefficients across imputations creates nice data frames of model results, but it doesn't create a model that stargazer can work with. This is unfortunate, especially given [how much I use stargazer](https://github.com/search?l=&q=stargazer+user%3Aandrewheiss&ref=advsearch&type=Code&utf8=%E2%9C%93). However, if we could make a `tidy()` and a `glance()` function that could work with a list of multiply imputed models, huxtable would solve all our problems.

So here's how to solve all your problems :)

First, we'll impute the missing data in the Africa data set, nest the imputed data in a larger data frame, and run a model on each imputed dataset:


```r
set.seed(1234)
imp_amelia <- amelia(x = africa, m = 5, cs = "country", ts = "year", 
                     logs = "gdp_pc", p2s = 0)

models_imputed_df <- bind_rows(unclass(imp_amelia$imputations), .id = "m") %>%
  group_by(m) %>%
  nest() %>% 
  mutate(model = data %>% map(~ lm(gdp_pc ~ trade + civlib, data = .)))

models_imputed_df
## # A tibble: 5 x 3
## # Groups:   m [5]
##   m     data               model 
##   <chr> <list>             <list>
## 1 imp1  <tibble [120 × 7]> <lm>  
## 2 imp2  <tibble [120 × 7]> <lm>  
## 3 imp3  <tibble [120 × 7]> <lm>  
## 4 imp4  <tibble [120 × 7]> <lm>  
## 5 imp5  <tibble [120 × 7]> <lm>
```

Before we do anything with the models in `models_imputed_df$model`, first we can define a few functions to extend broom. R's S3 object system means that a function named `whatever.blah()` will automatically work when called on objects with the class `blah`. This is how broom generally works—there are functions named `tidy.anova()`, `tidy.glm()`, `tidy.lm()`, etc. that will do the correct tidying when run on `anova`, `glm`, and `lm` objects. Huxtable also takes advantage of this S3 object system—it will call the appropriate tidy and glance functions based on the class of the models passed to it.

To make a list of models work with broom, we need to invent a new class of model. In this example I've named it `melded`, but it could be anything. Here are three functions designed to work on `melded` objects (the code for these is largely based on [the previous post about melding coefficients](https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/)). These functions are also found in [`broomify-amelia.R`](broomify-amelia.R), which you can add to your project (maybe someday this could be an actual package, but I don't see a reason for it yet).


```r
tidy.melded <- function(x, conf.int = FALSE, conf.level = 0.95) {
  # Get the df from one of the models
  model_degrees_freedom <- glance(x[[1]])$df.residual
  
  # Create matrices of the estimates and standard errors
  params <- tibble(models = unclass(x)) %>%
    mutate(m = 1:n(),
           tidied = models %>% map(tidy)) %>% 
    unnest(tidied) %>%
    select(m, term, estimate, std.error) %>%
    gather(key, value, estimate, std.error) %>%
    mutate(term = fct_inorder(term)) %>%  # Order the terms so that spread() keeps them in order
    spread(term, value)
  
  just_coefs <- params %>% filter(key == "estimate") %>% select(-m, -key)
  just_ses <- params %>% filter(key == "std.error") %>% select(-m, -key)
  
  # Meld the coefficients with Rubin's rules
  coefs_melded <- mi.meld(just_coefs, just_ses)
  
  # Create tidy output
  output <- as.data.frame(cbind(t(coefs_melded$q.mi),
                                t(coefs_melded$se.mi))) %>%
    magrittr::set_colnames(c("estimate", "std.error")) %>%
    mutate(term = rownames(.)) %>%
    select(term, everything()) %>%
    mutate(statistic = estimate / std.error,
           p.value = 2 * pt(abs(statistic), model_degrees_freedom, lower.tail = FALSE))
  
  # Add confidence intervals if needed
  if (conf.int & conf.level) {
    # Convert conf.level to tail values (0.025 when it's 0.95)
    a <- (1 - conf.level) / 2
    
    output <- output %>% 
      mutate(conf.low = estimate + std.error * qt(a, model_degrees_freedom),
             conf.high = estimate + std.error * qt((1 - a), model_degrees_freedom))
  }
  
  # tidy objects only have a data.frame class, not tbl_df or anything else
  class(output) <- "data.frame"
  output
}

glance.melded <- function(x) {
  # Because the properly melded parameters and the simple average of the
  # parameters of these models are roughly the same (see
  # https://www.andrewheiss.com/blog/2018/03/07/amelia-tidy-melding/), for the
  # sake of simplicty we just take the average here
  output <- tibble(models = unclass(x)) %>%
    mutate(glance = models %>% map(glance)) %>%
    unnest(glance) %>%
    summarize_at(vars(r.squared, adj.r.squared, sigma, statistic, p.value, df, 
                      logLik, AIC, BIC, deviance, df.residual),
                 list(mean)) %>%
    mutate(m = as.integer(length(x)))
  
  # glance objects only have a data.frame class, not tbl_df or anything else
  class(output) <- "data.frame"
  output
}

nobs.melded <- function(x, ...) {
  # Take the number of observations from the first model
  nobs(x[[1]])
}
```

With these three functions, we can now use `glance()` and `tidy()` on a list of models with the class `melded`, like so:


```r
# Extract the models into a vector and make it a "melded" class
models_imputed <- models_imputed_df$model
# Without this, R won't use our custom tidy.melded() or glance.melded() functions
class(models_imputed) <- "melded"
glance(models_imputed)
##   r.squared adj.r.squared sigma statistic  p.value df logLik  AIC  BIC deviance
## 1     0.657         0.651   348       112 9.68e-28  2   -871 1750 1761 14154815
##   df.residual m
## 1         117 5
tidy(models_imputed)
##          term estimate std.error statistic  p.value
## 1 (Intercept)    120.6     96.67      1.25 2.15e-01
## 2       trade     18.1      1.24     14.63 3.45e-28
## 3      civlib   -637.8    181.13     -3.52 6.13e-04
```

Even better, though, is that we can use these imputed models in a huxtable regression table. And, because I included a column named `m` in `glance.melded()`, we can also include it in the regression output!


```r
huxreg(model_original1, model_original2, models_imputed,
       statistics = c(N = "nobs", R2 = "r.squared", `Adj R2` = "adj.r.squared", 
                      "logLik", "AIC", "m")) %>% 
  print_screen()
##                ────────────────────────────────────────────────────────────
##                                         (1)            (2)            (3)  
##                              ──────────────────────────────────────────────
##                  (Intercept)    136.063        166.435        120.591      
##                                (100.409)       (94.871)       (96.669)     
##                  trade           18.027 ***     18.494 ***     18.089 ***  
##                                  (1.272)        (1.204)        (1.237)     
##                  civlib        -665.428 ***   -588.722 **    -637.832 ***  
##                                (185.436)      (175.717)      (181.130)     
##                  infl                           -6.336 ***                 
##                                                 (1.620)                    
##                              ──────────────────────────────────────────────
##                  N              115            115            120          
##                  R2               0.653          0.695          0.657      
##                  Adj R2           0.647          0.687          0.651      
##                  logLik        -836.136       -828.709       -870.954      
##                  AIC           1680.272       1667.418       1749.908      
##                  m                                              5.000      
##                ────────────────────────────────────────────────────────────
##                  *** p < 0.001; ** p < 0.01; * p < 0.05.                   
## 
## Column names: names, model1, model2, model3
```
