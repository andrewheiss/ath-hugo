---
title: Fun with empirical and function-based derivatives in R
date: 2018-02-15
year: "2018"
month: "2018/02"
description: Use R to do things with derivatives, both with actual functions and with existing empirical data.
images: 
- /blog/2018/02/15/derivatives-r-fun/plot-all-empirical-1.png
tags: 
  - r
  - ggplot
  - dataviz
  - economics
slug: derivatives-r-fun
math: True
---
<script src="{{< relref "blog/2018-02-15-derivatives-r-fun/index.markdown" >}}index_files/kePrint-0.0.1/kePrint.js"></script>
<link href="{{< relref "blog/2018-02-15-derivatives-r-fun/index.markdown" >}}index_files/lightable-0.0.1/lightable.css" rel="stylesheet" />



<span class="small">([See this notebook on GitHub](https://github.com/andrewheiss/derivatives-r-fun))</span>

---

*tl;dr*: Use functions like `Deriv::Deriv()`, `splinefun()`, `approxfun()`, and `uniroot()` to do things with derivatives in R, both with actual functions and with existing empirical data

---

A typical microeconomics problem involves finding the optimal price and quantity of a product, given its demand and cost across different quantities. You can optimize this price and quantity and maximize profit by finding the point where the marginal cost and the marginal revenue (or the first derivatives of the cost and revenue functions) are equal to each other.

For instance, the demand for some product can be defined as `\(Q = 10 - 2P\)` (where `\(Q =\)` quantity and `\(P =\)` price). The revenue you get from selling that product is defined as `\(R = PQ\)` (just multiplying price × quantity), so through some algebraic trickery and rearranging of Ps and Qs, you can create a revenue function for this demand curve: `\(R = 5Q - 0.5Q^2\)`. The cost function for this product can be defined as `\(C = 0.25Q + 0.5Q^2\)`.

To figure out the optimal profit, we set the marginal cost and marginal revenue equations equal to each other and solve for Q. Here, `\(\frac{dC}{dQ} = MC = 0.25 + 0.5Q\)` and `\(\frac{dR}{dQ} = MR = 5 - Q\)`, so with algebra we can find the optimal point:

$$
`\begin{aligned}
MC &= MR \\
0.25 + 0.5Q &= 5 - Q \\
1.5Q &= 4.75 \\
Q &= 3.1\overline{66}
\end{aligned}`
$$

Phew. Calculus.

Doing this in R is fairly straightforward and far more flexible and far less algebra-intensive. First, define the functions:


```r
library(tidyverse)
library(Deriv)
library(pander)

demand <- function(q) 5 - (0.5 * q)
revenue <- function(q) (5 - 0.5 * q) * q

cost <- function(q) (0.25 * q) + (0.5 * q)^2
```

Plotting these functions is easy with `geom_function()`: 


```r
ggplot(data = tibble(x = 0:10), aes(x = x)) +
  geom_function(fun = cost, size = 1, aes(color = "Total cost")) +
  geom_function(fun = revenue, size = 1, aes(color = "Total revenue")) +
  labs(x = "Quantity", y = "Price") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(values = c("Total cost" = "red", "Total revenue" = "blue"),
                     name = "Function") +
  theme_light() +
  theme(legend.position = "bottom")
```

<img src="{{< relref "blog/2018-02-15-derivatives-r-fun/index.markdown" >}}index_files/figure-html/plot-functions-1.png" width="75%" style="display: block; margin: auto;" />

Then, using `Deriv::Deriv()`, create derivative functions for the marginal cost and marginal revenue equations:


```r
mr <- Deriv(revenue, "q")
mc <- Deriv(cost, "q")
```

We can also plot these:


```r
ggplot(data = tibble(x = 0:10), aes(x = x)) +
  geom_function(fun = mc, size = 1, aes(color = "Marginal cost")) +
  geom_function(fun = mr, size = 1, aes(color = "Marginal revenue")) +
  labs(x = "Quantity", y = "Price") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(values = c("Marginal cost" = "red", "Marginal revenue" = "blue"),
                     name = "Function") +
  coord_cartesian(ylim = c(0, 6)) +
  theme_light() +
  theme(legend.position = "bottom")
```

<img src="{{< relref "blog/2018-02-15-derivatives-r-fun/index.markdown" >}}index_files/figure-html/plot-marginal-functions-1.png" width="75%" style="display: block; margin: auto;" />

Finally, use the `uniroot()` function to look for the point where `mc` and `mr` intersect within a given range (here I'm looking between 1 and 10 since the demand curve goes negative after `\(Q =\)` 10):


```r
optimal_q <- uniroot(function(x) mc(x) - mr(x), c(1, 10))
optimal_q$root
## [1] 3.166667
```

It's the same answer!

We can then plug `optimal_q$root` back into the marginal revenue and demand functions to find the optimal price (in a competitive market, the price should be equal to the marginal revenue, but this happens to be a monopoly, so the actual price is higher, but that's totally unrelated to the topic here):


```r
mr(optimal_q$root)
## [1] 1.833333
demand(optimal_q$root)
## [1] 3.416667
# oh noes monopolies
```

**However! Wait! Stop!** This is all well and fine if you have precise formulas for demand and cost. But real life is far messier than this. What if you don't know the underlying equations?

Often in economics, you have a set of quantities and prices based on empirical data. Market research and surveys can estimate the demand for a product, and tracking how fixed and variable costs change over time can estimate the costs for a product, but this data is all empirically based and not based in actual formulas.

For instance, suppose you have this table of prices, quantities, and costs (which is actually really based on the demand and cost functions from earlier):


```r
costs_revenues <- tibble(Quantity = seq(0, 10, 1),
                         Price = demand(Quantity),
                         `Total Revenue` = revenue(Quantity),
                         `Total Cost` = cost(Quantity),
                         Profit = `Total Revenue` - `Total Cost`)
```

<table class=" pure-table pure-table-horizontal" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Quantity </th>
   <th style="text-align:left;"> Price </th>
   <th style="text-align:left;"> Total Revenue </th>
   <th style="text-align:left;"> Total Cost </th>
   <th style="text-align:left;"> Profit </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> $5.00 </td>
   <td style="text-align:left;"> $0.00 </td>
   <td style="text-align:left;"> $0.00 </td>
   <td style="text-align:left;"> $0.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> $4.50 </td>
   <td style="text-align:left;"> $4.50 </td>
   <td style="text-align:left;"> $0.50 </td>
   <td style="text-align:left;"> $4.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> $4.00 </td>
   <td style="text-align:left;"> $8.00 </td>
   <td style="text-align:left;"> $1.50 </td>
   <td style="text-align:left;"> $6.50 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> $3.50 </td>
   <td style="text-align:left;"> $10.50 </td>
   <td style="text-align:left;"> $3.00 </td>
   <td style="text-align:left;"> $7.50 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> $3.00 </td>
   <td style="text-align:left;"> $12.00 </td>
   <td style="text-align:left;"> $5.00 </td>
   <td style="text-align:left;"> $7.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> $2.50 </td>
   <td style="text-align:left;"> $12.50 </td>
   <td style="text-align:left;"> $7.50 </td>
   <td style="text-align:left;"> $5.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> $2.00 </td>
   <td style="text-align:left;"> $12.00 </td>
   <td style="text-align:left;"> $10.50 </td>
   <td style="text-align:left;"> $1.50 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:left;"> $1.50 </td>
   <td style="text-align:left;"> $10.50 </td>
   <td style="text-align:left;"> $14.00 </td>
   <td style="text-align:left;"> -$3.50 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:left;"> $1.00 </td>
   <td style="text-align:left;"> $8.00 </td>
   <td style="text-align:left;"> $18.00 </td>
   <td style="text-align:left;"> -$10.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:left;"> $0.50 </td>
   <td style="text-align:left;"> $4.50 </td>
   <td style="text-align:left;"> $22.50 </td>
   <td style="text-align:left;"> -$18.00 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:left;"> $0.00 </td>
   <td style="text-align:left;"> $0.00 </td>
   <td style="text-align:left;"> $27.50 </td>
   <td style="text-align:left;"> -$27.50 </td>
  </tr>
</tbody>
</table>

We can still use R to find the optimal quantity, ***even without actual formulas***. R has two base functions for approximating functions based on existing data. `approxfun()` will try to fit data linearly, and `splinefun()` will try to fit data with cubic splines (i.e. it can handle curvy lines better than `approxfun()`).

First, we can plot the revenue and cost columns to see their shape:


```r
costs_revenues_plot <- costs_revenues %>% 
  select(Quantity, starts_with("Total")) %>% 
  gather(Variable, Price, -Quantity)

ggplot(costs_revenues_plot, aes(x = Quantity, y = Price, color = Variable)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(values = c("red", "blue")) +
  theme_light() +
  theme(legend.position = "bottom")
```

<img src="{{< relref "blog/2018-02-15-derivatives-r-fun/index.markdown" >}}index_files/figure-html/empirical-cost-revenue-1.png" width="75%" style="display: block; margin: auto;" />

Because both variables are curvilinear, it's probably best to approximate their functions using splines with `splinefun()`:


```r
cost_empirical <- splinefun(x = costs_revenues$Quantity, 
                            y = costs_revenues$`Total Cost`)

revenue_empirical <- splinefun(x = costs_revenues$Quantity, 
                               y = costs_revenues$`Total Revenue`)
```

If we compare the empirically-based functions with their real-life counterparts, we can see that the approximation worked great:


```r
cost(1:10)
##  [1]  0.5  1.5  3.0  5.0  7.5 10.5 14.0 18.0 22.5 27.5
cost_empirical(1:10)
##  [1]  0.5  1.5  3.0  5.0  7.5 10.5 14.0 18.0 22.5 27.5

revenue(1:10)
##  [1]  4.5  8.0 10.5 12.0 12.5 12.0 10.5  8.0  4.5  0.0
revenue_empirical(1:10)
##  [1]  4.5  8.0 10.5 12.0 12.5 12.0 10.5  8.0  4.5  0.0
```

Determining the marginal cost and revenue functions from these approximations is surprisingly easy because `splinefun()` objects have a built-in mechanism for returning derivatives with a `deriv` argument:


```r
mc(1:10)
##  [1] 0.75 1.25 1.75 2.25 2.75 3.25 3.75 4.25 4.75 5.25
cost_empirical(1:10, deriv = 1)
##  [1] 0.75 1.25 1.75 2.25 2.75 3.25 3.75 4.25 4.75 5.25

mr(1:10)
##  [1]  4  3  2  1  0 -1 -2 -3 -4 -5
revenue_empirical(1:10, deriv = 1)
##  [1]  4  3  2  1  0 -1 -2 -3 -4 -5
```

Magic!

We can plot these empirically-approximated marginal functions and see that they intersect, as expected:


```r
ggplot(data = tibble(x = 0:10), aes(x = x)) +
  geom_function(fun = cost_empirical, size = 1, args = list(deriv = 1),
                aes(color = "Marginal cost")) +
  geom_function(fun = revenue_empirical, size = 1, args = list(deriv = 1),
                aes(color = "Marginal revenue")) +
  labs(x = "Quantity", y = "Price") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(values = c("Marginal cost" = "red", "Marginal revenue" = "blue"),
                     name = "Empirical function") +
  coord_cartesian(ylim = c(0, 6)) +
  theme_light() +
  theme(legend.position = "bottom")
```

<img src="{{< relref "blog/2018-02-15-derivatives-r-fun/index.markdown" >}}index_files/figure-html/plot-empirical-marginal-functions-1.png" width="75%" style="display: block; margin: auto;" />

Finally, we can use `uniroot()` to find where these two functions intersect:


```r
optimal_q_empirical <- uniroot(function(x) cost_empirical(x, deriv = 1) - 
                                 revenue_empirical(x, deriv = 1), c(1, 10))
optimal_q_empirical$root
## [1] 3.166667
```

It's the same!

And just like before, we can find the optimal price, given this quantity. But first we have to create an empirical function for the demand. The demand variable is linear here, so we can use `approxfun()`, but `splinefun()` works just fine too (and it has built-in derivative capabilities, while `approxfun()` doesn't).


```r
revenue_empirical(optimal_q_empirical$root, deriv = 1)
## [1] 1.833333

demand_empricial_spline <- splinefun(x = costs_revenues$Quantity,
                                     y = costs_revenues$Price)

demand_empricial_approx <- approxfun(x = costs_revenues$Quantity,
                                     y = costs_revenues$Price)

demand_empricial_spline(optimal_q_empirical$root)
## [1] 3.416667
demand_empricial_approx(optimal_q_empirical$root)
## [1] 3.416667
# oh noes monopolies again
```

We can plot all of these things together:


```r
ggplot(data = tibble(x = 0:10), aes(x = x)) +
  geom_function(fun = demand_empricial_spline, size = 1,
                aes(color = "Demand")) +
  geom_function(fun = cost_empirical, size = 1, args = list(deriv = 1),
                aes(color = "Marginal cost")) +
  geom_function(fun = revenue_empirical, size = 1, args = list(deriv = 1),
                aes(color = "Marginal revenue")) +
  geom_vline(xintercept = optimal_q_empirical$root, 
             color = "grey50", linetype = "dashed") +
  geom_hline(yintercept = revenue_empirical(optimal_q_empirical$root, deriv = 1), 
             color = "grey50", linetype = "dashed") +
  labs(x = "Quantity", y = "Price") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(values = c("Marginal cost" = "red", "Marginal revenue" = "blue",
                                "Demand" = "darkgreen"),
                     name = "Function") +
  coord_cartesian(ylim = c(0, 6)) +
  theme_light() +
  theme(legend.position = "bottom")
```

<img src="{{< relref "blog/2018-02-15-derivatives-r-fun/index.markdown" >}}index_files/figure-html/plot-all-empirical-1.png" width="75%" style="display: block; margin: auto;" />

In this case, the empirical solution and the function-based solution are identical, but that's only because I created the empirical data from the functions. In real life, though, this same process should work on any empirical price, quantity, and cost data.
