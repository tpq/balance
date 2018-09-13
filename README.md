
<!-- README.md is generated from README.Rmd. Please edit that file -->
Quick start
-----------

Welcome to the `balance` GitHub page!

Balances have become a cornerstone of compositional data analysis. However, conceptualizing balances is difficult, especially for high-dimensional data. Most often, investigators visualize balances with "balance dendrograms". However, this visualization tool does not scale well for large data. This package provides an alternative scheme for visualizing balances.

``` r
library(devtools)
devtools::install_github("tpq/balance")
library(balance)
?balance
```

Generating balance figures
--------------------------

We will demonstrate this package using an example from the `robCompositions` package. The "expenditures" matrix contains 20 compositions (row), each measuring 5 components (columns). As compositional data, the abundances are irrelevant and each composition sums to unity. The "y1" matrix is a serial binary partition (SBP) matrix that describes how to partition the 5 components into 4 balances.

``` r
data(expenditures, package = "robCompositions")
y1 <- data.frame(c(1, 1, 1, -1, -1), c(1, -1, -1, 0, 0),
                 c(0, +1, -1, 0, 0), c(0, 0, 0, +1, -1))
colnames(y1) <- paste0("z", 1:4)
```

With the data loaded, we can calculate and visualize the balances.

``` r
res <- balance.plot(expenditures, y1, size.text = 8)
```

Optionally, users can color components (in left figure) or samples (in right figure) based on a user-defined grouping. To do this, users must provide a vector of group labels for each component via the `d.group`argument (or for each sample via the `n.group` argument). Here, we color components and samples by user-defined groupings.

``` r
res <- balance.plot(expenditures, y1,
                    d.group = c("A", "B", "A", "B", "C"),
                    n.group = c(rep("A", 10), rep("B", 10)),
                    size.text = 8)
```

Further reading
---------------

To learn more about `balance`, please see the vignette and relevant literature.

``` r
citation("balance")
#> 
#> To cite balance in publications use:
#> 
#>   Quinn T. 2018. Visualizing balances of compositional data: A new
#>   alternative to balance dendrograms. F1000Research, 7:1278. URL
#>   http://f1000research.com/articles/7-1278/.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Visualizing balances of compositional data: A new alternative to balance dendrograms},
#>     author = {Thomas Quinn},
#>     journal = {F1000Research},
#>     year = {2018},
#>     volume = {7},
#>     number = {1278},
#>     url = {http://f1000research.com/articles/7-1278/},
#>   }
```
