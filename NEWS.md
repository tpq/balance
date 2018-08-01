## balance 0.0.6
---------------------
* Update `balance` method
    * Use `weigh.var` argument to weigh line width by explained variance
    * Use `size.line` argument to alter line width factor

## balance 0.0.5
---------------------
* Update `balance` method
    * Compute balances using `exp(mean(log(x)))` to prevent overflow
    * Remove "linetype" aesthetic from partition figure

## balance 0.0.4
---------------------
* Update `balance` method
    * Re-write `robCompositions::balances` call as `balances` function
    * Re-write `reshape2::melt` call as `wide2long` function

## balance 0.0.3
---------------------
* Update `balance` method
    * Coerce input as `data.frame` to prevent errors

## balance 0.0.2
---------------------
* Update `balance` method
    * Fix the automatic sorting of serial binary partition matrix
    * Add test that sorting does not alter results

## balance 0.0.1
---------------------
* Create package skeleton
    * Add `balance.combine` function to merge figures
    * Add `balance` function to visualize balances
    * Complete package DESCRIPTION
    * Create README
