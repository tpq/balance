## balance 0.1.9
---------------------
* Update `balance` methods
    * Check for equivalent names in `balance.fromSBP` call
    * No longer export `balance.fromContrast` function
    * Add `sbp.fromHclust` test

## balance 0.1.8
---------------------
* Add `sbp` methods
    * New `sbp.subset` returns named ternary or ratio balances
    * New `sbp.fromRandom` returns random binary tree
    * New `sbp.fromABA` returns modified PBA tree
    * New `sbp.fromPropd`returns theta tree

## balance 0.1.7
---------------------
* Update documentation
    * Place all `pba` functions within `?pba` help file
    * Make `pba` method help more accessible

## balance 0.1.6
---------------------
* Update `balance` methods
    * `balance` now returns an S4 object called `bplot`
    * `bplot` has `[[` method for backwards compatibility
    * `balance.fromContrast` now 2x faster
* Update documentation
    * An example is now provided for each function

## balance 0.1.5
---------------------
* Update `pba` methods
    * `pba.plot` no longer prints variance when `y` is provided
    * `vlr` now uses less peak RAM

## balance 0.1.4
---------------------
* Extend zero handling
    * `vlr` will now replace 0s with next smallest value
    * `balance.fromSBP` will now replace 0s with next smallest value
* Extend `pba` methods
    * New `pba.show` method shows object structure
    * New `pba.plot` method plots balances in 2D

## balance 0.1.3
---------------------
* Add functions to perform a principal balance analysis
    * New `vlr` function calculates log-ratio variance using matrix algebra
    * New `sbp.fromPBA` function builds an SBP of principal balances
    * New `pba` object
        * New `pba` function constructs `pba` object
        * New `predict` method deploys `pba` model

## balance 0.1.2
---------------------
* Add functions to create an SBP matrix
    * New `sbp.fromHclust` creates SBP matrix from `hclust` object
    * New `sbp.sort` sorts an SBP matrix predictably

## balance 0.1.1
---------------------
* Modify `balance.plot` function
    * All points now overlap with their boxplot when `boxplot.split = TRUE`
* Revise vignette and README
    * Use `balance.plot()` instead of `balance()`

## balance 0.1.0
---------------------
* Modify function names
    * Rename `balance()` to `balance.plot()`
    * Create `balance()` wrapper for backwards compatibility
    * Rename `balance.compute()` to `balance.fromContrast()`
    * Rename `balances()` to `balance.fromSBP()`

## balance 0.0.9
---------------------
* Add CITATION to package and DESCRIPTION

## balance 0.0.8
---------------------
* Update `balance` method
    * Remove `size.line` argument and do not set size for `!weigh.var`

## balance 0.0.7
---------------------
* Update `balance` method
    * Fix bug caused by using `n.group` in presence of unique rownames

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
