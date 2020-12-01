## Resubmission
This is a resubmission because ver. 1.0.2 was archived on 2020-10-02 as check issues were not corrected in time.
Fixed/removed links.
Fiexed missing '()' after 'return' in the function 'inputchecks'.

## Possibly mis-spelled words in DESCRIPTION
"indices" and "survivorship" are correct.

## Increment patch
This is an increment minor. In this version we have:

- adding an option for plotting in color instead of linetype
- adding more functions for checking representativity of age data, calculating reproduction indices, masculinity index, maternal mortality rate as well as corrected life tables
- major re-factoring
- rewriting of tests to work with testthat's new snapshot-functionality
- adding two vignettes for life table correction and reproduction indices


## Test environments
* local OS X install, R 4.0.3
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.
There was one NOTE on "checking CRAN incoming feasibility".

## Reverse dependencies
There are no reverse dependencies.
