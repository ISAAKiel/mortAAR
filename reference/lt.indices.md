# Generates indices from a mortAAR life table for use in other functions

This function bundles a number of indices and vectors from a mortAAR
life table which are needed by other functions in mortAAR. The results
are only meaningful if 5-year-categories have been chosen to construct
the life table.

## Usage

``` r
lt.indices(life_table)
```

## Arguments

- life_table:

  an object of class mortaar_life_table.

## Value

A list with the following indices and vectors:

- **child_i**: ratio of individuals aged 5–9 to those aged 10–14
  according to *Masset and Bocquet 1977*.

- **juvenile_i**: ratio of individuals aged 5–14 to adults according to
  *Masset and Bocquet 1977*.

- **P(5-19)**: ratio of individuals aged 5–19 to those aged 5 or above
  according to *Bocquet-Appel 2002*.

- **D30_D5)**: ratio of individuals aged 30 and above to those aged 5 or
  above according to *Buikstra et al. 1986*.

- **D0_14_D**: proportion of individuals aged 0–14 according to
  *McFadden & Oxenham 2018a* if infants are represented well.

- **D15_49_D15plus**: proportion of individuals aged 15–49 according to
  *Taylor & Oxenham 2024*.

- **e0**: life expectancy at age 0.

## References

Bocquet-Appel J (2002). “Paleoanthropological Traces of a Neolithic
Demographic Transition.” *Current Anthropology*, **43**(4), 637–650.

Masset C, Bocquet J (1977). “Estimateurs en paléodémographie.”
*L’Homme*, **17**(4), 65–90.

Buikstra JE, Konigsberg LW, Bullington J (1986). “Fertility and the
Development of Agriculture in the Prehistoric Midwest.” *American
Antiquity*, **51**(3), 528–546.

McFadden C, Oxenham MF (2018). “The D0-14/D ratio: A new
paleodemographic index and equation for estimating total fertility
rates.” *American Journal of Physical Anthropology*, **165**(3),
471–479.

Taylor B, Oxenham M (2024). “A method for detecting bias in human
archaeological cemetery samples.” *International Journal of
Osteoarchaeology*, 0.
[doi:10.1002/oa.3379](https://doi.org/10.1002/oa.3379) .

## Examples

``` r
schleswig <- life.table(schleswig_ma[c("a", "Dx")])
lt.indices(schleswig)
#> $child_i
#> [1] 1.833333
#> 
#> $d5_9
#> [1] 22
#> 
#> $d10_14
#> [1] 12
#> 
#> $juvenile_i
#> [1] 0.2193548
#> 
#> $d5_14
#> [1] 34
#> 
#> $d20plus
#> [1] 155
#> 
#> $senility_i
#> [1] 0.2774194
#> 
#> $d0plus
#> [1] 247
#> 
#> $d60plus
#> [1] 43
#> 
#> $p5_19
#> [1] 0.213198
#> 
#> $D30_D5
#> [1] 0.5668016
#> 
#> $D0_14_D
#> [1] 0.340081
#> 
#> $d0_14
#> [1] 84
#> 
#> $D15_49_D15plus
#> [1] 0.5153374
#> 
#> $e0
#> [1] 30.67139
#> 
```
