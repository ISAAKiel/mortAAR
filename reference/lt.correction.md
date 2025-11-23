# Calculates a corrected life table from a mortAAR life table

It is generally assumed that most skeletal populations lack the youngest
age group. Life tables resulting from such populations will necessarily
be misleading as they lead to believe that the mortality of younger
children was lower than it actually was and that life expectancy was
higher. For correcting these missing individuals, *Bocquet-Appel and
Masset* (1977; see also *Herrmann et al. 1990*, 307) conceived of
several calculations based on regression analyses of modern comparable
mortality data. However, the applicability of these indices to
archaeological data is highly debated and does not necessarily lead to
reliable results. Therefore, the correction needs to be weighted
carefully and ideally only after the representativity of the base data
has been checked with function lt.representativity.

## Usage

``` r
lt.correction(life_table, agecor = TRUE, agecorfac = c(), option_spline = NULL)
```

## Arguments

- life_table:

  an object of class mortaar_life_table.

- agecor:

  logical, optional. Passed to
  [`life.table`](https://isaakiel.github.io/mortAAR/reference/life.table.md).

- agecorfac:

  numeric vector, optional. Passed to
  [`life.table`](https://isaakiel.github.io/mortAAR/reference/life.table.md).

- option_spline:

  integer, optional. Passed to
  [`life.table`](https://isaakiel.github.io/mortAAR/reference/life.table.md).

## Value

a list containing a data.frame with indices e0, 1q0 and 5q0 as well as
mortality rate m and growth rate r according to Bocquet-Appel and Masset
showing the computed exact value as well as ranges and an object of
class mortaar_life_table with the corrected values.

- **e0**: Corrected life expectancy.

- **1q0**: Mortality of age group 0–1.

- **5q0**: Mortality of age group 0–5.

## Details

For the parameters see the documentation of
[`life.table`](https://isaakiel.github.io/mortAAR/reference/life.table.md).

## References

Masset C, Bocquet J (1977). “Estimateurs en paléodémographie.”
*L’Homme*, **17**(4), 65–90.

Herrmann B, Grupe G, Hummel S, Piepenbrink H, Schutkowski H (1990).
*Praehistorische Anthropologie: Leitfaden der Feld- und Labormethoden*.
Springer, Berlin.

## Examples

``` r
# Calculate a corrected life table from real life dataset.
schleswig <- life.table(schleswig_ma[c("a", "Dx")])
lt.correction(schleswig)
#> $indices
#>   method  value range_start range_end
#> 1     e0 22.549      21.046    24.052
#> 2    1q0  0.290       0.274     0.306
#> 3    5q0  0.465       0.424     0.506
#> 
#> $life_table_corr
#> 
#>   mortAAR life table (n = 368.1 individuals)
#> 
#> Life expectancy at birth (e0): 21.129
#> 
#>         x a    Ax      Dx     dx      lx      qx      Lx       Tx     ex
#> 1    0--4 5 1.667 171.102 46.482 100.000  46.482 345.059 2112.914 21.129
#> 2    5--9 5 2.500  22.000  5.977  53.518  11.168 252.648 1767.854 33.033
#> 3  10--14 5 2.500  12.000  3.260  47.541   6.857 229.556 1515.207 31.871
#> 4  15--19 5 2.500   8.000  2.173  44.281   4.908 215.973 1285.650 29.034
#> 5  20--26 7 3.500  15.000  4.075  42.108   9.677 280.493 1069.677 25.403
#> 6  27--33 7 3.500  30.000  8.150  38.033  21.429 237.706  789.184 20.750
#> 7  34--40 7 3.500  12.000  3.260  29.883  10.909 197.771  551.478 18.455
#> 8  41--47 7 3.500  19.000  5.162  26.623  19.388 168.296  353.707 13.286
#> 9  48--54 7 3.500  36.000  9.780  21.461  45.570 116.001  185.411  8.639
#> 10 55--61 7 3.500  28.000  7.607  11.682  65.116  55.148   69.410  5.942
#> 11 62--68 7 3.500  15.000  4.075   4.075 100.000  14.262   14.262  3.500
#>    rel_popx
#> 1    16.331
#> 2    11.957
#> 3    10.864
#> 4    10.222
#> 5    13.275
#> 6    11.250
#> 7     9.360
#> 8     7.965
#> 9     5.490
#> 10    2.610
#> 11    0.675 
#> 

```
