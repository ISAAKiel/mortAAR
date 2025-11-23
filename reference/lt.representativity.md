# Checks the representativity of the non-adult age groups in a mortAAR life table

*Weiss 1973*, 46f. and *Bocquet-Appel and Masset 1977* (see also
*Herrmann et al. 1990*, 306f.) have devised indices which check if the
non-adult age groups are represented in proportions as can be expected
from modern comparable data. Whether this is really applicable to
archaeological data-sets is a matter of debate.  
Quite recently, *Taylor and Oxenham 2024* added a comparison of Total
fertility rates (TRF) according to different formulas which depend
either on subadults or adults.  
Weiss chose the mortality (qx) as deciding factor and claimed that (1)
the probability of death of the age group 10–15 (5q10) should be lower
than that of the group 15–20 (5q15) and that (2) the latter in turn
should be lower than that of age group 0–5 (5q0).  
In contrast, Bocquet-Appel and Masset took the raw number of dead (Dx)
and asserted that (1) the ratio of those having died between 5 and 10
(5D5) to those having died between 10 and 15 (5D15) should be equal or
larger than 2 and that (2) the ratio of those having died between 5 and
15 (10D5) and all adults (\>= 20) should be 0.1 or larger.  
The formualas Taylor and Oxenham used either weigh all individuals aged
0–14 against all individuals or all individuals aged 15–49 against all
individuals aged 15+. The formulas differ from the original ones
published by *McFadden and Oxenham 2018* and *Taylor et al. 2023*
because the data basis is slighty different. If the results of the
formulas deviate by more than 0.692 (the standard error of estimate,
SEE), there is a problem with the age structure.  
Due to the specific nature of the indices, they only give meaningful
results if 5-year-age categories have been chosen for the non-adults.

## Usage

``` r
lt.representativity(life_table)
```

## Arguments

- life_table:

  an object of class mortaar_life_table.

## Value

data.frame showing the indices and explaining their interpretation.

## References

Herrmann B, Grupe G, Hummel S, Piepenbrink H, Schutkowski H (1990).
*Praehistorische Anthropologie: Leitfaden der Feld- und Labormethoden*.
Springer, Berlin.

Masset C, Bocquet J (1977). “Estimateurs en paléodémographie.”
*L’Homme*, **17**(4), 65–90.

mcfadden_oxenham_2018a

Weiss KM, Wobst HM (1973). “Demographic Models for Anthropology.”
*Memoirs of the Society for American Archaeology*, **27**, i–186. ISSN
0081-1300.

Taylor B, Oxenham M (2024). “A method for detecting bias in human
archaeological cemetery samples.” *International Journal of
Osteoarchaeology*, 0.
[doi:10.1002/oa.3379](https://doi.org/10.1002/oa.3379) .
taylor_et_al_2023

## Examples

``` r
schleswig <- life.table(schleswig_ma[c("a", "Dx")])
lt.representativity(schleswig)
#>     approach            condition value1 value2 result outcome
#> 1   weiss_i1           5q0 > 5q15  20.24   4.91   4.12    TRUE
#> 2   weiss_i2          5q10 < 5q15   6.86   4.91   1.40   FALSE
#> 3    child_i    (5D5 / 5D10) >= 2  22.00  12.00   1.83   FALSE
#> 4 juvenile_i (10D5 / D20+) >= 0.1  34.00 155.00   0.22    TRUE
#> 5        TFR       TFR_SA = TFR_A   4.83   6.99   2.16   FALSE
```
