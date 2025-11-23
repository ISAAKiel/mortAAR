# Calculates population size from a mortAAR life table or numeric values

The estimation of the population size for a given cemetery is only
possible if a stationary population is assumed. In this case, the number
of deaths is simply multiplied with the life expectancy at birth and
divided be the time span in years the cemetery was in use. Additionally,
it is assumed that an unknown number of individuals is not represented
in the cemetery and, therefore, the resulting number is multiplied by an
arbitrary value k (*Herrmann et al. 1990*, 311f.).

## Usage

``` r
lt.population_size(x, e0, k = 1.1, t)
```

## Arguments

- x:

  either an object of class mortaar_life_table or
  mortaar_life_table_list or an arbitrary numeric value representing the
  number of deaths.

- e0:

  numeric. life expectancy at birth in years (if x is of class
  mortaar_life_table then e0 can be derived directly from the life
  table's ex column).

- k:

  numeric. Arbitrary number to cater for individuals not represented in
  the number of deaths. Default: 1.1.

- t:

  numeric. Time span of usage of cemetery in years.

## Value

A data.frame with the following items:

- **D**: Number of deaths.

- **e0**: Life expectancy at birth in years.

- **k**: Correction factor.

- **t**: Time span of usage of cemetery in years.

- **P**: Population size calculated by the formula \\P = D \* e0 \* k /
  t\\

## References

Herrmann B, Grupe G, Hummel S, Piepenbrink H, Schutkowski H (1990).
*Praehistorische Anthropologie: Leitfaden der Feld- und Labormethoden*.
Springer, Berlin.

## Examples

``` r
schleswig <- life.table(schleswig_ma[c("a", "Dx")])
lt.population_size(schleswig, t = 100)
#>   method value       description
#> 1      D 247.0  Number of deaths
#> 2     e0  30.7   Life expectancy
#> 3      k   1.1 Correction factor
#> 4      t 100.0         Time span
#> 5      P  83.3   Population size

odagsen <- life.table(list(
"corpus mandibulae" = odagsen_cm[c("a", "Dx")],
 "margo orbitalis" = odagsen_mo[c("a", "Dx")]
 ))
lt.population_size(odagsen, e0 = 30, t = 100)
#> $`corpus mandibulae`
#>   method value       description
#> 1      D  67.2  Number of deaths
#> 2     e0  30.0   Life expectancy
#> 3      k   1.1 Correction factor
#> 4      t 100.0         Time span
#> 5      P  22.2   Population size
#> 
#> $`margo orbitalis`
#>   method value       description
#> 1      D  74.8  Number of deaths
#> 2     e0  30.0   Life expectancy
#> 3      k   1.1 Correction factor
#> 4      t 100.0         Time span
#> 5      P  24.7   Population size
#> 

lt.population_size(x = 111, e0 = 32.2, k = 1.2, t = 100)
#>   method value       description
#> 1      D 111.0  Number of deaths
#> 2     e0  32.2   Life expectancy
#> 3      k   1.2 Correction factor
#> 4      t 100.0         Time span
#> 5      P  42.9   Population size
```
