# Creates the input for the function life.table

Prepares the input for
[`life.table()`](https://isaakiel.github.io/mortAAR/reference/life.table.md).
An individual based approach is supported as well as already pooled data
(e. g. from an already existing life table). In the latter case, the
user has to specify a numerical variable (**dec**) which defines the
count for each age class. If no life table exists, this function will
process a dataframe including the age ranges of individuals or groups of
individuals to discrete the age classes. The age range is spread to
single years. **agebeg** has to be specified for the beginning of an age
range, as well as **ageend** for the end of an age range. If a data-set
with year-known individuals is used, **ageend** can be omitted but then
the parameter **agerange** has to left on its default value
(`included`). The **method** defines in which way the single years
between the different age classes are split. If the data set comprises a
grouping variable (e.g., sex), this can be specified with **group**.

## Usage

``` r
prep.life.table(
  x,
  dec = NA,
  agebeg,
  ageend = NA,
  group = NA,
  method = "Standard",
  agerange = "included"
)
```

## Arguments

- x:

  single dataframe containing sex age and quantity of deceased
  (individuals or group of individuals).

- dec:

  column name (as character) of the count of deceased, optional.

- agebeg:

  column name (as character) for the beginning of an age range.

- ageend:

  column name (as character) for the end of an age range, optional.

- group:

  column name (as character) of the grouping field (e.g., sex),
  optional. Default setup is: `NA`.

- method:

  character string, optional. Default options is `Standard`, which will
  create age classes beginning with 1 year, up to 4 years, followed by
  steps of 5 years (1,4,5,5,...) until the maximum age is reached.
  `Equal5` will create age classes with an even distribution, stepped by
  5 years (5,5,...) until the maximum age is reached. If method is a
  single numeric, this number will be repeated until the maximum age is
  reached. Thereby, it is possible to create a year-wise life table.

- agerange:

  character string, optional. Default setup is: `included`. If the age
  ranges from "20 to 40" and "40 to 60", `excluded` will exclude the
  year 40 from "20 to 40", to prevent overlapping age classes.
  `included` is for age ranges like "20 to 39" where the year 39 is
  meant to be counted.

## Value

A list of input parameter needed for the function `life.table`.

- **x** or **Age**: age interval.

- **a**: years within x.

- **Dx**: number of deaths within **x**.

## Examples

``` r
# Separate age ranges in your data set.
df <- dplyr::mutate(
  tidyr::separate(
    replace(
     magdalenenberg,
     magdalenenberg=="60-x", "60-69"
    ),
    a,
    c("from", "to")
  ),
  from = as.numeric(from),
  to = as.numeric(to)
)

# Apply prep.life.table to a data set containing the age ranges.
magda_prep <- prep.life.table(
  df,
  dec = "Dx",
  agebeg = "from",
  ageend = "to",
  method = "Equal5",
  agerange = "included"
)

# Create a life.table.
life.table(magda_prep)
#> 
#>   mortAAR life table (n = 111 individuals)
#> 
#> Life expectancy at birth (e0): 32.196
#> 
#>         x a    Ax    Dx     dx      lx      qx      Lx       Tx     ex rel_popx
#> 1    0--4 5 1.667  3.79  3.414 100.000   3.414 488.619 3219.632 32.196   15.176
#> 2    5--9 5 2.500  4.62  4.162  96.586   4.309 472.523 2731.014 28.276   14.676
#> 3  10--14 5 2.500  4.54  4.090  92.423   4.425 451.892 2258.491 24.436   14.036
#> 4  15--19 5 2.500  4.21  3.793  88.333   4.294 432.185 1806.599 20.452   13.423
#> 5  20--24 5 2.500 14.99 13.505  84.541  15.974 388.941 1374.414 16.257   12.080
#> 6  25--29 5 2.500 20.61 18.568  71.036  26.138 308.761  985.473 13.873    9.590
#> 7  30--34 5 2.500 17.20 15.495  52.468  29.533 223.604  676.712 12.897    6.945
#> 8  35--39 5 2.500 14.39 12.964  36.973  35.063 152.455  453.108 12.255    4.735
#> 9  40--44 5 2.500  6.68  6.018  24.009  25.066 105.000  300.653 12.523    3.261
#> 10 45--49 5 2.500  4.04  3.640  17.991  20.230  80.856  195.653 10.875    2.511
#> 11 50--54 5 2.500  5.49  4.946  14.351  34.463  59.392  114.797  7.999    1.845
#> 12 55--59 5 2.500  5.72  5.153   9.405  54.789  34.144   55.405  5.891    1.060
#> 13 60--64 5 2.500  2.36  2.126   4.252  50.000  15.946   21.261  5.000    0.495
#> 14 65--69 5 2.500  2.36  2.126   2.126 100.000   5.315    5.315  2.500    0.165 
```
