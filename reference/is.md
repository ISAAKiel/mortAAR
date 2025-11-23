# Checks if a variable is of class mortaar_life_table or mortaar_life_table_list

Checks if a variable is of class mortaar_life_table or
mortaar_life_table_list

## Usage

``` r
is.mortaar_life_table_list(x, ...)

is.mortaar_life_table(x, ...)
```

## Arguments

- x:

  a variable.

- ...:

  further arguments passed to or from other methods.

## Value

true if x is a mortaar_life_table or a mortaar_life_table_list,
otherwise false.

## Examples

``` r
# Create a mortaar_life_table from a prepared dataset.
class(schleswig_ma)
#> [1] "data.frame"
is.mortaar_life_table(schleswig_ma)
#> [1] FALSE

schleswig_1 <- life.table(schleswig_ma[c("a", "Dx")])

class(schleswig_1)
#> [1] "mortaar_life_table" "tbl_df"             "tbl"               
#> [4] "data.frame"        
is.mortaar_life_table(schleswig_1)
#> [1] TRUE

# Create a mortaar_life_table_list from two datasets.
odagsen <- life.table(list(
  "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
  "margo orbitalis" = odagsen_mo[c("a", "Dx")]
))
is.mortaar_life_table_list(odagsen)
#> [1] TRUE
```
