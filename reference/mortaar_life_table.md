# mortaar_life_table and mortaar_life_table_list

The **mortaar_life_table** is the central data structure of the
`mortAAR` package. It's a data.frame with set of custom methods and
variables. Please see
[`mortAAR::life.table`](https://isaakiel.github.io/mortAAR/reference/life.table.md)
for a description of the variables. Further available variables are
ignored.  
If an object is of class data.frame or tibble (tbl & tbl_df), it can be
converted to an object of class mortaar_life_table. The only requirement
is that it contains at least the essential columns **a** and **Dx**. The
`as` function adds the string "mortaar_life_table" to the classes
vector.  
The **mortaar_life_table_list** is a list of mortaar_life_tables. It can
contain the additional attribute `group` that stores a string with the
name of the grouping variable relevant for the separation of the
different mortaar_life_tables in the list. The group variable is only
relevant for plot and print aesthetics.

## Usage

``` r
as.mortaar_life_table_list(x, ...)

as.mortaar_life_table(x, ...)
```

## Arguments

- x:

  an object

- ...:

  further arguments passed to or from other methods

## Examples

``` r
# a mortaar_life_table can be put together manually:
as.mortaar_life_table(data.frame(a = c(20, 20, 20), Dx = c(10, 15, 20)))
#> Warning: Unknown or uninitialised column: `ex`.
#> 
#>   mortAAR life table (n = 45 individuals)
#> 
#>    a Dx
#> 1 20 10
#> 2 20 15
#> 3 20 20 

# a mortaar_life_table_list can be constructed from multiple mortaar_life_tables
schleswig <- as.mortaar_life_table_list(
  list(
    "schleswig data 1" = life.table(schleswig_ma[c("a", "Dx")]),
    "schleswig data 2" = life.table(schleswig_ma[c("a", "Dx")])
  )
)

# you can add new mortaar_life_tables to plot them with the others
schleswig$`schleswig data 3` <- life.table(schleswig_ma[c("a", "Dx")])
schleswig[["schleswig data 4"]] <- life.table(schleswig_ma[c("a", "Dx")])

# and you can create arbitrary subsets of mortaar_life_table_lists
schleswig_data_3 <- schleswig$`schleswig data 3`
schleswig_data_1_3_4 <- schleswig[c(1,3,4)]
```
