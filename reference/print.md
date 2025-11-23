# Encode and print a mortaar_life_table or a mortaar_life_table_list

Format for pretty printing.

## Arguments

- x:

  a mortaar_life_table or a mortaar_life_table_list.

- class_of_deceased:

  optional string, specify the class of deceased (male, female, phase,
  ...).

- ...:

  further arguments passed to or from other methods.

## Value

A string representation of the mortaar_life_table or the
mortaar_life_table_list. For format.mortaar_life_table_list each
mortaar_life_table is formatted by itself and strung together. The names
of the elements are used to specify the name in the returned header of
the printout.

## Examples

``` r
# Create a mortaar_life_table from a prepared dataset.
schleswig_1 <- life.table(schleswig_ma[c("a", "Dx")])
print(schleswig_1)
#> 
#>   mortAAR life table (n = 247 individuals)
#> 
#> Life expectancy at birth (e0): 30.671
#> 
#>         x a    Ax Dx     dx      lx      qx      Lx       Tx     ex rel_popx
#> 1    0--4 5 1.667 50 20.243 100.000  20.243 432.524 3067.139 30.671   14.102
#> 2    5--9 5 2.500 22  8.907  79.757  11.168 376.518 2634.615 33.033   12.276
#> 3  10--14 5 2.500 12  4.858  70.850   6.857 342.105 2258.097 31.871   11.154
#> 4  15--19 5 2.500  8  3.239  65.992   4.908 321.862 1915.992 29.034   10.494
#> 5  20--26 7 3.500 15  6.073  62.753   9.677 418.016 1594.130 25.403   13.629
#> 6  27--33 7 3.500 30 12.146  56.680  21.429 354.251 1176.113 20.750   11.550
#> 7  34--40 7 3.500 12  4.858  44.534  10.909 294.737  821.862 18.455    9.610
#> 8  41--47 7 3.500 19  7.692  39.676  19.388 250.810  527.126 13.286    8.177
#> 9  48--54 7 3.500 36 14.575  31.984  45.570 172.874  276.316  8.639    5.636
#> 10 55--61 7 3.500 28 11.336  17.409  65.116  82.186  103.441  5.942    2.680
#> 11 62--68 7 3.500 15  6.073   6.073 100.000  21.255   21.255  3.500    0.693 

# Create a mortaar_life_table_list from two datasets.
odagsen <- life.table(list(
  "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
  "margo orbitalis" = odagsen_mo[c("a", "Dx")]
))
print(odagsen)
#> 
#> 
#>   mortAAR life table for : corpus mandibulae (n = 67.2 individuals)
#> 
#> Life expectancy at birth (e0): 37.888
#> 
#>         x a Ax  Dx     dx      lx      qx      Lx       Tx     ex rel_popx
#> 1    0--5 6  3 4.0  5.952 100.000   5.952 582.143 3788.839 37.888   15.365
#> 2   6--11 6  3 7.5 11.161  94.048  11.867 530.804 3206.696 34.097   14.010
#> 3  12--17 6  3 2.0  2.976  82.887   3.591 488.393 2675.893 32.284   12.890
#> 4  18--25 8  4 7.1 10.565  79.911  13.222 597.024 2187.500 27.374   15.757
#> 5  26--33 8  4 7.6 11.310  69.345  16.309 509.524 1590.476 22.936   13.448
#> 6  34--41 8  4 9.6 14.286  58.036  24.615 407.143 1080.952 18.626   10.746
#> 7  42--49 8  4 9.3 13.839  43.750  31.633 294.643  673.810 15.401    7.777
#> 8  50--57 8  4 5.8  8.631  29.911  28.856 204.762  379.167 12.677    5.404
#> 9  58--65 8  4 6.8 10.119  21.280  47.552 129.762  174.405  8.196    3.425
#> 10 66--73 8  4 7.5 11.161  11.161 100.000  44.643   44.643  4.000    1.178
#> 
#>   mortAAR life table for : margo orbitalis (n = 74.8 individuals)
#> 
#> Life expectancy at birth (e0): 34.739
#> 
#>         x a Ax   Dx     dx      lx      qx      Lx       Tx     ex rel_popx
#> 1    0--5 6  3  5.5  7.353 100.000   7.353 577.941 3473.930 34.739   16.637
#> 2   6--11 6  3  4.0  5.348  92.647   5.772 539.840 2895.989 31.258   15.540
#> 3  12--17 6  3  3.0  4.011  87.299   4.594 511.765 2356.150 26.989   14.732
#> 4  18--25 8  4 10.6 14.171  83.289  17.014 609.626 1844.385 22.144   17.549
#> 5  26--33 8  4 16.6 22.193  69.118  32.108 464.171 1234.759 17.865   13.362
#> 6  34--41 8  4  8.6 11.497  46.925  24.501 329.412  770.588 16.422    9.482
#> 7  42--49 8  4 11.0 14.706  35.428  41.509 224.599  441.176 12.453    6.465
#> 8  50--57 8  4  6.0  8.021  20.722  38.710 133.690  216.578 10.452    3.848
#> 9  58--65 8  4  6.5  8.690  12.701  68.421  66.845   82.888  6.526    1.924
#> 10 66--73 8  4  3.0  4.011   4.011 100.000  16.043   16.043  4.000    0.462 
```
