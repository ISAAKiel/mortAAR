# mortAAR 1.1.6
- fixed the error in plotting that occurred after the fix of "aes_string" of ggplot2
  and that still persisted
- fixed some typos
- added links to DESCRIPTION
- added more helpful error message if grouping variable contains NAs

# mortAAR 1.1.5
- fixed an error in plotting that occurred after the fix of "aes_string" of ggplot2
- enabled easier input of known-age data-sets

# mortAAR 1.1.4
- fixed an invalid url
 
# mortAAR 1.1.3
- fixed an error in the representativity calculations (thanks to hrncirv on github for
 pointing this out!)
- fixed deprecated "aes_string" of ggplot2

# mortAAR 1.1.2

- maintenance: removing references in the vignettes to csl-file as this caused an error

# mortAAR 1.1.1

- using the Stabilized MMR formula for maternal mortality
- adding the possibility to calculate population size from skeletal populations
- adding more reproduction indices

# mortAAR 1.1.0

- adding an option for plotting in color instead of linetype
- adding more functions for checking representativity of age data, calculating reproduction indices, masculinity index, maternal mortality rate as well as corrected life tables
- major re-factoring
- rewriting of tests to work with testthat's new snapshot-functionality
- adding two vignettes for life table correction and reproduction indices

# mortAAR 1.0.2

- tweaking the class-functions so that class mortaar_life_table is maintained when manipulating a mortaar_life_table_list
- adding a spline-option when constructing the life table for smoother adult age-groups

# mortAAR 1.0.1

- rewriting of some of the basic code
- defining of "class_functions.R"
- dividing of "output_functions.R" into "analytical_functions.R" and "plot_functions.R"
- fixing of some typos in the documentation and the output
- fixing of a small but serious bug which caused a non-alignment of the curves with the x-axis in the plots

# mortAAR 1.0.0

First release.
