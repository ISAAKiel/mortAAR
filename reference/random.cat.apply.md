# Generation of random age ranges

Helper function that generates random age categories of absolute ages.
It is mainly used together with the functions `pop.sim.gomp` and
`random.cat.apply`. It will run until the number of categories are
reached *and* there are no gaps in the sequence left.

Helper function that applies random age categories to "known" absolute
ages. It is mainly used together with the functions `pop.sim.gomp` and
`random.cat`.

## Usage

``` r
random.cat(n_cat = 20, min_age = 15, max_cat_low = 60, max_age = 74)

random.cat.apply(x, age, age_ranges, from, to)
```

## Arguments

- n_cat:

  numeric. Number of categories, default: 20.

- min_age:

  numeric. Minimum age, default: 15.

- max_cat_low:

  numeric. Lower boundary of highest age categoriy, default: 60.

- max_age:

  numeric. Maximum age, default: 74.

- x:

  a data.frame with individual absolute ages.

- age:

  the column containing the individual absolute ages.

- age_ranges:

  a data.frame with age ranges.

- from:

  numeric. Column name for the begin of an age range.

- to:

  numeric. Column name for the end of an age range.

## Value

One data.frame with the following items:

- **from**: Lower boundary of age category.

- **to**: Upper boundary of age category.

The original data.frame `x` with two additional columns:

- **from**: Lower boundary of age category.

- **to**: Upper boundary of age category.

## Examples

``` r
sim_ranges <- random.cat()

# Simulate population and age ranges first
pop_sim <- pop.sim.gomp(n = 10000)
sim_ranges <- random.cat()

# apply random age categories to simulated ages
sim_appl <- random.cat.apply(pop_sim$result, age = "age",
age_ranges = sim_ranges, from = "from", to = "to")
```
