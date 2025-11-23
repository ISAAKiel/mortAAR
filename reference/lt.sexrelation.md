# Calculate masculinity index and maternal mortality from a mortAAR life table list

The proportional relation between adult males and females (= Masculinity
index) is interesting for a number of reasons: (1) it can point to basic
problems in the datasets in that, say, one sex is grossly over- or
underrepresented (*Herrmann et al. 1990*, 310). (2) it may hint towards
cultural reasons like sex- specific mobility.  
Maternal mortality is a basic indicator for the health system of a given
population. Maternal mortality is defined as dying during pregnancy or
within the first 42 days after birth due to complications. Recently,
*McFadden and colleagues 2020* have provided an updated formula to
calculate it from archaeological data.

## Usage

``` r
lt.sexrelation(females, males)
```

## Arguments

- females:

  an object of class mortaar_life_table for females

- males:

  an object of class mortaar_life_table for males

## Value

Output of masculinity index and maternal mortality.

- **Masculinity index**.

  \\MI = D\>=15male / D\>=15female\\

- **Maternal mortality**.

  \\333.33 \* (D20-24female / D20-24male) \* MI - 76.07\\

## Details

The Masculinity index (MI) is defined for juvenile and older
individuals. Note that with a higher mortality rate of adult females, an
MI \< 100 does not necessarily speak for an unbalanced MI in life.  
Maternal mortality is calculated according to the formula provided by
*McFadden & Oxenham 2019* in the updated version of *McFadden et al.
2020*. McFadden and Oxenham show that with modern data a very high
correlation is achieved by only comparing the absolute numbers of the
age group 20 to 24. This has the additional advantage that for this age
group anthropological aging methods are reasonable exact.

## References

Herrmann B, Grupe G, Hummel S, Piepenbrink H, Schutkowski H (1990).
*Praehistorische Anthropologie: Leitfaden der Feld- und Labormethoden*.
Springer, Berlin.

McFadden C, Oxenham MF (2019). “The Paleodemographic Measure of Maternal
Mortality and a Multifaceted Approach to Maternal Health.” *Current
Anthropology*, **60**(1), 141–146. <http://dx.doi.org/10.1086/701476>.

McFadden C, Van Tiel B, Oxenham MF (2020). “A stabilized maternal
mortality rate estimator for biased skeletal samples.” *Anthropological
Science*, **128**(3), 113–117.

## Examples

``` r
# Calculate Masculinity index and maternal mortality from Nitra
# dataset.
nitra_prep <- prep.life.table(nitra, group="sex", agebeg = "age_start", ageend = "age_end")
nitra_life <- life.table(nitra_prep)
lt.sexrelation(nitra_life$f, nitra_life$m)
#>      method  value                           description
#> 1        MI   0.67                     Masculinity index
#> 2 Ratio_F_M   2.36 Ratio of females to males aged 20--24
#> 3      MMR1 449.00 Maternal mortality per 100,000 births
#> 4      MMR2   4.49   Maternal mortality per 1,000 births
```
