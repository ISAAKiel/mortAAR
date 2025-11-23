# Calculates reproduction indices from a mortAAR life table

For population studies it is of vital importance to estimate growth or
decline of a population. For archaeological datasets this is rarely
attempted, probably because the data quality seems to scanty.
Nevertheless, the calculation of such measures seems worth the try, at
least it should give an impression if the resulting values are
unrealistic high or low.

## Usage

``` r
lt.reproduction(
  life_table,
  fertility_rate = "BA_log",
  growth_rate = "fertility",
  gen_len = 20
)
```

## Arguments

- life_table:

  an object of class mortaar_life_table.

- fertility_rate:

  string or numeric. Either fertility rate according to *McFadden &
  Oxenham 2018a* if infants are represented well or *Taylor et al.* or
  fertility rate according to data by *McFadden & Oxenham 2018a* for
  P(5-19) index after *Bocquet-Appel 2002*. Options: 'McFO'
  (McFadden/Oxenham), 'TOMc' (Taylor et al.), 'BA_linear' (linear fit),
  'BA_power' (power fit) or 'BA_log' (logistic fit). Default: BA_log'.
  Additionally, the user can specify an arbitrary number in lieu of the
  fertility rate.

- growth_rate:

  string or numeric. Either derived directly from the fertility
  calculations or from regression analysis by either *McFadden & Oxenham
  2018b* (\\10.06 \* D0--14/D) -- 1.61\\) or *Bocquet-Appel and Masset*
  (\\(1.484 \* (log10(200 \* d5--14/d20 \* d60/d20))\*\*0.03 -
  1.485)\\). Options: 'fertility', 'MBA', McFO'. Additionally, the user
  can specify an arbitrary number in lieu of the growth rate.

- gen_len:

  numeric. Length of generation for determining the rate of doubling the
  population. Default: 20.

## Value

A data.frame with basic reproduction indices:

- **m**: Mortality rate (= natality rate n).

  \\0.127 \* d5--14/d20 + 0.016\\

- **dep**: Dependency ratio.

  \\DR = (sum(D0--14) + sum(D60+)) / sum(D15--59) \\

- **TFR**: Total fertility rate.

- **GRR**: Gross reproduction rate.

  \\GRR = TFR \* 0.488\\

- **NRR**: Net reproduction rate.

  \\NRR = sum(GRR \* age specific fertility \* age specific survival /
  10000)\\

- **r**: Intrinsic growth rate in per cent per year.

- **Dt**: Doubling time in years.

  \\Dt = 100 \* ln(2) / r\\

- **D30_D5**: Ratio D30+/D5+ after Buikstra et al.

- **BR**: Birth rate from ratio D30+/D5+.

  \\BR = -11.493 \* D30_D5 + 12.712\\

- **DR**: Death rate from ratio D30+/D5+.

  \\DR = -5.287 \* D30_D5 + 6.179\\

## Details

There are different approaches to calculate reproduction rates (e. g.
*Henneberg 1978*). We largely follow the methodology by *Hassan 1981*.
Typically, a Total fertility rate (TFR) of 6-8 is assumed for
prehistoric populations (*Ascadi/Nemeskeri 1970*; *Henneberg 1978*;
*Hassan 1981*). Recently, *McFadden and Oxenham 2018a* have published a
formula to estimate the Total fertility rate from archaeological data,
provided that infants are represented fully in the archaeological
record.  
Unfortunately, this will not be the case for most archaeological
datasets. Therefore, we used the data published by McFadden and Oxenham
to apply it to the P(5-19)-index after *Bocquet-Appel 2002*. We
approximated the ratio by three different methods of fitting (linear,
logistic, power) and recommend logistic fitting, but the others are
available as well.  
We have also added the option to use the formula by *Taylor et al. 2023*
that uses the ratio of adults aged 15–49 in relation to those aged 15 or
older.  
The Gross reproduction rate (GRR) is calculated by multiplying the TFR
with the ratio of female newborns, assumed to be a constant of 48.8 of
all children (*Hassan* 1981, 136). The Net reproduction rate is arrived
at by summing the product of the GRR, the age specific fertility rate as
defined by *Hassan* (1981, 137 tab. 8.7) and the age specific survival
taken from the life table and dividing the result by 10000.  
The Rate of natural increase or Intrinsic growth rate r (growth in per
cent per year) can be computed from the fertility following *Hassan*
(1981, 140). Alternative ways to calculate the intrinsic growth rate
derive from *Bocquet-Appel and Masset* (1977) and recently from
*McFadden and Oxenham 2018b*. The latter present a regression analysis
based on the index D0–15/D also used for fertility calculations (see
above) in connections with modern data.  
Whatever is chosen as base for the growth rate calculations is used for
computing the doubling time in years, assuming exponential steady
growth.  
Also calculated is the mortality rate m after *Bocquet-Appel and Masset*
(1977) in per cent of a given population. Furthermore, the ratio of
dependent individuals is reported that is usually (but probably
erroneously for archaic societies (*Grupe et al. 2015*, 423) assumed to
apply to those aged below 15 or 60 and above.  
Finally, *Buikstra et al. 1986* have made the interesting observation
that the relation of those individuals aged 30 years and above to those
aged 5 years and above is very closely related to the birth rate and
also closely (but less significantly) related to the death rate.
Therefore, these indices are calculated as well.

## References

Acsadi G, Nemeskeri J (1970). *History of Human Life Span and
Mortality*. Akademiai Kiado, Budapest.

Masset C, Bocquet J (1977). “Estimateurs en paléodémographie.”
*L’Homme*, **17**(4), 65–90.

Bocquet-Appel J (2002). “Paleoanthropological Traces of a Neolithic
Demographic Transition.” *Current Anthropology*, **43**(4), 637–650.

Buikstra JE, Konigsberg LW, Bullington J (1986). “Fertility and the
Development of Agriculture in the Prehistoric Midwest.” *American
Antiquity*, **51**(3), 528–546.

Grupe G, Harbeck M, McGlynn GC (2015). *Prähistorische Anthropologie*.
Springer, Berlin, Heidelberg.

Hassan FA (1981). *Demographic Archaeology*. Academic Press, New York.

Henneberg M (1976). “Reproductive possibilities and estimations of the
biological dynamics of earlier human populations.” *Journal of Human
Evolution*, **5**(1), 41–48.

McFadden C, Oxenham MF (2018). “The D0-14/D ratio: A new
paleodemographic index and equation for estimating total fertility
rates.” *American Journal of Physical Anthropology*, **165**(3),
471–479.

McFadden C, Oxenham MF (2018). “Rate of natural population increase as a
paleodemographic measure of growth.” *Journal of Archaeological Science:
Reports*, **19**, 352–356.

Taylor BR, Oxenham M, McFadden C (2023). “Estimating fertility using
adults: A method for under-enumerated pre-adult skeletal samples.”
*American Journal of Biological Anthropology*, **181**(2), 262–270.
[doi:10.1002/ajpa.24739](https://doi.org/10.1002/ajpa.24739) .

## Examples

``` r
schleswig <- life.table(schleswig_ma[c("a", "Dx")])
lt.reproduction(schleswig)
#>    method  value              description
#> 1       m   4.39                Mortality
#> 2     dep 105.83         Dependency ratio
#> 3     TFR   7.01     Total fertility rate
#> 4     GRR   3.42  Gross reproduction rate
#> 5     NRR   1.69    Net reproduction rate
#> 6       r   2.63 Rate of natural increase
#> 7      Dt  26.39   Doubling time in years
#> 8  D30_D5   0.57           ratio D30+/D5+
#> 9      BR   6.20               Birth rate
#> 10     DR   3.18               Death rate

odagsen <- life.table(list(
  "corpus mandibulae" = odagsen_cm[c("a", "Dx")],
  "margo orbitalis" = odagsen_mo[c("a", "Dx")]
))
lt.reproduction(odagsen)
#> $`corpus mandibulae`
#>    method value              description
#> 1       m  3.37                Mortality
#> 2     dep 62.32         Dependency ratio
#> 3     TFR  6.46     Total fertility rate
#> 4     GRR  3.15  Gross reproduction rate
#> 5     NRR  1.82    Net reproduction rate
#> 6       r  2.98 Rate of natural increase
#> 7      Dt 23.24   Doubling time in years
#> 8  D30_D5  0.69           ratio D30+/D5+
#> 9      BR  4.74               Birth rate
#> 10     DR  2.51               Death rate
#> 
#> $`margo orbitalis`
#>    method value              description
#> 1       m  2.42                Mortality
#> 2     dep 34.05         Dependency ratio
#> 3     TFR  5.83     Total fertility rate
#> 4     GRR  2.84  Gross reproduction rate
#> 5     NRR  1.45    Net reproduction rate
#> 6       r  1.87 Rate of natural increase
#> 7      Dt 37.09   Doubling time in years
#> 8  D30_D5  0.69           ratio D30+/D5+
#> 9      BR  4.77               Birth rate
#> 10     DR  2.52               Death rate
#> 
```
