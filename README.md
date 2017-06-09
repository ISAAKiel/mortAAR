[![Travis-CI Build Status](https://travis-ci.org/ISAAKiel/mortAAR.svg?branch=master)](https://travis-ci.org/ISAAKiel/mortAAR) [![Coverage Status](https://img.shields.io/codecov/c/github/ISAAKiel/mortAAR/master.svg)](https://codecov.io/github/ISAAKiel/mortAAR?branch=master)
[![license](https://img.shields.io/badge/license-GPL%203-B50B82.svg)](https://www.r-project.org/Licenses/GPL-3)

mortAAR
-------

R Library for the analysis of archaeological mortality data

With `mortAAR` you can calculate a life table based on archaeological demographic data. You just need the number of people of a certain age, but you can also use single individual data. `mortAAR` allows to separate the data according to sex/location/epoch.

What is a life table [aka discrete time survival analysis]? According to [Chamberlain](https://books.google.de/books?id=nG5FoO_becAC&lpg=PA27&ots=LG0b_xrx6O&dq=life%20table%20archaeology&pg=PA27#v=onepage&q&f=false): it is a "(...) mathematical device for representing the mortality experience of a population and for exploring the effects on survivorship of age-specific probabilities of death. One reason why life tables have been ubiquitous in demography is that mortality cannot easily be modelled as a single equation or continuous function of age."

To our knowledge, as of writing a simple to use and easily accessible tool calculate and create archaeological life tables has been lacking. That is why we sat down and put mortAAR for R together. We hope it will be of use for archaeologists world-wide.

In our view, mortAAR shines in the following areas:
- Ease and flexibility of input (different kinds of customisation and grouping)
- Sophisticated means of computation (exploding of age ranges, separation factor for average lived years)
- Comprehensiveness of output (life tables for all groups specified, plots for the most important measures, relative population calculation)

For further information, please have a look at the [Vignette]() and the [Manual](). 

#### Released version

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mortAAR)](http://cran.r-project.org/package=mortAAR)

Just started...

Licence
-------

`mortAAR` is released under the [GNU General Public Licence, version 3](http://www.r-project.org/Licenses/GPL-3). Comments and feedback are welcome, as are code contributions.

Installation
------------

`mortAAR` is currently not on [CRAN](http://cran.r-project.org/), but you can use [devtools](http://cran.r-project.org/web/packages/devtools/index.html) to install the development version. To do so:

    if(!require('devtools')) install.packages('devtools')
    library(devtools)
    install_github('ISAAKiel/mortAAR')
