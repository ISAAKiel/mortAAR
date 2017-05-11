[![Travis-CI Build Status](https://travis-ci.org/ISAAKiel/mortAAR.svg?branch=master)](https://travis-ci.org/ISAAKiel/mortAAR) [![Coverage Status](https://img.shields.io/codecov/c/github/ISAAKiel/mortAAR/master.svg)](https://codecov.io/github/ISAAKiel/mortAAR?branch=master)
[![license](https://img.shields.io/badge/license-GPL%203-B50B82.svg)](https://www.r-project.org/Licenses/GPL-3)

mortAAR
-------

R Library for the analysis of archaeological mortality data

With `mortAAR` you can calculated a life table based on archaeological demographic data. You just need the number of people of a certain age. `mortAAR` allows to seperate the data according to sex/location/epoch.

What is a life table [aka discrete time survival analysis]? According to [Chamberlain](https://books.google.de/books?id=nG5FoO_becAC&lpg=PA27&ots=LG0b_xrx6O&dq=life%20table%20archaeology&pg=PA27#v=onepage&q&f=false): it is a "(...) mathematical device for representing the mortality experience of a population and for exploring the effects on survivorship of age-specific probabilities of death. One reason why life tables have been ubiquitous in demography is that mortality cannot easily be modelled as a single equation or continuous function of age."

There are other ways and tools to create and analyse life table [e.g. [this one](https://web.stanford.edu/group/heeh/cgi-bin/web/node/75)]. What makes `mortAAR` special is that it implements a modern way of reprensenting the different weight of age classes by using an age-correction factor of 1/3 for all ages younger than 5. However, `mortAAR` also allows you to follow the "classic" approach. Just have a look at the [Vignette]() and the [Manual]() for further information. 

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
