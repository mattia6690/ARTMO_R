# ARTMO_R <img align="right" src="data/ARMOR_simple.png" width="400" height="150" /></a>

[![Build Status](https://travis-ci.org/mattia6690/ARTMO_R.svg?branch=dev)](https://travis-ci.org/mattia6690/ARTMO_R) 
[![Coverage Status](https://img.shields.io/codecov/c/github/mattia6690/ARTMO_R/dev.svg)](https://codecov.io/github/mattia6690/ARTMO_R)
[![CRAN](http://www.r-pkg.org/badges/version/ARTMOR)](https://cran.r-project.org/package=ARTMOR)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Link the ARTMO Software to R!

This repository contains an **R-Package** for utilizing the [Automated Radiative Transfer Models Operator](http://ipl.uv.es/artmo/), **ARTMO**. The functions as well as the package are being actively developed and futher expanded. All the functionalities can be considered to be in an **early beta stage**.  
The functions aim at standardizing the workflow around Radiative Transfer Opearations in order to facilitate the organization of In- and Outputs given by the software. It does NOT replace central ARTMO functions or to "translate" the functions originally used in Matlab. Is is solemly interacting the the MySQL backend of ARTMO and translates most of its storages in a "tidy"- [Tidyverse](https://www.tidyverse.org/) - data structure ready for futher analysis.  

#### Installation

```s
library("devtools")
devtools::install_github("mattia6690/ARTMO_R")
```

#### Work In Progress

* Transferrably of the Functions (MLRA/Inversion/Indices)
* Additional Functionalities for Data Wrangling
* Extraction of multiple Parameters
* Adaptation to further ARTMO releases

If you want to know more about core functionalities of the package please visit the [Homepage of the project](https://mattia6690.github.io/ARTMO_R/)

#### Contact

If you are interested in using contributing or testing the package in order to make it more stable don't hesitate to contact me or to create an issue in the Repository.

#### Special Thanks

A special thanks goes out to the ARTMO development team at the [Laboratory for Earth Observation (LEO) of the University of Valencia](http://ipl.uv.es/leo/) and especially to [Jochem Verrelst](https://www.researchgate.net/profile/Jochem_Verrelst) and [Pablo Morcillo Pallarés](https://www.linkedin.com/in/pablo-antonio-morcillo-pallar%C3%A9s-1823a9114/)