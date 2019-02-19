# ARTMO_R <a href="https://mattia6690.github.io/ARTMO_R/"><img align="right" src="docs/data/ARTMOR_logoR.png" width="300" height="120" /></a>

[![Build Status](https://travis-ci.org/mattia6690/ARTMO_R.svg?branch=dev)](https://travis-ci.org/mattia6690/ARTMO_R) 
[![Coverage Status](https://img.shields.io/codecov/c/github/mattia6690/ARTMO_R/master.svg)](https://codecov.io/github/mattia6690/ARTMO_R)
[![CRAN](http://www.r-pkg.org/badges/version/ARTMOR)](https://cran.r-project.org/package=ARTMOR)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Link the ARTMO Software to R!

This repository contains an **R-Package** for utilizing the **Automated Radiative Transfer Models Operator**, [Artmo](http://ipl.uv.es/artmo/). The functions as well as the package are being actively developed and futher expanded. All the functionalities can be considered to be in an **early beta stage**.  
The functions aim at standardizing the workflow around Radiative Transfer Opearations in order to facilitate the organization of In- and Outputs given by the software. It does NOT replace central ARTMO functions or to "translate" the functions originally used in Matlab. Is is solemly interacting the the MySQL backend of ARTMO and translates most of its storages in a "tidy"- [Tidyverse](https://www.tidyverse.org/) - data structure ready for futher analysis.  

#### Installation

```s
library("devtools")
devtools::install_github("mattia6690/ARTMO_R")
```

#### Work In Progress

* Preprocessing functions
* Evaluation of Projecs and Model performances
* Parameter Testing 
* Integrate functions for spatial analysis
* Build a Shiny Web Application

If you want to know more about core functionalities of the package please visit the [Homepage of the project](https://mattia6690.github.io/ARTMO_R/) orclick on the Artmo Logo above

#### Contact

If you are interested in using contributing or testing the package in order to make it more stable don't hesitate to contact me or to create an issue in the Repository.