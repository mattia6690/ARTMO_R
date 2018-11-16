# ARTMO R
## Link the ARTMO Software to R

This repository contains numerous functions useful for utilizing the Automated Radiative Transfer Models Operator, [Artmo](http://ipl.uv.es/artmo/). The functions are actively being developed and therefore in a early beta stage.
The codes aim at standardizing the workflow around Radiative Transfer Opearations in order to facilitate the organization of In- and Outputs given by the software. It does NOT aim at replace central ARTMO functions or to "translate" the functions originally used in Matlab.

#### Functionalities

For now the workflow foresees to interact directly with the local MySQL back-end and to extract the properties as well as the results of eac model. This facilitates the understanding of parameter influence and overall performance.
We've implemented both a Workflow (see. **ARTMO_sript**) as well as as a Shiny Application. Since the MySQL Server is locally and the constructed folder environment as well the Shiny can be called only within R and not yet via Web Browser.

#### In Progress

* Preprocessing functions
* Evaluation of Projecs and Model performances
* Parameter Testing 
* Integrate functions for spatial analysis
* Build a Shiny Web Application

#### Contact

If you are interested in using contributing or testing the package in order to make it more stable don't hesitate to contact me.