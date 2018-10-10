# ARTMO R
## Link the ARTMO Software to R
#

This repository contains numerous functions useful for utilizing the Automated Radiative Transfer Models Operator, [Artmo](http://ipl.uv.es/artmo/). The functions are actively being developed and therefore in a early alpha stage.

The codes aim at standardizing the workflow around Radiative Transfer Opearations in order to facilitate the organization of In- and Outputs given by the software. It does NOT aim at replace central ARTMO functions or to "translate" the functions originally used in Matlab. Now, the functions and scripts try to set up a working environment (file structure) and read the metainformation exportable from ARTMO during the Workflow. Finally, the scripts integrate the possibility to compare the generated Images to the in-situ validation data by Pixel integrating [ggplot](http://ggplot.yhathq.com/) and [sf](https://github.com/r-spatial/sf).

In future we would like to stabilize the functions and add extra ones for additional metadata handling and to generate more output optionalities for interpreting the retrievel of biophysical parameters.

### Contact

If you are interested in my work, face similar problems or projects, or feel interested to contact me regarding any of my work feel free to contact me either here or via Mail

### Dependencies

Most of the presented scripts rely on my own [Package Mfunction](https://github.com/mattia6690/Mfunctions) and the Packages developed within the [tidyverse](https://www.tidyverse.org/)

