# n2khab-mhq-analysis
This repository contains the data analysis for the Monitoring Habitat Quality (MHQ) programme 
Information on MHQ can be found in [this report](https://publicaties.vlaanderen.be/view-file/52364).

+ [lsvi_calculation](https://github.com/inbo/n2khab-mhq-analysis/tree/main/source/lsvi_calculation)
contains the R-code for the calculation of the local conservations status (LSVI) for each
sampling unit in MHQ by using the [LSVI package](https://inbo.github.io/LSVI/index.html).

+ [status_calculation](https://github.com/inbo/n2khab-mhq-analysis/tree/main/source/status_calculation)
contains the R-code for the calculation of the regional conservation status for the habitat types
included in MHQ by estimating the proportion of habitat that has a favourable LSVI.
It is based on the results of the LSVI calculation.

+ [msa_map](https://github.com/inbo/n2khab-mhq-analysis/tree/main/source/msa_map) contains the code 
for the creation of a map with the minimum structural area (MSA) for each forest habitat type.
MSA is one of the indicators of the LSVI of forest habitat types.

Most of the data on which the analysis is based are stored in separate repositories:

+ [n2khab-mhq-data](https://github.com/inbo/n2khab-mhq-data): contains the field measurement data of the MHQ sampling units
and the results of the LSVI calculation and the status calculation.

+ [n2khab-sample-admin](https://github.com/inbo/n2khab-sample-admin): contains information on the sampling units of the mhq monitoring schemes, 
such as the visit date, the target type, the observed type, whether a sampling unit was measured or not.

Both repositories should be cloned to run the R code in [lsvi_calculation](https://github.com/inbo/n2khab-mhq-analysis/tree/main/source/lsvi_calculation) and 
[status_calculation](https://github.com/inbo/n2khab-mhq-analysis/tree/main/source/status_calculation).

It is also required to install the [n2khab package](https://inbo.github.io/n2khab/index.html)
and setting up the local data storage as explained [here](https://inbo.github.io/n2khab/articles/v020_datastorage.html),
for following data sources:

+ [habitatmap_terr](https://zenodo.org/records/13886579)
+ [watersurfaces_hab](https://zenodo.org/records/14621825)
+ [habitatstreams](https://zenodo.org/records/10353508)
+ [sac](https://zenodo.org/records/3386815)








