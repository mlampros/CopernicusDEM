
## CopernicusDEM 1.0.3

* I renamed the *inst/COPYRIGHTS* file to *inst/COPYRIGHTS.pdf* (CRAN Team message)
* I fixed two broken URL's in the Vignette


## CopernicusDEM 1.0.2

* I've modified the *create_VRT_from_dir()* function to create the .vrt file only from *.tif* files (modified the *pattern* parameter in the *list.files()* function)
* I replaced the *'gdalUtils'* with the *'sf'* package (and especially the internal *'gdalbuildvrt'* function) due to the fact that it currently gives an error in the *'r-devel-windows-x86_64-new-UL'* test Flavor on CRAN


## CopernicusDEM 1.0.1

* I've added the Github repository URL to the DESCRIPTION file
* I've fixed mistakes in the Documentation of the R functions
* I've added a vignette explaining the functionality of the *CopernicusDEM* package
* I've modified the parallelzed loop in the **aoi_geom_save_tif_matches()** function so that it's compatible with both *Unix* and *Windows* Operating Systems
* I've fixed invalid URL's in the Vignette


## CopernicusDEM 1.0.0

