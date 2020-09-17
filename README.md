# pepperwood_uasStudy_2019
R scripts associated with analysis of UAS point cloud data collected at Pepperwood Preserve during 2019 field campaign with Sonoma State University/University of Oxford

Developed on a windows machine. Some scripts set up for linux cluster use. See individual script headers for additional details.

Contact Sean Reilly (sean.reilly@ouce.ox.ac.uk) for questions. Please open issue notes as they arise.

## Contains the following R scripts:

[**chm_data-compile.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/chm_data-compile.R): Generates large dataset containing uas dtm values, als dtm values, uas dtm error, vegetation classes, topography classes, and burn severities. Also generates several reference plots.

laspostprocessing.R: Merges Pix4D las point cloud and multispectral data, reprojects data if given, and clips to given boundary
  
lastransformation.R: Performs a spatial transformation on a las point cloud based on given transformation matrix

csfparamtesting.R: Produces sequence of DTMs from a point cloud using Cloth Simulation Filter (CSF) ground finding algorithm with a supplemental NDVI reclassification filter in order to test parameter effects on DTM accuracy.

nflightcompare.R: Compares dtm generation results for uas point clouds generated from either one or two flight directions

plotlasreg.R: Plots two las files on top of one another to visualize if registration was performed successfully

stdcanopymetrics.R: Compares standard canopy metrics as implemented in lidR between UAS and ALS point clouds
