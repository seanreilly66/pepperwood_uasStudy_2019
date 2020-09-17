# pepperwood_uasStudy_2019
R scripts associated with analysis of UAS point cloud data collected at Pepperwood Preserve during 2019 field campaign with Sonoma State University/University of Oxford

Developed on a windows machine. Some scripts set up for linux cluster use. See individual script headers for additional details.

Contact Sean Reilly (sean.reilly@ouce.ox.ac.uk) for questions. Please open issue notes as they arise.

### Table of contents:

#### 1. Las file processing

[**las_postprocessing.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/las_postprocessing.R): Merges Pix4D las point cloud and multispectral data, reprojects point cloud, and clips to given boundary

[**las_transformation.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/las_transformation.R): Performs a spatial transformation on a las point cloud based on given transformation matrix

[**las_plot-registration.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/las_plot-registration.R): Plots two las files on top of one another to visualize if registration was performed successfully

[**las_height-normalization,R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/las_height-normalization.R): Height normalizes UAS and ALS point cloud data using both UAS and ALS dtm data

[**las_height-normalization_cluster,R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/las_height-normalization_cluster.R): Height normalizes point clouds, as above, but optimized for linux cluster (i.e. high memory capacity) use by foregoing lascatalog

##### 2. Cloth simulation parameter optimization and DTM generation testing

##### 3. Canopy height model generation

[**chm_data-compile.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/chm_data-compile.R): Generates large dataset containing chm values, dtm values, errors, vegetation classes, topography classes, and burn severities. Also generates several reference plots.

[**chm_generation.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/chm_generation.R): Generates canopy height models for UAS and ALS height normalized data


  
x

csfparamtesting.R: Produces sequence of DTMs from a point cloud using Cloth Simulation Filter (CSF) ground finding algorithm with a supplemental NDVI reclassification filter in order to test parameter effects on DTM accuracy.

nflightcompare.R: Compares dtm generation results for uas point clouds generated from either one or two flight directions

plotlasreg.R: Plots two las files on top of one another to visualize if registration was performed successfully

stdcanopymetrics.R: Compares standard canopy metrics as implemented in lidR between UAS and ALS point clouds
