# Pepperwood 2019 UAS Study
R scripts associated with analysis of UAS point cloud data collected at Pepperwood Preserve during 2019 field campaign with Sonoma State University/University of Oxford

Developed on a windows machine. Some scripts set up for linux cluster use. See individual script headers for additional details.

Contact Sean Reilly (sean.reilly@ouce.ox.ac.uk) for questions. Please open issue notes as they arise.

## Table of contents:

### 1. Las file processing

[**las_postprocessing.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/las_postprocessing.R): Merges Pix4D las point cloud and multispectral data, reprojects point cloud, and clips to given boundary

[**las_transformation.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/las_transformation.R): Performs a spatial transformation on a las point cloud based on given transformation matrix

[**las_plot-registration.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/las_plot-registration.R): Plots two las files on top of one another to visualize if registration was performed successfully

[**las_height-normalization.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/las_height-normalization.R): Height normalizes UAS and ALS point cloud data using both UAS and ALS dtm data

[**las_height-normalization_cluster.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/las_height-normalization_cluster.R): Same as above, but optimized for linux cluster (i.e. high memory capacity) use by foregoing lascatalog

### 2. Cloth simulation parameter optimization and DTM generation testing

[**csf_paramtesting_rnd1.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/csf_parameter-testing_rnd1.R): Produces sequence of DTMs from a point cloud using Cloth Simulation Filter (CSF) ground finding algorithm with a supplemental NDVI reclassification filter in order to test parameter effects on DTM accuracy.

[**csf_paramtesting_rnd2.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/csf_parameter-testing_rnd2.R): Same as above but modified to only process one parameter at a time

[**csf_parameter-testing_data-compile.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/csf_parameter-testing_data-compile.R): Takes zonal error data from csf parameter testing and computes site-wide error values

[**csf_parameter-testing_visualization.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/csf_parameter-testing_visualization.R): Graphical visualization of CSF performance across parameter ranges

[**csf_parameter-testing_final-dtm-generation.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/csf_parameter-testing_final-dtm-generation.R): Produces final DTM from a point cloud using Cloth Simulation Filter (CSF) optimized parameter set

### 3. Canopy height model generation

[**chm_data-compile.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/chm_data-compile.R): Generates large dataset containing chm values, dtm values, errors, vegetation classes, topography classes, and burn severities. Also generates several reference plots.

[**chm_generation.R**](https://github.com/ussreilly/pepperwood_uasStudy_2019/blob/master/R/chm_generation.R): Generates canopy height models for UAS and ALS height normalized data
