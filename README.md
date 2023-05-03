# Tree Equity and Avian Species Richness in Tucson, Arizona

## Summary

This project uses community science observations of birds to investigate the relationship between tree equity scores (TES) and avian species richness in the semi-arid city of Tucson, Arizona, USA. Tree equity data was sourced from the [City of Tucson open data portal](https://gisdata.tucsonaz.gov/), while avian occurrence data was acquired from the Global Biodiversity Information Facility [(GBIF)](https://www.gbif.org/). Avian species richness in Tucson's neighborhoods was calculated by aggregating and summarizing all GBIF observations for each neighborhood and determining the total number of unique species observed. A Poisson regression within a generalized linear modeling framework was employed, using the log of neighborhood area as an offset term to account for variations in neighborhood size.

## Folders

- `data`: raw data and coordinate bounds
- `output`: modified data, output plots
- `scripts`: (descriptions below)

## Scripts

- [`query_gbif.R`](https://github.com/Big-Biodiversity-Collaborative/urbanforest/blob/main/scripts/query_gbif.R): Function to download records from GBIF for a set of coordinates and list of taxa
- [`download_data.R`]( https://github.com/Big-Biodiversity-Collaborative/urbanforest/blob/main/scripts/download_data.R): Download GBIF observation data
- [`download_large_data.R`]( https://github.com/Big-Biodiversity-Collaborative/urbanforest/blob/main/scripts/download_large_data.R): Processing of two large manually downloaded datasets
- [`extract_gbif_data.R`]( https://github.com/Big-Biodiversity-Collaborative/urbanforest/blob/main/scripts/extract_gbif_data.R): Extract individual neighborhood data files downloaded from GBIF
- [`assemble_data.R`]( https://github.com/Big-Biodiversity-Collaborative/urbanforest/blob/main/scripts/assemble_data.R): Assemble GBIF files, add neighborhood ID, create rows if a neighborhood has no obs.
- [`neighborhood_richness.R`]( https://github.com/Big-Biodiversity-Collaborative/urbanforest/blob/main/scripts/neighborhood_richness.R): Summary of GBIF data and calculate neighborhood species richness
- [`spatial_join_model.py`]( https://github.com/Big-Biodiversity-Collaborative/urbanforest/blob/main/scripts/spatial_join_model.py): Model parameters used in ArcGIS Pro for spatial join between GBIF and TES datasets
- [`urbanforest_dataset.R`]( https://github.com/Big-Biodiversity-Collaborative/urbanforest/blob/main/scripts/urbanforest_dataset.R): Combined neighborhood avian species richness and tree equity values
- [`regression_analyses.R`]( https://github.com/Big-Biodiversity-Collaborative/urbanforest/blob/main/scripts/regression_analyses.R): Generalized Linear Model

## Dependencies

- `tidyverse`: Data wrangling and visualization
- `dplyr`: Data wrangling (included within tidyverse, but sometimes best to load separately)
- `ggplot2`: Data visualization (included within tidyverse, but sometimes best to load separately)
- `sf`: Point filtering
- `raster`: Work with raster data
- `rgbif`: Search and retrieve data from GBIF

