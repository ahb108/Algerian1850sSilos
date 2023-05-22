# Cereal silo-pits, agro-pastoral practices and social organisation in 19th century Algeria: data and scripts

This repository contains all data and scripts required to fully reproduce all analyses presented in the following paper: 

Bevan, A., Cutler, B., Hennig, C. and Yermerche, O.  Cereal silo-pits, agro-pastoral practices and social organisation in 19th century Algeria (under review). 

The repository is organised as follows:
  - `./data` ... contains all datasets required for the analysis in both primary data  transcribed from the original documents and processed tabular data that has been reorganised and cleaned up in certain ways (see below)
  - `./outputs` ... contains pdf figures and data tables that are presented in the main text.
  -  `./scripts` ... contains R scripts for executing data preprocessing and core analyses

## Software
All data processing and analysis was conducted in R (version 4.2.2), on MacOS 13.0 (aarch64-apple-darwin20 64-bit), with the MASS (7.3-60), lme4 (1.1-33), terra (1.5-21), sf (1.0-12), rstanarm (2.21.4) and ape (5.7-1) packages.
  
## Primary Datasets
The primary datasets are stored as tab-delimited files in `./data/raw` and have been transcribed from documents in the Archives Nationales d'Outre-Mer (ANOM, Aix-en-Provence, France). Further details on the bibliographic and archival sources are provided in the main text and in the datasets themselves.

## Data Preparation
The above primary datasets have been reorganised and cleaned up via scripts in  `./scripts/0_prep` and the processed versions are then stored in `./data/postprep`, again as tab-delimited files. This preparation includes locational querying of raster layers by polygons representing reconstructed tribal territories and further combination of the original silo register and census data in more convenient ways for subsequent analysis.

## Analysis
The files  in `./scripts/1_main` provide a sequence of steps through the main analyses. They do not use primary datasets but instead operate exclusively on the pre-processed files in `./data/postprep`.  Working directories will need to be changed for the scripts to operate on a new computer. Simple R scripts have bee preferred over integrated R markdown documents, although there is some wider comment provided in each script.

# Funding Statement
This project is not funded directly by any grant and is instead the result of ordinary institutionally-allocated research time for each of the authors.

# Licence
CC-BY 3.0
