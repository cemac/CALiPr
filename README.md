<!-- image header -->
<div align="center">
<a href="https://www.cemac.leeds.ac.uk/">
  <img src="https://github.com/cemac/cemac_generic/blob/master/Images/cemac.png"></a>
  <br>
</div>

# Malawi-CALiPr Data Exploration Tool #

[![GitHub release](https://img.shields.io/github/release/cemac/CALiPr.svg)](https://github.com/cemac/CALiPr/releases) [![GitHub top language](https://img.shields.io/github/languages/top/cemac/CALiPr.svg)](https://github.com/cemac/CALiPr) [![GitHub issues](https://img.shields.io/github/issues/cemac/CALiPr.svg)](https://github.com/cemac/cemac_generic/CALiPr) [![GitHub last commit](https://img.shields.io/github/last-commit/cemac/CALiPr.svg)](https://github.com/cemac/CALiPr/commits/master) [![GitHub All Releases](https://img.shields.io/github/downloads/cemac/CALiPr/total.svg)](https://github.com/cemac/CALiPr/releases)![GitHub](https://img.shields.io/github/license/cemac/CALiPr.svg)


# Description #

This repository contains an app for the exploration of Farm Survey data for the project "Optimising Crop Residuals for Conservation Agriculture and Livestock Production in Malawi", or "Malawi-CALiPr". 

The app is written using Shiny, a GUI creator for R, and is run through RStudio. It uses as inputs the csv and shape files included in this repository, which should be kept in the same folder as the R script files.

Further documentation will be included in the [wiki](https://github.com/cemac/CALiPr/wiki) for this repository.

# Requirements #
This app **must** be run through RStudio, and was written using R version 3.5.1. 

For those running on a UNIX system, a conda environment yml file is included in this repository which can set up all the required packages and external programs. For windows the required packages are standard on University of Leeds systems.

# Installation #

Make sure you've met the Requirements, and ensure that you have obtained the correct and compatible version of the survey data CSV file, which is omitted from this repository.

## UNIX ##

Run the following commands to clone the repository and open 

```bash
git clone https://github.com/cemac/CALiPr
cd CALiPr
conda env create -f CALiPr.yml
conda activate CALiPr
rstudio ui.R
```
## Windows ##

Download a copy of the repository from the web interface or using the github GUI. Alternatively use the command `git clone  https://github.com/cemac/CALiPr` from within a git-bash terminal.

# Usage #

Place a copy of the survey csv file (expected filename is `FarmSurveyClean.csv`) in the same folder as the script files and run from within rstudio. Full instructions are given in the wiki [here](https://github.com/cemac/CALiPr/wiki/Running-the-app)


# Citing this work #

If you use this code we ask that you kindly cite the original source code using the above DOI.

<hr>

# Licence information #

This project is licensed under the terms of the MIT license.

# Acknowledgements #

This work was carried out by request of Dr Marcelo Valadares Galdos as part of the "Optimising Crop Residuals for Conservation Agriculture and Livestock Production" project. It makes use of various open source packages, among them [shinyjs](https://deanattali.com/shinyjs/), [plotly](https://plot.ly/r/), [shinyBS](https://ebailey78.github.io/shinyBS/), [shinyWidgets](https://github.com/dreamRs/shinyWidgets), [shiny](http://shiny.rstudio.com/) and [leaflet](https://leafletjs.com/), and obtains maps from [CartoDB](https://carto.com/attribution/), [OpenStreetMaps](https://www.openstreetmap.org/copyright), and [ESRI](https://www.esri.com/en-us/home). Malawi borders taken from shape files obtained at [maplibrary.org](http://www.maplibrary.org/library/stacks/Africa/Malawi/index.htm).
