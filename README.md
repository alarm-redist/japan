# 47-Prefecture Redistricting Simulations

<img src="https://alarm-redist.github.io/assets/alarm_256_tr.png" align="right" height=128>
<img src="assets/japan_47pref_256_tr.png" align="right" height=128>

### The ALARM Project (Japan)

[![License: CC0 1.0](https://img.shields.io/badge/Data%20License-Public%20domain-lightgrey.svg)](https://creativecommons.org/publicdomain/zero/1.0/)
[![License: MIT](https://img.shields.io/badge/Software%20License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Dataverse DOI-10.7910/DVN/Z9UKSH](<https://img.shields.io/badge/Dataverse DOI-10.7910/DVN/Z9UKSH-orange>)](https://doi.org/10.7910/DVN/Z9UKSH)

The goal of the 47-Prefecture Simulation Project is to generate and analyze redistricting plans for the single-member districts of the House of Representatives of Japan using a redistricting simulation algorithm. 
In this project, we use a redistricting simulation algorithm to generate redistricting plans that both limit the number of split municipalities and minimize the voting weight disparity (一票の格差) within each prefecture. 
Our simulations are designed to comply with the redistricting rules that the Council abides by. 
We will start with the 19 prefectures subject to redistricting and plan to expand the scope of our research to all 47 prefectures.

This repository contains code to sample districting plans for the Japanese House of Representatives for the prefectures subject to redistrict following the demographic shifts reflected in the 2020 Census, according to relevant legal requirements.

The sampled plans and accompanying summary statistics may be downloaded from
the [dataverse](https://doi.org/10.7910/DVN/Z9UKSH)
for this project. These consist of four files for each analysis:
- a documentation file describing data formats, analysis decisions, and data sources
- a CSV file of summary statistics for each of the generated plans
- two `.rds` files containing `redist_map` and `redist_plans` objects, which
contain the actual shapefiles and district assignment matrices and may be used
for further analysis.

## Repository Structure

- `analyses/` contains the code for each self-contained analysis
- `R/` contains common analysis and repository management code

## Data Sources

Unless otherwise noted, data for each prefecture comes from [the 2020 Japapnese Census (令和2年国勢調査)](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136472) 
and [the 2015 Japanese Census (平成27国勢調査)](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001080615) from the Statistics Bureau.
In these cases, shapefiles are also taken from the Census.

Also, for the additional geographical information used in this analysis (ferries, lakes, etc.)  comes from [Ministry of Land, Infrastructure, Transport and Tourism](https://nlftp.mlit.go.jp/ksj/index.html), 
and polygon files for the current legislative districts used for mapping comes from [Center for Spatial Information Science, at the University of Tokyo](https://home.csis.u-tokyo.ac.jp/~nishizawa/senkyoku/). 

## Contributing an Analysis
Please read the contribution guidelines (forthcoming).
