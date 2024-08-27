# 47-Prefecture Redistricting Simulations

<img src="https://alarm-redist.github.io/assets/alarm_256_tr.png" align="right" height=128>
<img src="assets/japan_47pref_256_tr.png" align="right" height=128>

## The ALARM Project (Japan)

[![License: CC0 1.0](https://img.shields.io/badge/Data%20License-Public%20domain-lightgrey.svg)](https://creativecommons.org/publicdomain/zero/1.0/)
[![License: MIT](https://img.shields.io/badge/Software%20License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Dataverse DOI-10.7910/DVN/Z9UKSH](<https://img.shields.io/badge/Dataverse DOI-10.7910/DVN/Z9UKSH-orange>)](https://doi.org/10.7910/DVN/Z9UKSH)

The goal of the 47-Prefecture Simulation Project is to generate and analyze redistricting plans for the single-member districts of the House of Representatives of Japan using a redistricting simulation algorithm. 
In this project, we analyzed the partisan bias of the 2022 redistricting for 25 prefectures subject to redistricting.
Our simulations are designed to comply with the [redistricting rules](https://www.soumu.go.jp/main_content/000794997.pdf) that the Council abides by. 

This repository contains code to sample districting plans for the Japanese House of Representatives for the prefectures subject to redistrict following the demographic shifts reflected in the 2020 Census, according to relevant legal requirements.

The sampled plans and accompanying summary statistics may be downloaded from
the [dataverse](https://doi.org/10.7910/DVN/Z9UKSH)
for this project. These consist of four files for each analysis:
- a documentation file describing data formats, analysis decisions, and data sources
- a CSV file of summary statistics for each of the generated plans
- two `.rds` files containing `redist_map` and `redist_plans` objects, which contain the actual shapefiles and district assignment matrices and may be used for further analysis.

## Acknowledgement

We acknowledge the partial support from the [Edwin O. Reischauer Institute of Japanese Studies (RIJS)](https://rijs.fas.harvard.edu) at Harvard University. We also thank Harvard Data Science Initiative and Microsoft for computational support.

## Repository Structure

- `analyses/` contains the code for each self-contained analysis
- `function/` contains common analysis and repository management code

## Data Sources

- [2020 Census](https://www.e-stat.go.jp/stat-search/files?page=1&toukei=00200521&tstat=000001136464&cycle=0&tclass1=000001136472)
- [2000 Census](https://www.e-stat.go.jp/gis/statmap-search?page=1&type=2&aggregateUnitForBoundary=A&toukeiCode=00200521&toukeiYear=2000&serveyId=A002005212000&coordsys=1&format=shape&datum=2000)
- [Periodical Ferry Route: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N09.html)
- [Lake and Ponds: National Geographic Information Download Service](https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-W09-v2_2.html)
- [Vote record of the House of Councilors election of July 21st 2019](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin25/index.html)
- [Vote record of the House of Councilors election of July 10th 2022](https://www.soumu.go.jp/senkyo/senkyo_s/data/sangiin26/index.html)

## Contributing an Analysis
Please read the contribution guidelines (forthcoming).
