# 50-State Redistricting Simulations

<img src="https://alarm-redist.github.io/assets/alarm_256_tr.png" align="right" height=128>

### The ALARM Project (Japan)

[![License: CC0 1.0](https://img.shields.io/badge/Data%20License-Public%20domain-lightgrey.svg)](https://creativecommons.org/publicdomain/zero/1.0/)
[![License: MIT](https://img.shields.io/badge/Software%20License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This repository contains code to sample districting plans for the 15 Japanese prefectures subject to 10増10減 (changes in the number of Lower House seats) following the demographic shifts reflected in the 2020 Census, according to relevant legal requirements.

The sampled plans and accompanying summary statistics may be downloaded from
the dataverse (link forthcoming)
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

Unless otherwise noted, data (shapefiles and census) for each prefecture comes from the Japanese Ministry of Land, Infrastructure, Transport and Tourism (MLIT) or the Statistics Bureau of Japan:
[2015 Census Files](https://github.com/reiy24/jcdf_data/releases/tag/06232021), 2020 Census Files (forthcoming).

Exceptions to these data sources are listed in the individual documentation files 
in the `analyses/` folder.

## Contributing an Analysis
Please read the contribution guidelines (forthcoming).
