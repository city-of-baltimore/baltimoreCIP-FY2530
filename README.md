# FY2025-2030 Baltimore City Capital Improvement Program Report

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

This repository includes the code used to produce the FY2025-2030 Baltimore City Capital Improvement Program Report.

For questions, please contact Eli Pousson, Data Lead with the Baltimore
City Department of Planning, at eli.pousson\@baltimorecity.gov.

> [!TIP]
>
> This project is built using {targets} and {tarchetypes}: two R
> packages designed to support the reproducible analytical pipelines
> (RAPs). For more information on {targets}, see [The {targets} R
> package user manual](https://books.ropensci.org/targets/).

## Background

The [Capital Improvement Program](https://planning.baltimorecity.gov/planning-capital-improvement/) is a six-year plan for funding capital projects by City agencies. The program is updated and adopted each year as part of the Baltimore City Budget.

This repository holds the code used to combine Capital Improvement Program data from Adaptive Planning with related reference data and location data to produce the annual report at each stage of review and approval. Other open-source repositories with supporting code for the Baltimore City Capital Improvement Program include:

- [baltimoreCIP](https://github.com/city-of-baltimore/baltimoreCIP/): Program and location data from 2008 to 2024.
- [baltimoreCIP-Workday-EIB](https://github.com/city-of-baltimore/baltimoreCIP-Workday-EIB/): Reproducible pipeline used to transform the Adaptive Planning data for loading into Workday.


## Organization

Supporting functions for this pipeline are located in the `R` folder. The source data is stored in the un-tracked folder `_targets/user/data` and can be provided to authorized users where appropriate.

As of August 2024, this project includes exports from the “Capital Projects - Six-Year CIP” sheet and the “Capital Projects - Project Details” sheet from Adaptive Planning.

## Usage

To run this pipeline, add the required data files to `_targets/user/data` and then run `targets::tar_make()`. The output files are stored in the `_output` folder.
