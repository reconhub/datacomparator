
## Welcome to the *datacomparator* project

<!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Introduction

This project aims to provide a shiny app for comparing different (dated)
versions of a given dataset. The app will allow:

* to upload separately the *old*, and *new* dataset from an `.xlsx` file

* to compare **data structures** using [`linelist::compare_data()`](https://www.repidemicsconsortium.org/linelist/reference/compare_data.html)

* to visualise **changes in data entries** using [`compareDF::compare_df()`](https://github.com/alexsanjoseph/compareDF)

* to identify **duplicates** in *old* and *new* data

* to visualise **changes in duplicates**

## Installation

First, install the `remotes` package:

```r
install.packages("remotes")
```

Use `remotes` to install `datacomparator`:

```r
remotes::install_github("paulc91/datacomparator")
```

## App Instructions

To launch the shiny app:

```r
datacomparator::run_app()
```

## Potential data security concerns

To update

## Contributors and collaborators

This project is currently only a concept. We are currently looking for
volunteers to help :)

Paul Campbell from [Epicentre MSF](https://epicentre.msf.org/en/acceuil)

---
 
Please note that the 'datacomparator' project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
