
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Package overview: `netropy` <img src="man/figures/hex_netropy.png" align="right" width="300px"/>

## Installation

<!-- You can install the released version of netropyfrom [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("netropy") -->
<!-- ``` -->

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("termehs/netropy")
```

## Statistical Entropy Analysis of Network Data

Multivariate entropy analysis is a general statistical method for
analyzing and finding dependence structure in data consisting of
repeated observations of variables with a common domain and with
discrete finite range spaces. Only nominal scale is required for each
variable, so only the size of the variable’s range space is important
but not its actual values. Variables on ordinal or numerical scales,
even continuous numerical scales, can be used, but they should be
aggregated so that their ranges match the number of available repeated
observations. By investigating the frequencies of occurrences of joint
variable outcomes, complicated dependence structures, partial
independence and conditional independence as well as redundancies and
functional dependence can be found.

## Loading Internal Data

The different entropy tools are explained and illustrated by exploring
data from a network study of a corporate law firm, which has previously
been analysed by several authors
([link](https://www.stats.ox.ac.uk/~snijders/siena/Lazega_lawyers_data.htm)).
The data set is included in the package as a list with objects
representing adjacency matrices for each of the three networks advice
(directed), friendship (directed) and co-work (undirected), together
with a data frame comprising 8 attributes on each of the 71 lawyers. To
load the data, extract each object and assign the correct names to them
we run the following syntax

``` r
data(lawdata) 
adj.advice <- lawdata[[1]]
adj.friend <- lawdata[[2]]
adj.cowork <-lawdata[[3]]
df.att <- lawdata[[4]]
```
