# catchR

Package to analyse catch data.

- Includes functions for importing commercial data (ziff, nafo, carbio, length-frequency)
- Includes functions for catch-at-age

# Installation

devtools::install_github("iml-assess/catchR")

# Example

Mackerel: https://github.com/iml-assess/catchMackerel

# Workflow

To get catch-, weight- and length at age, the following workflow can be used:

1. read.ziff, for reading in ziff data (find.species can be used to help find the correct species code)
2. read.nafoA and read.nafoB, for reading in nafo data
3. read.lf, for reading in length frequency data
4. read.bio, for reading in data usually named 'carbio'
5. fit.lw, for fitting length-weight relationships
6. get.samples, for attributing samples to a certain catch level (catch by region, gear and period)
7. get.caa, for calculating catch/weight/length at age

# To do list
- remove dependency plyr
