# Kendall transformation — computational experiments

This repository contains the code used to generate figures and table in the paper [*Kendall transformation: a robust representation of continuous data for  information theory*](https://arxiv.org/abs/2006.15991) by [Miron B. Kursa](https://orcid.org/0000-0001-7672-648X).

For a reference implementation of Kendall transformation, consider the [praznik](https://CRAN.R-project.org/package=praznik) R package.

## Results

The comparison of Kendall transformation with other estimators of mutual information, presented in the paper as Figure 2, can be reproduced with `fig-biv.R` script.

The evaluation of the ability of Kendall transformation to catch joint mutual information in a tri-variate setting based on synthetic benchmark, presented in the paper as Figure 3, can be reproduced with `fig-jmi.R` script.

The comparison of feature rankings obtained with various methods with the result of a standard, non-parametric statistical analysis of the *Morphine* data, presented in the paper as Table 1, can be reproduced with `tab-ag.R` script.

The comparison of the Random Forest accuracy on the original and Kendall transformed data, presented in the paper as Table 2, can be reproduced with `tab-cls.R` script.

The comparison of the stability of feature ranking when subjected to a simulated loss of calibration, presented in the paper as Table 3, can be reproduced with `tab-mrg.R` script.

## Datasets

The repository contains the `morphine.RDS` file with the [*Morphine* dataset](https://data.mendeley.com/datasets/s8mb5kbsv6/1) from the paper [*Inter-individual differences in serotonin and glutamate con-transmission reflect differentiation in context-induced conditioned 50-kHz USVs response after morphine withdrawal*](https://doi.org/10.1007/s00429-018-1683-4).
