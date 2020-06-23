# Modeling an Integrated HIV Prevention and Care Continuum to Achieve the Ending the HIV Epidemic Goals

This repository holds the source to code to reproduce the analysis featured in our HIV transmission model among men who have sex with men in the United States. This study investigated how scaling-up HIV prevention and care activities could reach the US Ending the HIV Epidemic (EHE) plan goals over the next decade.

## Citation

> Jenness SM, Johnson JA, Hoover KW, Smith DK, Delaney KP. Modeling an Integrated HIV Prevention and Care Continuum to Achieve the Ending the HIV Epidemic Goals. _Under Review_. [[Pre-Print]](https://doi.org/10.1101/2020.03.02.20030254)

<img src="https://github.com/EpiModel/CombPrev/raw/master/analysis/fig/Figure2.png">

## Abstract

#### Objective 	
We sought to evaluate which combinations of HIV prevention and care activities would have the greatest impact towards reaching the US Ending the HIV Epidemic (EHE) plan goals of reducing HIV incidence at least 75% by 2025 and 90% by 2030.

#### Design 	
A stochastic HIV transmission model for men who have sex with men (MSM), calibrated to local surveillance estimates in the Atlanta area, a focal EHE target jurisdiction.

#### Methods 	
Model scenarios varied HIV screening rates relative to current levels, under different assumptions of how HIV-negative MSM would be linked to PrEP initiation, and also considered improvements to HIV care linkage and retention for those screening positive.

#### Results 	
A 10-fold relative increase in HIV screening rates (to approximately biannual screening for black and Hispanic MSM and quarterly for white MSM) would lead to 41% of infections averted if integrated with PrEP initiation. Improvements to HIV care retention would avert 33.5% of infections if retention rates were improved 10-fold. If both screening and retention were jointly improved 10-fold, up to 67% of cumulative infections would be averted. Under this scenario, it would take 7.3 years to meet the 75% EHE goal and 30.4 years to meet the 90% goal for MSM in Atlanta.

#### Conclusions 	
Interventions to improve HIV screening linked with PrEP for those screening negative, and HIV care retention would have a substantial impact on HIV prevention. However, additional interventions will be necessary to reach the EHE goal of a 90% reduction in incidence for Atlanta MSM by 2030.

<img src="https://github.com/EpiModel/CombPrev/raw/master/analysis/fig/Figure3.png">

## Model Code

These models are written and executed in the R statistical software language. To run these files, it is necessary to first install our epidemic modeling software, [EpiModel](http://epimodel.org/), and our extension package specifically for modeling HIV/STI transmission dynamics among MSM, [EpiModelHIV](http://github.com/statnet/EpiModelHIV).

In R:
```
install.packages("EpiModel", dep = TRUE)
install.packages("tergmLite")

# install remotes if necessary, install.packages("remotes")
remotes::install_github("statnet/EpiModelHPC")
remotes::install_github("statnet/EpiModelHIV", ref = "CombPrev")
```
