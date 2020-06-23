# Network Targeted Strategies for Responding to HIV Outbreaks Under the Ending the HIV Epidemic Initiative

This repository holds the source to code to reproduce the analysis featured in our HIV transmission model among men who have sex with men in the United States. This study investigated how scaling-up HIV prevention and care activities could reach the US Ending the HIV Epidemic (EHE) plan goals over the next decade.

## Citation


## Abstract

#### Objective 	

#### Design 	

#### Methods 	

#### Results 	

#### Conclusions 	

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
