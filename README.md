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

These models are written and executed in the R statistical software language. To run these files, it is necessary to use the correct version of our epidemic modeling software, [EpiModel](http://epimodel.org/), and our extension package specifically for modeling HIV/STI transmission dynamics among MSM,
[EpiModelHIV](http://github.com/statnet/EpiModelHIV).

In R, load the necessary packages with the following command:
```
install.packages("renv")
renv::init()
```

Once `renv` has finished initializing, restart R.

### ARTnet Data Access 

To use this model, you will need a GitHub Private Access Token to install packages from private GitHub repositories (EpiModelHIV-p, ARTNetData). It should be set either in "~/.bashrc":
```
export GITHUB_PAT="<your github private access token>"
```

or in "~/.Renviron":
```
GITHUB_PAT="<your github private access token>"
```
