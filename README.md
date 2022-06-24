## The Role of HIV Partner Services in the Modern Biomedical HIV Prevention Era: A Network Modeling Study

This repository holds the source to code to reproduce the analysis featured in our HIV transmission model among men who have sex with men in the United States. This study investigated how scaling-up HIV partner services could achieve HIV prevention  the US Ending the HIV Epidemic (EHE) plan goals over the next decade.

## Citation

Jenness SM, Le Guillou A, Lyles C, Bernstein KT, Krupinsky K, Enns EA, Sullivan PS, Delaney KP. The Role of HIV Partner Services in the Modern Biomedical HIV Prevention Era: A Network Modeling Study. 2022. *Pre-Print at medRxiv*: [https://doi.org/10.1101/2022.05.20.22275395]


## Abstract

#### Background
HIV partner services can accelerate the use of antiretroviral-based HIV prevention tools (ART and PrEP), but its population impact on long-term HIV incidence reduction is challenging to quantify with traditional PS metrics of partner identified or HIV-screened. Understanding the role of partner services within the portfolio of HIV prevention interventions, including using it to efficiently deliver antiretrovirals, is needed to achieve HIV prevention targets. 	

#### Methods 	
We used a stochastic network model of HIV/STI transmission for men who have sex with men (MSM), calibrated to surveillance-based estimates in the Atlanta area, a jurisdiction with high HIV burden and suboptimal partner services uptake. Model scenar-ios varied successful delivery of partner services cascade steps (newly diagnosed “index” patient and partner identification, partner HIV screening, and linkage or reengagement of partners in PrEP or ART care) individually and jointly.

#### Results 	
At current levels observed in Atlanta, removal of HIV partner services had minimal impact on 10-year cumulative HIV incidence, as did improving a single partner services step while holding the others constant. These changes did not sufficiently impact overall PrEP or ART coverage to reduce HIV transmission. If all index patients and partners were identified, maximizing partner HIV screening, partner PrEP provision, partner ART linkage, and partner ART reengagement would avert 6%, 11%, 5%, and 18% of infections, respectively. Realistic improvements in partner identification and service delivery were estimated to avert 2–8% of infections, depending on the combi-nation of improvements.

#### Conclusions 	
Achieving optimal HIV prevention with partner services depends on pairing improve-ments in index patient and partner identification with maximal delivery of HIV screen-ing, ART, and PrEP to partners if indicated. Improving the identification steps without improvement to antiretroviral service delivery steps, or vice versa, is projected to result in negligible population HIV prevention benefit. 

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
