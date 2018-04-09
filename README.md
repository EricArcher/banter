# banter

### To Do

* Consistently report percent correctly classified
* Include expected mean inbag in model diagnostic summary

* What text output needs to be available (data.frames to save)?
* Function argument checking
* Completing help files
* Unit tests
* Vignettes

### Description

*banter* is a package for creating hierarchical acoustic event classifiers.

### Installation

To install the latest version from GitHub:

```r
# make sure you have Rtools installed
if(!require('devtools')) install.packages('devtools')
# install from GitHub
devtools::install_github('ericarcher/banter', build_vignettes = TRUE)
```

### Tutorial

Training a BANTER model:
```r
data(train.data)
ex.mdl <- initBanterModel(train.data$events) %>% 
  addBanterDetector(train.data$detectors, ntree = 10, sampsize = 2) %>% 
  runBanterModel(ntree = 5000, sampsize = 3)
summary(ex.mdl)
```

Predicting new data with an existing BANTER model:
```r
data(test.data)
predict(ex.mdl, test.data)
```

### Contact

* submit suggestions and bug-reports: <https://github.com/ericarcher/banter/issues>
* send a pull request: <https://github.com/ericarcher/banter/>
* e-mail: <eric.archer@noaa.gov>

### Reference
Rankin, S., Archer, F., Keating, J. L., Oswald, J. N., Oswald, M., Curtis, A. and Barlow, J. (2017) Acoustic classification of dolphins in the California Current using whistles, echolocation clicks, and burst pulses. Mar Mam Sci, 33: 520-540. [doi:10.1111/mms.12381](https://onlinelibrary.wiley.com/doi/abs/10.1111/mms.12381)

### version 0.0.1 (current on GitHub)

* Initial release