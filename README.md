# Applied-Statistics-2020Spring-Project
## Mixture cure model
This project contains two mixture cure models: logistic/Cure rate model and single-index/cure rate model. A mixture cure model assumes two parts: incidence and latency. Incidence means uncured status while latency means conditional survival function for the uncured subjects.

## Data
The data is about a dental disease which contains 65890 observations of 20 variables. In order to be operable, we select the observations with “tooth=2” which is a molar and rename some of the columns for convenience. Also, we remove some irrelevant variables.

## Code
data.r: data preprocessing and ploting

teeth_estimation.r: EM procedure for both SIC model and LC model

predict.r: compute prediction error for the incidence part

### optional Information
R version 3.6.3 (2020-02-29)

ggplot2, version 3.3.0 (https://cran.r-project.org/web/packages/ggplot2/index.html)

dplyr, version 0.8.5 (https://cran.r-project.org/web/packages/dplyr/index.html)

smcure, version 2.0 (https://cran.r-project.org/web/packages/smcure/index.html)

survival, version 3.1-12 (https://cran.r-project.org/web/packages/survival/index.html)
