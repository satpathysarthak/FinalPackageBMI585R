# FinalPackageBMI585R

An R package with functions that are commonly used in Biostatistics. This is the final assignment for the course BMI-585R Biostatistics for Machine Learning at Emory University. 

## Installation:

```install
devtools::install_git("https://github.com/satpathysarthak/FinalPackageBMI585R.git", force = T)
library(FinalPackageBMI585R)
```

### Requirements

The following dependencies can be installed for smoother working of functions.

```setup
install.packages('tibble')
install.packages('dplyr')
install.packages('tidyr')
install.packages('ggplot2')
install.packages('margittr')
install.packages('tidyvers3')
```

### Important Directories

 The directory that stores the documentation for functions in [R](R) directory. It is generated from Roxygen.

Tnhe directory that stores the functions


## Functions and their usage

The packages are well documented and can be accessed by `?function.name` where function.name is the function name from the following list: 

```functions
accuracy()
adjR2()
bhAdjust()
boxMuller()
chiSquareCounts()
effectSize()
f1()
fdrAdjust()
minimumN()
pcApprox()
pcLollipop()
postHocPower()
ppv()
r2()
sensitivity()
specificity()
twoSidedT()
twoSidedZ()
unscale()
welchT()
```

## Testing the functions

A link to the [PDF](test_usage.pdf) and [HTML](test_usage.html) document for testing the functions is attached here. It can be used to view the expected output format and some calculations to check whether the function comes through to the end-user
