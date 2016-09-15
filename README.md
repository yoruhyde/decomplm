
<!-- README.md is generated from README.Rmd. Please edit that file -->
decomplm
========

A package for various type of linear model decomposition.

Installation
------------

You can install `decomplm` from `github` using the `devtools` package

``` r
install.packages("devtools")
require(devtools)
install_github('yoruhyde/decomplm')
library(decomplm) # load package
```

Example
-------

Usually, work directory, data\_var and cs\_var change everytime you setting up a new project. Make sure double check them before running the code.

``` r
setwd("C:\\Users\\yuemeng1\\Desktop\\code\\decomplm\\data") # set work directory
data=fread("input_data.csv") # load data
var=fread("input_var.csv") # load variable coefficients
layer=fread("input_layer.csv") # load model structure

date_var="week" # date variable name in input_data
cs_var=c("cs") # variable names for cross section in input_data; support multiple dimensions
```

Then you can run the code below to get the final decomposition of the main model

``` r
# Get decomp for all models before passing through

decomp_list=get_log_decomp(input_data=data,
                       input_var=var,
                       input_layer=layer)


# Specify the model name below if you want to see the passed through decomp of the models other than the main one.
# For example, in the sample data, if you want to see the final result of Web model, 
# then change the last argument into model_name="web"

result=get_reduced_model(decomp_list=decomp_list,
                         layer=layer,
                         cs=cs_var,
                         date=date_var,
                         is.output=T,
                         model_name=NULL)



runApp(result$app) # activate output panel
```
