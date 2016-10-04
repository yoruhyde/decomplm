require(devtools)
install_github('yoruhyde/decomplm')
library(decomplm) # load package

# set up
setwd("C:\\Users\\yuemeng1\\Desktop\\code\\decomplm\\data") # set work directory
data=fread("input_data.csv") # load data
var=fread("input_var.csv") # load variable coefficients
layer=fread("input_layer.csv") # load model structure

date_var="week" # date variable name in input_data
# cs_var=c("bunit","route","product") # variable names for cross section in input_data; support multiple dimensions
cs_var=c("cs")



########################################################
if.direct=unique(layer$is_direct[!is.na(layer$is_direct)])

if (length(if.direct)==1) {
  if(if.direct==1) {
    result=direct_weight(is.output.final=T,model_name=NULL)
  } else {
    result=seperate_decomp(is.output.final=T,model_name=NULL)
  }
} else {
  result=mixed_approach(is.output.final=T,model_name=NULL)
}




runApp(result$app) # activate output panel


