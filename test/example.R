require(devtools)
install_github('yoruhyde/decomplm')
library(decomplm) # load package

# setwd("/Users/yue.meng/Downloads/yin/decomplm/R")
# for(i in list.files()) {
#   source(i,local=F)
# }


# set up
setwd("/Users/yue.meng/Downloads/yin/decomplm/data") # set work directory
data=fread("input_data.csv") # load data
var=fread("input_var.csv") # load variable coefficients
layer=fread("input_layer.csv") # load model structure

date_var="week" # date variable name in input_data
cs_var=c("SEGMENT","CHANNEL")


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


