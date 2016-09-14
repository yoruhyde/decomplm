require(devtools)
install_github('yoruhyde/decomplm')
library(decomplm) # load package

# set up
setwd("C:\\Users\\yuemeng1\\Desktop\\code\\decomplm\\data") # set work directory
data=fread("input_data.csv") # load data
var=fread("input_var.csv") # load variable coefficients
layer=fread("input_layer.csv") # load model structure

date_var="week" # date variable name in input_data
cs_var=c("cs") # variable names for cross section in input_data; support multiple dimensions



########################################################


decomp_list=get_decomp(input_data=data,
                       input_var=var,
                       input_layer=layer)

result=get_reduced_model(decomp_list=decomp_list,
                         layer=layer,
                         input_var=var,
                         cs=cs_var,
                         date=date_var,
                         is.output=T,
                         model_name=NULL)
# specify the model name if you want to see the passed thru decomp of the models other than the main one,or just keep NULL


runApp(result$app) # activate output panel


