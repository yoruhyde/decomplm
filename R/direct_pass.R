#' @return an updated input_files
#' @export
direct_pass=function(higher_model,lower_model,input_var=var,cs=cs_var) {
  lower_model=strsplit(lower_model,",")[[1]]
  higher_var=input_var[model_name_group %in% c(higher_model)]
  lower_var=input_var[model_name_group %in% c(lower_model)]
  higher_keep=higher_var[!var_group %in% c(lower_model)]
  higher_pass=higher_var[var_group %in% c(lower_model)]

  #higher keep for keep, higher_pass for pass, lower_var for pass

  setnames(higher_pass,c("var_group","para"),c("key1","para_temp"))
  setnames(lower_var,"model_name_group","key1")
  lower_var[,model_var:=NULL]
  higher_pass[,var:=NULL]

  passed_table=merge(lower_var,higher_pass,by=c(cs,"key1"),all.x=T)
  passed_table[,para:=para*para_temp]
  passed_table[,c("para_temp","key1"):=NULL]
  final=rbind(higher_keep,passed_table)
  final=final[,lapply(.SD,sum),by=c(colnames(final)[!colnames(final) %in% c("para")])]


  #update input_var
  input_var=input_var[!model_name_group %in% c(higher_model)]
  input_var=rbind(input_var,final)
  return (input_var)
}


#' @export
direct_weight=function(input_layer=layer,input_var=var,
                       input_data=data,cs=cs_var,date=date_var,is.output.final=F,model_name=NULL) {
  print("Note: You are using direct weighting approach across all models")
  input_layer[,var_group:=as.character(var_group)]
  for (i in nrow(input_layer):1) {
    if(input_layer[i, var_group] == "" |is.na(input_layer[i, var_group])) {
      next
    } else {
      higher_model=input_layer[i,model_name_group]
      lower_model=input_layer[i,var_group]
      input_var=direct_pass(higher_model,lower_model,input_var=input_var)
    }

  }
  if(is.null(model_name)) {
    model_name=input_layer[1,model_name_group]
  }
  input_layer=input_layer[model_name_group==model_name]
  input_layer[,var_group:=""]

  decomp_list=get_log_decomp(input_data=input_data,
                             input_var=input_var,
                             input_layer=input_layer)

  result=get_reduced_model(decomp_list=decomp_list,
                           layer=input_layer,
                           input_var=input_var,
                           cs=cs,
                           date=date,
                           is.output=is.output.final,
                           model_name=model_name)
  return (result)
}

#' Using this function when you have fake model. But lower then fake model,
#' you have some models to be passed thru by direct weight.
#' @export
mixed_approach = function (input_data=data,input_layer=layer,input_var=var,is.output.final=F,model_name=NULL) {
  layer_keep=input_layer[is_direct!=1]
  layer_1st=input_layer[var_group==""|is.na(var_group)]
  layer_direct=input_layer[is_direct==1]
  direct_mod_name=paste(layer_direct$model_name_group,collapse = ",")


  print(paste("Note: You are using direct weighting approach for ",direct_mod_name," and seperate decomp approach for others."),sep="")

  # using direct weighting method for some models and update var_table
  for (i in nrow(layer_direct):1) {
    if(layer_direct[i, var_group] == "" |is.na(layer_direct[i, var_group])) {
      next
    } else {
      higher_model=layer_direct[i,model_name_group]
      lower_model=layer_direct[i,var_group]
      input_var=direct_pass(higher_model,lower_model,input_var=input_var)
      layer_direct[i,var_group:=""]
    }

  }

  #update layer table
  layer_direct=rbind(layer_direct,layer_1st)
  sub_mod=paste(layer_keep$var_group,collapse=",")
  sub_mod=strsplit(sub_mod,",")[[1]]
  index=layer_direct$model_name_group %in% sub_mod
  layer_direct=layer_direct[index]
  layer_final=rbind(layer_keep,layer_direct)

  if(is.null(model_name)) {
    model_name=layer_final[1,model_name_group]
  }

  #using seperate decomp for the rest of models
  decomp_list=get_log_decomp(input_data=data,
                             input_var=input_var,
                             input_layer=layer_final)

  result=get_reduced_model(decomp_list=decomp_list,
                           layer=layer_final,
                           input_var=var,
                           cs=cs_var,
                           date=date_var,
                           is.output=is.output.final,
                           model_name=model_name)
  return(result)

}


# input_layer=layer
# input_var=var
# input_data=data
# cs=cs_var
# date=date_var
# is.output.final=F
# model_name=NULL
