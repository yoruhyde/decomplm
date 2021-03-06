#' get_log_decomp:
#' calculate the decomp for all true & fake models & sub-models by using the algorithm described in 'decomp_log'
#' @param input_data a data.table class dataset for modeling.
#' @param input_var a data.table class dataset for variable coefficient.
#' @param input_layer a data.table class dataset for model structure.
#' @return a list of modeling result. It contains the list of decomp for all models
#'
#' get_reduced_model:
#' @param decomp_list is from the result of get_decomp, it should be a list contains all the models' decomp
#' @param layer is the set up file layer, which specified the structure of the model
#' @param input_var a data.table class dataset for variable coefficient.
#' @param cs a charactore vector for the cross section variable names in input_data. It could be more than one variable.
#' @param date a charactor of the date variable name in input_data.
#' @param is.output if TRUE, then output tabels to work directory. Default is False.
#' @param model_name if NULL, then output the passed-thru model on the first layer from input_layer.
#'        Specific the model_name if need. I.E. model_name="web"
#' @return a list of modeling result. It contains the following components:
#'    contribution: contribution table;
#'    decomp: decomp table;
#'    for.plot: table for shiny;
#'    app: a shiny app;
#'
#'
#'@export
seperate_decomp=function(is.output.final=T,model_name=NULL) {
  print("Note: You are using seperate decomp approach across all models")
  decomp_list=get_log_decomp(input_data=data,
                             input_var=var,
                             input_layer=layer)

  result=get_reduced_model(decomp_list=decomp_list,
                           layer=layer,
                           input_var=var,
                           cs=cs_var,
                           date=date_var,
                           is.output=is.output.final,
                           model_name=model_name)
  return (result)
}




#' @export
get_log_decomp=function(input_data,input_var,input_layer) {
  model_nm=input_layer[true_model==1,model_name_group]
  model_fake=input_layer[true_model==0,model_name_group]

  if ((length(model_nm)+length(model_fake))==0) {
    print("Warning: You don't have any models in the layer set up. Please check input_layer")
    return(input_layer)
  }

  decomp.list=list()
  for(i in 1:length(model_nm)) {
    s_f=input_layer[model_name_group==model_nm[i],sm_factor]
    decomp.list[[model_nm[i]]]=decomp_log(
      input_data=input_data,
      input_var=input_var,
      y=unique(input_var[model_name_group==model_nm[i],model_var]), # dependent variable name in input_data
      y_m=NULL, # for ninah index process
      date=date_var, # date variable name in input_data
      cs=cs_var, # variable names for cross section in input_data; support multiple dimensions
      sm_factor=s_f
    )$decomp
  }

  if(length(model_fake)!=0) {
    temp=list()
    model_fake_dep=paste(model_fake,"_dep",sep="")
    for(i in 1:length(model_fake)) {
      var_split=input_layer[model_name_group==model_fake[i],var_group]
      var_split=strsplit(var_split,",")[[1]]
      f_getdep=function(x,input_var) {
        return(unique(input_var[model_name_group==x,model_var]))
      }
      var_split_dep=sapply(var_split,f_getdep,input_var)
      var_split_dep=paste(var_split_dep,"_dep",sep="")
      for(j in 1:length(var_split_dep)) {
        temp[[j]]=decomp.list[[var_split[j]]][,c(cs_var,date_var,var_split_dep[j]),with=F]
      }
      decomp.list[[model_fake[i]]]=Reduce(function(...) merge(...,by=c(cs_var,date_var),all=T),temp)
      expr=paste(var_split_dep,collapse = "+")
      expr=parse(text=paste(model_fake_dep[i],":=",expr,sep=""))
      decomp.list[[model_fake[i]]][,eval(expr)]
      setnames(decomp.list[[model_fake[i]]],c(var_split_dep),c(var_split))
    }
  }
  decomp.list$fake_model=model_fake
  decomp.list$true_model=model_nm

  return(decomp.list)
}


#' @export
get_reduced_model=function(decomp_list,layer,input_var=var,cs,date,is.output=F,model_name=NULL) {
  layer[,var_group:=as.character(var_group)]
  for (i in nrow(layer):1) {
    if(layer[i, var_group] == "" |is.na(layer[i, var_group])) {
      next
    } else {
      psthru_var=strsplit(layer[i,var_group],",")[[1]]
      temp_psthru=list()
      for(j in 1:length(psthru_var)) {
        if (psthru_var[j] %in% decomp_list$fake_model) {
          dependent=psthru_var[j]
        } else {
          dependent=unique(input_var[model_name_group==psthru_var[j],model_var])
        }
        tryCatch({
          temp_psthru[[psthru_var[j]]] =
            get_pec(decomp_list,model_name=psthru_var[j],
                    dep_var=dependent)
        }, error = function(e) {
          stop(paste(psthru_var[j]," model is not in your decomp list. Please check set up files.",sep=""))
        }, finally={

        })

      }
      percent=Reduce(rbind,temp_psthru)

      #pass thru into main model
      temp_keep=decomp_list[[layer[i,model_name_group]]]
      temp_pass=temp_keep[,c(cs_var,date_var,psthru_var),with=F]
      temp_keep=temp_keep[,c(colnames(temp_keep)[!colnames(temp_keep) %in% c(psthru_var)]),with=F]
      temp_pass=melt.data.table(temp_pass,id.vars = c(cs_var,date_var),variable.name="model",value.name = "final")
      temp_keep=melt.data.table(temp_keep,id.vars = c(cs_var,date_var),variable.name="variable",value.name = "final")

      temp_pass=merge(percent,temp_pass,by=c(cs_var,date_var,"model"),all.x=T)
      temp_pass[,final:=final*value]
      temp_pass[,c("value","model"):=NULL]
      temp=rbind(temp_pass,temp_keep)
      temp=temp[,lapply(.SD,sum),by=c(cs_var,date_var,"variable")]

      fml=as.formula(paste(paste(c(cs_var,date_var),collapse = "+"),"~variable",sep=""))
      final_ps=dcast.data.table(temp,fml,fun.aggregate = sum,value.var = "final")

      #renew_decomp_list
      decomp_list[[layer[i,model_name_group]]]=final_ps
    }
  }
  if(is.null(model_name)) {
    model_name=layer[1,model_name_group]
  }
  model_name_dep=unique(input_var[model_name_group==model_name,model_var])
  model_name_dep=paste(model_name_dep,"_dep",sep="")
  decomp_reduced=decomp_list[[model_name]]
  var.list=colnames(decomp_reduced)[!colnames(decomp_reduced) %in% c(cs,date,model_name_dep,"Base")]
  var_lkup=data.table(var=var.list,var_group=var.list)
  # var_lkup=merge(var_lkup,unique(input_var[,c("var","var_group"),with=F]),by=c("var"),all.x=T)

  expr=paste(paste("ifelse(",var.list,"<0,",var.list,",0)"),collapse = "+")
  expr=paste("Base:=Base+",expr,sep="")
  decomp_reduced[,eval(parse(text=expr))]

  decomp.list=f_decomp_split(decomp=decomp_reduced,cs,y=model_name_dep,date,input_var=var_lkup)
  # app=rshiny(decomp.list$con,decomp.list$decomp.chart,date,model_name)
  app=rshiny2(decomp.list$con,decomp.list$decomp.chart,date,model_name_dep)

  if (is.output){
    write.csv(decomp.list$decomp.export,"output_decomp.csv",row.names = F)
    #write.csv(decomp.list$con,"output_contribution.csv",row.names = F)
  }

  # return(decomp_reduced=decomp_list[[layer[1,model]]],decomp_all=decomp_list)
  return(list(decomp=decomp.list$decomp.export,contribution=decomp.list$con,for.plot=decomp.list$decomp.chart,app=app))

}
