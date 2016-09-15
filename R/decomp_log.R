#' Decomp and contribution calculation function for multiplicative model
#'
#' A function to calucate decomp and contribution for multiplicative model
#'
#' @param input_data a data.table class dataset for modeling.
#' @param input_var a data.table class dataset for variable coefficient.
#' @param y a charactor for the dependent variable name in input_data.
#' @param y_m a charactor for the mean of dependent variable name in input_data; default is NULL. If not NULL, then is.index must be True.
#' @param date a charactor of the date variable name in input_data.
#' @param cs a charactore vector for the cross section variable names in input_data. It could be more than one variable.
#sm_factor the smoothing factor for base exponential smoothing. The value range is from 0 to 1. The bigger, the more weight on current period status.
#' @param sm_factor the smoother span. This gives the proportion of points in the plot which influence the smooth at each value. Larger values give more smoothness.
#' @param is.index if TRUE, then y_m argument need to be setup. Default is False.
#' @param is.output if TRUE, then output tabels to work directory. Default is False.
#'
#' @return a list of modeling result. It contains the following components:
#'    con: contribution table;
#'    decomp: decomp table;
#'    app: a shiny app;
#'
#' @export
decomp_log=function(input_data,input_var,y,y_m=NULL,date,cs,sm_factor,is.output=F,is.index=F){
  # # setup
  # setwd("c:\\Users\\XinZhou\\Documents\\GitHub\\lmdecomp\\data-raw")
  # library(data.table);library(bit64);library(forecast);library(shiny);library(rCharts);library(DT)
  # input_data=fread("input_data.csv")
  # input_var=fread("input_var.csv")
  # y="sales"
  # y_m=NULL
  # date="week"
  # cs=c("cs1","cs2")
  # sm_factor=0.9 # sm base alpha, the bigger the more weight for current status, 0 means no smooth
  # is.output=F
  # is.index=F
  ################################################################################################
  # take var list
  input_var=input_var[model_var==y]
  var.list=unique(input_var[model_var==y,"var",with=F])$var
  # drop useless var
  input_data=input_data[,c(cs,date,y,y_m,var.list),with=F]
  # convert int to double
  int.index=which(sapply(input_data,is.integer))
  for (k in int.index) set(input_data, j=k, value=as.numeric(input_data[[k]]))
  # multiply by mean var
  if (is.index){
    expr=paste(c(y,var.list),"=",c(y,var.list),"*",y_m)
    expr=paste(expr,collapse = ",")
    expr=paste("':='(",expr,")")
    input_data[,eval(parse(text=expr))]
    input_data=input_data[,!y_m,with=F]
  }
  # calc y actual
  expr=paste(y,"_actual:=ifelse(",y,"==0,0,exp(",y,"))",sep="")
  input_data[,eval(parse(text=expr))]
  # formatting
  expr=paste(cs,"=as.character(",cs,")")
  expr=paste(expr,collapse = ",")
  expr=paste("':='(",expr,")")
  input_data[,eval(parse(text=expr))]
  expr=paste(date,":=as.Date(",date,",'%m/%d/%Y')")
  input_data[,eval(parse(text=expr))]
  index=which(sapply(input_data,is.integer))
  for (k in index) set(input_data, j=k, value=as.numeric(input_data[[k]]))
  input_var[,':='(model_var=as.character(model_var),var=as.character(var),para=as.numeric(para))]
  expr=paste(cs,"=as.character(",cs,")")
  expr=paste(expr,collapse = ",")
  expr=paste("':='(",expr,")")
  input_var[,eval(parse(text=expr))]

  # calc initial decomp
  input_data_melt=melt.data.table(input_data,id.var=c(date,cs),value.name="raw",variable.name="var")
  input_data_melt=merge(input_data_melt,input_var[,!c("model_var","model_name_group"),with=F],by=c(cs,"var"),all.x=T)
  input_data_melt[is.na(para),para:=0]
  input_data_melt[var %in% c(y,y_m,paste(y,"_actual",sep="")),para:=1]
  input_data_melt[,decomp:=raw*para]
  expr=as.formula(paste(paste(c(cs,date),collapse = "+"),"~var"))
  input_data=dcast.data.table(input_data_melt,expr,value.var="decomp")
  rm(input_data_melt)

  # sort data
  expr=paste(c(cs,date),collapse = ",")
  expr=paste("order(",expr,")")
  input_data=input_data[eval(parse(text=expr))]

  # gross base
  expr=paste(var.list,collapse = "+")
  expr=parse(text=paste("gross_base := exp(",y,"-(",expr,"))",sep=""))
  input_data[,eval(expr)]

  # smoothed base
  sm_exp=function(x,alpha){
    # x=rnorm(1000)
    # alpha=0.5
    result=rep(NA,length(x))
    for(i in seq_along(x)){
      if (i==1) result[i]=x[i] else result[i]=result[i-1]*alpha+x[i]*(1-alpha)
    }
    result
  }

  f_sm_base=function(x,data,alpha){
    #x=group[1];data=input_data;alpha=0.9;
    temp=merge(group[x],data[,c(cs,date,"gross_base"),with=F],by=c(cs),all.x=T)
    if (alpha==0) temp[,sm_base:=gross_base] else{
      #temp[,sm_base:=as.numeric(ses(gross_base,alpha,initial="simple",h=1)$fitted)]
      #temp[,sm_base:=sm_exp(gross_base,alpha)]
      temp[,sm_base:=lowess(gross_base,f=alpha)$y]
      temp$sm_base[1]=temp$gross_base[1]
    }
    return(temp[,!"gross_base",with=F])
  }
  group=unique(input_data[,cs,with=F],by=NULL)
  sm_base_list=lapply(1:nrow(group),f_sm_base,alpha=sm_factor,data=input_data)
  input_data=merge(input_data,rbindlist(sm_base_list),by=c(cs,date),all.x=T)

  # net base
  expr=paste(var.list,collapse = "+")
  temp=input_data[,var.list,with=F]
  temp[temp>=0]=0
  expr=parse(text=paste("sum_neg :=",expr,sep=""))
  input_data$sum_neg=temp[,eval(expr)]$sum_neg
  expr=paste("net_base:=pmin(exp(",y,"_actual),sm_base*exp(sum_neg))",sep="")
  input_data[,eval(parse(text=expr))]

  # variable decomp
  expr=paste(paste(var.list,"= exp(",var.list,")-1",sep=""),collapse = ",")
  expr=parse(text=paste("':='(",expr,")",sep=""))
  input_data[,eval(expr)]

  # calc pos and neg sum
  expr=paste(var.list,collapse = "+")
  temp=input_data[,var.list,with=F]
  temp[temp<0]=0
  expr=parse(text=paste("sum_pos :=",expr,sep=""))
  input_data$sum_pos=temp[,eval(expr)]$sum_pos

  expr=paste(var.list,collapse = "+")
  temp=input_data[,var.list,with=F]
  temp[temp>=0]=0
  expr=parse(text=paste("sum_neg :=",expr,sep=""))
  input_data$sum_neg=temp[,eval(expr)]$sum_neg

  # adjust variable decomp
  expr=paste(var.list,"=ifelse(",var.list,">0,",var.list,"/sum_pos*(",y,"_actual-net_base),",var.list,"/(-sum_neg)*abs(sm_base-net_base))",sep="")
  expr=paste(expr,collapse = ",")
  expr=parse(text=paste("':='(",expr,")",sep=""))
  input_data[,eval(expr)]
  input_data[is.na(input_data)]=0

  # check sum
  expr=paste(var.list,collapse = "+")
  expr=parse(text=paste("check:=",y,"_actual-sm_base-(",expr,")",sep=""))
  input_data[,eval(expr)]
  checksum=T
  if (any(abs(input_data$check)>0.1)) {
    checksum=F
    print("Warning: The decomp result doesn't sum up to actual. Please check the output data.")
  }

  # return
  if(checksum){

    decomp=input_data[,c(cs,date,paste(y,"_actual",sep=""),var.list,"sm_base"),with=F]
    # setnames(decomp,c(paste(y,"_actual",sep=""),"sm_base"),c(y,"Base"))
    m_n=paste(y,"_dep",sep="")
    setnames(decomp,c(paste(y,"_actual",sep=""),"sm_base"),c(m_n,"Base"))

    # prepare table for shiny
    decomp.list=f_decomp_split(decomp,cs,y=m_n,date,input_var)

    # plot
    app=rshiny(decomp.list$con,decomp.list$decomp.chart,date,m_n)

    if (is.output){
      write.csv(decomp.list$decomp.export,"output_decomp.csv",row.names = F)
      #write.csv(decomp.list$con,"output_contribution.csv",row.names = F)
    }
    return(list(decomp_detail=decomp,decomp=decomp.list$decomp.export,contribution=decomp.list$con,app=app))
  }else{
    return(decomp=input_data)
  }
}




