#' @export
# split pos and neg, and group variables
f_decomp_split=function(decomp,cs,y,date,input_var){
  # group variable
  f_group=function(x,decomp){
    #x=unique(input_var[["var_group"]])[1]
    temp=unique(input_var[,c("var","var_group"),with=F],by=NULL)
    dim=temp$var[temp[["var_group"]]==x]
    expr=paste(x,"=",paste(dim,collapse = "+"))
    expr=parse(text=paste("list(",expr,")"))
    decomp[,eval(expr)]
  }
  var.name=unique(input_var[["var_group"]])
  temp=lapply(var.name,f_group,decomp=decomp)
  temp=Reduce(cbind,temp)
  temp=cbind(decomp[,c(cs,date,y,"Base"),with=F],temp)
  export=copy(temp)
  # group cross section
  expr=paste(cs,collapse = ",")
  expr=paste("cross_section:=paste(",expr,",sep=' + ')",sep="")
  temp[,eval(parse(text=expr))]
  temp.date=temp[,lapply(.SD,sum),by=c(date),.SDcols=c(y,"Base",var.name)]
  temp.date$cross_section="All"
  temp=rbind(temp.date,temp[,!cs,with=F])
  temp.con=copy(temp)

  # calc con
  temp.con=temp.con[,!date,with=F][,lapply(.SD,sum),by=c("cross_section")]
  temp.con=melt.data.table(temp.con,id.vars=c("cross_section",y))
  temp.con[,con:=value/eval(parse(text=y))]
  temp.con=temp.con[,!y,with=F]
  temp.con=temp.con[order(cross_section,variable)]
  setnames(temp.con,c("value","con"),c("Decomp","Contribution"))

  # split values
  var.name.new=paste(var.name,"_old",sep="")
  setnames(temp,var.name,var.name.new)
  expr=paste(var.name,"=ifelse(",var.name.new,"<0,0,",var.name.new,")",sep="")
  expr=paste("':='(",paste(expr,collapse = ","),")")
  temp[,eval(parse(text=expr))]

  expr=paste(var.name,"_neg=ifelse(",var.name.new,">=0,0,",var.name.new,")",sep="")
  expr=paste("':='(",paste(expr,collapse = ","),")")
  temp[,eval(parse(text=expr))]
  for.areachart=temp[,!var.name.new,with=F]

  var_name_all = c(var.name,paste(var.name,rep("_neg",length(var.name)),sep=""))
  for.areachart=for.areachart[cross_section!="All"]
  for.areachart.date=for.areachart[,lapply(.SD,sum),by=c(date),.SDcols=c(y,"Base",var_name_all)]
  for.areachart.date$cross_section="All"
  for.areachart=rbind(for.areachart.date,for.areachart)



  # delete 0 columns
  f_ifzero=function(x,dt) {
    if(any(dt[[x]]!=0)) {
      return(F)
    } else {
      return(T)
    }
  }

  var_to_drop=c(var_name_all[sapply(var_name_all,f_ifzero,for.areachart)])
  if(length(var_to_drop)!=0){
    for.areachart[,c(var_to_drop):=NULL]
  }
  # for.export=for.areachart[cross_section!="All"]
  return(list(decomp.export=export,decomp.chart=for.areachart,con=temp.con))
}

#' @export
get_pec=function(decomp_list,model_name,dep_var) {
  working=decomp_list[[model_name]][,c(colnames(decomp_list[[model_name]])[!colnames(decomp_list[[model_name]]) %in% c(paste(dep_var,"_dep",sep=""))]),with=F]
  sub_var=c(colnames(working)[!colnames(working) %in% c(cs_var,date_var)])
  working_var=working[,sub_var,with=F]
  working_var=as.data.table(prop.table(as.matrix(working_var),1))
  working[,c(colnames(working_var)):=NULL]
  working=cbind(working,working_var)
  final=melt.data.table(working,id.vars=c(cs_var,date_var))
  final[,model:=model_name]
  return(final)
}

#' @export
area.hchart=function(x,data,date,y){
  # x="All"
  # data=decomp.list$decomp.split

  temp=data[cross_section==x]
  temp[[date]] <- paste("#!", as.numeric(temp[[date]])*86400000, "!#")
  a <- Highcharts$new()
  a$chart(zoomType='x',type="area")
  # a$title(text='Stacked Area Chart')
  a$xAxis(type='datetime',labels=list(format= '{value:%m/%d/%Y}',rotation=45,align='left'),tickmarkPlacement='on')
  a$yAxis(title=list(text=y))
  a$plotOptions(area=list(stacking='normal',lineColor='#666666',lineWidth=1))
  #a$series(data=toJSONArray2(temp[,c(date,y),with=F], json = F, names = F),name=y,type="line",color='#058DC7')
  var=names(temp) %in% c(date,y,"cross_section","Base")
  var=names(temp)[!var]
  var.neg=grep("_neg",var,value = T)
  var.pos=var[!var %in% var.neg]
  for(i in var.pos){
    a$series(data=toJSONArray2(temp[,c(date,i),with=F], json = F, names = F),name=i)
  }
  a$series(data=toJSONArray2(temp[,c(date,"Base"),with=F], json = F, names = F),name="Base")
  for(i in var.neg){
    a$series(data=toJSONArray2(temp[,c(date,i),with=F], json = F, names = F),name=i)
  }
  a$tooltip(pointFormat= '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:,.0f}</b> <br/>',
            shared= T,xDateFormat='%m/%d/%Y'
  )
  a$exporting(enabled=T)
  a$legend(align="center",verticalAlign= "top")
  a
}

#' @export
f_direct_pass=function(higher_model,lower_model,input_var=var,cs=cs_var) {
  lower_model=trimws(strsplit(lower_model,",")[[1]])
  higher_var=input_var[model_name_group %in% c(higher_model)]
  if(nrow(higher_var)==0) print(paste("Error: Cannot find the model ",higher_model,sep=""))
  lower_var=input_var[model_name_group %in% c(lower_model)]
  if(nrow(lower_var)==0) print(paste("Error: Cannot find the model ",lower_model,sep=""))
  if(length(unique(lower_var$model_name_group))!=length(lower_model)) {
    print (paste("Cannot find submodel ",
                 paste(lower_model[!lower_model %in% unique(lower_var$model_name_group)],collapse = ","),
                 " in your input_var file when passing through layer ",i,sep=""))
    stop()
  }
  higher_keep=higher_var[!var_group %in% c(lower_model)]
  higher_pass=higher_var[var_group %in% c(lower_model)]
  if(nrow(higher_pass)==0){
    print(paste("Error: Cannot find the variable ",lower_model," in the model ",higher_model,
                ", Make sure you have ",lower_model," in column E of input_var",sep=""))
  } 
  if(length(unique(higher_pass$var_group))!=length(lower_model)) {
    print(paste("Error: Cannot find the var_group ",
                paste(lower_model[!lower_model %in% unique(higher_pass$var_group)],collapse = ","),
                " in the main-model ", higher_model,sep=""))
    stop()
  }

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
rshiny=function(temp.con,forplot,date,y){
  # temp.con=decomp.list$con
  # forplot=decomp.list$decomp.split

  server <- function(input, output) {
    output$area=renderChart2(area.hchart(x=input$group,data=forplot,date,y))
    # options(DT.options=list(pageLength=10,autoWidth=T,searching = F))
    # output$con=renderDataTable(datatable(temp.con[cross_section==input$group][order(-Decomp)],
    #                                      rownames=F)%>%
    #                              formatCurrency("Decomp",currency="")%>%formatPercentage(c("Contribution","Decomp"),2))
  }

  ui <- fluidPage(
    selectizeInput("group", multiple = F,label=NULL,choices=as.list(unique(forplot[["cross_section"]])),selected = "All"),
    fluidRow(
      # column(8,showOutput("avp","highcharts")),
      # column(4,offset = 0,dataTableOutput("con",width="100%"))
      showOutput("area","highcharts")

      # br(),
      # dataTableOutput("con",width="50%")
    )
  )

  shinyApp(ui = ui, server = server)
}



#' @export
rshiny2=function(temp.con,forplot,date,y){
  server <- function(input, output) {
    output$area=renderChart2(area.hchart(x=input$group,data=forplot,date,y))
    options(DT.options=list(pageLength=10,autoWidth=T,searching = F))
    output$table = renderDataTable(
      datatable(temp.con[temp.con$cross_section == input$group][order(-Decomp)]) %>%
        formatCurrency('Decomp','',digits=1) %>%
        formatPercentage('Contribution',2)
    )
  }

  ui <- fluidPage(
    selectizeInput("group", multiple = F,label=NULL,choices=as.list(unique(forplot[["cross_section"]])),selected = "All"),
    showOutput("area","highcharts"),
    fluidRow(
      column(5,
             DT::dataTableOutput("table",width = "50%"))
              )
    )
  shinyApp(ui = ui, server = server)
}




