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
  return(list(decomp.export=for.areachart,decomp.chart=for.areachart,con=temp.con))
}


#' @export
area.hchart=function(x,data,date,y){
  # x="All"
  # data=decomp.list$decomp.split
  
  temp=data[cross_section==x]
  temp[[date]] <- paste("#!", as.numeric(temp[[date]])*86400000, "!#")
  a <- Highcharts$new()
  a$chart(zoomType='x',type="area")
  a$xAxis(type='datetime',labels=list(format= '{value:%m/%d/%Y}',rotation=45,align='left'))
  a$yAxis(title=list(text=y))
  a$plotOptions(area=list(stacking='normal'))
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
