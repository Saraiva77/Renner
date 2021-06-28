#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(dplyr)
library(magrittr)
library(DT)
library(leaflet)
library(rgdal)
library(lubridate)
library(shinymanager)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


# data.frame with credentials info
credentials <- data.frame(
    user = "admin",
    password = "admin06",
    # comment = c("alsace", "auvergne", "bretagne"), %>% 
    stringsAsFactors = FALSE
)


met_cd <- read.csv2("METRICAS_CD.csv",sep=";",header=T)

met_cd$div_concat <- paste(met_cd$div_num," - ",met_cd$div_name)
met_cd$grp_concat <- paste(met_cd$grp_num," - ",met_cd$grp_name)
met_cd$dept_concat <- paste(met_cd$dept_num," - ",met_cd$dept_name)
met_cd$class_concat <- paste(met_cd$class_num," - ",met_cd$class_name)
met_cd$subclass_concat <- paste(met_cd$subclass_num," - ",met_cd$subclass_name)

met_loja <- read.csv2("METRICAS_LOJA.csv",sep=";",header=T)
met_loja$div_concat <- paste(met_loja$div_num," - ",met_loja$div_name)
met_loja$grp_concat <- paste(met_loja$grp_num," - ",met_loja$grp_name)
met_loja$dept_concat <- paste(met_loja$dept_num," - ",met_loja$dept_name)
met_loja$class_concat <- paste(met_loja$class_num," - ",met_loja$class_name)
met_loja$subclass_concat <- paste(met_loja$subclass_num," - ",met_loja$subclass_name)





ui <-secure_app(head_auth = tags$script(inactivity),
                
    dashboardPage(
    dashboardHeader(title = "Monitor Sortimento"),
    dashboardSidebar(
        sidebarMenu(id="sideside",
           menuItem("Monitor", tabName = "dashloja", icon = icon("dashboard")),
           menuSubItem("Hierarquia Loja"),
           shiny::conditionalPanel(condition="input.sideside == 'dashloja'",
                                     selectInput('reg',"Regional:",c("",sort(unique(met_loja$mgr_name)))),
                                     uiOutput("lj")),
           menuSubItem("Hierarquia Produto"),
           shiny::conditionalPanel(condition="input.sideside == 'dashloja'",
                                   selectInput('div',"Divisão:",c("",sort(unique(met_cd$div_concat)))),
                                   uiOutput("grp"),
                                   uiOutput("cla"),
                                   uiOutput("sbcla")),
           downloadButton("downloadData", "Download")
    )),
        dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashloja",value=1,
                    fluidRow(
                        valueBoxOutput("buffer"),
                        valueBoxOutput("open_orders_total"),
                        valueBoxOutput("open_orders_semana"),
                        valueBoxOutput("open_4wk")
                    ),
                    fluidRow(
                        box(width=12,DTOutput('tabela1'))
                    )
            )
            
        )
    )
    )
)

server <- function(input, output,session) {

    
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    output$res_auth <- renderPrint({
        reactiveValuesToList(result_auth)
    })
    
    idinfo2 <- showNotification(paste("Carregando Informações...",sep=""),type= "message", duration = NULL)
    
    met_cd <- read.csv2("METRICAS_CD.csv",sep=";",header=T)
    met_cd$div_concat <- paste(met_cd$div_num," - ",met_cd$div_name)
    met_cd$grp_concat <- paste(met_cd$grp_num," - ",met_cd$grp_name)
    met_cd$dept_concat <- paste(met_cd$dept_num," - ",met_cd$dept_name)
    met_cd$class_concat <- paste(met_cd$class_num," - ",met_cd$class_name)
    met_cd$subclass_concat <- paste(met_cd$subclass_num," - ",met_cd$subclass_name)

    met_loja <- read.csv2("METRICAS_LOJA.csv",sep=";",header=T)
    met_loja$div_concat <- paste(met_loja$div_num," - ",met_loja$div_name)
    met_loja$grp_concat <- paste(met_loja$grp_num," - ",met_loja$grp_name)
    met_loja$class_concat <- paste(met_loja$class_num," - ",met_loja$class_name)
    met_loja$subclass_concat <- paste(met_loja$subclass_num," - ",met_loja$subclass_name)
    met_loja$loja_concat <- paste(formatC(met_loja$org_num,width=3, flag="0")," - ",met_loja$org_name)
    

    output$lj = renderUI({
        if (input$reg=="" | is.null(input$reg)){
            selectInput('loja',"Loja:",c("",sort(unique(met_loja$loja_concat))))  
        } else {
            selectInput('loja',"Loja:",c("",sort(unique(met_loja$loja_concat[met_loja$mgr_name==input$reg]))))
        }
    })   
       
    
 output$grp = renderUI({
     if (input$div=="" | is.null(input$div)){
         selectInput('grp',"Grupo:",c("",sort(unique(met_cd$grp_concat))))  
     } else {
     selectInput('grp',"Grupo:",c("",sort(unique(met_cd$grp_concat[met_cd$div_concat==input$div]))))
     } 
     
})


 
 output$cla = renderUI({
     
       if (is.null(input$div) & is.null(input$grp) ){
           selectInput('clas',"Classe:",c("a"),selected=NULL)  
     }
       else if ((input$div=="" & input$grp=="")){
         selectInput('clas',"Classe:",c("",sort(unique(met_cd$class_concat))),selected=NULL)  
     } else if ((input$div!="" & input$grp=="") | (input$div!="" & is.null(input$grp))) {
         selectInput('clas',"Classe:",c("",sort(unique(met_cd$class_concat[met_cd$div_concat==input$div]))),selected=NULL)
     } else if (input$grp!="") {
         selectInput('clas',"Classe:",c("",sort(unique(met_cd$class_concat[met_cd$grp_concat==input$grp]))),selected=NULL)
     } 
 })


 
 output$sbcla = renderUI({
     if ((input$div=="" & input$grp=="" & input$clas=="") | (is.null(input$div) & is.null(input$grp) & is.null(input$clas==""))){
         selectInput('sbclass',"Subclasse:",c("",sort(unique(met_cd$subclass_concat))),selected=NULL)  
     } else if (input$div!="" & input$grp=="" & input$clas=="") {
         selectInput('sbclass',"Subclasse:",c("",sort(unique(met_cd$subclass_concat[met_cd$div_concat==input$div]))),selected=NULL)
     } else if (input$grp!=""  & input$clas=="") {
         selectInput('sbclass',"Subclasse:",c("",sort(unique(met_cd$subclass_concat[met_cd$grp_concat==input$grp]))),selected=NULL)
     } else if (input$clas!="" ){
         selectInput('sbclass',"Subclasse:",c("",sort(unique(met_cd$subclass_concat[met_cd$class_concat==input$clas]))),selected=NULL)
     }
 })
 

    output$buffer <- renderValueBox({
         if ((input$div=="" & input$grp=="" & input$clas=="" & input$sbclass=="") | (is.null(input$div) & is.null(input$grp) & is.null(input$clas) & is.null(input$sbclass))){
            df_buffer <- data.frame(buffer=1)
            df_buffer$buffer[1]<-sum(met_cd$buffer)
        } else if (input$div!="" & input$grp=="" & input$clas=="" & input$sbclass=="") {
            df_buffer<-met_cd %>% 
                filter(div_concat==input$div)
        } else if (input$grp!=""  & input$clas=="" & input$sbclass=="") {
            df_buffer<-met_cd %>% 
                filter(grp_concat==input$grp)
        } else if (input$clas!="" & input$sbclass=="" ){
            df_buffer<-met_cd %>% 
                filter(class_concat==input$clas)
        } else if ( input$sbclass!=""){
            df_buffer<-met_cd %>% 
                filter(subclass_concat==input$sbclass)
        }
        valueBox(
             paste(sum(df_buffer$buffer)), "Buffer", icon = icon("list"),
            color = "green"
        )
    })
    output$open_orders_total <- renderValueBox({
        if (input$div=="" & input$grp=="" & input$clas=="" & input$sbclass==""){
            df_open_orders_total <- data.frame(open_orders_total=1)
            df_open_orders_total[1]<-sum(met_cd$open_orders_total)
        } else if (input$div!="" & input$grp=="" & input$clas=="" & input$sbclass=="") {
            df_open_orders_total<-met_cd %>% 
                filter(div_concat==input$div)
        } else if (input$grp!=""  & input$clas=="" & input$sbclass=="") {
            df_open_orders_total<-met_cd %>% 
                filter(grp_concat==input$grp)
        } else if (input$clas!="" & input$sbclass=="" ){
            df_open_orders_total<-met_cd %>% 
                filter(class_concat==input$clas)
        } else if ( input$sbclass!=""){
            df_open_orders_total<-met_cd %>% 
                filter(subclass_concat==input$sbclass)
        }
        valueBox(
            paste0( sum(df_open_orders_total$open_orders_total)), "Open Total", icon = icon("list"),
            color = "blue"
        )
    })    
    output$open_orders_semana <- renderValueBox({
        if (input$div=="" & input$grp=="" & input$clas=="" & input$sbclass==""){
            df_open_orders_week <- data.frame(open_orders_week=1)
            df_open_orders_week[1]<-sum(met_cd$open_orders_week)
        } else if (input$div!="" & input$grp=="" & input$clas=="" & input$sbclass=="") {
            df_open_orders_week<-met_cd %>% 
                filter(div_concat==input$div)
        } else if (input$grp!=""  & input$clas=="" & input$sbclass=="") {
            df_open_orders_week<-met_cd %>% 
                filter(grp_concat==input$grp)
        } else if (input$clas!="" & input$sbclass=="" ){
            df_open_orders_week<-met_cd %>% 
                filter(class_concat==input$clas)
        } else if ( input$sbclass!=""){
            df_open_orders_week<-met_cd %>% 
                filter(subclass_concat==input$sbclass)
        }
        valueBox(
            paste0(sum(df_open_orders_week$open_orders_week)), "Open Semana", icon = icon("list"),
            color = "purple"
        )
    })   
    output$open_4wk <- renderValueBox({
        if (input$div=="" & input$grp=="" & input$clas=="" & input$sbclass==""){
            df_open_4wk <- data.frame(open_4wk=1)
            df_open_4wk[1]<-sum(met_cd$open_orders_wk4)
        } else if (input$div!="" & input$grp=="" & input$clas=="" & input$sbclass=="") {
            df_open_4wk<-met_cd %>% 
                filter(div_concat==input$div)
        } else if (input$grp!=""  & input$clas=="" & input$sbclass=="") {
            df_open_4wk<-met_cd %>% 
                filter(grp_concat==input$grp)
        } else if (input$clas!="" & input$sbclass=="" ){
            df_open_4wk<-met_cd %>% 
                filter(class_concat==input$clas)
        } else if ( input$sbclass!=""){
            df_open_4wk<-met_cd %>% 
                filter(subclass_concat==input$sbclass)
        }
        valueBox(
            paste0(sum(df_open_4wk$open_4wk)), "Open 4 semanas", icon = icon("list"),
            color = "red"
        )
    })   
#    output$sales_unit <- renderValueBox({
#        if (input$div=="" & input$grp=="" & input$clas=="" & input$sbclass==""){
#            df_sales_units <- data.frame(sales_units=1)
#            df_sales_units[1]<-sum(met_cd$sales_units)
#        } else if (input$div!="" & input$grp=="" & input$clas=="" & input$sbclass=="") {
#            df_sales_units<-met_cd %>% 
#                filter(div_concat==input$div)
#        } else if (input$grp!=""  & input$clas=="" & input$sbclass=="") {
#            df_sales_units<-met_cd %>% 
#                filter(grp_concat==input$grp)
#        } else if (input$clas!="" & input$sbclass=="" ){
#            df_sales_units<-met_cd %>% 
#                filter(class_concat==input$clas)
#        } else if ( input$sbclass!=""){
#            df_sales_units<-met_cd %>% 
#                filter(subclass_concat==input$sbclass)
#        }
#        valueBox(
#            paste0(sum(df_sales_units$sales_units)), "Sales Units", icon = icon("list"),
#            color = "navy"
#        )
#    })      

    df_base<- reactive({
        if (input$div=="" & input$grp=="" & input$clas=="" & input$sbclass=="" & input$reg=="" & input$loja==""){
            df_calc <- met_loja %>%
                select(mgr_name,loja_concat,div_concat,grp_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>% 
                rename(Regional=mgr_name,Loja=loja_concat,Divisao = div_concat, Grupo = grp_concat, Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if (input$div!="" & input$grp=="" & input$clas=="" & input$sbclass=="" & input$reg=="" & input$loja=="") {
            df_calc <- met_loja %>% 
                select(mgr_name,loja_concat,div_concat,grp_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(div_concat==input$div) %>% 
                group_by(mgr_name,loja_concat,div_concat,grp_concat,class_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Regional=mgr_name,Loja=loja_concat,Divisao = div_concat, Grupo = grp_concat, Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if (input$grp!=""  & input$clas=="" & input$sbclass=="" & input$reg=="" & input$loja=="") {
            df_calc <- met_loja %>%
                select(mgr_name,loja_concat,grp_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(grp_concat==input$grp) %>% 
                group_by(mgr_name,loja_concat,grp_concat,class_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Regional=mgr_name,Loja=loja_concat, Grupo = grp_concat, Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if (input$clas!="" & input$sbclass=="" & input$reg=="" & input$loja==""){
            df_calc <- met_loja %>%
                select(mgr_name,loja_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(class_concat==input$clas) %>% 
                group_by(mgr_name,loja_concat,class_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Regional=mgr_name,Loja=loja_concat,Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if ( input$sbclass!="" & input$reg=="" & input$loja==""){
            df_calc <- met_loja %>%
                select(mgr_name,loja_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(subclass_concat==input$sbclass) %>% 
                group_by(mgr_name,loja_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Regional=mgr_name,Loja=loja_concat,Subclasse=subclass_concat)
            return(df_calc)
        }
        else if (input$div=="" & input$grp=="" & input$clas=="" & input$sbclass=="" & input$reg!="" & input$loja==""){
            df_calc <- met_loja %>%
            select(mgr_name,loja_concat,div_concat,grp_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>% 
            filter(mgr_name==input$reg) %>% 
            rename(Regional=mgr_name,Loja=loja_concat,Divisao = div_concat, Grupo = grp_concat, Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if (input$div!="" & input$grp=="" & input$clas=="" & input$sbclass=="" & input$reg!="" & input$loja=="") {
            df_calc <- met_loja %>% 
                select(mgr_name,loja_concat,div_concat,grp_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(div_concat==input$div & mgr_name==input$reg) %>% 
                group_by(mgr_name,loja_concat,div_concat,grp_concat,class_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Regional=mgr_name,Loja=loja_concat,Divisao = div_concat, Grupo = grp_concat, Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if (input$grp!=""  & input$clas=="" & input$sbclass=="" & input$reg!="" & input$loja=="") {
            df_calc <- met_loja %>%
                select(mgr_name,loja_concat,grp_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(grp_concat==input$grp & mgr_name==input$reg) %>% 
                group_by(mgr_name,loja_concat,grp_concat,class_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Regional=mgr_name,Loja=loja_concat, Grupo = grp_concat, Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if (input$clas!="" & input$sbclass=="" & input$reg!="" & input$loja==""){
            df_calc <- met_loja %>%
                select(mgr_name,loja_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(class_concat==input$clas & mgr_name==input$reg) %>% 
                group_by(mgr_name,loja_concat,class_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Regional=mgr_name,Loja=loja_concat,Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if ( input$sbclass!="" & input$reg!="" & input$loja==""){
            df_calc <- met_loja %>%
                select(mgr_name,loja_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(subclass_concat==input$sbclass & mgr_name==input$reg) %>% 
                group_by(mgr_name,loja_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Regional=mgr_name,Loja=loja_concat,Subclasse=subclass_concat)
            return(df_calc)
        }
        if (input$div=="" & input$grp=="" & input$clas=="" & input$sbclass=="" & input$loja!=""){
            df_calc <- met_loja %>%
                select(loja_concat,div_concat,grp_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>% 
                filter(loja_concat==input$loja) %>% 
                rename(Loja=loja_concat,Divisao = div_concat, Grupo = grp_concat, Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if (input$div!="" & input$grp=="" & input$clas=="" & input$sbclass==""& input$loja!="") {
            df_calc <- met_loja %>% 
                select(loja_concat,div_concat,grp_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(div_concat==input$div & loja_concat==input$loja) %>% 
                group_by(loja_concat,div_concat,grp_concat,class_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Loja=loja_concat,Divisao = div_concat, Grupo = grp_concat, Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if (input$grp!=""  & input$clas=="" & input$sbclass==""& input$loja!="") {
            df_calc <- met_loja %>%
                select(loja_concat,grp_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(grp_concat==input$grp & loja_concat==input$loja) %>% 
                group_by(loja_concat,grp_concat,class_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Loja=loja_concat, Grupo = grp_concat, Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if (input$clas!="" & input$sbclass==""& input$loja!="" ){
            df_calc <- met_loja %>%
                select(loja_concat,class_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(class_concat==input$clas & loja_concat==input$loja) %>% 
                group_by(loja_concat,class_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Loja=loja_concat,Classe= class_concat, Subclasse=subclass_concat)
            return(df_calc)
        } else if ( input$sbclass!=""& input$loja!=""){
            df_calc <- met_loja %>%
                select(loja_concat,subclass_concat,eoh_units,sales_units,in_transit_units,alloc_units) %>%
                filter(subclass_concat==input$sbclass  & loja_concat==input$loja) %>% 
                group_by(loja_concat,subclass_concat) %>% 
                summarise(eoh_units=sum(eoh_units),in_transit_units=sum(in_transit_units),alloc_units=sum(alloc_units),sales_units=sum(sales_units)) %>% 
                rename(Loja=loja_concat,Subclasse=subclass_concat)
            return(df_calc)
        }
        
        
    })
    output$tabela1 = renderDT(
        datatable(df_base(), rownames = FALSE,options = list(lengthChange = FALSE,scrollX = T,
                                            columnDefs = list(list(className = 'dt-center',targets="_all"))))
    )
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Monitor Sortimento ", formatC(mday(Sys.Date()),width=2,flag="0"),formatC(month(Sys.Date()),width=2,flag="0"), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(df_base(), file, row.names = FALSE)
        }
    )
    
    removeNotification(idinfo2) 

}

shinyApp(ui, server)
