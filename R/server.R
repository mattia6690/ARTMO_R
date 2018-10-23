server <- function(input, output, session) {
  
  output$MySQL.cred.def<- renderUI({
    
    checkboxInput("sql.def",label="Default MySQL Credentials")
    
  })
  
  output$MySQL.cred.user<- renderUI({
    
    if(!isTRUE(input$sql.def)){
      textInput("sql.user",label="MySQL User")
    } else { textInput("sql.user",label="MySQL User",value="root")}
    
  })
  
  output$MySQL.cred.pw<- renderUI({
    
    if(!isTRUE(input$sql.def)){
      passwordInput("sql.pw",label="MySQL Password")
    } else {passwordInput("sql.pw",label="MySQL Password",value="123456")}
    
  })
  
  output$MySQL.cred.host<- renderUI({
    
    if(!isTRUE(input$sql.def)){
      textInput("sql.host",label="MySQL Host")
    } else { textInput("sql.host",label="MySQL User",value="localhost")}
    
  })
  
  output$MySQL.connect<-renderUI({
    
    actionButton("sql.con","Connect")
    
  })
  
  
  con <- eventReactive(input$sql.con,{
    
    connect.raw<-artmo_con(user=input$sql.user,
                           password=input$sql.pw,
                           host=input$sql.host)
    
    })
  
  output$MySQL.databases<-renderUI({
    
    gq<-dbGetQuery(con(),"show databases") 
    selectInput("sql.db","SQL Databases",choices=gq)
    
  })
  
  database <- eventReactive(input$sql.db,{
    
    connect.raw<-artmo_con(user=input$sql.user,
                           password=input$sql.pw,
                           database=input$sql.db,
                           host=input$sql.host)
    
  })
  
  output$MySQL.str<- renderTable({
    
    dbGetQuery(database(),"show table status") %>% 
      filter(Rows>0) %>%  
      mutate(Rows=as.numeric(Rows),Avg_row_length=as.numeric(Avg_row_length),Data_length=as.numeric(Data_length)) %>% 
      select(Name,Rows,Avg_row_length,Data_length)
    
  })
  
  output$MySQL.master<- renderDataTable({
    
    master<-artmo_getmaster(database()) %>% 
      select(ID_MASTER,ID_PY,MODELS=NAME_MODEL,PROJECT=NAME_PROYECT,DATE1,TIME_MODEL,NAMESENSOR,SIM=SIMULACIONES)
    
  })
  
}