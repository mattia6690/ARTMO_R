server <- function(input, output, session) {
  
  
  # MySQL Functions ---------------------------
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
    
    connect.raw(user=input$sql.user,password=input$sql.pw,host=input$sql.host)
    
  })
  
  output$MySQL.databases<-renderUI({
    
    gq<-is.artmodb(con(),input$sql.user,input$sql.pw,input$sql.host)
    selectInput("sql.db","Select the ARTMO Database",choices=gq)
    
  })
  
  database <- eventReactive(input$sql.db,{
    
    connect.db(user=input$sql.user,password=input$sql.pw, database=input$sql.db,host=input$sql.host)
    
  })
  
  output$MySQL.str<- renderTable({
    
    dbGetQuery(database(),"show table status") %>% 
      filter(Rows>0) %>%  
      mutate(Rows=as.numeric(Rows),Avg_row_length=as.numeric(Avg_row_length),Data_length=as.numeric(Data_length)) %>% 
      select(Name,Rows,Avg_row_length,Data_length)
    
  })
  
  output$MySQL.master<- renderDataTable({
    
    master<-get.master.artmo(database()) %>% 
      select(ID_MASTER,ID_PY,ID_SIMULATION,
             MODELS=NAME_MODEL,
             PROJECT=NAME_PROYECT,
             DATE1,
             TIME_MODEL,
             NAMESENSOR,
             SIM=SIMULACIONES)
    
  })
  
  # Local Functions ---------------------------
  output$local.build.bt<-renderUI({
    
    actionButton("local.build","Construct Folder Infrastucture")
    
  })

  output$local.path<-renderUI({
    
    textInput(inputId="local.path",label="Digit Path",value="C:/ARTMO/")
    
  })
  
  observeEvent(input$local.build,{
    
    db<-database()
    withProgress(message="Constructing Local Environment",value=0,{
      set.directory.artmo(db,input$local.path)
      set.projects.artmo(db,input$local.path)
    })
    
  })
  
  # Project Functions ---------------------------
  
  output$myproject<-renderUI({
    
    ch<-list.dirs(paste0(input$local.path,"/",input$sql.db,"/"),full.names = F,recursive = F)
    selectInput("myproj","Select the Project",choices=ch)
    
  })
  
  output$mysimulation<-renderUI({
    
    ch<-list.dirs(paste0(input$local.path,"/",input$sql.db,"/",input$myproj,"/"),full.names = F,recursive = F)
    selectInput("mysim","Select the Simulation",choices=ch)
    
  })
  
  output$mysimulation.table<-renderDataTable({
    
    path<-paste0(input$local.path,"/",input$sql.db,"/",input$myproj,"/",input$mysim,"/")
    get.simtable(path)
    
  })
  
}


