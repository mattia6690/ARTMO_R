# Construct a Shiny Dashboard for the ARTMo Output
# Try it


ui <- dashboardPage(
  title= "ARTMO Shiny",
  skin = "green",
  dashboardHeader(title="ARTMO Shiny"),
  dashboardSidebar(
    disable=F,
    sidebarMenu(
      menuItem(
        "MySQL Connection",tabName="Connect",icon=icon("dashboard")
      )
    )
    
  ),
  
  dashboardBody(
    
    tabItem(tabName = "MySQL Connection",
            
            fluidRow(
              
              box(title="MYSQL Input",width = 4,heigth=5,collapsible = T,status="primary",
                  
                  
                  uiOutput("MySQL.cred.def"),
                  uiOutput("MySQL.cred.user"),
                  uiOutput("MySQL.cred.pw"),
                  uiOutput("MySQL.cred.host"),
                  uiOutput("MySQL.connect"),
                  br(),
                  uiOutput("MySQL.databases")
                  
              ),
              box(width = 8,heigth=2,collapsible = T,
                  div(style = 'overflow-y: scroll', DT::dataTableOutput('view_data')),
                  
                  tableOutput("MySQL.str")
                  
              )
            ),
            fluidRow(
              
              dataTableOutput("MySQL.master")
              
              
            )
            
    )
    
    
  
)
)



