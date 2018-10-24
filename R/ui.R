# Construct a Shiny Dashboard for the ARTMo Output
# Try it


ui <- dashboardPage(
  
  # Structure -------------------------------
  title= "ARTMO Shiny",
  skin = "green",
  dashboardHeader(title="ARTMO Shiny"),
  dashboardSidebar(
    disable=F,
    sidebarMenu(
      menuItem( "Connection" ,tabName="Connect",icon=icon("dashboard")),
      menuItem( "Environment",tabName="Envi",icon=icon("th"))
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      # MySQL Input -------------------------------
      tabItem(tabName = "Connect",
              
              fluidRow(
                
                box(title="MYSQL Input",width = 4,heigth="400",collapsible = T,status="primary",col="grey",
                    
                    
                    uiOutput("MySQL.cred.def"),
                    uiOutput("MySQL.cred.user"),
                    uiOutput("MySQL.cred.pw"),
                    uiOutput("MySQL.cred.host"),
                    uiOutput("MySQL.connect"),
                    br(),
                    uiOutput("MySQL.databases")
                    
                ),
                box(title="Available MySQL Tables", width = 8,heigth="400",collapsible = T,
                    
                    tableOutput("MySQL.str"),
                    style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
                    
                )
              ),
              fluidRow(
                
                box(title="Master File: List of Projects, Models and Simulations",width=12,collapsible = T,
                    dataTableOutput("MySQL.master"),
                    style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
                )
              )
      ),
      
      # Build Workspace -------------------------------
      tabItem(tabName = "Envi",
              
              fluidRow(
                
                box(title="Setup the Local Environment",
                    
                    uiOutput("local.path"),
                    uiOutput("local.build.bt")
                    
                )
              )
      ) 
    )
  )
)



