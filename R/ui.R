
ui <- dashboardPage(
  
  # Structure -------------------------------
  title= "ARTMO Shiny",
  skin = "green",
  dashboardHeader(title="ARTMO Shiny"),
  dashboardSidebar(
    disable=F,
    sidebarMenu(
      menuItem( "Connection" ,tabName="Connect",icon=icon("plug")),
      menuItem( "Explore",tabName="Projects",icon=icon("eye"))
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      # Connection -------------------------------
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
                
                box(title="Setup the Local Machine",width = 4,collapsible = T, 
                    
                    uiOutput("local.path"),
                    uiOutput("local.build.bt")
                    
                ),
                
                box(title="MySQL Master File: Overview",width=8,collapsible = T,
                    dataTableOutput("MySQL.master"),
                    style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
                )
              )
      ),
      
      # Explore -------------------------------
      tabItem(tabName = "Projects",
              
              fluidRow(
                
                box(title="Explore a Project",width = 4,heigth="400",collapsible = T,
                    
                    uiOutput("myproject"),
                    uiOutput("mysimulation")
                
                ),
                
                box(title="Simulation Metrics",width = 4,heigth="400",collapsible = T,
                    
                    dataTableOutput("mysimulation.table")
                    
                )
              )
      ) 
    )
  )
)



