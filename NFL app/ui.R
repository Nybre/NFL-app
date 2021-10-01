 shinyUI(
  dashboardPage( 
     dashboard_header(show_menu_button = T  ,titleWidth = "wide",logo_align = "center" , inverted = F,
                                      logo_path = "images/nfl.png",
                                      title= "NFL app"),  
                      dashboardSidebar(side = "left",disable = T,
                                       sidebarMenu( 
                                         menuItem("Teams Location", tabName = "dash", icon = icon("",lib = "font-awesome")) 
                                       )),  
                      dashboardBody(    
                        tabItems(   
                          tabItem(tabName = "dash",   
                                  Dashboard_file_UI("Dashboard_file-module")) 
                          )
            ) 
))
