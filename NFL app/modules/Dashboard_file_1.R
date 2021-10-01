Dashboard_file_1_UI <- function(id) {
  ns <- NS(id)
  tagList( 
  
    fluidRow( 
      box(title = "Pace vs Average/Week", color = "orange", width = 12, 
          plotOutput(ns("analysis1"))
          
      ), 
      box(title = "Control Menu", color = "blue", width =4,
          collapsible = T, 
        #main box must have a capped height 
          file_input(ns("fileDrive"), "",placeholder = "Upload a File ..."
                     ,type = "small",width = "150px"),  
          actionButton(ns("removeBtn"), "Delete file", icon("red trash")),
          br(),
          tags$div(tags$div(HTML("<b>Select Season Year:</b> ")),
                   #date_input(ns("date_from"), value =  Sys.Date() -31 , 
                        #      style = "width: 70%;")
                   text_input(ns("text1"))
                   ),
          br(), 
        tags$div(tags$div(HTML("<b>Select Team:</b> ")),
                # date_input(ns("date_from"), value =  Sys.Date() -31 , 
                #            style = "width: 70%;")
                text_input(ns("text2"))
                )
        ) 
      ) 
  )
}

Dashboard_file_1 <- function(input, output, session, pool) { 
  #date selector
  starting_date<-reactive({input$date_from}) 
  ending_date<-reactive({input$date_to})
  getwd()
  #upload file
  observe({
    if (is.null(input$fileDrive)) return()
    file.copy(input$fileDrive$datapath, 
              file.path(paste(getwd(),"data_folder", sep = "/"),
                        input$fileDrive$name), overwrite = TRUE) 
  }) 
  
  #delete files
  observeEvent(input$removeBtn,{
    ns <- session$ns
    create_modal(modal( 
      id="modal-pop-up",
      tagList(
        selectInput(ns("deletefilename"), 
                    label = "Delete a file", 
                    choices = list.files(paste(getwd(),"data_folder", sep = "/"), 
                              include.dirs = F, full.names = T, recursive = T))
        #use the server directory to source the files
      ),  
      footer = tagList(actionButton(ns("confirmDelete"), "Delete",icon("red trash")),
                       actionButton(ns("cancelAction"), "Cancel"),
      )
    ))
  })
  
  #delete confirmation action
  observeEvent(input$confirmDelete, {
    req(input$deletefilename)
    file.remove(input$deletefilename)
    removeModal() 
  })
  
  observeEvent(input$cancelAction, {
    hide_modal(id="modal-pop-up")
  })
   
  #check if the number of files are 4
  #file naming control limitation exists, count is the only control
  if(sapply(paste(getwd(),"data_folder",sep = "/"),
            function(dir){length(list.files(dir,pattern='xls'))})>=1){
  
  #source data
   ## Location <- read_excel(paste(getwd(),"data_folder/NFL_Teams.xls", sep = "/"))
    #dispatch <- read_excel(paste(getwd(),"data_folder/dispatch_template.xls", sep = "/"))
  #Quarterly_budget <- read_excel(paste(getwd(),"data_folder/budget_template.xls",sep = "/"))
    pace_data = read.csv(paste(getwd(),"data_folder/pace_data.csv", sep = "/"))
    logos = read.csv(paste(getwd(),"data_folder/logos.csv", sep = "/"))
    
    output$analysis1<-renderPlot({
  
      gg1 <- pace_data %>% filter(season == 2020) %>% group_by(posteam, week, defteam) %>% 
        summarize(pva = mean(time_between_plays - expected_tbp)) %>% 
        filter(posteam == 'ATL')  %>%
        left_join(logos, by = c('defteam' = 'team_abbr'))
      
      dd = ggplot(data=gg1, aes(x=week, y=pva))  +  geom_image(aes(image = team_logo_espn)) + geom_line() +
        stat_smooth(method = 'loess', se = F, col = 'blue', size = 1) +
        xlab("Week") + ylab("Pace Versus Average (Positive is Slower Pace)") + 
        ggtitle("Atlanta Falcons PVA (Pace Vs. Average) 2020 by Game", 
                subtitle = "Red line is league average, blue is trend. Logo is Opponent\n 
          PVA = Time Between Plays - Expected Time Between Plays") + 
        geom_hline(yintercept=0, linetype="dashed", color = "red", size = .75) + 
        scale_y_continuous(breaks = scales::pretty_breaks(10)) + 
        scale_x_continuous(breaks = scales::pretty_breaks(8))
     print(dd)
      })
  }else{
    #do nothing if theres no data
  }
} 