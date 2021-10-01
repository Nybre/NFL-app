Dashboard_file_UI <- function(id) {
  ns <- NS(id)
  tagList( 
    #fixtures ui box
    fluidRow(
      value_box("Fixtures",htmlOutput(ns("value_1")),color = "red",
                width = 16, size = "small") 
    ), 
    #NFL team location ui box
    fluidRow( 
      box(title = "NFL team location", color = "purple", width = 12, 
          leafletOutput(ns("team_stadium"))
          
      ),
      #control panel ui box
      box(title = "Control Panel", color = "blue", width =4,
          collapsible = T,  
          file_input(ns("fileDrive"), "",placeholder = "Upload a File ..."
                     ,type = "small",width = "150px"),  
          actionButton(ns("removeBtn"), "Delete file", icon("red trash")),
          br(),
          tags$div(tags$div(HTML("<b>Select Season:</b> ")), 
                   selectInput(ns("season_choices"),"",choices = c("2019","2020"))
          ),
          br(), 
          tags$div(tags$div(HTML("<b>Select Team:</b> ")),
                   selectInput(ns("team_choices"),"",choices = c(unique(pace_data$posteam)))
          )
      ) 
    ),
    #pace vs teams average ui box
    fluidRow( 
      box(title = "Pace vs Average/Week plot", color = "blue", width = 8, 
          plotOutput(ns("analysis1"))
          
      ),
      #pace vs team average table ui bix
      box(title = "Pace vs Average/Week table", color = "orange", width = 8, 
          DT::dataTableOutput(ns("table_analysis"))
          
      )
    )
  )
}

Dashboard_file <- function(input, output, session, pool) { 
  #selection choice inputs
  season_choices<-reactive({input$season_choices}) 
  team_choices<-reactive({input$team_choices})
  
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
  #run algorithm if there exist files in the working directory
  #this filter prevents application running without core required data
  #files includes location, pace_data and logos  
  if(sapply(paste(getwd(),"data_folder",sep = "/"),
            function(dir){length(list.files(dir,pattern='xls'))})>=1){
    #source data
    Location <- read_excel(paste(getwd(),"data_folder/NFL_Teams.xls", sep = "/"))
    pace_data = read.csv(paste(getwd(),"data_folder/pace_data.csv", sep = "/"))
    logos = read.csv(paste(getwd(),"data_folder/logos.csv", sep = "/"))
    
    
    output$value_1 <- renderText({ 
      strngg = " Arizona Cardinals vs Seattle Seahawks | Denver Broncos vs Kansas City Chiefs | Los Angeles Rams vs San Francisco 49ers | 
Las Vegas Raiders vs Los Angeles Chargers | Cleveland Browns vs Cincinnati Bengals | Buffalo Bills vs New York Jets | Baltimore Ravens vs Pittsburgh Steelers
Houston Texans vs Tennessee Titans |"
      formatedFont_1 <- sprintf(paste0('<font color="green" size = "10px" ><marquee behavior="scroll" direction="left" scrollamount="4">',strngg,'</marquee>'))
      return(formatedFont_1)
    })
    
    output$team_stadium <- renderLeaflet({
      
      Team_icons <- icons(
        iconUrl = ifelse(Location$Team=="Steelers", "https://upload.wikimedia.org/wikipedia/commons/thumb/d/de/Pittsburgh_Steelers_logo.svg/100px-Pittsburgh_Steelers_logo.svg.png",
                         ifelse(Location$Team=="Browns","https://upload.wikimedia.org/wikipedia/en/thumb/d/d9/Cleveland_Browns_logo.svg/100px-Cleveland_Browns_logo.svg.png",
                                ifelse(Location$Team=="Bengals","https://upload.wikimedia.org/wikipedia/commons/thumb/8/81/Cincinnati_Bengals_logo.svg/100px-Cincinnati_Bengals_logo.svg.png",
                                       ifelse(Location$Team=="Ravens","https://upload.wikimedia.org/wikipedia/en/thumb/1/16/Baltimore_Ravens_logo.svg/193px-Baltimore_Ravens_logo.svg.png",
                                              ifelse(Location$Team=="Bears","https://upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Chicago_Bears_logo.svg/100px-Chicago_Bears_logo.svg.png",
                                                     ifelse(Location$Team=="Texans","https://upload.wikimedia.org/wikipedia/en/thumb/2/28/Houston_Texans_logo.svg/100px-Houston_Texans_logo.svg.png",
                                                            ifelse(Location$Team=="Chiefs","https://upload.wikimedia.org/wikipedia/en/thumb/e/e1/Kansas_City_Chiefs_logo.svg/100px-Kansas_City_Chiefs_logo.svg.png",
                                                                   ifelse(Location$Team=="Saints","https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/New_Orleans_Saints_logo.svg/98px-New_Orleans_Saints_logo.svg.png",
                                                                          ifelse(Location$Team=="Dolphins","https://upload.wikimedia.org/wikipedia/en/thumb/3/37/Miami_Dolphins_logo.svg/100px-Miami_Dolphins_logo.svg.png",
                                                                                 ifelse(Location$Team=="Buccaneers","https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Tampa_Bay_Buccaneers_logo.svg/100px-Tampa_Bay_Buccaneers_logo.svg.png",
                                                                                        ifelse(Location$Team=="Falcons","https://upload.wikimedia.org/wikipedia/en/thumb/c/c5/Atlanta_Falcons_logo.svg/192px-Atlanta_Falcons_logo.svg.png",
                                                                                               ifelse(Location$Team=="Bills","https://upload.wikimedia.org/wikipedia/en/thumb/7/77/Buffalo_Bills_logo.svg/189px-Buffalo_Bills_logo.svg.png",
                                                                                                      ifelse(Location$Team=="Forty-Niners","https://upload.wikimedia.org/wikipedia/commons/thumb/3/3a/San_Francisco_49ers_logo.svg/100px-San_Francisco_49ers_logo.svg.png",
                                                                                                             ifelse(Location$Team=="Rams","https://upload.wikimedia.org/wikipedia/en/thumb/8/8a/Los_Angeles_Rams_logo.svg/100px-Los_Angeles_Rams_logo.svg.png",
                                                                                                                    ifelse(Location$Team=="Jaguars","https://upload.wikimedia.org/wikipedia/en/thumb/7/74/Jacksonville_Jaguars_logo.svg/100px-Jacksonville_Jaguars_logo.svg.png",
                                                                                                                           ifelse(Location$Team=="Eagles","https://upload.wikimedia.org/wikipedia/en/thumb/8/8e/Philadelphia_Eagles_logo.svg/100px-Philadelphia_Eagles_logo.svg.png",
                                                                                                                                  ifelse(Location$Team=="Seahawks","https://upload.wikimedia.org/wikipedia/en/thumb/8/8e/Seattle_Seahawks_logo.svg/100px-Seattle_Seahawks_logo.svg.png",
                                                                                                                                         ifelse(Location$Team=="Raiders","https://upload.wikimedia.org/wikipedia/en/thumb/4/48/Las_Vegas_Raiders_logo.svg/100px-Las_Vegas_Raiders_logo.svg.png",
                                                                                                                                                ifelse(Location$Team=="Cowboys","https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Dallas_Cowboys.svg/100px-Dallas_Cowboys.svg.png",
                                                                                                                                                       ifelse(Location$Team=="Panthers","https://upload.wikimedia.org/wikipedia/en/thumb/1/1c/Carolina_Panthers_logo.svg/100px-Carolina_Panthers_logo.svg.png",
                                                                                                                                                              ifelse(Location$Team=="Patriots","https://upload.wikimedia.org/wikipedia/en/thumb/b/b9/New_England_Patriots_logo.svg/100px-New_England_Patriots_logo.svg.png",
                                                                                                                                                                     ifelse(Location$Team=="Colts","https://upload.wikimedia.org/wikipedia/commons/thumb/0/00/Indianapolis_Colts_logo.svg/100px-Indianapolis_Colts_logo.svg.png",
                                                                                                                                                                            ifelse(Location$Team=="Vikings","https://upload.wikimedia.org/wikipedia/en/thumb/4/48/Minnesota_Vikings_logo.svg/98px-Minnesota_Vikings_logo.svg.png",
                                                                                                                                                                                   ifelse(Location$Team=="Redskins","https://upload.wikimedia.org/wikipedia/commons/thumb/7/72/Washington_football_team_wlogo.svg/100px-Washington_football_team_wlogo.svg.png",
                                                                                                                                                                                          ifelse(Location$Team=="Titans","https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/titans.png",
                                                                                                                                                                                                 ifelse(Location$Team=="Chargers","https://upload.wikimedia.org/wikipedia/en/thumb/7/72/NFL_Chargers_logo.svg/100px-NFL_Chargers_logo.svg.png",
                                                                                                                                                                                                        ifelse(Location$Team=="Broncos","https://upload.wikimedia.org/wikipedia/en/thumb/4/44/Denver_Broncos_logo.svg/100px-Denver_Broncos_logo.svg.png",
                                                                                                                                                                                                               ifelse(Location$Team=="Lions","https://upload.wikimedia.org/wikipedia/en/thumb/7/71/Detroit_Lions_logo.svg/100px-Detroit_Lions_logo.svg.png",
                                                                                                                                                                                                                      ifelse(Location$Team=="Packers","https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Green_Bay_Packers_logo.svg/100px-Green_Bay_Packers_logo.svg.png",
                                                                                                                                                                                                                             ifelse(Location$Team=="Cardinals","chttps://upload.wikimedia.org/wikipedia/en/thumb/7/72/Arizona_Cardinals_logo.svg/179px-Arizona_Cardinals_logo.svg.png",
                                                                                                                                                                                                                                    ifelse(Location$Team=="Giants", "https://upload.wikimedia.org/wikipedia/commons/thumb/6/60/New_York_Giants_logo.svg/100px-New_York_Giants_logo.svg.png",
                                                                                                                                                                                                                                           ifelse(Location$Team=="Jets","https://upload.wikimedia.org/wikipedia/en/thumb/6/6b/New_York_Jets_logo.svg/100px-New_York_Jets_logo.svg.png
", "")))))))))))))))))))))))))))))))),  
        iconWidth = 28, iconHeight = 35,
        iconAnchorX = 18, iconAnchorY = 34,
        shadowWidth = 20, shadowHeight = 34,
        shadowAnchorX = 4, shadowAnchorY = 32)
      
      
      Team <- as.character(Location$Team)
      df <- data.frame(lat=as.numeric(Location$latitude), lng = as.numeric(Location$longitude))
      
      df %>%
        leaflet() %>% 
        addTiles() %>% 
        addMarkers(icon=Team_icons, popup=Team,
                   clusterOptions = markerClusterOptions())
      
    })
    output$analysis1<-renderPlot({
      
      gg1 <- pace_data %>% filter(season == as.numeric(season_choices())) %>% 
        group_by(posteam, week, defteam) %>% 
        summarize(pva = mean(time_between_plays - expected_tbp)) %>% 
        filter(posteam == team_choices())  %>%
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
    
    output$table_analysis <- renderDT({
      data_table <- pace_data %>% filter(!is.na(expected_tbp), season == as.numeric(season_choices())) %>% 
        group_by(posteam) %>% summarize(pva = round(mean(time_between_plays - expected_tbp),2), 
                                        time_between_plays = round(mean(time_between_plays),2), expected_tbp = round(mean(expected_tbp),2)) %>%
        ungroup() %>% group_by(posteam) %>% mutate(extra_plays_per_30min = round(mean((1800/time_between_plays)-(1800/expected_tbp)),2))
      colnames(data_table) = c("Team","PVA","Time Between Plays","Expected TBP","Extra plays per 30 min")
      datatable(data_table, options = list( 
        pageLength = 8,
        scrollY=T) 
      )  
    })
  }else{
    #do nothing if theres no data
  }
} 