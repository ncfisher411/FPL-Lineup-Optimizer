packages <- c("shiny", "readr", "dplyr", "ggsoccer", "tidyverse", "rstudioapi")

for (package in packages) {
  if (!requireNamespace(package, quietly = T)) {
    install.packages(package, dependencies = T)
  }
  library(package, character.only = T)
  cat(paste(package, "package loaded.\n"))
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ui <- navbarPage(title = "Menu",
  tabPanel("Lineup Card",
           h1("FPL Lineup viewer and optimizer"),
           uiOutput("picture"),
           br(), br(),
           
           h2("Tool Description"),
           p("This tool uses outputs from the script 'fpl_fetch_new.R', which models predicted FPL
    points by gameweek, to organize, filter, and view data. The CSV output must be
    uploaded to the tool for use. Then select your gameweek, goalkeepers, defenders, midfielders,
    and forwards to view the outputs. A future iteration of this tool will project the
    optimal lineup for a given set of player inputs. This tool draws on FPL data sources and
    external Github files (https://github.com/vaastav/Fantasy-Premier-League/tree/master) on
    historic gameweek performance, and is only updated as quickly as these files."),
           br(),
           
           br(),
           
           fileInput("file", "Choose a CSV file"),
           
           sidebarLayout(
             sidebarPanel(
               uiOutput("gw"),
               uiOutput("gk"),
               uiOutput("def"),
               uiOutput("mid"),
               uiOutput("fwd")
             ),
             
             mainPanel(fluidRow(
               column(
                 width = 12,
                 h3("Optimal Lineup Card"),
                 uiOutput("Total_score"),
                 uiOutput("subs")
               )
             ),
             
             fluidRow(
               column(
                 width = 12,
                 plotOutput("plots", width = "100%", height = "500px")
               )
             )
           
           )
  
    )
  ),
  
  tabPanel(
    "Waiver Margins Tool",
    h1("Comparison of Marginal Player Performance"),
    uiOutput("picture2"),
    p("This tool uses the data uploaded to the Lineup Optimizer, and is designed to compare average
      to above average performances for each gameweek. These are comparable through Z-scores (https://www.statisticshowto.com/probability-and-statistics/z-score/),
      where being further away from zero represents being further above or below average. This tool can
      be utilized for a number of purposes, but its inital use is to compare players for waiver wire
      acquisition. A general guide to how the tool works:"),
    p("- Filter by gameweek"),
    p("- Filter by position"),
    p("- Select players for comparison"),
    p("- 0 indicates an average predicted performance, negative values indicate a below average performance, and positive values indicate an above average performance"),
    p("- The tool can currently only compare 2 players"),
    br(),
    br(),
    
    sidebarLayout(
      sidebarPanel(
        uiOutput("gw2"),
        uiOutput("position"),
        uiOutput("player1"),
        uiOutput("player2")
      ),
      
      mainPanel(
        fluidRow(
          column(
            width = 12,
            h3("Comparison of Z-scores, overall and by position")
          )
        ),
        
        fluidRow(column(
          width = 12,
          dataTableOutput("table")
        )
      )
    )
  )
  )
)

server <- function(input, output) {
  
  upload <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  
  output$gw <- renderUI({
    selectizeInput("gameweek",
                label = "Select Gameweek",
                choices=unique(upload()$GW),
                multiple = F,
                options = list(
                  maxItems = 1
                ))
  })
  
  gk <- reactive({
    data <- subset(upload(), GW %in% input$gameweek) %>%
      filter(position=="GKP")
  })
  
  def <- reactive({
    data <- subset(upload(), GW %in% input$gameweek) %>%
      filter(position=="DEF")
  })
  
  mid <- reactive({
    data <- subset(upload(), GW %in% input$gameweek) %>%
      filter(position=="MID")
  })
  
  fwd <- reactive({
    data <- subset(upload(), GW %in% input$gameweek) %>%
      filter(position=="FWD")
  })
  
  output$gk <- renderUI({
    selectizeInput("keeper", "Select Goalkeepers",
                choices = unique(gk()$name),
                multiple = T,
                options = list(
                  maxItems = 2
                ))
  })
  
  output$def <- renderUI({
    selectizeInput("defender", "Select Defenders",
                choices = unique(def()$name),
                multiple = T,
                options = list(
                  maxItems = 5
                ))
  })
  
  output$mid <- renderUI({
    selectizeInput("midfield", "Select Midfielders",
                choices = unique(mid()$name),
                multiple = T,
                options = list(
                  maxItems = 5
                ))
  })
  
  output$fwd <- renderUI({
    selectizeInput("forward", "Select Forwards",
                choices = unique(fwd()$name),
                multiple = T,
                options = list(
                  maxItems = 3
                ))
  })
  
  selected <- reactive({
    req(gk(), input$keeper)
    gk_sub <- subset(gk(), name %in% input$keeper)
    
    req(def(), input$defender)
    def_sub <- subset(def(), name %in% input$defender)
    
    req(mid(), input$midfield)
    mid_sub <- subset(mid(), name %in% input$midfield)
    
    req(fwd(), input$forward)
    fwd_sub <- subset(fwd(), name %in% input$forward)
    
    data <- rbind(gk_sub, def_sub, mid_sub, fwd_sub)
    return(data)
  })
  
  observe({
    req(selected())
    
    data <- selected()
    
    data_split <- data %>% split(.$position)
    one <- data_split$GKP$name
    two <- data_split$DEF$name
    three <- data_split$MID$name
    four <- data_split$FWD$name
    
    gkp_comb <- combn(one, 1) %>% t %>%
      data.frame() %>%
      as.tibble()
    
    def_comb <- data.frame()
    for(i in 3:min(5, length(two))){
      temp <- combn(two, i) %>% t %>% data.frame()
      colnames(temp) <- rep("n", ncol(temp))
      def_comb <- bind_rows(def_comb, temp)
    }
    def_comb <- def_comb %>%
      as.tibble()
    
    mid_comb <- data.frame()
    for(i in 2:min(5, length(three))){
      temp <- combn(three, i) %>% t %>% data.frame()
      colnames(temp) <- rep("n", ncol(temp))
      mid_comb <- bind_rows(mid_comb, temp)
    }
    mid_comb <- mid_comb %>%
      as.tibble()
    
    fwd_comb <- data.frame()
    for(i in 1:min(3, length(four))){
      temp <- combn(four, i) %>% t %>% data.frame()
      colnames(temp) <- rep("n", ncol(temp))
      fwd_comb <- bind_rows(fwd_comb, temp)
    }
    fwd_comb <- fwd_comb %>%
      as.tibble()
    
    combo <- crossing(gkp_comb, def_comb, mid_comb, fwd_comb, .name_repair = "unique") %>%
      mutate(sim_num=row_number()) %>%
      pivot_longer(-sim_num) %>%
      left_join(data, by=c("value"="name"), copy = TRUE) %>%
      filter(!is.na(value)) %>%
      ungroup() %>%
      group_by(sim_num) %>%
      mutate(total_points=sum(Predicted_points)) %>%
      mutate(n=n_distinct(value)) %>%
      ungroup() %>%
      filter(n == 11) %>%
      filter(total_points==max(total_points)) %>%
      select(value, position, team_name, opponent_team, Predicted_points, total_points) %>%
      rename(name=1) %>%
      data.frame()
    
    split <- split(combo, rep(1:nrow(combo), each=11)) %>%
      keep(~ nrow(.) > 0)
    
    optimal_team <- split[[1]]
    
    optimal_team <- optimal_team %>%
      data.frame() %>%
      group_by(position) %>%
      mutate(n=n_distinct(name)) %>%
      mutate(n2=as.numeric(factor(name))) %>%
      ungroup() %>%
      mutate(x=ifelse(position=="GKP", 97, 0)) %>% ### Goalkeepers: 1
      mutate(y=ifelse(position=="GKP", 50, 0)) %>%
      mutate(x=ifelse(position=="DEF", 87, x)) %>% ### Defenders: 3, 4, 5
      mutate(y=ifelse(position=="DEF" & n==4 & n2==1, 10, y)) %>% ##### 4 defenders
      mutate(y=ifelse(position=="DEF" & n==4 & n2==2, 35, y)) %>%
      mutate(y=ifelse(position=="DEF" & n==4 & n2==3, 65, y)) %>%
      mutate(y=ifelse(position=="DEF" & n==4 & n2==4, 90, y)) %>%
      mutate(y=ifelse(position=="DEF" & n==5 & n2==1, 5, y)) %>% ##### 5 defenders
      mutate(y=ifelse(position=="DEF" & n==5 & n2==2, 25, y)) %>%
      mutate(y=ifelse(position=="DEF" & n==5 & n2==3, 50, y)) %>%
      mutate(y=ifelse(position=="DEF" & n==5 & n2==4, 75, y)) %>%
      mutate(y=ifelse(position=="DEF" & n==5 & n2==5, 95, y)) %>%
      mutate(y=ifelse(position=="DEF" & n==3 & n2==1, 30, y)) %>% #### 3 defenders
      mutate(y=ifelse(position=="DEF" & n==3 & n2==2, 50, y)) %>%
      mutate(y=ifelse(position=="DEF" & n==3 & n2==3, 70, y)) %>%
      mutate(x=ifelse(position=="MID", 70, x)) %>% ### Midfielders: 2, 3, 4, 5
      mutate(y=ifelse(position=="MID" & n==4 & n2==1, 10, y)) %>% #### 4 mids
      mutate(y=ifelse(position=="MID" & n==4 & n2==2, 35, y)) %>%
      mutate(y=ifelse(position=="MID" & n==4 & n2==3, 65, y)) %>%
      mutate(y=ifelse(position=="MID" & n==4 & n2==4, 90, y)) %>%
      mutate(y=ifelse(position=="MID" & n==5 & n2==1, 5, y)) %>% #### 5 mids
      mutate(y=ifelse(position=="MID" & n==5 & n2==2, 30, y)) %>%
      mutate(y=ifelse(position=="MID" & n==5 & n2==3, 50, y)) %>%
      mutate(y=ifelse(position=="MID" & n==5 & n2==4, 70, y)) %>%
      mutate(y=ifelse(position=="MID" & n==5 & n2==5, 95, y)) %>%
      mutate(y=ifelse(position=="MID" & n==3 & n2==1, 25, y)) %>% #### 3 mids
      mutate(y=ifelse(position=="MID" & n==3 & n2==2, 50, y)) %>%
      mutate(y=ifelse(position=="MID" & n==3 & n2==3, 75, y)) %>%
      mutate(y=ifelse(position=="MID" & n==2 & n2==1, 35, y)) %>% #### 2 mids
      mutate(y=ifelse(position=="MID" & n==2 & n2==2, 70, y)) %>%
      mutate(x=ifelse(position=="FWD", 55, x)) %>% ### Forwards: 1, 2, 3
      mutate(y=ifelse(position=="FWD" & n==2 & n2==1, 35, y)) %>% #### 2 fwds
      mutate(y=ifelse(position=="FWD" & n==2 & n2==2, 65, y)) %>%
      mutate(y=ifelse(position=="FWD" & n==3 & n2==1, 25, y)) %>% #### 3 fwds
      mutate(y=ifelse(position=="FWD" & n==3 & n2==2, 50, y)) %>%
      mutate(y=ifelse(position=="FWD" & n==3 & n2==3, 75, y)) %>%
      mutate(y=ifelse(position=="FWD" & n==1 & n2==1, 50, y)) %>% #### 1 fwd
      mutate(x2=x-3) %>%
      mutate(x3=x-6)
    
    ##### Autosubs order
    subs <- data %>% filter(!name %in% optimal_team$name) %>%
      arrange(-Predicted_points)
    
    output$plots <- renderPlot({
      ggplot(optimal_team, aes(y=y)) +
        annotate_pitch(colour = "white",
                       fill = 'darkgreen',
                       limits = F) +
        geom_point(aes(x=x)) +
        geom_point(aes(x=x2)) +
        geom_point(aes(x=x3)) +
        geom_label(aes(x=x, label=name), color="white", fill="black", size=7) +
        geom_label(aes(x=x2, label=Predicted_points), color="white", fill="black", size=7) +
        geom_label(aes(x=x3, label=opponent_team), color="white", fill="black", size=7) +
        theme_pitch() +
        theme(panel.background = element_rect(fill = "darkgreen")) +
        coord_flip(xlim = c(49, 101)) +
        scale_y_reverse() 
      
    })
    
    output$Total_score <- renderUI({
      paste0("Team Score: ", sum(optimal_team$Predicted_points))
    })
    
    output$subs <- renderUI({
      paste0("Subs: ", subs$name[1], " (", subs$Predicted_points[1], "), ",
             subs$name[2], " (", subs$Predicted_points[2], "), ",
             subs$name[3], " (", subs$Predicted_points[3], "), ",
             subs$name[4], " (", subs$Predicted_points[4], "), ")
    })
    
  })
  
  output$picture <- renderUI({
    img_tag <- tags$img(src = "https://images.unsplash.com/photo-1555862124-94036092ab14?ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&w=2071&q=80",
                        width = "750px", height = "375px")
    div(img_tag, style = "text_align: left;")
  })
  
  output$picture2 <- renderUI({
      img_tag <- tags$img(src = "https://images.unsplash.com/photo-1624280157150-4d1ed8632989?ixlib=rb-4.0.3&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&w=1974&q=80",
                          width = "750px", height= "375px")
      div(img_tag, style = "text_align: left;")
    })
    
  output$gw2 <- renderUI({
    selectizeInput("gameweek2",
                   label = "Select Gameweek",
                   choices=unique(upload()$GW),
                   multiple = F,
                   options = list(
                     maxItems = 1
                   ))
  })
  
    output$position <- renderUI({
      selectizeInput("position", "Position",
                     choices = unique(upload()$position),
                     multiple = T,
                     options = list(
                       maxItems = 1
                     ))
    })
    
    comp <- reactive({
      req(upload(), input$position)
      data <- subset(upload(), position %in% input$position)
    })
    
    output$player1 <- renderUI({
      selectizeInput("player1", "Player 1",
                     choices = unique(comp()$name),
                     multiple = T,
                     options = list(
                       maxItems = 1
                     ))
    })
    
    output$player2 <- renderUI({
      selectizeInput("player2", "Player 2",
                     choices = unique(comp()$name),
                     multiple = T,
                     options = list(
                       maxItems = 1
                     ))
    })
    
    comp2 <- reactive({
      req(comp(), input$gameweek2, input$player1, input$player2)
      p1 <- subset(comp(), GW %in% input$gameweek2) %>%
        filter(name %in% input$player1)
      p2 <- subset(comp(), GW %in% input$gameweek2) %>% 
        filter(name %in% input$player2)
      data <- rbind(p1, p2) %>% 
        select(name, position, GW, Predicted_points, contains("z_score"), team_name, opponent_team)
    })
    
    output$table <- renderDataTable({
        comp2()
      })
}

shinyApp(ui, server)
