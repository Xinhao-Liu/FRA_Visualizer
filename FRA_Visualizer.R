library(shiny)
library(shinythemes)
library(plotly)
library(DT)
library(httr)
library(jsonlite)
library(readr)
library(ggrepel)
library(lubridate)
library(forcats)
library(tidyverse)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    
    theme = shinythemes::shinytheme('spacelab'),
    
    titlePanel(
      div(img(src = "data:image/png;base64,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", height = 100, width = 500, class = "pull-right"),
          h1("FRA REA Data Visualizer"),
          h3("RailTEC Safety and Risk Group"),
          h5("Version 1.5.1, 11/01/2023"))
      ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          radioButtons("plot_type",
                       h3("Let us know your interest:"),
                       choices = c("Frequency vs. Severity","Accident Severity vs. Accident Cause",
                                   "Rate Analysis"),
                       selected = character(0)),
          
          conditionalPanel(
            condition = "input.plot_type == 'Frequency vs. Severity'",
            
            radioButtons("rank",
                               h4("Rank by"),
                               choices = c("Frequency","Severity"),
                               selected = "Frequency"),
            radioButtons("top_n",
                         h4("Top # cause(s)"),
                         choices = c("Show me ALL","Show me top n"),
                         selected = "Show me ALL")),
          
          conditionalPanel(
            condition = "input.plot_type == 'Frequency vs. Severity' && input.top_n == 'Show me top n'",
            numericInput("top_N", "Top #", value=10)),
          
          
            ######
          conditionalPanel(
            condition = "input.plot_type == 'Frequency vs. Severity' || input.plot_type == 'Accident Severity vs. Accident Cause' || 
                         input.plot_type == 'Rate Analysis'",  
            dateRangeInput("date",
                        h4("Date Range"),
                        min = "1997-01-01",
                        max = floor_date(Sys.Date(), "year") - 1,
                        start = "2013-01-01",
                        end = "2022-01-01",
                        format = "yyyy-mm-dd",
                        startview = 'year'),
            
            checkboxGroupInput("Train_type",
                               h4("Train Type"),
                               choices = c("Freight"="F","Passenger"="P","Other"="O"),
                               selected = "F"),
            
            checkboxGroupInput("RRClass_type",
                               h4("Railroad Class"),
                               choices = c("Class I"="class1","Non Class I"="non-1"),
                               selected = "class1"),
            
            conditionalPanel(
              condition = "input.RRClass_type == 'class1'",
              checkboxGroupInput("ClassI_type",
                                 h4("Class I Railroad Name"),
                                 choices = c("BNSF"="BNSF","KCS"="KCS","UP"="UP","CSX"="CSX","NS"="NS","CN"="CNGT","CP"="CP(US)"),
                                 selected = c("BNSF","KCS","UP","CSX","NS","CNGT","CP(US)"))),
            
            checkboxGroupInput("accident_type",
                        h4("Accident Type(s)"),
                        choices = c("Derailments","Collisions","Grade Crossing",
                        "Other"),
                        selected = "Derailments"),
            
            checkboxGroupInput("track_type",
                        h4("Track Type(s)"),
                        choices = c("Mainline"=1,"Siding"=3,"Yard"=2,"Industry"=4),
                        selected = 1),
            
            checkboxGroupInput("track_class",
                               h4("FRA Track Class(es)"),
                               choices = c("1","2","3","4","5","6","7","8","9","X"),
                               selected = c("1","2","3","4","5","6","7","8","9","X")),
            
            sliderInput("speed", 
                        h4("Maximum Speed Reported"),
                        min = 0, max = 100, value = c(0, 100), step = 1),
            
            checkboxGroupInput("accident_cause",
                        h4("Accident Cause Group(s)"),
                        choices = c("Track"="T","Equipment"="E","Human Factor"="H","Signal"="S","Miscellaneous"="M"),
                        selected = "T")),
            
          
          actionButton("Button0","Let me see the plot"),
          actionButton("Button1","About the Dataset")
          
        ),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel("Plot", plotOutput('Number', width = 1200, height = 800),
                     conditionalPanel(
                       condition = "input.plot_type == 'Rate Analysis'",
                       radioButtons("breakdown",
                                    h4(HTML("Breakdown by:<br>(with all filters on the left applied)")),
                                    choices = c("Accident Type","Accident Cause Group","Class I RR Company", "Track Type"),
                                    selected = "Accident Type")),downloadButton("downloadPlot", "Download Plot")),
            tabPanel('Summary Statistics', DT::DTOutput('Summary'),downloadButton("downloadTable", "Download Table")),
            tabPanel("Example Data", DT::DTOutput('All'), textInput("passcode", "Enter Passcode:"), downloadButton("downloadData", "Download Data"), verbatimTextOutput("errorMessage"), textOutput("Data_Info"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  correct = "UIUCRailTEC"
  
  validate_passcode <- function(input_passcode) {
    if (input_passcode == correct) {
      return(TRUE)  # Passcode is correct
    } else {
      return(FALSE)  # Passcode is incorrect
    }
  }

  data = eventReactive(input$Button0, {
    
    raw = read_csv("https://raw.githubusercontent.com/Xinhao-Liu/FRA_Visualizer/main/All_year_FRA_1996_2022_10.25.2023.csv")
    
    raw %>% 
      filter(Year > 1996) %>% 
      filter(!is.na(TYPE_clean), !is.na(TotalDerail), TotalDerail >= 0) %>% 
      mutate(Accident_type = ifelse(TYPE_clean == "01", "Derailments",
                             ifelse(TYPE_clean %in% c("02","03","04","05","06","08"), "Collisions",
                             ifelse(TYPE_clean == "07", "Grade Crossing", "Other")))) %>% 
      mutate(Date = as.Date(as.character(Date),format = "%Y%m%d")) %>% 
      mutate(HIGHSPD = as.numeric(HIGHSPD)) %>% 
      filter(`class 1` %in% input$RRClass_type,
             TrainType %in% input$Train_type,
             Accident_type %in% input$accident_type,
             ACCTRK %in% input$track_type,
             Category %in% input$accident_cause,
             Date <= input$date[2],
             Date >= input$date[1],
             TRKCLAS %in% input$track_class,
             HIGHSPD >= input$speed[1],
             HIGHSPD <= input$speed[2]) %>% 
      filter(if(length(input$RRClass_type) == 1 && input$RRClass_type == 'class1') {
                  `Railroad Successor` %in% input$ClassI_type
              } else {
                SUMS != "999" 
              })
      
    
  })
  
  traffic_data = eventReactive(input$Button0, {
    raw_traffic = read_csv("https://raw.githubusercontent.com/Xinhao-Liu/FRA_Visualizer/main/All%20Traffic%20Data_1996-2022_class1_ONE.csv")
    
    inter = data() %>% 
      mutate(`Railroad Successor` = ifelse(`Railroad Successor` == "CNGT", "CN",
                                           ifelse(`Railroad Successor` == "CP(US)", "CP", `Railroad Successor`))) %>% 
      mutate(traffic_name = paste(ifelse(`class 1`=="class1", `Railroad Successor`, "Non"),
                                  "ClassI",
                                  "Freight", 
                                  ifelse(ACCTRK%in%c(1,3),"Both_Mainline","Non_Mainline"),sep = "_")) %>% 
      select(`Railroad Successor`,`class 1`,ACCTRK,Accident_type,Category,traffic_name,Year) %>% 
      left_join(raw_traffic,by=c("traffic_name"="...1")) %>% 
      mutate(index = Year - 1996 + 8, traffic_value = 0)
    
    num_vec = seq(1:nrow(inter))
    index_vec = as.vector(inter %>% select(index))[[1]]
    
    final = inter
    
    traffic_val = mapply(function(i, j) {
      final[i, ncol(inter)] = inter[i, j][[1]]
    }, i = num_vec, j = index_vec)
    
    final = final %>% 
      mutate(traffic_value = traffic_val) %>% 
      select(`Railroad Successor`,`class 1`,ACCTRK,Accident_type,Category,traffic_value,Year)
    
    final
    
  })
  
  
  summary_data = eventReactive(input$Button0,{
    data() %>% 
      filter(!is.na(Group)) %>% 
      group_by(Group) %>% 
      mutate(Frequency = n(), total_derail = sum(TotalDerail)) %>% 
      ungroup() %>% 
      mutate(frequency_ratio = Frequency/length(SUMS), severity_ratio = total_derail/sum(TotalDerail)) %>%
      group_by(Group) %>% 
      mutate(`Average Number of Cars Derailed` = round(total_derail/Frequency,1)) %>% 
      select(`Group Name`,Group, `Average Number of Cars Derailed`, Frequency, total_derail, severity_ratio)
  })
  
  summary_data_rate = eventReactive(input$Button0,{
    
    if (input$breakdown == "Accident Type") {
      traffic_data() %>% 
        group_by(Accident_type,Year) %>% 
        mutate(count=n(),final_traffic = sum(unique(traffic_value))) %>% 
        ungroup() %>% 
        group_by(Year) %>%
        mutate(final_traffic = max(final_traffic)) %>% 
        mutate(rate=count/final_traffic) %>% 
        unique() %>% 
        select(Accident_type,Year,rate) %>% 
        unique() %>% 
        rename(`Accident Type` = Accident_type)
    } else if (input$breakdown == "Accident Cause Group") {
      traffic_data() %>% 
        group_by(Category,Year) %>% 
        mutate(count=n(),final_traffic = sum(unique(traffic_value))) %>% 
        ungroup() %>% 
        group_by(Year) %>%
        mutate(final_traffic = max(final_traffic)) %>% 
        mutate(rate=count/final_traffic) %>% 
        unique() %>% 
        select(Category,Year,rate) %>% 
        unique() %>% 
        rename(`Accident Cause Group` = Category)
    } else if (input$breakdown == "Class I RR Company") {
      traffic_data() %>% 
        group_by(`Railroad Successor`,Year) %>% 
        mutate(count=n(),final_traffic = sum(unique(traffic_value))) %>% 
        ungroup() %>% 
        group_by(Year) %>%
        mutate(final_traffic = max(final_traffic)) %>% 
        mutate(rate=count/final_traffic) %>% 
        unique() %>% 
        select(`Railroad Successor`,Year,rate) %>% 
        unique() %>% 
        rename(`Class I RR Company` = `Railroad Successor`)
    } else if (input$breakdown == "Track Type") {
      traffic_data() %>% 
        mutate(ACCTRK = as.factor(ACCTRK)) %>% 
        group_by(ACCTRK,Year) %>% 
        mutate(count=n(),final_traffic = sum(unique(traffic_value))) %>% 
        ungroup() %>% 
        group_by(Year) %>%
        mutate(final_traffic = max(final_traffic)) %>% 
        mutate(rate=count/final_traffic) %>% 
        unique() %>% 
        select(ACCTRK,Year,rate) %>% 
        unique() %>% 
        rename(`Track Type` = ACCTRK)
    } 

  })
  
  summarize = eventReactive(input$Button0, {
    
    if (input$plot_type == "Frequency vs. Severity") {
      
      if (input$rank == "Frequency") {
        
        if (input$top_n == "Show me top n") {
          summary_data() %>% 
            unique() %>% 
            arrange(desc(Frequency)) %>% 
            select(`Group Name`,Group, `Average Number of Cars Derailed`, Frequency) %>% 
            head(input$top_N)
        } else{
          summary_data() %>% 
            unique() %>% 
            arrange(desc(Frequency)) %>% 
            select(`Group Name`,Group, `Average Number of Cars Derailed`, Frequency)
        }
      } else{
        
        if (input$top_n == "Show me top n") {
          summary_data() %>%
            unique() %>% 
            arrange(desc(`Average Number of Cars Derailed`)) %>% 
            select(`Group Name`,Group, `Average Number of Cars Derailed`, Frequency) %>% 
            head(input$top_N)
        } else{
          summary_data() %>% 
            unique() %>% 
            arrange(desc(`Average Number of Cars Derailed`)) %>% 
            select(`Group Name`,Group, `Average Number of Cars Derailed`, Frequency)
        }
        
      }
    } else if (input$plot_type == "Accident Severity vs. Accident Cause") {
      summary_data() %>% 
        unique() %>% 
        arrange(desc(total_derail)) %>% 
        ungroup() %>% 
        mutate(`Cumulative Percentage` = paste0(round(cumsum(severity_ratio),3)*100,"%"),`Total Number of Cars` = total_derail) %>% 
        select(`Group Name`,Group, Frequency, `Total Number of Cars`, `Cumulative Percentage`)
    }
    
  })
  
  

  
  plot_data = eventReactive(input$Button0, {
    
    if (input$plot_type == "Frequency vs. Severity") {
      summarize() %>% 
        ggplot(aes(x=Frequency,y=`Average Number of Cars Derailed`))+
        geom_point(shape=19, color="#FF8000", size = 5)+
        geom_text_repel(aes(label = `Group Name`),size =5, max.overlaps = 3, box.padding = 0.5) +
        geom_hline(aes(yintercept = mean(`Average Number of Cars Derailed`)), 
                   linetype = "dashed", color = "#000000") + 
        geom_vline(aes(xintercept = mean(Frequency)),
                   linetype = "dashed", color = "#000000") +
        xlab("Number of Accidents")+
        ylab("Average Number of Cars Derailed") +
        theme_bw() + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.position = "top",
              axis.text.x = element_text(color = "#000000", size = 16,
                                         margin = margin(t = 0, r = 0, b = 5, l = 0)),
              axis.text.y = element_text(color = "#000000", size = 16,
                                         margin = margin(t = 0, r = 0, b = 0, l = 10)),
              axis.title.x = element_text(color = "#000000", size = 20, face="bold"),
              axis.title.y = element_text(color = "#000000", size = 20, face="bold",
                                          margin = margin(t = 0, r = 0, b = 0, l = 10)),
              legend.title = element_text(color = "#000000", size = 16),
              legend.text = element_text(color = "#000000", size = 16))
    } else if (input$plot_type == "Accident Severity vs. Accident Cause") {
      max_val = max(summarize() %>% 
        select(`Total Number of Cars`))
      summarize() %>% 
        ggplot()+
        geom_col(aes(x=fct_reorder(`Group Name`, parse_number(`Cumulative Percentage`)+0.01), y = `Total Number of Cars`), fill = "#0000FF")+
        geom_line(aes(group=1,
                      x=`Group Name`, 
                      y = parse_number(`Cumulative Percentage`)/100 * max(`Total Number of Cars`)),
                  color = "red", lwd = 1)+
        xlab("Accident Cause")+
        ylab("Number of Cars Derailed") +
        theme_bw() + 
        scale_y_continuous(labels = comma,
                           sec.axis = sec_axis(~., 
                                               breaks = seq(0,max_val,max_val/5),
                                               labels = c("0%","20%","40%","60%","80%","100%"),
                                               name = "Cumulative Percentage")) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.position = "top",
              axis.text.x = element_text(color = "#000000", size = 16,
                                         margin = margin(t = 0, r = 0, b = 5, l = 0),
                                         angle = 90, vjust = 0.5, hjust=1),
              axis.text.y = element_text(color = "#000000", size = 16,
                                         margin = margin(t = 0, r = 0, b = 0, l = 10)),
              axis.title.y.right = element_text(color = "#000000", size = 20, face = "bold",
                                               margin = margin(t = 0, r = 0, b = 0, l = 10), angle = 90),
              axis.title.x = element_text(color = "#000000", size = 20, face="bold"),
              axis.title.y = element_text(color = "#000000", size = 20, face="bold",
                                          margin = margin(t = 0, r = 0, b = 0, l = 10)),
              legend.title = element_text(color = "#000000", size = 16),
              legend.text = element_text(color = "#000000", size = 16))
    } else if (input$plot_type == "Rate Analysis") {
      
      if (input$breakdown == "Track Type") {
        summary_data_rate() %>% 
          ggplot(aes(x=as.character(Year),y=rate,
                     group=`Track Type`,
                     color=`Track Type`))+
          geom_line(size=2)+
          xlab("Year")+
          ylab("Rate (per million miles)") +
          theme_bw() + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                panel.border = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.position = "right",
                axis.text.x = element_text(color = "#000000", size = 16,
                                           margin = margin(t = 0, r = 0, b = 5, l = 0)),
                axis.text.y = element_text(color = "#000000", size = 16,
                                           margin = margin(t = 0, r = 0, b = 0, l = 10)),
                axis.title.y.right = element_text(color = "#000000", size = 20, face = "bold",
                                                  margin = margin(t = 0, r = 0, b = 0, l = 10)),
                axis.title.x = element_text(color = "#000000", size = 20, face="bold"),
                axis.title.y = element_text(color = "#000000", size = 20, face="bold",
                                            margin = margin(t = 0, r = 0, b = 0, l = 10)),
                legend.title = element_text(color = "#000000", size = 16),
                legend.text = element_text(color = "#000000", size = 16))
      } else if (input$breakdown == "Class I RR Company") {
        summary_data_rate() %>% 
          ggplot(aes(x=as.character(Year),y=rate,
                     group=`Class I RR Company`,
                     color=`Class I RR Company`))+
                      geom_line(size=2)+
                      xlab("Year")+
                      ylab("Rate (per million miles)") +
                      theme_bw() + 
                      theme(panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(), 
                            panel.border = element_blank(),
                            axis.line = element_line(colour = "black"),
                            legend.position = "right",
                            axis.text.x = element_text(color = "#000000", size = 16,
                                                       margin = margin(t = 0, r = 0, b = 5, l = 0)),
                            axis.text.y = element_text(color = "#000000", size = 16,
                                                       margin = margin(t = 0, r = 0, b = 0, l = 10)),
                            axis.title.y.right = element_text(color = "#000000", size = 20, face = "bold",
                                                              margin = margin(t = 0, r = 0, b = 0, l = 10)),
                            axis.title.x = element_text(color = "#000000", size = 20, face="bold"),
                            axis.title.y = element_text(color = "#000000", size = 20, face="bold",
                                                        margin = margin(t = 0, r = 0, b = 0, l = 10)),
                            legend.title = element_text(color = "#000000", size = 16),
                            legend.text = element_text(color = "#000000", size = 16))
      } else if (input$breakdown == "Accident Cause Group") {
        summary_data_rate() %>% 
          ggplot(aes(x=as.character(Year),y=rate,
                     group=`Accident Cause Group`,
                     color=`Accident Cause Group`))+
            geom_line(size=2)+
            xlab("Year")+
            ylab("Rate (per million miles)") +
            theme_bw() + 
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  panel.border = element_blank(),
                  axis.line = element_line(colour = "black"),
                  legend.position = "right",
                  axis.text.x = element_text(color = "#000000", size = 16,
                                             margin = margin(t = 0, r = 0, b = 5, l = 0)),
                  axis.text.y = element_text(color = "#000000", size = 16,
                                             margin = margin(t = 0, r = 0, b = 0, l = 10)),
                  axis.title.y.right = element_text(color = "#000000", size = 20, face = "bold",
                                                    margin = margin(t = 0, r = 0, b = 0, l = 10)),
                  axis.title.x = element_text(color = "#000000", size = 20, face="bold"),
                  axis.title.y = element_text(color = "#000000", size = 20, face="bold",
                                              margin = margin(t = 0, r = 0, b = 0, l = 10)),
                  legend.title = element_text(color = "#000000", size = 16),
                  legend.text = element_text(color = "#000000", size = 16))+
                  scale_color_manual(values = c("#000000", "#EBC201", "#0000FF", "#FF00FF", "#00E8A6"))
      } else if (input$breakdown == "Accident Type") {
        summary_data_rate() %>% 
          ggplot(aes(x=as.character(Year),y=rate,
                     group=`Accident Type`,
                     color=`Accident Type`))+
                        geom_line(size=2)+
                        xlab("Year")+
                        ylab("Rate (per million miles)") +
                        theme_bw() + 
                        theme(panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              panel.background = element_blank(), 
                              panel.border = element_blank(),
                              axis.line = element_line(colour = "black"),
                              legend.position = "right",
                              axis.text.x = element_text(color = "#000000", size = 16,
                                                         margin = margin(t = 0, r = 0, b = 5, l = 0)),
                              axis.text.y = element_text(color = "#000000", size = 16,
                                                         margin = margin(t = 0, r = 0, b = 0, l = 10)),
                              axis.title.y.right = element_text(color = "#000000", size = 20, face = "bold",
                                                                margin = margin(t = 0, r = 0, b = 0, l = 10)),
                              axis.title.x = element_text(color = "#000000", size = 20, face="bold"),
                              axis.title.y = element_text(color = "#000000", size = 20, face="bold",
                                                          margin = margin(t = 0, r = 0, b = 0, l = 10)),
                              legend.title = element_text(color = "#000000", size = 16),
                              legend.text = element_text(color = "#000000", size = 16))
      }
      
    }
    
  })
  
  output$Number = renderPlot({
    plot_data()
  })
  
  output$Summary = DT::renderDT({
    
    if (input$plot_type == "Frequency vs. Severity" || input$plot_type == "Accident Severity vs. Accident Cause") {
      summarize() 
    } else if (input$plot_type == "Rate Analysis") {
      summary_data_rate()
    }
    
  })
  
  output$All = DT::renderDT({
    data() %>% 
      select(Date,RAILROAD,STATION, State, Accident_type, TotalDerail, Group, HIGHSPD)
  })
  
  
  observeEvent(input$Button1, {
    showModal(modalDialog("Current Visualizer contains FRA REA data from 1997-2022. Please contact Xinhao Liu (xinhaol2@illinois.edu) if you have
                          any questions or suggestions! Enjoy!" ))
  })
  
  show_data_info = eventReactive(input$Button0, {
    "You can download the processed FRA REA data based on the filters you have selected on the left. The table above is an example that only shows five columns."
  })

  output$Data_Info = renderText({
    show_data_info()
  })

  output$downloadData = downloadHandler(
    filename = function(){
      paste("FRA_REA","csv",sep=".")
    },
    
    content = function(file) {
      # Check if the passcode is correct before generating the file
      if (validate_passcode(input$passcode)) {
        # Generate or load the content of the file here
        # In this example, 'data' is a data frame
        write.csv(data(), file)
      } else {
        # Passcode is incorrect, prevent download
        output$errorMessage <- renderText({
          "Incorrect passcode. Access denied."
        })
        stop("Incorrect passcode. Access denied.")
      }
    }
  )
  
  output$downloadTable = downloadHandler(
    filename = function(){
      paste("Your_table","csv",sep=".")
    },
    
    content = function(file){
      if (input$plot_type == "Frequency vs. Severity" || input$plot_type == "Accident Severity vs. Accident Cause") {
        write.csv(summarize(), file)
      } else if (input$plot_type == "Rate Analysis") {
        write.csv(summary_data_rate(), file)
      }
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("Your_plot", 'png', sep=".") },
    content = function(file) {
      ggsave(file, plot = plot_data(), device = "png", width = 20, height = 14)
    }
  )
  


}

# Run the application 
shinyApp(ui = ui, server = server)
