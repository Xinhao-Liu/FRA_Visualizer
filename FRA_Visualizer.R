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
      div(img(src = "https://skins.webservices.illinois.edu/files/15890/wizard_header.png?iIndex=0928T094828", height = 100, width = 500, class = "pull-right"),
          h1("FRA REA Data Visualizer"),
          h3("RailTEC Safety and Risk Group"),
          h5("Version 1.0, 3/24/2023"))
      ),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          radioButtons("plot_type",
                       h3("Let us know your interest:"),
                       choices = c("Frequency vs. Severity","Accident Severity vs. Accident Cause"),
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
            condition = "input.plot_type == 'Frequency vs. Severity' || input.plot_type == 'Accident Severity vs. Accident Cause'",  
            dateRangeInput("date",
                        h4("Date Range"),
                        min = "1996-01-01",
                        max = floor_date(Sys.Date(), "year") - 1,
                        start = "2010-01-01",
                        end = "2020-01-01",
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
            
            checkboxGroupInput("accident_type",
                        h4("Accident Type(s)"),
                        choices = c("Derailments","Collisions","Grade Crossing",
                        "Other"),
                        selected = "Derailments"),
            
            checkboxGroupInput("track_type",
                        h4("Track Type(s)"),
                        choices = c("Mainline"=1,"Siding"=3,"Yard"=2,"Industry"=4),
                        selected = 1),
            
            checkboxGroupInput("accident_cause",
                        h4("Accident Cause Group(s)"),
                        choices = c("Track"="T","Equipment"="E","Human Factor"="H","Signal"="S","Miscellaneous"="M"),
                        selected = "T")),
            
          
          actionButton("Button0","Let me see the plot"),
          actionButton("Button1","About the Dataset")
          
        ),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(
            tabPanel("Plot", plotOutput('Number', width = 1200, height = 800), downloadButton("downloadPlot", "Download Plot")),
            tabPanel('Summary Statistics', DT::DTOutput('Summary'),downloadButton("downloadTable", "Download Table")),
            tabPanel("Example Data", DT::DTOutput('All'), downloadButton("downloadData", "Download Data"), textOutput("Data_Info"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data = reactive({
    
    raw = read_csv("https://raw.githubusercontent.com/Xinhao-Liu/FRA_Visualizer/main/All_year_FRA_1996_2022_3.22.2023.csv")
    
    raw %>% 
      mutate(Accident_type = ifelse(TYPE_clean == "01", "Derailments",
                             ifelse(TYPE_clean %in% c("02","03","04","05","06","08"), "Collisions",
                             ifelse(TYPE_clean == "07", "Grace Crossing", "Other")))) %>% 
      mutate(Date = as.Date(as.character(Date),format = "%Y%m%d")) %>% 
      filter(`class 1` %in% input$RRClass_type,
             TrainType %in% input$Train_type,
             Accident_type %in% input$accident_type,
             ACCTRK %in% input$track_type,
             Category %in% input$accident_cause,
             Date <= input$date[2],
             Date >= input$date[1])
      
    
  })
  
  summary_data = reactive({
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
  
  output$Summary = DT::renderDT({
    summarize() 
  })
  
  show_data = eventReactive(input$Button0, {
    data()
  })
    
  output$All = DT::renderDT({
    data() %>% 
      select(Date, Accident_type, TotalDerail, Group, HIGHSPD)
  })
  
  plot_data = eventReactive(input$Button0, {
    
    if (input$plot_type == "Frequency vs. Severity") {
      summarize() %>% 
        ggplot(aes(x=Frequency,y=`Average Number of Cars Derailed`))+
        geom_point(shape=19, color="#FF8000", size = 5)+
        geom_text_repel(aes(label = `Group Name`),size =5) +
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
              axis.text.x = element_text(color = "#000000", size = 14,
                                         margin = margin(t = 0, r = 0, b = 5, l = 0)),
              axis.text.y = element_text(color = "#000000", size = 14,
                                         margin = margin(t = 0, r = 0, b = 0, l = 10)),
              axis.title.x = element_text(color = "#000000", size = 16, face="bold"),
              axis.title.y = element_text(color = "#000000", size = 16, face="bold",
                                          margin = margin(t = 0, r = 0, b = 0, l = 10)),
              legend.title = element_text(color = "#000000", size = 14),
              legend.text = element_text(color = "#000000", size = 14))
    } else if (input$plot_type == "Accident Severity vs. Accident Cause") {
      max_val = max(summarize() %>% 
        select(`Total Number of Cars`))
      summarize() %>% 
        ggplot()+
        geom_col(aes(x=fct_rev(fct_reorder(`Group Name`, `Total Number of Cars`)), y = `Total Number of Cars`), fill = "#0000FF")+
        geom_line(aes(group=1,
                      x=fct_rev(fct_reorder(`Group Name`, `Total Number of Cars`)), y = parse_number(`Cumulative Percentage`)/100 * max(`Total Number of Cars`)),
                  color = "red", lwd = 1)+
        xlab("Accident Cause")+
        ylab("Number of Cars Derailed") +
        theme_bw() + 
        scale_y_continuous(expand  = c(0,100), labels = comma,
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
              axis.text.x = element_text(color = "#000000", size = 14,
                                         margin = margin(t = 0, r = 0, b = 5, l = 0),
                                         angle = 90, vjust = 0.5, hjust=1),
              axis.text.y = element_text(color = "#000000", size = 14,
                                         margin = margin(t = 0, r = 0, b = 0, l = 10)),
              axis.title.y.right = element_text(color = "#000000", size = 16, face = "bold",
                                               margin = margin(t = 0, r = 0, b = 0, l = 10), angle = 90),
              axis.title.x = element_text(color = "#000000", size = 16, face="bold"),
              axis.title.y = element_text(color = "#000000", size = 16, face="bold",
                                          margin = margin(t = 0, r = 0, b = 0, l = 10)),
              legend.title = element_text(color = "#000000", size = 14),
              legend.text = element_text(color = "#000000", size = 14))
    }
    
  })
  
  output$Number = renderPlot({
    plot_data()
  })
  
  observeEvent(input$Button1, {
    showModal(modalDialog("Current Visualizer contains FRA REA data from 1996-2022. Please contact Xinhao Liu (xinhaol2@illinois.edu) if you have
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
    
    content = function(file){
      write.csv(data(), file)
    }
  )
  
  output$downloadTable = downloadHandler(
    filename = function(){
      paste("Your_table","csv",sep=".")
    },
    
    content = function(file){
      write.csv(summarize(), file)
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
