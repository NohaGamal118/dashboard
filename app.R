data_set<-read.csv('cancer patient data sets.csv')
summary(data_set)

library(shiny)
library(shinydashboard)
library(DT)

# Define UI for application 
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Lung Cancer Prediction Dashboard" ,titleWidth = 650),
    dashboardSidebar(
      sidebarMenu(
        id="s",
        menuItem("Dataset",tabName = "Dataset",icon = icon("database")),
        menuItem(text = "Visualization",tabName = "Visualization",
                 icon = icon("chart-line"))
        
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Dataset",
                tabBox(id="tab1",width = 12,
                       tabPanel("Data",icon = icon("address-card")
                                ,dataTableOutput("dataset")))
        ),
        tabItem(tabName = "Visualization",
                tabBox(id="tab2",width = 12,
                       tabPanel("Piechart",
                                plotOutput("pieplot")),
                       tabPanel("Histogram",
                                sidebarLayout(
                                  sidebarPanel(sliderInput(inputId = "bins", label = "Num Of Bins", min = 1, max = 10, value = 2, step = 1)),
                                  mainPanel(
                                    plotOutput("histplot"))
                                )),
                    
                       tabPanel("BoxPlot",
                                plotOutput("display_Plot"))
                )))))
  
)

# Define server logic required 
server <- function(input, output) {
  

  output$dataset <-renderDataTable(
    data_set
  )
  
  output$pieplot <- renderPlot({
    Low=nrow(data_set[data_set$Level == 'Low', ])
    Low
    High=nrow(data_set[data_set$Level == 'High', ])
    High
    Medium=nrow(data_set[data_set$Level == 'Medium', ])
    Medium
    slices<-c(Low,High,Medium)
    
    lbls<-c("Low","High","Medium")
    
    pct<-slices/sum(slices)*100
    #calculate percentile
    lbls<-paste(lbls,pct)
    #add percent to labels
    lbls<-paste(lbls,"%",sep = "")
    #add % to labels
    pie(slices,labels = lbls,col = rainbow(length(lbls)),main="pie chart of Levels")
  })
  
  output$histplot <- renderPlot({
    bins <- input$bins
    hist(data_set$Shortness.of.Breath,col="lightblue",xlab = "Shortness of Breath",main = "Shortness of Breath Histogram",breaks= bins,prop=TRUE)
  })
 
  output$display_Plot<-renderPlot({
    male<- data_set[data_set$Gender=='1',]
    female<-data_set[data_set$Gender=='2',]
    boxplot(male$Smoking,female$Smoking,ylim=c(0,10), names=c("male","female"),main = "Smoking Boxplot")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
