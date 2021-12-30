#Program----------------------------------------------------------------------------------------------------
library(shiny)
library(datasets)
file<-read.csv('C:/Users/boonb/OneDrive/Documents/weather/current_year_weather.csv',header=TRUE)
cd1<-data.frame(file)
cd1
cd<-as.matrix(cd1,nrow=7,ncol=3,byrow=TRUE)

file<-read.csv('C:/Users/boonb/OneDrive/Documents/weather/previous_year_weather.csv',header=FALSE)
pd1<-data.frame(file)
pd<-as.matrix(pd1,nrow=15,ncol=3,byrow=TRUE)



# Define UI for dataset viewer app ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,600');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #48ca3b;
                    }
                    body {
                    background-image: url(C:/Users/boonb/Desktop/download.png);
                     background-position: center center;
                     background-attachment: fixed;
                    background-repeat: no-repeat;
                    background-size: 100% 100%;
                    }
.section .reveal .state-background {
    background-image: url(C:/Users/boonb/Desktop/download.png);
                    background-position: center center;
                    background-attachment: fixed;
                    background-repeat: no-repeat;
                    background-size: 100% 100%;
                    }
                    
                    "))
    ),
  
  
  
  # App title ----
  headerPanel("Weather Prediction"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs1",
                   label = "Number of observations of the current year:",
                   value = 7),
      
      numericInput(inputId = "obs2",
                   label = "Number of observations of the previous year:",
                   value = 14)
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      h3("Summary of Dataset of Previous Year"),
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary1"),
      h3("Summary of Dataset of Current Year"),
      verbatimTextOutput("summary2"),
      h3("Predicted Temperature"),
      # Output: Verbatim text for predicted temperature ----
      verbatimTextOutput("temp"),
      h3("Table View of Dataset of Previous Year"),
      # Output: HTML table with requested number of observations ----
      tableOutput("view1"),
      h3("Table View of Dataset of Current Year"),
      tableOutput("view2")
    )
  )
    )


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  sw1<-pd[1:7,]
  sw1
  
  sw2<-pd[2:8,]
  sw2
  
  sw3<-pd[3:9,]
  sw3
  
  sw4<-pd[4:10,]
  sw4
  
  sw5<-pd[5:11,]
  sw5
  
  sw6<-pd[6:12,]
  sw6
  
  sw7<-pd[7:13,]
  sw7
  
  sw8<-pd[8:14,]
  sw8
  
  library(pdist)
  ed<-pdist(t(cd),t(sw1))
  val1<-as.matrix(ed)
  ed<-pdist(t(cd),t(sw2))
  val2<-as.matrix(ed)
  
  ed<-pdist(t(cd),t(sw3))
  val3<-as.matrix(ed)
  
  ed<-pdist(t(cd),t(sw4))
  val4<-as.matrix(ed)
  
  ed<-pdist(t(cd),t(sw4))
  val4<-as.matrix(ed)
  
  
  ed<-pdist(t(cd),t(sw5))
  val5<-as.matrix(ed)
  
  
  ed<-pdist(t(cd),t(sw6))
  val6<-as.matrix(ed)
  
  ed<-pdist(t(cd),t(sw7))
  val7<-as.matrix(ed)
  
  ed<-pdist(t(cd),t(sw8))
  val8<-as.matrix(ed)
  
  
  x1<-min(val1)
  x2<-min(val2)
  x3<-min(val3)
  x4<-min(val4)
  x5<-min(val5)
  x6<-min(val6)
  x7<-min(val7)
  x8<-min(val8)
  
  
  cv<-cov(cd)
  pv<-cov(pd)
  
  mean1=mean(cv)
  mean2=mean(pv)
  mean=(mean1+mean2)/2
  mean
  
  predicted_temp<-mean+cd[1,1]
  predicted_temp
  
  
  
  
  # Generate a summary of the dataset ----
  
  output$temp<-renderText({
    predicted_temp
  })
  
  
  
  output$view<-renderTable({head(read.csv('C:/Users/boonb/OneDrive/Documents/weather/current_year_weather.csv'))
  })
  
  output$summary2 <- renderText({
    dataset <- read.csv('C:/Users/boonb/OneDrive/Documents/weather/current_year_weather.csv')
    summary(dataset)
  })
  
  
  output$view1<-renderTable({head(read.csv('C:/Users/boonb/OneDrive/Documents/weather/previous_year_weather.csv'))
  })
  
  output$view2<-renderTable({head(read.csv('C:/Users/boonb/OneDrive/Documents/weather/current_year_weather.csv'))
  })
  
  output$summary1 <- renderText({
    dataset <- read.csv('C:/Users/boonb/OneDrive/Documents/weather/previous_year_weather.csv')
    summary(dataset)
  })
  
  #Show the first "n" observations ----
  output$view1 <- renderTable({
    head(read.csv('C:/Users/boonb/OneDrive/Documents/weather/previous_year_weather.csv'), n = input$obs1)
  })
  #Show the first "n" observations ----
  output$view2 <- renderTable({
    head(read.csv('C:/Users/boonb/OneDrive/Documents/weather/previous_year_weather.csv'), n = input$obs2)
  })
  
  # To print the predicted Temperature----
  
}

# Create Shiny app ----
shinyApp(ui, server)


#Program----------------------------------------------------------------------------------------------------
library(shiny)
library(datasets)
file<-read.csv('C:/Users/boonb/OneDrive/Documents/weather/current_year_weather.csv',header=TRUE)
cd1<-data.frame(file)
cd1
cd<-as.matrix(cd1,nrow=7,ncol=3,byrow=TRUE)

file<-read.csv('C:/Users/boonb/OneDrive/Documents/weather/previous_year_weather.csv',header=FALSE)
pd1<-data.frame(file)
pd<-as.matrix(pd1,nrow=15,ncol=3,byrow=TRUE)



# Define UI for dataset viewer app ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,600');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #48ca3b;
                    }
                    body {
                    background-image: url(C:/Users/boonb/Desktop/download.png);
                     background-position: center center;
                     background-attachment: fixed;
                    background-repeat: no-repeat;
                    background-size: 100% 100%;
                    }
.section .reveal .state-background {
    background-image: url(C:/Users/boonb/Desktop/download.png);
                    background-position: center center;
                    background-attachment: fixed;
                    background-repeat: no-repeat;
                    background-size: 100% 100%;
                    }
                    
                    "))
    ),
  
  
  
  # App title ----
  headerPanel("Weather Prediction"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs1",
                   label = "Number of observations of the current year:",
                   value = 7),
      
      numericInput(inputId = "obs2",
                   label = "Number of observations of the previous year:",
                   value = 14)
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      h3("Summary of Dataset of Previous Year"),
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary1"),
      h3("Summary of Dataset of Current Year"),
      verbatimTextOutput("summary2"),
      h3("Predicted Temperature"),
      # Output: Verbatim text for predicted temperature ----
      verbatimTextOutput("temp"),
      h3("Table View of Dataset of Previous Year"),
      # Output: HTML table with requested number of observations ----
      tableOutput("view1"),
      h3("Table View of Dataset of Current Year"),
      tableOutput("view2")
    )
  )
    )


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  sw1<-pd[1:7,]
  sw1
  
  sw2<-pd[2:8,]
  sw2
  
  sw3<-pd[3:9,]
  sw3
  
  sw4<-pd[4:10,]
  sw4
  
  sw5<-pd[5:11,]
  sw5
  
  sw6<-pd[6:12,]
  sw6
  
  sw7<-pd[7:13,]
  sw7
  
  sw8<-pd[8:14,]
  sw8
  
  library(pdist)
  ed<-pdist(t(cd),t(sw1))
  val1<-as.matrix(ed)
  ed<-pdist(t(cd),t(sw2))
  val2<-as.matrix(ed)
  
  ed<-pdist(t(cd),t(sw3))
  val3<-as.matrix(ed)
  
  ed<-pdist(t(cd),t(sw4))
  val4<-as.matrix(ed)
  
  ed<-pdist(t(cd),t(sw4))
  val4<-as.matrix(ed)
  
  
  ed<-pdist(t(cd),t(sw5))
  val5<-as.matrix(ed)
  
  
  ed<-pdist(t(cd),t(sw6))
  val6<-as.matrix(ed)
  
  ed<-pdist(t(cd),t(sw7))
  val7<-as.matrix(ed)
  
  ed<-pdist(t(cd),t(sw8))
  val8<-as.matrix(ed)
  
  
  x1<-min(val1)
  x2<-min(val2)
  x3<-min(val3)
  x4<-min(val4)
  x5<-min(val5)
  x6<-min(val6)
  x7<-min(val7)
  x8<-min(val8)
  
  
  cv<-cov(cd)
  pv<-cov(pd)
  
  mean1=mean(cv)
  mean2=mean(pv)
  mean=(mean1+mean2)/2
  mean
  
  predicted_temp<-mean+cd[1,1]
  predicted_temp
  
  
  
  
  # Generate a summary of the dataset ----
  
  output$temp<-renderText({
    predicted_temp
  })
  
  
  
  output$view<-renderTable({head(read.csv('C:/Users/boonb/OneDrive/Documents/weather/current_year_weather.csv'))
  })
  
  output$summary2 <- renderText({
    dataset <- read.csv('C:/Users/boonb/OneDrive/Documents/weather/current_year_weather.csv')
    summary(dataset)
  })
  
  
  output$view1<-renderTable({head(read.csv('C:/Users/boonb/OneDrive/Documents/weather/previous_year_weather.csv'))
  })
  
  output$view2<-renderTable({head(read.csv('C:/Users/boonb/OneDrive/Documents/weather/current_year_weather.csv'))
  })
  
  output$summary1 <- renderText({
    dataset <- read.csv('C:/Users/boonb/OneDrive/Documents/weather/previous_year_weather.csv')
    summary(dataset)
  })
  
  #Show the first "n" observations ----
  output$view1 <- renderTable({
    head(read.csv('C:/Users/boonb/OneDrive/Documents/weather/previous_year_weather.csv'), n = input$obs1)
  })
  #Show the first "n" observations ----
  output$view2 <- renderTable({
    head(read.csv('C:/Users/boonb/OneDrive/Documents/weather/previous_year_weather.csv'), n = input$obs2)
  })
  
  # To print the predicted Temperature----
  
}

# Create Shiny app ----
shinyApp(ui, server)

