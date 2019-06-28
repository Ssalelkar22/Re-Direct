library(shiny)
library(plotrix)
library(shinydashboard)
library(ggplot2)
library(readxl)
library(dplyr)
library(ECharts2Shiny)
library(DT)
library(pipeR)
library(shinythemes)
library(data.table)
library(formattable)
library(tidyr)
library(rlist)

data3<-read.csv("C:\\Users\\Shweta\\Desktop\\FinalyearProject\\Final\\Final_Wholesalerexcel.csv");   #Fetch the data
ID=20695   #Retrieve the Wholesaler ID
data=filter(data3,Wholesaler_id==ID)     #Filter data based on wholesaler's ID
# pre1;
# df1=data.frame(Units_Sold=4139,Unit_Price=154.06,Unit_Cost=90.93,Year=2018)
# pre2=0;
# df2=data.frame(Units_Sold=4139,Unit_Price=154.06,Unit_Cost=90.93,Year=2019)
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Tables", tabName = "table", icon = icon("list-alt")),
    menuItem("Prediction", tabName = "prediction", icon = icon("clock"))
    
  )
)

# combine the two fluid rows to make the body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
  
  fluidRow(
    
    # Dynamic valueBoxes
    valueBoxOutput("profitBox2"),
    
    valueBoxOutput("salesBox2"),
    valueBoxOutput("prodBox2")
  ),
  fluidRow(
    title = "Revenue by Class"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("prediction2", height = "700px",click="plot_click"),verbatimTextOutput("hover2")
    
  ),
  fluidRow(
    box(
      selectInput("p", label = h3("Select Department"),
                  choices = list("OverAll" = 'a', "Bottoms" = 'b', "Dresses" = 'd',"Jackets" = 'j',"Tops" = 'o'),
                  selected = 'a')
    )
  ),
  frow3<-fluidRow(
    title = "Sales by season"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("prediction1", height = "300px",click="plot_click"),verbatimTextOutput("hover1")
    
  ),
  fluidRow(
    plotOutput("prediction5", height = "700px",click="plot_click"),verbatimTextOutput("hover5")
    
  ),
  

  fluidRow( box(plotOutput("prediction6",height = "300px",width="1000px",click="plot_click"),verbatimTextOutput("hover6"))
  )),
    tabItem(tabName = "table",
            fluidRow(
              selectInput("r", label = h3("Select Year"),
                          choices = list("OverAll" = 'a', "2010" = 'b', "2011" = 'c',"2012" = 'd',"2013" = 'e',"2014" = 'f',"2015" = 'g',"2016" = 'h',"2017" = 'i'),
                          selected = 'a')
            ),
  fluidRow(
    box(title="Quantity sold in each class on all Platforms",formattableOutput("table1")),
    box(title="Profit earned in each class on all Platforms",formattableOutput("table2"))
  )
  ),
  tabItem(tabName="prediction",
          fluidRow( title="Predicting profit",
            box(
              sliderInput("quan",
                          "Quantity:",
                          min = 900,
                          max = 6000,
                          value = 300),
              sliderInput("price",
                          "Price:",
                          min = 1000,
                          max = 5000,
                          value = 300),
              sliderInput("cost",
                          "Cost:",
                          min = 1000,
                          max = 5000,
                          value = 300)
            )
          ),
          fluidRow(title="Predicting Profit",
            plotOutput("prediction3", height = "10px",click="plot_click"),verbatimTextOutput("hover31"),verbatimTextOutput("hover32"),verbatimTextOutput("hover33")
          )
          
          )
)
)

#Sidebar content of the dashboard

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Wholesaler_Dashboard") 
#completing the ui part with dashboardPage
server <- function(input, output) {
  
  output$prediction2 <-renderPlot({ #This shows sales/profit for each class
    datay=data
      Revenue <- datay %>% group_by(Class_Name) %>% summarise(value = sum(Total_Revenue)) 
      rev=c(Revenue$value)
      barplot(rev/10000,
              main = "Revenue by class",
              xlab = "class",
              ylab = "Revenue",
              names.arg = Revenue$Class_Name,
            col = "darkred",ylim=c(0,10000))
    
  })
  output$hover2 <- renderText({
    paste0("VALUE = ", (input$plot_click$y)*10000)
  })
  output$prediction1<-renderPlot({  #This shows percentage of sales/profit for each season
    datay=data
    if(input$p=='b')
    {
      datad=filter(datay,Department_Name=="Bottoms")
    }
    if(input$p=='d')
    {
      datad=filter(datay,Department_Name=="Dresses")
    }
   
    if(input$p=='j')
    {
      datad=filter(datay,Department_Name=="Jackets")
    }
    if(input$p=='o')
    {
      datad=filter(datay,Department_Name=="Tops")
    }
    if(input$p=='a')
    {
      datad=datay
    }
      Revenue <- datad %>% group_by(Season) %>% summarise(value = sum(Total_Revenue))
      rev=c(Revenue$value)
      total<-c((rev/sum(rev))*100)
      piepercent<- round(100*rev/sum(rev), 1)
      barplot(piepercent,
              main = "Percentage of revenue earned during seasons",
              xlab = "Season",
              ylab = "Percentage of Revenue",
              names.arg = Revenue$Season,
              col = "purple",xlim=c(0,5),ylim=c(0,100))
   
     
  })
  
  output$hover1 <- renderText({ #This will show the y-coordinate at clicked location
    paste0("VALUE = ", input$plot_click$y)
  })
  output$table1<-renderFormattable({ #Quantity sold on various platforms
    if(input$r=='b')
    {
      datay=filter(data,Year=='2010')
    }
    if(input$r=='c')
    {
      datay=filter(data,Year=='2011')
    }
    if(input$r=='d')
    {
      datay=filter(data,Year=='2012')
    }
    if(input$r=='e')
    {
      datay=filter(data,Year=='2013')
    }
    if(input$r=='f')
    {
      datay=filter(data,Year=='2014')
    }
    if(input$r=='g')
    {
      datay=filter(data,Year=='2015')
    }
    if(input$r=='h')
    {
      datay=filter(data,Year=='2016')
    }
    if(input$r=='i')
    {
      datay=filter(data,Year=='2017')
    }
    if(input$r=='a')
    {
      datay=data
    }
    quan<-datay %>% group_by(Class_Name,Platform)  %>% summarise(Quantity=sum(Units_Sold))
    #print(quan)
    
    formattable(quan, align =c("l","l","c"), list(
      'Class' = formatter("span",
                          style = ~ style(color = "gray")),
      'Quantity'= formatter("span", style = x ~ style(color = ifelse(x< 1000, "red", "green")),
                            x ~ icontext(ifelse(x > 1000,"arrow-up", "arrow-down"), x)
      )
    ))
  })
  
  
  #value box for profit
  output$profitBox2<-renderValueBox({
    datay=data
    pr<-datay %>% group_by(Platform) %>% summarise(value=sum(Total_Profit))
    prsort<-pr[order(-pr$value),]
    r=c(prsort$value)
    piepercent<- round(100*r[1]/sum(r), 1)
    
    valueBox(
      piepercent,"Percentage of highest Profit", icon = icon("credit-card"),
      color = "purple"
    )
  })
  output$salesBox2<-renderValueBox({
    datay=data
    pr<-datay %>% group_by(Platform) %>% summarise(value=sum(Total_Profit))
    prsort<-pr[order(-pr$value),]
    r=c(prsort$value)
    piepercent<- round(100*r[1]/sum(r), 1)
    
    valueBox(
      prsort[1,1], "Grossed on",icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow")
  })
  output$prodBox2<-renderValueBox({
    datay=data
    sr<-datay %>% group_by(Platform,Department_Name) %>% summarise(value=sum(Total_Profit))
    srsort<-sr[order(-sr$value),]
    valueBox(
      srsort[1,2], "Department grossing highest profit",icon = icon("shopping-cart", lib = "glyphicon"),
      color = "green"
    )
  })
  output$prediction5<-renderPlot({  #This shows sales/profit on each platform
    datay=data
    if(input$p=='b')
    {
      datad=filter(datay,Department_Name=="Bottoms")
    }
    if(input$p=='d')
    {
      datad=filter(datay,Department_Name=="Dresses")
    }
    if(input$p=='j')
    {
      datad=filter(datay,Department_Name=="Jackets")
    }
    if(input$p=='o')
    {
      datad=filter(datay,Department_Name=="Tops")
    }
    if(input$p=='a')
    {
      datad=datay
    }
    
      revenue <- datad %>% group_by(Platform) %>% summarise(value=sum(Total_Revenue))
      Revenue=c(revenue$value)
      Revenue=Revenue/10000
      Platform_Name=c('Amazon',"Flipkart",'Snapdeal')
      BOD<-data.frame('Platform'=Platform_Name,'Revenue'=Revenue)
      bp<-ggplot(BOD, aes(x=Platform_Name, y=Revenue,group=1)) + geom_line()+geom_point()
      bp+ggtitle("Revenue by Platform")+theme(plot.title = element_text(lineheight=1.0, face="bold"))
    
    
  })
  output$hover5 <- renderText({
    paste0("VALUE = ", (input$plot_click$y)*10000)
  })
  output$prediction3<-renderPlot({
    # data3<-read.csv("C:\\Users\\Shweta\\Desktop\\FinalyearProject\\Final\\Final_Wholesalerexcel.csv");
    # ID=20695
    # data=filter(data3,Wholesaler_id==ID)
    lmsales<-lm((Total_Profit/10000)~Units_Sold+Unit_Price+Unit_Cost+Year, data=data)
    print(summary(lmsales))
    #plot(data2, pch = 16, col = "blue") 
    # scatter.smooth(x=data$Year, y=(data$Total_Profit)/10000, main="Predicting profit with Year",
    #                xlab = "Year", ylab = "Profit")
    quan=input$quan
    price=input$price
    cost=input$cost
    year=2019
    df1=data.frame(Units_Sold=quan,Unit_Price=price,Unit_Cost=cost,Year=year)
    pre1=predict(lmsales, data.frame(Units_Sold=quan,Unit_Price=price,Unit_Cost=cost,Year=year))
    #pre1=predict(lmsales, data.frame(Units_Sold=4139,Unit_Price=154.06,Unit_Cost=90.93,Year=2017))
    print(pre1)
    
  })
  output$hover31 <- renderText({
    paste0("Quantity  ","Price  ","Cost  ","Year  ")
  })
  output$hover32 <- renderText({
    quan=input$quan
    price=input$price
    cost=input$cost
    year=2019
    df1=data.frame(Units_Sold=quan,Unit_Price=price,Unit_Cost=cost,Year=year)
    paste0(df1,"      ")
  })
  output$hover33 <- renderText({
    lmsales<-lm((Total_Profit/10000)~Units_Sold+Unit_Price+Unit_Cost+Year, data=data)
    quan=input$quan
    price=input$price
    cost=input$cost
    year=2019
    df1=data.frame(Units_Sold=quan,Unit_Price=price,Unit_Cost=cost,Year=year)
    pre1=predict(lmsales, data.frame(Units_Sold=quan,Unit_Price=price,Unit_Cost=cost,Year=year))
    #plot(lmsales)
    if(price>cost)
    {
      paste0("Predicted Profit: ",pre1*10000)
    }
    else
    {
      paste0("Cost is greater than price.Prediction not possible")
    }
  })
  output$prediction6<-renderPlot({  #This shows sales/profit for each year
    datay=data
    if(input$p=='b')
    {
      datad=filter(datay,Department_Name=="Bottoms")
    }
    if(input$p=='d')
    {
      datad=filter(datay,Department_Name=="Dresses")
    }
    if(input$p=='j')
    {
      datad=filter(datay,Department_Name=="Jackets")
    }
    if(input$p=='o')
    {
      datad=filter(datay,Department_Name=="Tops")
    }
    if(input$p=='a')
    {
      datad=datay
    }
      Revenue <- datad %>% group_by(Year) %>% summarise(value = sum(Total_Revenue))
      rev=c(Revenue$value)
      piepercent<- round(100*rev/sum(rev), 1)
      year=c('2010','2011','2012','2013','2014','2015','2016','2017')
      # pie(piepercent, labels = piepercent,col=rainbow(9),main="Revenue by year")
      # legend("bottomright",c('2010','2011','2012','2013','2014','2015','2016','2017'), cex = 1.2,
      #        fill = rainbow(length(rev)))
      barplot(piepercent,
              main = "Percentage of revenue by year",
              xlab = "Year",
              ylab = "Percentage of Revenue",
              names.arg =year,
              col = "green",ylim=c(0,100))
    
  })
  output$hover6 <- renderText({
    paste0("VALUE = ", input$plot_click$y)
  })
  output$table2<-renderFormattable({
   
    {
      datay=filter(data,Year=='2011')
    }
    if(input$r=='d')
    {
      datay=filter(data,Year=='2012')
    }
    if(input$r=='e')
    {
      datay=filter(data,Year=='2013')
    }
    if(input$r=='f')
    {
      datay=filter(data,Year=='2014')
    }
    if(input$r=='g')
    {
      datay=filter(data,Year=='2015')
    }
    if(input$r=='h')
    {
      datay=filter(data,Year=='2016')
    }
    if(input$r=='i')
    {
      datay=filter(data,Year=='2017')
    }
    if(input$r=='a')
    {
      datay=data
    } 
    pname<-datay %>% group_by(Class_Name,Platform) %>% summarise(Profit = sum(Total_Profit))
    formattable(pname, align =c("l","l","c"), list(
      'Class' = formatter("span",
                          style = ~ style(color = "gray")),
      'Profit'= formatter("span", style = x ~ style(color = ifelse(x< 10000, "red", "green")),
                          x ~ icontext(ifelse(x > 10000,"arrow-up", "arrow-down"), x)
      )
    ))
  })
}
ui <- dashboardPage(title = "WELCOME", header, sidebar, body, skin='red')
shinyApp(ui, server)