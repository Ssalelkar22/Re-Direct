library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)

shinyServer(function(input, output){
    
    # output$msgOutput <- renderMenu({
    #    msgs <- apply(read.csv("messages.csv"),1,function(row){
    #        messageItem(from = row[["from"]],message = row[["message"]])
    #    })
    #    dropdownMenu(type = "messages", .list = msgs)
    # })
    
    
#yearly sales graph
    output$graph <- renderPlotly({
        if(input$pw=="Amazon"){
            cons <- read.csv("Final_Consumer.csv")
            consumer <- filter(cons,Platform == "Amazon")
            ag <- aggregate(consumer$quantity, by=list(consumer$year),FUN=sum)
            plot_ly(ag, x = ~Group.1, y = ~x, type = 'scatter', mode = 'lines+markers')%>%
                layout(xaxis = list(title = "Year"),yaxis = list (title = "Quantity"))
        }
        else if(input$pw=="Flipkart"){
            cons <- read.csv("Final_Consumer.csv")
            consumer <- filter(cons,Platform == "Flipkart")
            ag <- aggregate(consumer$quantity, by=list(consumer$year),FUN=sum)
            plot_ly(ag, x = ~Group.1, y = ~x, type = 'scatter', mode = 'lines+markers',marker = list(color = 'rgb(200, 200, 0)'),line = list(color = 'rgb(240, 240, 0)'))%>%
                layout(xaxis = list(title = "Year"),yaxis = list (title = "Quantity"))
        }
        else if(input$pw=="Snapdeal"){
            cons <- read.csv("Final_Consumer.csv")
            consumer <- filter(cons,Platform == "Snapdeal")
            ag <- aggregate(consumer$quantity, by=list(consumer$year),FUN=sum)
            plot_ly(ag, x = ~Group.1, y = ~x, type = 'scatter', mode = 'lines+markers',marker = list(color = 'rgb(205, 12, 24)'),line = list(color = 'rgb(205, 12, 24)'))%>%
                layout(xaxis = list(title = "Year"),yaxis = list (title = "Quantity"))
        }
    })
    
    
#min/max retailers
    output$graph2 <- renderPlot({
        ret <- read.csv("Final_Wholesalerexcel.csv")
        retailer <- filter(ret,Platform == "Amazon")
        ag <- aggregate(retailer$Units_Sold, by=list(retailer$Wholesaler_id),FUN=sum)
        if(input$mx>0){
            ag <- ag[order(ag$x,decreasing = T),]
            ag <- head(ag,input$mx)
            ggplot(ag,aes(factor(Group.1,levels=unique(Group.1)),x/1000, group=1))+geom_line() +geom_point()+geom_text(aes(label=x),hjust=0.5, vjust=-0.25)+ ylab("Units sold(Thousands)") + 
             xlab("Wholesaler ID")
        }
        else if(input$mx<0){
            ag <- ag[order(ag$x,decreasing = F),]
            ag <- head(ag,-input$mx)
            ggplot(ag,aes(factor(Group.1,levels=unique(Group.1)),x/1000, group=1))+geom_line()+geom_text(aes(label=x),hjust=0.5, vjust=1.25) +geom_point()+ ylab("Units sold(Thousands)") + 
                xlab("Wholesaler ID")
        }
        else{}
    })
    

#profits on items    
    output$graph3 <- renderPlotly({
         ret <- read.csv("Final_Wholesalerexcel.csv")
         retailer <- filter(ret,Platform == "Amazon")
         if(input$ch=="Bottoms"){
             dept <- filter(retailer,Department_Name == "Bottoms")
             ag <- aggregate(dept$Total_Profit, by=list(dept$Class_Name),FUN=sum)
             plot_ly(ag, labels= ~Group.1, values= ~x, type = 'pie', textposition= 'inside', textinfo= 'label+percent',
                     insidetextfont= list(color= '#FFFFFF'), hoverinfo= 'text', text= ~paste('$',x))  %>%
                     layout(title= "Profits on Bottoms", xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
         }
         else if(input$ch=="Dresses"){
             dept <- filter(retailer,Department_Name == "Dresses")
             ag <- aggregate(dept$Total_Profit, by=list(dept$Class_Name),FUN=sum)
             plot_ly(ag, labels= ~Group.1, values= ~x, type = 'pie', textposition= 'inside', textinfo= 'label+percent',
                     insidetextfont= list(color= '#FFFFFF'), hoverinfo= 'text', text= ~paste('$',x))  %>%
                 layout(title= "Profits on Dresses", xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
         }
         else if(input$ch=="Intimates"){
             dept <- filter(retailer,Department_Name == "Intimates")
             ag <- aggregate(dept$Total_Profit, by=list(dept$Class_Name),FUN=sum)
             plot_ly(ag, labels= ~Group.1, values= ~x, type = 'pie', textposition= 'inside', textinfo= 'label+percent',
                     insidetextfont= list(color= '#FFFFFF'), hoverinfo= 'text', text= ~paste('$',x))  %>%
                 layout(title= "Profits on Intimates", xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
         }
         else if(input$ch=="Jackets"){
             dept <- filter(retailer,Department_Name == "Jackets")
             ag <- aggregate(dept$Total_Profit, by=list(dept$Class_Name),FUN=sum)
             plot_ly(ag, labels= ~Group.1, values= ~x, type = 'pie', textposition= 'inside', textinfo= 'label+percent',
                     insidetextfont= list(color= '#FFFFFF'), hoverinfo= 'text', text= ~paste('$',x))  %>%
                 layout(title= "Profits on Jackets", xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
         }
         else if(input$ch=="Tops"){
             dept <- filter(retailer,Department_Name == "Tops")
             ag <- aggregate(dept$Total_Profit, by=list(dept$Class_Name),FUN=sum)
             plot_ly(ag, labels= ~Group.1, values= ~x, type = 'pie', textposition= 'inside', textinfo= 'label+percent',
                     insidetextfont= list(color= '#FFFFFF'), hoverinfo= 'text', text= ~paste('$',x))  %>%
                 layout(title= "Profits on Tops", xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
         }
             
    })
 
    
#customer locations
    output$graph4 <- renderPlotly({
        cons <- read.csv("Login.csv")
        consumer <- filter(cons,identity == "Consumer")
        ag <- aggregate(consumer$ID, by=list(consumer$city),FUN=length)
        plot_ly(ag, labels= ~Group.1, values= ~x, type = 'pie', textposition= 'inside', textinfo= 'label+percent',
                insidetextfont= list(color= '#FFFFFF'), hoverinfo= 'text', text= ~paste(x))  %>%
                layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
#sales comparisons across all platforms
    output$graph5 <- renderPlotly({
        cons <- read.csv("Final_Consumer.csv")
        consumer1 <- filter(cons,Platform == "Amazon")
        ag1 <- aggregate(consumer1$quantity, by=list(consumer1$year),FUN=sum)
        consumer2 <- filter(cons,Platform == "Flipkart")
        ag2 <- aggregate(consumer2$quantity, by=list(consumer2$year),FUN=sum)
        consumer3 <- filter(cons,Platform == "Snapdeal")
        ag3 <- aggregate(consumer3$quantity, by=list(consumer3$year),FUN=sum)
        plot_ly(ag1, x = ~Group.1, y = ~x, name = 'Amazon', type = 'scatter', mode = 'lines+markers', width = 900, height = 450)%>%
            add_trace(y = ~ag2$x, type = 'scatter', name = 'Flipkart',  mode = 'lines+markers',marker = list(color = 'rgb(200, 200, 0)'),line = list(color = 'rgb(240, 240, 0)'))%>%
            add_trace(y = ~ag3$x, type = 'scatter', name = 'Snapdeal', mode = 'lines+markers',marker = list(color = 'rgb(205, 12, 24)'),line = list(color = 'rgb(205, 12, 24)'))%>%
            layout(xaxis = list(title = "Year"),yaxis = list (title = "Quantity"))
    })
})