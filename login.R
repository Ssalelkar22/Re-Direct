library(shiny)
library(shinydashboard)
library(gWidgetstcltk)
library(shinyjs)

      ui <- fluidPage(
        #htmlTemplate("login1.html"),
        
        tags$head(
          tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        padding-top: 70px;
        font-size:50px;
        color: orange;
      }
      
      h2 {
        font-family: 'Times New Roman';
                          font-weight: 500;
                          line-height: 1.1;
                          
                          color:maroon;
                          }

      .jumbotron{
      background-image:url('user.jpg');  
      color: white;
      padding-top:140px;
      background-size:cover;
      }

    
     

    "))
        ), headerPanel("Re-Decide"),
        tags$body(
          tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
          
          body {
          background-image: url('back.png');
          }
                  "))),
        
        tags$div(class = "jumbotron", checked = NA),
     
       sidebarPanel(title="Login",
      textInput("username",h2("Username"),value=""),
      textInput("password",h2("Password"),value=""),
      actionButton("button", "Submit"),color="maroon"
      ),
     
     
      verbatimTextOutput("value")
      
     ) 
    
    server <-function(input,output,session)
    { 
      abc<- eventReactive(input$button,{
      my_data<-read.csv("d:\\Users\\Ritika Desai\\Documents\\FYproject\\Login.csv")
      c=0
      for(i in 1:2723)
      {
        if(input$username == my_data$username[i])
        {
          if(input$password == my_data$password[i])
          {
            print("Correct")
            c=c+1
            break()
          }
         
        }
      }
      if(c==0)
      {
        print("Incorrect. Please try again")
      }
    })
      output$value <- renderText({abc()})
      
    }
    shinyApp(ui,server)
  




    