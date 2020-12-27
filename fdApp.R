library(shiny)
library(tidyverse)
library(readxl)
library(plyr)
library(scales)
library(dplyr)
library(tm)
library(wordcloud)
library(memoise)
library(sjmisc)
library(shinydashboard)
library(DT)
library(formatR)
library(caret)
library(e1071)



fd_clean <- read.csv("fd_clean.csv")



majors <- data.frame(table(fd_clean$Major))



dm.index <- str_which(str_detect(majors$Var1, ","), "TRUE")

majors$Var1 <- as.character(majors$Var1)

majors[dm.index,]$Var1 <- "Double Major"

majors <- majors %>% filter(Var1 != "Double Major")

a <- data.frame(Var1 = "Double Major", Freq = 27)

majors <- rbind(majors, a)


##removing some datasets...


##doing some knn stuff!


temp<-ifelse(fd_clean$EmploymentStatus == "Seeking", "No", "Yes")

fd_knn <- cbind(fd_clean, temp)


fd_knn$EmploymentStatus <- factor(fd_knn$temp, levels = c("No", "Yes"))

fd_knn <- fd_knn %>% select(EmploymentStatus, FinAid, ApplicationCount, CareerCounselingVisits, InfoSessionCounts, ProgramParticipationCounts)

##cleaned data now...
set.seed(3446) 
training_rows <- caret::createDataPartition(
  fd_knn$EmploymentStatus,       # class labels
  p = 0.75,        # proportion for training
  list = FALSE     # return matrix if FALSE
)
training_set <- fd_knn %>% slice(training_rows)
test_set <- fd_knn %>% slice(-training_rows)




scale_transformer <- preProcess(training_set, method = c("center", "scale")) 
training_set <- predict(scale_transformer, training_set)
test_set <- predict(scale_transformer, test_set)



train_up <- upSample(x = training_set[,2:6],
                     y = training_set$EmploymentStatus)



train_up <- plyr::rename(train_up,  c(Class = 'EmploymentStatus'))



ks <- data.frame(k = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21))
train_control <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, classProbs = TRUE)
set.seed(4521)
choose_k2 <- train(
  form = EmploymentStatus ~ .,
  data = train_up,
  method = "knn", 
  tuneGrid = ks, 
  trControl = train_control, 
  metric = "Sens"
)
ggplot(choose_k2)


test_features <- test_set %>%
  data.frame()
test_predictions <- predict(choose_k2, newdata = test_features)
test_class <- test_set %>% pull(EmploymentStatus) %>% as.factor()
model_quality <-confusionMatrix(
  data = test_predictions,
  reference = test_class
)
model_quality





ui <- dashboardPage(
  dashboardHeader(title = "First Destinations Dashboard"),
  
  
  
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Exploratory Info ",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem("Data", tabName = "Data", icon = icon("th")),
    menuItem("Variable Distributions", tabName = "Distributions", icon = icon("fas fa-signal")),
    menuItem("Barplot Creator", tabName = "barplot", icon = icon("fas fa-binoculars")),
    menuItem("Employment Predictor", tabName = "knn", icon = icon("robot"))
  )),
  
  
  
  
  
  dashboardBody(tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            # Boxes need to be put in a row (or column)
            fluidRow(
              box(title = "Wordcloud of Class of '19 Majors",
                  plotOutput("plot1", height = 400)
                  
                  ),
              
              box(
                title = "About this Dashboard",
                width = 4,
                background = "light-blue",
                p(
                  "This dashboard was created for users to explore student-level data on first destinations for Carleton's class of '19.
          The data consists of 291 (approximatley 63%) students from the class of 2019.", br(), "One of the variables of interest, emplyoment status, was mostly self reported. Consequently, it is likley this data does not entirley reflect the true employment outcomes
          for the class of '19. However, the data still likley refelcts some overarching trends."
                )
              )
            ),
            fluidRow(
              box(title = "What's Here?",
                  width = 6, 
                  background = "light-blue",
                  HTML(
            '        <html>
                      <body>
                      <h6>To the right you can find tabs to</h6>
                      
                      <ul>
                      <li>Filter the data</li>
                      <li>Visualize distributions of varaibles in the data</li>
                      <li>A barplot creator given certain variables and groupings you pick</li>
                      <li>A classifcation algorithm that takes user inputs and predicts their emplyoment status</li>
                      </ul>  
                      
                      </body>
                      </html>
                      
            '
                    
                  )
                  
                  )
              
            )
            
            
            
            ),
    
    
    
    # Second tab content
    tabItem(
      tabName = "Data",
      h2("Class of '19 First Destinations Data"),
      DT::dataTableOutput("fdclean"),
      checkboxGroupInput(
        "show_vars",
        "Columns in First Destination data to show:",
        names(fd_clean),
        selected = names(fd_clean)[c(4, 6, 12:16)]
      )
    ),
    
    #third tab content
    tabItem(
      tabName = "Distributions",
      fluidRow(
        box(title = "Distribution Graph Creator",
            width = 12,
            selectInput(inputId = "varChoosen",
                        label = "Choose a Variable", 
                        selected = colnames(fd_clean)[3],
                        choices = colnames(fd_clean)[c(3,13:16)])
            ),
        box(
          width = 6,
          plotOutput("plot2", height = 250)
        ),
        box(
         width = 6,
         plotOutput("plot3", height = 250)
        )
      )
      
      
    ),
    
    #fourth tab content
    
    tabItem(
      tabName = "barplot",
      fluidRow(
        box(
          width = 6,
          selectInput(inputId = "vars",
                      label = "Choose a Variable",
                      selected = colnames(fd_clean)[3],
                      choices = colnames(fd_clean)
                      )
        ),
        
        box(
          width = 6,
          selectInput(inputId = "group",
                      label = "Choose a grouping Variable",
                      selected = colnames(fd_clean)[12],
                      choices = colnames(fd_clean)
                        )
          
        ),
        
        
        fluidRow(
          box(
            width = 6,
            plotOutput("plot4", height = 250)
            
          )
          
          
          
        )
        
      )
      
      
      
      
    ),
    
    
    tabItem(
      tabName = "knn",
      fluidRow(
      box(title = "Input values for the below fields",
        width = 6,
        numericInput(inputId = "Apps", label = "# of Applications", value = 0, min= 0, max = 100, step = 1),
        numericInput(inputId = "Info_Sessions", label = "# of Info Sessions Attened", value = 0, min= 0, max = 100, step = 1),
        numericInput(inputId = "Counseling_Visits", label = "# of Career Counseling Appointments", value = 0, min= 0, max = 100, step = 1),
        numericInput(inputId = "Financial_Aid", label = "Financial Aid Level", value = 0, min= 0, max = 5, step = 1),
        numericInput(inputId = "Program_Participatoin", label = "# Of Programs Participated In", value = 0, min= 0, max = 100, step = 1)
        
        
      ),
      
      box(
        width = 6,
        background = "light-blue",
        p("This tab allows the user to input data to be run through k-nearest neighbors (knn) algorithm. This algorithm is a simple supervised machine learning algorithm that can solve classification problems.", br(), "In our case, we are trying to predict if a student is seeking employment or not (because they are employed, volunteering, or in graduate school)."
          )
      ),
      ),
      
      fluidRow(
      box(
        width = 6,
        background = "light-blue",
        p("If the follow text is 'Yes', then the algorithm predicts that a student with the above attributes would have a job directly after graduation!"),
        textOutput(outputId = "prediction")
      )
      )

      
      
    )
    
    
  ))
)

server <- function(input, output) {
  
  wordcloud_rep <- repeatable(wordcloud)
  
 
  output$plot1 <- renderPlot({
    wordcloud_rep(majors$Var1, majors$Freq, scale=c(3,0.5),
                  min.freq = 0, max.words=Inf,
                  colors=brewer.pal(8, "Dark2"))
  })
  # sorted columns are colored now because CSS are attached to them
  output$fdclean <- DT::renderDataTable({
    DT::datatable(fd_clean[, input$show_vars, drop = FALSE], options = list(orderClasses = TRUE))
  })
  
  output$plot2 <- renderPlot({
    fd_clean %>%
      filter(input$varChoosen >= 0) %>%
      ggplot(., aes_string(input$varChoosen)) +
      geom_density(alpha = 0.6, fill = "#00BFC4", color = "#F8766D") +
      theme_minimal()
    
  })
  
  output$plot3 <- renderPlot({
    
    fd_clean %>%
      filter(input$varChoosen >= 0) %>%
      ggplot(., aes_string(y =input$varChoosen)) +
      geom_boxplot(alpha = 0.6, fill = "#00BFC4", outlier.colour = "#F8766D", outlier.size = 3, outlier.alpha = 1) +
      theme_minimal() +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  })
  
  
  plot_data <- reactive({
    
    data_to_plot<-fd_clean %>% 
                        group_by_(input$group,input$vars) %>% 
                        srvyr::summarize(n=n()) %>% 
                        group_by_(input$group) %>% 
                        mutate(percentage=100*n/sum(n))
                      
    
  })
  
  

  
  output$plot4 <- renderPlot({
    
    
     
      ggplot(plot_data(), aes_string(x = input$vars, y = "percentage")) + 
        facet_grid(.~get(input$group)) +
        geom_bar(alpha = 0.6, fill = "#00BFC4", color = "#F8766D", stat = "identity") + 
        theme_minimal()
      
    
    
  })
  
  
  
  output$prediction <- renderText({
    
    tibble(
      
      
      FinAid = input$Financial_Aid,
      ApplicationCount = input$Apps,
      CareerCounselingVisits = input$Counseling_Visits,
      InfoSessionCounts = input$Info_Sessions,
      ProgramParticipationCounts = input$Program_Participatoin) %>%
      
      
      predict(choose_k2, newdata = .) %>%
      as.character()
      

    
  })
  
  
}

shinyApp(ui, server)



