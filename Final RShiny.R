# Load required libraries
library(caTools)
library(shinydashboard)
library(rsconnect)
library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(caret)
library(dplyr)
library(randomForest)
library(DT)
library(gridExtra)

# Importing the new dataset
set.seed(123)
df <- read.csv("heart_statlog_cleveland_hungary_final.csv")

# Renaming columns to match existing code
colnames(df) <- c('age', 'sex', 'chest_pain_type', 'resting_bp_s', 'cholesterol', 
                  'fasting_blood_sugar', 'resting_ecg', 'max_heart_rate', 
                  'exercise_angina', 'oldpeak', 'ST_slope', 'target')

# Data Pre-processing
df[df == '?'] <- NA
df <- transform(
  df,
  age = as.integer(age),
  sex = factor(as.numeric(sex), levels = c(0, 1)),
  chest_pain_type = factor(as.numeric(chest_pain_type), levels = c(1, 2, 3, 4)),
  resting_bp_s = as.integer(resting_bp_s),
  cholesterol = as.integer(cholesterol),
  fasting_blood_sugar = factor(as.numeric(fasting_blood_sugar), levels = c(0, 1)),
  resting_ecg = factor(as.numeric(resting_ecg), levels = c(0, 1, 2)),
  max_heart_rate = as.integer(max_heart_rate),
  exercise_angina = factor(as.numeric(exercise_angina), levels = c(0, 1)),
  oldpeak = as.numeric(oldpeak),
  ST_slope = factor(as.numeric(ST_slope), levels = c(1, 2, 3)),
  target = factor(as.numeric(target), levels = c(0, 1))
)

# Dropping rows with nulls
df <- na.omit(df)

# Prepare the data
X <- df %>% select(-target)
y <- df$target

# Split the data
index <- createDataPartition(y, p=0.8, list=FALSE)
train <- df[index,]
test <- df[-index,]

# Standardize the data using caret's preProcess
preProcValues <- preProcess(train, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, train)
testTransformed <- predict(preProcValues, test)

# Convert target variable to a factor
trainTransformed$target <- factor(trainTransformed$target, levels = c(0, 1))
testTransformed$target <- factor(testTransformed$target, levels = c(0, 1))

# Model training
myModel1 <- randomForest(target ~ ., data=trainTransformed, ntree=100)

# Evaluate the model
pred_rf <- predict(myModel1, testTransformed)
confusionMatrix(pred_rf, testTransformed$target)

# UI definition
ui <- dashboardPage(
  title = 'Application for Heart Disease Classification',
  header = dashboardHeader(title = "Heart Disease Classification"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Attributes and Description", tabName = "AttributesDesc", icon = icon("info-circle")),
      menuItem("Example Data", tabName = "ExampleData", icon = icon("table")),
      menuItem("Descriptive Statistics", tabName = "DescStats", icon = icon("list-alt")),
      menuItem("Analysis", tabName = "Analysis", icon = icon("chart-pie")),
      menuItem("Prediction", tabName = "Prediction", icon = icon("bullseye")),
      menuItem("About", tabName = "About", icon = icon("info"))
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "AttributesDesc",
              fluidPage(
                titlePanel("Heart Disease"),
                mainPanel(
                  tags$div(
                    style = "text-align: justify; padding: 20px;",
                    p("Heart disease is a general term used to describe conditions that impair the heart, such as coronary artery disease, heart failure, arrhythmias, and congenital heart disease. The primary cause is atherosclerosis, which involves plaque buildup in the arteries that can obstruct blood flow. Risk factors include high blood pressure, high cholesterol, diabetes, obesity, unhealthy lifestyle choices (lack of physical activity, poor diet, smoking, excessive alcohol consumption), and family history of heart disease."),
                    tags$img(src = "https://storage.googleapis.com/kaggle-datasets-images/4755824/8062170/a089b7a9e38de5d2369ea274dcb2533d/dataset-cover.jpg?t=2024-04-08-09-44-58", width = "100%"),
                    tags$br(),
                    tags$a(href = "https://www.kaggle.com/datasets/mexwell/heart-disease-dataset", "View Kaggle Dataset", target = "_blank"),
                    tags$br(),
                    tags$div(
                      h3("Heart Disease Dataset Attributes:"),
                      tags$table(
                        class = "table table-bordered",
                        style = "width: 100%; text-align: left; margin-top: 10px;",
                        tags$tr(
                          tags$th("No."), tags$th("Attribute"), tags$th("Code"), tags$th("Unit"), tags$th("Data Type")
                        ),
                        tags$tr(
                          tags$td("1"), tags$td("Age"), tags$td("age"), tags$td("years"), tags$td("Numeric")
                        ),
                        tags$tr(
                          tags$td("2"), tags$td("Sex"), tags$td("sex"), tags$td("1, 0"), tags$td("Binary")
                        ),
                        tags$tr(
                          tags$td("3"), tags$td("Chest Pain Type"), tags$td("chest pain type"), tags$td("1, 2, 3, 4"), tags$td("Nominal")
                        ),
                        tags$tr(
                          tags$td("4"), tags$td("Resting Blood Pressure"), tags$td("resting bp s"), tags$td("mm Hg"), tags$td("Numeric")
                        ),
                        tags$tr(
                          tags$td("5"), tags$td("Serum Cholesterol"), tags$td("cholesterol"), tags$td("mm/dl"), tags$td("Numeric")
                        ),
                        tags$tr(
                          tags$td("6"), tags$td("Fasting Blood Sugar"), tags$td("fasting blood sugar"), tags$td("> 120 mg/dl (1 = true; 0 = false)"), tags$td("Binary")
                        ),
                        tags$tr(
                          tags$td("7"), tags$td("Resting Electrocardiographic Results"), tags$td("resting ecg"), tags$td("0, 1, 2"), tags$td("Nominal")
                        ),
                        tags$tr(
                          tags$td("8"), tags$td("Maximum Heart Rate Achieved"), tags$td("max heart rate"), tags$td("bpm"), tags$td("Numeric")
                        ),
                        tags$tr(
                          tags$td("9"), tags$td("Exercise Induced Angina"), tags$td("exercise angina"), tags$td("1 = yes, 0 = no"), tags$td("Binary")
                        ),
                        tags$tr(
                          tags$td("10"), tags$td("Oldpeak"), tags$td("oldpeak"), tags$td("depression"), tags$td("Numeric")
                        ),
                        tags$tr(
                          tags$td("11"), tags$td("The Slope of the Peak Exercise ST Segment"), tags$td("ST slope"), tags$td("1, 2, 3"), tags$td("Nominal")
                        ),
                        tags$tr(
                          tags$td("12"), tags$td("Class"), tags$td("target"), tags$td("0, 1"), tags$td("Binary")
                        )
                      ),
                      h3("Description of Nominal Attributes:"),
                      tags$table(
                        class = "table table-bordered",
                        style = "width: 100%; text-align: left; margin-top: 10px;",
                        tags$tr(
                          tags$th("Attribute"), tags$th("Description")
                        ),
                        tags$tr(
                          tags$td("Sex"), tags$td("1 = Male, 0 = Female")
                        ),
                        tags$tr(
                          tags$td("Chest Pain Type"), tags$td("1: Typical Angina, 2: Atypical Angina, 3: Non-anginal Pain, 4: Asymptomatic")
                        ),
                        tags$tr(
                          tags$td("Fasting Blood Sugar"), tags$td("Fasting Blood Sugar > 120 mg/dl (1 = True, 0 = False)")
                        ),
                        tags$tr(
                          tags$td("Resting Electrocardiogram Results"), tags$td("0: Normal, 1: ST-T wave Abnormality, 2: Probable or definite Left Ventricular Hypertrophy")
                        ),
                        tags$tr(
                          tags$td("Exercise Induced Angina"), tags$td("1 : Yes, 0 : No")
                        ),
                        tags$tr(
                          tags$td("The Slope of the Peak Exercise ST Segment"), tags$td("1 : Upsloping, 2 : Flat, 3 : Downsloping")
                        ),
                        tags$tr(
                          tags$td("Class"), tags$td("1 : Heart Disease, 0 : Normal")
                        )
                      )
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "ExampleData",
              fluidPage(
                titlePanel("Example Data"),
                mainPanel(
                  DTOutput("exampleData")
                )
              )
      ),
      tabItem(tabName = "DescStats",
              fluidPage(
                titlePanel("Descriptive Statistics"),
                mainPanel(
                  verbatimTextOutput("descStats")
                )
              )
      ),
      tabItem(tabName = "Analysis",
              fluidPage(
                titlePanel("Analysis"),
                fluidRow(
                  box(title = "Analysis Plot 1", status = "primary", solidHeader = TRUE, 
                      plotOutput("analysisPlot1")),
                  box(title = "Analysis Plot 2", status = "primary", solidHeader = TRUE, 
                      plotOutput("analysisPlot2"))
                ),
                fluidRow(
                  box(title = "Analysis Plot 3", status = "primary", solidHeader = TRUE, 
                      plotOutput("analysisPlot3")),
                  box(title = "Analysis Plot 4", status = "primary", solidHeader = TRUE, 
                      plotOutput("analysisPlot4"))
                ),
                fluidRow(
                  box(title = "Analysis Plot 5", status = "primary", solidHeader = TRUE, 
                      plotOutput("analysisPlot5")),
                  box(title = "Analysis Plot 6", status = "primary", solidHeader = TRUE, 
                      plotOutput("analysisPlot6"))
                )
              )
      ),
      tabItem(tabName = "Prediction",
              fluidPage(
                titlePanel("Heart Disease Prediction"),
                sidebarLayout(
                  sidebarPanel(
                    numericInput("age", "Age:", 30, min = 1, max = 120),
                    selectInput("sex", "Sex:", c("Male" = 1, "Female" = 0)),
                    selectInput("chest_pain_type", "Chest Pain Type:", c("Typical Angina" = 1, "Atypical Angina" = 2, "Non-anginal Pain" = 3, "Asymptomatic" = 4)),
                    numericInput("resting_bp_s", "Resting Blood Pressure:", 120, min = 80, max = 200),
                    numericInput("cholesterol", "Cholesterol:", 200, min = 100, max = 400),
                    selectInput("fasting_blood_sugar", "Fasting Blood Sugar > 120 mg/dl:", c("True" = 1, "False" = 0)),
                    selectInput("resting_ecg", "Resting ECG:", c("Normal" = 0, "ST-T wave abnormality" = 1, "Left ventricular hypertrophy" = 2)),
                    numericInput("max_heart_rate", "Maximum Heart Rate:", 150, min = 60, max = 220),
                    selectInput("exercise_angina", "Exercise Induced Angina:", c("Yes" = 1, "No" = 0)),
                    numericInput("oldpeak", "Oldpeak:", 1, min = 0, max = 6, step = 0.1),
                    selectInput("ST_slope", "ST Slope:", c("Upsloping" = 1, "Flat" = 2, "Downsloping" = 3)),
                    actionButton("go", "Predict")
                  ),
                  mainPanel(
                    uiOutput("pred"),
                    uiOutput("feedback")
                  )
                )
              )
      ),
      tabItem(tabName = "About",
              fluidPage(
                titlePanel("About"),
                mainPanel(
                  h3("Application for Heart Disease Classification"),
                  p("This application was developed by:"),
                  tags$ul(
                    tags$li("Muhammad Ridho Alfarid - 22611087"),
                    tags$li("Artika Riski Haryadi - 22611091"),
                    tags$li("Safwah Ayu Mardiyyah - 22611101"),
                    tags$li("Tazkia Az-zahra Mulyadewi - 22611106"),
                    tags$li("Dheandra Ayu Wardani- 22611115")
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  output$analysisPlot1 <- renderPlot({
    ggplot(df, aes(x = age, y = cholesterol, color = target)) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(title = "Cholesterol Levels by Age", x = "Age", y = "Cholesterol (mm/dl)", color = "Heart Disease")
  })
  
  output$analysisPlot2 <- renderPlot({
    ggplot(df, aes(x = age, fill = target)) +
      geom_density(alpha = 0.5) +
      theme_minimal() +
      labs(title = "Age Density by Heart Disease", x = "Age", y = "Density", fill = "Heart Disease")
  })
  
  output$analysisPlot3 <- renderPlot({
    count_of_target_by_chest_pain_type <- df %>%
      group_by(chest_pain_type, target) %>%
      summarise(count = n())
    
    ggplot(count_of_target_by_chest_pain_type, aes(x = chest_pain_type, y = count, fill = factor(target))) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Count of Target by Chest Pain Type", x = "Chest Pain Type", y = "Count", fill = "Target")
  })
  
  output$analysisPlot4 <- renderPlot({
    plot_sex <- ggplot(df, aes(x = factor(sex), fill = factor(target))) +
      geom_bar(position = "dodge", alpha = 0.7) +
      theme_minimal() +
      labs(x = "Sex", y = "Count", fill = "Target") +
      scale_fill_manual(values = c("0" = "#FFB4C2", "1" = "#667BC6"))
    
    plot_fbs <- ggplot(df, aes(x = factor(fasting_blood_sugar), fill = factor(target))) +
      geom_bar(position = "dodge", alpha = 0.7) +
      theme_minimal() +
      labs(x = "Fasting Blood Sugar", y = "Count", fill = "Target") +
      scale_fill_manual(values = c("0" = "#FFB4C2", "1" = "#667BC6"))
    
    plot_angina <- ggplot(df, aes(x = factor(exercise_angina), fill = factor(target))) +
      geom_bar(position = "dodge", alpha = 0.7) +
      theme_minimal() +
      labs(x = "Exercise Angina", y = "Count", fill = "Target") +
      scale_fill_manual(values = c("0" = "#FFB4C2", "1" = "#667BC6"))
    
    grid.arrange(plot_sex, plot_fbs, plot_angina, ncol = 3)
  })
  
  output$analysisPlot5 <- renderPlot({
    ggplot(df, aes(x = target, y = resting_bp_s, fill = target)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(title = "Target by Resting Blood Pressure", x = "Target", y = "Resting Blood Pressure", fill = "Heart Disease")
  })
  
  output$analysisPlot6 <- renderPlot({
    ggplot(df, aes(x = max_heart_rate, fill = target)) +
      geom_density(alpha = 0.5) +
      theme_minimal() +
      labs(title = "Maximum Heart Rate Density by Heart Disease", x = "Max Heart Rate", y = "Density", fill = "Heart Disease")
  })
  
  output$Age <- renderPlot({
    ggplot(df, aes(x = age)) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black") +
      theme_minimal()
  })
  
  output$restingBP <- renderPlot({
    ggplot(df, aes(x = resting_bp_s)) +
      geom_histogram(binwidth = 5, fill = "green", color = "black") +
      theme_minimal()
  })
  
  output$Chol <- renderPlot({
    ggplot(df, aes(x = cholesterol)) +
      geom_histogram(binwidth = 10, fill = "red", color = "black") +
      theme_minimal()
  })
  
  observeEvent(input$go, {
    tryCatch({
      age <- input$age
      sex <- factor(as.numeric(input$sex), levels = c(0, 1))
      chest_pain_type <- factor(as.numeric(input$chest_pain_type), levels = c(1, 2, 3, 4))
      resting_bp_s <- input$resting_bp_s
      cholesterol <- input$cholesterol
      fasting_blood_sugar <- factor(as.numeric(input$fasting_blood_sugar), levels = c(0, 1))
      resting_ecg <- factor(as.numeric(input$resting_ecg), levels = c(0, 1, 2))
      max_heart_rate <- input$max_heart_rate
      exercise_angina <- factor(as.numeric(input$exercise_angina), levels = c(0, 1))
      oldpeak <- input$oldpeak
      ST_slope <- factor(as.numeric(input$ST_slope), levels = c(1, 2, 3))
      
      testDF <- data.frame(
        age = age,
        sex = sex,
        chest_pain_type = chest_pain_type,
        resting_bp_s = resting_bp_s,
        cholesterol = cholesterol,
        fasting_blood_sugar = fasting_blood_sugar,
        resting_ecg = resting_ecg,
        max_heart_rate = max_heart_rate,
        exercise_angina = exercise_angina,
        oldpeak = oldpeak,
        ST_slope = ST_slope
      )
      
      # Ensure factor levels are consistent
      testDF$sex <- factor(testDF$sex, levels = levels(trainTransformed$sex))
      testDF$chest_pain_type <- factor(testDF$chest_pain_type, levels = levels(trainTransformed$chest_pain_type))
      testDF$fasting_blood_sugar <- factor(testDF$fasting_blood_sugar, levels = levels(trainTransformed$fasting_blood_sugar))
      testDF$resting_ecg <- factor(testDF$resting_ecg, levels = levels(trainTransformed$resting_ecg))
      testDF$exercise_angina <- factor(testDF$exercise_angina, levels = levels(trainTransformed$exercise_angina))
      testDF$ST_slope <- factor(testDF$ST_slope, levels = levels(trainTransformed$ST_slope))
      
      # Transform the data
      testTransformed <- predict(preProcValues, testDF)
      
      # Predict the outcome
      prediction <- predict(myModel1, testTransformed)
      
      # Output the prediction
      output$pred <- renderUI({
        if (prediction == 1) {
          tags$h4("Prediction: Positive for heart disease", style = "color:red;")
        } else {
          tags$h4("Prediction: Negative for heart disease", style = "color:green;")
        }
      })
      
      # Optional: Provide feedback on the prediction
      output$feedback <- renderUI({
        tags$h4("For further health information, please contact the nearest hospital.")
      })
      
    }, error = function(e) {
      output$pred <- renderUI({
        tags$h4("Error in prediction. Please check your input values.")
      })
    })
  })
  
  output$exampleData <- renderDT({
    datatable(df)
  })
  
  output$contents <- renderDT({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    datatable(df)
  })
  
  output$descStats <- renderPrint({
    summary(df)
  })
}

shinyApp(ui, server)
