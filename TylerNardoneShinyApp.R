library(tidyverse)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(pls)
library(Metrics)
library(shiny)

# Set path for data.
file_path <- "ASA_All_PGA_Raw_Data_Tour_Level.csv"

# Load data.
all_data <- read.csv(file_path)

# Select only relevant columns.
cols <- c("sg_putt", "sg_arg", "sg_app", "sg_ott", "sg_t2g", "sg_total", "Finish")
raw_df <- all_data %>% select(all_of(cols))

# Filter any non-numeric columns out of target column.
finish_values <- raw_df$Finish %>% unique()
finish_values_num <- finish_values[grep("[0-9]", finish_values)]
finish_values_non_num <- finish_values[!(finish_values %in% finish_values_num)]
raw_df <- subset(raw_df, !(raw_df$Finish %in% finish_values_non_num))

# Remove any leading "T"s from target column and convert to numbers.
raw_df$Finish <- parse_number(raw_df$Finish)

# Scale data.
scaled_df <- data.frame(scale(raw_df, center=TRUE, scale=TRUE))

# Apply principal component regression to scaled data frame.
set.seed(12)
model <- pcr(Finish~sg_putt+sg_arg+sg_app+sg_ott+sg_t2g+sg_total, data=scaled_df, validation="CV")

# Split scaled data frame into training and testing data sets.
train_size <- 0.7*nrow(scaled_df)
train <- scaled_df[1:train_size, c("Finish", "sg_putt", "sg_arg", "sg_app", "sg_ott", "sg_t2g", "sg_total")]
y_test <- scaled_df[(train_size+1):nrow(scaled_df), "Finish"]
test <- scaled_df[(train_size+1):nrow(scaled_df), c("sg_putt", "sg_arg", "sg_app", "sg_ott", "sg_t2g", "sg_total")]

# Run principal component regression model on training data, make predictions with testing data.
model <- pcr(Finish~sg_putt+sg_arg+sg_app+sg_ott+sg_t2g+sg_total, data=train, validation="CV")
# predictions <- predict(model, test, ncomp=2)

# Calculate RMSE.
# rmse <- sqrt(mean((predictions - y_test)^2))

# Calculate normalized RMSE.
max_scaled_Finish <- max(scaled_df$Finish)
min_scaled_Finish <- min(scaled_df$Finish)
# norm_rmse <- rmse / (max_scaled_Finish - min_scaled_Finish)

ui <- fluidPage(
  titlePanel("Predicting PGA Tour Tournament Finishes with Regression and Principal Component Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("n_pca",
                  h3("Select Number of Principal Components"),
                  choices = list(
                    "1" = 1,
                    "2" = 2,
                    "3" = 3,
                    "4" = 4,
                    "5" = 5,
                    "6" = 6
                    )
                  )
      ),
    mainPanel(
      includeMarkdown(
        "Tyler_Nardone_DS_501_Homework_6_Shiny.Rmd"
        ),
      h1("Results"),
      textOutput("selected_n"),
      br(),
      textOutput("text"),
      br(),
      # textOutput("rmse"), #This line of code prompts the "Warning: Error in $: Can't read output 'predictions'" error. See below.
      fluidRow(
        column(width = 4, tableOutput("y_test")),
        column(width = 4, tableOutput("predictions"))
      )
      )
    )
  
)

server <- function(input, output) {
  
  output$selected_n <- renderText({
    paste("Model predictions with selected number of principal components:", input$n_pca, sep=" ")
  })
  
  f <- reactive({
    as.numeric(input$n_pca)
  })
  
  output$predictions <- renderTable({
    predict(model, test, ncomp = f())
  })
  
  output$y_test <- renderTable({
    y_test
  })
  
  output$rmse <- renderText({
    rmse(output$predictions, output$y_test)
  })
    
  # Whenever the above code is tried to output from the UI function, an error is displayed saying 
  # "Error in output$predictions : Can't read output 'predictions'."
  # After many hours of troubleshooting, no solution was found and the best possible alternative is to simply display the
  # predictions and test data side by side to visualize how the predictions change with the number of 
  # principal components that are chosen.
  
  output$text <- renderText({
    "The shiny app did not allow for direct computation of RMSE or any other direct error measurement, due to what it said was an inability to read output predictions. In lieu of that, below are the predictions side by side with the test data to visualize the effect that the number of principal components has on the predictions."
  })
    
}

shinyApp(ui = ui, server = server)

