library("readxl")
dta = as.data.frame(read_excel("..."))
rowkeep = c(4, 45, 50, 62, 68, 0)
keep = c()
for (i in 1:20064){
  cur = i %% 76
  if (cur %in% rowkeep) keep = c(keep, i)
}
newdta = dta[keep, ]
row.names(newdta) = NULL
# Every 5th row is the data for greenhouse emissions (what we are predicting)

LinearRegression = function(
  x = all[, -1],
  y = all[, 1],
  cutoff = 0.9
) {
  
  # Compile data
  all <- data.frame(cbind(y,x))
  
  # Split data:
  train <- all[1:round(cutoff*nrow(all),0),]; dim(train) # Training set
  test <- all[(round(cutoff*nrow(all),0)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- data.frame(train[,-1]); colnames(train.x) <- colnames(train)[-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- data.frame(test[,-1]); dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Modeling fitting:
  # GLM or # LM
  model <- glm(
    train.y ~.,
    data = train.x
  )
  sum <- summary(model)
  
  # Make prediction on training:
  preds.train.prob <- predict(model, train.x)
  train.mae <-mean(abs(preds.train.prob - train.y), na.rm = 1)
  
  # Make prediction on testing:
  colnames(test.x) <- colnames(train.x)
  preds.prob <- predict(model, test.x)
  test.mae <- mean(abs(preds.prob - test.y), na.rm = 1)
  
  # Truth.vs.Predicted.Probabilities
  truth.vs.pred.prob <- cbind(test.y, preds.prob)
  colnames(truth.vs.pred.prob) <- c("True_Test_Y", "Predicted_Test_Y")
  
  # Final output:
  return(
    list(
      Summary = sum,
      Train = train,
      Test = test,
      Train.MAE = train.mae,
      Test.MAE = test.mae,
      Truth_and_Predicted = truth.vs.pred.prob
    )
  )
}

PredictYear = function(year = 2010){
  year = as.character(year)
  GasEmissions = newdta[rep(0:263) * length(rowkeep) + 2, year]
  Population = newdta[rep(0:263) * length(rowkeep) + 1, year]
  EnergyUse = newdta[rep(0:263) * length(rowkeep) + 3, year]
  AccessToElectricity = newdta[rep(0:263) * length(rowkeep) + 4, year]
  ForestArea = newdta[rep(0:263) * length(rowkeep) + 5, year]
  AgriculturalLand = newdta[rep(0:263) * length(rowkeep) + 6, year]
  dta1 = data.frame(
    GasEmissions,
    Population,
    EnergyUse,
    AccessToElectricity,
    ForestArea,
    AgriculturalLand
  )
  
  dta1 = cbind(newdta[rep(0:263) * length(rowkeep) + 1, "Country Name"], dta1)
  
  set.seed(652)
  dta1 = dta1[sample(nrow(dta1)), ]
  
  for (i in 2:5) dta1 = cbind(dta1, dta1$ForestArea^i)
  
  for (i in 2:10) dta1 = cbind(dta1, dta1$Population^i)
  
  for (i in 1:10) dta1 = cbind(dta1, (dta1$Population * dta1$AccessToElectricity)^i)
  
  for (i in 1:12) dta1 = cbind(dta1, (dta1$Population * dta1$EnergyUse)^i)
  
  for (i in 2:5) dta1 = cbind(dta1, dta1$AgriculturalLand^i)
  
  results = LinearRegression(
    x = dta1[, -c(1, 2, 4, 5)],
    y = dta1[, 2],
    cutoff = 0.85
  )
  
  country_results = data.frame(
    Country = dta1[(round(0.85 * nrow(dta1), 0) + 1):nrow(dta1), 1],
    True_Y = results$Truth_and_Predicted[, 1],
    Predicted_Y = results$Truth_and_Predicted[, 2]
  )
  row.names(country_results) = NULL
  
  return(list(
    all_results = results,
    country_results = country_results
  ))
}

MMAE = c()

Country_True_Results = list()
Country_Predicted_Results = list()

for (year in 2000:2013){
  current_all = PredictYear(year)$all_results
  MMAE = c(MMAE, current_all$Test.MAE)
  current_country = PredictYear(year)$country_results
  for (i in 1:nrow(current_country)){
    name = current_country[i, 1]
    if (year == 2000)
      Country_True_Results[[name]] = data.frame(
        Year = c(year),
        CO2_Emissions = c(current_country[i, 2])
      )
    else
      Country_True_Results[[name]] = rbind(
        Country_True_Results[[name]],
        c(year, current_country[i, 2])
      )
    if (year == 2000)
      Country_Predicted_Results[[name]] = data.frame(
        Year = c(year),
        CO2_Emissions = c(current_country[i, 3])
      )
    else
      Country_Predicted_Results[[name]] = rbind(
        Country_Predicted_Results[[name]],
        c(year, current_country[i, 3])
      )
  }
}

Resultant_MAE = mean(MMAE, na.rm = 1)
print(Resultant_MAE)

#################### SHINY APP ####################
library("shiny")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Prediction of CO2 Emissions by Countries' Development Indicators"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a country:",
                  choices = names(Country_Predicted_Results)),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption", container = span)),
      
      # Output: Plot a time-series graph
      plotOutput("Plot")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Create caption ----
  # The output$caption is computed based on a reactive expression
  # that returns input$caption. When the user changes the
  # "caption" field:
  #
  # 1. This function is automatically called to recompute the output
  # 2. New caption is pushed back to the browser for re-display
  #
  # Note that because the data-oriented reactive expressions
  # below don't depend on input$caption, those expressions are
  # NOT called when input$caption changes
  output$caption <- renderText({
    input$caption
  })
  
  output$Plot <- renderPlot({
    country = input$dataset
    
    pred = Country_Predicted_Results[[country]]$CO2_Emissions
    actual = Country_True_Results[[country]]$CO2_Emissions
    
    if (TRUE %in% is.na(pred)) {
      plot(0, 0, main="No data available!")
    } else {
      lim = max(max(pred, na.rm = 1), max(actual, na.rm = 1))
      plot(Country_Predicted_Results[[country]],
           type = 'l', col = "red", lwd = 3,
           ylab = "CO2 Emissions (kt)", xlab = "",
           ylim = c(0, lim),
           main = paste("Results for: ", country),
           sub = paste("Mean Absolute Percentage Error: ", mean(abs(pred - actual) / actual, na.rm = 1) * 100, "%.",
                       "\nMean Absolute Error: ", mean(abs(pred - actual), na.rm = 1), "kt."
                       )
      ); lines(Country_True_Results[[country]], type = 'l', col = "blue", lwd = 3)
      legend(x = Country_Predicted_Results[[country]][length(pred), 1] - 4, y = lim / 7,
             legend = c("Predicted Emissions", "Actual Emissions"), col = c("red", "blue"),
             lty = rep(1, 2), lwd = rep(2, 2), cex = 0.8, text.font = 2
             )
    }
    
    
  })
  
  
} # End

# Create Shiny app ----
shinyApp(ui, server)
