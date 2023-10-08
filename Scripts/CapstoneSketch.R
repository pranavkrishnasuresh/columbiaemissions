library("readxl")
dta = as.data.frame(read_excel("C:/Users/Lyudmila/Desktop/ClimateCapstone.xlsx"))
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
  
  set.seed(652)
  dta1 = dta1[sample(nrow(dta1)), ]
  
  for (i in 2:5) dta1 = cbind(dta1, dta1$ForestArea^i)
  
  for (i in 2:10) dta1 = cbind(dta1, dta1$Population^i)
  
  for (i in 1:10) dta1 = cbind(dta1, (dta1$Population * dta1$AccessToElectricity)^i)
  
  for (i in 1:12) dta1 = cbind(dta1, (dta1$Population * dta1$EnergyUse)^i)
  
  for (i in 2:5) dta1 = cbind(dta1, dta1$AgriculturalLand^i)
  
  results = LinearRegression(
    x = dta1[, -c(1, 3, 4)],
    y = dta1[, 1],
    cutoff = 0.85
  )
  
  return(results)
}

MMAE = c()

par(mfrow = c(3, 6))

for (year in 1996:2013){
  current = PredictYear(year)
  MMAE = c(MMAE, current$Test.MAE)
  plot(current$Truth_and_Predicted, main = year)
}

Resultant_MAE = mean(MMAE, na.rm = 1)
print(Resultant_MAE)
