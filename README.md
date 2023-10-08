### Random-Variables.md

# Prediction of CO2 Emissions by Countries' Development Indicators

## Vincent Li, Innokentiy Kaurov, Henry Greenhut, Oliver Zhou, Krishna Suresh

## Data

[Dataset with World Bank country indicators](https://www.kaggle.com/ploverbrown/world-bank-indicators-collection)

## Importance of data
### To be able to predict the global and domestic greenhouse emissions by such indicators as:
- Population
- Agriculture
- Forested Area
- Energy Use
- Access to Electricity
## Benchmark. Existing projects include:
### [Sample 1](https://www.kaggle.com/ghenima/temperature-change-analysis-in-progress)
This person did a forecast of temperature. We are going to use World Bank indicators to predict the greenhouse emissions in individual areas.

### [Sample 2](https://www.kaggle.com/gatandubuc/forecast-with-n-beats-interpretable-model)
This person also did a forecast of temperature. We are going to use neural networks to make more accurate forecasts of global warming.
## Proposed Model/Algorithm
We will try to build a model that predicts the global and domestic greenhouse gas emissions, using the "Climate Change" dataset, produced by World Bank. We will then assess the accuracy of the model by comparing the results to the actual values.


**The Approach.** In the second method we will try to come up with a linear formula to forecast the future greenhouse emissions based on the 
countries' World Bank indicators. This approach will use machine learning, in particular the Linear Regression algorithm.
For this approach, we will only work with data over 1 year (e.g. 2019). Unlike the first model, this algorithm will not consider data from previous years, 
and the prediction will purely be based on contemporary statistics.
We will take the countries' indicators from the dataset as the explanatory variables (*X*), and the greenhouse emissions as the response variable (*Y*).
We will then write an algorithm that learns from *X* to predict *Y*.
If the results are unsatisfactory, we may need to use a Neural Network (a Regressor) to boost the algorithm's performance.

## Diagram: coming soon...
