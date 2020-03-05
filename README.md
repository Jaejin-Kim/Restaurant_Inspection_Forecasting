# Restaurant Inspection Forecasting
Messy Data & Machine Learning 
December 13, 2019
Jaejin Kim


Research Question: "Is there a difference in the pass/fail rate for restaurant inspections between Chicago and NYC when controlling for characteristics intuitively predictive of whether a restaurant passes or fails?"

1. Run preprocess_zip.codes.R
    - This file reads in IRS Individual Income Tax ZIP Code data for Illinois and New York state for 2015, 2016, and 2017, then selects aggregate Adjusted Gross Income, Total Income, and Number of Returns by zip code.
    - We then filter the data to zip codes that fall within the bounds of Chicago and NYC, and regroup these zip codes into neighborhoods. 
    - Once the neighborhoods are created, the file generates average Adjusted Gross Income and Total Income 
    - This produces IRS_demographics_by_zipcode.csv
        
2. Run preprocess NYC and preprocess Chicago
    - These files read in NYC Restaurant Inspections Data & Chicago Food Inspections Data, respectively
    - Each file then filters irrelevant columns and incomparable inspections from the provided data
    - Following data cleaning, the files standardize common existing fields in the provided data and generate new common features for analysis
    - This produces preprocessed_nyc_final.csv and preprocessed_chicago_final.csv, which both contain the same columns
    
3. Run join.R to aggregate NYC and Chicago data and merge with IRS_demographics_by_zipcode
    
4. Run analysis.R
    - Creates training, validation, and testing sets
    - Runs a baseline logistic regression with just city as a predictor, and then runs logistic regression, random forest, and lasso models with the full set of features
    - Computes AUC for each model as a means of model evaluation
    - Generates one plot with ROC curves for the validation set of all three models and one with the ROC curve for the random forest model
    
5. Read writeup of the full analysis in Restaurant_Inspection_Forcasting_Write_Up.pdf

		

