# GiveMeSomeCredit
Predict the probability that somebody will experience financial distress in the next two years.

[GiveMeSomeCredit](https://www.kaggle.com/c/GiveMeSomeCredit/overview)

## Data preprocessing
First with use summary() to overview training.csv and test.csv.

<img src="https://github.com/CCMinyi/Rproject_GiveMeSomeCredit/blob/main/image/training%20data.png" width=500>

In training.csv, there are some data which max value is really big such as:

RevolvingUtilizationOfUnsecuredLines,NumberOfTime30.59DaysPastDueNotWorse,DebtRatio,

NumberOfTimes90DaysLate,NumberRealEstateLoansOrLines and NumberOfTime60.89DaysPastDueNotWorse

so I decided to turn those value to 1 if the value in those factor are bigger than 1.

In MonthlyIncome and NumberOfDependents there are some value contributed to na.

According to the summary , the number of na is large. Portion matters ! so we cant drop out.

we fill na with the mean value. and so do the test.csv.

## Model 

Generally speaking , we would like to use logistic regression to predict the probability that 

somebody will experience financial distress in the next two years.

However we got the warning message from glm()
```R
glm.fit: fitted probabilities numerically 0 or 1 occurred
```
Which means our dataset is not suitable for this model

Thus , I pick three model to predict the probability. NaiveBayes , DecisionTree and Linear Regression . 

In 5 fold cross validation Decision Tree got the highest ave.test

<img src="https://github.com/CCMinyi/Rproject_GiveMeSomeCredit/blob/main/image/5%20fold%20cross%20validation.png" width=500>

but the prediction in test.csv only got 0.65

In 10 fold cross validation Linear Regression got the highest ave.test

<img src="https://github.com/CCMinyi/Rproject_GiveMeSomeCredit/blob/main/image/10%20fold%20cross%20validation.png" width=500>

we got 0.85 in test.csv

## Conclusion






