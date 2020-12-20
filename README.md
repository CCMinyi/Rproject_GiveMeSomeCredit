# GiveMeSomeCredit
Predict the probability that somebody will experience financial distress in the next two years.

[GiveMeSomeCredit](https://www.kaggle.com/c/GiveMeSomeCredit/overview)

## Data preprocessing
First with use summary() to overview training.csv and test.csv.

<img src="https://github.com/CCMinyi/Rproject_GiveMeSomeCredit/blob/main/image/training%20data.png" width=480>

In training.csv, there are some data which max value is really big such as:

RevolvingUtilizationOfUnsecuredLines,NumberOfTime30.59DaysPastDueNotWorse,DebtRatio,

NumberOfTimes90DaysLate,NumberRealEstateLoansOrLines and NumberOfTime60.89DaysPastDueNotWorse

so I decided to turn those value to 1 if the value in those factor are bigger than 1.

In MonthlyIncome and NumberOfDependents there are some value contributed to na.

According to the summary , the number of na is large. Portion matters ! so we cant drop out.

we fill na with the mean value. and so do the test.csv.

## Model 
