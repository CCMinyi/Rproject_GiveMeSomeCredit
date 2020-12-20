#Data preprocessing
training_data <- read.table('Data/training.csv',header = T,sep = ',')
testing_data <- read.table('Data/test.csv',header = T,sep = ',')
#training data
#RevolvingUtilizationOfUnsecuredLines 大於1的變成1
RevolvingUtilizationOfUnsecuredLines.fix <- ifelse((training_data$RevolvingUtilizationOfUnsecuredLines>1),
                                                   1,training_data$RevolvingUtilizationOfUnsecuredLines)
#age 
age.fix <- training_data$age
#NumberOfTime30.59DaysPastDueNotWorse 大於1的變成1
NumberOfTime30.59DaysPastDueNotWorse.fix <- ifelse((training_data$NumberOfTime30.59DaysPastDueNotWorse>1)
                                                   ,1,training_data$NumberOfTime30.59DaysPastDueNotWorse)
#DebtRatio 大於2的變成2
DebtRatio.fix <- ifelse((training_data$DebtRatio>2),2,training_data$DebtRatio)
#MonthlyIncome na取ave
MonthlyIncome.fix <- ifelse(is.na(training_data$MonthlyIncome),
                            mean(training_data$MonthlyIncome,na.rm = T),training_data$MonthlyIncome)
#NumberOfOpenCreditLinesAndLoans 
NumberOfOpenCreditLinesAndLoans.fix <- training_data$NumberOfOpenCreditLinesAndLoans
#NumberOfTimes90DaysLate 大於1取1
NumberOfTimes90DaysLate.fix <- ifelse((training_data$NumberOfTimes90DaysLate>1),
                                      1,training_data$NumberOfTimes90DaysLate)
#NumberRealEstateLoansOrLines 大於2取2
NumberRealEstateLoansOrLines.fix <- ifelse((training_data$NumberRealEstateLoansOrLines>2),2
                                           ,training_data$NumberRealEstateLoansOrLines)
#NumberOfTime60.89DaysPastDueNotWorse 大於1取1
NumberOfTime60.89DaysPastDueNotWorse.fix <- ifelse((training_data$NumberOfTime60.89DaysPastDueNotWorse>1)
                                                   ,1,training_data$NumberOfTime60.89DaysPastDueNotWorse)
#NumberOfDependents na means
NumberOfDependents.fix <- ifelse((is.na(training_data$NumberOfDependents))
                                 ,mean(training_data$NumberOfDependents,na.rm = T)
                                 ,training_data$NumberOfDependents)

training_data_after_cleaning <- data.frame(X=training_data$X,
                                           SeriousDlqin2yrs=training_data$SeriousDlqin2yrs
)
training_data_after_cleaning_2 <- cbind(training_data_after_cleaning,
                                        RevolvingUtilizationOfUnsecuredLines=RevolvingUtilizationOfUnsecuredLines.fix,
                                        age=age.fix,
                                        NumberOfTime30.59DaysPastDueNotWorse=NumberOfTime30.59DaysPastDueNotWorse.fix,
                                        DebtRatio=DebtRatio.fix,
                                        MonthlyIncome = MonthlyIncome.fix,
                                        NumberOfOpenCreditLinesAndLoans=NumberOfOpenCreditLinesAndLoans.fix,
                                        NumberOfTimes90DaysLate=NumberOfTimes90DaysLate.fix,
                                        NumberRealEstateLoansOrLines=NumberRealEstateLoansOrLines.fix,
                                        NumberOfTime60.89DaysPastDueNotWorse=NumberOfTime60.89DaysPastDueNotWorse.fix,
                                        NumberOfDependents=NumberOfDependents.fix)
write.table(training_data_after_cleaning_2,'cleaned_training_data.csv',quote = F,sep = ',',row.names = F)

#testing data
#RevolvingUtilizationOfUnsecuredLines 大於1的變成1
RevolvingUtilizationOfUnsecuredLines.fix <- ifelse((testing_data$RevolvingUtilizationOfUnsecuredLines>1),
                                                   1,testing_data$RevolvingUtilizationOfUnsecuredLines)
#age 
age.fix <- testing_data$age
#NumberOfTime30.59DaysPastDueNotWorse 大於1的變成1
NumberOfTime30.59DaysPastDueNotWorse.fix <- ifelse((testing_data$NumberOfTime30.59DaysPastDueNotWorse>1)
                                                   ,1,testing_data$NumberOfTime30.59DaysPastDueNotWorse)
#DebtRatio 大於2的變成2
DebtRatio.fix <- ifelse((testing_data$DebtRatio>2),2,testing_data$DebtRatio)
#MonthlyIncome na取ave
MonthlyIncome.fix <- ifelse(is.na(testing_data$MonthlyIncome),
                            mean(testing_data$MonthlyIncome,na.rm = T),testing_data$MonthlyIncome)
#NumberOfOpenCreditLinesAndLoans 
NumberOfOpenCreditLinesAndLoans.fix <- testing_data$NumberOfOpenCreditLinesAndLoans
#NumberOfTimes90DaysLate 大於1取1
NumberOfTimes90DaysLate.fix <- ifelse((testing_data$NumberOfTimes90DaysLate>1),
                                      1,testing_data$NumberOfTimes90DaysLate)
#NumberRealEstateLoansOrLines 大於2取2
NumberRealEstateLoansOrLines.fix <- ifelse((testing_data$NumberRealEstateLoansOrLines>2),2
                                           ,testing_data$NumberRealEstateLoansOrLines)
#NumberOfTime60.89DaysPastDueNotWorse 大於1取1
NumberOfTime60.89DaysPastDueNotWorse.fix <- ifelse((testing_data$NumberOfTime60.89DaysPastDueNotWorse>1)
                                                   ,1,testing_data$NumberOfTime60.89DaysPastDueNotWorse)
#NumberOfDependents na means
NumberOfDependents.fix <- ifelse((is.na(testing_data$NumberOfDependents))
                                 ,mean(testing_data$NumberOfDependents,na.rm = T)
                                 ,testing_data$NumberOfDependents)

testing_data_after_cleaning <- data.frame(X=testing_data$X
)
testing_data_after_cleaning_2 <- cbind(testing_data_after_cleaning,
                                        RevolvingUtilizationOfUnsecuredLines=RevolvingUtilizationOfUnsecuredLines.fix,
                                        age=age.fix,
                                        NumberOfTime30.59DaysPastDueNotWorse=NumberOfTime30.59DaysPastDueNotWorse.fix,
                                        DebtRatio=DebtRatio.fix,
                                        MonthlyIncome = MonthlyIncome.fix,
                                        NumberOfOpenCreditLinesAndLoans=NumberOfOpenCreditLinesAndLoans.fix,
                                        NumberOfTimes90DaysLate=NumberOfTimes90DaysLate.fix,
                                        NumberRealEstateLoansOrLines=NumberRealEstateLoansOrLines.fix,
                                        NumberOfTime60.89DaysPastDueNotWorse=NumberOfTime60.89DaysPastDueNotWorse.fix,
                                        NumberOfDependents=NumberOfDependents.fix)
write.table(testing_data_after_cleaning_2,'cleaned_testing_data.csv',quote = F,sep = ',',row.names = F)
