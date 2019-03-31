# Universal Bank is a relatively young bank growing rapidly in terms of overall customer acquisition. 
# The majority of these customers are liability customers (depositors) with varying sizes of relationship with the bank. 
# The customer base of asset customers (borrowers) is quite small, and the bank is interested in expanding this base rapidly to bring in more loan business. 
# In particular, it wants to explore ways of converting its liability customers to personal loan customers (while retaining them as depositors). 
# A campaign that the bank ran last year for liability customers showed a healthy conversion rate of over 9% success. This has encouraged the retail marketing department to devise smarter campaigns with better target marketing. 
# The goal of our analysis is to model the previous campaign's customer behavior to analyze what combination of factors make a customer more likely to accept a personal loan. 
# This will serve as the basis for the design of a new campaign. 
# The file UniversalBank.xlsx contains data on 5000 customers. 
# The data include customer demographic information (age, income, etc.), the customer's relationship with the bank (mortgage, securities account, etc.), and the customer response to the last personal loan campaign (Personal Loan). 
# Among these 5000 customers, only 480(= 9.6%) accepted the personal loan that was offered to them in the earlier campaign.


#import data
setwd("C:/Users/Think/Desktop/data mining")
library(xlsx)
bank.df <- read.xlsx('UniversalBank.xlsx', sheetIndex = 2)
N <- nrow(bank.df)

#running result: 
# > N
# [1] 5000

# Drop ID and zip code columns
bank.df <- bank.df[ , -c(1, 5)]
# treat Education as categorical (R will create dummy variables) 
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), labels = c("Undergrad", "Graduate", "Advanced/Professional"))
library(fastDummies)
e <- dummy_cols(bank.df$Education)
bank.df <- bank.df[, -c(6)]
bank.df <- data.frame(bank.df, e)
bank.df <- bank.df[, -c(12)]

# partition data into 6:4
set.seed(2) 
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6) 
train.df <- bank.df[train.index, ] 
valid.df <- bank.df[-train.index, ]

#test example data using K = 1
newdata <- data.frame(
  Age = 40, Experience = 10, Income = 84, Family = 2,
  CCAvg = 2, Mortgage = 0, 
  Securities.Account = 0, CD.Account = 0, Online = 1, CreditCard = 1,
  .data_Undergrad = 0, .data_Graduate = 1, .data_Advanced.Professional = 0)

pred <- knn(train=train.df[,-c(7),drop=TRUE],test=newdata, cl=train.df[,7,drop=TRUE], k =1)

#running result:
#[1] 0
#attr(,"nn.index")
#     [,1]
#[1,]  452
#attr(,"nn.dist")
#         [,1]
#[1,] 4.605432
#Levels: 0

#the sample is classified as "personal loan is not accepted"

#trying to find the best K value
library(caret)
# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
# compute knn for different k on validation.
for(i in 1:14) {
  knn.pred <- knn(train=train.df[,-c(7),drop=TRUE],test=valid.df[,-c(7),drop=TRUE],
                  cl=train.df[,7,drop=TRUE], k=i)
  accuracy.df[i,2]<-confusionMatrix(knn.pred, as.factor(valid.df$Personal.Loan))$overall[1]
}
accuracy.df
# k accuracy
# 1   1   0.9050
# 2   2   0.9085
# 3   3   0.9145
# 4   4   0.9080
# 5   5   0.9000
# 6   6   0.9045
# 7   7   0.9020
# 8   8   0.9065
# 9   9   0.9015
# 10 10   0.9045
# 11 11   0.9030
# 12 12   0.9050
# 13 13   0.9045
# 14 14   0.9070
#due to K = 3 has the highest accuracy, it is our choice

#test the model with the chosen K and validation set
library(gmodels)
test_pred2 = knn(train=train.df[,-c(7),drop=TRUE],
                 test=valid.df[,-c(7),drop=TRUE], 
                 cl=train.df$Personal.Loan, 
                 k=3)
CrossTable(x = valid.df$Personal.Loan, y = test_pred2, prop.chisq = FALSE)

#running result:
#    Cell Contents
#   |-------------------------|
#   |                       N |
#   |           N / Row Total |
#   |           N / Col Total |
#   |         N / Table Total |
#   |-------------------------|
#   
#   
#   Total Observations in Table:  2000 
# 
# 
#                        | test_pred2 
# valid.df$Personal.Loan |         0 |         1 | Row Total | 
# -----------------------|-----------|-----------|-----------|
#                      0 |      1750 |        59 |      1809 | 
#                        |     0.967 |     0.033 |     0.904 | 
#                        |     0.940 |     0.428 |           | 
#                        |     0.875 |     0.029 |           | 
# -----------------------|-----------|-----------|-----------|
#                      1 |       112 |        79 |       191 | 
#                        |     0.586 |     0.414 |     0.096 | 
#                        |     0.060 |     0.572 |           | 
#                        |     0.056 |     0.040 |           | 
# -----------------------|-----------|-----------|-----------|
#           Column Total |      1862 |       138 |      2000 | 
#                        |     0.931 |     0.069 |           | 
# -----------------------|-----------|-----------|-----------|
# accuracy rate = 0.875 + 0.04 = 0.915 

#use this model to classify the sample
pred2 <- knn(train=train.df[,-c(7),drop=TRUE],test=newdata, cl=train.df[,7,drop=TRUE], k =3)
# [1] 0
# attr(,"nn.index")
# [,1] [,2] [,3]
# [1,]  452 2235  350
# attr(,"nn.dist")
# [,1]     [,2]     [,3]
# [1,] 4.605432 4.651881 4.737088
# Levels: 0
#sample is still classified as "personal loan is not accepted"

#using a different portion of training : validation : test = 5:3:2
set.seed(2) 
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.5) 
train.df <- bank.df[train.index, ] 
rest.df <- bank.df[-train.index, ]
set.seed(2) 
valid.index <- sample(c(1:dim(rest.df)[1]), dim(rest.df)[1]*0.6) 
valid.df <- rest.df[valid.index, ] 
test.df <- rest.df[-valid.index, ]

train_pred = knn(train.df[,-c(7),drop=TRUE], 
                 train.df[,-c(7),drop=TRUE], 
                 cl=train.df$Personal.Loan, 
                 k=3)
CrossTable(x = train.df$Personal.Loan, y = train_pred, prop.chisq = FALSE)

#    Cell Contents
#   |-------------------------|
#   |                       N |
#   |           N / Row Total |
#   |           N / Col Total |
#   |         N / Table Total |
#   |-------------------------|
#   
#   
#   Total Observations in Table:  2500 
# 
# 
#                        | train_pred 
# train.df$Personal.Loan |         0 |         1 | Row Total | 
# -----------------------|-----------|-----------|-----------|
#                      0 |      2233 |        34 |      2267 | 
#                        |     0.985 |     0.015 |     0.907 | 
#                        |     0.965 |     0.183 |           | 
#                        |     0.893 |     0.014 |           | 
# -----------------------|-----------|-----------|-----------|
#                      1 |        81 |       152 |       233 | 
#                        |     0.348 |     0.652 |     0.093 | 
#                        |     0.035 |     0.817 |           | 
#                        |     0.032 |     0.061 |           | 
# -----------------------|-----------|-----------|-----------|
#           Column Total |      2314 |       186 |      2500 | 
#                        |     0.926 |     0.074 |           | 
# -----------------------|-----------|-----------|-----------|
# accuracy rate = 0.893 + 0.061 = 0.954 


valid_pred = knn(train.df[,-c(7),drop=TRUE], 
                 valid.df[,-c(7),drop=TRUE], 
                 cl=train.df$Personal.Loan, 
                 k=3)
CrossTable(x = valid.df$Personal.Loan, y = valid_pred, prop.chisq = FALSE)
#    Cell Contents
#   |-------------------------|
#   |                       N |
#   |           N / Row Total |
#   |           N / Col Total |
#   |         N / Table Total |
#   |-------------------------|
#   
#   
#   Total Observations in Table:  1500 
# 
# 
#                        | valid_pred 
# valid.df$Personal.Loan |         0 |         1 | Row Total | 
# -----------------------|-----------|-----------|-----------|
#                      0 |      1289 |        54 |      1343 | 
#                        |     0.960 |     0.040 |     0.895 | 
#                        |     0.931 |     0.470 |           | 
#                        |     0.859 |     0.036 |           | 
# -----------------------|-----------|-----------|-----------|
#                      1 |        96 |        61 |       157 | 
#                        |     0.611 |     0.389 |     0.105 | 
#                        |     0.069 |     0.530 |           | 
#                        |     0.064 |     0.041 |           | 
# -----------------------|-----------|-----------|-----------|
#           Column Total |      1385 |       115 |      1500 | 
#                        |     0.923 |     0.077 |           | 
# -----------------------|-----------|-----------|-----------|
# accuracy rate = 0.859 + 0.041 = 0.900 

test_pred = knn(train.df[,-c(7),drop=TRUE], 
                test.df[,-c(7),drop=TRUE], 
                cl=train.df$Personal.Loan, 
                k=3)
CrossTable(x = test.df$Personal.Loan, y = test_pred, prop.chisq = FALSE)
# Cell Contents
# |-------------------------|
#   |                       N |
#   |           N / Row Total |
#   |           N / Col Total |
#   |         N / Table Total |
#   |-------------------------|
#   
#   
#   Total Observations in Table:  1000 
# 
# 
#                       | test_pred 
# test.df$Personal.Loan |         0 |         1 | Row Total | 
# ----------------------|-----------|-----------|-----------|
#                     0 |       873 |        37 |       910 | 
#                       |     0.959 |     0.041 |     0.910 | 
#                       |     0.945 |     0.487 |           | 
#                       |     0.873 |     0.037 |           | 
# ----------------------|-----------|-----------|-----------|
#                     1 |        51 |        39 |        90 | 
#                       |     0.567 |     0.433 |     0.090 | 
#                       |     0.055 |     0.513 |           | 
#                       |     0.051 |     0.039 |           | 
# ----------------------|-----------|-----------|-----------|
#          Column Total |       924 |        76 |      1000 | 
#                       |     0.924 |     0.076 |           | 
# ----------------------|-----------|-----------|-----------|
# accuracy rate = 0.873 + 0.039 = 0.912 
#we can tell from the result that the change of the data set portion did not largely change the classification accuracy(from 
#0.915 to 0.912)