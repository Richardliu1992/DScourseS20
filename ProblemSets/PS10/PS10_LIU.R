install.packages("rpart")
install.packages("e1071")
install.packages("kknn")
install.packages("nnet")
install.packages("readr")

library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(mlr)
library(tidyverse)
library(magrittr)
library(readr)



# Question 4 Start your code file by importing the starter code.
set.seed(100)

# There is an error when I use the url to import data so I download it and then import it.
income <- read.table("/Users/Richa/Downloads/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]



# Question 5 Using the mlr library, create the following objects for your cross validation exercise.
# Question 5.1 The classification task.
theClassifTask <- makeClassifTask(data = income.train,target = "high.earner")

# Question 5.2 The 3-fold cross-validation strategy.
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

# Question 5.3 The tuning strategy (e.g. random with 10 guesses).
tuneMethod <- makeTuneControlRandom(maxit = 10L)

# Question 5.4 Each of the six “learners” (algorithms).
# Question 5.4.1 Trees: classif.rpart.
Trees <- makeLearner("classif.rpart", predict.type = "response")

# Question 5.4.2 Logistic regression: classif.glmnet.
Logistic <- makeLearner("classif.glmnet", predict.type = "response")

# Question 5.4.3 Neural network: classif.nnet
Neural <- makeLearner("classif.nnet", predict.type = "response")

# Question 5.4.4 Naive Bayes: classif.naiveBayes
Naive <- makeLearner("classif.naiveBayes", predict.type = "response")

# Question 5.4.5 kNN: classif.kknn
kNN <- makeLearner("classif.kknn", predict.type = "response")

# Question 5.4.6 SVM: classif.svm
SVM <- makeLearner("classif.svm", predict.type = "response")



# Question 6 Now set up the hyperparameters of each algorithm that will need to be cross validated.
# Question 6.1 Tree model
TreeParams <- makeParamSet(makeIntegerParam("minsplit",lower=10,upper=50), makeIntegerParam("minbucket",lower=5,upper=50),makeNumericParam("cp", lower=0.001,upper=0.2))

# Question 6.2 Logit model
LogitParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),makeNumericParam("alpha",lower=0,upper=1))

# Question 6.3 Neural network model
NeuralParams <- makeParamSet(makeIntegerParam("size" ,lower=1,upper=10),makeNumericParam("decay",lower=0.1,upper=0.5),makeIntegerParam("maxit",lower=1000,upper=1000))

# Question 6.4 Naive Bayes
#There’s nothing to regularize here, so you don’t need to do any cross-validation or tuning for this model

# Question 6.5 kNN
kNNParams <- makeParamSet(makeIntegerParam("k",lower=1,upper=30))

# Question 6.6 SVM
SVMParams <- makeParamSet(makeDiscreteParam("kernel", values = "radial"),makeDiscreteParam("cost", values = 2^c(-2,-1,0, 1,2,10)),makeDiscreteParam("gamma", values = 2^c(-2,-1,0, 1,2,10)))



# Question 7 Now tune the models.Remember that you don’t need to tune the Naive Bayes model.
# Question 7.1 Tree model
tunedTree <- tuneParams(learner = Trees,task = theClassifTask,resampling = resampleStrat,measures=list(f1, gmean),par.set = TreeParams,control = tuneMethod,show.info = TRUE)

# Question 7.2 Logit model
tunedLogit <- tuneParams(learner = Logistic,task = theClassifTask,resampling = resampleStrat,measures=list(f1, gmean),par.set = LogitParams,control = tuneMethod,show.info = TRUE)

# Question 7.3 Neural network model
tunedNeural <- tuneParams(learner = Neural,task = theClassifTask,resampling = resampleStrat,measures=list(f1, gmean),par.set = NeuralParams,control = tuneMethod,show.info = TRUE)

# Question 7.4 Naive Bayes
# Remember that you don’t need to tune the Naive Bayes model.

# Question 7.5 kNN
tunedkNN <- tuneParams(learner = kNN,task = theClassifTask,resampling = resampleStrat,measures=list(f1, gmean),par.set = kNNParams,control = tuneMethod,show.info = TRUE)

# Question 7.6 SVM
tunedSVM <- tuneParams(learner = SVM,task = theClassifTask,resampling = resampleStrat,measures=list(f1, gmean),par.set = SVMParams,control = tuneMethod,show.info = TRUE)



# Question 8 apply the optimal tuning parameters to each of the algorithms. Then train the models, generate predictions, and assess performance.
# Question 8.1 Apply the optimal algorithm parameters to the model
predTree <- setHyperPars(learner=Trees, par.vals = tunedTree$x)
predLogit <- setHyperPars(learner=Logistic, par.vals = tunedLogit$x)
predNeural <- setHyperPars(learner=Neural, par.vals = tunedNeural$x)
predkNN <- setHyperPars(learner=kNN, par.vals = tunedkNN$x)
predSVM <- setHyperPars(learner=SVM, par.vals = tunedSVM$x)

# Question 8.2 Verify performance on cross validated sample sets
resample(predTree,theClassifTask,resampleStrat,measures=list(f1, gmean))
resample(predLogit,theClassifTask,resampleStrat,measures=list(f1, gmean))
resample(predNeural,theClassifTask,resampleStrat,measures=list(f1, gmean))
resample(Naive,theClassifTask,resampleStrat,measures=list(f1, gmean))
resample(predkNN,theClassifTask,resampleStrat,measures=list(f1, gmean))
resample(predSVM,theClassifTask,resampleStrat,measures=list(f1, gmean))

# Question 8.3 Train the final model
finalTree <- train(learner = predTree, task = theClassifTask)
finalLogit <- train(learner = predLogit, task = theClassifTask)
finalNeural <- train(learner = predNeural, task = theClassifTask)
finalNaive <- train(learner = Naive, task = theClassifTask)
finalkNN <- train(learner = predkNN, task = theClassifTask)
finalSVM <- train(learner = predSVM, task = theClassifTask)

# Question 8.4 Predict in test set!
predictionTree <- predict(finalTree, newdata = income.test)
predictionLogit <- predict(finalLogit, newdata = income.test)
predictionNeural <- predict(finalNeural, newdata = income.test)
predictionNaive <- predict(finalNaive, newdata = income.test)
predictionkNN <- predict(finalkNN, newdata = income.test) 
predictionSVM <- predict(finalSVM, newdata = income.test) 



# Question 9 As a table in your .tex file, report the optimal values of the tuning parameters for each of the algorithms.
print(tunedTree)
print(tunedLogit)
print(tunedNeural)
print(tunedkNN)
print(tunedSVM)

