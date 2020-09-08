# https://www.datacamp.com/community/blog/ai-humility

library(rpart)
library(rpart.plot)
install.packages("yardstick")
library(yardstick)
library(ggplot2)

fakeCustomers <- read.csv(file = '/Users/Andrew/Desktop/Data Science/data/propensity.csv')
head(fakeCustomers)
# 1 Let’s build a simple decision tree
# Now we apply the rpart() function to construct our decision tree. Since you’re accepting all default model parameters, you only need to pass in the model formula Y_AcceptedOffer ~ . and the data to make the tree. However, using the period (Y_AcceptedOffer~.,) in the model formula adds risk to your model’s behavior. Suppose later the underlying training data changes to include additional columns. By using the period the model will simply inherit all columns not defined as the Y-variable. So if you rebuild the model by sourcing this code with data that has changed without explicitly declaring the x-variables, you invite target leakage or overfitting without even knowing it. Thus, its often a good idea to declare x-variables explicitly in the formula. In the end, the resulting fit object is a model which we do not want to simply pass to the IT department. Let’s also define a safe model response when fit gets an unknown value!
fit <- rpart(Y_AcceptedOffer ~ . , data = fakeCustomers)
fit
# 2 Make some predictions
# Let’s make sure our model functions as expected with perfect inputs. At this point in your workflow, you should be assessing model performance against a training and validation set. Here we use predict() on the original data, examine a portion of it with tail(), then construct a simple confusion matrix. Finally, you create a confusion matrix with table() and then nest yardstick’s conf_mat() inside summary() to get 13 model metrics including accuracy. Keep in mind marketers don’t have unlimited budgets, so you should care more about accuracy within the top 1% or 5% of prospects rather than just accuracy alone.
pred <- predict(fit, newdata = fakeCustomers, type = 'class')
results <- data.frame(preds = pred,
                      actuals = fakeCustomers$Y_AcceptedOffer)
tail(results,10)
# confusion matrix
confMat <- table(results$preds, results$actuals)
confMat
summary(conf_mat(confMat))
conf_mat(confMat)
# visual inspection
# In addition to numeric KPI you can inspect the confusion matrix visually with a mosaic plot. In this example, a mosaic plot will have rectangles representing each section of the confusion matrix such as true positives and false positives. The area of each rectangle corresponds to the value from the confusion matrix. This view lets you easily understand how balanced your class assignments are compared to actuals. The code below nests the original confusion matrix in conf_mat and ggplot2’s autoplot function to create a basic mosaic plot.
autoplot(conf_mat(confMat))
# One benefit of using a simple model is that you can interrogate the model’s behavior. For decision trees you can use rpart.plot() function to visualize the result. This plot will let you understand the split values and importance of variables in each node.
# 3 All good right? Not so fast.
# Don’t send this model code to IT and expect a warm response! Sure, it works just fine with these fake prospects—because they are exactly like the training data. Even in normal model building, you usually pass in a partition with similar distributions and certainly the same factor levels. But in reality, data integrity and other factors can be issues with the real incoming data—and they can break your model.
fakeNew <- fakeCustomers[c(6:8),]
fakeNew
predict(fit, fakeNew, type = 'prob')
# Add a layer of protection for your predictions
# In this section, you explore what happens when the make of a car is changed from Lexus to lexus. Data entry errors and mis-keys happen all the time because people are involved. Mis-keying factors and transposing numerical inputs often break models in production, as you’ll see if you run predict(fit, fakeRecord) below.
# Error: factor carMake has new level lexus
# Entry form error
table(fakeCustomers$carMake)
str(fakeCustomers$carMake)
fakeRecord <- fakeNew[1,]
fakeRecord
fakeRecord[,2] <- as.factor('lexus')
predict(fit, newdata = fakeRecord)
# 4 Adding a humble layer to your model
# Let’s add a protective prediction layer by checking the inputs all make sense and if so then call predict(). In this code, you write a wrapper function called humblePredict() that accepts the new observations to be scored. Within a for-loop the function checks:
# - That each row is part of a dataframe using is.data.frame
# - That the dataframe’s columns match the model training formula using the match %in% operator.
# - That the value of the observation in the carMake column is an expected level from model training data. This is another match operator call with %in%
# - Finally, that the 'RecentBalance' column is a numeric value using the is.numeric function.
# If all four of these logical conditions are met then the if statement simply calls predict() as usual. However the logical conditions occur within an if-else statement. Therefore if any of these conditions returns a FALSE, then the false code block executes. In this example the default response is the “safe” model response of “DidNotAccept”. This level is safe because it means the company wouldn’t spend money marketing to this potential customer. Of course in your own work, you could have a more explicit error, use a different model or simply return the average Y value from your training set. The point is that you have complete control of the false code behavior and should ensure that your model has guardrails corresponding to the business need. This type of function wrapping helps you dictate how your model should behave with bad inputs. Do you want an error, a safe value, NA or other output when a model is confronted with bad inputs?
# protection function
humblePredict <- function(x){
  classification <- list()
  for (i in 1:nrow(x)) {
    if(
      is.data.frame(x[i,] == T) &
      all(all.vars(formula(fit)[-2]) %in% names(x[i,])) == T &
      x[i, grep('carMake', names(x[i,]))] %in% unlist(attributes(fit)$xlevels) == T &
      is.numeric(x[i, grep('RecentBalance', names(x[i,]))])
    ) {
      response <- predict(fit, x, type = 'class')
      classification[[i]] <- response
    } else {
      response <- 'DidNotAccept'
      classification[[i]] <- response
    }
  }
  return(unlist(classification))
}
humblePredict(fakeRecord)
# how it works
x <- fakeRecord; x
is.data.frame(x)
all.vars(formula(fit)[-2])
names(x)
all(all.vars(formula(fit)[-2]) %in% names(x))
formula(fit)
formula(fit)[-2]
all.vars(formula(fit))
all.vars(formula(fit)[-2])
grep('carMake', names(x))
x[, grep('carMake', names(x))] 
x[, grep('carMake', names(x))] %in% unlist(attributes(fit)$xlevels)
attributes(fit)
attributes(fit)$xlevels
attributes(fit)$xlevels$carMake
unlist(attributes(fit)$xlevels)
is.numeric(x[, grep('RecentBalance', names(x))])
x[, grep('RecentBalance', names(x))]
grep('RecentBalance', names(x))
x[,3]
# 5 Final thoughts
# This is just the tip of the iceberg for making models more robust in production. You can write code within humblePredict to change outlier numeric inputs or change factor levels to the most frequent if a level is unknown. If you want to learn more, start with both the testthat() and assertive() libraries for unit testing and run-time testing respectively. No model should be sent to IT without assertions or at least documentation for safe behaviors.











rpart.plot(fit, roundint = F)
