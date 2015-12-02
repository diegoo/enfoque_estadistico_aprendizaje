library(glmnet)										# load the package			
load("hiv.rda")										# HIV data				
class(hiv.train)									# The data are stored as a list 	
names(hiv.train)									# The names of the list elements are x and y   
dim(hiv.train$x)									# The explanatory data consists of 704 observations of
											# 208 binary mutation variables		
head(hiv.train[[1]])									# Look at the explanatory data		
head(hiv.train[[2]])									# Look at the response data: changes in susceptibility to antiviral drugs
fit=glmnet(hiv.train$x,hiv.train$y)							# fit the model				
plot(fit,xvar="lambda", main="HIV model coefficient paths")				# Plot the paths for the fit
fit											# look at the fit for each coefficient	
											#					
cv.fit=cv.glmnet(hiv.train$x,hiv.train$y)						# Perform cross validation on the fited model
plot(cv.fit)										# Plot the mean sq error for the cross validated fit as a function
											# of lambda the shrinkage parameter	
											# First vertical line indicates minimal mse
											# Second vertical line is one sd from mse: indicates a smaller model
											# is "almost as good" as the minimal mse model
tpred=predict(fit,hiv.test$x)								# Predictions on the test data
mte=apply((tpred-hiv.test$y)^2,2,mean)							# Compute mse for the predictions
points(log(fit$lambda),mte,col="blue",pch="*")						# overlay the mse predictions on the plot
legend("topleft",legend=c("10 fold CV","Test"),pch="*",col=c("red","blue"))

# Each curve represents a coefficient in the model. The x axis is a function of lambda, the regularization penalty parameter. The y axis gives the value of the coefficient. The graph shows how the coefficients “enter the model” (become non-zero) as lambda changes. The following code, based on an example from the webinar, produces the plot and also shows how easy it is to perform cross-validation.
# http://blog.revolutionanalytics.com/2013/05/hastie-glmnet.html
# hiv.coefficients.plot.png
# hiv article: http://www.pnas.org/content/103/46/17355.full.pdf
# hastie slides: http://web.stanford.edu/~hastie/TALKS/enet_talk.pdf
