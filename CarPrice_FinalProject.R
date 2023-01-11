## Car Price Data
carprice<- read.csv("carprice_assignment.csv",header=TRUE,stringsAsFactors = TRUE)

####check for missing data###
is.na(carprice)

####### summary statistics ###########
summary(carprice)


###Summarize Numeric Data####
summary(Filter(is.numeric,carprice))


#######BOX PLOTS#########

# boxplot 1  
boxplot(carprice$carbody ~ carprice$drivewheel, color = "blue", xlab = "Drivewheel", 
        ylab = "Carbody", main = "Distribution of Carbody by Drivewheel")
# boxplot 2
boxplot(carprice$citympg ~ carprice$enginetype, # numeric.var ~ categorical.var
        xlab = "Enginetype", ylab = "Citympg",
        main = "Distribution of City MPG by Enginetype")


####BAR CHARTS #####

#1: Drivewheel 
data.for.plot <- aggregate(carprice$price,                # variable for aggregation
                           by = list(carprice$drivewheel), 
                           FUN = length, 
                           drop = FALSE)      

# rename the variables in data.for.plot for DRIVEWHEEL 
names(data.for.plot) <- c("drivewheel","CountofDrivewheels")

# create the bar chart
barplot(height = data.for.plot$CountofDrivewheel, # value for bar height
        names.arg = data.for.plot$drivewheel,       # label the bars
        xlab = "Drivewheel", ylab = "number of cars",  # axis titles
        main = "Drivewheel Count")             # chart title


##2: Car Body 
data.for.plot <- aggregate(carprice$price,                # variable for aggregation
                           by = list(carprice$carbody), 
                           FUN = length, 
                           drop = FALSE)      

# rename the variables in data.for.plot
names(data.for.plot) <- c("CarBody","CountofCarBody")

# create the bar chart
barplot(height = data.for.plot$CountofCarBody, # value for bar height
        names.arg = data.for.plot$CarBody,       # label the bars
        xlab = "carbody", ylab = "number of cars",  # axis titles
        main = "Car Body Count")             # chart title



## check for multicollinearity ##
library(gplots)
colfunc <- colorRampPalette(c("green","white","red"))
heatmap.2(cor(Filter(is.numeric,carprice), use = "complete.obs"), Rowv = FALSE, Colv = FALSE,
          dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), col = colfunc(15),
          cellnote = round(cor(Filter(is.numeric, carprice), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


##HISTOGRAMS 

####### histogram of carlength#######
hist(carprice$carlength,  # the numeric variable of interest
     ylim = c(0, 200), # defines the y-axis range
     xlab = "carlength")


####### histogram of carheight#######
hist(carprice$carheight,  # the numeric variable of interest
     ylim = c(0, 200), # defines the y-axis range
     xlab = "carheight")


####### histogram of carwidth#######
hist(carprice$carwidth,  # the numeric variable of interest
     ylim = c(0, 200), # defines the y-axis range
     xlab = "carwidth")

####### histogram of horsepower#######
hist(carprice$horsepower,  # the numeric variable of interest
     ylim = c(0, 200), # defines the y-axis range
     xlab = "horsepower")


##SCATTER PLOTS##
####### scatter plot of curbweight vs price#######
input <- carprice[,c('curbweight','price')]
plot(x = input$curbweight,y = input$price,
     xlab = "curbweight",
     ylab = "price",
     xlim = c(1400,4100),
     ylim = c(5000,60000),              
     main = "curbweight vs price"
)

####### scatter plot of horsepower vs price#######
input2 <- carprice[,c('horsepower','price')]
plot(x = input2$horsepower,y = input2$price,
     xlab = "horsepower",
     ylab = "price",
     xlim = c(50,200),
     ylim = c(5000,60000),             
     main = "horsepower vs price"
)

####### scatter plot of enginesize vs price#######
input3 <- carprice[,c('enginesize','price')]
plot(x = input3$enginesize,y = input3$price,
     xlab = "enginesize",
     ylab = "price",
     xlim = c(50,200),
     ylim = c(5000,60000),             
     main = "enginesize vs price"
)

####### scatter plot of carlength vs price#######
input4 <- carprice[,c('carlength','price')]
plot(x = input4$carlength,y = input4$price,
     xlab = "carlength",
     ylab = "price",
     xlim = c(50,200),
     ylim = c(5000,60000),             
     main = "carlength vs price"
)




# REGRESSION TREE #

summary(carprice)
t(t(names(carprice)))
t.carprice <- carprice[-c(1,3)]
summary(t.carprice)
# partition the data
set.seed(1)
train.index <- sample(rownames(t.carprice), nrow(t.carprice) * 0.7)
carprice.train <- t.carprice[train.index, ]
valid.index <- setdiff(rownames(t.carprice), train.index)
carprice.valid <- carprice[valid.index, ]

####### full regression tree #######
library(rpart)
library(rpart.plot)
library(caret)
carprice.tree <- rpart(price ~ .,              # quantitative outcome ~ predictors
                     data = carprice.train,    # training data
                     method = "anova",       # regression tree - this is the default
                     cp = 0,                 # complexity parameter
                     minsplit = 2)           # minimum observations required to attempt split

prp(carprice.tree,          # tree model
    type = 1,             # label all the nodes
    extra = 1,            # show # of observations in each node
    varlen = -10,         # truncate variable names to 10 characters
    box.col = ifelse(carprice.tree$frame$var == "<leaf>", 'gray', 'white'))  # make leaves gray, decision nodes white

# count number of leaves
length(carprice.tree$frame$var[carprice.tree$frame$var == "<leaf>"])

# assess predictive performance
full.tree.pred <- predict(carprice.tree, carprice.valid)
RMSE(full.tree.pred, carprice.valid$price)


####### best pruned tree #######
cv.carprice.tree <- rpart(price ~ .,
                        data = carprice.train,
                        cp = 0,
                        minsplit = 2,
                        xval = 10)            # 10-fold cross validation
options(scipen = 999, digits = 8)
printcp(cv.carprice.tree)

minerror <- min(cv.carprice.tree$cptable[ ,4 ])                                      # find minimum xerror
minerrorstd <- cv.carprice.tree$cptable[cv.carprice.tree$cptable[,4] == minerror, 5]   # and its corresponding xstd
minerror
minerrorstd
cv.carprice.tree$cptable[cv.carprice.tree$cptable[,4] == minerror, ]

# get list of trees where xerror is less than minerror + minerrorstd
simplertrees <- cv.carprice.tree$cptable[cv.carprice.tree$cptable[,4] < minerror + minerrorstd, ]

# use the cp from the simplest of those trees
bestcp <- simplertrees[1, 1]
carprice.pruned <- prune(cv.carprice.tree, cp = bestcp)
prp(carprice.pruned, type = 1, extra = 1, varlen = -10, digits = -3,
    box.col = ifelse(carprice.pruned$frame$var == "<leaf>", 'gray', 'white'))
length(carprice.pruned$frame$var[carprice.pruned$frame$var == "<leaf>"])

pruned.valid.rt.pred <- predict(carprice.pruned, newdata = carprice.valid)
RMSE(pruned.valid.rt.pred, carprice.valid$price)

# trying different cps from different trees
carprice.pruned2 <- prune(cv.carprice.tree, cp =  0.00874362965763152)
prp(carprice.pruned2, type = 1, extra = 1, varlen = -10, digits = -3,
    box.col = ifelse(carprice.pruned2$frame$var == "<leaf>", 'gray', 'white'))
length(carprice.pruned2$frame$var[carprice.pruned$frame$var == "<leaf>"])

pruned.valid.rt.pred2 <- predict(carprice.pruned2, newdata = carprice.valid)
RMSE(pruned.valid.rt.pred2, carprice.valid$price) #smallest rmse




### VARIABLE SELECTION MODELS #####
t(t(names(t2.carprice)))
t2.carprice <- t.carprice[-c(14)]
set.seed(1)
train.rows <- sample(rownames(t2.carprice), nrow(t2.carprice)*0.6)
train.data <- t2.carprice[train.rows, ]
valid.rows <- setdiff(rownames(t2.carprice), train.rows)
valid.data <- t2.carprice[valid.rows, ]

## Exhaustive Search ## 
library(leaps)
# use regsubsets() to perform exhaustive search
carprice.search <- regsubsets(price ~ .,                   # full model formula
                            data = train.data,           # training dataset
                            nbest = 1,                   # number of subsets of each size
                            nvmax = ncol(train.data),    # maximum number of variables to consider
                            method = "exhaustive")       # specify exhaustive search

search.summary <- summary(carprice.search)      # define summary for easy reference
search.summary$which                          # show which variables are included in each best subset

# compare the models returned by the exhaustive search
options(digits = 8)
t(t(search.summary$rsq))          # shows r-squared values
t(t(search.summary$adjr2))        # shows adjusted r-squared values
t(t(search.summary$cp))           # shows Mallow's Cp values
t(t(search.summary$bic))          # shows BIC values

carprice.best1 <- lm(price ~ enginesize,   # remove 3 predictors not in 8 variable model 
                   data = train.data)
options(scipen = 999)
carprice.best1summary <- summary(carprice.best1)
carprice.best1summary

####### Forward Selection #######
carprice.lm <- lm(price ~ ., data = train.data)         # full model with all predictors
carprice.lm.null <- lm(price ~ 1, data = train.data)    # intercept-only model

# use step() to run forward selection
carprice.lm.fwd <- step(carprice.lm.null,                                     # initial model
                      scope = list(carprice.lm.null, upper = carprice.lm),    # range of models
                      direction = "forward")                              # forward selection

summary(carprice.lm.fwd)

####### Backward Elimination #######
carprice.lm.back <- step(carprice.lm,                    # start with full model
                       direction = "backward")       # backward elimination

summary(carprice.lm.back)

####### Stepwise Regression #######
carprice.lm.step <- step(carprice.lm.null,                                    # initial model
                       scope = list(carprice.lm.null, upper = carprice.lm),   # range of models
                       direction = "both")                                # stepwise regression

summary(carprice.lm.step)

####### Prediction Performance #######
library(forecast)
# fwd prediction
valid.fwd.pred <- predict(carprice.lm.fwd, valid.data)
options(digits = 6)
accuracy(valid.fwd.pred, valid.data$price)

# back prediction
valid.back.pred <- predict(carprice.lm.back, valid.data)
options(digits = 6)
accuracy(valid.back.pred, valid.data$price)

# step prediction
valid.step.pred <- predict(carprice.lm.step, valid.data)
options(digits = 6)
accuracy(valid.step.pred, valid.data$price)

# exhaustive prediction
valid.full.pred <- predict(carprice.best1, valid.data)
options(digits = 6)
accuracy(valid.full.pred, valid.data$price)







#### neural net car price + RMSE ######
summary(carprice)
#remove ID, symboling, carname, enginetype, cylindernumber, fuelsystem
carprice.df <- carprice[,-c(1:3,15,16,18)]
summary(carprice.df)
#create dummy variable for fueltype, aspiration, doornumber, carbody, drivewheel, enginlocation
library(fastDummies)
carprice.df <- dummy_cols(carprice.df, 
                          select_columns = c("fueltype","aspiration"),
                          remove_first_dummy = TRUE,
                          remove_selected_columns = TRUE)
carprice.df <- dummy_cols(carprice.df, 
                          select_columns = c("doornumber","carbody"),
                          remove_first_dummy = TRUE,
                          remove_selected_columns = TRUE)
carprice.df <- dummy_cols(carprice.df, 
                          select_columns = c("drivewheel","enginelocation"),
                          remove_first_dummy = TRUE,
                          remove_selected_columns = TRUE)
#check skewness for number varible in carprice.df for predictor
library(e1071)
skewness(carprice.df$wheelbase) # >1
skewness(carprice.df$carlength)
skewness(carprice.df$carwidth)
skewness(carprice.df$carheight)
skewness(carprice.df$curbweight)
skewness(carprice.df$enginesize)
skewness(carprice.df$boreratio)
skewness(carprice.df$stroke)
skewness(carprice.df$compressionratio) #>2
skewness(carprice.df$horsepower) #>1
skewness(carprice.df$peakrpm)
skewness(carprice.df$citympg)
skewness(carprice.df$highwaympg)

# apply a log transformation to highly skewed predictors.
carprice.df$wheelbase <- log(carprice.df$wheelbase + 1)
carprice.df$compressionratio <- log(carprice.df$compressionratio + 1)
carprice.df$horsepower<- log(carprice.df$horsepower + 1)

# partition the data 60% training and 40% valid
set.seed(1)

train.index <- sample(rownames(carprice.df), nrow(carprice.df) * 0.6)
carprice.train <- carprice.df[train.index, ]
valid.index <- setdiff(rownames(carprice.df), train.index)
carprice.valid <- carprice.df[valid.index, ]

# convert all variables (INCLUDING QUANTITATIVE OUTCOME) to a 0-1 scale
carprice.train.norm <- carprice.train
carprice.valid.norm <- carprice.valid

cols <- colnames(carprice.train)
for (i in cols) {
  carprice.valid.norm[[i]] <- 
    (carprice.valid.norm[[i]] - min(carprice.train[[i]])) / (max(carprice.train[[i]]) - min(carprice.train[[i]]))
  carprice.train.norm[[i]] <- 
    (carprice.train.norm[[i]] - min(carprice.train[[i]])) / (max(carprice.train[[i]]) - min(carprice.train[[i]]))
}
summary(carprice.train.norm)
summary(carprice.valid.norm)



####### neural net with 1 hidden layer of 3 nodes #######
library(neuralnet)
carprice.nn.3 <- neuralnet(price ~ .,                  # categorical outcome ~ predictors 
                           data = carprice.train.norm,   # data for training model    
                           linear.output = FALSE,      # assume relationship is nonlinear
                           hidden = 3)                 # a single hidden layer containing 3 nodes
plot(carprice.nn.3, rep = "best")

predict.nn.3 <- predict(carprice.nn.3, carprice.valid.norm)

head(predict.nn.3)

# convert back to original scale
minprice <- min(carprice.train$price)
maxprice <- max(carprice.train$price)
actpred <- data.frame(actual = carprice.valid$price, 
                      predicted = minprice + predict.nn.3*(maxprice - minprice))
head(actpred)

library(caret)
RMSE(actpred$predicted, actpred$actual) #3173.13

####### neural net with 2 hidden layers of 3 nodes #######
carprice.nn.3.3 <- neuralnet(price ~ .,                  # categorical outcome ~ predictors
                             data = carprice.train.norm,   # data for training model
                             linear.output = FALSE,      # assume relationship is nonlinear
                             hidden = c(3,3))            # 2 hidden layers of 3 nodes each
plot(carprice.nn.3.3, rep = "best")

predict.nn.3.3 <- predict(carprice.nn.3.3, carprice.valid.norm)

# convert back to original scale
actpred <- data.frame(actual = carprice.valid$price, 
                      predicted = minprice + predict.nn.3.3*(maxprice - minprice))
RMSE(actpred$predicted, actpred$actual) #4329.759

####### neural net with 1 hidden layer of 10 nodes #######
carprice.nn.10 <- neuralnet(price ~ .,                  # categorical outcome ~ predictors
                            data = carprice.train.norm,   # data for training model
                            linear.output = FALSE,      # assume relationship is nonlinear
                            hidden = 10)            # 1 hidden layer of 10 nodes
plot(carprice.nn.10, rep = "best")

predict.nn.10 <- predict(carprice.nn.10, carprice.valid.norm)

# convert back to original scale
actpred <- data.frame(actual = carprice.valid$price, 
                      predicted = minprice + predict.nn.10*(maxprice - minprice))
RMSE(actpred$predicted, actpred$actual) #3163.62

####### loop through different hidden layer sizes #######
RMSE.df <- data.frame(n = seq(1, 20, 1), RMSE.k = rep(0, 20))

for (i in 1:20) {
  carprice.nn <- neuralnet(price ~ .,                  # categorical outcome ~ predictors
                           data = carprice.train.norm,   # data for training model
                           linear.output = FALSE,      # assume relationship is nonlinear
                           hidden = i)            # 1 hidden layer of 10 nodes
  predict.nn <- predict(carprice.nn, carprice.valid.norm)
  
  # convert back to original scale
  actpred <- data.frame(actual = carprice.valid$price, 
                        predicted = minprice + predict.nn*(maxprice - minprice))
  RMSE.df[i,2] <- RMSE(actpred$predicted, actpred$actual)
}

RMSE.df #for i from 1-20, n= 3 is the lowest





####### neural net with 1 hidden layer of 10 hidden layer of 13 nodes #######
carprice.nn.13 <- neuralnet(price ~ .,                  # categorical outcome ~ predictors
                            data = carprice.train.norm,   # data for training model
                            linear.output = FALSE,      # assume relationship is nonlinear
                            hidden = c(3))            # 1 hidden layer of 10 nodes
plot(carprice.nn.13, rep = "best")

predict.nn.13 <- predict(carprice.nn.13, carprice.valid.norm)

# convert back to original scale
actpred <- data.frame(actual = carprice.valid$price, 
                      predicted = minprice + predict.nn.13*(maxprice - minprice))
RMSE(actpred$predicted, actpred$actual) #3271.8
