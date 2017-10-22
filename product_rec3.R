setwd("~/projects/datascience/product_rec")

library(data.table)
library(xgboost)

df       <- as.data.frame(fread("data/df_by_feature.csv", nrows=-1))
test.df  <- as.data.frame(fread("data/test_df_cleaned.csv",nrows=-1))

# process factors
catcols <- c("ind_empleado", "pais_residencia", "sexo", "ind_nuevo","indrel", 
             "ult_fec_cli_1t", "indrel_1mes", "tiprel_1mes", "indresi", "indext", 
             "conyuemp", "canal_entrada", "indfall", "nomprov", "ind_actividad_cliente", "segmento", "month")
m.df <- rbind(df[,catcols], test.df[,catcols])
for (col in catcols) {
  m.df[,col] <- as.factor(m.df[,col])
}
remove(col)
df[, catcols] <- m.df[1:nrow(df), catcols]
test.df[, catcols] <- m.df[(nrow(df)+1):nrow(m.df), catcols]
remove(m.df)

#### list of features
features <- unique(df[,"feature"])

############################## EXISTING CUSTOMERS #########################################
# 0. Run plain xgb on full set (Kind of fake)


# 1. For each feature check how long on average it's used, how long are the gaps,
# -> for each person check how long ago she used feature and if it's time to renew
# 1a. gaps/usage intervals might depend on the month (say, season)

# 2. If a person has a set of features by a date - what's her next steps?
# 2a. Features that go together / antiphase. 
# 2b. Features that go sequentially 
# 2b - month 'a' adds 'feature1' -> month 'b' adds 'feature2')
# 2b - month 'a' dropps 'feature1' -> month 'b' adds 'feature2')

# 3. Fake - if a person doesn't have most used features - add them

# 4. what features can be added to user in general (and aren't still added) - add top N 
# 5. Popular features of the month

# Combination: 
#


################## MOST POPULAR FEATURES ################
feature.popularity <- c()
for (i in 1:length(features)) {
     feature.popularity[i] <- length(intersect(which(df$feature == features[i]), which(df$status == 'Added')))
 }

feature.popularity.bymonth <- c()
for (i in 1:length(features)) {
  feature.popularity.bymonth[i] <- length(intersect(intersect(which(df$feature == features[i]), which(df$status == 'Added'))), which(df$month == 6))
}

df.orig <- data.frame(fread('data/train_ver2.csv'))
customer.features <- df.orig[intersect(which(df.orig$fecha_dato == max(df.orig$fecha_dato)), 
                                   which(df.orig$ncodpers %in% test.df$ncodpers)), 
                         c('ncodpers', features)]
customer.features <- customer.features[order(customer.features$ncodpers),]

feature.popularity.index <- order(feature.popularity, decreasing = TRUE)

test.persons <- sort(unique(test.df$ncodpers))
forecast <- data.frame(cbind(test.persons, rep("", length(test.persons))))
colnames(forecast) <- c('ncodpers', 'added_products')
forecast$ncodpers       <- as.character(forecast$ncodpers)
forecast$added_products <- as.character(forecast$added_products)

# prepare result file
sub_file <- paste0('submissions/sub_new_xgb_', Sys.time(),'.csv')
write.csv(forecast, sub_file, row.names = FALSE)

for (k in 1:length(forecast$ncodpers)) {
  if (k %% 10000 == 0) {
    print(paste(Sys.time(),'next', k, 'persons processed'))
    write.csv(forecast, sub_file, row.names = FALSE)
  }
  
  new.features <- which(customer.features[k,features] == 0)
  forecast$added_products[k] <- paste0(features[intersect(feature.popularity.index, new.features)[1:7]], collapse=' ')
}
write.csv(forecast, sub_file, row.names = FALSE)

################## XGB PREDICTION ######################
train.cols <- c("ind_empleado", "pais_residencia", "sexo", "age", "ind_nuevo", 
                "antiguedad", "indrel", "tiprel_1mes", "indresi",
                "indext","conyuemp", "canal_entrada", "indfall", "nomprov",
                "ind_actividad_cliente", "renta", "segmento", "month", "total.services")


#### prepare test sets
test <- test.df[order(test.df$ncodpers),c('ncodpers', train.cols)]
xgb.test <- data.matrix(test)

# make table of probability of feature being added next for test set:
df.probs <- data.frame(matrix(0, ncol = 1 + length(features), nrow = length(test.df$ncodpers)))
colnames(df.probs) <- c('ncodpers', features)

folder = paste0('temp',Sys.time(), '/')
dir.create(folder)

#### RUN XGB PREDICTION ####
xgb.params <- list(max.depth = 5, 
                   eta = 0.1, 
                   min_child_weight = 1,
                   colsample_bytree = 1,
                   subsample = 0.8,
                   alpha = 1,
                   gamma = 1,
                   objective = "reg:linear")


for (i in 1:length(features)) {
  train.rows <- which(df$feature == features[i])
  train <- df[train.rows,]
  # set output
  output <- rep(0, length(train.rows))
  # if a feature was added by a customer - assign 1 to corresponding output
  output[which(train$status == 'Added')] <- 1
  # if a feature was dropped by a customer - assign -1 to corresponding output
  output[which(train$status == 'Dropped')] <- -1
  
  print(paste(Sys.time(), "Start next feature:", features[i], '(train sample length:', length(output),')'))
  
  ### XGB
  xgb.train <- data.matrix(train[,train.cols])
  xgb.train.out <- data.matrix(output)
  dfull <- xgb.DMatrix(data = xgb.train, label = xgb.train.out)
  xgb.cv <- xgb.cv(data = dfull, params = xgb.params, nthread = 4, nround = 3000, 
                 nfold = 5, early.stop.round = 15, maximize = FALSE, verbose = 1, 
                  print.every.n = 30, nthread = 4)
  
  nrounds <- as.integer(which(xgb.cv$test.rmse.mean == min(xgb.cv$test.rmse.mean))[1]/0.8)
  xgb.fit <- xgboost(data = xgb.train, label = xgb.train.out, params = xgb.params, 
                     nrounds = nrounds, verbose = 1, print.every.n = 30, nthread = 2)
  
  
  pred <- predict(xgb.fit, xgb.test)
  df.probs[,features[i]] <- pred
  
  write.csv(cbind(test$ncodpers, pred), paste0(folder, features[i], '.csv'), row.names = FALSE)
}

df.probs.scaled <- df.probs
for (i in 1:length(features)) {
  df.probs.scaled[, features[i]] <- scale(df.probs.scaled[, features[i]])
}

print(all.equal(df.probs.scaled$ncodpers, df.probs.init$ncodpers))

forecast <- data.frame(cbind(test$ncodpers, rep("", length(test$ncodpers))))
colnames(forecast) <- c('ncodpers', 'added_products')
forecast$ncodpers       <- as.character(forecast$ncodpers)
forecast$added_products <- as.character(forecast$added_products)

#### prepare storage for our result
df.orig <- data.frame(fread('data/train_ver2.csv'))
df.probs.init <- df.orig[intersect(which(df.orig$fecha_dato == max(df.orig$fecha_dato)), 
                                   which(df.orig$ncodpers %in% test.df$ncodpers)), 
                         c('ncodpers', features)]
df.probs.init <- df.probs.init[order(df.probs.init$ncodpers),]

# prepare result file
sub_file <- paste0('submissions/sub_new_xgb_', Sys.time(),'.csv')
write.csv(forecast, sub_file, row.names = FALSE)

# fill result file
for (i in 1:length(forecast$ncodpers)) {
  if (i %% 10000 == 0) {
    print(paste(Sys.time(),'next', i, 'persons processed'))
    write.csv(forecast, sub_file, row.names = FALSE)
  }
  
  #new.features <- features[which(df.probs.init[which(df.probs.init$ncodpers==df.probs.scaled$ncodpers[i]),features]==0)]
  # believe that ncodpers's are sorted the same in init and prediction
  new.features <- features[which(df.probs.init[i,features] == 0)]
  forecast$added_products[i] <- paste0(colnames(sort(df.probs.scaled[i, new.features],decreasing = TRUE)[1:7]), collapse=' ')
}

write.csv(forecast, sub_file, row.names = FALSE)





################## XGB PREDICTION ON JUNE + CLASSIFICATION ######################
train.cols <- c("ind_empleado", "pais_residencia", "sexo", "age", "ind_nuevo", 
                "antiguedad", "indrel", "tiprel_1mes", "indresi",
                "indext","conyuemp", "canal_entrada", "indfall", "nomprov",
                "ind_actividad_cliente", "renta", "segmento")


#### prepare train/test sets
df.june <- df[which(df$month == 6),]
train.df <- unique(df.june[order(df.june$ncodpers), c(train.cols, 'ncodpers', 'feature', 'status')])
test <- test.df[order(test.df$ncodpers), c('ncodpers', train.cols)]
xgb.test <- data.matrix(test)

# make table of probability of feature being added next for test set:
df.probs <- data.frame(matrix(0, ncol = 1 + length(features), nrow = length(test.df$ncodpers)))
colnames(df.probs) <- c('ncodpers', features)
df.probs$ncodpers <- test.df$ncodpers
df.probs <- df.probs[order(df.probs$ncodpers),]

folder = paste0('xgb_full_june_class',Sys.time(), '/')
dir.create(folder)

#### RUN XGB PREDICTION ####
xgb.params <- list(max.depth = 5, 
                   eta = 0.1, 
                   min_child_weight = 1,
                   colsample_bytree = 1,
                   subsample = 0.8,
                   alpha = 1,
                   gamma = 1,
                   objective = "binary:logistic")

for (i in 1:length(features)) {
  train.rows <- which(train.df$feature == features[i])
  train <- train.df[train.rows, ]
  # set output
  output <- rep(0, length(train.rows))
  # if a feature was added by a customer - assign 1 to corresponding output
  output[which(train$status == 'Added')] <- 1
  # if a feature was dropped by a customer - assign 0 to corresponding output
  output[which(train$status == 'Dropped')] <- 0
  
  print(paste(Sys.time(), "Start next feature:", features[i], '(train sample length:', length(output),')'))
  
  ### XGB
  xgb.train <- data.matrix(train[,train.cols])
  xgb.train.out <- data.matrix(output)
  dfull <- xgb.DMatrix(data = xgb.train, label = xgb.train.out)
  xgb.cv <- xgb.cv(data = dfull, params = xgb.params, nthread = 4, nround = 3000, 
                   nfold = 5, metrics=list("error", "auc"), early.stop.round = 15, maximize = FALSE, verbose = 1, 
                   print.every.n = 30, nthread = 4)
  
  nrounds <- as.integer(which(xgb.cv$test.error.mean == max(xgb.cv$test.error.mean))[1]/0.8)
  xgb.fit <- xgboost(data = xgb.train, label = xgb.train.out, params = xgb.params, 
                     nrounds = nrounds, verbose = 1, print.every.n = 30, nthread = 2)
  
  
  pred <- predict(xgb.fit, xgb.test)
  df.probs[,features[i]] <- pred
  
  write.csv(cbind(test$ncodpers, pred), paste0(folder, features[i], '.csv'), row.names = FALSE)
}

df.probs.init <- data.frame(fread('data/existing_features.csv'))
df.probs[, features] <- df.probs[, features] - df.probes.init[, features]

# make list of feature popularity - in case no features are considered to be predicted just add popular features
feature.popularity <- c()
for (i in 1:length(features)) {
  feature.popularity[i] <- length(intersect(which(df$feature == features[i]), which(df$status == 'Added')))
}
feature.popularity.index <- order(feature.popularity, decreasing = TRUE)
remove(feature.popularity)

# check lists are synchronized
print(all.equal(df.probs$ncodpers, df.probs.init$ncodpers))

# prepare result list
forecast <- data.frame(cbind(test$ncodpers, rep("", length(test$ncodpers))))
colnames(forecast) <- c('ncodpers', 'added_products')
forecast$ncodpers       <- as.character(forecast$ncodpers)
forecast$added_products <- as.character(forecast$added_products)

# prepare result file
sub_file <- paste0('submissions/sub_new_xgb_', Sys.time(),'.csv')
write.csv(forecast, sub_file, row.names = FALSE)

# fill result file
for (i in 1:length(forecast$ncodpers)) {
  if (i %% 10000 == 0) {
    print(paste(Sys.time(),'next', i, 'persons processed'))
    write.csv(forecast, sub_file, row.names = FALSE)
  }
  
  #new.features <- features[which(df.probs.init[which(df.probs.init$ncodpers==df.probs.scaled$ncodpers[i]),features]==0)]
  # believe that ncodpers's are sorted the same in init and prediction
  #new.features <- features[which(df.probs.init[i,features] == 0)]
  #forecast$added_products[i] <- paste0(colnames(sort(df.probs[i, new.features],decreasing = TRUE)[1:7]), collapse=' ')
  #forecast$added_products[i] <- paste0(colnames(sort(df.probs[i, ],decreasing = TRUE)[1:7]), collapse=' ')
  pred.features <- df.probs[i,]
  most.popular.features <- features[intersect(feature.popularity.index, which(df.probs[i] > 0.1))]
  if (length(pred.features) < 7) {
    forecast$added_products[i] <- paste(c(pred.features, most.popular.features[1:(7-length(pred.features))]), collapse=' ')
  } else {
    forecast$added_products[i] <- paste(pred.features, collapse=' ')
  }
}

write.csv(forecast, sub_file, row.names = FALSE)






############################ XGB ON FULL SET ####################################
# prepare train set
df       <- as.data.frame(fread('data/train_df_cleaned.csv'))
test.df  <- as.data.frame(fread("data/test_df_cleaned.csv",nrows=-1))

# process factors
catcols <- c("ind_empleado", "pais_residencia", "sexo", "ind_nuevo","indrel", 
             "ult_fec_cli_1t", "indrel_1mes", "tiprel_1mes", "indresi", "indext", 
             "conyuemp", "canal_entrada", "indfall", "nomprov", "ind_actividad_cliente", "segmento", "month")
m.df <- rbind(df[,catcols], test.df[,catcols])
for (col in catcols) {
  m.df[,col] <- as.factor(m.df[,col])
}
remove(col)
df[, catcols] <- m.df[1:nrow(df), catcols]
test.df[, catcols] <- m.df[(nrow(df)+1):nrow(m.df), catcols]
remove(m.df)

#### list of features
features <- colnames(df[29:52])#unique(df[,"feature"])

##### will mix with most popular features
df.by.feature <- as.data.frame(fread("data/df_by_feature.csv", nrows=-1))
feature.popularity <- c()
for (i in 1:length(features)) {
  feature.popularity[i] <- length(intersect(which(df.by.feature$feature == features[i]), which(df.by.feature$status == 'Added')))
}
remove(df.by.feature)
feature.popularity.index <- order(feature.popularity, decreasing = TRUE)
remove(feature.popularity)

# prepare train/test sets
train.cols <- c("ind_empleado", "pais_residencia", "sexo", "age", "ind_nuevo", 
                "antiguedad", "indrel", "tiprel_1mes", "indresi",
                "indext","conyuemp", "canal_entrada", "indfall", "nomprov",
                "ind_actividad_cliente", "renta", "segmento", "month")
test <- test.df[order(test.df$ncodpers),c('ncodpers', train.cols)]
xgb.test <- data.matrix(test)

# make table of probability of feature being added next for test set:
df.probs <- data.frame(matrix(0, ncol = 1 + length(features), nrow = length(test.df$ncodpers)))
colnames(df.probs) <- c('ncodpers', features)

folder = paste0('temp',Sys.time(), '/')
dir.create(folder)

#### RUN XGB PREDICTION ####
xgb.params <- list(max.depth = 5, 
                   eta = 0.3, 
                   min_child_weight = 1,
                   colsample_bytree = 1,
                   subsample = 0.8,
                   alpha = 1,
                   gamma = 1,
                   objective = "reg:linear")

max.train.size <- 1000000

for (i in 1:length(features)) {
  # set sample train rows
  train.rows <- which(df[,features[i]] == 1)
  if (length(train.rows) > max.train.size/10) {
    train.rows <- sample(train.rows, as.integer(max.train.size/10))
  }
  train.rows <- c(train.rows, sample(which(!is.na(df[, features[i]])), length(train.rows)*10))
  # set output
  train  <- df[train.rows, train.cols]
  output <- df[train.rows, features[i]]
  
  print(paste(Sys.time(), "Start next feature:", features[i], '(train sample length:', length(output),')'))
  
  ### XGB
  xgb.train <- data.matrix(train)
  xgb.train.out <- data.matrix(output)
  dfull <- xgb.DMatrix(data = xgb.train, label = xgb.train.out)
  xgb.cv <- xgb.cv(data = dfull, params = xgb.params, nthread = 4, nround = 3000, 
                   nfold = 5, early.stop.round = 7, maximize = FALSE, verbose = 1, 
                   print.every.n = 30, nthread = 4)
  
  nrounds <- as.integer(which(xgb.cv$test.rmse.mean == min(xgb.cv$test.rmse.mean))[1]/0.8)
  xgb.fit <- xgboost(data = xgb.train, label = xgb.train.out, params = xgb.params, 
                     nrounds = nrounds, verbose = 1, print.every.n = 30, nthread = 2)
  
  
  pred <- predict(xgb.fit, xgb.test)
  df.probs[,features[i]] <- pred
  
  write.csv(cbind(test$ncodpers, pred), paste0(folder, features[i], '.csv'), row.names = FALSE)
}

df.probs.scaled <- df.probs
for (i in 1:length(features)) {
  df.probs.scaled[, features[i]] <- scale(df.probs.scaled[, features[i]])
}

forecast <- data.frame(cbind(test$ncodpers, rep("", length(test$ncodpers))))
colnames(forecast) <- c('ncodpers', 'added_products')
forecast$ncodpers       <- as.character(forecast$ncodpers)
forecast$added_products <- as.character(forecast$added_products)

#### prepare storage for our result
df.probs.init <- df[intersect(which(df$fecha_dato == max(df$fecha_dato)), 
                                   which(df$ncodpers %in% test.df$ncodpers)), 
                         c('ncodpers', features)]
df.probs.init <- df.probs.init[order(df.probs.init$ncodpers),]

print(all.equal(df.probs.scaled$ncodpers, df.probs.init$ncodpers))

# prepare result file
sub_file <- paste0('submissions/sub_new_xgb_', Sys.time(),'.csv')
write.csv(forecast, sub_file, row.names = FALSE)

# fill result file
for (i in 1:length(forecast$ncodpers)) {
  if (i %% 10000 == 0) {
    print(paste(Sys.time(),'next', i, 'persons processed'))
    write.csv(forecast, sub_file, row.names = FALSE)
  }
  
  # believe that ncodpers's are sorted the same in init and prediction
  new.features <- which(df.probs.init[i,features] == 0)
  pred.features <- colnames(sort(df.probs.scaled[i, new.features[which(df.probs.scaled[i, new.features] > 0.5)]], decreasing = TRUE))
  most.popular.features <- features[intersect(feature.popularity.index, new.features)]
  if (length(pred.features) < 7) {
    forecast$added_products[i] <- paste(c(pred.features, most.popular.features[1:(7-length(pred.features))]), collapse=' ')
  } else {
    forecast$added_products[i] <- paste(pred.features, collapse=' ')
  }
}

write.csv(forecast, sub_file, row.names = FALSE)




############################ NEW CUSTOMERS ######################################
# Pick new customers from train set and make a model for them for 
# our new 3263 customers in test set

train.pers <- unique(df$ncodpers)
test.pers  <- unique(test.df$ncodpers)

# most of persons in test data are in train data
length(test.pers)
length(which(!test.pers %in% train.pers))
# these 3263 customers might be new

# cols used for learning for new customers:
# month and fecha_data are represented in month.id
# for now remove fecha_alta
# antiguedad and ind_nuevo, as already used for df rows selection
# tiprel_1mes, indrel_1mes - essentially the same
# TODO: total.services - not present in test data; process fecha_alta?
cols.remove.new.customers <- c("month","month.next.id", "fecha_dato", 
                               "feacha_alta", "antiguedad", "ind_nuevo",
                               "tiprel_1mes", "indrel_1mes", "fecha_alta", 
                               "month.id", "tipodom", "cod_prov", "ult_fec_cli_1t",
                               "total.services", "V1")

cols <- colnames(df)[which(!colnames(df) %in% cols.remove.new.customers)]

########### new customers - customer's first month
df.new <- (df[intersect(which(df$antiguedad %in% c(0, 1)), which(df$status=='Added')),])
sapply(df.new ,function(x)any(is.na(x)))

########### split data by feature ########
# list of features
features <- unique(df.new[,"feature"])
# list of customers
df.customers <- unique(df.new[, cols[which(!cols %in% c("feature", "status"))]])

# pick data cols for all new customers
train.cols <- cols[which(!cols %in% c("ncodpers", "feature", "status"))]
df.train <- df.customers[,train.cols]

test.df.new <- test.df[which(!test.df$ncodpers %in% df$ncodpers),]
xgb.pred <- c()
for (i in 1:length(features)) {
  # set output
  output <- rep(0, nrow(df.customers))
  # if a feature was added by a customer - assign 1 to corresponding output
  output[which(df.customers$ncodpers %in% df.new$ncodpers[which(df.new$feature == features[i])])] <- 1
  
  print(paste(Sys.time(), "Start next feature:", features[i], '(train sample:', sum(output),')'))
  ### XGB
  xgb.train <- data.matrix(df.train)
  xgb.train.out <- data.matrix(output)
  xgb.params <- list(max.depth = 5, 
                     eta = 0.1, 
                     min_child_weight = 1,
                     colsample_bytree = 1,
                     subsample = 0.9,
                     alpha = 1,
                     gamma = 1,
                     objective = "reg:linear")
  dfull <- xgb.DMatrix(data = xgb.train, label = xgb.train.out)
  xgb.cv <- xgb.cv(data = dfull, params = xgb.params, nthread = 4, nround = 3000, 
                   nfold = 5, early.stop.round = 7, maximize = FALSE, verbose = 1, 
                   print.every.n = 30)

  nrounds <- which(xgb.cv$test.rmse.mean == min(xgb.cv$test.rmse.mean))[1]
  xgb.fit <- xgboost(data = xgb.train, label = xgb.train.out, params = xgb.params, 
                     nrounds = nrounds, verbose = 1, print.every.n = 30, nthread = 2)
  xgb.test <- data.matrix(test.df.new[,train.cols])
  xgb.pred[[i]] <- predict(xgb.fit, xgb.test)
}


#forecast <- data.frame()
#colnames(forecast) <- c('ncodpers', 'added_products')

forecast <- data.frame(cbind(test.df$ncodpers, rep("", length(test.df$ncodpers))))
colnames(forecast) <- c('ncodpers', 'added_products')
forecast$added_products <- as.character(forecast$added_products)
for (i in 1:length(features)) {
  percentage <- nrow(df.new[which(df.new$feature == features[i]),])/nrow(df.new)
  threshold <- sort(xgb.pred[[i]])[as.integer(length(xgb.pred[[i]])*(1-percentage))]
  #print(paste(percentage, "%", ",threshold:", threshold))
  pers.f <- test.df.new$ncodpers[(which(xgb.pred[[i]] > threshold))]

  #forecast <- rbind(forecast, cbind(ncodpers=pers.f, added_products=rep(features[i], length(pers.f))))
  forecast$added_products[which(forecast$ncodpers %in% pers.f)] <- paste0(forecast$added_products[which(forecast$ncodpers %in% pers.f)], ',', features[i])
  print (paste(Sys.time(), length(pers.f),"features added for feature", features[i]))
}

write.csv(forecast, paste0('submissions/sub_new_xgb_', Sys.time(),'.csv'), row.names = FALSE)




### RPART
#rpart.m <- rpart(status~., data=cbind(df.train, status=output))






################# EXPLORATION ###########################################
#test.pers.cols <- c('fecha_dato', 'month.id','total.services','feature.id','status')

# explore person's behavior
#test.pers.1.df <- df[which(df$ncodpers==test.pers[1]),]
#test.pers.1.test <- test.df[which(test.df$ncodpers==test.pers[1]),]
# what features person 1 added and deleted for the last 17 months
#test.pers.1.df[order(test.pers.1.df$fecha_dato), test.pers.cols]

# explore person's behavior
#test.pers.2.df <- df[which(df$ncodpers==test.pers[2]),]
#test.pers.2.test <- test.df[which(test.df$ncodpers==test.pers[2]),]
# what features person 2 added and deleted for the last 17 months
#test.pers.2.df[order(test.pers.2.df$fecha_dato), test.pers.cols]



# group_by example
#ff <- data.frame(x = as.character(forecast$ncodpers), y=as.character(forecast$added_products), stringsAsFactors=FALSE)
#forecast.collapsed <- aggregate(x=ff$y, by=list(ff$x),paste, collapse=',')
#colnames(forecast.collapsed) <- colnames(forecast)

#fc <- data.frame(cbind(ncodpers=as.character(unique(test.df$ncodpers)), added_products=rep("", length(unique(test.df$ncodpers)))))

#train.pers <- unique(df$ncodpers)
#test.pers  <- unique(test.df$ncodpers[which(test.df$ncodpers %in% df$ncodpers)])

#train.df <- df[which(df$month.id < 17),]
#val.df   <- df[which(df$month.id == 17),]


# check on validation set
# get list of persons who did add feature
#val.ouput <- val.df$ncodpers[intersect(which(val.df$feature == features[i]), which(val.df$status == 'Added'))]
# get list of persons who did add feature
#xgb.pred[[i]] <- predict(xgb.fit, xgb.test)

#hash <- function(x,n) {
#  r<-0
#  for (i in 1:n) {   if (i %in% x) {r <- r+ 2^i }} r}
#colstring <- list()




#### prepare storage for our result
df.orig <- data.frame(fread('data/train_ver2.csv'))
# make table of probability of feature being added next for test set:
df.probs <- df.orig[intersect(which(df.orig$fecha_dato == max(df.orig$fecha_dato)), 
                              which(df.orig$ncodpers %in% test.df$ncodpers)), 
                    c('ncodpers', features)]
# test set and df.probs are ordered the same - by ncodpers
df.probs <- df.probs[order(df.probs$ncodpers),]
# if features is already added, set probability of it's addition to -2
df.probs[,features] <- -2* df.probs[,features]


folder = paste0('temp',Sys.time(), '/')
dir.create(folder)

#### RUN XGB PREDICTION ####
xgb.params <- list(max.depth = 5, 
                   eta = 0.1, 
                   min_child_weight = 1,
                   colsample_bytree = 1,
                   subsample = 0.8,
                   alpha = 1,
                   gamma = 1,
                   objective = "reg:linear")

for (i in 1:length(features)) {
  train.rows <- which(df$feature == features[i])
  train <- df[train.rows,]
  # set output
  output <- rep(0, length(train.rows))
  # if a feature was added by a customer - assign 1 to corresponding output
  output[which(train$status == 'Added')] <- 1
  # if a feature was dropped by a customer - assign -1 to corresponding output
  output[which(train$status == 'Dropped')] <- -1
  
  print(paste(Sys.time(), "Start next feature:", features[i], '(train sample length:', length(output),')'))
  
  ### XGB
  xgb.train <- data.matrix(train[,train.cols])
  xgb.train.out <- data.matrix(output)
  dfull <- xgb.DMatrix(data = xgb.train, label = xgb.train.out)
  xgb.cv <- xgb.cv(data = dfull, params = xgb.params, nthread = 4, nround = 3000, 
                   nfold = 5, early.stop.round = 7, maximize = FALSE, verbose = 1, 
                   print.every.n = 30, nthread = 4)
  
  nrounds <- as.integer(which(xgb.cv$test.rmse.mean == min(xgb.cv$test.rmse.mean))[1]/0.8)
  xgb.fit <- xgboost(data = xgb.train, label = xgb.train.out, params = xgb.params, 
                     nrounds = nrounds, verbose = 1, print.every.n = 30, nthread = 2)
  
  
  pred <- predict(xgb.fit, xgb.test)
  df.probs[,features[i]] <- df.probs[,features[i]] + pred
  
  write.csv(cbind(test$ncodpers, pred), paste0(folder, features[i], '.csv'), row.names = FALSE)
}



forecast <- data.frame(cbind(test$ncodpers, rep("", length(test$ncodpers))))
colnames(forecast) <- c('ncodpers', 'added_products')
forecast$ncodpers       <- as.character(forecast$ncodpers)
forecast$added_products <- as.character(forecast$added_products)

sub_file <- paste0('submissions/sub_new_xgb_', Sys.time(),'.csv')

write.csv(forecast, sub_file, row.names = FALSE)
for (i in 1:length(forecast$ncodpers)) {
  if (i %% 10000 == 0) {
    print(paste(Sys.time(),'next', i, 'persons processed'))
    write.csv(forecast, sub_file, row.names = FALSE)
  }
  forecast$added_products[i] <- paste0(colnames(sort(df.probs[i,features], decreasing = TRUE)[1:7]), collapse=' ')#hash(sort(order(df.probs[i,features], decreasing = TRUE)[1:7]))#
  #paste0(colnames(sort(df.probs[i,features])[1:7]), collapse=',')
}

write.csv(forecast, sub_file, row.names = FALSE)

