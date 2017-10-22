setwd("~/projects/datascience/product_rec")

library(data.table)
library(xgboost)
library(tidyr)

df.clean <- data.frame(fread('data/train_df_cleaned.csv'))
df.clean <- df.clean[order(df.clean$ncodpers),]
features <- colnames(df.clean[24:47])

## Add more columns
# age changed
# was not in system
# features added/removed

change.period = c(1,2,3,4)
change.cols  <- c('age', 'ind_actividad_cliente', features)
added.cols <- c()
train.month <- 5
for (cp in change.period) {
  print(paste(Sys.time(), 'Start next period:', cp))
  pers.per.period <- df.clean$ncodpers[which(df.clean$month.id == train.month & 
                                               df.clean$ncodpers %in% df.clean$ncodpers[which(df.clean$month.id == (train.month-cp))])]
  print(paste(Sys.time(), 'Number of persons:', length(pers.per.period)))
  for (cc in change.cols) {
    col_name = paste0(cc,'__',cp,'__m_change')
    col_num <- (ncol(df.clean) + 1)
    #print(paste(Sys.time(), "Start next column", col_name))
    
    prev.values <- as.numeric(as.factor(df.clean[which(df.clean$month.id == (train.month-cp) & df.clean$ncodpers %in% pers.per.period), cc]))
    cur.values  <- as.numeric(as.factor(df.clean[which(df.clean$month.id == train.month & df.clean$ncodpers %in% pers.per.period), cc]))
    if (!is.na(sum(cur.values)) & !is.na(sum(prev.values)) & all.equal(cur.values, prev.values) != TRUE) {
      df.clean[,col_num] <- rep(0, nrow(df.clean))
      df.clean[which(df.clean$month.id == train.month & df.clean$ncodpers %in% pers.per.period), col_num] <- cur.values - prev.values
      colnames(df.clean)[col_num] <- col_name
      added.cols <- c(added.cols, col_name)
      print(paste(Sys.time(), 'Column', col_name, 'added, params:', length(df.clean$ncodpers[which(df.clean[,col_num] != 0)])))
    }
  }
  #col_name = paste0('new_pers__',cp,'m_change')
  #col_num <- (ncol(df.clean) + 1)
  #df.clean[,col_num] <- rep(0, nrow(df.clean))
  #df.clean[which(df,clean$ncodpers %in% pers.per.period), colnum] <- 1
  #colnames(df.clean)[col_num] <- col_name
  #added.cols <- c(added.cols, col_name)
  #print(paste(Sys.time(), "Start next column", col_name))
}

# read test set
test.df              <- as.data.frame(fread("data/test_df_cleaned.csv",nrows=-1))
#test.df              <- test.df[order(test.df$ncodpers), ]
# add new columns to test set (with time shift)
test.month = 17
for (col in added.cols) {
  print(paste(Sys.time(),'Processing next col on test set:', col))
  test.df[,col] <- rep(0, nrow(test.df))
  #col_name = paste0('__', cc,'__',cp,'m_change')
  cc <- strsplit(col,"__")[[1]][1]
  cp <- as.numeric(strsplit(col,"__")[[1]][2])
  pers.per.period <- df.clean$ncodpers[which(df.clean$ncodpers %in% test.df$ncodpers & 
                                               df.clean$month.id == (test.month - cp) )]
                                             #& df.clean$ncodpers %in% df.clean$ncodpers[which(df.clean$month.id == (test.month-cp))])]
  
  prev.values <- df.clean[which(df.clean$month.id == (test.month-cp) & df.clean$ncodpers %in% pers.per.period), cc]
  cur.values  <- df.clean[which(df.clean$month.id == (test.month) & df.clean$ncodpers %in% pers.per.period), cc] #test.df[which(test.df$ncodpers %in% pers.per.period), cc]
  test.df[which(test.df$ncodpers %in% pers.per.period), col] <- cur.values - prev.values
}

# take data for may and june only
df.may <- df.clean[which(df.clean$month.id == 5),]
df.june <- df.clean[which(df.clean$month.id == 6),]
# take data for only those customers who were in system both months
common.pers <- df.may$ncodpers[which(df.may$ncodpers %in% df.june$ncodpers)]
df.may <- df.may[which(df.may$ncodpers %in% common.pers),]
df.june <- df.june[which(df.june$ncodpers %in% common.pers),]
# make same ordering
df.may <- df.may[order(df.may$ncodpers),]
df.june <- df.june[order(df.june$ncodpers),]
# check sets are equal
all.equal(df.may$ncodpers, df.june$ncodpers)
# get added features
diff <- df.june[,features] - df.may[,features]
diff[diff < 0] <- 0
length(which(rowSums(diff) > 0))
# for training set get only those features that were added in june 
df            <- df.june[which(rowSums(diff) > 0),]
df[,features] <- diff[which(rowSums(diff) > 0),]

prev.features.cols <- c()
# add full set of features a month before training
for (i in 1:length(features)) {
  col_name <- paste0('prev_',features[i])
  prev.features.cols <- c(prev.features.cols, col_name)
}
df[,prev.features.cols] <- df.may[which(df.may$ncodpers %in% df$ncodpers),features]


# check for sorting!!!!
head(test.df$ncodpers)
test.prev.features <- df.clean[which(df.clean$ncodpers %in% test.df$ncodpers & df.clean$month.id == 17), c('ncodpers', features)]
head(test.prev.features$ncodpers)
test.prev.features <- test.prev.features[match(test.df$ncodpers, test.prev.features$ncodpers),]
head(test.prev.features$ncodpers)

test.df[,prev.features.cols] <- test.prev.features[, features]


# clean
remove(df.june)
remove(df.may)
remove(diff)


# compare test persons's metrics in test/train sets
#df.clean.test <- df.clean[which(df.clean$ncodpers %in% test.df$ncodpers & df.clean$fecha_dato == '2016-05-28'),colnames(test.df[2:23])]
#for (col in colnames(df.orig.test.1)) {
#  print(col)
#  print(all.equal(df.clean.test[,col], test.df[,col]))
#}
#test.df$age_changed <- df.clean.test$age_changed
#remove(df.clean.test)



## Process categorical cols
catcols <- c("ind_empleado", "pais_residencia", "sexo", "ind_nuevo","indrel", 
             "ult_fec_cli_1t", "indrel_1mes", "tiprel_1mes", "indresi", "indext", 
             "conyuemp", "canal_entrada", "indfall", "nomprov", "ind_actividad_cliente", 
             "segmento", "month")
m.df <- rbind(df[,catcols], test.df[,catcols])
for (col in catcols) {
  m.df[,col] <- as.factor(m.df[,col])
}
remove(col)
df[, catcols] <- m.df[1:nrow(df), catcols]
test.df[, catcols] <- m.df[(nrow(df)+1):nrow(m.df), catcols]
remove(m.df)

## Make train set
data.cols <- which(!colnames(df) %in% features)
train.df <- melt(df, id=data.cols)
train.df <- train.df[which(train.df$value == 1),]
train.df$feature <- train.df$variable
train.df$value    <- NULL
train.df$variable <- NULL

features <- as.character(unique(train.df[, 'feature']))


## Make feature id column for model target
train.df$feature.id <- rep(0, length(train.df$ncodpers))
for (i in 1:length(features)) {
  train.df$feature.id[which(train.df$feature == features[i])] <- i
}


train.cols <- colnames(test.df[,which(colnames(test.df) %in% colnames(train.df))])
train.cols <- setdiff(train.cols,c("V1", "fecha_dato", "fecha_alta", "ult_fec_cli_1t", "indrel_1mes", 'ncodpers', 'month', 'month.id', 'month.next.id', "tiprel_1mes", "feature", "feature.id"))

xgb.train <- data.matrix(train.df[, train.cols])
xgb.train.out <- data.matrix(as.factor(train.df$feature.id - 1))
dtrain <- xgb.DMatrix(data = xgb.train, label = xgb.train.out)

xgb.params <- list(max.depth = 5, 
                   eta = 0.03, 
                   min_child_weight = 1,
                   colsample_bytree = 1,
                   subsample = 0.8,
                   alpha = 1,
                   gamma = 0,
                   objective = "multi:softprob",
                   num_class=length(features))
# 5, 0.03, 1,1, 0.8, 1, 0, 127 train cols    - 446rounds

print(paste(Sys.time(), "Start training..."))
#xgb.cv <- xgb.cv(data = dtrain, params = xgb.params, nthread = 4, nround = 500, 
#                 nfold = 5, metrics=list("mlogloss"), verbose = 1, 
#                 print.every.n = 3)

#nrounds <- as.integer(which(xgb.cv$test.mlogloss.mean == min(xgb.cv$test.mlogloss.mean))[1]/0.8)
nrounds <- 446
xgb.fit <- xgboost(data = xgb.train, label = xgb.train.out, params = xgb.params, 
                   nrounds = nrounds, verbose = 1, print.every.n = 3, nthread = 4)

print(paste(Sys.time(), "Start predicting..."))
test <- test.df[, train.cols]
xgb.test <- data.matrix(test)

pred <- predict(xgb.fit, xgb.test)
pred <- matrix(pred, ncol=length(features), byrow=TRUE)

## remove currenlty added features
current.features <- df.clean[intersect(which(df.clean$fecha_dato == max(df.clean$fecha_dato)), 
                                       which(df.clean$ncodpers %in% test.df$ncodpers)), 
                             c('ncodpers', features)]
# sort table of customer's current features by test set 
current.features <- current.features[match(test.df$ncodpers, current.features$ncodpers),]

# update prediction to exclude current customer's features
pred.new <- pred - current.features[features]

forecast     <- matrix(ncol = 8, nrow = nrow(pred.new))
forecast[,1] <- test.df$ncodpers

print(paste(Sys.time(), "Building top 7 predictions matrix..."))
for (i in 1:nrow(pred.new)) {
  if (i %% 10000 == 0) print(paste(Sys.time(), i))
  forecast[i,2:8] <- order(pred.new[i,], decreasing = TRUE)[1:7]
}
print(paste(Sys.time(), "Top 7 predictions matrix completed"))
print(paste(Sys.time(), "Processing index to feature completed"))
for (c in 2:ncol(forecast)) {
  for (f in 1:length(features)) {
    forecast[which(forecast[,c] == f),c] <- features[f]
  }
}
print(paste(Sys.time(), "Uniting columns..."))
forecast <- data.frame(forecast)
forecast <- unite(forecast, col='added_products', c(2:8), sep = " ", remove = TRUE)
colnames(forecast) <- c('ncodpers', 'added_products')

print(paste(Sys.time(), "Writing file..."))
sub_file <- paste0('submissions/sub_new_xgb_', Sys.time(),'.csv')
write.csv(forecast, sub_file, row.names = FALSE)
print(paste(Sys.time(), 'Done'))
