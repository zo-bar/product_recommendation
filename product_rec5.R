setwd("~/projects/datascience/product_rec")

library(data.table)
library(xgboost)
library(tidyr)


train.df <- data.frame(fread('data/train_df_with_added_features.csv'))
# read test set
test.df              <- as.data.frame(fread("data/test_df_with_added_features.csv",nrows=-1))
test.df              <- test.df[order(test.df$ncodpers), ]

## Process categorical cols
catcols <- c("ind_empleado", "pais_residencia", "sexo", "ind_nuevo","indrel", 
             "ult_fec_cli_1t", "indrel_1mes", "tiprel_1mes", "indresi", "indext", 
             "conyuemp", "canal_entrada", "indfall", "nomprov", "ind_actividad_cliente", 
             "segmento", "month")
m.df <- rbind(train.df[,catcols], test.df[,catcols])
for (col in catcols) {
  m.df[,col] <- as.factor(m.df[,col])
}
remove(col)
train.df[, catcols] <- m.df[1:nrow(train.df), catcols]
test.df[, catcols] <- m.df[(nrow(train.df)+1):nrow(m.df), catcols]
remove(m.df)

train.cols <- colnames(test.df[,which(colnames(test.df) %in% colnames(train.df))])
train.cols <- setdiff(train.cols,c("V1", "fecha_dato", "fecha_alta", "ult_fec_cli_1t", 
                                   "indrel_1mes", 'ncodpers', 'month', 'month.id', 'month.next.id', 
                                   "tiprel_1mes", "feature", "feature.id"))

xgb.train <- data.matrix(train.df[, train.cols])
xgb.train.out <- data.matrix(as.factor(train.df$feature.id - 1))
dtrain <- xgb.DMatrix(data = xgb.train, label = xgb.train.out)

xgb.params <- list(max.depth = 8, 
                   eta = 0.03, 
                   min_child_weight = 1,
                   colsample_bytree = 1,
                   subsample = 0.8,
                   alpha = 1,
                   gamma = 0,
                   objective = "multi:softprob",
                   num_class=length(features))
print(paste(Sys.time(), "Start training..."))
xgb.cv <- xgb.cv(data = dtrain, params = xgb.params, nthread = 4, nround = 700, 
                 nfold = 5, metrics=list("mlogloss"), verbose = 1, 
                 print.every.n = 3)

nrounds <- as.integer(which(xgb.cv$test.mlogloss.mean == min(xgb.cv$test.mlogloss.mean))[1]/0.8)
#215

xgb.fit <- xgboost(data = xgb.train, label = xgb.train.out, params = xgb.params, 
                   nrounds = nrounds, verbose = 1, print.every.n = 3, nthread = 4)

print(paste(Sys.time(), "Start predicting..."))
test <- test.df[, train.cols]
xgb.test <- data.matrix(test)

pred <- predict(xgb.fit, xgb.test)
pred <- matrix(pred, ncol=length(features), byrow=TRUE)
#pred <- pred[order(test.df$ncodpers),]

## remove currenlty added features
df.clean <- data.frame(fread('data/train_df_cleaned.csv'))
df.clean <- df.clean[order(df.clean$ncodpers),]
current.features <- df.clean[intersect(which(df.clean$fecha_dato == max(df.clean$fecha_dato)), 
                                       which(df.clean$ncodpers %in% test.df$ncodpers)), 
                             c('ncodpers', features)]
pred.new <- pred - current.features[features]

forecast <- matrix(ncol=8, nrow=nrow(pred.new))
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
sub_file <- paste0('submissions/sub_xgb_new_way_', Sys.time(),'.csv')
write.csv(forecast, sub_file, row.names = FALSE)
print(paste(Sys.time(), 'New way Done'))


forecast.new <- forecast

df.orig <- data.frame(fread('data/train_ver2.csv'))
customer.features <- df.orig[intersect(which(df.orig$fecha_dato == max(df.orig$fecha_dato)), 
                                       which(df.orig$ncodpers %in% test.df$ncodpers)), 
                             c('ncodpers', features)]
customer.features <- customer.features[order(customer.features$ncodpers),]

test.persons <- sort(unique(test.df$ncodpers))
forecast <- data.frame(cbind(test.persons, rep("", length(test.persons))))
colnames(forecast) <- c('ncodpers', 'added_products')
forecast$ncodpers       <- as.character(forecast$ncodpers)
forecast$added_products <- as.character(forecast$added_products)

# prepare result file
sub_file <- paste0('submissions/sub_xgb_old_way', Sys.time(),'.csv')
write.csv(forecast, sub_file, row.names = FALSE)

for (k in 1:length(forecast$ncodpers)) {
  if (k %% 10000 == 0) {
    print(paste(Sys.time(),'next', k, 'persons processed'))
    write.csv(forecast, sub_file, row.names = FALSE)
  }
  
  new.features <- which(customer.features[k,features] == 0)
  forecast$added_products[k] <- paste0(features[intersect(order(pred[k,], decreasing=TRUE), new.features)[1:7]], collapse=' ')
}
write.csv(forecast, sub_file, row.names = FALSE)
print (paste(Sys.time(), "Done"))
