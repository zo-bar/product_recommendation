######### INCOMPLETE! No predictions here



setwd("~/projects/datascience/product_rec")

library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Themes will be used for plots
my_theme <- theme_bw() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))
my_theme_dark <- theme_dark() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

# Load data
set.seed(309)
df           <- fread("data/train_ver2.csv",nrows=-1)

# these rows are totally incomplete!
rows.to.remove <- which(is.na(df$antiguedad))
df <- df[which(!1:nrow(df) %in% rows.to.remove),]

# convert dates to dates format
df$fecha_dato <- as.POSIXct(strptime(df$fecha_dato,format="%Y-%m-%d"))
df$fecha_alta <- as.POSIXct(strptime(df$fecha_alta,format="%Y-%m-%d"))
unique(df$fecha_dato)

# Generate feature "month" for month of purchase
df$month <- month(df$fecha_dato)

# Checking if there are NA columns
sapply(df,function(x)any(is.na(x)))
# TRUE -> need data cleaning

# Fix unusual age
df$age[(df$age < 18)]  <- mean(df$age[(df$age >= 18) & (df$age <=30)],na.rm=TRUE)
df$age[(df$age > 100)] <- mean(df$age[(df$age >= 30) & (df$age <=100)],na.rm=TRUE)
df$age[is.na(df$age)]  <- median(df$age,na.rm=TRUE)
df$age                 <- round(df$age)

# - SOME UNUSED VALUES - JUST REMOVE FEATURES
df <- df %>% select(-tipodom,-cod_prov)

# - GROSS INCOME
sum(is.na(df$renta))
# many missing values - try to fill them by region

# draw gross income by region plot
df %>%
  filter(!is.na(renta)) %>%
  group_by(nomprov) %>%
  summarise(med.income = median(renta)) %>%
  arrange(med.income) %>%
  mutate(city=factor(nomprov,levels=nomprov)) %>% # the factor() call prevents reordering the names
  ggplot(aes(x=city,y=med.income)) + 
  geom_point(color="#c60b1e") + 
  guides(color=FALSE) + 
  xlab("City") +
  ylab("Median Income") +  
  my_theme + 
  theme(axis.text.x=element_blank(), axis.ticks = element_blank()) + 
  geom_text(aes(x=city,y=med.income,label=city),angle=90,hjust=-.25) +
  theme(plot.background=element_rect(fill="#c60b1e"),
        panel.background=element_rect(fill="#ffc400"),
        panel.grid =element_blank(),
        axis.title =element_text(color="#ffc400"),
        axis.text  =element_text(color="#ffc400"),
        plot.title =element_text(color="#ffc400",size=32)) +
  ylim(c(50000,200000)) +
  ggtitle("Income Distribution by City")

# fill gross income with region median value
new.incomes <-df %>%
  select(nomprov) %>%
  merge(df %>%
          group_by(nomprov) %>%
          summarise(med.income=median(renta,na.rm=TRUE)),by="nomprov") %>%
  select(nomprov,med.income) %>%
  arrange(nomprov)
df <- arrange(df,nomprov)
df$renta[is.na(df$renta)] <- new.incomes$med.income[is.na(df$renta)]
rm(new.incomes)

# value for all region Alava is NA, fill it with total median
df$renta[is.na(df$renta)] <- median(df$renta,na.rm=TRUE)
df <- arrange(df,fecha_dato)

# - FILLING EMPTY STRINGS
# get data range for feature
char.cols <- names(df)[sapply(df,is.character)]
for (name in char.cols){
  print(sprintf("Unique values for %s:", name))
  print(unique(df[[name]]))
  cat('\n')
}

# Fill empty strings with Unknown values or most appropriate looking at the previous table
df$indfall[df$indfall==""]                 <- "N"
df$tiprel_1mes[df$tiprel_1mes==""]         <- "A"
df$indrel_1mes[df$indrel_1mes==""]         <- "1"
df$indrel_1mes[df$indrel_1mes=="P"]        <- "5" # change to just numbers because it currently contains letters and numbers
df$indrel_1mes                             <- as.factor(as.integer(df$indrel_1mes))
df$pais_residencia[df$pais_residencia==""] <- "UNKNOWN"
df$sexo[df$sexo==""]                       <- "UNKNOWN"
df$ult_fec_cli_1t[df$ult_fec_cli_1t==""]   <- "UNKNOWN"
df$ind_empleado[df$ind_empleado==""]       <- "UNKNOWN"
df$indext[df$indext==""]                   <- "UNKNOWN"
df$indresi[df$indresi==""]                 <- "UNKNOWN"
df$conyuemp[df$conyuemp==""]               <- "UNKNOWN"
df$segmento[df$segmento==""]               <- "UNKNOWN"
df$nomprov[df$nomprov==""]                 <- "UNKNOWN"

# CONVERT FEATURES TO NUMERIC DUMMY INDICATORS
features          <- grepl("ind_+.*ult.*",names(df))
df[,features]     <- lapply(df[,features],function(x)as.integer(round(x)))
df$total.services <- rowSums(df[,features],na.rm=TRUE)

# MONTH->NUMERIC
df               <- df %>% arrange(fecha_dato)
df$month.id      <- as.numeric(factor((df$fecha_dato)))
df$month.next.id <- df$month.id + 1

#############
# functions says if client added, dropped or mainteined poduct on month-to-month base 
status.change <- function(x){
  if ( length(x) == 1 ) { # if only one entry exists, I'll assume they are a new customer and therefore are adding services
    label = ifelse(x==1,"Added","Maintained")
  } else {
    diffs <- diff(x) # difference month-by-month
    diffs <- c(0,diffs) # first occurrence will be considered Maintained, which is a little lazy. A better way would be to check if the earliest date was the same as the earliest we have in the dataset and consider those separately. Entries with earliest dates later than that have joined and should be labeled as "Added"
    label <- rep("Maintained", length(x))
    label <- ifelse(diffs==1,"Added",
                    ifelse(diffs==-1,"Dropped",
                           "Maintained"))
  }
  label
}

# add feature based on function
df[,features] <- lapply(df[,features], function(x) return(ave(x,df$ncodpers, FUN=status.change)))

# get rid of "maintained" status as useless
interesting <- rowSums(df[,features]!="Maintained")
df          <- df[interesting>0,]
df          <- df %>% gather(key=feature,
                             value=status,
                             ind_ahor_fin_ult1:ind_recibo_ult1)
df          <- filter(df,status!="Maintained")


####################### MY CODE ############################
##################### PREPARE TEST DATA ########################################
test.df           <- fread("data/test_ver2.csv",nrows=-1)
sapply(test.df,function(x)any(is.na(x)))

# process dates
test.df$fecha_dato <- as.POSIXct(strptime(test.df$fecha_dato,format="%Y-%m-%d"))
test.df$fecha_alta <- as.POSIXct(strptime(test.df$fecha_alta,format="%Y-%m-%d"))
unique(test.df$fecha_dato)

# Generate feature "month" for month of purchase
test.df$month <- month(test.df$fecha_dato)

# Fix unusual age
test.df$age[(test.df$age < 18)]  <- mean(df$age[(df$age >= 18) & (df$age <=30)],na.rm=TRUE)
test.df$age[(test.df$age > 100)] <- mean(df$age[(df$age >= 30) & (df$age <=100)],na.rm=TRUE)
test.df$age[is.na(test.df$age)]  <- median(df$age,na.rm=TRUE)
test.df$age                 <- round(test.df$age)

# - SOME UNUSED VALUES - JUST REMOVE FEATURES
test.df <- test.df %>% select(-tipodom,-cod_prov)

# renta, indrel_1mes, cod_prov have NA's
# FILL RENTA BY MEAN OF TRAIN SET!!
test.df$nomprov[test.df$nomprov==""] <- "UNKNOWN"
new.incomes <-test.df %>%
  select(nomprov) %>%
  merge(df %>%
          group_by(nomprov) %>%
          summarise(med.income=median(renta,na.rm=TRUE)),by="nomprov") %>%
  select(nomprov,med.income) %>%
  arrange(nomprov)
test.df <- arrange(test.df,nomprov)
test.df$renta[is.na(test.df$renta)] <- new.incomes$med.income[is.na(test.df$renta)]
rm(new.incomes)
#np <- unique(new.incomes$nomprov)
#ni <- unique(new.incomes)
#mean(new.incomes$med.income[which(new.incomes$nomprov == np[[1]])])
#rentana <- which(is.na(test.df$renta))

# TODO only 1and 3 values in indrel_1mes are in use for test - maybe use only those values in training data? (or match with neighbors)
# OK , 106 rows only are not in c(1,3) in train data
# only 23 rows are missing value in test set
# 929565 out of 929615 values = 1. Fill na's with 1's
# fill missing values with 1
test.df$indrel_1mes[which(is.na(test.df$indrel_1mes))] <- 1

# delete features, as deleted in train set
test.df <- test.df %>% select(-tipodom,-cod_prov)

# check no more missing values
sapply(test.df,function(x)any(is.na(x)))

# Fill empty strings with Unknown values or most appropriate looking at the previous table
test.df$indfall[test.df$indfall==""]                 <- "N"
test.df$tiprel_1mes[test.df$tiprel_1mes==""]         <- "A"
test.df$indrel_1mes[test.df$indrel_1mes==""]         <- "1"
test.df$indrel_1mes[test.df$indrel_1mes=="P"]        <- "5" # change to just numbers because it currently contains letters and numbers
test.df$indrel_1mes                             <- as.factor(as.integer(test.df$indrel_1mes))
test.df$pais_residencia[test.df$pais_residencia==""] <- "UNKNOWN"
test.df$sexo[test.df$sexo==""]                       <- "UNKNOWN"
test.df$ult_fec_cli_1t[test.df$ult_fec_cli_1t==""]   <- "UNKNOWN"
test.df$ind_empleado[test.df$ind_empleado==""]       <- "UNKNOWN"
test.df$indext[test.df$indext==""]                   <- "UNKNOWN"
test.df$indresi[test.df$indresi==""]                 <- "UNKNOWN"
test.df$conyuemp[test.df$conyuemp==""]               <- "UNKNOWN"
test.df$segmento[test.df$segmento==""]               <- "UNKNOWN"
test.df$nomprov[test.df$nomprov==""]                 <- "UNKNOWN"

# CONVERT FEATURES TO NUMERIC DUMMY INDICATORS
features          <- grepl("ind_+.*ult.*",names(test.df))
test.df[,features]     <- lapply(test.df[,features],function(x)as.integer(round(x)))
test.df$total.services <- rowSums(test.df[,features],na.rm=TRUE)

# Add new features to test set
# Generate feature "month" for month of purchase
test.df$month <- month(test.df$fecha_dato)

# MONTH->NUMERIC
test.df               <- test.df %>% arrange(fecha_dato)
test.df$month.id      <- as.numeric(factor((test.df$fecha_dato)))
test.df$month.next.id <- test.df$month.id + 1


########################### MODELING #####################################
# TODO: convert fecha_alta to category (say: 6 month, 1 year, 5 years, more)

# print how much data on each feature we have
features <- unique(df[,"feature"])
fcol <- which(colnames(df) == "feature")

for(f in features) {print(paste(f, ":", length(which(df[,fcol] == f))))}

# rows per feature
outrows <- c()
for (o in features) {outrows[[o]] <- which(df[,fcol] == o)}

# cols used for learning:
cols <- colnames(df)
# output colunms
cols <- cols[which(!cols %in% c("feature", "status"))]
# month and fecha_data are represented in month.id
cols <- cols[which(!cols %in% c("month","month.next.id", "fecha_dato"))]
# TODO: make fecha_data categorical and use for modeling
# for now remove fecha_alta
cols <- cols[which(!cols %in% c("feacha_alta"))]

catcols <- c("ind_empleado", "pais_residencia", "sexo", "ind_nuevo","indrel", 
             "ult_fec_cli_1t", "indrel_1mes", "tiprel_1mes", "indresi", "indext", 
             "conyuemp", "canal_entrada", "indfall", "nomprov", "ind_actividad_cliente", "segmento")
for (col in catcols) {
  df[,col] <- as.factor(df[,col])
}

# output
outputcol <- "status"
output <- as.factor(df[,outputcol])

######### GLM #################
glm.output <- ifelse(output == "Added", 1, 0)
glm.test <- data.matrix(test.df[,cols])


glm.test.predict <- list()

for (i in length(outrows)) {
  df.sub <- df[outrows[[i]], cols]
  glm.train <- data.matrix(df.sub)
  glm.output <- ifelse(output == "Added", 1, 0)
  glm.cv <- cv.glmnet(glm.train, glm.output[outrows[[i]]], alpha = 1)
  glm.test.predict[i] <- predict(glm.cv, newx = glm.test, s="lambda.min")
  
}

######### RPART ###################

rpart.model <- rpart(status ~ ., data=df, method='anova')


#glm.test.predict <- predict(glm.cv, newx = glm.test, s="lambda.min")








