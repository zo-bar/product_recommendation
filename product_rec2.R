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

############################### Load data #########################################
set.seed(309)
df           <- fread("data/train_ver2.csv",nrows=-1)
test.df      <- fread("data/test_ver2.csv",nrows=-1)
df           <- as.data.frame(df)
test.df      <- as.data.frame(test.df)

# these rows are totally incomplete!
rows.to.remove <- which(is.na(df$antiguedad))
df <- df[which(!1:nrow(df) %in% rows.to.remove),]
remove(rows.to.remove)

################## Make temp merge df and test df for data preprocessing ##########
m.df <- rbind(df[, colnames(test.df)], test.df)

# convert dates to dates format
m.df$fecha_dato <- as.POSIXct(strptime(m.df$fecha_dato,format="%Y-%m-%d"))
m.df$fecha_alta <- as.POSIXct(strptime(m.df$fecha_alta,format="%Y-%m-%d"))
unique(m.df$fecha_dato)

# Generate feature "month" for month of purchase
m.df$month <- month(m.df$fecha_dato)

# Checking if there are NA columns
sapply(m.df,function(x)any(is.na(x)))
# TRUE -> need data cleaning

# Fix unusual age
m.df$age[(df$age < 18)]  <- mean(m.df$age[(m.df$age >= 18) & (m.df$age <=30)],na.rm=TRUE)
m.df$age[(df$age > 100)] <- mean(m.df$age[(m.df$age >= 30) & (m.df$age <=100)],na.rm=TRUE)
m.df$age[is.na(df$age)]  <- median(m.df$age,na.rm=TRUE)
m.df$age                 <- round(m.df$age)

# - SOME UNUSED VALUES - JUST REMOVE FEATURES
m.df <- m.df %>% select(-tipodom,-cod_prov)

# - GROSS INCOME
sum(is.na(m.df$renta))
# many missing values - try to fill them by region

# draw gross income by region plot
m.df %>%
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
new.incomes <-m.df %>%
  select(nomprov) %>%
  merge(m.df %>%
          group_by(nomprov) %>%
          summarise(med.income=median(renta,na.rm=TRUE)),by="nomprov") %>%
  select(nomprov,med.income) %>%
  arrange(nomprov)
m.df <- arrange(m.df,nomprov)
m.df$renta[is.na(m.df$renta)] <- new.incomes$med.income[is.na(m.df$renta)]
rm(new.incomes)

# value for all region Alava is NA, fill it with total median
m.df$renta[is.na(m.df$renta)] <- median(m.df$renta,na.rm=TRUE)
m.df <- arrange(m.df,fecha_dato)

# - FILLING EMPTY STRINGS
# get data range for feature
char.cols <- names(m.df)[sapply(m.df,is.character)]
for (name in char.cols){
  print(sprintf("Unique values for %s:", name))
  print(unique(m.df[[name]]))
  cat('\n')
}
remove(char.cols)
remove(name)

# Fill empty strings with Unknown values or most appropriate looking at the previous table
m.df$indfall[m.df$indfall==""]                 <- "N"
m.df$tiprel_1mes[m.df$tiprel_1mes==""]         <- "A"
m.df$indrel_1mes[m.df$indrel_1mes==""]         <- "1"
m.df$indrel_1mes[m.df$indrel_1mes=="P"]        <- "5" # change to just numbers because it currently contains letters and numbers
m.df$indrel_1mes                             <- as.factor(as.integer(m.df$indrel_1mes))
m.df$pais_residencia[m.df$pais_residencia==""] <- "UNKNOWN"
m.df$sexo[m.df$sexo==""]                       <- "UNKNOWN"
m.df$ult_fec_cli_1t[m.df$ult_fec_cli_1t==""]   <- "UNKNOWN"
m.df$ind_empleado[m.df$ind_empleado==""]       <- "UNKNOWN"
m.df$indext[m.df$indext==""]                   <- "UNKNOWN"
m.df$indresi[m.df$indresi==""]                 <- "UNKNOWN"
m.df$conyuemp[m.df$conyuemp==""]               <- "UNKNOWN"
m.df$segmento[m.df$segmento==""]               <- "UNKNOWN"
m.df$nomprov[m.df$nomprov==""]                 <- "UNKNOWN"

# MONTH->NUMERIC
m.df               <- m.df %>% arrange(fecha_dato)
m.df$month.id      <- as.numeric(factor((m.df$fecha_dato)))
m.df$month.next.id <- m.df$month.id + 1

# cols used for learning:
cols <- colnames(m.df)
# output colunms
cols <- cols[which(!cols %in% c("feature", "status"))]
# month and fecha_data are represented in month.id
cols <- cols[which(!cols %in% c("month","month.next.id", "fecha_dato"))]
# TODO: make fecha_data categorical and use for modeling
# for now remove fecha_alta
cols <- cols[which(!cols %in% c("feacha_alta"))]
# TODO: check ncodpres same for test and train sets
cols <- cols[which(!cols %in% c("ncodpers"))]

catcols <- c("ind_empleado", "pais_residencia", "sexo", "ind_nuevo","indrel", 
             "ult_fec_cli_1t", "indrel_1mes", "tiprel_1mes", "indresi", "indext", 
             "conyuemp", "canal_entrada", "indfall", "nomprov", "ind_actividad_cliente", "segmento")
for (col in catcols) {
  m.df[,col] <- as.factor(m.df[,col])
}
remove(col)

#################### Split df to test and train again ##############
test.df <- m.df[(nrow(df)+1:nrow(m.df)),]
df <- cbind(m.df[1:nrow(df),], df[,which(!colnames(df) %in% colnames(test.df))])
remove(m.df)

############################ Process train data #####################
features          <- grepl("ind_+.*ult.*",names(df))
df[,features]     <- lapply(df[,features],function(x)as.integer(round(x)))
df$total.services <- rowSums(df[,features],na.rm=TRUE)

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



################### Process test data ##############################
sapply(test.df,function(x)any(is.na(x)))

# Only 1 and 3 values in indrel_1mes are in use for test - maybe use only those values in training data? (or match with neighbors)
# OK , 106 rows only are not in c(1,3) in train data
# only 23 rows are missing value in test set
# 929565 out of 929615 values = 1. Fill na's with 1's
# fill missing values with 1
test.df$indrel_1mes[which(is.na(test.df$indrel_1mes))] <- 1



########################### MODELING #####################################
# split data by feature
features <- unique(df[,"feature"])
fcol <- which(colnames(df) == "feature")

# print how much data on each feature we have
# for(f in features) {  print(paste(f, ":", length(which(df[,fcol] == f))))}

# rows per feature
outrows <- c()
for (o in features) {
  outrows[[o]] <- which(df[,fcol] == o)
}
remove(o)

# output
outputcol <- "status"
output <- as.factor(df[,outputcol])
output.binary <- ifelse(output == "Added", 1, 0)


########## RPART #################
library(rpart)
rpart.train <- data.frame(cbind(df[,cols], status = output.binary))

rpart.predict <- list()
for (i in 1:length(outrows)) {
  df.sub <- rpart.train[outrows[[i]],]
  rpart.model <- rpart(status ~ ., data=df.sub, method='anova')
  rpart.predict[[i]] <- predict(rpart.model, newdata=test.df)
}




