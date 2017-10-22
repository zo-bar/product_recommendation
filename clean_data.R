setwd("~/projects/datascience/product_rec")

library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)

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
#m.df               <- m.df %>% arrange(fecha_dato)
m.df$month.id      <- as.numeric(factor((m.df$fecha_dato)))
m.df$month.next.id <- m.df$month.id + 1

#################### Split df to test and train again ##############
test.df <- m.df[(nrow(df)+1:nrow(m.df)),]
df <- cbind(m.df[1:nrow(df),], df[,which(!colnames(df) %in% colnames(test.df))])
remove(m.df)

################## Store result ##################################
write.csv(df, 'data/train_df_cleaned.csv')
