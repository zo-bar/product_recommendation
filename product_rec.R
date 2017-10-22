setwd("~/projects/datascience/product_rec")
#data <- read.csv('data/train_ver2.csv', quote="\"", comment.char="", stringsAsFactors = FALSE)
#data1000000 <- read.csv('data/train_1000000.csv', quote="\"", comment.char="", stringsAsFactors = FALSE)
data100000 <- read.csv('data/train_100000.csv', quote="\"", comment.char="", stringsAsFactors = FALSE)
test.data <- read.csv('data/test_ver2.csv', quote="\"", comment.char="", stringsAsFactors = FALSE)

################## Pre-process data ################################
df <- data.frame(data100000)
test.df <- data.frame(test.data)

trainrows <- read.csv("data/trainrows09_5.csv")[,1] #sample(1:nrow(df),nrow(df)*0.9)
valrows <- read.csv("data/valrows09_5.csv")[,1] #(which(!1:nrow(df) %in% c(trainrows, testrows))) write.csv(file='testrows.csv', testrows, row.names = FALSE)


## TODO:
# - generate train/test rows (test rows - predicted dates (2016-05-28))
# - split input/output
# - set seed
# - make factors
# - rfImpute()
#

# ideas:
# - is it important how long ago was product purchased? (make date factor as factor -or- as number -or- scip it)

