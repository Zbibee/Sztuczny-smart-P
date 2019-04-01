db <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/ecoli/ecoli.data");
colnames(db) <- c('SequenceName','mcg','gvh','lip','chg','aac','alm1','alm2', 'class');
table(db$class)

summary(db)
table(db$class)

library(neuralnet)
set.seed(100)
sample.size <- 236
samples.id <- sample(1:nrow(db), size.sample)
db.train <- db[samples.id,]
db.validation <- db[-samples.id,]
summary(df.train$class)

db.train$cp <- db.train$class == 'cp'
db.train$im <- db.train$class == 'im'
db.train$imL <- db.train$class == 'imL'
db.train$imS <- db.train$class == 'imS'
db.train$imU <- db.train$class == 'imU'
db.train$om <- db.train$class == 'om'
db.train$omL <- db.train$class == 'omL'
db.train$pp <- db.train$class == 'pp'

pie(table(db.train$class))

colnames(db.train)
nn <- neuralnet(cp +  im + imL +imS + imU + om + omL + pp  ~ 
                  mcg + gvh + lip + chg + aac + alm1 + alm2 , 
                data = db.train, 
                hidden = c(8))

my.results <- compute(nn, db.validation[-9])$net.result
head(my.results)

atr <- my.results[1,]
atr == max(atr)
which(atr == max(atr))

max.indeks <- function(atr){
  return(which(atr == max(atr)))
}
klasyfikacja <- apply(my.results, c(1),max.indeks)
summary(db.validation$class)
wynik <- c('cp','im','imL','imS','imU','om','omL','pp')[klasyfikacja]
table(wynik, db.validation$class)
print("hidden 8")

