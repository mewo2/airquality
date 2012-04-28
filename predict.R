# Dumbass initial submission
# Give median by hour/target

train <- read.csv('data/TrainingData.csv')
submit <- read.csv('data/SubmissionZerosExceptNAs.csv')

library(plyr)

medians <- colwise(function (x) median(x, na.rm=T), is.numeric)(train);

preds <- lapply(c('hour', 'chunkID', 'month_most_common'), function (var) {
  medians.by.var <- ddply(train, var, colwise(function (x) median(x, na.rm=T), is.numeric));
  join(submit[,1:5], medians.by.var, by=var);
});

for (target in colnames(submit)[6:44]) {
  idxs <- which(submit[,target] > -999999);
  submit[idxs, target] <- apply(sapply(preds, function (pred) pred[idxs,target]), 1, median);
}

argv <- commandArgs(T);
filename <- argv[1];

if (sum(is.na(submit)) > 0) stop('NAs detected');

write.csv(submit, filename)