# Dumbass initial submission
# Give median by hour/target

train <- read.csv('data/TrainingData.csv')
submit <- read.csv('data/SubmissionZerosExceptNAs.csv')

library(plyr)

rmedian <- function (x) median(x, na.rm=T);

medians <- colwise(rmedian, is.numeric)(train);

mae <- function (x, y) {mean(abs(x - y))};

varsets <- list(
    NULL,
    'hour',
    'chunkID',
    'month_most_common',
    c('hour', 'chunkID'),
    c('hour', 'month_most_common')
  );
preds <- lapply(varsets, function (var) {
  medians.by.var <- ddply(train, var, colwise(rmedian, is.numeric));
  list(
    submit=join(submit[,1:5], medians.by.var, by=var),
    train=join(train[,1:6], medians.by.var, by=var),
    );
});


for (target in colnames(submit)[6:44]) {
  idxs <- which(submit[,target] > -999999);
  cat(target, '\n');
  maes <- sapply(preds, function (pred) {
    m <- mae(pred$train[idxs, target], train[idxs, target]);
    cat(' ', m, '\n');
    m;
    });
    
  submit[idxs, target] <- apply(sapply(preds, function (pred) pred$submit[idxs,target]), 1, rmedian);
}

argv <- commandArgs(T);
filename <- argv[1];

if (sum(is.na(submit)) > 0) stop('NAs detected');

write.csv(submit, filename)