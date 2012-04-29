# Dumbass initial submission
# Give median by hour/target

train <- read.csv('data/TrainingData.csv')
submit <- read.csv('data/SubmissionZerosExceptNAs.csv')

library(plyr);
library(laeken);

lagged.df <- data.frame(weekday=train$weekday, chunkID=train$chunkID, position_within_chunk=train$position_within_chunk+168);
submit$weekday <- join(submit, lagged.df, by=c('chunkID', 'position_within_chunk'))$weekday;

rmedian <- function (x) median(x, na.rm=T);

medians <- colwise(rmedian, is.numeric)(train);

mae <- function (x, y) {mean(abs(x - y), na.rm=T)};

varsets <- list(
    'hour',
    'chunkID',
    'month_most_common',
    'weekday',
    c('hour', 'chunkID'),
    c('hour', 'month_most_common'),
    c('hour', 'weekday'),
    c('chunkID', 'weekday'),
    c('month_most_common', 'weekday')
  );
preds <- lapply(varsets, function (var) {
  medians.by.var <- ddply(train, var, colwise(rmedian, is.numeric));
  list(
    submit=join(submit[,c(1:5, 45)], medians.by.var, by=var),
    train=join(train[,1:6], medians.by.var, by=var)
    );
});
submit$weekday <- NULL;

for (target in colnames(submit)[6:44]) {
  idxs <- which(submit[,target] > -999999);
  cat(target, '\n');
  maes <- sapply(preds, function (pred) {
    m <- mae(pred$train[,target], train[,target]);
    cat(' ', m, '\n');
    m;
    });
    wm <- function(x) weightedMedian(x, 1/maes, na.rm=T);
    submit[idxs, target] <- apply(sapply(preds, function (pred) pred$submit[idxs,target]), 1, wm);
}

argv <- commandArgs(T);
filename <- argv[1];

write.csv(submit, filename);
if (sum(is.na(submit)) > 0) stop('NAs detected');

