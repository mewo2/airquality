train <- read.csv('data/TrainingData.csv')
submit <- read.csv('data/SubmissionZerosExceptNAs.csv')

library(plyr);
library(zoo);
library(foreach);
library(doMC);

registerDoMC(4);

rmedian <- function (x) median(x, na.rm=T);
rmax <- function (x) max(x, na.rm=T);
rmin <- function (x) min(x, na.rm=T);

medians <- colwise(rmedian, is.numeric)(train);
mins <- colwise(rmin, is.numeric)(train);
maxes <- colwise(rmax, is.numeric)(train);

pred <- ddply(train, .(chunkID), function (data) {
  df <- data.frame(position_within_chunk=c(193:264));
  data <- join(data.frame(position_within_chunk=c(1:192)), data, by='position_within_chunk', match='first');
  for (var in colnames(data)) {
    if (substr(var, 1, 6) == 'target') {
      d <- data[,var];
      if (all(is.na(d))) {
        df[,var] <- rep(medians[[var]], 72);
      } else {
        max.nona <- max(which(!is.na(d)));
        alpha <- (df$position_within_chunk - max.nona) / (264 - max.nona)
        df[,var] <- alpha * rmedian(d) + (1 - alpha) * d[max.nona];
      }
    }
  }
  return(df)
  }, .progress='text', .parallel=T);
  
pred <- join(submit[,1:5], pred, by=c('chunkID', 'position_within_chunk'), match='first');

for (target in colnames(submit)[6:44]) {
  idxs <- which(submit[,target] > -999999);
  submit[idxs, target] <- pred[idxs, target];
  submit[,target] <- na.fill(submit[,target], medians[target]);
}

argv <- commandArgs(T);
filename <- argv[1];
if (length(filename) == 0) {
  filename <- 'predictions/scratch.csv';
}
write.csv(submit, filename);