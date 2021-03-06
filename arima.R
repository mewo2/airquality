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

prediction <- function(data, varname) {
  fallback <- rep(medians[[varname]], 72);
  if (sum(!is.na(data)) < 48) return(fallback);
  
  data[data <= 0] <- min(data[data > 0], na.rm=T);
  p <- tryCatch({
    target <- log(na.trim(data, 'left'));
    target <- na.spline(target, na.rm=F);
  
    arma <- arima0(target, c(1,0,1), list(order=c(0,0,0), period=24));
    pred <- as.vector(predict(arma, 72)$pred);
    if (any(abs(pred) > 10)) return(fallback);
    exp(pred);
  }, error = function (e) {return(fallback)});
  p <- pmax(p, rmin(data), mins[[varname]], na.rm=T);
  p <- pmin(p, rmax(data), maxes[[varname]], na.rm=T);
  return(p);
}

pred <- ddply(train, .(chunkID), function (data) {
  df <- data.frame(position_within_chunk=c(193:264));
  data <- join(data.frame(position_within_chunk=c(1:192)), data, by='position_within_chunk', match='first');
  for (var in colnames(data)) {
    if (substr(var, 1, 6) == 'target') {
      df[,var] <- prediction(data[,var], var);
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
if (sum(is.na(submit)) > 0) stop('NAs detected');
if (sum(submit[,-c(1:5)] > 1000) > 0) stop('Large values detected');
