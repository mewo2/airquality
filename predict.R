# Dumbass initial submission
# Give median by hour/target

train <- read.csv('data/TrainingData.csv')
submit <- read.csv('data/SubmissionZerosExceptNAs.csv')

library(plyr)

medians <- colwise(function (x) median(x, na.rm=T), is.numeric)(train);

medians.by.hour <- ddply(train, .(hour), colwise(function (x) median(x, na.rm=T), is.numeric));

for (target in colnames(submit)[6:44]) {
  idxs <- which(submit[,target] > -999999);
  hours <- submit$hour[idxs] + 1;
  submit[idxs, target] <- medians.by.hour[hours, target]; 
}

argv <- commandArgs(T);
filename <- argv[1];

write.csv(submit, filename)