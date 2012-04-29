train <- read.csv('data/TrainingData.csv')
argv <- commandArgs(T);
infile <- argv[1];
outfile <- argv[2];

submit <- read.csv(infile);

for (target in colnames(submit)) {
  if (substr(target, 1, 6) == 'target') {
    delta <- min(diff(sort(unique(train[,target]))));
    idxs <- which(submit[,target] > -999999);
    submit[idxs,target] <- delta * round(submit[idxs,target] / delta);
  }
}

write.csv(submit, outfile, row.names=F);
