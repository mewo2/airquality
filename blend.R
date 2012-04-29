argv <- commandArgs(T);
n <- length(argv);

outfile <- argv[n];

submit <- read.csv('data/SubmissionZerosExceptNAs.csv')

components <- lapply(argv[1:(n-1)], read.csv);

for (target in colnames(submit)[6:44]) {
  submit[,target] <- rowMeans(sapply(components, function(x) x[,target]));
}

write.csv(submit, outfile);
