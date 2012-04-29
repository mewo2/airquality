argv <- commandArgs(T);

early <- argv[1];
late <- argv[2];
outfile <- argv[3];

submit <- read.csv('data/SubmissionZerosExceptNAs.csv')
early <- read.csv(early);
late <- read.csv(late);

alpha <- (submit$position_within_chunk - 193) / 71
for (target in colnames(submit)[6:44]) {
  submit[,target] <- alpha * late[,target] + (1-alpha) * early[,target];
}

write.csv(submit, outfile);