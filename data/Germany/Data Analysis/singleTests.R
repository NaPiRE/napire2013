# This file is an R script which analyses the data from the
# Naming the Pain in RE survey. It tests all hypotheses related
# to a single question.

# load the dataset
input = read.table("R-Import.csv", header=TRUE, sep=";")

# load the questions
tests = read.table("singleTests.csv", header=TRUE, sep=";")

# Build result table
resultTable = data.frame(question=character(0), statistic=numeric(0),
    pvalue = numeric(0), alternative = character(0), mu = numeric(0), 
    row.names = NULL)

# iterate over tests table to run hypothesis tests (for tests with
# no additional selection)
for (index in 1:nrow(tests)){
    row = tests[index, ];
    result = wilcox.test(input[,as.character(row$Spalte)], alternative = as.character(row$alternative), 
        mu = row$mu, 
        paired = FALSE, conf.level = 0.95)
    resultTable <- rbind(resultTable, data.frame(question=row$question, 
        statistic=result$statistic, 
        pvalue=result$p.value, alternative=result$alternative, 
        mu=result$null.value))
}

# add results for tests with additional selection
result = wilcox.test(subset(input, v_12==1)[,"v_173"], alternative="less", 
    mu=3, paired=FALSE, 
    conf.level = 0.95)
resultTable <- rbind(resultTable, data.frame(question="12h-1",
        statistic=result$statistic,
        pvalue=result$p.value, alternative=result$alternative,
        mu=result$null.value))


result = wilcox.test(subset(input, (v_12==2|v_12==3|v_12==6))[,"v_173"], 
    alternative="greater", 
    mu=3, paired=FALSE, 
    conf.level = 0.95)
resultTable <- rbind(resultTable, data.frame(question="12h-2,3,6",
        statistic=result$statistic,
        pvalue=result$p.value, alternative=result$alternative,
        mu=result$null.value))


result = wilcox.test(subset(input, (v_12==2|v_12==3|v_12==6))[,"v_174"], 
    alternative="greater", 
    mu=3, paired=FALSE, 
    conf.level = 0.95)
resultTable <- rbind(resultTable, data.frame(question="12i-2,3,6",
        statistic=result$statistic,
        pvalue=result$p.value, alternative=result$alternative,
        mu=result$null.value))

result = wilcox.test(subset(input, v_12==1)[,"v_174"], alternative="less", 
    mu=3, paired=FALSE, 
    conf.level = 0.95)
resultTable <- rbind(resultTable, data.frame(question="12i-1",
        statistic=result$statistic,
        pvalue=result$p.value, alternative=result$alternative,
        mu=result$null.value))

result = wilcox.test(subset(input, (v_1==1|v_12==2|v_12==3))[,"v_161"], 
    alternative="less", 
    mu=3, paired=FALSE, 
    conf.level = 0.95)
resultTable <- rbind(resultTable, data.frame(question="13a-1,2,3",
        statistic=result$statistic,
        pvalue=result$p.value, alternative=result$alternative,
        mu=result$null.value))

result = wilcox.test(subset(input, v_1==7)[,"v_161"], alternative="greater", 
    mu=3, paired=FALSE, 
    conf.level = 0.95)
resultTable <- rbind(resultTable, data.frame(question="13a-7",
        statistic=result$statistic,
        pvalue=result$p.value, alternative=result$alternative,
        mu=result$null.value))

result = wilcox.test(subset(input, v_1==7)[,"v_208"], alternative="greater", 
    mu=0, paired=FALSE, 
    conf.level = 0.95)
resultTable <- rbind(resultTable, data.frame(question="26,v_208, 7",
        statistic=result$statistic,
        pvalue=result$p.value, alternative=result$alternative,
        mu=result$null.value))

result = wilcox.test(subset(input, v_1==7)[,"v_209"], alternative="greater", 
    mu=0, paired=FALSE, 
    conf.level = 0.95)
resultTable <- rbind(resultTable, data.frame(question="26,v_209, 7",
        statistic=result$statistic,
        pvalue=result$p.value, alternative=result$alternative,
        mu=result$null.value))

result = wilcox.test(subset(input, (v_1==1|v_12==2|v_12==3))[,"v_211"], 
   alternative="greater", 
    mu=0, paired=FALSE, 
    conf.level = 0.95)
resultTable <- rbind(resultTable, data.frame(question="26, v_211-1,2,3",
        statistic=result$statistic,
        pvalue=result$p.value, alternative=result$alternative,
        mu=result$null.value))

# write dataset to file
write.csv(resultTable, file = "singleTestResults.csv", row.names = FALSE)
