install.packages("rpart", "rpart.plot")
# install.packages("assocrules")

library(rpart)
library(rpart.plot)

# library(assocrules)

# cjuyanne@shockers.wichita.edu

# Change the folder location here
setwd("/Users/parthasarathikundu/Downloads")

# Load the dataset (replace "Online_retail.csv" with the actual file path)
bank <- read.csv("bank-sample.csv", header=TRUE, sep=",")
summary(bank)

drops <-c("age", "balance", "day", "campaign", "pdays", "previous", "month")
banktrain <-bank[,!(names(bank) %in% drops)]
summary(banktrain)

banktrain
bank

table(banktrain)
# Count the values in each column
column_counts <- apply(banktrain, 2, table)

# Print the results
print(column_counts)

fit <-rpart(subscribed ~ job + marital + education + default + housing + loan + contact + poutcome,
            method="class", data=banktrain,control=rpart.control(minsplit = 1),
            parms=list(split='information'))

summary(fit)
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=2)


