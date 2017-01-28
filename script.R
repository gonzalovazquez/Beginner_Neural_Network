library(ISLR)

# Get maxs and mins of data of each column
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)

scaled.data <- as.data.frame(scale(College[,2:18], center = mins, scale=maxs-mins)) # scale result (from 0-1) and put it in a dataframe

# Convert Yes/No to 1/0 respectively
Private = as.numeric(College$Private)-1
data = cbind(Private, scaled.data)

# Split data up for training and testing
library(caTools)
split = sample.split(data$Private, SplitRatio = 0.70)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

# Create formula
f <- paste(names(scaled.data), collapse = " + ")
f <- paste('Private ~', f)
f <- as.formula(f)

# Neural Networks
library(neuralnet)
nn <- neuralnet(f,train, hidden=c(10, 10, 10), linear.output=FALSE)

predicted.nn.values <- compute(nn, test[2:18])
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result, round, digits=0)
print(table(test$Private, predicted.nn.values$net.result))
plot(nn)

