require(mlbench)
require(mxnet)

data(BostonHousing, package="mlbench")

train.ind <- seq(1, 506, 3)
train.x <- data.matrix(BostonHousing[train.ind, -14])
train.y <- BostonHousing[train.ind, 14]
test.x <- data.matrix(BostonHousing[-train.ind, -14])
test.y <- BostonHousing[-train.ind, 14]

# Define the input data
data <- mx.symbol.Variable("data")
# A fully connected hidden layer
# data: input source
# num_hidden: number of neurons in this hidden layer
fc1 <- mx.symbol.FullyConnected(data, num_hidden=1)

# Use linear regression for the output layer
lro <- mx.symbol.LinearRegressionOutput(fc1)


mx.set.seed(0)
model <- mx.model.FeedForward.create(lro, X=train.x, y=train.y,
                                     ctx=mx.cpu(), num.round=50, array.batch.size=20,
                                     learning.rate=2e-6, momentum=0.9, eval.metric=mx.metric.rmse)

graph.viz(model$symbol)

preds <- predict(model, test.x)
sqrt(mean((preds-test.y)^2))
preds

demo.metric.mae <- mx.metric.custom("mae", function(label, pred) {
  res <- mean(abs(label-pred))
  return(res)
})


mx.set.seed(0)
model <- mx.model.FeedForward.create(lro, X=train.x, y=train.y,
                                     ctx=mx.cpu(), num.round=50, array.batch.size=20,
                                     learning.rate=2e-6, momentum=0.9, eval.metric=demo.metric.mae)

graph.viz(model$symbol)

train.x <- data.matrix(BostonHousing[train.ind, -(13:14)])
train.y <- BostonHousing[train.ind, c(13:14)]
test.x <- data.matrix(BostonHousing[-train.ind, -(13:14)])
test.y <- BostonHousing[-train.ind, c(13:14)]


data <- mx.symbol.Variable("data")
fc2 <- mx.symbol.FullyConnected(data, num_hidden=2)
lro2 <- mx.symbol.LinearRegressionOutput(fc2)


mx.set.seed(0)
train_iter = mx.io.arrayiter(data = t(train.x), label = t(train.y))

model <- mx.model.FeedForward.create(lro2, X=train_iter,
                                     ctx=mx.cpu(), num.round=50, array.batch.size=20,
                                     learning.rate=2e-6, momentum=0.9)

model
graph.viz(model$symbol)

preds <- t(predict(model, test.x))
dim(preds)
dim(test.y)
preds

