library('RUnit')

source('cost.R')
source('gradientDescent.R')
source('predict.R')
source('randInitWeights.R')
source('sigmoid.R')
source('splitTheta.R')
source('train.R')

test.suite <- defineTestSuite("sigmoid",
                    dirs = file.path("tests"),
                    testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
