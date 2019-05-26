library(h2o)
library(tidyverse)
library(magrittr)

h2o.init()

## GLM ----

data <- read_table('https://data.princeton.edu/wws509/datasets/smoking.dat')
data <- data %>% select(-X1) %>% mutate(prop = floor(dead/pop*1000))
data <- as.h2o(data)
data$age %<>% as.factor()
data$smoke %<>% as.factor

summary(data)
str(data)
x <- 1:2
y <- 5

mGLM <- h2o.glm(x, y, data,
                family = 'poisson',
                model_id = 'smoking_p')

mGLM


## Naive Bayes -------

h2o.iris <- as.h2o(iris)
parts <- h2o.splitFrame(h2o.iris, 0.8)

train <- parts[[1]]
test <- parts[[2]]

mNB <- h2o.naiveBayes(1:4, 5, train)
mNB

h2o.performance(mNB, test)

## Grid Search -----------

mGLM2 <- h2o.grid('glm', x = x, y = y, training_frame = data,
                family = 'poisson',
                search_criteria = list(
                  strategy = 'Cartesian'
                ),
                hyper_params = list(
                  alpha = c(0.1, 0.25, 0.5, 0.75)
                ),
                lambda_search = TRUE)

h2o.getModel(mGLM2@model_ids[[1]])


