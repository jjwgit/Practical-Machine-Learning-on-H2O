# Week 2 Assignment


library(h2o)
library(tidyverse)

h2o.init()

# STEP 1, 2 ------

set.seed(46)

id <- 1:1000
blood_types <- c('a','a','a','o','o','o','ab','b') %>% str_to_upper()
age <- runif(1000, 18, 65)
he <- round(rnorm(1000, 5, 2))
he <- pmax(v, 0)
he <- pmin(v, 9)

als <-round(rnorm(1000, 5, 2))
als <- als + ifelse(age > 30, 1, 0)
als <- pmax(v, 0)
als <- pmin(v, 9)

inc <- 20000 + (age*3)^2
inc <- inc + hl*500
inc <- inc - als * 300
inc <- inc + runif(1000, 0, 5000)


df <- data.frame(id = id, blood_type = blood_types, age = age, healthy_eating = he, active_life_style = als, income = inc)
df$blood_type <- as.factor(blood_types[d$id %% length(blood_types) + 1])

as.h2o(df, destination_frame = 'people')

### PART 3 -----
people <- h2o.getFrame('people')
parts <- h2o.splitFrame(
  people,
  c(0.8, 0.1),
  destination_frames = c('people_train', 'people_valid', 'people_test'),
  seed = 46
)

map_dbl(parts, nrow)

train <- parts[[1]]
valid <- parts[[2]]
test <- parts[[3]]

# STEP 4, 5 -----

arguments <- names(people)[!names(people) %in% ('id')]
x <- arguments[!(arguments %in% ('blood_type'))]
y <- 'blood_type'

## Classify Blood type using RF -----

mRF <- h2o.randomForest(x, y, train)
mRF

h2o.performance(mRF, valid)
h2o.performance(mRF, test)

mRF2 <- h2o.randomForest(x, y, train, ntrees = 100, max_depth = 20)
mRF2
h2o.performance(mRF2, valid)
h2o.performance(mRF2, test)

## Classify Blood type using GBM ----

mGBM <- h2o.gbm(x, y, train)
mGBM

h2o.performance(mGBM, valid)
h2o.performance(mGBM, test)

mGBM2 <- h2o.gbm(x, y, train, max_depth = 10)
mGBM2

h2o.performance(mGBM2, valid)
h2o.performance(mGBM2, test)


# STEP 6 ------

h2o.shutdown()
