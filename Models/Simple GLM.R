### Simple model 1.0 --------------------------------

# save 25% of training for cross validation. Keep that separate validation
# data completely separate, we will look at it once we have gathered more 
# data and made a more sophisticated model. For now just do a simple
# glm predicting p(over)

library(glmnet)

x.matrix.train <- model.matrix( ~ over + competitiveness + strong_w_dir + ADI +
                                  Temp + W_speed + home.team + over.under + 
                                  Gametime_ET + Condition,
                                data = training.ou[training.ou$Date < '2015-07-01',])

x.matrix.test <- model.matrix( ~ over + competitiveness + strong_w_dir + ADI +
                                  Temp + W_speed + home.team + over.under + 
                                  Gametime_ET + Condition,
                                data = training.ou[training.ou$Date >= '2015-07-01',])


over.under.glm <- cv.glmnet(x = x.matrix.train[,-2], y = x.matrix.train[,2],
                            nfolds = 5)
actual.glm <- glmnet(x = x.matrix.train[,-2], y = x.matrix.train[,2],
                  family = 'binomial')

# check out the betas
predict(actual.glm, type = 'coef', s = over.under.glm$lambda.min)
# ok, basically this is not even a model, lets just lower lambda 

predict(actual.glm, type = 'coef', s = 0.01)


glm.predictions <- predict(actual.glm, s = 0.01,
                           newx = x.matrix.test[,-2],
                           type = 'response')
# lets see how good these predictions were
table(glm.predictions > 0.55, training.ou$result[training.ou$Date >= '2015-07-01'])
# looks pretty decent maybe? Lets take a look at the ones we think wont hit the over
table(glm.predictions < 0.45, training.ou$result[training.ou$Date >= '2015-07-01'])
# Doesn't look too good. 

roi <- function(pred.p.over, p.threshold, actual.result){
  # assume house takes a 10% rake on winnings, we bet over above threshold, under below
  # 1-threshold
  bet.over <- pred.p.over > p.threshold
  bet.under <- pred.p.over < (1-p.threshold)
  win <- (bet.over & actual.result == 'Over') | 
    (bet.under& actual.result == 'Under')
  loss <- (bet.over & actual.result == 'Under') | 
    (bet.under& actual.result == 'Over')
  
  investment <- sum(bet.over + bet.under)
  winnings <- sum(win*0.9 - loss)
  paste0(round(winnings*100/investment,2),'%')
}
  
roi(glm.predictions, 0.55, training.ou$result[training.ou$Date >= '2015-07-01'])


