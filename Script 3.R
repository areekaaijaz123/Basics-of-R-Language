View(mtcars)

#glm function for logistic regression

mod1 <- glm(vs ~ mpg, data = mtcars, family = "binomial")
summary(mod1)

predict(mod1,data.frame(mpg=20), type="response")

predict(mod1,data.frame(mpg=20:30), type="response")

mod2 <- glm(vs ~ hp, data = mtcars, family = "binomial")

predict(mod2,data.frame(hp=20),type="response")

predict(mod2,data.frame(hp=c(150,100,50)),type="response")