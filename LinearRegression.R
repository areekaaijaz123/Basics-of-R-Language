#Linear Regression
#Model Selection on State.x77                                 

states = as.data.frame(state.x77)
view(states)

mod1 = lm(Income~Illiteracy, data = states)
plot(states$Illiteracy,states$Income)
abline(mod1)
summary(mod1)

mod2 = lm(Income~Murder, data = states)
plot(states$Murder,states$Income)
abline(mod2)
summary(mod2)

mod3 = lm(Income~Population,data = states)
plot(states$Population,states$Income)
abline(mod3)
summary(mod3)

mod4 = lm(Income ~ Illiteracy + Murder + Population, data = states)
summary(mod4)

mod5 = lm(Income ~ Illiteracy*Murder*Population, data = states)
summary(mod5)

mod6 = lm(Income ~ Population*Area*states$`HS Grad`, data = states)
summary(mod6)

mod7 = lm(Income ~ Population + Area + states$`HS Grad`, data = states)
summary(mod7)

#Simple model in Backward direction
bst_model_backward = lm(Income ~ . , data = states)
bst_model_backward <- step(bst_model_backward, direction = "backward")
summary(bst_model_backward)

#Model with Interaction term in Backward direction
bst_int_backward = lm(Income ~ (.)^2 , data = states)
bst_int_backward <- step(bst_int_backward, direction = "backward")
summary(bst_int_backward)

#Simple Model in Forward direction
bst_model_forward = lm(Income ~ 1 , data = states)
bst_model_forward <- step(bst_model_forward, direction = "forward", scope = (~Population + Illiteracy + Murder + Frost + Area + states$`Life Exp` + states$`HS Grad`))
summary(bst_model_forward)
