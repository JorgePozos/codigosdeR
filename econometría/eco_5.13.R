library(haven)
library(ggplot2)
library(plot3D)

br2 <- read_dta("Downloads/br2.dta")
attach(br2)

scatter3D(sqft, age, price)
mod1 <- lm(price ~ sqft + bedrooms)
summary(mod1)



