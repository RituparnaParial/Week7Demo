str(beaver2)
transformed_beaver_data <- transform(beaver2, 
                                     activ = factor(activ, labels = c("no", "yes")))
library("lattice")
# The histogram uses a 1 sided formula
# so we don't specify anything on the
# left of ~
histogram(~temp | activ, data = transformed_beaver_data)

# Quantile-quantile (qq) plot allows us to
# compare the quantiles of both samples
# we use square brackets to select the cases we want

with(transformed_beaver_data,
     qqplot(temp[activ == "yes"], 
            temp[activ == "no"],
            main = "Comparing 2 samples",
            xlab = "Active temp = yes",
            ylab = "Active temp = no"))

# Using a QQ plot to check for normalit
# qqnorm function plots your sample
# against a normal distribution

with(transformed_beaver_data,{
  qqnorm(temp[activ == "no"],
         main = "Inactive")
         qqline(temp[activ == "no"])
})

with(transformed_beaver_data,{
  qqnorm(temp[activ == "yes"],
         main = "Active")
  qqline(temp[activ == "yes"])
})

# Formal test for normality
# using the Shaprio-wilks test

normality_test <- shapiro.test(transformed_beaver_data$temp)
normality_test
normality_test$p.value

# p-value indicated whether the sample
# comes from a normal distribution 
# p-value is clearly lower than 0.05
# so it is not normally distributed

