# Install and load necessary libraries
install.packages(c("ggplot2"))
library(ggplot2)

# Data for logistic regression
data <- data.frame(hours = c(2,4,5,7,8), pass = c(0,0,1,1,1))

# Image 1: Scatter plot of the data
image1 <- ggplot(data, aes(x=hours, y=pass)) +
  geom_point(aes(color=pass), size=4) +
  labs(title="Study Hours vs Pass/Fail", x="Hours Studied", y="Pass (1) / Fail (0)") +
  theme_minimal()+
  theme(text = element_text(color = "white"))
ggsave("image1.png", image1, width=5, height=4)

# Logistic model
model <- glm(pass ~ hours, data=data, family="binomial")

# Image 2: Data with logistic curve
image2 <- ggplot(data, aes(x=hours, y=pass)) +
  geom_point(aes(color=pass), size=4) +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE, color="blue") +
  labs(title="Logistic Regression Fit", x="Hours Studied", y="Pass Probability") +
  theme_minimal()+
  theme(text = element_text(color = "white"))
ggsave("image2.png", image2, width=5, height=4)

# Predicting for 6 hours
new_data <- data.frame(hours=6)
predicted_prob <- predict(model, new_data, type="response")

# Image 3: Highlighting prediction
image3 <- ggplot(data, aes(x=hours, y=pass)) +
  geom_point(aes(color=pass), size=4) +
  geom_smooth(method="glm", method.args=list(family="binomial"), se=FALSE, color="blue") +
  geom_vline(aes(xintercept=6), color="red", linetype="dashed") +
  geom_label(aes(x=6, y=predicted_prob, label=paste0("Probability: ", round(predicted_prob, 2))), hjust=1.5) +
  labs(title="Prediction for 6 Hours", x="Hours Studied", y="Pass Probability") +
  theme_minimal()+
  theme(text = element_text(color = "white"))
ggsave("image3.png", image3, width=5, height=4)
