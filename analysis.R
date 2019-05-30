library(naniar)
library(ggplot2)
library(reshape2)

# importing data
our.data = data.frame(longley)
head(our.data, 10)
colnames(our.data)

str(our.data)

# check missing values
vis_miss(our.data) # no missing value
ggsave("Plots/missing_value.png")


# Visualizing data

# GNP.deflator
ggplot(data = our.data, mapping = aes(x = GNP.deflator, y = Employed)) +
  geom_point(col = "red") +
  geom_smooth(method = "lm", se = F) +
  ylab("Employed") +
  xlab("GNP.deflator") +
  ggtitle("GNP.deflator vs Employed")

ggsave("Plots/GNP.deflator vs Employed.png")

# GNP
ggplot(data = our.data, mapping = aes(x = GNP, y = Employed)) +
  geom_point(col = "red") +
  geom_smooth(method = "lm", se = F) +
  ylab("Employed") +
  xlab("GNP") +
  ggtitle("GNP vs Employed")

ggsave("Plots/GNP vs Employed.png")

# Unemployed
ggplot(data = our.data, mapping = aes(x = Unemployed, y = Employed)) +
  geom_point(col = "red") +
  geom_smooth(method = "lm", se = F) +
  ylab("Employed") +
  xlab("Unemployed") +
  ggtitle("Unemployed vs Employed")

ggsave("Plots/Unemployed vs Employed.png")

# Armed.Forces
ggplot(data = our.data, mapping = aes(x = Armed.Forces, y = Employed)) +
  geom_point(col = "red") +
  geom_smooth(method = "lm", se = F) +
  ylab("Employed") +
  xlab("Armed.Forces") +
  ggtitle("Armed.Forces vs Employed")

ggsave("Plots/Armed.Forces vs Employed.png")

# Population
ggplot(data = our.data, mapping = aes(x = Population, y = Employed)) +
  geom_point(col = "red") +
  geom_smooth(method = "lm", se = F) +
  ylab("Employed") +
  xlab("Population") +
  ggtitle("Population vs Employed")

ggsave("Plots/Population vs Employed.png")

# dropping year column
our.data = our.data[-6]

# sampling creating tsd and trd
sf = sample(x = 2, size = nrow(our.data), replace = T, prob = c(0.7, 0.3))
trd = our.data[sf == 1, ]
tsd = our.data[sf == 2, ]

# regressor using lm
regressor = lm(formula = Employed ~., 
               data = trd)
summary(regressor)

# predict 
pred.regressor = predict(object = regressor, newdata = tsd)

# Visualising regressor
pred.regressor.df = data.frame(index = c(1:length(pred.regressor)), pred = pred.regressor)
actual.df = data.frame(index = c(1:nrow(tsd)), actual = tsd$Employed)

result.regressor = merge(x = pred.regressor.df, y = actual.df, by = "index")
result.regressor.melt = melt(data = result.regressor, id.vars = "index")

ggplot(data = result.regressor.melt, mapping = aes(x = index, y = value, col = variable)) + 
  geom_line() +
  ggtitle("Actual vs Prediction (Regressor)") 

ggsave("Plots/Actual Vs Prediction (Regressor).png")

# calculate accuracy using rmse
rmse.reg = sqrt(mean(pred.regressor - tsd$Employed) ^ 2)
rmse.reg
# ------------MODEL 2------------------------------------- #

# regressor using lm with using GNP and Armed.Forces
regressor.2 = lm(formula = Employed ~ GNP + Armed.Forces, 
               data = trd)
summary(regressor.2)

# predict 
pred.regressor2 = predict(object = regressor.2, newdata = tsd)

# Visualising regressor

result.regressor = data.frame(index = c(1:nrow(tsd)), pred = pred.regressor2, 
                              actual = tsd$Employed)
result.regressor2.melt = melt(data = result.regressor, id.vars = "index")

ggplot(data = result.regressor2.melt, mapping = aes(x = index, y = value, col = variable)) + 
  geom_line() +
  ggtitle("Actual vs Prediction (Regressor2)") 

ggsave("Plots/Actual Vs Prediction (Regressor2).png")

# calculate accuracy using rmse
rmse.reg2 = sqrt(mean(pred.regressor2 - tsd$Employed) ^ 2)
rmse.reg2

our.regressor = data.frame(reg = c("reg1", "reg2"), accuracy = c(rmse.reg, rmse.reg2))

ggplot(data = our.regressor, mapping = aes(x = reg, y = accuracy, 
                                           label = accuracy)) + 
  geom_point(col = "red") +
  ggtitle("Accuracy") +
  xlab(" Regressor") +
  geom_text(aes(label = round(accuracy, digits = 2)), hjust = 1, vjust = 0) +
  scale_y_continuous(limits = c(0:1))

ggsave("Plots/Accuracy.png")
