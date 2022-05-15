# SIGNIFICANCE OF MACROOECONOMIC FACTORS TO THE LEVEL OF NON PERFROMING LOANS IN KENYA

## R Packages

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
install.packages("here")
install.packages("patchwork")
install.packages("hrbrthemes")
install.packages("ggrepel")
install.packages("rmarkdown")

library(tidyverse)
library(patchwork)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(tidyr)
library(readr)
library(here)

## The data
non_performing_loans <- read.csv("/cloud/project/Non Performing Loans Dataset.csv")
head(non_performing_loans)

## Data Cleaning
library(dplyr)
NPL_loans <- non_performing_loans %>%
  select(Year, NPL, GDP, Exchange_Rate, Interest_Rate)%>%
  rename(GDPUSD = GDP)%>%
  mutate(GDPUSD = GDPUSD / 1E9) 
as_tibble(NPL_loans)
summary(NPL_loans)

## Explorative Analyses 
### 1. Independence of observation i.e. no correlation 
cor(NPL_loans$GDPUSD, NPL_loans$Exchange_Rate)

cor(NPL_loans$GDPUSD, NPL_loans$Interest_Rate)

cor(NPL_loans$Exchange_Rate, NPL_loans$Interest_Rate)

### 2. Normality
hist(NPL_loans$NPL)

### 3. Linearity
par(mfrow = c(1,2))
plot(NPL ~ Exchange_Rate, data = NPL_loans)
plot(NPL ~ Interest_Rate, data = NPL_loans)

### 4. Homoscedasticity

## Perform the linear regression analysis
NPL_data <- lm(NPL ~ Exchange_Rate + Interest_Rate, data = NPL_loans)
summary(NPL_data)

#### Check for homoscedasticity
par(mfrow=c(2,2))
plot(NPL_data)

## Visualize the results with a graph
### Plot the data points on a graph
NPL_graph<-ggplot(NPL_data, aes(x=NPL, y=Interest_Rate))+
  geom_point()
NPL_graph

### Add the linear regression line to the plotted data
NPL_graph <- NPL_graph + geom_smooth(formula = y ~ x, method = "lm", col ="black")
NPL_graph

### Add the equation for the regression line
NPL_graph <- NPL_graph +
  stat_regline_equation(label.x = 16, label.y = 17)
NPL_graph

###Make the graph ready for publication
NPL_graph +
  theme_bw() +
  labs(title = "Reported Interested Rate as a factor of Level of NPLs",
       x = "NPL (%)",
       y = "Interest Rate (%)")

