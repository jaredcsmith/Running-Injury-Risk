
library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggdark)
library(plotly)
library(ggpubr)
library(xgboost)
library(MASS)
library(caret)
library(corrplot)
library(ggExtra) # Load ggExtra
library(xgboost) # Load XGBoost
source("a_insights_shap_functions.r") # Load SHAP functions
library(Metrics) # Load metrics
library(pROC) # Load proc 
day <- read_csv("day_approach_maskedID_timeseries.csv")
week <- read_csv("week_approach_maskedID_timeseries.csv")
# Remove rows with missing or invalid values in injury column
week <- week[complete.cases(week$injury), ]
summary(week)

week %>%
  ggplot( )+
  geom_boxplot(mapping=aes(y=`max exertion`)) 
week %>%
  ggplot( )+
  geom_boxplot(mapping=aes(y=`avg exertion`))


noninjured <-week[week$injury == 0,]
summary(noninjured)
injured <- week[week$injury == 1, ]
summary(injured)
injured$injury = (as.factor(injured$injury))
levels(injured$injury) <- c("Injure

noninjured$injury = (as.factor(noninjured$injury))
levels(noninjured$injury) <- c("NotInjured")

injury_dist <- week %>%
  group_by(`Athlete ID`, injury) %>%
  summarise(count = n()) %>%
  spread(injury, count, fill = 0) 
injury_dist
colnames(injury_dist) <- c("Athlete ID", "Not Injured", "Injured")

injury_sum <- week %>%
  subset(week$injury==1,) 
injury_sum <- injury_sum %>%
  group_by(`Athlete ID`, injury) %>%
  mutate(Freq = n()) 
injury_sum

ggplot(injury_sum, aes(x=`Athlete ID`, fill=Freq))+
  geom_bar()

# Create plot
p <- ggplot(injury_sum, aes(x = `max exertion`, fill = injury)) + # Set dataset and aesthetics
  geom_density(alpha = 0.3, fill='red3') + 
  labs(x = "Max Exertion", y = "Injuries", # Set labels for plot 
       title = "Injuries by Max Exertion") +
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  dark_theme_bw() # Turn theme to dark mode

p1 <- ggplot(injury_sum, aes(x = `avg recovery`, fill = injury)) + # Set dataset and aesthetics
  geom_density(alpha = 0.3, fill ="blue2") + 
  labs(x = "Average Recovery", y = "Injuries", # Set labels for plot 
       title = "Injuries by Average Recovery") +
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  dark_theme_bw() # Turn theme to dark mode

p <- ggarrange(p, p1, ncol = 2, widths = c(3,3), common.legend = TRUE, legend = "bottom")
print(p)


# For Weeks that an Athlete was Injured this was their average exertion

week$injury <- (as.factor(week$injury))
levels(week$injury) <- c("Not Injured", "Injured")
summary(week$injury)
# Create a ggplot boxplot
p <- ggplot(week, aes(x = injury, y = `avg exertion`, colour=injury)) +
  geom_boxplot() +
  scale_colour_manual(values = c("green", "red"))
p
# Create an interactive plotly version of the ggplot
ggplotly(p) %>%
  layout(
    title = "Boxplot of Average Exertion by Injury Status",
    xaxis = list(title = "Injury Status"),
    yaxis = list(title = "Average Exertion")
  )

library(ggpubr)
# Create a ggplot bar chart
p1 <- week %>%
  ggplot(aes(x = Date, y = `avg exertion`, fill = injury)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red2", "green")) +
  labs(title = "Bar Chart and Density Plot of Average Exertion by Injury Status",
       x = "Date", y = "Average Exertion", fill = "Injury Status")

# Create a ggplot density plot
p2 <- week %>%
  ggplot(aes(x = `avg exertion`, fill = injury, color = injury)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("green", "red2")) +
  scale_color_manual(values = c("green", "red2")) +
  labs(x = "Average Exertion", y = "Density", fill = "Injury Status", color = "Injury Status")

# Combine the two plots
p <- ggarrange(p1, p2, ncol = 2, widths = c(3, 2), common.legend = TRUE, legend = "bottom")
library(cowplot)
p3 <- plot_grid(p1, p2, ncol = 1, align = "h", axis = "tb", labels = "AUTO")

# Print the plot
print(p3)
# Print the plot
print(p)


p <- week %>%
  ggplot(aes(x = Date)) +
  geom_density(aes( fill = injury), alpha = 0.3) +
  scale_fill_manual(values = c("red2", "green")) +
  labs(title = "Bar Chart and Density Plot of Average Exertion by Injury Status",
       x = "Date", y = "Density", fill = "Injury Status")
p

summary(week)
# distance sum over 3 weeks 
# week 0-2 (Distance in week 0 and the average across 1 and 2)

ac_7_21 <- week$`total kms`/((week$`total kms.1` + week$`total kms.2`)/2)

ac_7_21[is.infinite(ac_7_21)] <- NA

week$ac_7_21 <- ac_7_21
p <- week %>%
  ggplot(aes(x = ac_7_21)) +
  geom_density(aes( fill = factor(injury)), alpha = 0.3) +
  scale_fill_manual(values = c("red2", "green")) +
  labs(title = "Bar Chart and Density Plot of Average Exertion by Injury Status",
       x = "Date", y = "Density", fill = "Injury Status") +
  xlim(0,3)
p

## Derived Variables 


ae_7_21 <- week$`avg exertion`/((week$`avg exertion.1` + week$`avg exertion.2`)/2)
ae_7_21[is.infinite(ae_7_21)] <- NA


exer_v_rec_7_21 <- week$`max exertion`/((week$`avg recovery.1`+ week$`avg recovery.2`)/2) ## Exertion 0 / recovery 1 over 3 weeks 
exer_v_rec_7_21[is.infinite(exer_v_rec_7_21)] <- NA

er_7_21 <- week$`avg exertion`/((week$`avg recovery.1` + week$`avg recovery.2`)/2)
er_7_21[is.infinite(er_7_21)] <- NA

rec_maxexer_7_21<- week$`avg recovery`/((week$`max exertion.1` + week$`max exertion.2`)/2)
rec_maxexer_7_21[is.infinite(rec_maxexer_7_21)] <- NA




week$ae_7_21 <- ae_7_21
threshold <- 2.25
week$ae_7_21 <- ifelse(week$ae_7_21 > threshold, mean(week$ae_7_21), week$ae_7_21)
summary(week$ae_7_21)

week$exer_v_rec_7_21 <- exer_v_rec_7_21 
threshold <- 2.25
week$exer_v_rec_7_21 <- ifelse(week$exer_v_rec_7_21 > threshold, mean(week$exer_v_rec_7_21), week$exer_v_rec_7_21)
summary(week$exer_v_rec_7_21)

week$er_7_21 <- er_7_21
threshold <- 2.25
week$er_7_21 <- ifelse(week$er_7_21 > threshold, mean(week$er_7_21), week$er_7_21)
summary(week$er_7_21)

week$rec_maxexer_7_21 <- rec_maxexer_7_21
threshold <- 2.25
week$rec_maxexer_7_21 <- ifelse(week$rec_maxexer_7_21 > threshold, mean(week$rec_maxexer_7_21), week$rec_maxexer_7_21)
summary(week$rec_maxexer_7_21)

model <- week[,c(1:3, 12:14, 16, 17, 20, 68, 73:76)]
model <- na.omit(model)

full_model <- glm(injury ~ ., data = model, family = binomial())
summary(full_model)
step_model <- stepAIC(full_model, direction = "forward", scope = list(lower = ~1, upper = ~.), trace = FALSE)
summary(step_model)

#cor.matrix <- week[,c(1:4, 12:22,68, 73:76)]

#cor_matrix <- cor(na.omit(cor.matrix))
# Create a heatmap of the correlation matrix
#corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
#         addCoef.col = "black", tl.col = "black", tl.srt = 55, tl.cex = .85)




### New Analysis 
## Lets investigate injuries as a part of our data set 
#Looks as if our current percentage of injuries as opposed to training days is 1.42 %

training_days <- sum(week$`nr. sessions` > 0)
sum(as.numeric(week$injury)) / training_days
summary(as.factor(week$injury))

athlete <- week %>%
  group_by(`Athlete ID`) %>%
  summarise(Injuries = sum(as.numeric(injury))) %>%
  arrange(desc(Injuries)) %>%
  head(10)
athlete

Injury_Plot <- ggplot(athlete, mapping = aes(x = Injuries)) +
  geom_density(alpha=0.5, fill="maroon") +
  labs(x = "Total Injuries",  
       title = "Distribution of Injuries among Athletes")
Injury_Plot

## Lets focus on the most injured athlete

athlete1 <- week[week$`Athlete ID` == "26",]

# Create plot
Athlete_Workloads <- ggplot(athlete1, # Set data
                            aes(x = `Date`, y = `avg exertion`, color = factor(injury))) + # Set aesthetics
  geom_point(alpha = 0.5) + # Set geom_point for scatter plot
  labs(x = "Days",  # Set labels
       title = "Athlete 26 Injuries",
       fill = "Injuries") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) +  # Remove grid 
  scale_color_manual(values = c("0" = "blue", "1" = "red"), # Set color values
                     labels = c("0" = "Healthy", "1" = "Injured"))

# Generate graph
ggMarginal(Athlete_Workloads , groupFill = TRUE,
           type = "histogram")

pivotdat <- pivot_longer(athlete1[, c("min recovery", "avg recovery", "max recovery", "min exertion", "avg exertion", "max exertion", "min training success", "avg training success", "max training success", "Athlete ID", "injury")],
                         !c("Athlete ID", "injury")) # Set variables to use as ID
pivotdatall <- pivot_longer(week[, c("min recovery", "avg recovery", "max recovery", "min exertion", "avg exertion", "max exertion", "min training success", "avg training success", "max training success", "Athlete ID", "injury")],
                            !c("Athlete ID", "injury")) # Set variables to use as ID

g <- ggplot(pivotdat,
            aes(x = value, fill = factor(injury))) + # Set aesthetics
  geom_density(alpha = 0.3) + # Set geom density for density plot
  labs(x = "Percieved Metric Values",  # Set labels
       title = "Injuries v Percieved Metric Values Leading Up to Injury",
       fill = "injury") +
  facet_wrap(~name, scale = "free") +
  theme_bw() + # Set theme
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), # Set color values
                    labels = c("0" = "Healthy", "1" = "Injury"))

# Generate graph
g

gall <- ggplot(pivotdatall,
               aes(x = value, fill = factor(injury))) + # Set aesthetics
  geom_density(alpha = 0.3) + # Set geom density for density plot
  labs(x = "Percieved Metric Values",  # Set labels
       title = "Injuries v Percieved Metric Values Leading Up to Injury",
       fill = "injury") +
  facet_wrap(~name, scale = "free") +
  theme_bw() + # Set theme
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), # Set color values
                    labels = c("0" = "Healthy", "1" = "Injury"))

# Generate graph
gall

## Athlete ID 26 Week before injury

# Lets select top 10 most injured athletes to see if there are any outstanding similarities
athletes <- week[week$`Athlete ID` %in% athlete$`Athlete ID`,]
# Create graph
athlete_ex <- ggplot(athlete1, # Set data
                     aes(x = `avg recovery.1`, y = `max exertion` , color = factor(injury))) + # Set aesthetics
  geom_point(alpha = 0.3) + # Set geom point for scatter plot
  labs(x = "Avg Recovery week before Injury",  # Set labels
       y = "Max Exertion week of Injury",
       title = "Max Exertion versus Average Recovery before injury",
       fill = "injury") +
  theme_bw() + # Set theme
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  scale_color_manual(values = c("0" = "blue", "1" = "red"), # Set color values
                     labels = c("0" = "Healthy", "1" = "Injury"))

# Generate graph
ggMarginal(athlete_ex, groupFill = TRUE)

# Create graph
week_ex <- ggplot(week, # Set data
                  aes(x = `avg recovery.1`, y = `max exertion`,color = factor(injury))) + # Set aesthetics
  geom_point(alpha = 0.3) + # Set geom point for scatter plot
  labs(x = "Avg Recovery week before Injury",  # Set labels
       y = "Max Exertion week of Injury",
       title = "Max Exertion versus Average Recovery before injury",
       fill = "injury") +
  theme_bw() + # Set theme
  theme(axis.line = element_line(colour = "black"), # Set axis line as black
        panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  scale_color_manual(values = c("0" = "blue", "1" = "red"), # Set color values
                     labels = c("0" = "Healthy", "1" = "Injury"))
# Generate graph
ggMarginal(week_ex, groupFill = TRUE)

set.seed(42069) # Set seed for reproducibility
week$injury_labels <- ifelse(week$injury == 1, 1, 0)
trainIndex <- createDataPartition(y = week$injury, p = 0.75, list = FALSE)
trainData <- week[trainIndex, ]
testData <- week[-trainIndex, ]

# c(1:4,6, 12:22, 73:76)
# Create training data
dtrain_1 <- xgb.DMatrix(data = as.matrix(trainData[,c(1:4,6, 12:22, 36:44, 58:66, 73:76)]), label = (((trainData$injury))))
# Create test data
dtest_1 <- xgb.DMatrix(data = as.matrix(testData[,c(1:4,6, 12:22, 36:44, 58:66, 73:76)]), label = (((testData$injury))))

fit_1 <- xgboost(dtrain_1,  # Set data set to use
                 nrounds = 200, # Set number of rounds
                 eta = 0.05 , 
                 verbose = 1, # 1 - Prints out fit
                 print_every_n = 20, # Prints out result every 20th iteration
                 
                 objective = "binary:logistic", # Set objective
                 eval_metric = "auc",
                 eval_metric = "error")

#set.seed(42069) # Set seed for reproducibility
#trainIndex <- createDataPartition(y = week$injury, p = 0.75, list = FALSE)
#trainData <- week[trainIndex, ]
#testData <- week[-trainIndex, ]

#threshold <- 0.5 # Set threshold
#trainData$injury_binary <- ifelse(trainData$injury > threshold, 1, 0) # Convert to binary values
#dtrain_1 <- xgb.DMatrix(data = as.matrix(trainData[,c(1:4,6, 12:22, 36:44, 58:66, 73:76)]), label = trainData$injury_binary) # Use binary labels
# Create test data
#testData$injury_binary <- ifelse(testData$injury > threshold, 1, 0) # Convert to binary values
#dtest_1 <- xgb.DMatrix(data = as.matrix(testData[,c(1:4,6, 12:22, 36:44, 58:66, 73:76)]), label = testData$injury_binary) # Use binary labels

#fit_1 <- xgboost(dtrain_1,  # Set data set to use
 #                nrounds = 200, # Set number of rounds
  #               eta = 0.05 , 
   #              verbose = 1, # 1 - Prints out fit
    #             print_every_n = 20, # Prints out result every 20th iteration
     #            objective = "reg:logistic", # Set objective
      #           eval_metric = "auc",
       #          eval_metric = "error"
#) # Choose one evaluation metric

preds_injury <- predict(fit_1, dtest_1)
roc1 = roc((testData$injury), preds_injury)
plot.roc(roc1, print.auc = TRUE, col = "red", print.auc.col = "red")

pred_class <- rep(0, length(preds_injury))

pred_class[which(preds_injury > 0.0145)] <- 1

fpred <- as.factor(pred_class)
ipred <- as.factor(testData$injury)
confusionMatrix(data = fpred, # Set predicted values
                reference = ipred) # Set true values

shap_result_1 <- shap.score.rank(xgb_model = fit_1, 
                                 X_train = as.matrix(trainData[,c(1:4,6, 12:22, 36:44, 58:66, 73:76)]),
                                 shap_approx = F)

shap_long_1 = shap.prep(shap = shap_result_1,
                        X_train =  as.matrix(trainData[,c(1:4,6, 12:22, 36:44, 58:66, 73:76)]), 
                        top_n = 8)

plot.shap.summary(data_long = shap_long_1)

## Future Steps Could possibly redo study to focus on the injuries of the top 10 most injured runners to see if they have any signficant metrics that lead to injury. 

athletes <- week[week$`Athlete ID` %in% athlete$`Athlete ID`,]

