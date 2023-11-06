library(tidyverse)
library(dplyr)
library(sciplot)
library(lsr)
library(ggplot2)


knitr::opts_chunk$set(echo = TRUE)

########## Loading Data into R ##########
data_orig <- read.csv("mTurk_sp23_G23514 - mTurk_sp23_G23514.csv")

view(data_orig)

########## Data Wrangling and Mutation for Analysis ##########
data_1 <- data_orig %>%
  select(control, Question.1, Question.2, Question.1.1, Question.2.1,race_2, race_3, race_4,
         race_6, race_7, race_8, race_9, race_9_TEXT, Residence, Residence_5_TEXT, HIncome, ethnicity, incomechange) %>%
  subset(control == 1) %>%
  na_if("") %>%
  mutate(race = coalesce(race_2, race_3, race_4, race_6, race_7, race_8, race_9, race_9_TEXT)) %>%
  select(!c(race_2, race_3, race_4, race_6, race_7, race_8, race_9, race_9_TEXT)) %>%
  mutate(treatment = ifelse(grepl('increased', incomechange), "increase", "decrease")) %>%
  select(!c(control, incomechange, Residence_5_TEXT)) %>%
  rename("question.1.pre" = "Question.1", "question.2.pre" = "Question.2" , "question.1.post" = "Question.1.1", "question.2.post" = "Question.2.1")

view(data_1)

data_final <- data_1 %>%
  mutate(question.1.pre = ifelse(question.1.pre == "Less than $100", 0, 
                                 ifelse(question.1.pre == "$100  - $199", 1, 
                                        ifelse(question.1.pre == "$200 - $299", 2, 
                                               ifelse(question.1.pre == "$300 - $399", 3,
                                                      ifelse(question.1.pre == "$400 - $499", 4,
                                                             ifelse(question.1.pre == "Greater than $500", 5, NA))))))) %>%
  mutate(question.1.post = ifelse(question.1.post == "Less than $100", 0, 
                                  ifelse(question.1.post == "$100  - $199", 1, 
                                         ifelse(question.1.post == "$200 - $299", 2, 
                                                ifelse(question.1.post == "$300 - $399", 3,
                                                       ifelse(question.1.post == "$400 - $499", 4,
                                                              ifelse(question.1.post == "Greater than $500", 5, NA))))))) %>%
  mutate(question.2.pre = ifelse(question.2.pre == "Less than 1 Mile", 0, 
                                 ifelse(question.2.pre == "1 - 5 Miles", 1, 
                                        ifelse(question.2.pre == "5 - 10 Miles", 2, 
                                               ifelse(question.2.pre == "10 - 15 Miles", 3,
                                                      ifelse(question.2.pre == "Over 15 Miles", 4, NA)))))) %>%
  mutate(question.2.post = ifelse(question.2.post == "Less than 1 Mile", 0, 
                                  ifelse(question.2.post == "1 - 5 Miles", 1, 
                                         ifelse(question.2.post == "5 - 10 Miles", 2, 
                                                ifelse(question.2.post == "10 - 15 Miles", 3,
                                                       ifelse(question.2.post == "Over 15 Miles", 4, NA)))))) %>%
  mutate(Residence = ifelse(Residence == "Rural", 0, 
                            ifelse(Residence == "Suburb", 1, 
                                   ifelse(Residence == "Small city", 2, 
                                          ifelse(Residence == "Large city", 3, NA))))) %>%
  mutate(HIncome = ifelse(HIncome == "Less than $10,000", 0,
                          ifelse(HIncome == "$10,000 - $19,999", 1,
                                 ifelse(HIncome == "$20,000 - $29,999", 2,
                                        ifelse(HIncome == "$30,000 - $39,999", 3,
                                               ifelse(HIncome == "$40,000 - $49,999", 4,
                                                      ifelse(HIncome == "$50,000 - $59,999", 5,
                                                             ifelse(HIncome == "$60,000 - $69,999", 6,
                                                                    ifelse(HIncome == "$70,000 - $79,999", 7,
                                                                           ifelse(HIncome == "$80,000 - $89,999", 8,
                                                                                  ifelse(HIncome == "$90,000 - $99,999", 9,
                                                                                         ifelse(HIncome == "$100,000 - $149,999", 10,
                                                                                                ifelse(HIncome == "More than $150,000", 11, NA))))))))))))) %>%
  mutate(race = ifelse(race == "American Indian or Alaskan Native", 1,
                       ifelse(race == "Asian or Asian American", 2,
                              ifelse(race == "Black or African American", 3,
                                     ifelse(race == "Middle Eastern or Northern African", 4,
                                            ifelse(race == "Native Hawaiian or Other Pacific Islander", 5,
                                                   ifelse(race == "White or Caucasian", 6,
                                                          ifelse(race == "Other (please identify)", 7, NA))))))))

view(data_final)

########### Creating 2 Subsets containing either the increase or decrease treatment ##########

data_increase <- data_final %>%
  subset(treatment == "increase") %>%
  select(!treatment)

view(data_increase)

data_decrease <- data_final %>%
  subset(treatment == "decrease")%>%
  select(!treatment)

view(data_decrease)

data_hispanic <- data_final%>%
  filter(ethnicity == "Yes")

data_non_hispanic <- data_final%>%
  filter(ethnicity == "No")


########## Pre-Tests ##########
#Shapiro-Wilk Normality Test

#Test Increase Treatment
with(data_final, shapiro.test(question.1.pre[treatment == "increase"])) # p-value = 2.755e-05
with(data_final, shapiro.test(question.1.post[treatment == "increase"])) # p-value = 9.683e-05
with(data_final, shapiro.test(question.2.pre[treatment == "increase"])) # p-value = 2.159e-07
with(data_final, shapiro.test(question.2.post[treatment == "increase"])) # p-value = 3.526e-07

#Test Decrease Treatment
with(data_final, shapiro.test(question.1.pre[treatment == "decrease"])) # p-value = 0.0001014
with(data_final, shapiro.test(question.1.post[treatment == "decrease"])) # p-value = 1.307e-05
with(data_final, shapiro.test(question.2.pre[treatment == "decrease"])) # p-value = 3.693e-07
with(data_final, shapiro.test(question.2.post[treatment == "decrease"])) # p-value = 6.513e-08

#Conclusion: all p-Values are very low, which concludes that there is very strong evidence against the null hypothesis.

#Non-parametric Two-Samples Wilcoxon Rank Test
var.test(question.1.pre ~ treatment, data = data_final) # p-value = 0.9221
var.test(question.1.post ~ treatment, data = data_final) # p-value = 0.08029
var.test(question.2.pre ~ treatment, data = data_final) # p-value = 0.4847
var.test(question.2.post ~ treatment, data = data_final) # p-value = 0.9382

#Conclusion: While the test showed minimal evidence againt the null hypothesis for question.1.post, 
#overall the test concluded that there is no evidence against the null hypothesis.

# Paired T-Test
t_1_increase <- t.test(data_increase$question.1.pre, data_increase$question.1.post, paired = TRUE)
t_1_decrease <- t.test(data_decrease$question.1.pre, data_decrease$question.1.post, paired = TRUE)

t_2_increase <- t.test(data_increase$question.2.pre, data_increase$question.2.post, paired = TRUE)
t_2_decrease <- t.test(data_decrease$question.2.pre, data_decrease$question.2.post, paired = TRUE)



t_1_increase # t = 0.13404, df = 82, p-value = 0.8937
t_1_decrease # t = 2.1269, df = 87, p-value = 0.03626

t_2_increase # t = -1.0213, df = 82, p-value = 0.3101
t_2_decrease # t = 0.66014, df = 87, p-value = 0.5109


#Conclusion: Since P-Values are not consistently low enough to discount the null hypothesis for any of the tests completed, it can be concluded that neither
#the increase nor decrease treatment made much difference between





########## Graphing change in answers ##########

facet_labs <- c("Hispanic", "Non-Hispanic")
names(facet_labs) <- c("Yes", "No")

########## Increase Treatment ##########
# Question 1 - Spending
data_increase_pivot_question_1 <- data_increase %>%
  select(!c(question.2.pre, question.2.post)) %>%
  pivot_longer(c(question.1.pre, question.1.post),
               names_to = "question",
               values_to = "response") %>%
  mutate(question = ifelse(question == "question.1.pre", 0,
                           ifelse(question == "question.1.post", 1, NA)))

ggplot(data = data_increase_pivot_question_1, mapping = aes(x = question , y = response, color = ethnicity)) + 
  geom_jitter() + 
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ethnicity, labeller = labeller(ethnicity = facet_labs)) + 
  xlab("Question: Pre- (0) & Post- (1) Treatment") +
  ylab("Response (Refer to Key)")

#Question 2 - Travel Distance
data_increase_pivot_question_2 <- data_increase %>%
  select(!c(question.1.pre, question.1.post)) %>%
  pivot_longer(c(question.2.pre, question.2.post),
               names_to = "question",
               values_to = "response") %>%
  mutate(question = ifelse(question == "question.2.pre", 0,
                           ifelse(question == "question.2.post", 1, NA)))

ggplot(data = data_increase_pivot_question_2, mapping = aes(x = question , y = response, color = ethnicity)) + 
  geom_jitter() + 
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ethnicity, labeller = labeller(ethnicity = facet_labs)) + 
  xlab("Question: Pre- (0) & Post- (1) Treatment") +
  ylab("Response (Refer to Key)")



########## Decrease Treatment ##########
# Question 1 - Spending
data_decrease_pivot_question_1 <- data_decrease %>%
  select(!c(question.2.pre, question.2.post)) %>%
  pivot_longer(c(question.1.pre, question.1.post),
               names_to = "question",
               values_to = "response") %>%
  mutate(question = ifelse(question == "question.1.pre", 0,
                           ifelse(question == "question.1.post", 1, NA)))

ggplot(data = data_decrease_pivot_question_1, mapping = aes(x = question , y = response, color = ethnicity)) + 
  geom_jitter() + 
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ethnicity, labeller = labeller(ethnicity = facet_labs)) + 
  xlab("Question: Pre- (0) & Post- (1) Treatment") +
  ylab("Response (Refer to Key)")

# Question 2 - Travel Distance
data_decrease_pivot_question_2 <- data_decrease %>%
  select(!c(question.1.pre, question.1.post)) %>%
  pivot_longer(c(question.2.pre, question.2.post),
               names_to = "question",
               values_to = "response") %>%
  mutate(question = ifelse(question == "question.2.pre", 0,
                           ifelse(question == "question.2.post", 1, NA)))

ggplot(data = data_decrease_pivot_question_2, mapping = aes(x = question , y = response, color = ethnicity)) + 
  geom_jitter() + 
  geom_smooth(method = 'lm', se = FALSE) +
  facet_wrap(~ethnicity, labeller = labeller(ethnicity = facet_labs)) + 
  xlab("Question: Pre- (0) & Post- (1) Treatment") +
  ylab("Response (Refer to Key)")

########## New Graphs ##########

########## Increase Treatment ##########
# Question 1 - Spending
graph_increase_question_1 <- data_increase_pivot_question_1 %>%
  group_by(ethnicity,question) %>%
  select(!c(race,Residence,HIncome))%>%
    summarise_all(list(
            mean = mean,
            sd = sd
            ))
graph_increase_question_1$ethnicity<-as.factor(graph_increase_question_1$ethnicity)
graph_increase_question_1$question<-as.factor(graph_increase_question_1$question)

ggplot(graph_increase_question_1, mapping = aes(x = question , y = mean, fill = question)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +
  facet_wrap(~ethnicity, labeller = labeller(ethnicity = facet_labs)) + 
  scale_x_discrete(labels = c("Pre", "Post")) +
  xlab("Question 1: Average Grocery Store Spending") +
  ylab("Mean Response") +
  ggtitle("Increase Treatment Group") +
  coord_cartesian(ylim=c(0,3))  +
  scale_fill_manual(values=c("darkred", "grey32"), 
                     labels = c("Pre", "Post")) +
  labs(fill = "Pre/Post") +
  theme(plot.title = element_text(hjust = 0.5)) 

#Question 2 - Travel Distance
graph_increase_question_2 <- data_increase_pivot_question_2 %>%
  group_by(ethnicity,question) %>%
  select(!c(race,Residence,HIncome))%>%
  summarise_all(list(
    mean = mean,
    sd = sd
  ))
graph_increase_question_2$ethnicity<-as.factor(graph_increase_question_2$ethnicity)
graph_increase_question_2$question<-as.factor(graph_increase_question_2$question)

ggplot(graph_increase_question_2, mapping = aes(x = question , y = mean, fill = question)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +
  facet_wrap(~ethnicity, labeller = labeller(ethnicity = facet_labs)) + 
  scale_x_discrete(labels = c("Pre", "Post")) +
  xlab("Question 2: Distance Travel to Grocery Store") +
  ylab("Mean Distance (Miles)") +
  ggtitle("Increase Treatment Group") +
  coord_cartesian(ylim=c(0,3))  +
  scale_fill_manual(values=c("darkred", "grey32"), 
                    labels = c("Pre", "Post")) +
  labs(fill = "Pre/Post") +
  theme(plot.title = element_text(hjust = 0.5)) 


########## Decrease Treatment ##########
# Question 1 - Spending
graph_decrease_question_1 <- data_decrease_pivot_question_1 %>%
  group_by(ethnicity,question) %>%
  select(!c(race,Residence,HIncome))%>%
  summarise_all(list(
    mean = mean,
    sd = sd
  ))
graph_decrease_question_1$ethnicity<-as.factor(graph_decrease_question_1$ethnicity)
graph_decrease_question_1$question<-as.factor(graph_decrease_question_1$question)

ggplot(graph_decrease_question_1, mapping = aes(x = question , y = mean, fill = question)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +
  facet_wrap(~ethnicity, labeller = labeller(ethnicity = facet_labs)) + 
  scale_x_discrete(labels = c("Pre", "Post")) +
  xlab("Question 1: Average Grocery Store Spending") +
  ylab("Mean Response") +
  ggtitle("Decrease Treatment Group") +
  coord_cartesian(ylim=c(0,3))  +
  scale_fill_manual(values=c("darkred", "grey32"), 
                    labels = c("Pre", "Post")) +
  labs(fill = "Pre/Post") +
  theme(plot.title = element_text(hjust = 0.5)) 

#Question 2 - Travel Distance
graph_decrease_question_2 <- data_decrease_pivot_question_2 %>%
  group_by(ethnicity,question) %>%
  select(!c(race,Residence,HIncome))%>%
  summarise_all(list(
    mean = mean,
    sd = sd
  ))

graph_decrease_question_2$ethnicity<-as.factor(graph_decrease_question_2$ethnicity)
graph_decrease_question_2$question<-as.factor(graph_decrease_question_2$question)

ggplot(graph_decrease_question_2, mapping = aes(x = question , y = mean, fill = question)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +
  facet_wrap(~ethnicity, labeller = labeller(ethnicity = facet_labs)) + 
  scale_x_discrete(labels = c("Pre", "Post")) +
  xlab("Question 2: Distance Travel to Grocery Store") +
  ylab("Mean Distance (Miles)") +
  ggtitle("Decrease Treatment Group") +
  coord_cartesian(ylim=c(0,3))  +
  scale_fill_manual(values=c("darkred", "grey32"), 
                    labels = c("Pre", "Post")) +
  labs(fill = "Pre/Post") +
  theme(plot.title = element_text(hjust = 0.5))


 
  

# Create sample data
set.seed(123)
data <- data.frame(
  Identifier = sample(c(0, 1), 100, replace = TRUE),
  Treatment = sample(c("A", "B"), 100, replace = TRUE),
  Answer = rnorm(100, mean = c(10, 15)[Identifier + 1], sd = 2)
)

# Calculate average answer for each treatment by Identifier
avg_data <- aggregate(Answer ~ Treatment + Identifier, data, mean)

# Plot histogram with data labels and trendline
ggplot(graph_question_1_pre, aes(x = ethnicity, y = question.1.pre, fill = as.factor(ethnicity))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = round(question.1.pre, 2)), vjust = -0.5, color = "black") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  labs(title = "Average Answer by Treatment", x = "Treatment", y = "Average Answer") +
  scale_fill_discrete(name = "Identifier") +
  theme_minimal()





