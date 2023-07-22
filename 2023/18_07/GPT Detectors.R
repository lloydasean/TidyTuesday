library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(ggridges)

#load data
tuesdata <- tidytuesdayR::tt_load('2023-07-18')
tuesdata <- tidytuesdayR::tt_load(2023, week = 29)

detectors <- tuesdata$detectors

#explore data
head(detectors)

#was model prediction correct?
mutate(detectors,correct=0) 
detectors$correct[detectors$.pred_class==detectors$kind] <- TRUE
detectors$correct[detectors$.pred_class!=detectors$kind] <- FALSE

#calculate % of correct predictions and plot
detector_accuracy <- detectors %>% 
  group_by(detector) %>% 
  summarise(percent_correct=(sum(correct))/sum(correct=n()))

detector_accuracy %>% ggplot(aes(y=percent_correct,x=reorder(detector,percent_correct)))+
  geom_col(width=0.5, aes(fill=percent_correct))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_gradient("Accuracy",low = "#ff9696",high = "#b8ff96",labels=scales::percent)+
  labs(title="AI Detector Accuracy",x="Detector",y=NULL)+
  theme(axis.text.x=element_text(angle=50,vjust=0.6))


#compare GPT3 vs GPT4 on how undetectable their essays are
model_undetectable <- detectors %>%
  filter(model!="Human") %>% 
  group_by(model) %>% 
  summarise(percent_undetected=(sum(!correct))/sum(correct=n()))

model_undetectable %>% ggplot(aes(y=percent_undetected,x=model))+
  geom_col(width=0.5)+
  scale_y_continuous(labels=scales::percent)+
  labs(title="GPT3 vs GPT4",x="Model",y=NULL, subtitle="Percentage of essays written by AI that are undetected")

#native vs non-native English speakers 
false_positives <- detectors %>% 
  filter(kind=="Human") %>% 
  group_by(native,detector) %>% 
  summarise(percent_correct=(sum(correct))/sum(correct=n()))

false_positives %>% ggplot(aes(y=percent_correct,x=detector,fill=native))+
  geom_bar(stat="identity",position="dodge",width=0.5)+
  scale_y_continuous(labels=scales::percent)+
  labs(fill="Native English Speaker",title="Percentage of Correct Detections",subtitle="Native vs Non-Native English Spekaers",x="Detector",y=NULL)+
  theme(axis.text.x=element_text(angle=50,vjust=0.6))

