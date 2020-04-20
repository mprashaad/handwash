#Location of project folder
loc = "C:/Users/mpras/Desktop/washing"

setwd(loc)
Packages <- c("dplyr", "ggplot2","readxl", "stringr", "reshape2", "plyr", "ggpubr", "scales")
lapply(Packages, library, character.only = TRUE)
raw_data <- read_excel("SPH3104 Group Project Dataset.xlsx")

#Adding UID column
uid <- c(1:dim(raw_data)[1])
raw_data <-cbind(uid, raw_data)

#Renaming columns
new_names <- read.table("C:/Users/mpras/Desktop/washing/new_names.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
names(raw_data) <- new_names$V1

#Identifying columns to be converted into factors
factor_col <- c(4:8,16:19,21:81,83:95,104:105,107,110:123)
ordered_col <- c(20,82,96:103,106,108,109,124:138)

#Conversion of datatypes for question responses
factor_col_names <- colnames(raw_data)[factor_col]
raw_data[factor_col_names] <- lapply(raw_data[factor_col_names], factor)

ordered_col_names <- colnames(raw_data)[ordered_col]
raw_data[ordered_col_names] <- lapply(raw_data[ordered_col_names], factor)

#Check of data - Ethnicity
select(filter(raw_data, !is.na(ethnicity_other)), ethnicity_other)
#Due to the possibility that some Singaporeans may have indicated their...
#ethnicity by race rather than country, this column is to be disregarded

#Check of data - family members
fam_mem <- c(9:15)
sapply(raw_data[fam_mem],is.numeric)

#Removal of non-Singapore residents
levels(raw_data$city)
clean_data <- raw_data %>% filter(grepl("Singapore|Tampines|SG", city, ignore.case = TRUE))
removed <- raw_data$uid[!grepl("Singapore|Tampines|SG", raw_data$city, ignore.case = TRUE)]
print(removed)

#Family size calculation
clean_data <- clean_data %>%  mutate(fam_size = select(., fam_mem) %>% rowSums(na.rm = TRUE))
clean_data$fam_size <- clean_data$fam_size + 1

#Creating race variable
clean_data <- clean_data %>% mutate(race = ethnicity)
clean_data$race <- as.character(clean_data$race)

for (val_race in 1:nrow(clean_data)){
  if (clean_data[val_race, 'race'] == "Other" | clean_data[val_race, 'race'] == "Other Asian"){
    clean_data[val_race,'race'] <- as.character(clean_data[val_race, "ethnicity_other"])
  }
}

levels(clean_data$ethnicity)
levels(clean_data$ethnicity_other)
other_asian <- levels(clean_data$ethnicity_other)
chinese <- other_asian[c(1,3:4,6,8:10)]
chinese <- c(chinese, "Chinese from Taiwan")
unknown <- other_asian[c(5,7,11)]

for (val_race in 1:nrow(clean_data)){
  if (clean_data[val_race,'race'] %in% chinese)
  {
    clean_data[val_race,'race'] <- "Chinese"
  }
  if (clean_data[val_race,'race'] %in% unknown)
  {
    clean_data[val_race,'race'] <- "Unknown"
  }
}

clean_data$race <- as.factor(clean_data$race)

#Check and recoding of healthcare-related occupation question
sub<-filter(clean_data, !grepl("employment|Others",working_status))
summary(sub$working_status)
occ_no <- sub$uid[which(sub$healthcare_occ == "Yes")]
clean_data$healthcare_occ[which(clean_data$uid %in% occ_no)] <- "No"

#Check and recoding of rings and nails questions
sub<-filter(clean_data, rings == "No")
summary(sub$part_ring)
part_ring_no <- sub$uid[which(sub$part_ring == "Checked")]
clean_data$part_ring[which(clean_data$uid %in% part_ring_no)] <- "Unchecked"

sub<-filter(clean_data, nails == "No")
summary(sub$part_nails)
part_nails_no <- sub$uid[which(sub$part_nails == "Checked")]
clean_data$part_nails[which(clean_data$uid %in% part_nails_no)] <- "Unchecked"

#Check of responses to Q25
wash_livestock <- select(clean_data, contains("wash_livestock"))
wash_animal <- select(clean_data, contains("wash_animal"))
wash_garbage <- select(clean_data, contains("wash_garbage"))
wash_garden <- select(clean_data, contains("wash_garden"))

check_unique <- function(dataset){
  ans <- c()
  for (item in c(1:nrow(dataset))){
    cnt <- 0
    for (col_name in names(dataset)){
      if (dataset[item,col_name] == "Checked"){
        cnt <- cnt + 1
      }
    }
    if (cnt != 1){
      ans <- c(ans, item)
    }
  }
  
  return(ans)
}

uid_invalid_wash_livestock <- clean_data$uid[check_unique(wash_livestock)]
uid_invalid_wash_garden <- clean_data$uid[check_unique(wash_garden)]
uid_invalid_wash_garbage <-clean_data$uid[check_unique(wash_garbage)]
uid_invalid_wash_animal <- clean_data$uid[check_unique(wash_animal)]
uid_invalid_wash <- unique(c(uid_invalid_wash_animal,uid_invalid_wash_garbage,uid_invalid_wash_garden, uid_invalid_wash_livestock))
clean_data[which(clean_data$uid %in% uid_invalid_wash), c(56:79) ] <- NA
select(filter(clean_data, uid %in% uid_invalid_wash), contains("_livestock"))

#Check of Q31 responses given Q30 responses
table(clean_data$soap_refill, clean_data$soap_refill_method)
q31_na <- clean_data$uid[which(clean_data$soap_refill != "Refill the dispenser when it is almost empty")]
clean_data$soap_refill_method[which(clean_data$uid %in% q31_na)] <- NA

#Demographic data
nrow(clean_data) #total number of observations
min(clean_data$timestamp) #oldest observation
max(clean_data$timestamp) #newest observation

summary(clean_data$gender) #Gender
summary(clean_data$gender)/nrow(clean_data)

summary(clean_data$race) #Race
summary(clean_data$race)/nrow(clean_data)

summary(clean_data$fam_size) #Family size
summary(clean_data$education)
summary(clean_data$education)/nrow(clean_data)

summary(clean_data$working_status) #Working status and healthcare sector employment
summary(clean_data$working_status)/nrow(clean_data)
summary(clean_data$working_status_other)
summary(clean_data$healthcare_occ)
summary(clean_data$healthcare_occ)/nrow(clean_data)

summary(clean_data$disease) #Disease
summary(clean_data$disease)/nrow(clean_data)
summary(clean_data$disease_other)
summary(clean_data$disease_other)/nrow(clean_data)

summary(clean_data$age_group) #Age group
summary(clean_data$age_group)/nrow(clean_data)

summary(clean_data$marital_status) #Marital status
summary(clean_data$marital_status)/nrow(clean_data)


#Knowledge metric
q21_ans = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE)
q22_ans <- c(FALSE, FALSE, FALSE, FALSE, TRUE)

q21_score_func <- function(x){
  sum(q21_ans == slice(select(clean_data, 32:38),x))
}

q22_score_func <- function(x){
  sum(q21_ans == slice(select(clean_data, 39:43),x))
}

q21_score <- sapply(c(1:nrow(clean_data)),q21_score_func)
clean_data <- clean_data %>% mutate(score_21 = q21_score/length(q21_ans))

q22_score <- sapply(c(1:nrow(clean_data)),q22_score_func)
clean_data <- clean_data %>% mutate(score_22 = q22_score/length(q21_ans))

clean_data <- clean_data %>% mutate(knowledge = (length(q21_ans)*score_21 + length(q22_ans)*score_22)/length(c(q21_ans,q22_ans)))

#Knowledge findings
summary(clean_data$score_21)
summary(clean_data$score_22)
summary(clean_data$knowledge)

boxplot( clean_data$score_21, clean_data$score_22, clean_data$knowledge,
         names = c("Q21 Score", "Q22 Score", "Knowledge"),
         xlab = "Metric", # x axis label
         ylab = "Fraction Score", # y axis label
         frame.plot = FALSE, # don't draw a frame
         staplewex = 0, # don't draw staples
         staplecol = "white", # (fixes a tiny display issue)
         boxwex = .75, # narrow the boxes slightly
         boxfill = "grey80", # lightly shade the boxes
         whisklty = 1, # solid line for whiskers
         whiskcol = "grey70", # dim the whiskers
         boxcol = "grey70", # dim the box borders
         outcol = "grey70", # dim the outliers
         outpch = 20, # outliers as solid dots
         outcex = .5, # shrink the outliers
         medlty = "blank", # no line for the medians
         medpch = 20, # instead, draw solid dots
         medlwd = 1.5 # make them larger
         )



male <-  select(filter(clean_data,gender == "Male"), c(score_21, score_22, knowledge))
female <-  select(filter(clean_data,gender == "Female"), c(score_21, score_22, knowledge))
summary(male)
summary(female)

df.gender <- select(clean_data, c(gender,score_21, score_22, knowledge))
names(df.gender) <- c("gender", "Q21", "Q22", "Knowledge metric")
df.gender <- melt(df.gender, id.var = "gender")
names(df.gender) <- c("Gender", "Variable", "Fraction Score")
ggplot(data = df.gender, aes(x=Variable, y=`Fraction Score`)) + geom_boxplot(aes(fill=`Gender`))

#knowledge.healthcare <- select(clean_data, c(healthcare_occ,score_21, score_22, knowledge))
#knowledge.healthcare <- melt(knowledge.healthcare, id.var = "healthcare_occ")
#names(knowledge.healthcare) <- c("Healthcare-related occupation", "Variable", "Value")
#ggplot(data = knowledge.healthcare, aes(x=Variable, y=Value)) + geom_boxplot(aes(fill=`Healthcare-related occupation`))

#Handwashing metric
wash_metric_col <- c()
fraction_area <- c()
for (response in 1:nrow(clean_data)){
  q27_score <- list("< 5 seconds"=1, "5 - 10 seconds"=2, "11 - 19 seconds"=3, "20 seconds or more"=4)
  wash_metric_val <- (q27_score[[clean_data[response,"time_wash"]]])/4
  q29_score <- 0
  divisor <- 8
  for (col_val in c(84:93)){
    if (clean_data[response,col_val] == "Checked"){
      q29_score <- q29_score + 1
    }
  }
  if (clean_data[response,"rings"] == "Yes"){
    divisor <- divisor + 1
  }
  if (clean_data[response,"nails"] == "Yes"){
    divisor <- divisor + 1
  }
  wash_metric_val <- wash_metric_val + q29_score/divisor
  fraction_area <- c(fraction_area, q29_score/divisor)
  wash_metric_col <- c(wash_metric_col, wash_metric_val/2)
}

clean_data <- mutate(clean_data, wash_metric = wash_metric_col)
clean_data <- mutate(clean_data, fraction_wash = fraction_area)
levels(clean_data$time_wash) <- list("<5" = "< 5 seconds", "5-10" = "5 - 10 seconds", "11-19"="11 - 19 seconds", ">=20"="20 seconds or more")

#Vulnerable family
clean_data <- clean_data %>%  mutate(vulnerable = select(., infant,toddler,preschool,elderly) %>% rowSums(na.rm = TRUE))
clean_data$vulnerable <- as.factor(as.logical(clean_data$vulnerable))
levels(clean_data$vulnerable) <- c("No", "Yes")

#Accessory use
accessory_use <- c()
for (response in 1:nrow(clean_data)){
  use <- FALSE
  for (col_val in c(26:29)){
    if (clean_data[response,col_val] == "Yes"){
      use <- TRUE
    }
  }
  if (use){
    accessory_use <- c(accessory_use, "Yes")
  } else {
    accessory_use <- c(accessory_use, "No")
  }
}

clean_data <- mutate(clean_data, acc_use = accessory_use)

#GRAPHS

#Hand hygiene knowledge
know_data <- melt(clean_data[c("score_21", "score_22", "knowledge")])
levels(know_data$variable) <- list("Q21"="score_21","Q22"="score_22","Knowledge"="knowledge")
know_plot <- ggboxplot(know_data, x = "variable", y = "value",
                                  color = "variable", palette = "npg",
                                  add = "jitter", xlab = "Metrics for Hand Hygiene Knowledge",
                       ylab = "Percentage Score (%)", legend.title = "Metrics")
  
know_plot

#Hand hygiene knowledge by gender
shapiro.test(clean_data$score_21)
shapiro.test(clean_data$score_22)
shapiro.test(clean_data$knowledge)
compare_means(score_21 ~ gender, data = clean_data, 
              group.by = "dose")
know_gender_data <- melt(clean_data[c("gender","score_21", "score_22", "knowledge")], id.vars = "gender")
levels(know_gender_data$variable) <- list("Q21"="score_21","Q22"="score_22","Knowledge"="knowledge")
know_gender_plot <- ggboxplot(know_gender_data, x = "variable", y = "value",
                       color = "gender", palette = "npg",
                       add = "jitter", xlab = "Metrics for Hand Hygiene Knowledge",
                       ylab = "Percentage Score (%)", legend.title = "Gender") +
  stat_compare_means(method="wilcox.test",
                     aes(group = gender, label = paste0(..method.., "\n", " p = ", ..p.format..)),
                     label.y = 1.2)

know_gender_plot

#Gender
gender_plot <-ggplot(data=clean_data, aes(gender)) +
  geom_bar(aes(fill=gender, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Gender", y = "Percentage (%)") +
  scale_fill_discrete(name = "Gender", labels = c("Female", "Male")) +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                  y = 100*(..count..)/sum(..count..) + 2.5))
gender_plot

#Age
age_plot <-ggplot(data=clean_data, aes(age_group)) +
  geom_bar(aes(fill=age_group, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Age Group (Years Old)", y = "Percentage (%)") +
  scale_fill_discrete(name = "Age Group") +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                y = 100*(..count..)/sum(..count..) + 2.5))
age_plot

#Race
levels(clean_data$race) <- gsub(" ", "\n", levels(clean_data$race))
levels(clean_data$race)[3] <- "Eurasian/\nCaucasian/\nEuropean"
race_plot <-ggplot(data=clean_data, aes(race)) +
  geom_bar(aes(fill=race, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Race", y = "Percentage (%)") +
  scale_fill_discrete(name = "Race") +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                y = 100*(..count..)/sum(..count..) + 2.5)) +
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.45,
    default.unit="inch")
  )
race_plot

#Family
df.family <- select(clean_data, c(9:15))
df.family <- melt(df.family)
names(df.family) <- c("Family member", "Number")
df.family$`Family member` <- as.factor(as.numeric(df.family$`Family member`))
family_plot <- ggplot(df.family, aes(x = `Family member`, y = Number)) +
  geom_boxplot(aes(fill=`Family member`)) +
  scale_x_discrete(labels = c("Infant", "Toddler", "Pre-school", 
                              "Primary\nschool", "Secondary\nschool", "Adult", "Elderly")) +
  scale_fill_discrete(labels = c("Infant\n(0 to < 1)", "Toddler\n(1 to <3)", "Pre-school\n(3 to <6)", 
                                 "Primary\nschool\n(6 to <12)", "Secondary\nschool\n(12 to <18)",
                                 "Adult\n(18 to <60)", "Elderly\n(>=60)")) +
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.5,
    default.unit="inch")
  )
family_plot

#Family Size
family_size_plot <- ggplot(clean_data, aes(y=fam_size)) + geom_boxplot() + scale_x_discrete(labels = "") +
  labs(y = "Family Size") + scale_y_continuous(breaks=seq(0,16,2))
family_size_plot

#Education
edu_plot <-ggplot(data=clean_data, aes(education)) +
  geom_bar(aes(fill=education, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Highest Level of Education", y = "Percentage (%)") +
  scale_fill_discrete(name = "Education", labels= c("Secondary", "Tertiary/College\nor above")) +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                y = 100*(..count..)/sum(..count..) + 2.5))
edu_plot

#Marital status
marital_plot <-ggplot(data=clean_data, aes(marital_status)) +
  geom_bar(aes(fill=marital_status, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Marital Status", y = "Percentage (%)") +
  scale_fill_discrete(name = "Marital_status") +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                y = 100*(..count..)/sum(..count..) + 2.5))
marital_plot

#score_22 vs score_22
knowledge_corr <- ggplot(data = clean_data, aes(score_21, score_22)) +
  geom_jitter(width = 0.025, height = 0.025) + geom_smooth(method = "lm") +
  labs(x="Q21 Percentage Score (%)",y="Q22 Percentage Score (%)")+
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent)
knowledge_corr
cor(clean_data$score_21, clean_data$score_22)

#Employment
employ_plot <-ggplot(data=clean_data, aes(working_status)) +
  geom_bar(aes(fill=working_status, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Employment Category", y = "Percentage (%)") +
  scale_fill_discrete(name = "Employment", labels = c("Full-time", "Internship", "Part-time", "Retired/Unemployed", "Student")) +
  scale_x_discrete(labels = c("Full-time", "Internship", "Part-time", "Retired/\nUnemployed", "Student")) +
  guides("none") +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                y = 100*(..count..)/sum(..count..) + 2.5))
employ_plot

#Healthcare sector
healthsect_plot <-ggplot(data=clean_data, aes(healthcare_occ)) +
  geom_bar(aes(fill=healthcare_occ, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Response", y = "Percentage (%)") +
  scale_fill_discrete(name = "Response", labels = c("No", "Yes")) +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                y = 100*(..count..)/sum(..count..) + 2.5))
healthsect_plot

#Disease
labels_disease <- c("Acne", "Anxiety Disorder", "Asthma", "Atopic Dermatitis", "Chronic Yeast Infection",
                    "Depression", "High Cholesterol", "Sleep Disorder", "None")
disease_plot <-ggplot(data=clean_data, aes(disease_other)) +
  geom_bar(aes(fill=disease_other, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Disease", y = "Percentage (%)") +
  scale_fill_discrete(name = "Disease", labels = labels_disease) +
  scale_x_discrete(labels = gsub(" ", "\n", labels_disease)) +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                y = 100*(..count..)/sum(..count..) + 2.5))
disease_plot

#washing behaviour
wash_data <- melt(clean_data[c("fraction_wash","wash_metric")])
levels(wash_data$variable) <- list("Area Washed"="fraction_wash","Hand Washing"="wash_metric")
wash_plot <- ggboxplot(wash_data, x = "variable", y = "value",
                       color = "variable", palette = "npg",
                       add = "jitter", xlab = "Metrics for Hand Washing Behaviour",
                       ylab = "Percentage Score (%)", legend.title = "Metrics")+
  scale_y_continuous(labels = percent)

wash_plot

#time_wash
time_wash_plot <-ggplot(data=clean_data, aes(time_wash)) +
  geom_bar(aes(fill=time_wash, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Duration of Hand Washing (s)", y = "Percentage (%)") +
  scale_fill_discrete(name = "Duration (s)", labels = c("<5","5-10", "11-19", ">=20")) +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                y = 100*(..count..)/sum(..count..) + 2.5)) +
  scale_x_discrete(limits=c("<5","5-10", "11-19", ">=20"))
time_wash_plot

#wash_metric against gender
shapiro.test(clean_data$wash_metric)
wash_gender_plot <- ggboxplot(clean_data, x = "gender", y = "wash_metric",
               color = "gender", palette = "npg",
               add = "jitter", xlab = "Gender", ylab = "Hand Washing Metric", legend.title = "Gender") +
  stat_compare_means(method="wilcox.test",
                     aes(label = paste0(..method.., "\n", " p = ", ..p.format..)),
                     label.x = 1.45, label.y = 1) +
  scale_y_continuous(labels = percent)
wash_gender_plot

#wash_metric against healthcare_occ
shapiro.test(clean_data$wash_metric)
wash_healthcare_plot <- ggboxplot(clean_data, x = "healthcare_occ", y = "wash_metric",
                              color = "healthcare_occ", palette = "npg",
                              add = "jitter", xlab = "Healthcare-related Occupation", ylab = "Hand Washing Metric", legend.title = "Healthcare-related Occupation") +
  stat_compare_means(method="wilcox.test",
                     aes(label = paste0(..method.., "\n", " p = ", ..p.format..)),
                     label.x = 1.45, label.y = 1)+
  scale_y_continuous(labels = percent)
wash_healthcare_plot

#wash_metric against vulnerable
shapiro.test(clean_data$wash_metric)
wash_vulnerable_plot <- ggboxplot(clean_data, x = "vulnerable", y = "wash_metric",
                                  color = "vulnerable", palette = "npg",
                                  add = "jitter", xlab = "Vulnerable Age-group Family Members", ylab = "Hand Washing Metric",
                                  legend.title = "Vulnerable Age-group Family Members") +
  stat_compare_means(method="wilcox.test",
                     aes(label = paste0(..method.., "\n", " p = ", ..p.format..)),
                     label.x = 1.45, label.y = 1) +
  scale_y_continuous(labels = percent)
wash_vulnerable_plot

#wash_metric vs knowledge
knowledge_wash_corr <- ggplot(data = clean_data, aes(knowledge, wash_metric)) + 
  geom_jitter(width = 0.025, height = 0.025) + geom_smooth(method = "lm") +
  labs(x="Knowledge Metric",y="Hand Washing Metric")+
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent)
knowledge_wash_corr
cor(clean_data$knowledge,clean_data$wash_metric)

#Important factors in public washrooms

colSums(clean_data[110:123] == "Important")
washroom_data_var <- c("Overall cleanliness","Tissue paper availability",
                       "Toilet drum cleanliness","Disposable toilet seat\ncover availability",
                       "Toilet seat disinfectant\navailability","Hand sanitiser/soap availability",
                       "Paper towel availability",
                       "Paper towel location easily seen","Ease of getting paper\ntowels from dispenser",
                       "Distance between wastebasket\nand hand dryer","Hand dryer availability",
                       "Space for temporary storage of\nbags during handwashing",
                       "Potential recontamination of\nwashed hands by faucet",
                       "Potential recontamination of\nwashed hands by door handle")
washroom_data_val <- c(124, 119, 122, 59, 79, 117, 106, 96, 96, 58, 59, 89, 96, 97)
washroom_data <- data.frame('var'= factor(washroom_data_var, levels = washroom_data_var), 'val'= washroom_data_val)
washroom_plot <-ggplot(data=washroom_data, aes(x=var, y=100*val/nrow(clean_data))) +
  geom_bar(stat="identity", aes(fill=var)) +
  labs(x = "Factor", y = "Percentage (%)") +
  scale_fill_discrete(name = "Factor", labels = washroom_data_var) +
  geom_text(stat = "identity", aes(label = sprintf("%s\n(n = %s)", round(100*val/nrow(clean_data), 1), val),
                                y = 100*val/nrow(clean_data) + 6)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())+
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.35,
    default.unit="inch")
  )
washroom_plot

#health vs wash_metric
shapiro.test(clean_data$wash_metric)
wash_health_plot <- ggboxplot(clean_data, x = "health", y = "wash_metric",
                                  color = "health", palette = "npg",
                                  add = "jitter", xlab = "Perception of Health", ylab = "Wash Metric",
                                  legend.title = "Perception of Health",
                              order = c("Poor", "Fair", "Average", "Good", "Excellent")) +
  stat_compare_means(method="kruskal.test",
                       aes(label = paste0(..method.., "\n", "   p = ", ..p.format..)),
                       label.x = 0.8, label.y = 2)
wash_health_plot 

#wash_fraction vs time_wash
compare_means(fraction_wash ~ time_wash,  data = clean_data)
shapiro.test(clean_data$fraction_wash)
wash_fraction_time_plot <- ggboxplot(clean_data, x = "time_wash", y = "fraction_wash",
                              color = "time_wash", palette = "npg",
                              add = "jitter", xlab = "Washing Time (s)", ylab = "Percentage Washed",
                              legend.title = "Washing Time (s)",
                              order = c("<5","5-10", "11-19", ">=20")) +
  stat_compare_means(method="kruskal.test",
                     aes(label = paste0(..method.., "\n", "   p = ", ..p.adj..)),
                     label.x = 2.3, label.y = 1.25) +
  stat_compare_means(label = "p.format", method = "wilcox.test",ref.group = ".all.", label.y = 1.1) +
  geom_hline(yintercept = mean(clean_data$fraction_wash), linetype = 2) +
  scale_y_continuous(labels=percent)
wash_fraction_time_plot

#health
health_plot <-ggplot(data=clean_data, aes(health)) +
  geom_bar(aes(fill=health, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Perception of Health", y = "Percentage (%)") +
  scale_fill_discrete(name = "Perception of Health") +
  scale_x_discrete(limits = c("Poor", "Fair", "Average", "Good", "Excellent")) +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                y = 100*(..count..)/sum(..count..) + 2.5))
health_plot

#urti vs vaccination
urti_flu <- table(clean_data$urti, clean_data$flu)
chisq.test(urti_flu)

#rings vs part_ring
wear_wash_ring <- table(clean_data$rings, clean_data$part_ring)
chisq.test(wear_wash_ring)

#fraction_wash vs acc_use
shapiro.test(clean_data$fraction_wash)
acc_fraction_plot <- ggboxplot(clean_data, x = "acc_use", y = "fraction_wash",
                                     color = "acc_use", palette = "npg",
                                     add = "jitter", xlab = "Accessory Use", ylab = "Area Washed",
                                     legend.title = "Accessory Use") +
  stat_compare_means(method="wilcox.test",
                     aes(label = paste0(..method.., "\n", " p = ", ..p.adj..)),
                     label.x = 1.4, label.y = 1.25)
acc_fraction_plot

#acc_use makes no significant difference for wash_metric as well

ggboxplot(clean_data, x = "acc_use", y = "wash_metric",
          color = "acc_use", palette = "npg",
          add = "jitter", xlab = "Accessory Use", ylab = "Hand Washing Metric",
          legend.title = "Accessory Use") +
  stat_compare_means(method="wilcox.test",
                     aes(label = paste0(..method.., "\n", " p = ", ..p.adj..)),
                     label.x = 1.4, label.y = 1.25)

#ignore hand hygiene
summary(clean_data[132:138])
labels_ignore <- c("In a hurry", "No free hands", "Only urinated",
                   "Inadequate handwashing facilities",
                   "Inadequate hand drying facilities",
                   "Alone in washroom", "Washroom is too dirty")
ignore_var <- c(rep(labels_ignore,3))
ignore_cond <- c(rep("Always",7),rep("Sometimes",7),rep("Never",7))
ignore_val <- c(1,1,2,5,2,2,7,12,23,5,35,10,4,30,111,100,117,84,112,118,87)
df.ignore <- data.frame(var=ignore_var, cond=ignore_cond, val=ignore_val)

ignore_plot <- ggplot(data=df.ignore, aes(fill=factor(cond, levels=c("Always", "Sometimes", "Never")), y=val, x=var)) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Factor", y = "Percentage") +
  scale_fill_discrete(name = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_y_continuous(labels = percent)
ignore_plot

#wash_type
summary(clean_data[46:79])
labels_washtype <- c("Before handling food or cooking", "After handling food or cooking", "Before eating", "After urination",
                     "After defecation", "After feeding a child", "After caring for sick person", "After daily work",
                     "When hands are visibly dirty", "After sneezing or coughing", "After touching livestock",
                     "After touching animal waste", "After garbage disposal", "After gardening")
washtype_var <- c(rep(labels_washtype,each=6))
washtype_cond <- c(rep(c("Alcohol handrub", "None", "N.A.","Water + Soap","Water","Wet Wipes"),14))
washtype_val <- c(0,0,2,92,30,0,
                  0,0,2,100,22,0,
                  1,11,0,65,46,1,
                  0,1,0,72,51,0,
                  0,0,0,115,9,0,
                  0,3,65,32,24,0,
                  5,5,51,61,2,0,
                  2,18,11,75,18,0,
                  1,0,0,120,3,0,
                  7,19,3,58,35,2,
                  2,1,43,52,5,0,
                  2,0,55,46,0,0,
                  0,1,4,82,16,0,
                  0,0,41,56,6,0)
df.washtype <- data.frame(var=washtype_var, cond=washtype_cond, val=washtype_val)
washtype_plot <- ggplot(data=df.washtype,
                        aes(fill=factor(cond,
                                        levels=c("Wet Wipes","Alcohol handrub", "None", "N.A.","Water","Water + Soap")),
                            y=val, x=var)) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Situation", y = "Percentage (%)") +
  scale_fill_discrete(name = "Method of hand hygiene") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_y_continuous(labels = percent)
washtype_plot


#hand drying public washrooms
summary(clean_data[96:103])
labels_dry_public <- c("Rubbing hands on own clothing", "Drying by evaporation in air",
                   "Use personal towel or handkerchief",
                   "Use own disposable tissue",
                   "Paper towels supplied by the washroom",
                   "Warm hand dryer", "Jet hand dryer", "Cloth towel rolls")
dry_public_var <- c(rep(labels_dry_public,each=3))
dry_public_cond <- c(rep(c("Always","Sometimes","Never"),8))
dry_public_val <- c(41,67,16,
                    22,84,18,
                    11,43,70,
                    8,70,46,
                    45,70,9,
                    10,60,54,
                    13,57,54,
                    11,59,54)
df.dry_public <- data.frame(var=dry_public_var, cond=dry_public_cond, val=dry_public_val)

dry_public_plot <- ggplot(data=df.dry_public, aes(fill=factor(cond, levels=c("Never", "Sometimes", "Always")), y=val, x=var)) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Method of Hand Drying", y = "Percentage") +
  scale_fill_discrete(name = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_y_continuous(labels = percent)
dry_public_plot

#Hand drying at home
dry_home_plot <-ggplot(data=clean_data, aes(home_dry)) +
  geom_bar(aes(fill=home_dry, y = 100*(..count..)/sum(..count..))) + 
  labs(x = "Method of Hand Drying", y = "Percentage (%)") +
  scale_fill_discrete(name = "Method of Hand Drying", labels= c("A towel shared\nwith family members" ,"Drying by evaporation\nin air", "Paper towels",
                                                               "Rubbing hands on\nclothes", "Use personal towel", "Warm hand dryer")) +
  geom_text(stat = "count", aes(label = sprintf("%s (n = %s)", round(100*(..count..)/sum(..count..), 1), ..count..),
                                y = 100*(..count..)/sum(..count..) + 2.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_x_discrete(labels = c("A towel shared\nwith family members",
                              "Drying by evaporation\nin air", "Paper towels",
                              "Rubbing hands on\nclothes", "Use personal towel", "Warm hand dryer")) +
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.35,
    default.unit="inch")
  )
dry_home_plot

#habits in public washrooms
summary(clean_data[124:131])
labels_habit_public <- c("Shaking my hands several times to get\nrid of excess water before hand drying",
                       "Use a paper towel that you have used to\ndry your hands to turn off the faucet",
                       "Use a new paper towel to turn off the faucet",
                       "Use water to splash the faucet before\nturning it off",
                       "Use a paper towel that you have used to\ndry your hands to open the door of washroom upon exit",
                       "Use a new paper towel to open the door\nof washroom upon exit",
                       "Upon leaving the washroom, will try to\nlet other people open the door",
                       "Not using the hand dryer if it is too\nclose to the wastebasket without lid")
habit_public_var <- c(rep(labels_habit_public,each=3))
habit_public_cond <- c(rep(c("Always","Never","Sometimes"),8))
habit_public_val <- c(71,13,40,
                    7,96,21,
                    4,106,14,
                    3,24,62,
                    6,93,25,
                    6,104,14,
                    11,51,62,
                    20,72,32)
df.habit_public <- data.frame(var=habit_public_var, cond=habit_public_cond, val=habit_public_val)

habit_public_plot <- ggplot(data=df.habit_public, aes(fill=factor(cond, levels=c("Never", "Sometimes", "Always")), y=val, x=var)) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Habit", y = "Percentage") +
  scale_fill_discrete(name = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  scale_y_continuous(labels = percent) +
  coord_flip()
habit_public_plot