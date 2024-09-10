library(ggplot2)
library(dplyr)
##
data = read.csv( file.choose(), header = TRUE )
x<-data$home_team
y<-unique(x)
home<-c(rep(0,length(y)))
home
for(i in 1:length(data$home_team))
{
  if(data$home_score[i]>data$away_score[i])
  {
      z<-which(y==data$home_team[i])
      home[z]=home[z]+1; 
  }
}

ind<-which(home==max(home))
print("THE BEST HOME TEAM OF ALL TIME")
print(y[ind])

##
era1<-c("")
era2<-c("")
era3<-c("")
for(i in 1:length(data$date))
{
  d<-substring(data$date[i],1,2)
  d2<-substring(data$date[i],1,2)
  if(d=="18")
  {
    era1<-append(era1,data$home_team[i])
  }else if(d=="19"){
    era2<-append(era2,data$home_team[i])
  }else
  {
    era3<-append(era3,data$home_team[i])
  }
}
era1<-unique(era1)
era1
era2<-unique(era2)
era2
era3<-unique(era3)
era3
e1<-c(rep(0,length(era1)))
e2<-c(rep(0,length(era2)))
e3<-c(rep(0,length(era3)))

for (i in 1:length(data$date)) {
  d <- substring(data$date[i], 1, 2)
  if (d %in% c("18", "19", "20")) {
    if (d == "18") {
        if (data$home_score[i] > data$away_score[i]) {
          j <- which(era1 == data$home_team[i])
          e1[j] <- e1[j] + 1
        }
    } else if (d == "19") {
        if (data$home_score[i] > data$away_score[i]) {
          j <- which(era2 == data$home_team[i])
          e2[j] <- e2[j] + 1
      }
    } else if (d == "20") {
      if (data$home_score[i] > data$away_score[i]) {
        j <- which(era3 == data$home_team[i])
        e3[j] <- e3[j] + 1
      }
    }
  }
}

ind<-which(e1==max(e1))
print("DOMINATION OF COUNTRY IN 18 era")
print(era1[ind])
ind<-which(e2==max(e2))
print("DOMINATION OF COUNTRY IN 19 era")
print(era2[ind])
ind<-which(e3==max(e3))
print("DOMINATION OF COUNTRY IN 20 era")
print(era3[ind])

##
non_participating_hosts <- subset(data, country != home_team & neutral == TRUE)
hosted_counts <- table(non_participating_hosts$country)
most_matches_hosted <- names(hosted_counts[which.max(hosted_counts)])
cat("COUNTRY WHICH HOSTED MOST MATCHES WHERE THEY ARE NOT PARTICIPATING:", most_matches_hosted, "\n")

##
team<-c("")
for(i in 1:length(data$home_team))
{
  if(data$tournament[i]=="Friendly")
  {
    team<-append(team,data$home_team[i])
  }
}
team<-unique(team)
tour<-c(rep(0,length(team)))
for(i in 1:length(data$home_team))
{
  if(data$tournament[i]=="Friendly")
  {
    if(data$home_team[i]>data$away_team[i])
    {
      j<-which(team==data$home_team[i])
      tour[j]=tour[j]+1
    }
  }
}
ind<-which(tour==max(tour))
print("TEAM WHICH IS ACTIVE IN FRIENDLY TOURNAMENT")
print(team[ind])


combined_scores <- rbind(
  data.frame(Score = data$home_score, Type = "Home Score"),
  data.frame(Score = data$away_score, Type = "Away Score")
)

#Which away team Cuba or France or Greece is consistent in scoring goals in Fifa World Cup qualification

s1<-subset(data,tournament=='FIFA World Cup qualification'& away_team=='Cuba')
s2<-subset(data,tournament=='FIFA World Cup qualification'& away_team=='France')
s3<-subset(data,tournament=='FIFA World Cup qualification'& away_team=='Greece')
cv1=mean(s1$away_score)/sd(s1$away_score) *100
cv2=mean(s2$away_score)/sd(s2$away_score)*100
cv3=mean(s3$away_score)/sd(s3$away_score)*100
p=c(cv1,cv2,cv3)
if(min(p)==cv1)
{
  cat("Team Cuba is a more consistent goal scorer")
}else if(min(p)==cv2){
  cat("Team France is a more consistent goal scorer")
}else{
  cat("Team Greece is a more consistent goal scorer")
}

# Testing of Hypothesis

cat("\nNull Hypothesis (H0): There is no home advantage in football, and teams perform equally well in home and away matches.\n")
cat("Alternative Hypothesis (Ha): Teams perform better in home matches than in away matches.\n")

str(data) #To Check the structure of the data

data$score_difference <- data$home_score - data$away_score

test_result <- t.test(data$score_difference, mu = 0)

# Check the result
#print(test_result)

# Interpret the result
if (test_result$p.value < 0.05) {
  cat("Reject the null hypothesis. \nThus,Teams perform better in home matches.\n")
  cat("There is home advantage in football!!\n")
} else {
  cat("Fail to reject the null hypothesis. No significant difference in performance between home and away matches.\n")
}

# Perform a one-way ANOVA

cat("\nHo : There are no significant differences among the away_score means for different tournament types\nHa : There are significant differences among the away_score means for different tournament types")
result <- aov(away_score ~ tournament, data = data)
# Print the ANOVA summary
d=summary(result)
#print(d)
DFn <- d[[1]]$Df[1]  # Numerator degrees of freedom
DFd <- d[[1]]$Df[2]  # Denominator degrees of freedom

u=qf(0.95,DFn,DFd)
if(d[[1]][1,4] < u){
  cat("\nReject Ho\nThere are significant differences among the away_score means for different tournament types")}else{
  cat("\nAccept Ho\nNo significant differences among the away_score means for different tournament types")}

#Plots

# Create a facet plot for comparison
ggplot(combined_scores, aes(x = Score, fill = Type)) +
  geom_histogram(binwidth = 1, color = "black") +
  labs(title = "Comparison of Home and Away Scores",
       x = "Score",
       y = "Frequency") +
  scale_fill_manual(values = c("blue", "red")) +
  facet_wrap(~Type, scales = "free_y") +
  theme_minimal()

data$date <- as.Date(data$date)
matches_per_year <- data %>% group_by(year = as.numeric(format(date, "%Y"))) %>% summarize(count = n())
ggplot(matches_per_year, aes(x = year, y = count)) +
  geom_line(color = "purple") +
  labs(title = "Number of Matches Over the Years", x = "Year", y = "Number of Matches")+xlim(1870,2025)
cat("\nThe deep curve in the number of mathches in the later years is due to the COVID - 19 Pandemic")

data$date <- as.Date(data$date)
data$decade <- as.integer(substr(format(data$date, "%Y"), 1, 3)) * 10

# Group data by decade and calculate the mean home_score and away_score
decade_means <- data %>%
  group_by(decade) %>%
  summarise(
    mean_home_score = mean(home_score),
    mean_away_score = mean(away_score)
  )

# line plot to compare mean scores for each decade
ggplot(decade_means, aes(x = decade, y = mean_home_score, group = 1)) +
  geom_line(color = "blue", linetype = "solid", linewidth = 1) +
  geom_line(aes(y = mean_away_score), color = "red", linetype = "solid", size = 1) +
  labs(
    title = "Mean Home and Away Scores by Decade",
    x = "Decade",
    y = "Mean Score"
  ) +
  theme_minimal()


# Group data by decade and calculate the median home_score and away_score
decade_medians <- data %>%
  group_by(decade) %>%
  summarise(
    median_home_score = median(home_score),
    median_away_score = median(away_score)
  )

# line plot to compare median scores for each decade
ggplot(decade_medians, aes(x = decade, y = median_home_score, group = 1)) +
  geom_line(color = "blue", linetype = "solid", linewidth = 1) +
  geom_line(aes(y = median_away_score), color = "red", linetype = "solid", size = 1) +
  labs(
    title = "Median Home and Away Scores by Decade",
    x = "Decade",
    y = "Median Score"
  ) +
  theme_minimal()

