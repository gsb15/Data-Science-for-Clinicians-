#Data Visualizations


#Download and install packages to be used. 

install.packages('dplyr')

install.packages('ggplot2')


#Attaching Libraries 
library(dplyr)
library(ggplot2)


######Getting the Running Through Data###########

#Save the data set to your computer R Console, directly from Github.
running_reliability <- read.csv("https://raw.githubusercontent.com/gsb15/Running-Through-Reliability-Data/main/RunningThrough_Reliability_Data.csv")

View(running_reliability)
names(running_reliability)



#############################################
#Basic Data Visualization of Single Variables
#############################################

#First we want to assess the distributions of different variables

hist(running_reliability$distance_kilometers)

hist(running_reliability$duration_minutes)

hist(running_reliability$averageSpeedInMetersPerSecond)

#The first two histograms we see a right skew, which makes sense as more people will
#running less distance compared to a few who will run much longer distance 

#The third histogram is more inline with a 'bell' curve. 



#################################################
#Bi-variable Visualizations 
#################################################

#Next we want to visualize bivariable (i.e., two variables) in a scatter plot format 


ggplot(running_reliability, 
       aes(distance_kilometers, duration_minutes)) + 
geom_point()

#So we see a good general vector, of increased distancer ran is in association to increased time 
#However, this could look better, let's improve the visual neatness of this graph


ggplot(running_reliability, 
       aes(distance_kilometers, duration_minutes)) + 
  geom_point() + 
  xlab('Distance Ran Per Week (Km)') + 
  ylab('Distance Ran Per Week (Minutes)') + 
  theme_minimal()

#Looking better!
#But we can go further, to make this more discernible, 
#Let's create better visualizations of the points themselves for better readability 

ggplot(running_reliability, 
       aes(distance_kilometers, duration_minutes)) + 
  geom_point(alpha = 0.5, color = 'Blue') + # 50% opaque, blue
  xlab('Distance Ran Per Week (Km)') + 
  ylab('Distance Ran Per Week (Minutes)') + 
  theme_minimal()


#Finally, let's add a title

ggplot(running_reliability, 
       aes(distance_kilometers, duration_minutes)) + 
  geom_point(alpha = 0.5, color = 'Blue') + # 50% opaque, blue
  xlab('Distance Ran Per Week (Km)') + 
  ylab('Distance Ran Per Week (Minutes)') + 
  ggtitle("Association of Distance Ran and Time Weekly") +
  theme_minimal()


#######################################################
#Adding A Comparison To Your Bi-variable Plot 
#######################################################

#So you now know how to create a bi-variable scatter plot.
#But what if you want to compare different groups and their associations?

#Let us know compare two groups, using two distinct visualization methods 


#First let's compare within the same graph 

#This time we will look at the association between running speed and heart rate

ggplot(running_reliability, 
       aes(averageSpeedInMetersPerSecond, averageHeartRateInBeatsPerMinute, 
           colour = as.factor(age_strata))) + 
  geom_point(alpha = 0.5) + # 50% opaque
  xlab('Average Running Speed (m/s)') + 
  ylab('Average Heart Rate (BPM)') + 
  ggtitle("Association of Running Speed & Heart Rate") +
  theme_minimal()

#This is definitely messier!

#Let's add information to the new legend

ggplot(running_reliability, 
       aes(averageSpeedInMetersPerSecond, averageHeartRateInBeatsPerMinute, 
           colour = as.factor(age_strata))) + 
  geom_point(alpha = 0.5) + # 50% opaque
  xlab('Average Running Speed (m/s)') + 
  ylab('Average Heart Rate (BPM)') + 
  ggtitle("Association of Running Speed & Heart Rate") +
  labs(color = "Age") +
  theme_minimal()


#And now what each different age strata means 

ggplot(running_reliability, 
       aes(averageSpeedInMetersPerSecond, averageHeartRateInBeatsPerMinute, 
           colour = as.factor(age_strata))) + 
  geom_point(alpha = 0.5) + # 50% opaque
  xlab('Average Running Speed (m/s)') + 
  ylab('Average Heart Rate (BPM)') + 
  ggtitle("Association of Running Speed & Heart Rate") +
  labs(color = "Age") +
  scale_color_discrete(labels = c("18-30 years", "31-50 years", "51+ years")) +
  theme_minimal()


#This is still pretty messy,
#and hard to discern what's going on in where most of the data point are
#What if we made these separate figures side by side?

#We start with the same figure as before.


ggplot(running_reliability, 
       aes(x = averageSpeedInMetersPerSecond, 
           y = averageHeartRateInBeatsPerMinute, 
             color = as.factor(age_strata))) + 
  geom_point(alpha = 0.5) + 
  xlab('Average Running Speed (m/s)') + 
  ylab('Average Heart Rate (BPM)') + 
  ggtitle("Association of Running Speed & Heart Rate") +
  facet_wrap(~as.factor(age_strata)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.position = "none")


#However, we don't know what 1,2,3 means here. 
#Need to relabel 


# Relabel the factor levels
running_reliability$age_strata <- factor(running_reliability$age_strata,
                                         levels = c(1, 2, 3),
                                         labels = c("18–30 years", "31–50 years", "51+ years"))

  
ggplot(running_reliability, 
       aes(x = averageSpeedInMetersPerSecond, 
           y = averageHeartRateInBeatsPerMinute, 
           color = as.factor(age_strata))) + 
  geom_point(alpha = 0.5) + 
  xlab('Average Running Speed (m/s)') + 
  ylab('Average Heart Rate (BPM)') + 
  ggtitle("Association of Running Speed & Heart Rate") +
  facet_wrap(~as.factor(age_strata)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.position = "none")

#Looks good!


###################################
#Violin Plots
###################################

#As we saw from the lecture, there are multiple ways to visualize data.
#Let's perform this data visualization through violin plots. 
#While we cannot perform an X/Y association like we can in a scatter plot.
#We can assess the spread and density of the data through violin plots. 


#Heart Rate
ggplot(running_reliability, 
       aes(x = as.factor(age_strata), 
           y = averageHeartRateInBeatsPerMinute, 
           fill = as.factor(age_strata))) + #Fill here allows for the full colorto be filled
  geom_violin(trim = FALSE, alpha = 0.6) +   # violin
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  
  xlab('Age Group') + 
  ylab('Average Heart Rate (BPM)') + 
  ggtitle("Data Spread of Heart Rate by Each Age Group") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.position = "none")



#Now Let's Make It for Running Speed
ggplot(running_reliability, 
       aes(x = as.factor(age_strata), #Notice Group is now at the 'x' position
           y = averageSpeedInMetersPerSecond, 
           fill = as.factor(age_strata))) + #Fill here allows for the full color to be filled
  geom_violin(trim = FALSE, alpha = 0.6) +   # violin
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  #Box & whisker plot
  xlab('Age Group') + 
  ylab('Average Running Speed (m/s)') + 
  ggtitle("Data Spread of Average Running Speed by Each Age Group") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.position = "none")

#Notice the large outliers for 31-50 years!

###############################################
#Visualizing a Regression Line on Top of Data 
###############################################

#Finally we want to  assess the association of running speed on heart rate. 
#We want to visualize this regression on top of our data

ggplot(running_reliability, 
       aes(x = averageSpeedInMetersPerSecond, 
           y = averageHeartRateInBeatsPerMinute)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = T) +  # Add regression line with confidence interval
  xlab('Average Running Speed (m/s)') + 
  ylab('Average Heart Rate (BPM)') + 
  ggtitle("Association of Running Speed & Heart Rate") +
  theme_minimal() 

#This one is OK, but we know that different age strata demonstrated different associations

#Adding a Separate Regression Line per Strata
ggplot(running_reliability, 
      aes(x = averageSpeedInMetersPerSecond, 
          y = averageHeartRateInBeatsPerMinute,
          color = as.factor(age_strata))) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = T) +  # Add regression line with confidence interval
  xlab('Average Running Speed (m/s)') + 
  ylab('Average Heart Rate (BPM)') + 
  ggtitle("Association of Running Speed & Heart Rate") +
  facet_wrap(~as.factor(age_strata)) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.position = "none")

  labs(color = "Age") +
  scale_color_discrete(labels = c("18-30 years", "31-50 years", "51+ years")) +
  theme_minimal()



