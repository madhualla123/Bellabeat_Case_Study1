## Bellabeat_Case_Study
 ### Introduction
Welcome to the Bellabeat data analysis case study! In this case study, you will perform many real-world tasks of a junior data
analyst. You will imagine you are working for Bellabeat, a high-tech manufacturer of health-focused products for women, and
meet different characters and team members. 
### Step 1: Ask
#### Business task 
Collect and analyse usage data for both Bellabeat smart devices to better inform Bellabeat’s marketing and business strategy decisions.
#### Key stakeholders
• Urška Sršen - Chief Executive Officer (CEO) and co-founder of Bellabeat
• Sando Mur - Mathematician and co-founder of Bellabeat
• Bellabeat Marketing Analytics Team
#### Questions to explore:
  What are some trends in smart device usage?
  How could these trends apply to Bellabeat customers?
  How could these trends help influence Bellabeat marketing strategy?
###  Step 2: Prepare
### Dataset
*  The dataset used in this case study is from Kaggle:
    + Fitbit Fitness Tracker Data, a CC0: Public Domain, dataset that is available through Mobius.This Kaggle data set contains personal fitness tracker from thirty fitbit users.
   + Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring.
   + It includes information about daily activity, steps, and heart rate that can be used to explore users’ habits.
* This dataset is downloaded and uploaded to RStudio. It was then analyse using the ROCC approach:
   + Reliability: LOW. Dataset is collected from 30 Fitbit users but their gender is unknown.
  + Originality: LOW. Third party collected the data; Amazon Mechanical Turk.
  + Comprehensive: MEDIUM. It contains a number of information such as sleep activity, 
   daily activity and their weight. However, it does not contain information on one’s 
   gender, age and their health status.
  + Current: LOW. This dataset was collected in 2016 and it is considered old as 
   people’s fitness level may change over time.
  + Cited: LOW. It does state that the dataset was from Amazon Mechanical Turk; 
     however,there is no link or information regarding this.


## Install and load packages
 * install.packages(c("tidyverse", "lubridate", "tidyr", "skimr", "janitor", "dplyr", "ggplot2"))
  * apply(c("tidyverse", "lmtest", "skimr", "janitor", "lubridate", "tidyr", "dplyr", "ggplot2"), require, character.only = TRUE)
### Step 3: Process


### Importing datasets
```{r}
dailyActivity_merged <- read_csv("dailyActivity_merged.csv")
sleepDay_merged <- read_csv("sleepDay_merged.csv")
weightLogInfo_merged <- read_csv("weightLogInfo_merged.csv")
```

* Clean and process data
 * It seems that the datatype of ActivityDate in both activity and calories dataframe is character instead of Datetime. So we need to change it. Same goes to the sleep and weight dataframe where the “Date” and “SleepDay” has both date and time and the datatype is character as well.
```{r}
activity$ActivityDate<-as.Date(activity$ActivityDate, format="%m/%d/%Y")
activity$Date<- as.Date(activity$ActivityDate, format="%m/%d/%Y")
weight$Date<-as.POSIXct(weight$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight$Date_2 <- as.Date(weight$Date, format = "%m/%d/%Y")
weight$Time <- format(as.POSIXct(weight$Date), format = "%H:%M:%S")
weight$Month <- format(weight$Date, format = "%B")
sleep$Date<-as.Date(sleep$SleepDay, format="%m/%d/%Y")
sleep$SleepDay<-as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
```
* Clean the columns
```{r}  
  activity<-clean_names(activity)
  sleep <- clean_names(sleep)
  weight <- clean_names(weight) %>%
  na.omit(weight)
 ```
* Check again to see if there are any null values left
  ```{r}
  apply(weight, 2, function(x) any(is.na(x)))
  ```
### Step 4: Analyse

* n_distinct(activity$id)
  ```{r}
  > [1] 33
 ```
 * n_distinct(sleep$id)
```{r}
> [1] 24
```
* n_distinct(weight$id)
  ```{r}
  > [1] 2
  ```
* There are 33 participants in the activity dataset, 24 participants in the sleep datasets and 2 participants in the weight dataset. Only 2 participants in the weight dataset; therefore, more data is required in order to make any recommendations or conclusions that is concrete and this dataset will not be used for further analysis.

### activity
```{r}
activity %>%
  select(total_steps, total_distance, sedentary_minutes, calories) %>%
summary()
'''
### explore active distance per minutes in activity
```{r}
activity %>%
  select(very_active_minutes, fairly_active_minutes, lightly_active_minutes) %>%
summary()
```
# sleep
```{r}
sleep %>%
  select(total_sleep_records, total_minutes_asleep, total_time_in_bed) %>%
summary()
```
# weight
```{r}
weight %>%
  select(weight_kg, bmi) %>%
summary()
```
* Majority of the participants fall under the lightly active group.

* The average number of steps per day is 7638. According to CDC, the recommended steps per day is 10,000.

* On average, people sleep for 7 hours.

### Merge data
  ```{r}
  activity_sleep <- merge(activity, sleep, by= c("id", "date"))
   head(activity_sleep)
```
### Step 5: Share
   * Total Steps vs Calories
```{r}
   ggplot(data = activity) + aes(x = total_steps, y=calories) + labs(title = "Total 
   Steps vs Calories") + geom_point(color = "green")
```
 * Time Asleep vs Time in Bed
  ```{r}
    ggplot(data = sleep) + aes(x = total_time_in_bed, y=total_minutes_asleep) + 
     labs(title = "Time Asleep vs Time in Bed") + geom_point(color = "red") 
  ```
   * Sleep Duration vs Sedentary Time
     ```{r}
        ggplot(data = activity_sleep) + aes(x = sedentary_minutes, 
      y=total_minutes_asleep) + labs(title = "Sleep Duration vs Sedentary Time") + 
      geom_point(color = "purple")
     
     ```
 *  Participants More Active
    ```{r}
      activity_sleep <- mutate(activity_sleep, 
                    day = wday(date, label = TRUE))

     summarised_activity_sleep <- activity_sleep %>%
          group_by(day) %>%
         summarise(AvgDailySteps = mean(total_steps),
            AvgAsleepMinutes = mean(total_minutes_asleep),
            AvgAwakeTimeInBed = mean(total_time_in_bed), 
            AvgSedentaryMinutes = mean(sedentary_minutes),
            AvgLightlyActiveMinutes = mean(lightly_active_minutes),
            AvgFairlyActiveMinutes = mean(fairly_active_minutes),
            AvgVeryActiveMinutes = mean(very_active_minutes), 
            AvgCalories = mean(calories))
                                    
            head(summarised_activity_sleep)
 ```

  * Daily Sleep Duration
 ```{r}
   ggplot(data = summarised_activity_sleep) + aes(x = day, y = AvgAsleepMinutes, fill 
   = day) + geom_bar(stat = "identity") + labs(title = "Daily Sleep Duration")
  ``` 
   + Most people sleep on Sundays.
   * Daily Steps Taken
 ```{r}
    ggplot(data = summarised_activity_sleep) + aes(x = day, y = AvgDailySteps, fill = 
    day) + geom_bar(stat = "identity") + labs(title = "Daily Steps Taken")
  ```  
### Recommendations
 * As the majority of users are under the lightly active category, Bellabeat can have 
   a progression system where users have to reach a certain number of steps or certain 
  number of workouts in order to collect points. Perhaps when Bellabeat has the funds 
  or capacity to allow users to change the points collected into either gift cards, 
  exercise classes or goods. This progression system can not only motivate users but b 
   also encourage them to be more active.

  * We see that when people are less active, they tend to have lesser sleep. 
    Therefore, to improve one’s quality sleep, a feature that can be implemented into 
     the app is that users can enter their desired sleep time and awake time; then the 
     app will calculate the hours and recommend a sleep time that is suitable 
      depending on their sleep schedule. An app notification can also be sent out 
      whenever no activity has been detected for a period of time that is considered 
       sedentary.

 














  





