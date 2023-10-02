# Load the required libraries
library(dplyr)
library("ggplot2")
library(lubridate)
library("ggpubr")
library(scales) # Load the scales package for formatting labels


#---------------------------------------------------------------------------------------

#1. Read the CSV file
df_India <- read.csv("C:/Users/varsh/OneDrive/Desktop/Covid_R/covid_19_india.csv")
class(df_India)
str(df_India)
colnames(df_India)
summary(df_India)


#2.total cases and max single day by states
df_India%>%
  group_by(State.unionTerritory) %>%
  summarise(cases_sum=sum(Confirmed),cases_max=max(Confirmed))%>%
  arrange(desc(cases_sum))

df_India[df_India$State.unionTerritory=="Bihar****",]





#plot graph OF  state VS mean of confirmed cases

df_India %>%
  group_by(State.unionTerritory) %>%
  summarise(mean_conf = mean(Confirmed)) %>%
  ggplot(aes(x = State.unionTerritory, y = mean_conf, fill = State.unionTerritory)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(n.dodge = 13)) +
  theme_classic() +
  labs(
    x = "State/UnionTerritory",
    y = "Mean of Confirmed Cases",
    title = "Grouped by State/UnionTerritory with Summarise()"
  )

#----------------------------------------------------------------------------------------

#4.identifying the columns with NAs
colnames(df_India)[apply(df_India,2,anyNA)]

unique(df_India$State.unionTerritory)

#5.GETTING ONLY NUMERIC COLUMNS FROM THE DATASET

# Select numeric columns
num <- df_India %>%
  select_if(is.numeric)
# Print the numeric columns
print(num)


#6.PIE CHART FOR CONFIRMED , CURED AND DEATH CASES IN INDIA

# Convert Date to Date format
df_India$Date <- as.Date(df_India$Date, format = "%d/%m/%Y")

# Summarize the total cases
confirm <- sum(df_India$Confirmed)
cured <- sum(df_India$Cured)
deaths <- sum(df_India$Deaths)

# Create a data frame for the pie chart
df_value <- c(confirm, cured, deaths)
df_key <- c("Confirmed", "Cured", "Deaths")

# Create a pie chart
pie_chart <- ggplot(data = data.frame(df_key, df_value), aes(x = "", y = df_value, fill = df_key)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Confirmed" = "lightblue", "Cured" = "orange", "Deaths" = "red")) +
  labs(title = "COVID-19 Cases Distribution",
       fill = "Category",
       x = NULL,
       y = NULL) +
  theme_void() +
  theme(legend.position = "right")

print(pie_chart)


#-------------------------------------------------------------------------------------------------------

#7.TO GET VALUE OF NUMERIC COLUMNS & THE UNIQUE  VALUE IN THOSE COLUMNS

# Select numeric columns
num_cols <- df_India %>%
  select_if(is.numeric) %>%
  colnames()

# Iterate through numeric columns
for (col in num_cols) {
  cat("Column: ", col, "\n")
  cat("Value counts of", col, ": ", sum(!is.na(df_India[[col]])), "\n")
  cat("Number of Unique Values in", col, ": ", n_distinct(df_India[[col]]), "\n\n")
}

#8.total cases and max cases  in particular states
df_India[df_India$State.unionTerritory=="Bihar****",]
df_India[df_India$State.unionTerritory=="Karnataka",]
df_India[df_India$State.unionTerritory=="Maharastra",]
df_India[df_India$State.unionTerritory=="Kerala",]

#9.BAR GRAPH FOR CONFIRMED , CURED , DEATHS


confirm <- sum(df_India$Confirmed)
cured <- sum(df_India$Cured)
deaths <- sum(df_India$Deaths)
active <- sum(df_India$Active)

print(paste("Total Confirmed cases =", confirm))
print(paste("Total Cured cases =", cured))
print(paste("Total Active cases =", active))
print(paste("Total Death cases =", deaths))

barplot <- ggplot(data = data.frame(x = c('Confirmed','Cured','Deaths'), y = c(confirm, cured, deaths)), aes(x = x, y = y)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = comma) +  # Use the comma function to format labels
  labs(x = "", y = "") +
  theme_minimal()

print(barplot)

#-----------------------------------------------------------------------------------------------------------------------------------------
