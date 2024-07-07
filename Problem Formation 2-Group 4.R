#for loading data
library(readr)
library(here)
#skimming data
library(skimr)
#data manipulation and analysis
library(dplyr)
library(ggplot2)
library(lubridate)
library(cluster)
library(fpp3)
library(tsibble)

# Adjust the `filename` and use the below to load the data into R.
fileData       <- paste0( here(), "bags-medical-devices-company-commission-detail-tentor.csv")
BagsCommissionDetail_ <- read_csv( fileData
                                   , col_types = cols( ContractId          = col_integer( )
                                                       , ContractYear        = col_integer( )
                                                       , ContractDate        = col_date   (format = "%m/%d/%Y")
                                                       , PrimaryCommission   = col_double ( )
                                                       , SecondaryCommission = col_double ( )
                                                       , TotalCommission     = col_double ( )
                                   )
)

skim(BagsCommissionDetail_)

#Modify the columns to include the commission calculations
BagsCommissionDetail_ <- BagsCommissionDetail_ %>%
  mutate(
    PrimaryCommission = ContractAmount * 0.1,
    SecondaryCommission = if_else(
      SecondarySalesRep != "No secondary sales rep", 
      ContractAmount * 0.05, 
      0
    ),
    TotalCommission = PrimaryCommission + SecondaryCommission,
    Year = year(ContractDate)
  )

head(BagsCommissionDetail_)

#Summarize the total commission by year
total_commission_by_year <- BagsCommissionDetail_ %>%
  group_by(Year) %>%
  summarise(TotalCommission = sum(TotalCommission, na.rm = TRUE))

#View the summarized data
print(total_commission_by_year)

#Commissions by year add up to what the income statement shows.

#Summarize the total commissions by year
total_commissions_by_year <- BagsCommissionDetail_ %>%
  group_by(Year) %>%
  summarise(
    TotalPrimaryCommission = sum(PrimaryCommission, na.rm = TRUE),
    TotalSecondaryCommission = sum(SecondaryCommission, na.rm = TRUE),
    TotalCommission = sum(TotalCommission, na.rm = TRUE)
  )

#Calculate the percentage of secondary commission relative to the total commission for each year
total_commissions_by_year <- total_commissions_by_year %>%
  mutate(
    SecondaryCommissionPercentage = (TotalSecondaryCommission / TotalCommission) * 100
  )

#View the summarized data with percentages
print(total_commissions_by_year)

#Convert to a tsibble
df_tsibble <- BagsCommissionDetail_ %>%
  mutate(ContractDate = as.Date(ContractDate)) %>%
  as_tsibble(index = ContractDate, key = ContractId)

#Aggregate by month and PrimarySalesRep
monthly_data <- df_tsibble %>%
  group_by(PrimarySalesRep) %>% 
  index_by(YearMonth = yearmonth(ContractDate)) %>%
  summarise(TotalCommissionAmount = sum(PrimaryCommission, na.rm = TRUE))

#Convert YearMonth to Date for plotting
monthly_data <- monthly_data %>%
  mutate(YearMonth = as.Date(YearMonth))

#Plot the data with x-axis starting at 2008
monthly_data %>%
  ggplot(aes(x = YearMonth, y = TotalCommissionAmount, color = PrimarySalesRep)) +
  geom_line() +
  labs(title = "Total Commission Amount per Month by Primary Sales Rep",
       x = "Month",
       y = "Total Primary Commission Amount") +
  theme_minimal() +
  scale_x_date(limits = as.Date(c("2008-01-01", NA)), date_labels = "%Y-%m")


#Note there are two tests... Test Sales A and Test Sales B.
BagsCommissionDetail_ <- BagsCommissionDetail_ %>%
  filter(!(PrimarySalesRep %in% c("Test Sales A", "Test Sales B")))

#Aggregate by month and Secondary SalesRep
monthly_data_sec <- df_tsibble %>%
  group_by(SecondarySalesRep) %>% 
  index_by(YearMonth = yearmonth(ContractDate)) %>%
  summarise(TotalCommissionAmount = sum(SecondaryCommission, na.rm = TRUE))

#Convert YearMonth to Date for plotting
monthly_data_sec <- monthly_data_sec %>%
  mutate(YearMonth = as.Date(YearMonth))

#Plot the data with x-axis starting at 2008
monthly_data_sec %>%
  ggplot(aes(x = YearMonth, y = TotalCommissionAmount, color = SecondarySalesRep)) +
  geom_line() +
  labs(title = "Total Commission Amount per Month by Secondary Sales Rep",
       x = "Month",
       y = "Total Secondary Commission Amount") +
  theme_minimal() +
  scale_x_date(limits = as.Date(c("2008-01-01", NA)), date_labels = "%Y-%m")

#Willie Johnson appears to be well above the rest of the pack in secondary commission. Might require more looking into.

# Running a frequency table of pairs of primary sales reps with secondary sales reps
# Create the frequency table
freq_table <- BagsCommissionDetail_ %>%
  count(PrimarySalesRep, SecondarySalesRep, name = "Frequency") %>%
  arrange(desc(Frequency))

# View the table
print(freq_table)

# Visualize
freq_table %>%
  ggplot(aes(x = PrimarySalesRep, y = SecondarySalesRep, fill = Frequency)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Frequency of Primary and Secondary Sales Reps Working Together",
       x = "Primary Sales Rep",
       y = "Secondary Sales Rep",
       fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#The one that significantly stands out is Mitchel Bogart as the primary and Amanda as the secondary
#We need to take a closer look at their relationship over time
#Filter for the relevant sales reps
filtered_bags <- df_tsibble %>%
  filter(PrimarySalesRep == "Mitchel Bogart" & SecondarySalesRep == "Amanda Smith")

#Aggregate data by month
monthly_freq_MB_AS <- filtered_bags %>%
  index_by(YearMonth = yearmonth(ContractDate)) %>%
  summarise(Frequency = n())

# Plot the frequency over time
monthly_freq_MB_AS %>%
  ggplot(aes(x = YearMonth, y = Frequency)) +
  geom_line(color = "blue") +
  labs(title = "Mitch Bogart as Primary and Amanda Smith as Secondary Over Time",
       x = "Month",
       y = "Frequency") +
  theme_minimal()

#When you look at the monthly_freq_MB_AS table, you see a steady one per month until June 2010. Given that Amanda has felt her job is in jeopardy for the last two years it feels awfully suspicious that Mitchell randomly starts using Amanda as his secondary at a significantly higher rate two years ago. 

# Filter data for Mitch Bogart as the primary sales rep
filtered_MB <- df_tsibble %>%
  filter(PrimarySalesRep == "Mitchel Bogart")

# Aggregate data by month and secondary sales rep
monthly_freq_MB <- filtered_MB %>%
  group_by(SecondarySalesRep) %>%
  index_by(YearMonth = yearmonth(ContractDate)) %>%
  summarise(Frequency = n())

# Plot the frequency over time
monthly_freq_MB %>%
  ggplot(aes(x = YearMonth, y = Frequency, color = SecondarySalesRep)) +
  geom_line() +
  labs(title = "Frequency of Secondary Sales Reps Used by Mitchel Bogart Over Time",
       x = "Month",
       y = "Frequency") +
  theme_minimal()

#This plot paints a much clearer picture that Mitchel has made a significant transition from no secondary sales rep to Amanda.
#Perform a two sample t test to statistically prove the significance of the shift.

#Convert YearMonth to Date for easier manipulation
monthly_freq_MB_AS <- monthly_freq_MB_AS %>%
  mutate(YearMonth = as.Date(YearMonth))

#Split data into before and after March 2010
before_2010 <- monthly_freq_MB_AS %>%
  filter(YearMonth < as.Date("2010-03-01"))

after_2010 <- monthly_freq_MB_AS %>%
  filter(YearMonth >= as.Date("2010-03-01"))

#Aggregate the frequencies by month for before and after 2010
before_2010_freq <- before_2010 %>%
  index_by(YearMonth) %>%
  summarise(TotalFrequency = sum(Frequency))

after_2010_freq <- after_2010 %>%
  index_by(YearMonth) %>%
  summarise(TotalFrequency = sum(Frequency))

#Perform the two-sample t-test
t_test_result <- t.test(before_2010_freq$TotalFrequency, after_2010_freq$TotalFrequency)

#Display the t-test result
print(t_test_result)

#The p-value of 0.003349 is well below 0.05 significance level. We reject the the null hypothesis, or in other words there is a statistically significant difference in Mitchel Bogarts use of Amanda Smith as a secondary sales rep beginning in March 2010. Given Amanda's long time relationship with Barry it would be easy for him to sign off on the terms of the contract. 

###From ChatGPT###

# ChatGPT
# The updated results of the Welch Two Sample t-test show a statistically significant difference in the mean frequency of secondary sales reps used by Mitchel Bogart before and after March 2010. Here's how you can interpret these results:
# 
# Interpretation
# t-value: -3.8245
# Degrees of freedom (df): 10
# p-value: 0.003349
# Confidence interval (95%): [-2.014212, -0.531243]
# Mean of x (before 2010): 1.000000
# Mean of y (after 2010): 2.272727
# Detailed Interpretation
# p-value: The p-value of 0.003349 is much less than the common significance level of 0.05. This means that we reject the null hypothesis, indicating that there is a statistically significant difference between the mean frequencies before and after March 2010.
# Confidence interval: The 95% confidence interval for the difference in means is [-2.014212, -0.531243]. Since this interval does not include 0 and is entirely negative, it indicates a significant decrease in the mean frequency of secondary sales reps used by Mitchel Bogart before 2010 compared to after 2010.
# Means: The mean frequency of secondary sales reps before 2010 is 1.000000, and the mean frequency after 2010 is 2.272727. The higher mean frequency after 2010 suggests that Mitchel Bogart used secondary sales reps more frequently after March 2010.


#Back to Willie
#Filter data for Willie as the secondary sales rep
filtered_WJ <- df_tsibble %>%
  filter(SecondarySalesRep == "Willie Johnson")

#Aggregate data by month and primary sales rep
monthly_freq_WJ <- filtered_WJ %>%
  group_by(PrimarySalesRep) %>%
  index_by(YearMonth = yearmonth(ContractDate)) %>%
  summarise(Frequency = n())

#Plot the frequency over time
monthly_freq_WJ %>%
  ggplot(aes(x = YearMonth, y = Frequency, color = PrimarySalesRep)) +
  geom_line() +
  labs(title = "Frequency of Primary Sales Reps Used by Willie Johnson Over Time",
       x = "Month",
       y = "Frequency") +
  theme_minimal()
#When you look at Willie's frequency chart of primary's used, there does not appear to be any fraud. Still worth investigating further with the sales people why he is used on the big $ contracts as a secondary.

