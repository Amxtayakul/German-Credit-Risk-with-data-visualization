# ---- Loading Necessary Packages ----
library(dplyr) # For Data Manipulation
library(ggplot2) # For Data Representation and Visualiazation
library(vioplot) # Data visualization through violin plot

# ---- Importing German_Credit_Risk Dataset ----
library(readxl)
German_Credit_Risk <- read_excel("C:/Users/josh/downloads/German Credit Risk.xlsx")
View(German_Credit_Risk)
GRC_Data <- German_Credit_Risk
attach(GRC_Data)

# ---- Data Structures ----
vector_age      <- GRC_Data$Age           # vector
vector_credit   <- GRC_Data$Credit_amount # vector
vector_duration <- GRC_Data$Duration      # vector
vector_job      <- GRC_Data$Job           # vector
factor_savings_acc <- factor(GRC_Data$Saving_accounts, ordered = T, levels = c("NA", "little", "moderate", "rich", "quite rich")) # factor
table_sex <- table(Sex) # table 
data_frame <- data.frame(Age = GRC_Data$Age, Housing = GRC_Data$Housing)

factor_savings_acc
table_sex
data_frame


# ---- Univariate Analysis ----
# 1. Compute Statistics (Mean, Median, Mode) (Add Custom Functions)
# Custom function to compute mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 2. Tabulate Results (Dataframe)
# Compute statistics for Age and tabulate into a dataframe
stats_df <- data.frame(
  Variable = c("Age", "Credit_amount", "Duration", "Job"),
  Mean     = c(mean(vector_age,      na.rm = TRUE),
               mean(vector_credit,   na.rm = TRUE),
               mean(vector_duration, na.rm = TRUE),
               mean(vector_job,      na.rm = TRUE)),
  Median   = c(median(vector_age,      na.rm = TRUE),
               median(vector_credit,   na.rm = TRUE),
               median(vector_duration, na.rm = TRUE),
               median(vector_job,      na.rm = TRUE)),
  Mode     = c(get_mode(vector_age),
               get_mode(vector_credit),
               get_mode(vector_duration),
               get_mode(vector_job)),
  SD       = c(sd(vector_age,      na.rm = TRUE),
               sd(vector_credit,   na.rm = TRUE),
               sd(vector_duration, na.rm = TRUE),
               sd(vector_job,      na.rm = TRUE)),
  Range    = c(diff(range(vector_age,      na.rm = TRUE)),
               diff(range(vector_credit,   na.rm = TRUE)),
               diff(range(vector_duration, na.rm = TRUE)),
               diff(range(vector_job,      na.rm = TRUE))),
  Variance = c(var(vector_age,      na.rm = TRUE),
               var(vector_credit,   na.rm = TRUE),
               var(vector_duration, na.rm = TRUE),
               var(vector_job,      na.rm = TRUE))
)
print(stats_df)

# ---- Data Visualization with Base Graphics ----

#Set up a 2x2 plotting grid so all four univariate plots display together
par(mfrow = c(2, 2))

#Histogram of Age
#shows the frequency distribution of applicant ages;
#helps identify the most common age range in the dataset.
hist(
  vector_age,
  main   = "Distribution of Age",
  xlab   = "Age (years)",
  ylab   = "Frequency",
  col    = "steelblue",
  border = "white",
  breaks = 15
)

#Histogram of Credit Amount
#reveals how loan sizes are distributed across applicants;
#a right skew would indicate most loans are small with a few very large ones.
hist(
  vector_credit,
  main   = "Distribution of Credit Amount",
  xlab   = "Credit Amount (DM)",
  ylab   = "Frequency",
  col    = "tomato",
  border = "white",
  breaks = 20
)

#Vioplot of Duration
#displays the spread and typical loan duration in months,
#highlighting any outliers on the longer end.
vioplot(
  vector_duration,
  main   = "Violin Plot of Loan Duration",
  ylab   = "Duration (months)",
  col    = "mediumseagreen",
  border = "gray30",
  names  = "Duration"
)

#bar chart of Job category counts
#shows how many applicants fall into each job skill category (0-3),
#giving a quick picture of the workforce composition in the dataset.
job_counts <- table(vector_job)
barplot(
  job_counts,
  main   = "Applicant Count by Job Category",
  xlab   = "Job Category (0 = unskilled, 3 = highly skilled)",
  ylab   = "Count",
  col    = c("orchid", "goldenrod", "cornflowerblue", "salmon"),
  border = "white"
)

#Reset plotting layout back to a single panel after the four charts
par(mfrow = c(1, 1))



# ---- Data Visualization with ggplot2 ----

#Density plot of Age
#a smoothed alternative to the histogram that reveal the underlying
#distribution shape (e.g.- slight right skew) more clearly.
plot_age_density <- ggplot(GRC_Data, aes(x = Age)) +
  geom_density(fill = "steelblue", alpha = 0.6, color = "steelblue4") +
  geom_vline(aes(xintercept = mean(Age, na.rm = TRUE)),
             color = "firebrick", linetype = "dashed", linewidth = 0.8 ) +
  labs(
    title    = "Density Distribution of Applicant Age",
    subtitle = "Dashed line marks the mean age",
    x        = "Age (years)",
    y        = "Density"
  ) +
  theme_minimal(base_size = 13)

print(plot_age_density)

#Histogram of Credit Amount with a normal-curve overlay
#combines a frequency histogram with a fitted normal curve to show
#how far the credit-amount distribution deviates from normality.
plot_credit_hist <- ggplot(GRC_Data, aes(x = Credit_amount)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 25, fill = "tomato", color = "white", alpha = 0.8 ) +
  stat_function(
    fun  = dnorm,
    args = list(mean = mean(GRC_Data$Credit_amount, na.rm = TRUE),
                sd   = sd(GRC_Data$Credit_amount,   na.rm = TRUE)),
    color = "navy", linewidth = 1
  ) +
  labs(
    title    = "Distribution of Credit Amount with Normal Curve",
    subtitle = "Blue curve = fitted normal distribution",
    x        = "Credit Amount (DM)",
    y        = "Density"
  ) +
  theme_minimal(base_size = 13)

print(plot_credit_hist)

#Vioplot of Loan Duration
#Highlights the median, interquartile range, and any outlier loans
#that run significantly longer than typical.
plot_duration_violin <- ggplot(GRC_Data, aes(x = "", y = Duration)) +
  geom_violin(fill = "mediumseagreen", color = "gray30", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", color = "gray30",
               outlier.color = "firebrick", outlier.shape = 16) +
  labs(
    title = "Violin Plot of Loan Duration",
    y     = "Duration (months)",
    x     = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

print(plot_duration_violin)
#Bar chart of Job category
#displays the count of applicants per job-skill level using distinct
#fill colours; category 2 (skilled employee) is expected to dominate.
plot_job_bar <- ggplot(GRC_Data, aes(x = factor(Job), fill = factor(Job))) +
  geom_bar(color = "white", show.legend = FALSE) +
  scale_fill_manual(values = c("orchid3", "goldenrod2",
                               "cornflowerblue", "salmon3")) +
  scale_x_discrete(labels = c("0\nUnskilled\n(non-resident)",
                              "1\nUnskilled\n(resident)",
                              "2\nSkilled",
                              "3\nHighly\nSkilled")) +
  labs(
    title = "Applicant Count by Job Category",
    x     = "Job Category",
    y     = "Count"
  ) +
  theme_minimal(base_size = 13)

print(plot_job_bar)

# ---- Bivariate Analysis ----
# 1. Group Data

# Groups data by Sex and computes the mean Age,
# mean Credit Amount, and total count per group.
# This shows how credit behavior differs between male and female applicants.
group_sex <- GRC_Data %>%
  group_by(Sex) %>%
  summarise(
    Mean_Age    = mean(Age,           na.rm = TRUE),
    Mean_Credit = mean(Credit_amount, na.rm = TRUE),
    Count       = n()
  )
group_sex

# Groups data by Housing status and computes the mean Age,
# mean Credit Amount, and total count per group.
# This shows how housing type relates to credit amount requested.
group_housing <- GRC_Data %>%
  group_by(Housing) %>%
  summarise(
    Mean_Age    = mean(Age,           na.rm = TRUE),
    Mean_Credit = mean(Credit_amount, na.rm = TRUE),
    Count       = n()
  )
group_housing

# Groups data by Risk (good/bad) and computes the mean Credit Amount,
# mean Duration, and total count per group.
# This reveals how loan size and duration differ between
# good and bad credit risk applicants.
group_risk <- GRC_Data %>%
  group_by(Risk) %>%
  summarise(
    Mean_Credit   = mean(Credit_amount, na.rm = TRUE),
    Mean_Duration = mean(Duration,      na.rm = TRUE),
    Count         = n()
  )
group_risk

# Groups data by Purpose of loan and computes the mean Credit Amount,
# mean Duration, and total count per group.
# This shows which loan purposes tend to have higher amounts
# and longer repayment periods.
group_purpose <- GRC_Data %>%
  group_by(Purpose) %>%
  summarise(
    Mean_Credit   = mean(Credit_amount, na.rm = TRUE),
    Mean_Duration = mean(Duration,      na.rm = TRUE),
    Count         = n()
  )
group_purpose

# Age vs Saving_accounts (Correlation)
# Convert Saving_accounts to numeric for correlation
GRC_Data$Saving_accounts_num <- as.numeric(factor(GRC_Data$Saving_accounts,
                                          levels = c("little", "moderate", 
                                                     "quite rich", "rich")))

cor_age_saving <- cor.test(GRC_Data$Age, GRC_Data$Saving_accounts_num,
                           method = "spearman", use = "complete.obs")
cat("=== Age vs Saving Accounts (Spearman Correlation) ===\n")

# Credit_amount vs Job (Kruskal-Wallis Test)
kw_credit_job <- kruskal.test(Credit_amount ~ factor(Job), data = GRC_Data)
cat("=== Credit Amount vs Job (Kruskal-Wallis Test) ===\n")
print(kw_credit_job)
print(cor_age_saving)

#Duration vs Sex (T-Test)
ttest_duration_sex <- t.test(Duration ~ Sex, data = GRC_Data)
cat("=== Duration vs Sex (Independent T-Test) ===\n")
print(ttest_duration_sex)

#Housing vs checking account (Chi-square)
chisq_risk_checking <- chisq.test(table(GRC_Data$Risk,
                                        GRC_Data$Checking_account))
cat("=== Risk vs Checking Account (Chi-Square Test) ===\n")
print(chisq_risk_checking)

# 2. Tabulate Results
# Combines all four bivariate group summaries into
# individual data frames for clean, readable output.

bivariate_table_sex <- data.frame(
  Sex         = group_sex$Sex,
  Mean_Age    = group_sex$Mean_Age,
  Mean_Credit = group_sex$Mean_Credit,
  Count       = group_sex$Count
)
print(bivariate_table_sex)

bivariate_table_housing <- data.frame(
  Housing     = group_housing$Housing,
  Mean_Age    = group_housing$Mean_Age,
  Mean_Credit = group_housing$Mean_Credit,
  Count       = group_housing$Count
)
print(bivariate_table_housing)

bivariate_table_risk <- data.frame(
  Risk          = group_risk$Risk,
  Mean_Credit   = group_risk$Mean_Credit,
  Mean_Duration = group_risk$Mean_Duration,
  Count         = group_risk$Count
)
print(bivariate_table_risk)

bivariate_table_purpose <- data.frame(
  Purpose       = group_purpose$Purpose,
  Mean_Credit   = group_purpose$Mean_Credit,
  Mean_Duration = group_purpose$Mean_Duration,
  Count         = group_purpose$Count
)
print(bivariate_table_purpose)

#Age vs Saving_accounts
table_age_saving <- GRC_Data %>%
  filter(!is.na(Saving_accounts)) %>%
  group_by(Saving_accounts) %>%
  summarise(Mean_Age = mean(Age, na.rm = TRUE), Count = n()) %>%
  mutate(Saving_accounts = factor(Saving_accounts,
                                  levels = c("little", "moderate",
                                             "quite rich", "rich"))) %>%
  arrange(Saving_accounts)
print(table_age_saving)

# Median Credit_amount vs Job
table_credit_job <- GRC_Data %>%
  group_by(Job) %>%
  summarise(Median_Credit = median(Credit_amount, na.rm = TRUE), Count = n())
print(table_credit_job)

#Duration vs Sex
table_duration_sex <- GRC_Data %>%
  group_by(Sex) %>%
  summarise(
    Mean_Duration   = mean(Duration,   na.rm = TRUE),
    Median_Duration = median(Duration, na.rm = TRUE),
    SD_Duration     = sd(Duration,     na.rm = TRUE),
    Min_Duration    = min(Duration,    na.rm = TRUE),
    Max_Duration    = max(Duration,    na.rm = TRUE),
    Count           = n()
  )
print(table_duration_sex)

#Risk vs Chekcing account
table_risk_checking <- GRC_Data %>%
  filter(!is.na(Checking_account)) %>%
  group_by(Risk, Checking_account) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Checking_account = factor(Checking_account,
                                   levels = c("little", "moderate", "quite rich", "rich")))


# ---- Data Visualization with Base Graphics ----

# Set up a 2x2 grid to display all four bivariate bar charts together
par(mfrow = c(2, 2))

# Bar chart of Mean Credit Amount by Sex
# Compares the average loan size requested by male vs female applicants.
barplot(
  bivariate_table_sex$Mean_Credit,
  names.arg = bivariate_table_sex$Sex,
  main      = "Mean Credit Amount by Sex",
  xlab      = "Sex",
  ylab      = "Mean Credit Amount (DM)",
  col       = c("steelblue", "tomato"),
  border    = "white"
)

# Bar chart of Mean Credit Amount by Housing Status
# Shows whether applicants who own, rent, or live for free
# tend to request different loan sizes.
barplot(
  bivariate_table_housing$Mean_Credit,
  names.arg = bivariate_table_housing$Housing,
  main      = "Mean Credit Amount by Housing",
  xlab      = "Housing Status",
  ylab      = "Mean Credit Amount (DM)",
  col       = c("mediumseagreen", "goldenrod", "orchid"),
  border    = "white"
)

# Bar chart of Mean Credit Amount by Risk
# Highlights whether bad-risk applicants tend to borrow
# more than good-risk applicants on average.
barplot(
  bivariate_table_risk$Mean_Credit,
  names.arg = bivariate_table_risk$Risk,
  main      = "Mean Credit Amount by Risk",
  xlab      = "Credit Risk",
  ylab      = "Mean Credit Amount (DM)",
  col       = c("firebrick", "cornflowerblue"),
  border    = "white"
)

# Bar chart of Mean Credit Amount by Loan Purpose
# Reveals which loan purposes (e.g. business, car) are associated
# with the highest average borrowing amounts.
barplot(
  bivariate_table_purpose$Mean_Credit,
  names.arg = bivariate_table_purpose$Purpose,
  main      = "Mean Credit Amount by Purpose",
  xlab      = "Loan Purpose",
  ylab      = "Mean Credit Amount (DM)",
  col       = "salmon",
  border    = "white",
  las       = 2,   # rotates x-axis labels vertically so they don't overlap
  cex.names = 0.7  # shrinks label text slightly to fit all purpose names
)

#Bar plot of Age vs Saving accounts
#Reveals the correation of Age and their savings account in base graphics 
barplot(
  table_age_saving$Mean_Age,
  names.arg = table_age_saving$Saving_accounts,
  main      = "Mean Age by Saving Account Level",
  xlab      = "Saving Account Level",
  ylab      = "Mean Age (years)",
  col       = c("steelblue", "goldenrod", "mediumseagreen", "tomato"),
  border    = "white"
)

#Boxplot of Credit amount vs Job
#Displays an upward trend to the Job skill level, and the amount of loan they get
boxplot(
  Credit_amount ~ Job,
  data   = GRC_Data,
  main   = "Credit Amount by Job Category",
  xlab   = "Job Category (0 = Unskilled, 3 = Highly Skilled)",
  ylab   = "Credit Amount (DM)",
  col    = c("orchid", "goldenrod", "cornflowerblue", "salmon"),
  border = "gray30"
)

#Vioplot of Duration vs Sex
#displays the diffrence of loan duration between male and female
library(vioplot)
vioplot(
  Duration ~ Sex,
  data   = GRC_Data,
  main   = "Loan Duration by Sex",
  xlab   = "Sex",
  ylab   = "Duration (months)",
  col    = c("tomato", "steelblue")
)

#Stacked bar chart of Risk vs Checking account
risk_checking_matrix <- table(GRC_Data$Risk, GRC_Data$Checking_account)
barplot(
  risk_checking_matrix,
  main   = "Risk by Checking Account Level",
  xlab   = "Checking Account Level",
  ylab   = "Count",
  col    = c("firebrick", "cornflowerblue"),
  legend = rownames(risk_checking_matrix),
  beside = FALSE,
  border = "white"
)

# Reset plotting layout back to a single panel after the four charts
par(mfrow = c(1, 1))


# ---- Data Visualization with ggplot2 ----

# Bar chart of Mean Credit Amount by Sex
# Compares average loan size between male and female applicants
# using labelled bars for easy reading.
plot_biv_sex <- ggplot(bivariate_table_sex,
                       aes(x = Sex, y = Mean_Credit, fill = Sex)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  geom_text(aes(label = round(Mean_Credit, 0)),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("female" = "tomato",
                               "male"   = "steelblue")) +
  labs(
    title    = "Mean Credit Amount by Sex",
    subtitle = "German Credit Risk Dataset",
    x        = "Sex",
    y        = "Mean Credit Amount (DM)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(plot_biv_sex)

# Bar chart of Mean Credit Amount by Housing Status
# Shows how the average loan amount differs across applicants
# who own, rent, or live for free.
plot_biv_housing <- ggplot(bivariate_table_housing,
                           aes(x = Housing, y = Mean_Credit, fill = Housing)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  geom_text(aes(label = round(Mean_Credit, 0)),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("free" = "mediumseagreen",
                               "own"  = "goldenrod",
                               "rent" = "orchid3")) +
  labs(
    title    = "Mean Credit Amount by Housing Status",
    subtitle = "German Credit Risk Dataset",
    x        = "Housing Status",
    y        = "Mean Credit Amount (DM)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(plot_biv_housing)

# Bar chart of Mean Credit Amount by Risk
# Highlights the difference in average loan size between
# good and bad credit risk applicants.
plot_biv_risk <- ggplot(bivariate_table_risk,
                        aes(x = Risk, y = Mean_Credit, fill = Risk)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  geom_text(aes(label = round(Mean_Credit, 0)),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("bad"  = "firebrick",
                               "good" = "cornflowerblue")) +
  labs(
    title    = "Mean Credit Amount by Risk",
    subtitle = "German Credit Risk Dataset",
    x        = "Credit Risk",
    y        = "Mean Credit Amount (DM)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(plot_biv_risk)

# Bar chart of Mean Credit Amount by Loan Purpose
# Reveals which loan categories are associated with the
# highest average borrowing amounts across all applicants.
plot_biv_purpose <- ggplot(bivariate_table_purpose,
                           aes(x = reorder(Purpose, -Mean_Credit),
                               y = Mean_Credit,
                               fill = Purpose)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = round(Mean_Credit, 0)),
            vjust = -0.5, size = 3.2) +
  labs(
    title    = "Mean Credit Amount by Loan Purpose",
    subtitle = "Sorted from highest to lowest average loan",
    x        = "Loan Purpose",
    y        = "Mean Credit Amount (DM)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x     = element_text(angle = 30, hjust = 1)
  )

print(plot_biv_purpose)

#Bar chart of Age vs Saving accounts
#Reveals the correation of Age and their savings account 
#Spearman shows weak positive correlation between age,
#and their Savings account level
plot_age_saving <- ggplot(table_age_saving,
                          aes(x = Saving_accounts, y = Mean_Age,
                              fill = Saving_accounts)) +
  geom_bar(stat = "identity", color = "white", width = 0.5) +
  geom_text(aes(label = round(Mean_Age, 1)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("little"     = "steelblue",
                               "moderate"   = "goldenrod",
                               "quite rich" = "mediumseagreen",
                               "rich"       = "tomato")) +
  labs(
    title    = "Mean Age by Saving Account Level",
    subtitle = paste("Spearman r =", round(cor_age_saving$estimate, 3),
                     "| p-value =", round(cor_age_saving$p.value, 4)),
    x        = "Saving Account Level",
    y        = "Mean Age (years)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(plot_age_saving)

#boxplot of Credit amount vs Job
#Displays a clear upward trend on job skill level to the credit amount
#The more skilled the individual, the more likely they are to loan more
plot_credit_job <- ggplot(GRC_Data, aes(x = factor(Job), y = Credit_amount,
                                        fill = factor(Job))) +
  geom_boxplot(color = "gray30", outlier.color = "firebrick",
               outlier.shape = 16) +
  scale_fill_manual(values = c("0" = "orchid3", "1" = "goldenrod2",
                               "2" = "cornflowerblue", "3" = "salmon3")) +
  scale_x_discrete(labels = c("0\nUnskilled\n(non-resident)",
                              "1\nUnskilled\n(resident)",
                              "2\nSkilled",
                              "3\nHighly\nSkilled")) +
  labs(
    title    = "Credit Amount by Job Category (Kruskal-Wallis)",
    subtitle = paste("Chi-squared =", round(kw_credit_job$statistic, 3),
                     "| p-value =", round(kw_credit_job$p.value, 4)),
    x        = "Job Category",
    y        = "Credit Amount (DM)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(plot_credit_job)

#Violin plot of Duration vs Sex
#displays the diffrence of loan duration between male and female
#Shows that loan duration among female are shorter than male's since the
#plotting in short loans are wider in female than male
plot_duration_sex <- ggplot(GRC_Data, aes(x = Sex, y = Duration, fill = Sex)) +
  geom_violin(color = "gray30", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", color = "gray30",
               outlier.color = "firebrick", outlier.shape = 16) +
  scale_fill_manual(values = c("female" = "tomato", "male" = "steelblue")) +
  labs(
    title    = "Loan Duration by Sex (T-Test)",
    subtitle = paste("t =", round(ttest_duration_sex$statistic, 3),
                     "| p-value =", round(ttest_duration_sex$p.value, 4)),
    x        = "Sex",
    y        = "Duration (months)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(plot_duration_sex)

#Stacked bar chart of Risk vs Checking account
#Shows that credit risk significantly difers accorss checking account levels
#with NA having an overwhelming good risk
plot_risk_checking <- ggplot(table_risk_checking,
                             aes(x = Checking_account, y = Count, fill = Risk)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("bad" = "firebrick", "good" = "cornflowerblue")) +
  labs(
    title    = "Risk Distribution by Checking Account Level (Chi-Square)",
    subtitle = paste("Chi-squared =", round(chisq_risk_checking$statistic, 3),
                     "| p-value =", round(chisq_risk_checking$p.value, 4)),
    x        = "Checking Account Level",
    y        = "Count",
    fill     = "Credit Risk"
  ) +
  theme_minimal(base_size = 13)

print(plot_risk_checking)
