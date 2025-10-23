# Educational Performance Analysis
# This script analyzes factors influencing student exam performance
# using nonparametric methods due to the non-normal distribution of variables.

# ===============================
# BUSINESS UNDERSTANDING
# ===============================

# The goal of this analysis is to uncover key factors that affect student exam performance 
# (measured by the variable "Exam_Score") in an academic context. 
# By identifying these factors, educators and administrators can prioritize areas for 
# intervention and resource allocation.

# Dependent Variable (Y): 
# - Exam_Score: The score students achieved in their final exams (numeric).

# Independent Variables (X):
# - Hours_Studied: Study hours allocated by students (numeric).
# - Attendance: Percentage of attendance in classes (numeric).
# - Sleep_Hours: Average sleep hours per day (numeric).
# - Previous_Scores: Scores from prior assessments (numeric).
# - Tutoring_Sessions: Count of extra tutoring sessions attended (numeric).
# - Physical_Activity: Weekly hours of physical activity (numeric).
# - Categorical Factors (e.g., Gender, Parental Education, etc.).

# Nonparametric methods are used because most numeric variables exhibit non-normal 
# distributions, and the data includes ordinal and nominal types.
# Load required libraries
library(ggplot2)      # For visualization
library(gridExtra)    # For arranging multiple plots
library(dplyr)        # For data manipulation
library(tidyr)        # For data reshaping
library(rstatix)      # For statistical tests
library(corrplot)     # For correlation visualization
library(tseries)      # For Jarque-Bera test
library(VIM)          # For KNN imputation

# ===============================
# 1. DATA LOADING AND PREPARATION
# ===============================

# Load data
data <- read.table(file=file.choose(), header=TRUE, sep=",", dec=".")

#===============================
# 0-1 Data Understanding
#===============================
str(data)
dim(data)
summary(data)
# Students studied an average of 20 hours (Hours_Studied), but the range is wide (0 to 44 hours).

# Attendance is relatively high, with an average of 79.95%, but the negative values require further investigation.

# Students sleep an average of 7 hours per day, indicating a healthy range overall.

# Exam performance shows a mean of 67.22, suggesting average scores, but the negative score distorts the data.

# Physical Activity and Tutoring Sessions are relatively low, indicating that most students do not engage in many extracurricular or tutoring activities.


#define relevant numeric variables
numeric_vars <- c("Hours_Studied", "Attendance", "Sleep_Hours", 
                 "Previous_Scores", "Tutoring_Sessions", 
                 "Physical_Activity", "Exam_Score")

# ------------------------------
# 1.1 OUTLIER DETECTION AND TREATMENT
# ------------------------------

# Initial inspection of outliers using boxplots

# Reshape data into long format for ggplot
data_numeric <- data[, numeric_vars]

#Treating outliers using the IQR method (removes values outside of 1.5 times the IQR)
get_outliers <- function(x) {
  # Remove NAs for outlier calculation
  x <- na.omit(x)
  
  # Calculate IQR and find outliers
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Return the outliers
  return(x[x < lower_bound | x > upper_bound])
}
# The IQR method was chosen for its simplicity, robustness, 
# and suitability for non-normal data, effectively handling outliers 
# without being influenced by extreme values.

# Reshape data into long format for ggplot
data_long <- pivot_longer(data_numeric, cols = everything(), 
                          names_to = "Variable", values_to = "Value")

# Filter out the outliers using the get_outliers function
outliers_data <- data_long %>%
  group_by(Variable) %>%
  filter(Value %in% get_outliers(Value))

# Check the resulting outliers (optional)
print(outliers_data)

# Create the enhanced boxplot with colored components
ggplot(data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 3, outlier.fill = "red", 
               outlier.color = "black", color = "black", width = 0.6) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 4, fill = "yellow") +  
  theme_minimal(base_size = 12) +
  labs(title = "Enhanced Box Plots for Selected Variables",
       subtitle = "Highlighting Median, Outliers, and Quartiles",
       x = "Variables",
       y = "Values") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        axis.text.x = element_text(face = "bold", size = 10, angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Pastel1")  


# Treating outliers using the IQR method (removes values outside of 1.5 times the IQR)
for (var in numeric_vars) {
  q <- quantile(data[[var]], c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  
  # Replace outliers with NA
  data[[var]][data[[var]] < lower_bound | data[[var]] > upper_bound] <- NA
}

# Create a colorful boxplot after treating outliers
boxplot(data[numeric_vars], 
        main = "Box Plots After Outlier Treatment", 
        col = rainbow(length(numeric_vars)),  
        border = "black",                     
        outcol = "red",                       
        outpch = 16,                          
        notch = TRUE,                         
        horizontal = FALSE)                   



# Interpretation:
# - `Hours_Studied`: The box plot shows a wide range of values, with some potential outliers on the high end.
# - `Sleep_Hours`: The box plot indicates a relatively symmetric distribution with a median around 7-8 hours of sleep.
# - `Tutoring_Sessions`: The box plot reveals a skewed distribution, with the majority of students having a low number of tutoring sessions and a few outliers with a high number of sessions.
# - `Exam_Score`: The box plot shows a relatively wide range of exam scores, with some potential outliers on both the high and low end.

# After outlier treatment:
# - `Hours_Studied`: The distribution is more compact after the removal of outliers, with a clearly defined median and fewer extreme values.
# - `Sleep_Hours`: No significant change after outlier removal, indicating minimal outliers for this variable.
# - `Tutoring_Sessions`: The box plot is now more symmetric and compact, with the removal of some high outliers.
# - `Exam_Score`: The distribution appears tighter and more centered, with the elimination of both high and low outliers.

# The outlier treatment has helped to reduce the influence of extreme values, leading to box plots that better represent the central tendency and spread of each numeric variable.

# ------------------------------
# 1.2 MISSING VALUE TREATMENT
# ------------------------------

# Identify missing values
missing_cols <- colnames(data)[colSums(is.na(data)) > 0]
missing_percentage <- colSums(is.na(data)) / nrow(data) * 100
print("Missing Value Percentages:")
print(missing_percentage[missing_percentage > 0])
#Missing Value visualization
# Calculate missing values (counts and percentages)
missing_data <- data %>%
  summarise_all(~sum(is.na(.))) %>%            
  pivot_longer(cols = everything(),           
               names_to = "Features", 
               values_to = "Missing_Count") %>%
  mutate(Missing_Percent = round((Missing_Count / nrow(data)) * 100, 2)) %>%  
  filter(Missing_Count > 0) %>%               
  arrange(desc(Missing_Percent))              

#Plot missing values
ggplot(missing_data, aes(x = reorder(Features, Missing_Percent), y = Missing_Count)) +
  geom_bar(stat = "identity", 
           aes(fill = Missing_Percent > 5),   
           width = 0.6) +
  geom_text(aes(label = paste0(Missing_Percent, "%")), 
            hjust = -0.1, size = 4, color = "black") +
  scale_fill_manual(values = c("TRUE" = "cyan", "FALSE" = "salmon"), 
                    labels = c("<= 5%" = "Few Missing", "> 5%" = "High Missing")) +
  labs(title = "Missing Values in Dataset",
       subtitle = "Features with missing data (in counts and percentages)",
       x = "Features",
       y = "Number of Missing Values",
       fill = "Missing %") +
  coord_flip() +                               
  theme_minimal(base_size = 12) +              
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.position = "top")
#we visualiza the graph and we found that the qualitative columns have not missing value
#so we make imputation only for quantitative variable using knn

# Impute missing values using k-Nearest Neighbors (k = 5)
data_imputed <- kNN(data[, missing_cols], k = 5)
data[, missing_cols] <- data_imputed[, missing_cols]

# Interpretation:
# The KNN method (k = 5) was chosen for its ability to preserve data relationships, 
# handle both numeric and categorical variables, and impute values based on similar records, 
# ensuring realistic and consistent results.

# ===============================
# 2. NORMALITY TESTING
# ===============================

# Visualize variable distributions
plots <- list()
for (var in numeric_vars) {
  # Histogram and density plot
  hist_plot <- ggplot(data, aes_string(var)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
    geom_density(color = "red") +
    ggtitle(paste("Distribution of", var))
  plots[[length(plots) + 1]] <- hist_plot
  
  # Q-Q plot
  qq_plot <- ggplot(data, aes(sample = .data[[var]])) +
    stat_qq() + stat_qq_line() +
    ggtitle(paste("Q-Q Plot:", var))
  plots[[length(plots) + 1]] <- qq_plot
}
do.call("grid.arrange", c(plots, ncol = 4))

# Perform Jarque-Bera test for normality
jb_results <- data %>% 
  select(all_of(numeric_vars)) %>% 
  summarise(across(everything(), ~ jarque.bera.test(.)$p.value))

print("Jarque-Bera Test Results:")
print(jb_results)

# Interpretation:
# - Most variables exhibit non-normal distributions (p < 0.05), confirming the need for nonparametric methods in subsequent analyses.
# - Only Physical_Activity has a p-value > 0.05, suggesting it may follow a normal distribution.

# ===============================
# 3. CORRELATION ANALYSIS
# ===============================

# Compute Spearman correlation
cor_matrix <- cor(data[numeric_vars], method = "spearman", use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", 
         title = "Spearman Correlation Matrix")

# Examine relationships with Exam_Score
for (var in numeric_vars) {
  if (var != "Exam_Score") {
    corr_test <- cor.test(data[[var]], data$Exam_Score, method = "spearman")
    print(paste("Correlation:", var, "vs Exam_Score"))
    print(corr_test)
  }
}

# Observation:
# - Attendance (ρ = 0.69) and Hours_Studied (ρ = 0.49) are the strongest predictors of Exam_Score.
# - Previous_Scores (ρ = 0.19) and Tutoring_Sessions (ρ = 0.15) have moderate relationships with Exam_Score.
# - Physical_Activity (ρ = 0.04) and Sleep_Hours (ρ = 0.03) have weak correlations with Exam_Score, suggesting their limited impact.

# ===============================
# 4. CATEGORICAL VARIABLE ANALYSIS
# ===============================

# Analyze categorical variables using Kruskal-Wallis test
	categorical_vars <- data %>% select_if(is.character)
for (var in names(categorical_vars)) {
    
  formula <- reformulate(var, response = "Exam_Score")
    # Kruskal-Wallis test: non-parametric test to determine if there are 
    # statistically significant differences between two or more groups of a #categorical variable 
  kw_test <- kruskal_test(formula, data = data)
   
    # Dunn's test: post-hoc test that follows the Kruskal-Wallis test
    # It performs pairwise comparisons between groups
    # applies Bonferroni correction to control for multiple comparisons
  dunn_test <- dunn_test(formula, data = data, p.adjust.method = "bonferroni")
  
  print(paste("Kruskal-Wallis Test Results for", var, ":"))
  print(kw_test)
  print(paste("Dunn's Test Results for", var, ":"))
  print(dunn_test)
}
#Choice of test : we chose the Kruskal-Wallis test because we have categorical variables with more than two modalities.

# Observation:  
# -> Parental Involvement, Access to Resources, Motivation Level, and Family Income:
#   These categorical variables show significant differences in Exam_Score, meaning that different levels of these variables are associated with different median exam scores.
#   - Parental Involvement: Significant differences between High, Low, and Medium involvement levels, with High parental involvement leading to better exam scores.
#   - Access to Resources: Significant differences are found between High, Low, and Medium resource levels, showing that better access to resources improves exam scores.
#   - Motivation Level: Significant differences were found, particularly between High and Low motivation levels, suggesting that motivated students tend to score higher.
#   - Family Income: Significant differences in exam scores based on income levels, with higher income groups showing better performance.

# - Extracurricular Activities:
#   The Kruskal-Wallis test shows a significant difference in exam scores based on whether students participate in extracurricular activities.
#   - Dunn's Test reveals that participation in extracurricular activities (Yes vs. No) significantly correlates with higher exam scores, suggesting the positive impact of such activities on student performance.

# - School Type and Gender:
#   These variables show no significant differences in exam scores. The Kruskal-Wallis test results for School Type (Private vs. Public) and Gender (Male vs. Female) have p-values above 0.05, indicating that these factors do not have a measurable effect on exam performance.
#   - Even after applying Dunn's test for pairwise comparisons, no significant differences between Private vs. Public schools or between Male and Female students were found.
#   - Conclusion: These variables do not need to be prioritized when analyzing factors affecting student performance.

# Specific Test Results Interpretation:
# - Parental Involvement:
#   - Kruskal-Wallis Test: p-value < 2.2e-16 (significant).
#   - Dunn’s Test: Significant pairwise differences between High vs. Low, High vs. Medium, and Low vs. Medium parental involvement levels (all p.adj < 0.05).
#   - Interpretation: Higher parental involvement (High) correlates with significantly better exam scores compared to Low and Medium levels of involvement.
 
# - Access to Resources:
#   - Kruskal-Wallis Test: p-value < 2.2e-16 (significant).
#   - Dunn’s Test: Significant differences between High vs. Low, High vs. Medium, and Low vs. Medium (all p.adj < 0.05).
#   - Interpretation: Students with better access to resources (High) score significantly better on exams compared to those with Low or Medium access.

# - Extracurricular Activities:
#   - Kruskal-Wallis Test: p-value = 1.945e-06 (significant).
#   - Dunn’s Test: Significant difference between Yes and No (p.adj < 0.05).
#   - Interpretation: Participation in extracurricular activities significantly impacts student exam performance, with those participating scoring higher on average.

# - Motivation Level:
#   - Kruskal-Wallis Test: p-value = 7.004e-12 (significant).
#   - Dunn’s Test: Significant differences between High vs. Low, High vs. Medium, and Low vs. Medium motivation levels (all p.adj < 0.05).
#   - Interpretation: High motivation students tend to perform significantly better in exams, and motivation level is a strong determinant of exam success.

# - Internet Access:
#   - Kruskal-Wallis Test: p-value = 5.832e-07 (significant).
#   - Dunn’s Test: Significant difference between No vs. Yes (p.adj < 0.05).
#   - Interpretation: Students with internet access perform significantly better than those without, indicating that internet access plays a crucial role in academic performance.

# - Family Income:
#   - Kruskal-Wallis Test: p-value = 4.788e-14 (significant).
#   - Dunn’s Test: Significant differences between High vs. Low, High vs. Medium, and Low vs. Medium (all p.adj < 0.05).
#   - Interpretation: Higher family income is associated with better exam scores, highlighting the importance of economic factors in student performance.

# - Teacher Quality:
#   - Kruskal-Wallis Test: p-value = 1.537e-09 (significant).
#   - Dunn’s Test: Significant differences between High vs. Low, and High vs. Medium (p.adj < 0.05).
#   - Interpretation: Better teacher quality correlates with higher exam scores, emphasizing the importance of skilled teachers in student success.

# - Peer Influence:
#   - Kruskal-Wallis Test: p-value < 2.2e-16 (significant).
#   - Dunn’s Test: Significant differences between Negative vs. Neutral, Negative vs. Positive, and Neutral vs. Positive (p.adj < 0.05).
#   - Interpretation: Positive peer influence is associated with better academic performance. Students with positive peer influence tend to perform significantly better than those with negative or neutral influences.

# - Learning Disabilities:
#   - Kruskal-Wallis Test: p-value < 2.2e-16 (significant).
#   - Dunn’s Test: Significant difference between No vs. Yes (p.adj < 0.05).
#   - Interpretation: Students without learning disabilities score significantly higher than those with learning disabilities, indicating a substantial impact of learning disabilities on exam performance.

# - Parental Education Level:
#   - Kruskal-Wallis Test: p-value < 2.2e-16 (significant).
#   - Dunn’s Test: Significant differences between College vs. High School, College vs. Postgraduate, and High School vs. Postgraduate (all p.adj < 0.05).
#   - Interpretation: Students whose parents have higher education levels (College and Postgraduate) tend to score significantly higher, reflecting the positive impact of parental education.

# - Distance from Home:
#   - Kruskal-Wallis Test: p-value = 4.525e-16 (significant).
#   - Dunn’s Test: Significant differences between Far vs. Moderate, Far vs. Near, and Moderate vs. Near (p.adj < 0.05).
#   - Interpretation: Students living closer to the school tend to perform better, likely due to ease of access and less time spent commuting.

# - Gender:
#   - Kruskal-Wallis Test: p-value = 0.4548 (not significant).
#   - Dunn’s Test: All comparisons non-significant (p.adj > 0.05).
#   - Interpretation: Gender does not significantly affect exam performance, showing no measurable impact on student success based on gender.

# ===============================
# 5. FEATURE IMPORTANCE ANALYSIS
# ===============================

# Calculate and sort feature importance
feature_importance <- data %>% 
  summarise(across(all_of(numeric_vars), 
                  ~ cor(.x, data$Exam_Score, method = "spearman"))) %>%
  pivot_longer(everything(), 
               names_to = "Variable", 
               values_to = "Correlation") %>%
  arrange(desc(abs(Correlation)))

# Visualize feature importance
ggplot(feature_importance, 
       aes(x = reorder(Variable, -abs(Correlation)), 
           y = Correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature Importance Based on Correlation with Exam Score",
       x = "Variables",
       y = "Spearman Correlation")

# Interpretation of the graph:
# The graph titled "Feature Importance Based on Correlation with Exam Score" shows the strength of association between each numeric variable and the Exam_Score:

# - Exam_Score: The variable correlates perfectly with itself (ρ = 1.0), as expected.
# - Attendance: Shows a strong positive correlation with Exam_Score (ρ = 0.69). Students with higher attendance tend to score better on exams, making it one of the strongest predictors.
# - Hours_Studied: Also shows a strong positive correlation with Exam_Score (ρ = 0.49). This suggests that studying more hours is associated with better exam performance.
# - Previous_Scores: Exhibits a moderate positive correlation (ρ = 0.19), indicating that past academic performance helps predict current exam results.
# - Tutoring_Sessions: Shows a smaller positive correlation (ρ = 0.15), suggesting that additional tutoring has a limited but beneficial influence on exam performance.
# - Physical_Activity and Sleep_Hours: Both variables show weaker correlations (ρ = 0.04 and ρ = 0.03, respectively), implying they have less significant impact on predicting exam performance compared to the other variables.

# ===============================
# 6. KEY FINDINGS
# ===============================

# 1. Attendance and Hours_Studied are the strongest predictors of Exam_Score.
#	=> Students with better attendance and more study hours perform significantly better.
# 2. Sleep_Hours and Physical_Activity show weaker correlations but are still noteworthy.
#	=> These variables show some effect, but not as strongly as academic-focused variables like attendance and study hours.
# 3. Parental Education Level and Parental Involvement significantly impact Exam_Score distribution.
#	=> Higher parental education and involvement correlate with higher exam scores.
# 4. Non-normality in distributions necessitated the use of nonparametric methods for analysis, ensuring that conclusions were drawn appropriately.
# 5. Extracurricular Activities and Internet Access also have a positive impact, showing that outside-the-classroom factors influence performance.
# 6. Non-significant Variables: School type and gender do not show a measurable effect on exam performance.
#	=> Gender and school type should not be prioritized when addressing academic success factors.

# To conclude:
# - Strongest predictors: Attendance, Hours Studied, Parental Involvement.
# - Secondary predictors: Previous Scores, Family Income, Motivation.
# - Non-significant: Gender and School Type.