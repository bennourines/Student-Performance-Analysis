---
title: "Student Performance Analysis"
date: "December 2024"
---

# 🎓 Project Overview

This project explores the key academic and lifestyle factors that influence **student exam performance**.  
The goal is to help educators and students identify the conditions that lead to better academic outcomes and optimize their study habits.

---

# 🎯 Objectives

- Identify significant predictors of student performance (Exam_Score).  
- Compare the influence of quantitative (e.g., study hours, attendance) and qualitative factors (e.g., motivation, parental involvement).  
- Provide actionable insights to improve academic success.

---

# 🧩 Dataset

**Dependent Variable (Y):**
- Exam_Score

**Independent Variables (X):**
- Hours_Studied, Attendance, Sleep_Hours, Previous_Scores, Tutoring_Sessions, Physical_Activity  
- Gender, Parental_Involvement, Access_to_Resources, Motivation_Level, Family_Income, etc.

**Size:**  Several hundred student records collected in an academic context.  
**Nature:** Mixed numeric and categorical variables.

---

# ⚙️ Methodology

1. **Data Cleaning**
   - Outlier removal using the IQR method.
   - Missing value imputation using KNN (k = 5).

2. **Exploratory Data Analysis**
   - Visualization of variable distributions.
   - Jarque–Bera test for normality.

3. **Correlation Analysis**
   - Spearman correlation for non-normal numeric data.

4. **Categorical Variable Analysis**
   - Kruskal–Wallis test and Dunn’s post-hoc test to compare group differences.

5. **Feature Importance**
   - Ranked based on Spearman correlation with `Exam_Score`.

---

# 📈 Key Findings

- **Attendance (ρ = 0.69)** and **Hours_Studied (ρ = 0.49)** are the strongest predictors of exam performance.  
- **Parental involvement**, **motivation**, and **internet access** significantly improve results.  
- **Gender** and **school type** have **no significant effect** on performance.  
- Students benefit most when they combine high attendance, consistent study habits, and supportive learning environments.

---

# 💡 Recommendations

### For Students
- Maintain consistent attendance.
- Plan study hours efficiently.
- Engage in tutoring and extracurricular activities.
- Balance study, sleep, and physical activity.

### For Educators
- Support low-motivation students with personalized interventions.
- Promote parental involvement and equitable resource access.

---

# 🧰 Technologies Used

- **Language:** R  
- **Libraries:** ggplot2, dplyr, tidyr, rstatix, corrplot, tseries, VIM  
- **Statistical Tests:** Spearman Correlation, Kruskal–Wallis, Dunn’s Test  
- **Visualization:** ggplot2 and corrplot  
- **Imputation & Outlier Treatment:** KNN and IQR Method

---

# 📊 Impact

This analysis offers **data-driven insights** to:
- Guide **educators** in improving teaching strategies.  
- Help **students** understand performance drivers.  
- Inform **administrators** about effective academic policies.

---

# 🖥️ Reproducibility

To run the analysis:

```r
source("Educational Performance Analysis.R")
