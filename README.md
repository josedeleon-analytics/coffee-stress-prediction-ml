# Coffee & Stress Prediction (Supervised Machine Learning)
License: MIT • Language: R • Models: Multinomial LR, Random Forest, XGBoost

📊 Supervised machine learning analysis of how coffee consumption and lifestyle factors relate to stress levels.

---

## 📌 Project Overview
This project investigates how **coffee intake, sleep behavior, and health indicators** influence individual **stress levels** (Low / Medium / High) using a synthetic health dataset of **10,000 observations**.

The analysis follows a complete **data mining pipeline**, including:
- Exploratory Data Analysis (EDA)
- Feature engineering
- Data leakage detection and removal
- Class imbalance handling
- Supervised model training and evaluation

The goal is both **predictive performance** and **behavioral insight**, showing how lifestyle data can inform wellness analytics.

---

## 🔍 Key Research Questions
- How strongly is sleep duration associated with stress levels?
- Does coffee consumption directly increase stress, or is its effect mediated by sleep?
- Which lifestyle and health variables are the strongest predictors of stress?
- Can supervised ML models accurately classify stress levels in imbalanced data?
- How do linear and tree-based models compare in this context?

---

## 🛠 Methods Used

### Exploratory & Statistical Analysis
- Distribution analysis (histograms, boxplots)
- Correlation analysis (numeric + ordinal encoding)
- Categorical trend analysis (grouped means + confidence intervals)

### Feature Engineering
- Ordinal encoding of sleep quality and health issues
- Creation of **Sleep Deficit** feature (`max(7.5 − sleep_hours, 0)`)
- Removal of redundant and collinear variables
- **Explicit detection and removal of leaker variables**

### Supervised Machine Learning
- **Multinomial Logistic Regression** (ridge-regularized baseline)
- **Random Forest (ranger)** for non-linear interactions
- **XGBoost** for high-resolution decision boundaries

### Model Evaluation
- Stratified train/test split (80/20)
- Upsampling **only inside CV folds**
- Metrics:
  - Accuracy
  - Macro-F1
  - Multiclass ROC-AUC (OvR)
  - Cohen’s Kappa

---

## 📈 Results Summary

- **Best Model:** XGBoost
- **Accuracy:** 0.994
- **Macro-F1:** 0.985
- **ROC-AUC (OvR):** 0.9997
- **Kappa:** 0.987

### Key Insights
- **Sleep Hours** is the dominant predictor of stress.
- **Health Issues severity** strongly amplifies stress risk.
- **Coffee intake has a weak direct effect** once sleep is controlled, indicating an **indirect (mediated) relationship**.
- Errors are concentrated between adjacent classes (Medium ↔ High), which is expected in ordinal outcomes.

---

## 📂 Repository Structure

coffee-stress-prediction-ml/
├── scripts/
│ ├── 01_eda.R
│ ├── 02_feature_engineering.R
│ └── 03_modeling.R
├── outputs/
│ ├── figures/
│ └── tables/
├── report/
│ └── Final_Report.pdf
├── data/ # dataset (not tracked)
├── LICENSE
├── .gitignore
└── README.md


---

## ▶️ How to Reproduce This Project

```bash
git clone https://github.com/josedeleon-analytics/coffee-stress-prediction-ml.git
cd coffee-stress-prediction-ml


---

⚠️ Limitations

The dataset is synthetic, resulting in stronger-than-real correlations.

Real-world data would introduce noise, missingness, and weaker signal.

Results demonstrate methodology and modeling rigor, not clinical diagnosis.

---

🔑 License

This project is licensed under the MIT License — see the LICENSE file for details.


---

👤 Author

Jose De Leon
🎓 Master’s in Analytics Candidate — Northeastern University
📍 Vancouver, Canada
🔗 LinkedIn | GitHub
