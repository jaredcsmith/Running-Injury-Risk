Human Performance Analytics: Injury Risk Prediction in Runners

Project Overview

This project investigates the relationship between training metrics and injury risk in elite middle- and long-distance runners. Using a dataset spanning nine years and containing over 42,000 training observations, we applied machine learning techniques (XGBoost, Logistic Regression, and SHAP analysis) to predict injury risk and identify key contributing factors.

Data Description
	•	Participants: 74 elite runners (47 male, 27 female)
	•	Dataset Size: 42,798 observations over 9 years
	•	Key Features:
	•	Training volume (sessions, total distance)
	•	Perceived exertion, recovery, and training success
	•	Injury occurrence (binary classification)
	•	Derived metrics factoring historical exertion and recovery trends

Methodology
	•	Exploratory Data Analysis (EDA): Visualized key training trends leading to injuries.
	•	Feature Engineering: Created variables tracking historical exertion/recovery trends.
	•	Modeling: Applied XGBoost for classification, achieving 0.713 AUC and 75.08% accuracy.
	•	SHAP Analysis: Identified max exertion, session frequency, and prior recovery trends as key predictors.

Key Findings & Actionable Insights
	•	High exertion + low recovery increases injury risk.
	•	Athletes with frequent injuries exhibit distinct training patterns.
	•	Introducing a “Vulnerability to Injury” metric could help coaches manage workload.
	•	More granular athlete-level insights (e.g., event specialization) would improve predictions.

Next Steps
	•	Refine the “Vulnerability to Injury” metric for real-world applications.
	•	Develop an interactive dashboard for athletes/coaches to monitor injury risk.
	•	Expand dataset to include injury severity, event specialization, and return-to-play metrics.
