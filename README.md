# Smartphone Resale Price Prediction (R) — End-to-End Analytics Practicum

A practical, data-driven pricing system for used smartphones.  
This project predicts **normalized_used_price** (regression) and also assigns devices into **High vs Low** price bands (classification) to support fast quoting and consistent tiering.

➡️ Main Portfolio Hub: https://github.com/<your-username>/pratyusha-data-portfolio

---

## Business Goal
Used-phone pricing is often inconsistent and subjective. This project builds an explainable approach to:
1) produce **consistent resale price estimates** (regression), and  
2) generate a **simple High/Low tier** for fast operational decision-making (classification).  

(Problem framing + goals are documented in the practicum report.)  

---

## Dataset
- ~3,400+ devices with technical specs + usage history (brand, OS, camera MP, RAM, battery, weight, release year, days used, and normalized new/used prices). :contentReference[oaicite:2]{index=2}  
- Split: **70% train / 15% validation / 15% test** :contentReference[oaicite:3]{index=3}  
- Target: **normalized_used_price** (and a derived High/Low band using the training median). :contentReference[oaicite:4]{index=4}  


---

## Methods
### Regression (continuous price)
- Multiple Linear Regression (baseline vs full numeric MLR)
- k-NN Regression (standardized predictors; tuned k)
- Random Forest (tuned mtry)

### Classification (tiering)
- Logistic Regression (High vs Low)
- C5.0 Decision Tree (High vs Low)

Preprocessing included missing-value handling, careful zero handling, outlier filtering, and dummy encoding for categorical variables where needed.   

---

## Results (Test Set)
### ✅ Best Regression Model: Multiple Linear Regression
- **RMSE ≈ 0.2006**
- **R² ≈ 0.7821** :contentReference[oaicite:6]{index=6}  

(Validation and test metrics were very close, indicating stable generalization.) :contentReference[oaicite:7]{index=7}  

### ✅ Best Classification Model: Logistic Regression
- **Accuracy ≈ 0.8628** :contentReference[oaicite:8]{index=8}  

---

## Key Drivers (High-level)
Resale value is strongly anchored by **normalized_new_price**, and is further influenced by specs and age/usage (e.g., camera MP, screen size, release year, days used). :contentReference[oaicite:9]{index=9}  

---

## Visuals
See `/assets` for:
- Model comparison (MLR vs kNN vs RF)
- MLR metrics (validation vs test)
- Confusion matrix for logistic regression
- Top drivers summary

---

## Business Recommendations
- Use **MLR** for day-to-day resale quotes (accurate + explainable).
- Use **Logistic Regression** for fast High/Low tiering as a pricing guardrail.
- Refresh periodically as inventory and market conditions change. :contentReference[oaicite:10]{index=10}  

---

## Full Report & Slides
- `Used_Smartphone_Analytics_SAI PRATYUSHA GORAPALLI.pptx`
