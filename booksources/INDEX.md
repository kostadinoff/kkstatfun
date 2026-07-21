# Epidemiological & Biostatistical Book Sources Index

This index tracks key references and statistical formulas used for developing `kkstatfun` functions under the tidy in / tidy out philosophy.

## Book Sources Overview

| Book File | Author / Source | Key Focus Areas |
| :--- | :--- | :--- |
| `modern-epidemiology.pdf` | Rothman, Greenland, Lash | Study design, stratified analysis (CMH, Breslow-Day), causal inference, interaction (RERI), bias analysis |
| `epidemiology-beyond-the-basics.pdf` | Szklo & Nieto | Intermediate/advanced epi, rate standardization, Cox PH, regression models, sensitivity analysis |
| `epidemiology-data-analysis.pdf` | Jewell | Mathematical & statistical foundations of epidemiological data analysis |
| `epidemiology-with-r.pdf` | Various / R Epi Community | Tidy R implementations, surveillance, outbreak analysis, diagnostic testing |
| `biostatistical-analysis.pdf` | Zar | Classical biostatistics, non-parametric tests, contingency tables, agreement, circular/trend tests |
| `handbook-of-parametric-and-nonparametric-statistical-procedures.pdf` | Sheskin | Comprehensive formulas & edge cases for parametric/non-parametric tests |

---

## Method & Formula Quick Reference

*Add entries here as you implement or refine `kk_*` functions:*

### 1. Measures of Association & Stratified Analysis
- **Mantel-Haenszel Pooled OR / RR**: Rothman (Modern Epi) Ch. 15; Szklo & Nieto Ch. 6.
- **Breslow-Day Test for Homogeneity**: Zar Ch. 23; Rothman Ch. 15.
- **Additive Interaction (RERI, AP, S)**: Rothman Ch. 10. -> Implemented in [`kk_reri`](file:///c:/Users/drkos/Desktop/kkstatfun/R/kk_twobytwo.R)

### 2. Rate Standardization & Surveillance
- **Direct & Indirect Standardization (SMR)**: Szklo & Nieto Ch. 3. -> Implemented in `kk_std_rates`, `kk_smr`.
- **Modified Poisson Regression for RRs (Zou 2004)**: Szklo & Nieto Ch. 7. -> Implemented in `kk_rr_reg`.

### 3. Survival Analysis & Competing Risks
- **Restricted Mean Survival Time (RMST)**: Implemented in `kk_rmst`.
- **Competing Risks (CIF & Gray's Test)**: Implemented in `kk_cuminc`.

### 4. Diagnostic Accuracy & Model Calibration
- **DeLong Test for ROC Comparison**: Zar / Sheskin. -> Implemented in `kk_compare_roc`.
- **Decision Curve Analysis (Net Benefit)**: Implemented in `kk_decision_curve`.

---

## How to Search the Extracted Text
Run [`booksources/extract_books.R`](file:///c:/Users/drkos/Desktop/kkstatfun/booksources/extract_books.R) in R once. 
Text versions will be generated under `booksources/extracted_text/`.
You can ask Antigravity to search any topic (e.g. *"Search Modern Epidemiology for Breslow-Day exact variance formula"*), and it will search instantly using `grep_search`.
