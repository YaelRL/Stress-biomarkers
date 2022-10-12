# Stress biomarkers
End-to-end ML classification model project. Work process included data cleaning, statistical analysis, clustering using PCA, fitting regression and classification models and performance evaluation.

Aim: In an effort to develop a stress-detection monitor using breath sensors, this study’s aim is to investigate possible stress biomarkers in breath biochemicals.

Methods: Data from breath sensors, as well as heart rate variability measures (HRV) and self-reported anxiety measures (Trait and State Anxiety Inventory questionnaires and VAS anxiety) was collected before, during and after a stressful task, the Trier Social Stress Test (TSST). TSST includes preparing and performing a presentation and performing arithmetic calculations. The intervention group consisted of 14 participants, and the control group consisted of 12 participants.
Each participant had measures recorded at 4 stages: baseline (A), end of TSST (B), 30 minutes after TSST (C), and 60 minutes after TSST (D).
Statistical analysis was performed using linear models, linear mixed models and bootstrapping.
Prediction models for HRV, Trait and State anxiety, VAS anxiety and group assignment were fitted for each stage using K-Nearest Neighbor (KNN) regression and classification models.

Results: prediction models based on breath data had minor success in predicting State anxiety in stages A, C and D, and also in predicting VAS anxiety, HRV and mean heart rate in stage A. Classification models for group assignment achieved an accuracy rate of 73% in stages B and D, and 62% in stage C.

Conclusions: Further research with larger samples is needed to establish whether breath biochemicals can be used as detectors of stress.
