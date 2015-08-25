# Analytics
Repository of R code for the course "The Analytics Edge" offered by MIT via edX.  

The course was completed in May, 2015.

### Index:

1. Introduction to Anaytics  
2. Linear Regression  
3. Logistic Regression  
4. Trees  
5. Text Analytics
6. Clustering  
7. Visualization  
8. Linear Optimization (R script was written by the course's staff) 
9. Integer Optimization  (no R code)

Some course materials can be found [here](https://drive.google.com/folderview?id=0B5rfruovGqsOTkdQN05zbFBDZW8&usp=sharing)

I have also added my scripts for the Kaggle In-Class competition. The competition consisted of predicting which NYT articles would be popular. The following files relate to the competition:

1. **NYTimesBlogTest.csv** and **NYTimesBlogTrain**: the split datasets.
2. **Data Exploration.R**: my initial analysis of the data.
3. **Models.R**: script with a series of 20 models (not all of them were submitted). 
4. **Submission_mod9_941.csv**: the top model of the two I chose for evaluation, based on Random Forests. This model gave the 852nd position (out of 2923).
5. **Submission_mod17_123.csv**: my best model, based on Generalized Boosting Regression, which would have given me the 123rd position (top 4%) if I had chosen it for evaluation. I ran the gbm models in a hurry, by just guessing the parameters instead of optimizing them, and yet I got a result superior to RF. I only worked 2 nigths on creating models for submission. The winners used optimized GBM and ensembles with RF, as well as additional feature engineering.

The instructions for the Kaggle competition can be found [here](https://www.kaggle.com/c/15-071x-the-analytics-edge-competition-spring-2015)  

