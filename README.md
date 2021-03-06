# Functions for Multi-Variable Prediction and Evaluation using *bnlearn package*
![image](https://user-images.githubusercontent.com/32513366/85713471-c9456b00-b6bf-11ea-8252-f3cbefec5db2.png)

**Overview**: This set of functions was created for **Discrete Bayesian Networks** using *bnlearn package* to apply multi-variable prediction and metrics to evaluate your model quality.

## Functions

### bnMultiVarPrediction
**What it does**: Calculate a multi-variable prediction for discrete bayesian models.

**Parameters**
- ```bnFit```: a object type *bn.fit* (created using *bnlearn package*)
- ```trainSet```: *dataframe* used to train your model
- ```testSet```: *dataframe* to evaluate your model i.e. prediction
- ```to_predict```: *vector* with variables that you want to predict with your model
- ```to_evidence```: *vector* with variables from your model that you willll give as evidence to your model to predict
- ```nSamples```: *integer* related to how many samples that you want to generete in your prediction. Available only if *calcFunction* parameter is set to **cpdist**.
- ```calcFunction```: *string* to define the method that will calculate your prediction. The options are *predict* and *cpdist*. For more information about those methods acess [here for predict](http://www.bnlearn.com/documentation/man/impute.html) and [here for cpdist](http://www.bnlearn.com/documentation/man/cpquery.html). The NULL option is *predict*.

**PS**: If you want to predict any root variable in your model, is strictly recommend to use *predict* method. Most because *cpdist* will give the data distribution for that variable.

**Values**

Return a list of 2 elements:
- ```probList``` is a *list* of probabilities given by your model for every sample for every class from that variable.
- ```dominantList``` is a *list* of most probable class for that variable (higher probability) predict by your your model.

### bnMetricsMultiVarPrediction
**What it does**: Calculate a set of metrics based on your predictions. See more in **Details**.

**Parameters**
- ```reference```: *dataframe* from your test set that contains the predicted variables
- ```prediction```: *dominantList* returned from **bnMultiVarPrediction**
- ```predProbList```: *predProbList* returned from **bnMultiVarPrediction**

**Values**

Return a list of 3 elements:
- ```cmList``` is a *list of tables* for confusion matrix of each predicted variable
- ```ovaList``` is a *list of tables* for OVA matrix of each predicted variable
- ```eval``` is a *dataframe* where the rows are the predicted variables and the columns are the calculated metrics.

## How to use this content
- Clone/download this repository to your working directory
- In your code, source the file *bnlearn_discrete_multivar_prediction_eval.R* as in the example bellow.

## Example Code
```
library(bnlearn)
source('bnlearn_discrete_multivar_prediction_eval.R')
# Define Train and Test
test <- learning.test[1:1000,]
train <- learning.test[1001:nrow(learning.test),]
# Create and fit model
bnFitted <- bn.fit(x = hc(train), data = train)
# Plot model
plot(hc(train))
# Define Target variables (Variables to be predicted)
pred <- c('B','D')
# Evidence variables (Variables that you will give information to the BN to do the prediction)
evid <- names(train)[!names(train) %in% pred]
# Multi var Prediction
results <- bnMultiVarPrediction(bnFit = bnFitted, 
                                trainSet = train,
                                testSet = test,
                                to_predict = pred,
                                to_evidence = evid, 
                                calcFunction = 'predict')
# Metrics Evaluation
metrics <- bnMetricsMultiVarPrediction(reference = test[pred],
                                       prediction = results$dominantList,
                                       predProbList = results$probList)
```

## Details
The metrics was based in *One VS All (OVA)* method for the confusion matrix metrics, *Scoring Rules* and the accuracy based on the *multi-level confusion matrix*. The OVA metrics final result for a *j Variable* is the mean of all metrics calculated for each *v level*.
### One VS All 
- Accuracy
- Sensibility
- Specificity
- Precision
- F1-Score
- MCC (Matthews Correlation Coefficient)

### Scoring Rules
- Spherical Payoff
- Brier Loss
- Log Loss

### Multi-Level Confusion Matrix
- Accuracy

## References
- For Confusion Matrix Metrics (One VS All) [here](https://en.wikipedia.org/wiki/Confusion_matrix)
- For the Scoring Rules [here](https://www.norsys.com/tutorials/netica/secD/tut_D2.htm) and [here for brier score](https://en.wikipedia.org/wiki/Scoring_rule)

## Acknowledgment
For the *bnlearn* package creator, **Marco Scutari**, that helped in multi-variable prediction algorithm creation.
