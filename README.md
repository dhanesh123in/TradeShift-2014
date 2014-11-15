TradeShift-2014
===============

Kaggle TradeShift Competition (selected submissions)

The top submission was just a minor modification of a code that was published by Dimitri, YSDA (YSDA_Dimitri_Code-03). This featured a meta learner RF with first stage learners from a RF and SVM.

R-gbmtest-3.r has the best submission from a gbm (using caret package). This codes takes nearly a day to run. It removes infrequent features from the high cardinality columns before training.
I wanted to try this approach with scikit but ran out of time.

Data for this competitation can be found here:
http://www.kaggle.com/c/tradeshift-text-classification/data
