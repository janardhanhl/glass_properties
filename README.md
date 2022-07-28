# Refractive Index (RI) and Abbe number calculation by ML

RI and Abbe number for different composition of oxide glass are retrieved from glasspy hosted at https://github.com/drcassar/glasspy.
Glasspy is a python library that has a portion of sciglass database hosted at https://github.com/epam/SciGlass. 
Data is saved from glasspy colab notebook hosted at https://colab.research.google.com/drive/1MmghrTPGdt45u25XB5MsYU1-9UtwWAdP

ML Models are trained for predicting RI and Abbe number based on the composition as input.
96-97 % prediction accuracy is acheived for the testing sets.

...

Random forest models are better at prediction of glass properties.
Deep Learnin models gave good fit for validation data but for actual data variance is more.
Hence, you can use random forest model for prediction.


Prototype Shiny web app for random forest prediction can be accessed at https://janardhanhl.shinyapps.io/Random_forest_predictor/
Prototype Shiny web app for Deep Learning prediction can be accessed at https://janardhanhl.shinyapps.io/Deep_Learning_Predictor/