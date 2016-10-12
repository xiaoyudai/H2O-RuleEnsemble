# MCF-paper
Rcode: 
  Codes

# H2O-RuleEnsemble
This is based on my 2016 summer intern project at Amazon/A9 (https://www.a9.com/, Palo Alto, CA), under supervision of Dr. Giovanni Seni. 

This project implemented Rule Ensembles, a Machine Learning ensemble method, on top of H2O's big-data platform. H2O provides implememnntations of the Gradient Boosting Machines and Generalized Linear Regression with Regularization (e.g. LASSO), and this work built a link between these two algorithms to implement Rule Ensembles. Specifically, individual rules are generated from the output of GBM by parsing the model definition file (given as a JAVA file), and then a GLM is fitted using these rules along with linear terms and regularization. 

This project follows the interface and borrowed and modified some R-ultility functions from REgo(https://github.com/intuit/rego), which is developed and maintained by Dr. Giovanni Seni, and was initially sponsored by the Data Engineering and Analytics group (IDEA) of Intuit, Inc. 
