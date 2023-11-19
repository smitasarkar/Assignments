import csv,os,re,sys,codecs
import numpy as np
import pandas as pd

import joblib,  statistics
from sklearn.model_selection import GridSearchCV 
from sklearn.pipeline import Pipeline
from sklearn.ensemble import AdaBoostClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn import svm 
from sklearn.linear_model import LogisticRegression 
from sklearn.ensemble import RandomForestClassifier
from sklearn.naive_bayes import MultinomialNB
from sklearn.feature_selection import SelectKBest,chi2
from sklearn.model_selection import StratifiedKFold, train_test_split
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, confusion_matrix
from sklearn.metrics import classification_report
from collections import Counter



from classification2 import classification

import warnings
warnings.filterwarnings("ignore")
list=['lr','rf']

for i in list:
    clf=classification('/home/rasel/Downloads/MLproject/', clf_opt=i,no_of_selected_features=4)

    clf.classification()
