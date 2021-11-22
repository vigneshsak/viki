#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

import os
import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from nltk.tokenize import word_tokenize
from nltk.corpus import wordnet
#from sklearn.linear_model import LogisticRegression , LinearRegression , Lasso
#from sklearn.model_selection import cross_val_score
#import statsmodels.api as sm
from sklearn.tree import DecisionTreeClassifier, DecisionTreeRegressor
#from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
#import matplotlib.pyplot as plt
from wordcloud import WordCloud
import nltk
#from nltk.corpus import stopwords
#from nltk.tag import pos_tag
#from stemming.porter2 import stem
import itertools
#import tqdm
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
#from wordcloud import WordCloud, STOPWORDS, ImageColorGenerator
import spacy
en_core = spacy.load('en_core_web_sm')
from sklearn import tree
from sklearn.feature_extraction.text import  CountVectorizer


## Change working directory
os.getcwd()

## upload dataset

os.chdir('/Users/viki/One Drive/healthcare.csv')

df=pd.read_csv('New_Control_Inventory_Scoring_and_Comments_English_only_phase_2.csv')

df_2021_data=pd.read_csv('2021 Control Descriptions Population.csv')  ## 2021 control description Population test data

df_2021=df_2021_data

# to drop all the rows containing NaN's
df=df.dropna()

df_2021=df_2021.dropna() #### test data


# to lower all the data in the datset
df= df.apply(lambda x: x.astype(str).str.lower())

df_2021= df_2021.apply(lambda x: x.astype(str).str.lower())

# removing stop words 

# stop = set(stopwords.words("english"))  
# df['Control Description'] = df['Control Description'].apply(lambda x: ' '.join([word for word in x.split() if word not in (stop)]))

#######################################################
#      lemmatizing
#######################################################

#w_tokenizer = nltk.tokenize.WhitespaceTokenizer()

### for train dataset

def get_wordnet_pos(word):
    """Map POS tag to first character lemmatize() accepts"""
    tag = nltk.pos_tag([word])[0][1][0].upper()
    tag_dict = {"J": wordnet.ADJ,
                "N": wordnet.NOUN,
                "V": wordnet.VERB,
                "R": wordnet.ADV}

    return tag_dict.get(tag, wordnet.NOUN)

lemmatizer = nltk.stem.WordNetLemmatizer()

def lemmatize_text(text):
    return ([lemmatizer.lemmatize(w,get_wordnet_pos(w)) for w in nltk.word_tokenize(text)])

df['Control Description'] = df['Control Description'].apply(lemmatize_text)


## rejoining all tokenized words
def rejoin_words_name(row):
    my_list3 = row['Control Description']
    joined_words3 = ( " ".join(my_list3))
    return joined_words3

df['Control Description'] = df.apply(rejoin_words_name, axis=1)


### 2021 test data

def get_wordnet_pos_test(word):
    """Map POS tag to first character lemmatize() accepts"""
    tag = nltk.pos_tag([word])[0][1][0].upper()
    tag_dict = {"J": wordnet.ADJ,
                "N": wordnet.NOUN,
                "V": wordnet.VERB,
                "R": wordnet.ADV}

    return tag_dict.get(tag, wordnet.NOUN)

lemmatizer = nltk.stem.WordNetLemmatizer()

def lemmatize_text(text):
    return ([lemmatizer.lemmatize(w,get_wordnet_pos_test(w)) for w in nltk.word_tokenize(text)])

df_2021['Control Description'] = df_2021['Control Description'].apply(lemmatize_text)


## rejoining all tokenized words
def rejoin_words_name_test(row):
    my_list3 = row['Control Description']
    joined_words3 = ( " ".join(my_list3))
    return joined_words3

df_2021['Control Description'] = df_2021.apply(rejoin_words_name_test, axis=1)


##################################
### for keywords 
##################################

keywords=pd.read_csv('Final Keywords as it is.csv')

def get_wordnet_pos_keyword(word):
    """Map POS tag to first character lemmatize() accepts"""
    tag = nltk.pos_tag([word])[0][1][0].upper()
    tag_dict = {"J": wordnet.ADJ,
                "N": wordnet.NOUN,
                "V": wordnet.VERB,
                "R": wordnet.ADV}

    return tag_dict.get(tag, wordnet.NOUN)

lemmatizer = nltk.stem.WordNetLemmatizer()

def lemmatize_text(text):
    return ([lemmatizer.lemmatize(w,get_wordnet_pos_keyword(w)) for w in nltk.word_tokenize(text)])

keywords['keywords'] = keywords['keywords'].apply(lemmatize_text)


## rejoining all tokenized words
def rejoin_words_name(row):
    my_list3 = row['keywords']
    joined_words3 = ( " ".join(my_list3))
    return joined_words3

keywords['keywords'] = keywords.apply(rejoin_words_name, axis=1)
keywords= keywords.apply(lambda x: x.astype(str).str.lower())


keywords_list=keywords.values.tolist()

## flattening of list
keywords_list = list(itertools.chain(*keywords_list))

###############################
# COUNTING TOTAL WORDS AND FINDING THE DIFREENCE OF EACH SENTENCE WITH THE MEDIAN
###############################

# counting the number of words in each control description (without stop words)

df['Total Words'] = df['Control Description'].str.split().str.len()

df_2021['Total Words'] = df_2021['Control Description'].str.split().str.len()  ## 2021 control description Population test data

# ## subset consistng of only good reviews and the median of each review
# df_good= df[df['GOOD or BAD']=='g']

# df.loc[df['GOOD or BAD']=='g','Total Words'].median()

# ## subset consistng of only bad reviews and the median of each review

# df_bad= df[df['GOOD or BAD']=='b']

# df.loc[df['GOOD or BAD']=='b','Total Words'].median()

# ## creating a column named difference to show the difference between the median word length and no of words in each review 
# df['difference_good']=df.loc[df['GOOD or BAD']=='b','Total Words']-df.loc[df['GOOD or BAD']=='b','Total Words'].median()
# df['difference_bad']=df.loc[df['GOOD or BAD']=='g','Total Words']-df.loc[df['GOOD or BAD']=='g','Total Words'].median()

# df['difference_good'] = df['difference_good'].fillna(0)
# df['difference_bad'] = df['difference_bad'].fillna(0)


# df['difference']= df['difference_good']+df['difference_bad']




df.loc[df['Total Words'] < 10, 'difference'] = 0

df.loc[(df['Total Words'] >= 10) & (df['Total Words'] <15), 'difference'] = 1
df.loc[(df['Total Words'] >= 15) & (df['Total Words'] <20), 'difference'] = 2

df.loc[(df['Total Words'] >= 20) & (df['Total Words'] <25), 'difference'] = 3
df.loc[(df['Total Words'] >= 25) & (df['Total Words'] <30), 'difference'] = 4

df.loc[(df['Total Words'] >= 30) & (df['Total Words'] <35), 'difference'] = 5
df.loc[(df['Total Words'] >= 35) & (df['Total Words'] <40), 'difference'] = 6

df.loc[(df['Total Words'] >= 40) & (df['Total Words'] <45), 'difference'] = 7
df.loc[(df['Total Words'] >= 45) & (df['Total Words'] <50), 'difference'] = 8

df.loc[(df['Total Words'] >= 50) & (df['Total Words'] <55), 'difference'] = 9
df.loc[df['Total Words'] >= 55 , 'difference'] = 10

## 2021 control description Population test data

df_2021.loc[df_2021['Total Words'] < 10, 'difference'] = 0

df_2021.loc[(df_2021['Total Words'] >= 10) & (df_2021['Total Words'] <15), 'difference'] = 1
df_2021.loc[(df_2021['Total Words'] >= 15) & (df_2021['Total Words'] <20), 'difference'] = 2

df_2021.loc[(df_2021['Total Words'] >= 20) & (df_2021['Total Words'] <25), 'difference'] = 3
df_2021.loc[(df_2021['Total Words'] >= 25) & (df_2021['Total Words'] <30), 'difference'] = 4

df_2021.loc[(df_2021['Total Words'] >= 30) & (df_2021['Total Words'] <35), 'difference'] = 5
df_2021.loc[(df_2021['Total Words'] >= 35) & (df_2021['Total Words'] <40), 'difference'] = 6

df_2021.loc[(df_2021['Total Words'] >= 40) & (df_2021['Total Words'] <45), 'difference'] = 7
df_2021.loc[(df_2021['Total Words'] >= 45) & (df_2021['Total Words'] <50), 'difference'] = 8

df_2021.loc[(df_2021['Total Words'] >= 50) & (df_2021['Total Words'] <55), 'difference'] = 9
df_2021.loc[df_2021['Total Words'] >= 55 , 'difference'] = 10



########################################################
## splitting data 
########################################################


df['Y'] = [1 if 'g' in i else 0 for i in df['GOOD or BAD']]
X=df[['Control Description','difference','Total Words']]
y=df[['Y','Score']]

X_2021=df_2021[['Control Description','difference','Total Words']]



x_train_set, x_test_set, y_train_set, y_test_set =train_test_split(X,y,test_size= 0.3,shuffle= True)

########################################################
##      diving datsets based on columns
########################################################

control_description = pd.DataFrame(x_train_set['Control Description'])
difference = pd.DataFrame(x_train_set['difference'])
total_words = pd.DataFrame(x_train_set['Total Words'])

#########################################################
##    changing the row index 
#########################################################

control_description.index=range(len(control_description))
total_words.index=range(len(total_words))
difference.index=range(len(difference))

y_train_set.index=range(len(y_train_set))
y_test_set.index=range(len(y_test_set))

#########################################################
##     transforming all training predictor dataframes
#########################################################
## TFIDF vectorizer and also create 3 and 4 ngram models  
   
ctv_description = CountVectorizer(analyzer='word',ngram_range=(1,4),binary=True)   
description= pd.DataFrame(ctv_description.fit_transform(x_train_set['Control Description']).toarray(),columns=ctv_description.get_feature_names())   
   
df_description = description[description.columns.intersection(keywords_list)]

df_description=df_description.astype('category')
difference=difference.astype('category')

X_train=pd.concat([control_description,df_description,difference, total_words],axis=1)   

#########################################################
##     transforming all test predictor dataframes              ## 2021 control description Population test data
#########################################################

control_description_2021 = pd.DataFrame(X_2021['Control Description'])
difference_2021 = pd.DataFrame(X_2021['difference'])
total_words_2021 = pd.DataFrame(X_2021['Total Words'])


control_description_2021.index=range(len(control_description_2021))
total_words_2021.index=range(len(total_words_2021))
difference_2021.index=range(len(difference_2021))


description_test= pd.DataFrame(ctv_description.transform(X_2021['Control Description']).toarray(),columns=ctv_description.get_feature_names())

df_description_test = description_test[description_test.columns.intersection(keywords_list)]

df_description_test=df_description_test.astype('category')
difference=difference.astype('category')

X_test=pd.concat([control_description_2021,df_description_test,difference_2021, total_words_2021],axis=1)  


########################################
## Decision Tree Classification Model 
########################################

## dropping unnecessary columns 
y_classification=y_train_set.drop(columns=['Score'])
X_train=X_train.drop(columns=['Control Description'])
X_test=X_test.drop(columns=['Control Description'])
y_test_classification=y_test_set.drop(columns=['Score'])


decisiontree_model = DecisionTreeClassifier(max_depth =9, min_samples_split=2).fit(X_train, y_classification)  ##max_depth=9, min_sample_split=2, accuracy score 0.8529411764705882
y_hat_classification= decisiontree_model.predict(X_test)
     


#accuracy_score(y_hat_classification,y_test_classification)
#confusion_matrix(y_test_classification, y_hat_classification)


# dt_feature_names=list(X_train.columns)


# fig= plt.figure(figsize= (100,75))
# tree.plot_tree(decisiontree_model,feature_names=dt_feature_names, filled=True,node_ids=True)
########################################
###       regression 
########################################

### df_description and difference should not be categories 


y_regression=y_train_set.drop(columns=['Y'])
y_regression['Score']=y_regression.astype(float)
y_regression=np.ravel(y_regression)

X_train=X_train.astype('int64')
X_train=X_train.select_dtypes(include=np.number)
y_test_regression=y_test_set.drop(columns=['Y'])


## DecisionTree Regression Model


regression_model=DecisionTreeRegressor(max_depth=7 , min_samples_split= 20).fit(X_train,y_regression)   ##max_depth=7, min_sample_split=20, MSE 2.864822849627186
y_hat_regression=regression_model.predict(X_test)
# mean_squared_error(y_test_regression, y_hat)
    

# dt_feature_names=list(X_train.columns)



# fig= plt.figure(figsize= (100,75))
# tree.plot_tree(regression_model,feature_names=dt_feature_names, filled=True,node_ids=True)


################################
##post modeling processing
################################

X_test['1 or 0']=pd.Series(y_hat_classification)

X_test['GOOD or BAD'] = X_test["1 or 0"].astype("str")
X_test['GOOD or BAD'] = ['G' if '1' in i else 'B' for i in X_test['GOOD or BAD']]


X_test['Score']=pd.Series(y_hat_regression)


### formatting the final file with control description its score and good bad classification

GB = pd.DataFrame(X_test['GOOD or BAD'])
Score= pd.DataFrame(X_test['Score'])


Final=pd.concat([df_2021_data,GB,Score],axis=1)  






