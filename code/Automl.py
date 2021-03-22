# for own learner
from sklearn.base import BaseEstimator, ClassifierMixin

import numpy as np
import pandas as pd

from sklearn.model_selection import cross_val_score

import pdb

class AutomlClassfier(BaseEstimator, ClassifierMixin):

  # inherited:
  # get_params():
  # set_params():
  # score():

  def fit(self, X, y=None):
    
    # self.model = 

    return self

  def predict(self, X, y=None):
    pred = np.zeros(X.shape[0]) # Implement!
    return pred


def test():
  dataset = pd.read_csv('../datasets/arrivals_ATL.csv', sep=",", header = 0)
  X, y = dataset.iloc[:, :-1], dataset.iloc[:, -1]
  automl = AutomlClassfier()
  scores = cross_val_score(automl, X, y, cv=5)
  print(scores)

def main():
  test()

if __name__ == "__main__":
    main()