
module MachineLearning.LogisticRegression where

a $/ b = a / b
infixr 0 $/

sigmoid z = 1 $/ 1 + exp (-z)

