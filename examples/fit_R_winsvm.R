library(e1071)
df = read.csv('~/Desktop/exampleData.csv')

features = df[,1:12]
d = df[,14]
ipweights = df[,13]

# Fit SVM with IP weights
fit1 = svm(x=features, y=d, W=ipweights, data=df, kernel='linear', cost=1, scale=FALSE, type='C-classification')

# Fit SVM without IP weights
fit2 = svm(x=features, y=d, data=df, kernel='linear', cost=1)

