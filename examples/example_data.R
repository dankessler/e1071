set.seed(215)
# Total sample
n = 200
# Number of noise features
k = 10
# a1 and a2 are confounders
a1 = runif (n, 0, 1)
a2 = rbinom(n, 1, .5)
# d is a vector of class labels
ld = -1 + a1 + a2 + rnorm(n, 0, .5)
d = 1*(exp(ld)/(1+exp(ld))>.5)
# covariance structure for features
covmat = matrix (c (2, .5, .5, 2), 2, 2)
errs = cbind(rnorm (n), rnorm (n))%*%covmat
# x1 and x2 are features
x1mean = 5 - 2*d - .5*a1
x2mean = -3*a1 + .5*a2 - .5*d*(a1 + .5*a2 + .25*a1*a2)
x1 = scale(x1mean + errs[,1])
x2 = scale(x2mean + errs[,2])
noise = matrix (rnorm(n*k), n, k)
features = data.frame(x1=x1, x2=x2, noise=noise)

lr.fit = glm(d~a1+a2, family=binomial)
# Obtain predicted values of pr(d=1 | a1, a2) for each subject
lr.predict = lr.fit$fitted.values
# Obtain predicted probabilities of each subject's observed class
# given observed confounder values
lr.obs = lr.predict*d + (1-lr.predict)*(1-d)
# The inverse probability weights are the inverse of the former quantity
ipweights = 1/lr.obs

df = features
df$w = ipweights
colnames(df) = c(paste0('x', 1:12), 'w')
df$y = d

write.csv(df, '~/Desktop/exampleData.csv', row.names=FALSE)

