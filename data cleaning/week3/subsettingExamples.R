set.seed(13435)
X <- data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
X <- X[sample(1:5), ]; X$var2[c(1,3)] = NA
X[, 1]
X[, "var1"]
X[1:2, "var1"]
X[1:2, "var2"]
X[X$var1 <= 3 & X$var3 > 11, ]
X[X$var1 <= 3 | X$var3 > 15, ]

X[X$var2 > 8, ] #Returns NAs
X[which(X$var2 > 8), ] # Ignores NAs

sort(X$var1)
sort(X$var1, decreasing = TRUE)

sort(X$var2, na.last = TRUE)
sort(X$var2, na.last = FALSE)

X[order(X$var2), ]
X[order(X$var2, na.last = TRUE), ]
X[order(X$var1, X$var3), ]

library(plyr)
arrange(X, var1)
arrange(X, desc(var1))