
# Central Tendency --------------------------------------------------------

## Arithmatic Mean
# Construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)

# for vector x
n_x <- length(x) # number of elements in x = the number of data points
sum_x <- sum(x) # summation for x
mu_x <- sum_x/n_x # arithmatic mean
print(mu_x) # printed calculated value

# for vector y
mu_y <- sum(y)/length(y)
print(mu_y)

print(mean(x))
print(mean(y))

## Geometric Mean
# for vector x
prod_x <- prod(x) # product of vector x; x1*x2*x3...
n_x <- length(x)
mug_x <- prod_x^(1/n_x)
print(mug_x)

# for vector y
mug_y <- prod(y)^(1/length(y))
print(mug_y)

## Median

# for vector x
x <- sort(x) # sort x from small to large
index <- (length(x)+1)/2 # (N+1)/2th index as length(x) is an odd number
med_x <- x[index]
print(med_x)

# for vector y
y <- sort(y)
med_y <- y[(length(y)+1)/2]
print(med_y)

print(median(x))
print(median(y))

# Variation ---------------------------------------------------------------

## Variance 
sqd_x <- (x-mean(x))^2 # squared deviance
sum_sqd_x <- sum(sqd_x)
var_x <- sum_sqd_x/length(x)
print(var_x)

var_y <- sum((y-mean(y))^2)/length(y)
print(var_y)

## Standard Deviation (SD)
sd_x <- sqrt(var_x) 
print(sd_x)

sd_y <- sqrt(var_y)
print(sd_y)

## IQR
# We will be using 25 and 75 percentiles as x1 and xh

x_1 <- quantile(x,0.25) # quantile(); return quantile values, 25 percentile
x_h <- quantile(x,0.75) # quantile(); return quantile values, 75 percentile
iqr_x <- abs(x_1-x_h) # abs(); absolute value
print(iqr_x)

y_q <- quantile(y,c(0.25,0.75)) # return as a vector
iqr_y <- abs(y_q[1]-y_q[2]) # y_1[1] = 25 percentile; y_q[2] = 75 percentile
print(iqr_y)

## MAD
# Median(|xi-umed|)
ad_x <- abs(x-median(x))
mad_x <- median(ad_x)
print(mad_x)

mad_y <- median(abs(y-median(y)))
print(mad_y)

## Coefficient of Variation (CV)
cv_x <- sd_x/mean(x)
print(cv_x)

cv_y <- sd_y/mean(y)
print(cv_y)

## MAD/Median
mm_x <- mad_x/median(x)
print(mm_x)

mm_y <- mad_y/median(y)
print(mm_y)

# Laboratory ---------------------------------------------------------------
z <- exp(rnorm(n=100, mean=0, sd=0.1))
mean(z)
mug_z <- prod(z)^(1/(length(z)))
mug_z
median(z)

library(ggplot2)
df <- data.frame(scores = z)
ggplot(df,aes(x=scores))+
  geom_histogram()
