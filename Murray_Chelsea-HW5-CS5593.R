#Written for Data Mining Fall 2024 by Chelsea Murray
#Please update working directory as necessary for your machine.
setwd("C:/Users/Chelsea/Documents/FA24/Data Mining/Homework/homework 5")


#2a: Graph and Identify Distribution

#make vector for this data
v2a <- c(152.36, 130.38, 101.54, 96.26, 88.03, 85.66, 83.62, 76.53, 74.36, 
         73.87, 73.36, 73.35, 68.26, 65.25, 63.68, 63.05, 57.53)
#xv <- 1:length(v2a)
#graph <- data.frame(xv, v2a)
#qqnorm(v2a, pch = 1, frame = FALSE)
#qqline(v2a, col = "steelblue", lwd = 2)
#dev.copy(png, filename="qq_2a.png", width = 640, height = 496)
#dev.off()

#histogram with a good bin size to show the rough shape
hist(v2a, prob=TRUE, breaks=10)
#export histogram to image
dev.copy(png, filename="hist_2a.png", width = 640, height = 496)
dev.off()


#2b: Grubbs Test

#This function performs Grubbs Test to detect outliers.
#Takes x (vector of values), alpha (for significance), and length of x (n) 
#as input.
#Returns a vector of all indices for outliers.
grubbs <- function(x, alpha, n, mn=NULL, s=NULL, tv=NULL, thr=NULL) {
  #only calculate these on the first iteration
  #(they will be wrong if calculated every time)
  if(is.null(mn)) {
    #mean
    mn = mean(x)
    #standard deviation
    s = sd(x)
  }
  
  #distance from mean for all vals
  md = abs(x-mn)
  #find maxiumum z-score
  g = max(md) / s
  #find index of that max val object
  idx = which.max(md)
  
  
  #only calculate these two on the first iteration
  #(just for computational expense)
  if(is.null(tv)) {
    #t distribution
    tv = qt(p=alpha/(2*n), df=n-2, lower.tail=FALSE)
    #outlier threshold
    thr = ((n-1) / sqrt(n)) * sqrt(tv / (n-2+tv))
  }
  
  #if grubbs test val is over threshold, check for next potential outlier
  if(g > thr) {
    #get index of this outlier
    outs = c(idx)
    #copy x and replace the furthest outlier with the mean
    #so that the next iteration will find the second furthest outlier
    x2 = x
    x2[idx] = mn
    #call function again with modified vector (recursion to find all)
    nxt <- grubbs(x2, alpha, n, mn, s, tv, thr)
    #if the returned value is an outlier, append
    if(nxt[1]!=0){
      outs = append(outs, nxt)
    }
    #return indices of all outliers
    outs
  }
  #if grubbs test val is not over threshold, return value of 0
  else {
    c(0)
  }
  
}


#2c: k-th Nearest Neighbor

