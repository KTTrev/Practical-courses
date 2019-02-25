library("dplyr")

#data frame that contains the data
#This can be done by creating a matrix, a list, and then using the function as.data.frame
M<- cbind(c(497, 221, 212, 503, 1841), c(62, 12, 20, 102, 305))
N<- cbind(c(694, 4840, 383, 320, 201), c(117, 415, 65, 129, 61))
#renaming rows and columns of M and N
rownames(M)<- c('Los Angeles', 'Phoenix', 'San Diego', 'San Francisco', 'Seattle')
colnames(M)<- c('On time', 'Delayed')
rownames(N)<- c('Los Angeles', 'Phoenix', 'San Diego', 'San Francisco', 'Seattle')
colnames(N)<- c('On time', 'Delayed')
#creating list
L<- list("Alaska Airlines" = M, "America West" = N)
#Finally the dataframe
df <- data.frame(L)
df_city_delayed<- select(df, c(Alaska.Airlines.Delayed, America.West.Delayed))

#(a)
#Chi-squared test for homogeneity with the test statistics
freq<-df_city_delayed
Total.r<-c(sum(freq$Alaska.Airlines.Delayed), sum(freq$America.West.Delayed))
freq<-rbind(df_city_delayed, Total.r)
Total.c<-freq$Alaska.Airlines.Delayed + freq$America.West.Delayed
freq<-cbind(freq, Total.c)
#In the following lines, we calculate the observed freequencies
a<-(freq[6,1]/freq[6,3])*(Total.c)
a<-head(a, -1)   #to remove the last element
b<-(freq[6,2]/freq[6,3])*(Total.c)
b<-head(b, -1)
k<-(df_city_delayed$Alaska.Airlines.Delayed - a)**2
l<-(df_city_delayed$America.West.Delayed - b)**2
test.statistics<-sum(k/a)+sum(l/b)

#Chi-squared test for independence with the r command chisq.test
chisq.test(df_city_delayed)

#(b)
#Test with an appropriate test if the delay is independent on the airline.
#
chisq.test(t(df_city_delayed))
Total.Delayed<-c(sum(df_city_delayed$Alaska.Airlines.Delayed), sum(df_city_delayed$America.West.Delayed))
chisq.test(Total.Delayed)
#(c)
#For each airline use the appropriate test to test if the delay probability equals 0.14 .
#In each airline, we can consider the number of delays as success, and on time as failures.
#Therefore, at Alaska.Airlines, the number of success
#is nothing but sum(df$Alaska.Airlines.Delayed), and the number of failures is sum(df$Alaska.Airlines.On.time)
#Alaska.Airlines
x<-sum(df$Alaska.Airlines.Delayed)
p<-0.14
n<-sum(df$Alaska.Airlines.Delayed) + sum(df$Alaska.Airlines.On.time)
binom.test(x, n, p)

#America.West
y<-sum(df$America.West.Delayed)
q<-0.14
m<-sum(df$America.West.Delayed) + sum(df$America.West.On.time)
binom.test(y, m, q)

#Now we perform the one-sided test
#Alaska.Airlines: one-sided test
x<-sum(df$Alaska.Airlines.Delayed)
p<-0.14
n<-sum(df$Alaska.Airlines.Delayed) + sum(df$Alaska.Airlines.On.time)
binom.test(x, n, p, alternative = "greater")

#America.West: one-sided test
y<-sum(df$America.West.Delayed)
q<-0.14
m<-sum(df$America.West.Delayed) + sum(df$America.West.On.time)
binom.test(y, m, q, alternative = "greater")








