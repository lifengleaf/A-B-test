
control<- read.csv("control.csv")
control[is.na(control)] <- 0

# Gross conversion: probability of enrolling, given click
control$gc<- control$Enrollments / control$Clicks

# Net conversion: probability of payment, given click
control$nc<- control$Payments / control$Clicks

experiment<- read.csv("experiment.csv")
experiment[is.na(experiment)] <- 0

# Gross conversion
experiment$gc<- experiment$Enrollments / experiment$Clicks

# Net conversion
experiment$nc<- experiment$Payments / experiment$Clicks

# create a data frame to store the difference
diff<- data.frame(Date = control$Date)

# columns to store experiment proportion of total
diff$Ratio_Pageviews<- experiment$Pageviews /(experiment$Pageviews + control$Pageviews)
diff$Ratio_Clicks<- experiment$Clicks / (experiment$Clicks + control$Clicks)


################# Sanity Check ################# 
n1<- sum(control$Pageviews + experiment$Pageviews)
se<- sqrt(0.5*(1-0.5)/n1)
m<- 1.96*se
lower<- 0.5-m
upper<- 0.5+m

# observed ratio of pageviews in experimental group
mean(diff$Ratio_Pageviews)

n2<- sum(control$Clicks + experiment$Clicks)
se<- sqrt(0.5*(1-0.5)/n2)
m<-1.96*se
lower<- 0.5-m
upper<- 0.5+m

# observed ratio of cookies that click in experimental group
mean(diff$Ratio_Clicks)

# click-through-probability: unique cookies that click divided by pageviews
# CTP in control group
CTP_control<- sum(control$Clicks)/sum(control$Pageviews)
se<- sqrt(CTP_control*(1-CTP_control)/sum(control$Pageviews))
m<- 1.96*se
lower<- CTP_control-m
upper<- CTP_control+m

# CTP in experimental group
CTP_exp<- sum(experiment$Clicks)/sum(experiment$Pageviews)


################# Effect Size Test ################# 
# Gross conversion
gc_exp<- sum(experiment$Enrollments)/sum(experiment[1:23,]$Clicks)
gc_control<- sum(control$Enrollments)/sum(control[1:23,]$Clicks)
gc_diff<- gc_exp - gc_control

# variance
var_exp<- gc_exp*(1-gc_exp)/sum(experiment[1:23,]$Clicks)
var_control<- gc_control*(1-gc_control)/sum(control[1:23,]$Clicks)
var_diff<- var_exp+var_control

# 95% confidence interval
m<- 1.96*sqrt(var_diff)
lower<- gc_diff-m
upper<- gc_diff+m
dmin<- 0.01

# Net conversion
nc_exp<- sum(experiment$Payments)/sum(experiment[1:23,]$Clicks)
nc_control<- sum(control$Payments)/sum(control[1:23,]$Clicks)
nc_diff<- nc_exp - nc_control

# variance
var_exp<- nc_exp*(1-nc_exp)/sum(experiment[1:23,]$Clicks)
var_control<- nc_control*(1-nc_control)/sum(control[1:23,]$Clicks)
var_diff<- var_exp+var_control

# 95% confidence interval
m<- 1.96*sqrt(var_diff)
lower<- nc_diff-m
upper<- nc_diff+m
dmin<- 0.0075


################# Sign Test ################# 
# columns to store the difference of gc and nc day by day
diff$gc_diff<- ifelse((experiment$gc - control$gc) > 0, TRUE, FALSE)
# how many positive gc: 4/23
sum(diff[1:23,]$gc_diff == TRUE)

# sign test
binom.test(4, 23) # p-value = 0.002599

diff$nc_diff<- ifelse((experiment$nc - control$nc) > 0, TRUE, FALSE)
# how many positive nc: 10/23
sum(diff[1:23,]$nc_diff == TRUE)
binom.test(10, 23) # p-value = 0.6776

with(control[1:23,], lm(gc ~ nc))
plot(control[1:23,]$gc, control[1:23,]$nc,
     xlab = "Gross Conversion in Control Group",
     ylab = "Net Conversion in Control Group")
with(control[1:23,], abline(lm(nc ~ gc), col = "red", lwd=2))

with(experiment[1:23,], lm(gc ~ nc))
plot(experiment[1:23,]$gc, experiment[1:23,]$nc,
     xlab = "Gross Conversion in Experiment Group",
     ylab = "Net Conversion in Experiment Group")
with(experiment[1:23,], abline(lm(nc ~ gc), col = "blue", lwd=2))

write.csv(diff, "difference.csv")

