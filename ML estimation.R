rm(list=ls())

data=read.csv("C:/... /Brush_Data.csv",header=T) # Set the path to the data folder 
attach(data); head(data) 

# Organize data
high.F=data[data$Type=="highfail",]
high.C=data[data$Type=="highcensor",]
low.F=data[data$Type=="lowfail",]
low.C=data[data$Type=="lowcensor",]
mixed.F=data[data$Type=="mixedfail",]
mixed.C=data[data$Type=="mixedcensor",]


## Different Betas: Maximum likelihood estimation #########################################################################################
## scale1=parameter[1]     shape1=parameter[2]     scale2=parameter[3]    shape2=parameter[4]
loglik=function(parameter) 
{z=sum( dweibull(high.F$t,scale=parameter[1],shape=parameter[2],log=TRUE),pweibull(high.C$t,scale=parameter[1],shape=parameter[2],log=TRUE, lower.tail = FALSE),
        dweibull(low.F$t,scale=parameter[3],shape=parameter[4],log=TRUE),pweibull(low.C$t,scale=parameter[3],shape=parameter[4],log=TRUE, lower.tail = FALSE),
        dweibull(mixed.F$t-mixed.F$t1 + parameter[3]*((mixed.F$t1/parameter[1])^(parameter[2]/parameter[4])), scale=parameter[3],shape=parameter[4],log=TRUE),
        pweibull(mixed.C$t-mixed.C$t1 + parameter[3]*((mixed.C$t1/parameter[1])^(parameter[2]/parameter[4])) ,scale=parameter[3],shape=parameter[4],log=TRUE, lower.tail = FALSE))
return(-z)}
results <- NULL
out <- nlm(loglik, p = c(5000,2,10000,2), hessian = TRUE); out


## Common Beta: Maximum likelihood estimation ##########################################################################################
## scale1=parameter[1]     shape=parameter[2]     scale2=parameter[3] 
loglik=function(parameter) 
{z=sum( dweibull(high.F$t,scale=parameter[1],shape=parameter[2],log=TRUE),pweibull(high.C$t,scale=parameter[1],shape=parameter[2],log=TRUE, lower.tail = FALSE),
        dweibull(low.F$t,scale=parameter[3],shape=parameter[2],log=TRUE),pweibull(low.C$t,scale=parameter[3],shape=parameter[2],log=TRUE, lower.tail = FALSE),
        dweibull(mixed.F$t-mixed.F$t1 + parameter[3]*((mixed.F$t1/parameter[1])^(parameter[2]/parameter[2])), scale=parameter[3],shape=parameter[2],log=TRUE),
        pweibull(mixed.C$t-mixed.C$t1 + parameter[3]*((mixed.C$t1/parameter[1])^(parameter[2]/parameter[2])) ,scale=parameter[3],shape=parameter[2],log=TRUE, lower.tail = FALSE))
return(-z)}
results <- NULL
out <- nlm(loglik, p = c(5000,2,10000), hessian = TRUE);out

