data = read.csv("C:/Users/Smita Sarkar/Documents/MS Project/PCAT.csv", header = TRUE)
data

install.packages("geepack")
install.packages("lme4")
install.packages("lmerTest")

library(geepack)

fit.exch <- geeglm(ldl ~ sex+race+age+group+triglyc+hdl+calprot+calfat+calcarb+dchol+dfiber, family=gaussian, data=data, id=id, corstr = "exchangeable", std.err="san.se")
summary(fit.exch)

fit.unstr <- geeglm(ldl ~ sex+race+age+group+triglyc+hdl+calprot+calfat+calcarb+dchol+dfiber, family=gaussian, data=data, id=id, corstr = "unstructured", std.err="san.se")
summary(fit.unstr)

fit.ar1 <- geeglm(ldl ~ sex+race+age+group+triglyc+hdl+calprot+calfat+calcarb+dchol+dfiber, family=gaussian, data=data, id=id, corstr = "ar1", std.err="san.se")
summary(fit.ar1)

library(lme4)
library(lmerTest)

fm1 <- lmer(ldl ~ sex+race+age+group+triglyc+hdl+calprot+calfat+calcarb+dchol+dfiber + (1|id), data)
fm2 <- lmer(ldl ~ sex+race+age+group+triglyc+hdl+calprot+calfat+calcarb+dchol+dfiber + (1|id) + (0+time|id), data)
fm3 <- lmer(ldl ~ sex+race+age+group+triglyc+hdl+calprot+calfat+calcarb+dchol+dfiber + (1+time|id), data)

AIC(fm1, fm2, fm3)
summary(fm1)

summary(fm2)
summary(fm3)

library(dplyr)
df = data %>% filter(id > 110200 & id < 122100)
require(lattice)
xyplot(ldl ~ time | id, data=df, group=id,  xlab=list(label="Months", cex=2), ylab=list(label="LDL", cex=2), aspect = "xy", type = c("p", "r"), col.line = "darkorange")

interaction.plot(df$time, df$id, df$ldl, xlab="Months", ylab="LDL", col=c(1:20), legend=F) 

require(ggplot2)
p <- ggplot(data = data, aes(x = time, y = ldl, group = id))
p + geom_line() + facet_grid(. ~ group) + labs(y = "LDL", x = "Months") + theme(axis.text = element_text(size = 10)) + theme(axis.title = element_text(size = 30)) 

a = select(data, id)
x = distinct(a)
x

num1 = data%>%filter(sex == "1")
y1 = unique(num1$id)
print(length(y1))

num2 = data%>%filter(sex == "2")
y2 = unique(num2$id)
print(length(y2))

num3 = data%>%filter(race == "1")
y3 = unique(num3$id)
print(length(y3))

num4 = data%>%filter(race == "2")
y4 = unique(num4$id)
print(length(y4))

num5 = data%>%filter(group == "1")
y5 = unique(num5$id)
print(length(y5))

num6 = data%>%filter(group == "2")
y6 = unique(num6$id)
print(length(y6))

num7 = data%>%filter(group == "0")
y7 = unique(num7$id)
print(length(y7))

num8 = num5%>%filter(sex == "1")
y8 = unique(num8$id)
print(length(y8))

num9 = num5%>%filter(sex == "2")
y9 = unique(num9$id)
print(length(y9))

num10 = num6%>%filter(sex == "1")
y10 = unique(num10$id)
print(length(y10))

num11= num6%>%filter(sex == "2")
y11 = unique(num11$id)
print(length(y11))

num12 = num7%>%filter(sex == "1")
y12 = unique(num12$id)
print(length(y12))

num13= num7%>%filter(sex == "2")
y13 = unique(num13$id)
print(length(y13))

num14 = num5%>%filter(race == "1")
y14 = unique(num14$id)
print(length(y14))

num15 = num5%>%filter(race == "2")
y15 = unique(num15$id)
print(length(y15))

num16 = num6%>%filter(race == "1")
y16 = unique(num16$id)
print(length(y16))

num17 = num6%>%filter(race == "2")
y17 = unique(num17$id)
print(length(y17))

num18 = num7%>%filter(race == "1")
y18 = unique(num18$id)
print(length(y18))

num19 = num7%>%filter(race == "2")
y19 = unique(num19$id)
print(length(y19))

mean(num5$age)
mean(num5$ldl)
mean(num5$hdl)
mean(num5$calfat)
mean(num5$calcarb)
mean(num5$calprot)
mean(num5$triglyc)
mean(num5$dchol)
mean(num5$dfiber)

mean(num6$age)
mean(num6$ldl)
mean(num6$hdl)
mean(num6$calfat)
mean(num6$calcarb)
mean(num6$calprot)
mean(num6$triglyc)
mean(num6$dchol)
mean(num6$dfiber)

mean(num7$age)
mean(num7$ldl)
mean(num7$hdl)
mean(num7$calfat)
mean(num7$calcarb)
mean(num7$calprot)
mean(num7$triglyc)
mean(num7$dchol)
mean(num7$dfiber)

min(data$ldl)
min(data$hdl)
min(data$triglyc)
min(data$calprot)
min(data$calcarb)
min(data$calfat)
min(data$dchol)
min(data$dfiber)

max(data$ldl)
max(data$hdl)
max(data$triglyc)
max(data$calprot)
max(data$calcarb)
max(data$calfat)
max(data$dchol)
max(data$dfiber)


