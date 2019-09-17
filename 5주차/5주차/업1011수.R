faithful -> Yellowstone
head(faithful)

waiting <- faithful[,2]
range(waiting)

table(waiting)

attach(dataset)
table(Sex, Age)

Car_Colors <- data.frame(Color - factor(), Percent = numeric())
Car_Colors <- edit(Car_Colors)
Car_Colors
attach(Car_Colors)
pie(Percent)
pie(Percent, col=Color)
names(Percent) <- Color
pie(Percent, col=Color, main="Pie Graph of Auto Color Preferences")

barplot(Percent, col=colors, main = "Bar Chart of Auto Color Preferences")
attach(grades)

attach(dataset)
boxplot(Age, main = "Boxplot for Age")
quizzes
boxplot(quizzes)

attach(faithful)
hist(waiting)

attach(Min_Wage)
Min_Wage
plot(Percent, type="o", col="blue", main="Minimum Wage")
plot(Year, Value, type="o")

attach(hours)
hours
plot(hours, GPA)
plot(hours, GPA, abline(lm(GPA~hours)), main="Scatterplot of Hour and GPA")

iris
plot(iris)
