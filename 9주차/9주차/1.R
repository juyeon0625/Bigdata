iris <- read.table("iris.txt", header = TRUE)

data("iris")
attach(iris)
plot(iris)

i <- data.frame(Sepal.Length, Sepal.Width , Petal.Length, Petal.Width)
cor(i)


require(ggplot2)
library(ggplot2)
ex_graph = ggplot(iris, aes(Sepal.Length, Sepal.Width))
ex_graph + geom_point(aes(colour = Species))
ex_graph + geom_point(aes(colour = Species)) + ggtitle("ggplot1") + stat_smooth(method = "lm")
ex_graph + geom_point(aes(colour = Species)) + ggtitle("Facet1") + stat_smooth(method = "lm") + facet_grid(~Species)
ex_graph + geom_point(aes(colour = Species)) + ggtitle("Facet1") + stat_smooth(method = "lm") + w_rap(~Species)

ex_graph = ggplot(iris, aes(Petal.Length, Petal.Width))
ex_graph + geom_point(aes(colour = Species))
ex_graph + geom_point(aes(colour = Species)) + ggtitle("ggplot2") + stat_smooth(method = "lm")
ex_graph + geom_point(aes(colour = Species)) + ggtitle("Facet2") + stat_smooth(method = "lm") + facet_grid(~Species)
ex_graph + geom_point(aes(colour = Species)) + ggtitle("Facet1") + stat_smooth(method = "lm") + w_rap(~Species)


