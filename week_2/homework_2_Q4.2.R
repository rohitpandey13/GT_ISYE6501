#load ggplot2 for plotting
library(ggplot2)

sepal_plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()
petal_plot <- ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
print(sepal_plot + ggtitle("sepal lengtha vs. width of iris"))
print(petal_plot + ggtitle("petal lengtha vs. width of iris"))


#find the tot.withinss for every number clusters in range 1:20
kmeans_tot_withinss <- matrix(0, ncol = 1, nrow = 20)
for(i in 1:20){
  #iter.max is set to 100 to ensure convergence
  temp_kmeans = kmeans(iris[,1:4], centers = i, iter.max = 100)
  kmeans_tot_withinss[i] =  temp_kmeans$tot.withinss
}
plot(kmeans_tot_withinss, type = 'b', xlab = "number of centers (k)", ylab = "total within-cluster sum of squares (tot.withinss)", main = "total within-cluster sum of squares vs. # of centers")

#use kmeans clusters on iris data set, setting number of clusters equal to 3
iris_3 <- kmeans(x = iris[,1:4], centers = 3, iter.max = 10)

iris_3_table <- table(iris_3$cluster, iris$Species)

