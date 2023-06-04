####################################
# Statistical Data Mining Example8 #
####################################

######################
# K-means clustering #
######################

x = iris[,1:4]

?kmeans

km = kmeans(x, 3, nstart=20)
km

km$cluster

plot(x, col=(km$cluster+1), pch=20, cex=2)


###########################################
# Hierarchical Clustering (Agglomerative) #
###########################################

?hclust

hc.s = hclust(dist(x), method='single')

cutree(hc.s, 3)

hc.c = hclust(dist(x), method='complete')

cutree(hc.c, 3)

hc.a = hclust(dist(x), method='average')

cutree(hc.a, 3)


par(mfrow=c(1,3))
plot(hc.s, main='Single linkage', cex=0.5)
plot(hc.c, main='Complete linkage', cex=0.5)
plot(hc.a, main='Average linkage', cex=0.5)







