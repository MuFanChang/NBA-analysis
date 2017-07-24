# purify environment
rm(list = ls())
# import data and fix colnames
nba = read.csv('/Users/changmufan/Desktop/mva_finals/player_traditional.csv',header = T)
colnames(nba)[7:10] = c('FG%','3PM','3PA','3P%')
colnames(nba)[13] = c('FT%')
colnames(nba)[24] = c('+/-') 
colnames(nba)
# statistic info
summary(nba)
# create new data without player and conference toplot correlation plot and calculate euclidean dist
nba.clust = nba[,-c(1,2)]
plot(nba.clust,main = '2016-17 NBA Correlation Plot')
means = apply(nba.clust, 2, mean)
sds = apply(nba.clust, 2, sd)
# rescale with standardization
nba.clust = scale(nba.clust, center = means, scale = sds)
# calculate euclidean dist
nba.dist = dist(nba.clust, ,method = 'euclidean')
# cluster analysis with three method
nba.fit1 = hclust(nba.dist,method = 'single')
nba.fit2 = hclust(nba.dist,method = 'complete')
nba.fit3 = hclust(nba.dist,method = 'average')
nba.fit4 = hclust(nba.dist,method = 'ward.D')
nba.fit5 = hclust(nba.dist,method = 'ward.D2')
# plot three method's dendrogram
plot(nba.fit1,main = '2016-17 NBA Player cluster - Single')
rect.hclust(nba.fit1, k=5, border="red")
plot(nba.fit2,main = '2016-17 NBA Player cluster - Complete')
rect.hclust(nba.fit2, k=5, border="red")
plot(nba.fit3,main = '2016-17 NBA Player cluster - Average')
rect.hclust(nba.fit3, k=5, border="red")
plot(nba.fit4,main = '2016-17 NBA Player cluster - Ward.D')
rect.hclust(nba.fit4, k=5, border="red")
plot(nba.fit5,main = '2016-17 NBA Player cluster - Ward.D2')
rect.hclust(nba.fit5, k=5, border="red")
# check out the cluster results
cluster1 = cutree(nba.fit1, k=5)
sapply(unique(cluster1),function(a)nba$PLAYER[cluster1==a])
cluster2 = cutree(nba.fit2, k=5)
sapply(unique(cluster1),function(a)nba$PLAYER[cluster2==a])
cluster3 = cutree(nba.fit3, k=5)
sapply(unique(cluster1),function(a)nba$PLAYER[cluster3==a])
cluster4 = cutree(nba.fit4, k=5)
sapply(unique(cluster1),function(a)nba$PLAYER[cluster4==a])
cluster5 = cutree(nba.fit5, k=5)
sapply(unique(cluster1),function(a)nba$PLAYER[cluster5==a])
# after result's valuation, I choose cluster4 as my cluster approach to this dataset
# so...append it to nba
nba.new = cbind(nba, cbind(cluster4))
# rename the cluster's factor
nba.new$cluster4 = factor(nba.new$cluster4, levels=c(1:5), labels=c("Supporter", "Second_scorer", "Drag_legger", "Defender", "Main_Scorer"))
# count the distribution of Eastern and Western
table(nba.new$Conference, nba.new$cluster4)
plot(nba.new$cluster4 , nba.new$Conference)
# try anova with PTS & cluster....to compare with cluster anaysis
anova(lm(nba.new$PTS~factor(cluster4)))
qf(c(0.05), df1=4, df2=481, lower.tail=FALSE)