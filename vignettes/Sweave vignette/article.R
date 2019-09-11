### R code from vignette source 'article.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 65, useFancyQuotes = FALSE)
library(MASS) # 
library(statnet) # to get the sociomatrices from data
library(mlogit) # for comparing with mlogit estimation
library(Hmisc) # For latex tables
library(igraph)
library(statnet)


###################################################
### code chunk number 2: package (eval = FALSE)
###################################################
## library(goldfish)


###################################################
### code chunk number 3: clean_environment
###################################################
rm(list=ls())


###################################################
### code chunk number 4: data
###################################################
data("Social_Evolution")


###################################################
### code chunk number 5: look
###################################################
head(calls)


###################################################
### code chunk number 6: histogram1
###################################################
par(mar = c(4, 4, 1, 1))
hist(calls$time, breaks = 100, main="Histogram of calls over time", xlab="Time", ylab="Frequency of calls", col="gray70", col.main="gray14",  col.lab="gray14")
box()


###################################################
### code chunk number 7: subset
###################################################

#### Subset of social_evolution
calls_subset<-calls[20:30,]
rownames(calls_subset)<-c(1:nrow(calls_subset))

## Actors
actors_subset1<-unique(c(unique(calls_subset$sender), calls_subset$receiver))
actors_subset2<-actors_subset1[order(actors_subset1, decreasing=F)]

actors_subset<-actors[actors$label%in%actors_subset2,]
rownames(actors_subset)<-c(1:(nrow(actors_subset)))
label2<-actors_subset$label
label<-actors_subset$label<-paste("Actor", c(rownames(actors_subset)[1:8],12))
labels<-cbind(label2, label)
rownames(labels)<-label2

## Calls
# Sender
newsender<-labels[calls_subset$sender,"label"]
names(newsender)<-c()
calls_subset$sender<-newsender

# Receiver
newreceiver<-labels[calls_subset$receiver,"label"]
names(newreceiver)<-c()
calls_subset$receiver<-newreceiver

# Time
CStime<-calls_subset$time
calls_subset$time<-calls_subset$time-(calls_subset$time[1]-1936225)

## Friendship
friendship_subset<-friendship[which(friendship$sender%in%actors_subset2 & friendship$receiver %in% actors_subset2),]
rownames(friendship_subset)<-c(1:nrow(friendship_subset))

# Sender
newfriendsender<-labels[friendship_subset$sender,"label"]
names(newfriendsender)<-c()
friendship_subset$sender<-newfriendsender

# Receiver
newfriendreceiver<-labels[friendship_subset$receiver,"label"]
names(newfriendreceiver)<-c()
friendship_subset$receiver<-newfriendreceiver


 # Time if we want a unique wave
 # friendship_subset$time<-rep(min(friendship_subset$time), times=nrow(friendship_subset))-(CStime[1]-1936225)
  
 friendship_subset$time<-friendship_subset$time-(CStime[1]-1936225)
# 
# # Adjacency (unique wave)
# friendship_edgelist<- cbind(friendship_subset$sender,friendship_subset$receiver)
# friend_net <- network(friendship_edgelist,matrix.type="edgelist") 
# summary(friend_net)
# friendship_subset_w1_adjacency<-as.sociomatrix(friend_net)
#isSymmetric.matrix(friendship_subset_w2_adjacency)


# Waves
friendship_subset_w1<-friendship_subset[friendship_subset$time%in%min(friendship_subset$time),]
friendship_subset_w2<-friendship_subset[friendship_subset$time%in%max(friendship_subset$time),]
rownames(friendship_subset_w2)<-c(1:nrow(friendship_subset_w2))

# Adjacency (1st wave)
friendship_edgelist_w1 <-data.frame(friendship_subset_w1$sender,friendship_subset_w1$receiver)
colnames(friendship_edgelist_w1)<-c("sender", "receiver")
friendship_edgelist_w1$sender<-as.numeric(gsub("Actor ","", friendship_edgelist_w1$sender))
friendship_edgelist_w1$receiver<-as.numeric(gsub("Actor ","", friendship_edgelist_w1$receiver))
friend_net_w1 <- network(friendship_edgelist_w1, matrix.type="edgelist", directed = T) 
network.vertex.names(friend_net_w1) <- paste("Actor", c(1:12))
friendship_subset_w1_adjacency<-as.sociomatrix(friend_net_w1)
friendship_subset_w1_adjacency<-friendship_subset_w1_adjacency[-c(9:11),-c(9:11)]

#isSymmetric.matrix(friendship_subset_w1_adjacency)

# friendship_edgelist_w2 <- cbind(friendship_subset_w2$sender,friendship_subset_w2$receiver)
# friend_net_w2 <- network(friendship_edgelist_w2, matrix.type="edgelist", directed = T) 
# friendship_subset_w2_adjacency<-as.sociomatrix(friend_net_w2)
#isSymmetric.matrix(friendship_subset_w2_adjacency)

# Network
# actors_subset <- defineNodes(nodes = actors_subset) 
# call.Network.subset <- defineNetwork(nodes = actors_subset, directed = T) #
# call.Network.subset <- linkEvents(x = call.Network.subset, changeEvent = calls_subset,   nodes = actors_subset)
# ex.calls_subset<-defineDependentEvents(events=calls_subset, nodes=actors_subset, defaultNetwork = call.Network.subset)


###################################################
### code chunk number 8: dataframe
###################################################
gender <- sample.int(2, 9, replace = T)
(actors <- data.frame(actors_subset,gender))


###################################################
### code chunk number 9: nodeset
###################################################
actors <- defineNodes(nodes = actors)  


###################################################
### code chunk number 10: compositionChangeEvents
###################################################
compositionChangeEvents <- data.frame(time = c(2484870, 2487266), node = 5, 
                                      replace = c(F, T))

head(compositionChangeEvents)


###################################################
### code chunk number 11: linkevents
###################################################
actors_ca <- linkEvents(x = actors, 
                    attribute = "present",
                    changeEvents = compositionChangeEvents)


###################################################
### code chunk number 12: datacalls
###################################################
#calls <- data.frame(time = 1:5, sender = c(1, 1:4), receiver = c(5:1))
calls_subset


###################################################
### code chunk number 13: definedepnetwork
###################################################
callNetwork <- defineNetwork(nodes = actors_ca, directed = T)


###################################################
### code chunk number 14: definedepnetwork
###################################################
callNetwork <- linkEvents(x = callNetwork, changeEvent = calls_subset, 
                          nodes = actors_ca)


###################################################
### code chunk number 15: friendshipnetwork
###################################################
friendship<-friendship_subset_w1
friendship.w1<-friendship_subset_w1_adjacency


###################################################
### code chunk number 16: definefriendshipnetwork
###################################################
friendshipNetwork <- defineNetwork(matrix =friendship.w1, nodes = actors_ca, 
                                   directed = T)


###################################################
### code chunk number 17: friendshipnetworkmatrix
###################################################
latex(friendship.w1,
              caption=paste("Friendship Sociomatrix Subset"),
              file="",
              #rowname=rownames(friendship.w1),
              #colname=colnames(friendship.w1),
              size = "footnotesize",
              #where='!H', 
              label=paste0("tab:sociomat"),
              #ctable=F,
              #first.hline.double = F,
              na.blank = TRUE,
              longtable=TRUE
              )


###################################################
### code chunk number 18: histogram
###################################################
par(mar = c(4, 4, 1, 1))
dval <- as.sociomatrix(friendship.w1,attrname=colnames(friendship.w1)) 
d.val<-network(dval)
gplot(dval,gmode="graph", vertex.col="lightblue", vertex.cex=2.2,
      label.cex=0.55,label.pos=5,displaylabels=TRUE)
box()


###################################################
### code chunk number 19: friendshipChange
###################################################
friendship_subset_w2[3:4,]$replace<-c(0,0)
friendshipChange <- friendship_subset_w2[1:4,]


###################################################
### code chunk number 20: friendshipChangeshow
###################################################
friendshipChange


###################################################
### code chunk number 21: friendshipnetwork2
###################################################
friendshipNetwork <- linkEvents(x = friendshipNetwork,
                                nodes = actors_ca,
                                changeEvents = friendshipChange)


###################################################
### code chunk number 22: seasons (eval = FALSE)
###################################################
## seasons <- defineGlobalAttribute(data.frame(time = 1:12, replace = 1:12))


###################################################
### code chunk number 23: depcalls
###################################################
callsDependent <- defineDependentEvents(events = calls_subset, 
                                        nodes = actors_ca, 
                                        defaultNetwork = callNetwork)


###################################################
### code chunk number 24: modelRepreparation
###################################################
rm(list=ls())

data("Social_Evolution")
 # 1
callNetwork <- defineNetwork(nodes = actors, directed = T)
 # 2
callNetwork <- linkEvents(x = callNetwork, changeEvent = calls, nodes = actors)
 # 3
callsDependent <- defineDependentEvents(events = calls, nodes = actors, 
                                        defaultNetwork = callNetwork)


###################################################
### code chunk number 25: modelspecif
###################################################
formula_rate<-(callsDependent ~ alter(actors$gradeType) 
               + same(actors$floor))


###################################################
### code chunk number 26: modelspecifb
###################################################
formula_choice<-(callsDependent ~ inertia(callNetwork, window=3600) 
                 + recip(callNetwork, ignoreRep=T) 
                 + trans(callNetwork, weighted=T))


###################################################
### code chunk number 27: modelspecif1
###################################################
(DynamM_est_rate<- estimate(formula_rate, model="DyNAM", 
                            subModel="rate",
                            preprocessingOnly = T))


###################################################
### code chunk number 28: modelspecif3a (eval = FALSE)
###################################################
## (DynamM_est_rate<- estimate(formula_rate, model="DyNAM", 
##                             subModel="rate"))


###################################################
### code chunk number 29: modelspecif3b
###################################################
(DynamM_est_rate<- estimate(formula_rate, model="DyNAM", subModel="rate", silent=T))


###################################################
### code chunk number 30: modelspecif4a (eval = FALSE)
###################################################
## (DynamM_est_choice<- estimate(formula_choice, model="DyNAM", 
##                               subModel="choice"))


###################################################
### code chunk number 31: modelspecif4b
###################################################
(DynamM_est_choice<- estimate(formula_choice, model="DyNAM", subModel="choice", silent=T))


