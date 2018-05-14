
#SARSWAT ORIGINAL NOTES

spectral_clustering=function(W,k)
{
 n <- nrow(W)
 D <- diag(rowSums(W))
 L <- diag(rowSums(W)^(-1/2))%*%(D-W)%*%diag(rowSums(W)^(-1/2)) #equation of Lsym
 G <- eigen(L)$vectors[,(n-k+1):n] 
 H <- diag(rowSums(W)^(-1/2))%*%G 
 cluster_vector <- kmeans(H,k)$cluster
 for(i in 1:k) print(which(cluster_vector==i))
}
W <- matrix(0,nr=11,nc=11)
k <- 3

W[1,c(2,11)]=1
W[2,c(1,3,11)]=1
W[3,c(2,10,4)]=1
W[4,c(3,10,5)]=1
W[5,c(10,4,6)]=1
W[6,c(5,7,9)]=1
W[7,c(8,6)]=1
W[8,c(7,9)]=1
W[9,c(6,8)]=1
W[10,c(3,4,5)]=1
W[11,c(1,2)]=1

W
spectral_clustering(W,k=2)

Q2 <- (5/14 - (7/14)^2) + (7/14 - (9/14)^2)
Q2

spectral_clustering(W,k=3)
Q3 <- (5/14 - (7/14)^2) + (3/14 - (4/14)^2 ) + (4/14 - (5/14)^2)
Q3

spectral_clustering(W,k=4)
Q4 <- (1/14 - (5/14)^2) + (3/14 - (6/14)^2) + (2/14 - (4/14)^2) + (3/14 - (4/14)^2)
Q4


W=matrix(c(0,0.5,0.1,0,0,0,0,
           0.5,0,0.3,0,0,0,0,
           0.1,.3,0,.9,0,0,0,
           0,0,0.9,0,.2,0,0.1,
           0,0,0,.2,0,.5,.7,
           0,0,0,0,.5,0,0.6,
           0,0,0,0.1,.7,0.6,0),nr=7)






