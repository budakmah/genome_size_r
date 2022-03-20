peaks<-function(x)
{
min1<-x[diff(sign(diff(x[1:200,2])))==+2,][1,1]+1
min2<-x[diff(sign(diff(x[1:200,2])))==+2,][2,1]+1
max<-x[diff(sign(diff(x[1:200,2])))==-2,][1,1]+1
return(c(min1,min2,max))
}

gnome<-function(x)
{
data<-read.table(x)
xl<-peaks(data)[2]+50
xs<-peaks(data)[1]
g<-sum(as.numeric(data[peaks(data)[1]:dim(data)[1],1]*data[peaks(data)[1]:dim(data)[1],2]))/peaks(data)[3]
s<-sum(as.numeric(data[peaks(data)[1]:peaks(data)[2],1]*data[peaks(data)[1]:peaks(data)[2],2]))/peaks(data)[3]
df <- data.frame(genome_size=g,single_copy=s,single_copy_ratio= round(s/g*100,digits = 0))
plot(dpois(1:xl,peaks(data)[3])*s, type='l', lty=2, col="green")
lines(data[1:xl,],type= "l")
points(data[diff(sign(diff(data[1:200,2])))==+2,][1,1]+1,data[diff(sign(diff(data[1:200,2])))==+2,][1,2], pch=19,col=2)
text(data[diff(sign(diff(data[1:200,2])))==+2,][1,1]+1,data[diff(sign(diff(data[1:200,2])))==+2,][1,2]+10000,peaks(data)[1])
points(data[diff(sign(diff(data[1:200,2])))==+2,][2,1]+1,data[diff(sign(diff(data[1:200,2])))==+2,][2,2], pch=19,col=2)
text(data[diff(sign(diff(data[1:200,2])))==+2,][2,1]+1,data[diff(sign(diff(data[1:200,2])))==+2,][2,2]+10000,peaks(data)[2])
points(data[diff(sign(diff(data[1:200,2])))==-2,][1,1]+1,data[diff(sign(diff(data[1:200,2])))==-2,][1,2], pch=19,col=2)
text(data[diff(sign(diff(data[1:200,2])))==-2,][1,1]+1,data[diff(sign(diff(data[1:200,2])))==-2,][1,2]-10000,peaks(data)[3])
polygon(data[xs:(peaks(data)[2]-50),],col = rgb(0.78, 0.89, 1, alpha = 0.6))
return(df)
}