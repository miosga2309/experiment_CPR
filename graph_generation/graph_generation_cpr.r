library(ggplot2)

kwh_pool <- c(4250,4000,4200,3950,4100,4250,3900,3500,3100,2800,
              2500,2250,1850,1650,1500,1200,900,750)
eff = 4250

n = 6 # fix
r = 18 # variable, depends on pre-test results

# graph-generating function
f <- function() {
  # generate data
  # inefficient group norm, stage 1
  sub_ine1 = 1
  while (mean(sub_ine1) > 186 | mean(sub_ine1) < 184) {
    sub_ine1 = round(rnorm(5,185,15))}
  ine1 = c(0,sub_ine1)
  # efficient group norm, stage 1
  sub_eff1 = 1
  while (mean(sub_eff1) > 323 | mean(sub_eff1) < 321) {
    sub_eff1 = round(rnorm(5,322,15))}
  eff1 = c(0,sub_eff1)
  # inefficient group norm, stage 2
  sub_ine2 = 1
  while (mean(sub_ine2) > 267 | mean(sub_ine2) < 263) {
    sub_ine2 = round(rnorm(6,265,15))}
  # efficient group norm, stage 2
  sub_eff2 = 1
  while (mean(sub_eff2) > 267 | mean(sub_eff2) < 263) {
    sub_eff2 = round(rnorm(6,265,15))}
  # inefficient group norm, stage 3
  sub_ine3 = 1
  while (mean(sub_ine3) > 359 | mean(sub_ine3) < 357) {
    sub_ine3 = round(rnorm(6,358,15))}
  # efficient group norm, stage 3
  sub_eff3 = 1
  while (mean(sub_eff3) > 172 | mean(sub_eff3) < 170) {
    sub_eff3 = round(rnorm(6,171,15))}
  # put into dataframe
  df <- data.frame(1:18,c(ine1,sub_ine2,sub_ine3),c(eff1,sub_eff2,sub_eff3),kwh_pool)
  names(df) <- c("r","ine","eff","kwh")
  return(df)
}

# check results
res <- f(6,18)
resa <- reshape2::melt(res, id.vars="r")
ggplot(resa, aes(x=r, y=value, color=variable)) + 
  geom_line()

# save data to df (if suitable)
#write.csv(resa,"graphdata_cpr.csv")
#resa <- read.csv("graphdata_cpr.csv")

# reshape data for plotting
plotdf <- as.data.frame(cbind(c(res$r,res$r),c(rep("ine",18),rep("eff",18)),
                c(res$ine,res$eff),c(res$kwh,res$kwh)))
names(plotdf) <- c("r","var","wh","pool")
plotdf[,"r"] <- as.numeric(as.character(plotdf[,"r"]))
plotdf[,"wh"] <- as.numeric(as.character(plotdf[,"wh"]))
plotdf[,"pool"] <- as.numeric(as.character(plotdf[,"pool"]))

#write.csv(plotdf,"plotdf_cpr.csv")
plotdf <- read.csv("plotdf_cpr.csv")

ggplot(plotdf[c(2:18,20:36),], aes(x=r, y=kwh, color=var)) +
  ylab("Wh") +
  geom_line()
ggsave("groupnorm.jpeg", dpi=300)

ggplot(plotdf, aes(x=r, y=pool)) +
  ylab("Wh") +
  geom_line()
ggsave("pool.jpeg", dpi=300)

# create plots
for (i in 1:dim(plotdf)[1]) {
  # open
  a <- paste(paste(plotdf[i,"var"],plotdf[i,"r"],sep=""),"_cpr.jpeg",sep="")
  pth <- sprintf("graphs/%s", a)
  jpeg(pth,width = 800, height = 600)
  
  # plotting
  par(mar=c(2, 5, 2, 2.1))
  layout(matrix(c(1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,2,1,2,1,2
                  ,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2), nrow = 20, ncol = 2, byrow = TRUE))
  barplot(height=plotdf[i,"pool"], names="", 
          col="#66B2FF",
          ylim=c(0,5000),
          #xlab="categories", 
          #ylab="Wh"
          cex.axis=2, cex.names=2, cex.main=2
          #main="Available Wh",
          #sub=sprintf("Other people average comsumption: %s",plotdf[i,5])
  )
  #axis(2,at=seq(1,11,1))
  mtext(side=2, line=3, "Wh",cex=1.2)
  mtext(side=3,"State of battery charge",line=0,cex=1.5)
  barplot(height=plotdf[i,"kwh"]*5, names="", 
          col="#A0A0A0",
          ylim=c(0,3000),
          #xlab="categories", 
          #ylab="Wh"
          cex.axis=2, cex.names=2
          #main="CO2 Output"
  )
  #axis(2,at=seq(1,4,1))
  mtext(side=2, line=3, "Wh",cex=1.2)
  mtext(side=3,"Total power usage of the others \n in the previous round",line=20,cex=1.5)
  
  # close
  dev.off() }