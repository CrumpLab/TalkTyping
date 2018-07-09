#Amazon turk paragraph task analysis
#requires outlier functions at the bottom
AllWPM<-matrix(0,ncol=2,nrow=48)
for(sub in 1:48){
  test<-scan(file="external_hit.results", what = "character", sep = " ", skip=(0+sub), nlines=1)
  data<-unlist(strsplit(test,split=":"))
  if (length(data)==20){
    print(c(sub,length(data)))
  #13 is paragraph 17, 21, 25, 29 -5 total paragraphs
  #14 is times
  #15 is characters typed
  #16 is correct
  AllRTs<-c()
  AllChars<-c()
  WPM<-c()
  for(i in 0:1){
    RTs<-c()
    Chars<-c()
    RTindex<-14+i*4
    Charindex<-15+i*4
    RTs<-unlist(strsplit(data[RTindex],split=","))
    RTs<-as.numeric(RTs[3:length(RTs)])
    #WPM[i+1]<-(RTs[length(RTs)]-RTs[1])/1000/length(RTs)/5*60
    WPM[i+1]<-60/((RTs[length(RTs)]-RTs[1])/1000/(length(RTs)/5))
  }
    AllWPM[sub,]<-WPM
  }
}
    

dfWPM<-data.frame(AllWPM)
colnames(dfWPM)[1]<-"SayLetters"
colnames(dfWPM)[2]<-"SayWords"
dfWPM[dfWPM==0]<-NA
dfWPM<-na.omit(dfWPM)
colMeans(dfWPM)
t.test(dfWPM$SayLetters,dfWPM$SayWords,paired=TRUE)
quartz(width=1.8,height=1.5)
plot(dfWPM$SayWords,dfWPM$SayWords-dfWPM$SayLetters, xlab="Say Words WPM", ylab="Words-Letters WPM")
par(mar=c(4,4,1.5,4)-1, cex=.65,mgp=c(1.5,.5,0))
abline(lm((dfWPM$SayWords-dfWPM$SayLetters)~dfWPM$SayWords), col="red")


