library(tm)

merge<-function(t){
  PlainTextDocument(paste(t,collapse=""))
}

removeScript<-function(t){
  sp<-strsplit(as.character(t), "<script")[[1]]
  vec<-sapply(1:length(sp),function(i) gsub(sp[i], pattern=".*</script>", replace=" "))
  PlainTextDocument(paste(vec,collapse=""))
}

removeBalises<-function(t){
  t1<-gsub("<[^>]*>", " ", t)
  PlainTextDocument(gsub("[ \t]+"," ",t1))
}

nettoyage1<-function(corpus){
  Vcorpus<-tm_map(corpus,merge)
  Vcorpus<-tm_map(corpus,content_transformer(tolower))
  Vcorpus<-tm_map(corpus,removeScript)
  Vcorpus<-tm_map(corpus,removeBalises)
}

nettoyage2<-function(corpus)
{
  Vcorpus<-tm_map(corpus,removeWords,words=stopwords('en'))
  Vcorpus<-tm_map(corpus,removeNumbers)
  Vcorpus<-tm_map(corpus,removePunctuation)
  Vcorpus<-tm_map(corpus,stemDocument,language='en')
}

train<-VCorpus(DirSource("training",recursive=TRUE))
trainN<-nettoyage1(train)
trainN
trainN<-nettoyage2(trainN)