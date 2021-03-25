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

mrPropre<-function(corpus){
  corpus<-tm_map(corpus,merge)
  corpus<-tm_map(corpus,content_transformer(tolower))
  corpus<-tm_map(corpus,removeScript)
  corpus<-tm_map(corpus,removeBalises)
  corpus<-tm_map(corpus,removeWords,words=stopwords('en'))
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,removePunctuation)
  corpus<-tm_map(corpus,stemDocument,language='en')
}

erreur_kppv <- function(k,data){
  predictions <- vapply(seq_len(nrow)) function(i){
    classerKPPV(data[i, -ncol(data)], k, data[-i,])
  }, numeric(1L)
  sum(predictions != data[, ncol(data)])
}


train<-VCorpus(DirSource("training",recursive=TRUE))
trainN<-mrPropre(train)
mat<-DocumentTermMatrix(trainN)

vocab<-findFreqTerms(mat,lowfreq=200)
mat200<-DocumentTermMatrix(trainN,control=list(dictionary=vocab))
M<-as.matrix(mat200)
classes<-c(rep(0,150),rep(1,150),rep(2,150),rep(3,150),rep(4,150),rep(5,150),rep(6,150))
M<-cbind(M,classes)
M<-erreur_kppv(1050,M)

#classer<-function(fic){
  #corpus<-Vcorpus(URISource(fic))
  #corpusN<-mrPropre(corpus)
  #DocumentTerm avec vocab
  #Classer KPPV
  #Interpreter la classe
#}