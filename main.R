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

dist_voisins=function(vecteur,data){
  apply(data[,-ncol(data)],1,distance,x=vecteur)
}

kppv=function(vecteur,k,data){
  dist=dist_voisins(vecteur,data)
  order(dist)[1:k]
}

classerKPPV=function(vecteur, k, data) {
  v=kppv(vecteur,k,data)
  classes=data[v,ncol(data)]
  t=table(classes)
  names(t)[which.max(t)]
}

erreurKPPV<-function(k,data){
  pred<-sapply(1:nrow(data),function(i)classerKPPV(data[i,-ncol(data)],k,data[-i]))
  classes<-data[,ncol(data)]
  return(1-sum(pred==classes)/nrow(data))
}


train<-VCorpus(DirSource("training",recursive=TRUE))
trainN<-mrPropre(train)
mat<-DocumentTermMatrix(trainN)

vocab<-findFreqTerms(mat,lowfreq=200)
mat200<-DocumentTermMatrix(trainN,control=list(dictionary=vocab))
M<-as.matrix(mat200)
classes<-c(rep(0,150),rep(1,150),rep(2,150),rep(3,150),rep(4,150),rep(5,150),rep(6,150))
M<-cbind(M,classes)
M<-erreurKPPV(1050,M)

#classer<-function(fic){
  #corpus<-Vcorpus(URISource(fic))
  #corpusN<-mrPropre(corpus)
  #DocumentTerm avec vocab
  #Classer KPPV
  #Interpreter la classe
#}