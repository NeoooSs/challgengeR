library(tm)

merge=function(t){
  PlainTextDocument(paste(t,collapse=""))
}

removeScript=function(t){
  sp=strsplit(as.character(t), "<script")[[1]]
  vec=sapply(1:length(sp),function(i) gsub(sp[i], pattern=".*</script>", replace=" "))
  PlainTextDocument(paste(vec,collapse=""))
}

removeBalises=function(t){
  t1=gsub("<[^>]*>", " ", t)
  PlainTextDocument(gsub("[ \t]+"," ",t1))
}

mrPropre=function(corpus){
  corpus=tm_map(corpus,merge)
  corpus=tm_map(corpus,content_transformer(tolower))
  corpus=tm_map(corpus,removeScript)
  corpus=tm_map(corpus,removeBalises)
  corpus=tm_map(corpus,removeWords,words=stopwords('en'))
  corpus=tm_map(corpus,removeNumbers)
  corpus=tm_map(corpus,removePunctuation)
  corpus=tm_map(corpus,stemDocument,language='en')
}

distance=function(x,y){
  return(sqrt(sum((as.numeric(x)-as.numeric(y))^2)))
}

dist_voisins<-function(vecteur, data){
  return(apply(data, 1, distance, x = vecteur))
}

kppv=function(vecteur,k,data){
  dist=dist_voisins(vecteur,data)
  return(order(dist)[1:k])
}

classerKPPV=function(vecteur, k, data) {
  v=kppv(vecteur,k,data)
  classes=data[v,ncol(data)]
  t=table(classes)
  return (names(t)[which.max(t)])
}

erreurKPPV=function(k, data) {
  pred=sapply(1:nrow(data), function(i)classerKPPV(data[i,-ncol(data)],k,data[-i,]))
  return (1-sum((pred==(data[,ncol(data)]))/nrow(data)))
}


train=VCorpus(DirSource("training",recursive=TRUE))
trainN=mrPropre(train)
mat<-DocumentTermMatrix(corpusN)
vocab=findFreqTerms(mat,lowfreq=250)
mat200=DocumentTermMatrix(trainN,control=list(dictionary=vocab))
M=as.matrix(mat200)
classes<-c(rep('accueil',150),rep('blog',150),rep('commerce',150),rep('FAQ',150),rep('home',150),rep('liste',150),rep('recherche',150))
M<-cbind(M,classes)

write.csv(M,'data.csv')


classer <- function(fic) {
  mat <- as.matrix(DocumentTermMatrix(mrPropre(VCorpus(URISource(fic))), list(dictionary=vocab)))
  return (classerKPPV(mat, 4, data))
}

classer=function(fic){
  corpus=VCorpus(URISource(fic))
  corpusN=mrPropre(corpus)
  matrix=as.matrix(DocumentTermMatrix(corpusN,list(dictionary=vocab)))
  return (classerKPPV(mat,3,fic))
}


