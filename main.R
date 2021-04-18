data=read.csv('data.csv')
source('mrPropre.r')
source('KPPV.r')

classer=function(fic){
  corpus=VCorpus(URISource(fic))
  corpusN=mrPropre(corpus)
  matrix=as.matrix(DocumentTermMatrix(corpusN,list(dictionary=vocab)))
  return (classerKPPV(matrix,3,data))
}