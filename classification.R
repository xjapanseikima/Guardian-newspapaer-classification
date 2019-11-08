library(stringr)
library(dplyr)
library(tokenizers)
library(SnowballC)
#Data Collecct
world_news<-get_guardian("",section="world",from.date="2019-06-13", to.date="2019-08-20",api.key="212d23d3-c7b2-4273-8f1b-289a0803ca4b")
sport<-get_guardian("",section="sport",from.date="2019-06-01", to.date="2019-08-20",api.key="212d23d3-c7b2-4273-8f1b-289a0803ca4b")
technology<-get_guardian("",section="technology",from.date="2018-04-20", to.date="2019-08-20",api.key="212d23d3-c7b2-4273-8f1b-289a0803ca4b")
world_news<-get_guardian("",section="world",from.date="2019-06-13", to.date="2019-08-20",api.key="212d23d3-c7b2-4273-8f1b-289a0803ca4b")
business<-get_guardian("",section="business",from.date="2019-02-20", to.date="2019-07-20",api.key="212d23d3-c7b2-4273-8f1b-289a0803ca4b")
football<-get_guardian("",section="football",from.date="2019-05-20", to.date="2019-08-20",api.key="212d23d3-c7b2-4273-8f1b-289a0803ca4b")
science<-get_guardian("",section="science",from.date="2018-01-10", to.date="2019-08-20",api.key="212d23d3-c7b2-4273-8f1b-289a0803ca4b")
total<-bind_rows(football, science,sport, technology,world_news,business)
names(total)
#Data Clean
cleanData<-total[,c("id","sectionId","webTitle","wordcount","body")]
cleanData$body=gsub("<.*?>", "",cleanData$body)
x=tolower(cleanData$body)
cleanData$body=x
cleanData$body=gsub("[^a-z,^ ]","",cleanData$body)
cleanData$body=gsub("\\s+"," ",cleanData$body)
#title Clean
cleanData$webTitle=gsub("[^a-z,^A-Z ]","",cleanData$webTitle)
cleanData$webTitle=gsub("\\s+"," ",cleanData$webTitle)
#Random Data
Y=sample(1:6000,1,replace=T)
print(cleanData$body[Y])
#Tokenization
cleanData$body = removeWords(cleanData$body, stopwords("english"))
testmyCorpus<-Corpus(VectorSource(cleanData$body))
testmyCorpus = tm_map(testmyCorpus, PlainTextDocument)
testmyCorpus = tm_map(testmyCorpus, removePunctuation)
w<-DocumentTermMatrix(testmyCorpus,list(global = c(1, Inf)))
w <- removeSparseTerms(testTDF, 0.99)
View(x)
inspect(w)
View(w)

# load the library
library(mlbench)
library(caret)
library(e1071)
x<-(as.matrix(w))# trainning set
train<-floor(0.8*nrow(x))
View(x[1,1:train])
df2 = cor(x)
hc = findCorrelation(df2, cutoff=0.8) # putt any value as a "cutoff" 
hc = sort(hc)
reduced_Data = x[,-c(hc)]
y=cleanData$sectionId
model=naiveBayes(ability ~ defender,y, data=reduced_Data)
print(model)

print(reduced_Data[,1:80])
yhat<-predict(model)

#NBclassifier=naiveBayes()
