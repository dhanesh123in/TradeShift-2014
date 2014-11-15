rm(list=ls())
gc()

sprune<-function(fvar,levs=NA,supp=5000) {
	if (length(levs)<=1) {
		xx<-table(fvar)
		levs<-row.names(xx)[which(xx>supp)]
	}
	fvarn<-rep("Others",length(fvar))
	for (ll in levs) {
		fvarn[which(fvar==ll)]<-ll
	}
	fvarn<-as.factor(as.character(fvarn))
	fvarn
}

#library(randomForest)

#This takes a while
train_dat<-read.csv("train.csv")
test_dat<-read.csv("test.csv")
trainlabels_dat<-read.csv("trainLabels.csv")

train_dat[is.na(train_dat)]<-"None"
test_dat[is.na(test_dat)]<-"None"

ldvars<-c("x3","x4","x34","x35","x61","x64","x65","x91","x94","x95")

# for (ldvar in ldvars) {
	# train_dat[,ldvar]<-sprune(train_dat[,ldvar],supp=5000)
	# test_dat[,ldvar]<-sprune(test_dat[,ldvar],levs=levels(train_dat[,ldvar]))
# }

for (ldvar in ldvars) {
	train_dat[,ldvar]<-sprune(train_dat[,ldvar],supp=7500)
	test_dat[,ldvar]<-sprune(test_dat[,ldvar],levs=levels(train_dat[,ldvar]))
}

for (ldvar in ldvars) { print(c(ldvar,length(levels(train_dat[,ldvar])),length(levels(test_dat[,ldvar]))))}

trainlabels_dat<-as.data.frame(apply(trainlabels_dat,2,as.factor))
train_index<-sample(1:nrow(train_dat),0.8*nrow(train_dat))
test_index<-setdiff(1:nrow(train_dat),train_index)

rf<-list()
set.seed(123)
x=train_dat[train_index,2:ncol(train_dat)]
xtest=train_dat[test_index,2:ncol(train_dat)]
stestx<-test_dat[,2:ncol(test_dat)]
# for(i in 1:1){

	# y=trainlabels_dat[train_index,i+1]
	# ytest=trainlabels_dat[test_index,i+1]
	# nmin=sum(trainlabels_dat[train_index,i+1]==1)
	# rf[[i]]<-randomForest(x=x,y=y,xtest=xtest,ytest=ytest,ntree=20,sampsize=c(nmin,nmin),strata=y,do.trace=TRUE,nodesize=10,keep.forest=TRUE)
# }

rm(list=c("train_dat"))
gc()

library(caret)

# fitControl <- trainControl(## 5-fold CV
                           # method = "repeatedcv",
                           # number = 1,
                           # repeated 3 times
                           # repeats = 1)
fitControl <- trainControl(## OOB
                           method = "cv",p=0.9)
						   
gbmGrid <-  expand.grid(interaction.depth = c(5,10),
                        n.trees = c(5,10,15)*10,
                        shrinkage = 0.1)

nrow(gbmGrid)

#gbmFit2<-list()
trainpreds<-list()
testpreds<-list()
stestprobs<-list()
cmtr<-list()
cmte<-list()


for(i in 1:33) {
	print(paste(c("=================",i,"=====================")));
	
	y=trainlabels_dat[train_index,i+1]
	ytest=trainlabels_dat[test_index,i+1]
	
	#Undersampling of majority class except y33
	set.seed(123)
	if (i!=14) {
		y1samp<-which(y=='1')
		if (length(y1samp)>50000) {
			y1samp<-sample(y1samp,50000,replace=T)
		}
		y0samp<-sample(which(y=='0'),length(y1samp),replace=T)
		xn<-x[c(y0samp,y1samp),]
		yn<-y[c(y0samp,y1samp)]
		
		
		set.seed(825)
		gbmFit2 <- train(x = xn, y=yn,
						 method = "gbm",
						 trControl = fitControl,
						 verbose = TRUE,
						 ## Now specify the exact models 
						 ## to evaludate:
						 tuneGrid = gbmGrid)
		trainpreds[[i]]<-predict(gbmFit2,newdata=xn)
		testpreds[[i]]<-predict(gbmFit2,newdata=xtest)
		cmtr[[i]]<-confusionMatrix(trainpreds[[i]],yn)
		cmte[[i]]<-confusionMatrix(testpreds[[i]],ytest)
		stestprobs[[i]]<-extractProb(list(gbm=gbmFit2),unkX=stestx)$X1
	} else {
		stestprobs[[i]]<-rep(0.0,nrow(stestx))
	}
}
for (i in 1:33) {print(cmtr[[i]]$byClass)}
for (i in 1:33) {print(cmte[[i]]$byClass)}

for (i in 1:33) {
	print (i)
	print (summary(stestprobs[[i]]))
}

save(stestprobs,file="R-gbm-samp-try3.Rdata")
##Writing output
res<-read.csv("sampleSubmission.csv")
nrespt<-nrow(res)/33

#for (j in 1:nrespt) {
	for (i in 1:33) {
		res$pred[i+33*(0:(nrespt-1))]<-stestprobs[[i]][1:nrespt]
	}
#}

summary(res)

write.csv(res,"R-gbm-samp-try3.csv",row.names=FALSE)

