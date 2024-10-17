makeresponse<-function(x) { #see https://github.com/ben-domingue/imv/blob/main/R/imv_mirt.R
    ##make IR matrix
    nms<-unique(x$item)
    if (all(nms %in% 1:length(nms))) x$item<-paste("item_",x$item,sep='')
    ##make response matrix
    id<-unique(x$id)
    L<-split(x,x$item)
    out<-list()
    for (i in 1:length(L)) {
        z<-L[[i]]
        index<-match(z$id,id)
        resp<-rep(NA,length(id))
        resp[index]<-z$resp
        out[[i]]<-resp
    }
    resp<-do.call("cbind",out)
    resp<-data.frame(resp)
    names(resp)<-names(L)
    resp$id<-id

    #nr<-apply(resp,2,function(x) length(table(x)))
    #resp<-resp[,nr>1]
    #resp<-resp[rowSums(!is.na(resp))>1,]

    resp
}

library(rlang)
library(mirt)
library(dplyr)
# load save of by-task pre-processed data + model specifications
load("task_data_args.RData")

##
imv_local<-function(args1,
                    args2,
                    nfold=5,
                    x,
                    fscores.options=(list(method="MAP"))
                    )
{
    library(imv)
    ## dichotomous responses that have means <98% or >2%
    ## at least 10 responses in both categories
    ## and people with at least 5 non-NA responses
    cm<-colMeans(x,na.rm=TRUE)
    x<-x[,cm>.02 & cm<.98]
    tab<-t(apply(x,2,table))
    x<-x[,tab[,1]>=10 & tab[,2]>=10]
    rs<-rowSums(!is.na(x))
    x<-x[rs>=5,]
    ##update args
    new.ni<-ncol(x)
    old.ni<-length(args1$guess)
    args1$guess<-rep(args1$guess[1],new.ni)
    args1$model<-gsub(old.ni,new.ni,args1$model)
    old.ni<-length(args2$guess)
    args2$guess<-rep(args2$guess[1],new.ni)
    args2$model<-gsub(old.ni,new.ni,args2$model)
    ##make long data
    id<-rownames(x)
    L<-list()
    for (i in 1:ncol(x)) L[[i]]<-data.frame(id=id,item=colnames(x)[i],resp=x[,i])
    x<-data.frame(do.call("rbind",L))
    ##remove NA
    x<-x[!is.na(x$resp),]
    kk<-length(unique(x$resp))
    if (!(kk==2)) stop("only works for dichotomous responses")
    ##
    x$group<-sample(1:nfold,nrow(x),replace=TRUE)
    ##
    mirt_call <- function(df, args) inject(mirt(data = df, technical=list(NCYCLES=10000),!!!args))
    om<-numeric()
    for (i in 1:nfold) {
        ##get training data, estimate models
        train<-makeresponse(x[x$group!=i,])
        id<-train$id
        train$id<-NULL
        mm1<-mirt_call(train,args1)
        mm2<-mirt_call(train,args2)
        ##get ability estimates
        th1<-do.call("fscores",c(list(object=mm1),fscores.options))
        th2<-do.call("fscores",c(list(object=mm2),fscores.options))
        ##get fitted values
        ll<-list()
        items<-unique(x$item)
        for (j in 1:length(items)) {
            item<-items[j]
            it<-extract.item(mm1,item)
            pp1<-probtrace(it,th1)
            it<-extract.item(mm2,item)
            pp2<-probtrace(it,th2)
            ll[[j]]<-data.frame(id=id,item=item,pr1=pp1[,2],pr2=pp2[,2])
        }
        y<-data.frame(do.call("rbind",ll)) #dataframe of predictions from models derived from training data
        ##get test data
        test<-x[x$group==i,]
        y<-merge(test,y,all.x=TRUE)
        ##compute imv
        om[i]<-imv.binary(y$resp,y$pr1,y$pr2)
    }
    return(mean(om))
}


rows<-seq(1,28,by=4)
om.out<-list()
for (i in rows[-c(2,3)]) { #the one with polytomous responses
                                        # data as passed to mirt
    df <- task_data_args$data_prepped[[i]]
    om<-numeric()
    for (j in c(1,3)) {
        print(c(i,j))
        ##
        args1 <- list(itemtype = task_data_args$item_type[[i]],
                      model = task_data_args$model_str[[i]],
                      guess = task_data_args$guess[[i]])
        args2 <- list(itemtype = task_data_args$item_type[[i+j]],
                      model = task_data_args$model_str[[i+j]],
                      guess = task_data_args$guess[[i+j]])
        om[j]<-imv_local(args1=args1,
                  args2=args2,
                  x=df,
                  nfold=4)
    }
    om.out[[as.character(i)]]<-om
    print(task_data_args$task_id[i])
    print(om.out)
}
