flatfile2 <-read.csv("flatfile.csv")

finals <-read.csv("FinalsResults.csv")

flatfile2<-merge(flatfile2, finals, by= "user_id",  all.x=TRUE)
