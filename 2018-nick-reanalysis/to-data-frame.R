source(file="cleaning-functions.R")

library(dplyr)

######## GET E1A Data ##################
all.data<-scan(file="E1A/raw-data/raw-data.txt", what = "character", sep = "\n")

data<-c()

for (i in 1:length(all.data)){
  print(i)
  sub_data<-create_df_mTurk(all.data[i],i)
  
  data<-rbind(data,sub_data)
}

data$subject<-as.factor(data$subject)
data$word_inDictionary<-as.factor(data$word_inDictionary)
data<-data%>%
  mutate(word_ACC = if_else(word_inDictionary == "TRUE",1,0))


save(data,file="E1A/data.Rda")



####### GET E1B Data ###################
files<-list.files(path="E1B/raw-data/")

# Key for tab seperated data
key<-list()
key$length<-5

key$label<-c(1,18,30,42,54)
key$paragraph<-c(4,21,33,45,57)
key$response<-c(8,25,37,49,61)
key$timestamp<-c(12,29,41,53,65)

key$Qdata<-c(14,15,16,17)


data<-c()
qData_E1B<-c()

  
for(i in 1:length(files)){
    subject  <- gsub(".txt","",files[i])
    path     <- paste0("E1B/raw-data/", files[i])
    raw.data <- scan(file=path, what = "character", sep="\t")
  
    
    temp.qData<-data.frame("E1B",subject,raw.data[14],raw.data[15],raw.data[16],raw.data[17])
    qData_E1B<-rbind(qData_E1B,temp.qData)
    
    
    if(length(raw.data) == 65){
      sub_data<-create_dataframe(raw.data = raw.data, 
                                 subject = subject,
                                 key = key)
      
      data<-rbind(data,sub_data)  
    } else {
      message("skipped subject ", subject, " error in data")
    }
    
}



data$subject<-as.factor(data$subject)
data$word_inDictionary<-as.factor(data$word_inDictionary)
data<-data%>%
  mutate(word_ACC = if_else(word_inDictionary == "TRUE",1,0))

data$talk<-""
data$type<-""

data[data$condition == "TalkLetter",]$talk<-"talk"
data[data$condition == "SilentLetter",]$talk<-"silent"
data[data$condition == "TalkWord",]$talk<-"talk"
data[data$condition == "SilentWord",]$talk<-"silent"

data[data$condition == "TalkLetter",]$type<-"letter"
data[data$condition == "SilentLetter",]$type<-"letter"
data[data$condition == "TalkWord",]$type<-"word"
data[data$condition == "SilentWord",]$type<-"word"

data$talk<-as.factor(data$talk)
data$type<-as.factor(data$type)




save(data,file="E1B/data.Rda")
save(qData_E1B,file="E1B/q-data.Rda")



####### GET E2 Data ###################
files<-list.files(path="E2/data/")

# Key for tab seperated data
key<-list()
key$length<-6

key$label     <-c(1, 16, 31, 46, 61, 76)
key$paragraph <-c(4, 19, 34, 49, 64, 79)
key$response  <-c(8, 23, 38, 53, 68, 83)
key$timestamp <-c(12,27, 42, 57, 72, 87)

key$Qdata     <-c(92, 93, 94, 95)


data<-c()
qData_E2<-c()

for(i in 1:length(files)){
  subject  <- gsub("random.txt","",files[i])
  path     <- paste0("E2/data/", files[i])
  raw.data <- scan(file=path, what = "character", sep="\t")
  
  temp.qData<-data.frame("E2",subject,raw.data[92],raw.data[93],raw.data[94],raw.data[95])
  qData_E2<-rbind(qData_E2,temp.qData)
  
  if(length(raw.data) == 95){
    sub_data<-create_dataframe(raw.data = raw.data, 
                               subject = subject,
                               key = key)
    
    data<-rbind(data,sub_data)  
  } else {
    message("skipped subject ", subject, " error in data")
  }
  
}



data$subject<-as.factor(data$subject)
data$word_inDictionary<-as.factor(data$word_inDictionary)
data<-data%>%
  mutate(word_ACC = if_else(word_inDictionary == "TRUE",1,0))


save(data,file="E2/data.Rda")
save(qData_E1B,file="E2/q-data.Rda")



####### GET E3 Data ###################
files<-list.files(path="E3/data/")

# Key for tab seperated data
key<-list()
key$length <-6

key$label     <-c(1, 16, 31, 46, 61, 76)
key$paragraph <-c(4, 19, 34, 49, 64, 79)
key$response  <-c(8, 23, 38, 53, 68, 83)
key$timestamp <-c(12,27, 42, 57, 72, 87)

key$Qdata     <-c(92, 93, 94, 95)


data<-c()
qData_E3<-c()

for(i in 1:length(files)){
  subject  <- files[i]
  path     <- paste0("E3/data/", files[i])
  raw.data <- scan(file=path, what = "character", sep="\t")
  
  temp.qData<-data.frame("E3",subject,raw.data[92],raw.data[93],raw.data[94],raw.data[95])
  qData_E3<-rbind(qData_E3,temp.qData)
  
  if(length(raw.data) == 95){
    sub_data<-create_dataframe(raw.data = raw.data, 
                               subject = subject,
                               key = key)
    
    data<-rbind(data,sub_data)  
  } else {
    message("skipped subject ", subject, " error in data")
  }
  
}


data$subject<-as.factor(data$subject)
data$word_inDictionary<-as.factor(data$word_inDictionary)
data<-data%>%
  mutate(word_ACC = if_else(word_inDictionary == "TRUE",1,0))


data$delay<-""
data[data$condition == "D0A" | data$condition == "D0B",]$delay <- "0ms"
data[data$condition == "D100A" | data$condition == "D100B",]$delay <- "100ms"
data[data$condition == "D200A" | data$condition == "D200B",]$delay <- "200ms"
data$delay<-as.factor(data$delay)


data<-data%>%
  select(subject,delay,keyNum,letter,IKSI,word,word_Num,word_letterPos,word_IKSI,word_length,word_inDictionary,word_ACC)

save(data,file="E3/data.Rda")



names(qData)<-c("experiment","subject","silent","words","letters","not")
save(qData,file="E3/q-data.Rda")





