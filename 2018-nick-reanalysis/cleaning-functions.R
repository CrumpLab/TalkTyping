####### functions ##########

# creates single subject data frame
create_dataframe<-function(raw.data, subject, key){
  sub_data<-c()
  message("getting data for ",subject,"...")
  
  for (i in 1:key$length){
    keys<-unlist(strsplit(raw.data[key$response[i]],split=""))
    rt<-as.numeric(unlist(strsplit(raw.data[key$timestamp[i]],split=",")))
    labels<-rep(raw.data[key$label[i]],length(keys))
    keynum<-seq(1:length(keys))
    dictionary<-get_dictionary(raw.data[key$paragraph[i]])
    
    
    temp.data<-data.frame(keynum,labels,keys,rt)
    names(temp.data)<-c("keyNum","condition","letter","timestamp")
    temp.data$timestamp_N1<-c(NA,temp.data[1:nrow(temp.data)-1,]$timestamp)
    temp.data$IKSI<-temp.data$timestamp - temp.data$timestamp_N1
    
    
    temp.data<-get_word(data = temp.data, 
                        dictionary = dictionary)
    
    sub_data<-rbind(sub_data,temp.data)
    
  }
  
  sub_data$subject<-rep(subject,nrow(sub_data))
  
  
  return(sub_data)
}

create_df_mTurk<-function(raw.data,subject){
  data<-raw.data
  data<-unlist(strsplit(data,split=":"))
  
  if(length(data) != 23){
    message("line ", subject, " skipped.")
    return()
  }
  
  p1<-data[17]
  
  # ",,," indicates a comma was typed
  p1<-gsub(",,,", ",comma,", p1)
  p1<-unlist(strsplit(p1,split=","))
  p1<-p1[-1:-2]
  
  r1<-unlist(strsplit(data[16],split=","))
  r1<-r1[-1:-2]
  
  a1<-unlist(strsplit(data[18],split=","))
  a1<-a1[-1:-2]
  
  l1<-rep('letters',length(p1))
  k1<-seq(1:length(p1))
  
  p2<-data[21]
  
  p2<-gsub(",,,", ",comma,", p2)
  p2<-unlist(strsplit(p2,split=","))
  p2<-p2[-1:-2]
  
  r2<-unlist(strsplit(data[20],split=","))
  r2<-r2[-1:-2]
  
  a2<-unlist(strsplit(data[22],split=","))
  a2<-a2[-1:-2]
  
  l2<-rep('words',length(p2)) 
  k2<-seq(1:length(p2))
  
  p<-c(p1,p2)
  r<-c(r1,r2)
  a<-c(a1,a2)
  l<-c(l1,l2)
  k<-c(k1,k2)
  s<-rep(subject,length(p))
  
  
  temp.data<-data.frame(s,l,k,p,r,a)
  names(temp.data)<-c("subject","condition","keyNum","letter","timestamp","ACC")
  
  temp.data$timestamp<-as.numeric(as.character(temp.data$timestamp))
  temp.data$timestamp_N1<-c(NA,temp.data[1:nrow(temp.data)-1,]$timestamp)
  temp.data$IKSI<-temp.data$timestamp - temp.data$timestamp_N1
  
  # get dictionary
  par1<-unlist(strsplit(data[15], split=" "))
  par1<-gsub("0,", "", par1)
  par1<-gsub("1,", "", par1)
  
  par2<-unlist(strsplit(data[19], split=" "))
  par2<-gsub("0,", "", par2)
  par2<-gsub("1,", "", par2)
  
  par<-unique(c(par1,par2))
  dictionary<-tolower(par)
  
  #get word data
  temp.data<-get_word(data=temp.data,dictionary=dictionary)
  
  return(temp.data)
  
}

# gets the set of words from target paragraph
get_dictionary<-function(paragraph){
  d<-unlist(unique(strsplit(paragraph,split=" ")))
  d<-d[d != ""]
  d<-d[!is.null(d)]
  d<-tolower(d)
  d<-gsub("[[:punct:]]","",d)
  d<-unique(d)
  return(d)
}

# groups typed letters to words and gets relevent info 
get_word<-function(data,dictionary){
  
  ### FIND word being typed / unedited
  #data<-typing
  data$word<-""
  data$word_IKSI<-""
  data$word_Num<-""
  data$word_letterPos<-""
  data$word_length<-""
  data$word_inDictionary<-""
  trackWordCount<-0
  trackLetterPos<-0
  trackWord<-c()
  trackIndex<-c()
  count<-0
  
  wordStop<-c(" ","," , ".",";",":","!","?",")","(",'"')
  for (i in 1:nrow(data)){
    #print(i)
    if (data[i,]$keyNum == 1){
      count = count + 1
      #print(count)
      
      trackWord = c()
      trackIndex = c()
      trackLetterPos <- 0
      trackWordCount<-0
    }
    
    #if we haven't started a word, then start a word
    if(length(trackWord) == 0){
      # add to word count
      trackWordCount <- trackWordCount + 1
      # start tracking the word
      trackWord = data[i,]$letter
      trackIKSI = data[i,]$IKSI
      # if we've already started tracking a word...
    } else {
      # paste new letter
      trackWord = paste(trackWord,data[i,]$letter, sep="")
      #create array of IKSIs
      trackIKSI <-c(trackIKSI, as.numeric(data[i,]$IKSI))
    }
    
    #add to letter position counter
    trackLetterPos <- trackLetterPos + 1
    data[i,]$word_letterPos<-trackLetterPos
    
    #add to index tracker
    trackIndex<-c(trackIndex,i)
    
    
    #if a space, then reboot
    if (data[i,]$letter%in%wordStop || data[i + 1,]$keyNum == 0 || i == nrow(data)){
      #print(trackWord)
      data[trackIndex,]$word        <- trackWord
      data[trackIndex,]$word_IKSI   <- list(as.numeric(trackIKSI))
      data[trackIndex,]$word_Num    <- trackWordCount
      
      #get word length (minus punctuation)
      data[trackIndex,]$word_length <- (trackLetterPos-1)
      
      #check to see if word is in the dictionary (brute spellcheck)
      data[trackIndex,]$word_inDictionary <- tolower(gsub("[[:punct:]]","",gsub(" ","", trackWord)))%in%dictionary
      
      trackWord = c()
      trackIndex = c()
      trackLetterPos <- 0
    }
  }
  
  return(data)
  
}



############# END FUNCTIONS ####################################
