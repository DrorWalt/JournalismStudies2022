####################################
#Project: Walter, D., Ophir, Y., Pruden, M., & Golan, G. (2022). 
   #Watching the Whole World: The Media Framing of Foreign Countries in US News and its Antecedents. 
   #Journalism Studies, 1–21. https://doi.org/10.1080/1461670X.2022.2137838

#Author: Dror Walter

####################################

options(stringsAsFactors = FALSE)
options(scipen=999)

#### Importing Packages ####
library(readr) #for reading files
library(xlsx) #for reading/writing xlsx (might be problematic on macs)
library(quanteda) #for text analysis
library(topicmodels) #running topic modeling
library(ldatuning) #choosing K
library(tidyverse) #tidy
library(lubridate) # dealing with dates
library(tidytext) # DICTIONARIES
library(cld2) # for lang recog
library(foreign) # not sure but afraid to delete?
library(stringi) #strings
library(stringr) # strings
library(ggplot2) # plotting
library(doParallel) # another paralleling
library(dplyr) # deplyr
library(igraph) #nets (even though it is imported in func?)
library(rvest) #strip html
library(RNewsflow)
library(lubridate)
library(scales)
#library(tm)
library(RSQLite)
library(parallel)
library(doParallel)
library(scales)
library(irr)
library(ggthemes)
library(lsa)

#### Importing and Processing ProQuest News Files ####

myfilelist<- list.files(path = "proquest raw data/", full.names = TRUE, recursive=TRUE, include.dirs=FALSE)

content_list<-list()
for (itemnumber in 1:length(myfilelist)) {
  print (itemnumber)
  print (myfilelist[itemnumber])
  newfilename<-gsub("proquest raw data//","",myfilelist[itemnumber])
  newfilename<-unlist(strsplit(newfilename,"/|_"))
  print(newfilename)
  thecountry<-newfilename[1]
  thepub<-newfilename[length(newfilename)-1]
  filenum<-newfilename[length(newfilename)]
  content<-read_file(myfilelist[itemnumber])
  content_list[[itemnumber]]<-c(content,thecountry,thepub,filenum)
}

all_raw_data<-list()
for (i in 1:length(content_list)) {
  print(paste0(i,"out of", length(content_list)))
  tryonefile<-strsplit(content_list[[i]][1],"document [0-9]{1,4} of [0-9]{1,4}",fixed=FALSE)
  tryonefile<-(unlist(tryonefile))[-1]
  if (length(tryonefile)>0) {
    dftostore<-data.frame(text=tryonefile)
    dftostore$country=content_list[[i]][2]
    dftostore$pub=content_list[[i]][3]
    dftostore$filenum=content_list[[i]][4]
    all_raw_data[[i]]<-dftostore
  }
}

all_raw_data_2<-do.call(rbind,all_raw_data)

list_to_merge_data<-list()
#for (i in 67000:nrow(all_raw_data_2)) {
for (i in 1:nrow(all_raw_data_2)) {
#for (i in 1:200) {
    
  print(i)
  mytext<-all_raw_data_2[i,1]
  #title<-gsub("\\\r\\\n\\\r\\\n","",gsub("Title:","",unlist(regmatches(mytext, gregexpr("Title:.*?\\\r\\\n\\\r\\\n",mytext)))))
  #title<-gsub("\\\n","",gsub("\\\nTitle\\\n","",unlist(regmatches(mytext, gregexpr("\\\nTitle[\\\n]{0,1}[\\\r]{0,1}.*?\\\n",mytext)))))
  title<-gsub("[\\\n\\\r]","",gsub("Title","",unlist(regmatches(mytext, gregexpr("\\\nTitle[\\\n]{0,1}[\\\r]{0,1}.{2,}?\\\n",mytext)))))
  #publication<-gsub("\\\n","",gsub("Publication title","",unlist(regmatches(mytext, gregexpr("\\\nPublication title\\\n.*?\\\n",mytext)))))
  publication<-gsub("[\\\n\\\r]","",gsub("Publication title","",unlist(regmatches(mytext, gregexpr("\\\nPublication title[\\\n]{0,1}[\\\r]{0,1}.{2,}?\\\n",mytext)))))
  #date<-gsub("\\\n","",gsub("Publication date","",unlist(regmatches(mytext, gregexpr("\\\nPublication date\\\n.*?\\\n",mytext)))))
  date<-gsub("[\\\n\\\r]","",gsub("Publication date","",unlist(regmatches(mytext, gregexpr("\\\nPublication date[\\\n]{0,1}[\\\r]{0,1}.{2,}?\\\n",mytext)))))
  
  #text<-unlist(regmatches(mytext, gregexpr("Full text:.*?[CSL][ruo][ebc][dja][tet][ci]{0,1}[to]{0,1}[n]{0,1}:",mytext)))
  #text<-gsub("\\\n\\\n[(]{0,1}Copyright ","",gsub("Full Text\\\n[\\\n]{0,1}","",unlist(regmatches(mytext, 
  #    gregexpr("Full Text\\\n.*?\\\n\\\n[(]{0,1}[Cc][)]{0,1}[o]{0,1}[p]{0,1}[y]{0,1}[r]{0,1}[i]{0,1}[g]{0,1}[h]{0,1}[t]{0,1} ",mytext)))))
  text<-gsub("\\\n\\\n[(]{0,1}Copyright ","",gsub("Full Text\\\n[\\\n]{0,1}","",unlist(regmatches(mytext, 
      gregexpr("Full Text[\\\n]{0,1}[\\\r]{0,1}.{2,}?[\\\n\\\r]\\\n[?]{0,1}[ ]{0,1}[(]{0,1}[Cc][)]{0,1}[o]{0,1}[p]{0,1}[y]{0,1}[r]{0,1}[i]{0,1}[g]{0,1}[h]{0,1}[t]{0,1} ",mytext)))))
  list_to_merge_data[[i]]<-data.frame(country=all_raw_data_2[i,2],
                             pub1=all_raw_data_2[i,3],
                             pub2=publication,
                             filenum=all_raw_data_2[i,4],
                             date=date,
                             title=title,
                             text=text)
}


finaldata<-do.call(rbind,list_to_merge_data)

# Dropping Israel due to no cultural similarity data in Ingelhart's WVS

finaldata2<-finaldata %>% filter (country!="Israel")

write.xlsx(finaldata,"countriesdata.xlsx")
save.image("countries_data.RData")

#### Text Pre-Processing ####

data<-finaldata2

# date from text

data$date2<-as.Date(data$date,"%b %d, %Y")
data$index<-seq(1,nrow(data))

# extra-short text removal
removed_short<-subset(data,nchar(as.character(data$text))<500)
data2<-subset(data,!nchar(as.character(data$text))<500)

# duplicate items removal
removed_df<-data2[duplicated(data2$text),]
data3 <- data2[!duplicated(data2$text),]

data3$textBU<-data3$text

data3$text<-gsub("@[A-Za-z0-9_]{1,20}","",data3$text)

# printing Countries
TEMP<-data3 %>% group_by(country) %>% summarise(n=n()) %>% arrange(country)
write.csv(TEMP, "TEMP.csv")
# printing newspapers
TEMP<-data3 %>% group_by(pub2) %>% summarise(n=n()) %>% arrange(n)
write.csv(TEMP, "TEMP.csv")

# creating corpus object
mycorpus <- corpus(data3)

# remove very short tokens (1 and 2 letter words)
toks <- tokens(mycorpus)
toks <-tokens_select(toks, min_nchar=3)

# token list cleaning

stopwords_and_single<-unique(c(tm::stopwords(kind = "en"),stopwords("english")))
           boilerplate))

dfm_counts <- dfm(toks,tolower = TRUE, remove_punct = TRUE,remove_numbers=TRUE, 
                  remove = stopwords_and_single,stem = FALSE,
                  remove_separators=TRUE, include_docvars=TRUE,remove_url=TRUE) 


docnames(dfm_counts)<-data3$index

# check dims
dim(dfm_counts)

# make data a bit sparser (remove words too common or too rare)
dfm_counts2<-dfm_trim(dfm_counts, max_docfreq = 0.5*nrow(data3), min_docfreq=500,docfreq_type="count")

# check dims again
dim(dfm_counts2)

# Wordcloud to examine tokens
textplot_wordcloud(dfm_counts2, max_words=400)


# Convert to topic model ready data
dtm_lda <- convert(dfm_counts2, to = "topicmodels")

full_data<-dtm_lda



#### Optimizing K and Alpha ####
parallel::detectCores()

Sys.time()
FTN_result <- FindTopicsNumber(
  dtm_lda,
  topics = c(2, seq(5,150,by=5)), # Specify how many topics you want to try.
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 734), # add random seed for reproducability
  mc.cores = 80L, # Specify the number of cores that you computer has to achieve the best performance. 
  verbose = TRUE
)
Sys.time()

FindTopicsNumber_plot(FTN_result)  

save.image("FNV_data_POST_searchk.RData")
load("FNV_data_POST_searchk.RData")



#### Running the models ####
runsdf<-data.frame(myk=c(45,55,60,65))
mymodels<-list()

cluster <- makeCluster(detectCores(logical = TRUE) - 6) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(topicmodels)
})

clusterExport(cluster, c("dtm_lda","runsdf"))

system.time({
  mymodels <- foreach(j = 1:nrow(runsdf)) %dopar%{
    k_run <- runsdf[j,1]
    #alpha_run<-runsdf[j,2]
    fitted <- LDA(dtm_lda, k = k_run, method = "Gibbs",
                  #control = list(alpha=myalpha,seed=527) )
                  control = list(seed=527) )
  }
})
stopCluster(cluster)

LDAlist=mymodels

save.image("FNV_data_POST_MODELS.RData")





####  PRINTING MAIN FILES FOR ANALYSIS (WORDS/FREX/TEXTS) ####

# FIRST PRINTING BETA AND THETA
#Get beta out give colnames from vocab, transpose so columns are topics and name topics from 1 to n
# FIRST PRINTING BETA AND THETA
#Get beta out give colnames from vocab, transpose so columns are topics and name topics from 1 to n
for (eachLDA in LDAlist)  {
  LDAfit<-eachLDA
  datacolnum=which(colnames(data3)=="text")
  
  mybeta<-data.frame(LDAfit@beta)
  colnames(mybeta)<-LDAfit@terms
  mybeta<-t(mybeta)
  colnames(mybeta)<-seq(1:ncol(mybeta))
  mybeta=exp(mybeta)
  
  ## Now we cycle and print top words for each topic
  nwords=50
  
  topwords <- mybeta[1:nwords,]
  for (i in 1:LDAfit@k) {
    tempframe <- mybeta[order(-mybeta[,i]),]
    tempframe <- tempframe[1:nwords,]
    tempvec<-as.vector(rownames(tempframe))
    topwords[,i]<-tempvec
  }
  
  rownames(topwords)<-c(1:nwords)
  
  kalpha<-paste0(as.character(LDAfit@k),"_",gsub("\\.","",as.character(LDAfit@alpha)))
  write.xlsx(topwords, paste0(kalpha,"_COUNTRIES_Topwords.xlsx"))
  
  ## FREX TIME
  # getting the beta
  mybeta<-data.frame(LDAfit@beta)
  colnames(mybeta)<-LDAfit@terms
  mybeta<-t(mybeta)
  colnames(mybeta)<-seq(1:ncol(mybeta))
  mybeta=exp(mybeta)
  
  # apply formula below
  # 1/(w/(bword/sumbrow)+(1-w)/(bword)) for each cell
  myw=0.3
  word_beta_sums<-rowSums(mybeta)
  my_beta_for_frex<-mybeta
  for (m in 1:ncol(my_beta_for_frex)) {
    for (n in 1:nrow(my_beta_for_frex)) {
      my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
    }
    print (m)
  }
  ##  print 50 frex:
  nwords=50
  
  topwords <- my_beta_for_frex[1:nwords,]
  for (i in 1:LDAfit@k) {
    tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
    tempframe <- tempframe[1:nwords,]
    tempvec<-as.vector(rownames(tempframe))
    topwords[,i]<-tempvec
  }
  
  rownames(topwords)<-c(1:nwords)
  
  kalpha<-paste0(as.character(LDAfit@k),"_",gsub("\\.","",as.character(LDAfit@alpha)))
  write.xlsx(topwords,paste0(kalpha,"_COUNTRIES_TopFREX.xlsx"))
  
  ## TOP TEXTS --->
  #data3$index2<-data3$index+1
  data33<-data3
  #data33$index<-data33$index+1
  deleted_lda_texts<-(setdiff(data33$index, as.numeric(LDAfit@documents)))
  #deleted_lda_texts2<-(setdiff(as.character(LDAfit@documents),as.character(data3$doc_id)))
  
  #deleted_lda_texts<-unique(c(deleted_lda_texts1,deleted_lda_texts2))
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  #data33<-data3
  data33<-data33[data33$index %!in% deleted_lda_texts,]
  
  metadf<-data33
  meta_theta_df<-cbind(metadf[datacolnum],LDAfit@gamma)
  
  ntext=50
  
  toptexts <- mybeta[1:ntext,]
  for (i in 1:LDAfit@k) {
    print(i)
    tempframe <- meta_theta_df[order(-meta_theta_df[,i+1]),]
    tempframe <- tempframe[1:ntext,]
    tempvec<-as.vector(tempframe[,1])
    toptexts[,i]<-tempvec
  }
  
  rownames(toptexts)<-c(1:ntext)
  
  kalpha<-paste0(as.character(LDAfit@k),"_",gsub("\\.","",as.character(LDAfit@alpha)))
  write.xlsx(toptexts, paste0(kalpha,"_COUNTRIES_TopTexts.xlsx"))
  
}


#### Running ANTMN ####

network_from_LDA<-function(LDAobject,deleted_topics=c(),topic_names=c(),save_filename="",topic_size=c(),randomseed=sample(1:1000, 1),bbone=FALSE) {
  # Importing needed packages
  require(lsa) # for cosine similarity calculation
  require(dplyr) # general utility
  require(igraph) # for graph/network managment and output
  require(corpustools)
  
  print("Importing model")
  
  # first extract the theta matrix form the topicmodel object
  theta<-LDAobject@gamma
  # adding names for culumns based on k
  colnames(theta)<-c(1:LDAobject@k)
  
  # claculate the adjacency matrix using cosine similarity on the theta matrix
  mycosine<-cosine(as.matrix(theta))
  colnames(mycosine)<-colnames(theta)
  rownames(mycosine)<-colnames(theta)
  
  # Convert to network - undirected, weighted, no diagonal
  
  print("Creating graph")
  
  topmodnet<-graph.adjacency(mycosine,mode="undirected",weighted=T,diag=F,add.colnames="label") # Assign colnames
  # add topicnames as name attribute of node - importend from prepare meta data in previous lines
  if (length(topic_names)>0) {
    print("Topic names added")
    V(topmodnet)$name<-topic_names
  } 
  # add sizes if passed to funciton
  if (length(topic_size)>0) {
    print("Topic sizes added")
    V(topmodnet)$topic_size<-topic_size
  }
  newg<-topmodnet
  
  # delete 'garbage' topics
  if (length(deleted_topics)>0) {
    print("Deleting requested topics")
    
    newg<-delete_vertices(topmodnet, deleted_topics)
  }
  
  # Backbone
  if (bbone==TRUE) {
    print("Backboning")
    
    nnodesBASE<-length(V(newg))
    for (bbonelvl in rev(seq(0,1,by=0.05))) {
      #print (bbonelvl)
      nnodes<-length(V(backbone_filter(newg,alpha=bbonelvl)))
      if(nnodes>=nnodesBASE) {
        bbonelvl=bbonelvl
        #  print ("great")
      }
      else{break}
      oldbbone<-bbonelvl
    }
    
    newg<-backbone_filter(newg,alpha=oldbbone)
    
  }
  
  # run community detection and attach as node attribute
  print("Calculating communities")
  set.seed(randomseed)
  mylouvain<-(cluster_louvain(newg)) 
  mywalktrap<-(cluster_walktrap(newg)) 
  myspinglass<-(cluster_spinglass(newg)) 
  myfastgreed<-(cluster_fast_greedy(newg)) 
  myeigen<-(cluster_leading_eigen(newg)) 
  
  V(newg)$louvain<-mylouvain$membership 
  V(newg)$walktrap<-mywalktrap$membership 
  V(newg)$spinglass<-myspinglass$membership 
  V(newg)$fastgreed<-myfastgreed$membership 
  V(newg)$eigen<-myeigen$membership 
  
  # if filename is passsed - saving object to graphml object. Can be opened with Gephi.
  if (nchar(save_filename)>0) {
    print("Writing graph")
    write.graph(newg,paste0(save_filename,".graphml"),format="graphml")
  }
  
  # graph is returned as object
  return(newg)
}


# Applying Net 

LDAfit<-LDAlist[[1]]
LDAfit@k

topicnames<-read.xlsx("CODED 70_0714285714285714_COUNTRIES_TopFREX.xlsx",1)
topicnames<-colnames(topicnames)[-1]

deleted_topics<-grep("DELETED",topicnames)

topic.proportion <- colMeans(LDAfit@gamma)


mynewnet70nojunkfull<-network_from_LDA(LDAobject=LDAfit,
                                 topic_names=topicnames,
                                 topic_size=topic.proportion,
                                 deleted_topics=deleted_topics,
                                 save_filename="FNV new 70 noisr nojunk",
                                 randomseed=2345,
                                 bbone=TRUE)

#save.image("FNV_data_POST_MODELS+analysis.RData")
load("FNV_data_POST_MODELS+analysis.RData")

newg<-mynewnet70nojunkfull




#### Combining Net and LDA data ####
LDAfit<-LDAlist[[1]]

#data33<-data3
#data33$index<-as.character(data33$index)
#deleted_lda_texts<-(setdiff(data33$index, LDAfit@documents))
#deleted_lda_texts2<-(setdiff(as.character(LDAfit@documents),as.character(data3$doc_id)))
#deleted_lda_texts<-unique(c(deleted_lda_texts1,deleted_lda_texts2))
'%!in%' <- function(x,y)!('%in%'(x,y))
#data33<-data3
#data33<-data33[data33$index %!in% deleted_lda_texts,]
#metadf<-data33

metadf<-data3

metadf$tojoin<-metadf$textBU
meta_theta_df<-cbind(metadf,LDAfit@gamma)
removed_df2<-removed_df
removed_df2$tojoin<-removed_df2$text

removed_df2<-inner_join(removed_df2,meta_theta_df,by="tojoin")
removed_df2<-removed_df2[,-c(11:19)]
colnames(removed_df2)<-gsub("\\.x","",colnames(removed_df2))
#removed_df2$index<-as.character(removed_df2$index)
meta_theta_df2<-bind_rows(meta_theta_df,removed_df2)
meta_theta_df<-meta_theta_df2
rm(meta_theta_df2)

colnames(meta_theta_df)[12:81]<-paste0("X",colnames(meta_theta_df)[12:81])

meta_theta_df_comm<-meta_theta_df

meta_theta_df_comm$comm1<-rowSums(meta_theta_df_comm[,(as.numeric(V(newg)$label[which(V(newg)$eigen == 1)]))+11]) # green cult
meta_theta_df_comm$comm2<-rowSums(meta_theta_df_comm[,(as.numeric(V(newg)$label[which(V(newg)$eigen == 2)]))+11]) # blue econ
meta_theta_df_comm$comm3<-rowSums(meta_theta_df_comm[,(as.numeric(V(newg)$label[which(V(newg)$eigen == 3)]))+11]) # red pol 

meta_theta_df_AGG<-aggregate(meta_theta_df_comm[,12:ncol(meta_theta_df_comm)],by=list(meta_theta_df_comm$country),FUN="mean")
colnames(meta_theta_df_AGG)[1]<-"country"

meta_theta_df_AGG_SIZE<-aggregate(meta_theta_df_comm[,"country"],by=list(meta_theta_df_comm$country),FUN="NROW")
colnames(meta_theta_df_AGG_SIZE)[1]<-"country2"
colnames(meta_theta_df_AGG_SIZE)[2]<-"size"

meta_theta_df_AGG_ALL<-cbind(meta_theta_df_AGG_SIZE,meta_theta_df_AGG)

########################### PLOTS OF FRAMES PER COUNTRY
dat_long <- meta_theta_df_AGG[,c("country","comm1","comm2","comm3")] %>%
  gather("Stat", "Value", -country)

meta_theta_df_AGG_ALL$size2<-log(meta_theta_df_AGG_ALL$size)
meta_theta_df_AGG_ALL$size2<-(meta_theta_df_AGG_ALL$size2-min(meta_theta_df_AGG_ALL$size2))/(max(meta_theta_df_AGG_ALL$size2)-min(meta_theta_df_AGG_ALL$size2))
meta_theta_df_AGG_ALL$size2<-meta_theta_df_AGG_ALL$size2+(1-max(meta_theta_df_AGG_ALL$size2))
colnames(meta_theta_df_AGG_ALL)[1]<-"countryx"

#### Plotting ####
colalpha=0.7
fontsize=13

pp1<-ggplot(meta_theta_df_AGG_ALL, aes(x=reorder(country,comm1)))+
  geom_col(aes(y=meta_theta_df_AGG_ALL$comm1), fill="#a1d99b", 
           color="gray",alpha=colalpha)+
  coord_flip()+
  theme_bw()+
  ggtitle("Human Interest")+
  labs(y = "Frame Prevalence", 
       x = "")+
  theme(axis.text.y = element_text(size = fontsize)) #+
  #scale_x_discrete(limits = rev(levels(meta_theta_df_AGG_ALL$country)))
  #theme(text = element_text(size=7))

pp2<-ggplot(meta_theta_df_AGG_ALL, aes(x=reorder(country,comm2)))+
  geom_col(aes(y=meta_theta_df_AGG_ALL$comm2), fill="#9ebcda", 
           color="gray",alpha=colalpha)+
  coord_flip()+
  theme_bw()+
  ggtitle("Economic")+
  labs(y = "Frame Prevalence", 
       x = "")+
  theme(axis.text.y = element_text(size = fontsize))#+
#scale_x_discrete(limits = rev(levels(meta_theta_df_AGG_ALL$country)))


pp3<-ggplot(meta_theta_df_AGG_ALL, aes(x=reorder(country,comm3)))+
  geom_col(aes(y=meta_theta_df_AGG_ALL$comm3), fill="#fa9fb5", #chose specfic fill color
           color="gray",alpha=colalpha)+ #because coords flip reveses ABC and we want to rerevrse it - we need rev size
  coord_flip()+
  theme_bw()+
  ggtitle("Conflict")+
  labs(y = "Frame Prevalence", 
       x = "")+
  theme(axis.text.y = element_text(size = fontsize))#+
  #scale_x_discrete(limits = rev(levels(meta_theta_df_AGG_ALL$country)))
#reverse so A is top - make sure to rev size or it becomes mixed
                                                                      # make sure data is ABC ordered

pp4<-ggplot(meta_theta_df_AGG_ALL, aes(x=reorder(country,size)))+
  geom_col(aes(y=meta_theta_df_AGG_ALL$size), fill="gray", #chose specfic fill color
           color="gray",alpha=colalpha)+ #because coords flip reveses ABC and we want to rerevrse it - we need rev size
  coord_flip()+
  theme_bw()+
  ggtitle("Total Volume")+
  labs(y = "# of Articles", 
       x = "")+
  theme(axis.text.y = element_text(size = fontsize)) #+
  #scale_x_discrete(limits = rev(levels(meta_theta_df_AGG_ALL$country))) #reverse so A is top - make sure to rev size or it becomes mixed

library(ggpubr)

figure<-ggarrange(pp1,pp2,pp3,pp4,
          labels = c("A", "B","C","D"),
          ncol = 4, nrow = 1)

figure

annotate_figure(figure,
    top = text_grob("Type and Volume of Coverage for foreign Countries in US Media", color = "black", face = "bold", size = 14),
    bottom = text_grob("Bar length indicates frame prevalence (0-1). Shade indicates volume of coverage (in log scale; darker shades show higher volume)", 
                       color = "black",face = "italic", size = 10))
                      #color = "black",hjust = 1, x = 1, face = "italic", size = 10))

# NEW FIGURE 1

ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")

meta_theta_df_AGG_ALL$sizelog<-log(meta_theta_df_AGG_ALL$size)

meta_theta_df_AGG_TEMP<-meta_theta_df_AGG_ALL
#meta_theta_df_AGG_TEMP$comm1<-meta_theta_df_AGG_TEMP$comm1*meta_theta_df_AGG_TEMP$sizelog
#meta_theta_df_AGG_TEMP$comm2<-meta_theta_df_AGG_TEMP$comm2*meta_theta_df_AGG_TEMP$sizelog
#meta_theta_df_AGG_TEMP$comm3<-meta_theta_df_AGG_TEMP$comm3*meta_theta_df_AGG_TEMP$sizelog

dat_long <- meta_theta_df_AGG_TEMP[,c("country","comm1","comm2","comm3")] %>%
  gather("Stat", "Value", -country)

dat_long$size <- c(meta_theta_df_AGG_TEMP$size,meta_theta_df_AGG_TEMP$size,meta_theta_df_AGG_TEMP$size)
dat_long$sizelog <- c(meta_theta_df_AGG_TEMP$sizelog,meta_theta_df_AGG_TEMP$sizelog,meta_theta_df_AGG_TEMP$sizelog)

ggplot(dat_long, aes(x=(country),y=Value, fill = Stat)) +
  geom_col()+
  coord_flip()+
  scale_fill_manual(values=c("#a1d99b", "#9ebcda","#fa9fb5"))+
  theme_bw()
  
ggplot(dat_long) + 
  geom_bar(aes(x=country, y=Value, fill=Stat,alpha=sizelog), colour="gray", stat="identity")+
  coord_flip()+
  scale_fill_manual(values=c("#a1d99b", "#9ebcda","#fa9fb5"))+
  theme_tufte()

### WORLD MAPS

library(ggtern)
meta_theta_df_AGG$color=
  rgb(meta_theta_df_AGG$comm1,meta_theta_df_AGG$comm2,meta_theta_df_AGG$comm3, maxColorValue=1)

#write.xlsx(meta_theta_df_AGG,"countries and comms.xlsx")
# OPTION 1
library(rworldmap)
library(rcol)
library(RColorBrewer)

meta_theta_df_AGG_ALL$sizelog<-log(meta_theta_df_AGG_ALL$size)
sPDF <- joinCountryData2Map( meta_theta_df_AGG_ALL
        , joinCode = "NAME"
        , nameJoinColumn = "country")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

par(mfrow=c(2,2))
colourPalette <- brewer.pal(7,'Greens')
p1<-mapCountryData( sPDF, nameColumnToPlot="comm1" ,colourPalette=colourPalette,
                    mapTitle="Human Interest Frame")

colourPalette <- brewer.pal(7,'Blues')
p2<-mapCountryData( sPDF, nameColumnToPlot="comm2" ,colourPalette=colourPalette,
                    mapTitle="Economic Frame")

colourPalette <- brewer.pal(7,'Reds')
p3<-mapCountryData( sPDF, nameColumnToPlot="comm3" ,colourPalette=colourPalette,
                    mapTitle="Conflict Frame")

colourPalette <- brewer.pal(7,'Greys')
p4<-mapCountryData( sPDF, nameColumnToPlot="size" ,colourPalette=colourPalette,
                    mapTitle="# of Articles")

#ggarrange(p1,p2,p3,
#          labels = c("A", "B","C"),
#          ncol = 3, nrow = 1)



# OPTION 2
library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

xlsx::write.xlsx(meta_theta_df_AGG_ALL,"countries and comms for analysis.xlsx")


#### DATA ANALYSIS ####
for_reg1<-meta_theta_df_AGG_ALL
for_reg2<-xlsx::read.xlsx("NB Variables v3.xlsx",1)

for_reg<-dplyr::inner_join(for_reg1,for_reg2,by=c("country"="country"))

for_reg$cultprox1emABS<-abs(for_reg$cultprox1em-0.35)
for_reg$cultprox2secABS<-abs(for_reg$cultprox2sec-0.54)

for_reg$volume<-for_reg$size

for_reg$c1<-for_reg$comm1*for_reg$volume #cult
for_reg$c2<-for_reg$comm2*for_reg$volume #econ
for_reg$c3<-for_reg$comm3*for_reg$volume #poli

# Prominence: GDP_b, size_sqkm and pop
# Relevance: trade_b, cultprox1ABS, cultprox2ABS, usboots, geo_prox
# CONFLICT: violent_deaths, nat_disast

# correlation matrix:
# Prominence: GDP_b, size_sqkm and pop
library(performance)

corrdata<-for_reg[,c(97,74,75,76,81,79,80,83,90,91,89,92,93,94,87)]

apaTables::apa.cor.table(
  corrdata,show.conf.interval = FALSE,
  filename="ex.CorTable1.doc"
  )


for (eachcolumn in 1:ncol(corrdata)) {
  print (colnames(corrdata)[eachcolumn])
  print(shapiro.test((corrdata[,eachcolumn])))
}


#### REGRESSION MODELS ####
#### VOLUME
for_reg<-for_reg_BU

for_reg<-for_reg[-c(4:73)]

for_reg$military_exp_perc[17]<-NA


mod1<-lm(log(volume)~log(GDP_b)+log(size_sqkm)+log(pop),data=for_reg)
summary(mod1)
lmtest::bptest(mod1)
car::vif(mod1)
summary(lm.beta::lm.beta(mod1))
shapiro.test(residuals(mod1))
check_model(mod1)

mod2<-lm(log(volume)~log(trade_b)+log(cultprox1emABS)+log(cultprox2secABS)+log(usboots+1)+log(geo_prox),data=for_reg)
summary(mod2)
lmtest::bptest(mod2)
car::vif(mod2)
summary(lm.beta::lm.beta(mod2))
shapiro.test(residuals(mod2))

check_model(mod2)

mod3<-lm(log(volume)~log(violent_deaths)+log(nat_disast)+log(military_exp_perc+1),data=for_reg)
summary(mod3)
lmtest::bptest(mod3)
car::vif(mod3)
summary(lm.beta::lm.beta(mod3))
shapiro.test(residuals(mod3))

## COMM 1

mod4<-lm(log(comm1)~log(GDP_b)+log(size_sqkm)+log(pop),data=for_reg)
summary(mod4)
lmtest::bptest(mod4)
car::vif(mod4)
summary(lm.beta::lm.beta(mod4))
shapiro.test(residuals(mod4))
check_model(mod4)

mod5<-lm(log(comm1)~log(trade_b)+log(cultprox1emABS)+log(cultprox2secABS)+log(usboots+1)+log(geo_prox),data=for_reg)
summary(mod5)
lmtest::bptest(mod5)
car::vif(mod5)
summary(lm.beta::lm.beta(mod5))
shapiro.test(residuals(mod5))

check_model(mod5)

mod6<-lm(log(comm1)~log(violent_deaths)+log(nat_disast)+log(military_exp_perc+1),data=for_reg)
summary(mod6)
lmtest::bptest(mod6)
car::vif(mod6)
summary(lm.beta::lm.beta(mod6))
shapiro.test(residuals(mod6))

check_model(mod6)


#### Comm2


mod7<-lm(log(comm2)~log(GDP_b)+log(size_sqkm)+log(pop),data=for_reg)
summary(mod7)
lmtest::bptest(mod7)
car::vif(mod7)
summary(lm.beta::lm.beta(mod7))
shapiro.test(residuals(mod7))
check_model(mod7)

mod8<-lm(log(comm2)~log(trade_b)+log(cultprox1emABS)+log(cultprox2secABS)+log(usboots+1)+log(geo_prox),data=for_reg)
summary(mod8)
lmtest::bptest(mod8)
car::vif(mod8)
summary(lm.beta::lm.beta(mod8))
shapiro.test(residuals(mod8))

check_model(mod8)

mod9<-lm(log(comm2)~log(violent_deaths)+log(nat_disast)+log(military_exp_perc+1),data=for_reg)
summary(mod9)
lmtest::bptest(mod9)
car::vif(mod9)
summary(lm.beta::lm.beta(mod9))
shapiro.test(residuals(mod9))

check_model(mod9)



#### Comm3


mod10<-lm(log(comm3)~log(GDP_b)+log(size_sqkm)+log(pop),data=for_reg)
summary(mod10)
lmtest::bptest(mod10)
car::vif(mod10)
summary(lm.beta::lm.beta(mod10))
shapiro.test(residuals(mod10))
check_model(mod10)

mod11<-lm(log(comm3)~log(trade_b)+log(cultprox1emABS)+log(cultprox2secABS)+log(usboots+1)+log(geo_prox),data=for_reg)
summary(mod11)
lmtest::bptest(mod11)
car::vif(mod11)
summary(lm.beta::lm.beta(mod11))
shapiro.test(residuals(mod11))

check_model(mod11)

mod12<-lm(log(comm3)~log(violent_deaths)+log(nat_disast)+log(military_exp_perc+1),data=for_reg)
summary(mod12)
lmtest::bptest(mod12)
car::vif(mod12)
summary(lm.beta::lm.beta(mod12))
shapiro.test(residuals(mod12))

check_model(mod12)


##### NOW ALL

mod13<-lm(log(volume)~log(pop)+log(GDP_b)+log(size_sqkm)+
           log(trade_b)+log(cultprox1emABS)+log(cultprox2secABS)+log(usboots+1)+log(geo_prox)+
           log(violent_deaths)+log(nat_disast)+log(military_exp_perc+1),data=for_reg)
summary(mod13)
lmtest::bptest(mod13)
car::vif(mod13)
shapiro.test(residuals(mod13))
check_model(mod13)


mod14<-lm(log(comm1)~log(pop)+log(GDP_b)+log(size_sqkm)+
           log(trade_b)+log(cultprox1emABS)+log(cultprox2secABS)+log(usboots+1)+log(geo_prox)+
           log(violent_deaths)+log(nat_disast)+log(military_exp_perc+1),data=for_reg)
summary(mod14)
lmtest::bptest(mod14)
car::vif(mod14)
shapiro.test(residuals(mod14))
check_model(mod14)

mod15<-lm(log(comm2)~log(pop)+log(GDP_b)+log(size_sqkm)+
           log(trade_b)+log(cultprox1emABS)+log(cultprox2secABS)+log(usboots+1)+log(geo_prox)+
           log(violent_deaths)+log(nat_disast)+log(military_exp_perc+1),data=for_reg)
summary(mod15)
lmtest::bptest(mod15)
car::vif(mod15)
shapiro.test(residuals(mod15))
check_model(mod15)

mod16<-lm(log(comm3)~log(pop)+log(GDP_b)+log(size_sqkm)+
            log(trade_b)+log(cultprox1emABS)+log(cultprox2secABS)+log(usboots+1)+log(geo_prox)+
            log(violent_deaths)+log(nat_disast)+log(military_exp_perc+1),data=for_reg)
summary(mod16)
lmtest::bptest(mod16)
car::vif(mod16)
shapiro.test(residuals(mod16))
check_model(mod16)

#toprint<-meta_theta_df_comm[order(-meta_theta_df_comm$comm1),]
#toprint<-toprint[1:1000,]

toprint<-meta_theta_df_comm

topicnamesnumbers<-data.frame(number=colnames(toprint)[12:81],topicnames)

write.csv(topicnamesnumbers,"topicnamesnumbers.csv")


colnames(toprint)[12:81]<-topicnames

toprint<-toprint[sample(1:nrow(toprint),5000),]

write.csv(meta_theta_df_AGG_ALL,"TEMPmetathetaAGG.csv")


write.csv(meta_theta_df_AGG_ALL,"TEMPmetathetaAGG.csv")

#### Top FRex for appendix ####
# first - create the topic community ttitle paste

tempNAMES<-topicnames[-deleted_topics]

tempCOMMS<-V(mynewnet70nojunkfull)$eigen
tempCOMMS<-dplyr::recode(tempCOMMS,"1"="Hum.Int","2"="Economic","3"="Conflict")

name_comm<-tempNAMES
name_comm[11]<-"Middle-East.conflicts"
name_comm[58]<-"US.Congress.and.Senate"
name_comm[17]<-"Sports.competitions"  
name_comm[56]<-"Mueller.investigation"  

name_comm<-gsub("\\."," ",name_comm)

#### Appendix - country topic table

colnames(meta_theta_df_AGG)

TEMP_meta_thetaAGG<-meta_theta_df_AGG[,2:71]
rownames(TEMP_meta_thetaAGG)<-meta_theta_df_AGG$country

colnames(TEMP_meta_thetaAGG)

deleted_topics

TEMP_meta_thetaAGG<-TEMP_meta_thetaAGG[,-(deleted_topics)]

colnames(TEMP_meta_thetaAGG)<-name_comm

df_for_count_topic_table<-list()
for (i in 1:nrow(TEMP_meta_thetaAGG)) {
  TEMP<-TEMP_meta_thetaAGG[i,]
  TEMP<-as.data.frame(t(TEMP))
  myorder<-order(TEMP[,1],decreasing = TRUE)[1:5]
  
  
  df_for_count_topic_table[[i]]<-c(rownames(TEMP_meta_thetaAGG)[i],paste0(rownames(TEMP)[myorder]," (",round(TEMP[myorder,1],4)*100,"%)"))
  
}

df_for_count_topic_table<-as.data.frame(do.call(rbind,df_for_count_topic_table))

colnames(df_for_count_topic_table)<-c("Country","Topic #1","Topic #2","Topic #3","Topic #4","Topic #5")

df_for_count_topic_table

write.csv(df_for_count_topic_table,"df_for_count_topic_table.csv")