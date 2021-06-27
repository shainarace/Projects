
# install.packages("tm.corpus.Reuters21578", repos = "http://datacube.wu.ac.at")
# install.packages("SnowballC")
# install.packages("textreg")
# install.packages('tm.corpus.Reuters21578')
# install.packages('text2vec')
# install.packages('Matrix')
# install.packages('umap')
# install.packages('tm')
# install.packages('slam')
# install.packages('irlba')
# install.packages('dbscan')
# install.packages('plotly')
# install.packages('gridExtra')
# install.packages('lubridate')
# install.packages('maxmatching')
# install.packages('plyr')
# install.packages('rARPACK')
# install.packages('textrank')
# install.packages('rvest')
# install.packages('tidytext')
# install.packages('tsne')

##################################################################
library(textrank)
library(rvest)
library(tidytext)
library(dbscan)
library(ggplot2)
library(irlba)
library(umap)
library(slam)
library(text2vec)
#library(tm.corpus.Reuters21578)
library(SnowballC)
library(tm)
library(textreg)
library(stringr)
library(Matrix)
library(plotly)
library(gridExtra)
library(lubridate)
library(rARPACK)
library(htmlwidgets)
library(bookdown)
library(stringr)
library(fs)
library(tsne)

# #setwd('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Datasets and Code/reuters21578/')
# 
# PATH = '/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Datasets and Code/reuters21578/Files/'
# filenames=dir_ls(PATH)
# datetime=vector()
# text = vector()
# lewissplit=vector()
# 
# 
# for(file in filenames){
#   con = file(file,"r", encoding = "UTF-8")
#   line = readLines(con, encoding="UTF-8")
#   line = paste(line, sep=' ', collapse = ' ')
#   # String size fixed
#   lewis_idx=str_locate_all(pattern='LEWISSPLIT=',line)
#   date_idx=str_locate_all(pattern='<DATE>',line)
#   for(i in 1:nrow(lewis_idx[[1]])){
#     datetime[length(datetime)+1]=substr(line, date_idx[[1]][i,2]+1,date_idx[[1]][i,2]+20)
#     lewissplit[length(lewissplit)+1]=substr(line, lewis_idx[[1]][i,2]+2,lewis_idx[[1]][i,2]+5)
#   }
#   # String size not fixed
#   line = paste(line, sep=' ', collapse = ' ')
#   line=gsub('</','<',line)
#   text_idx=str_locate_all(pattern='<TEXT',line)
#   for(i in seq(1,nrow(text_idx[[1]])-1,2)){
#     text[length(text)+1]=substr(line, text_idx[[1]][i,2]+2,text_idx[[1]][i+1,1]-1)
#   }
#   close(con)
# }
# 
# # Annoying that I end up with fewer documents than I should. Investigate that.
# 
# # Explore why I end up with fewer documents than I should by counting occurrences of <TEXT on each file: <aha> # Fixed UTF-8 encoding on file 17 and now it works.
# date = as.POSIXct(datetime,format = '%d-%b-%Y %H:%M:%S')
# datetime[which(is.na(date))]
# datetime[which(is.na(date))] = c("5-APR-1987 01:53:30", "31-MAR-1987 05:12:1")
# datetime = as.POSIXct(datetime,format = '%d-%b-%Y %H:%M:%S')
# lewissplit[lewissplit=='TRAI']='train'
# lewissplit[lewissplit=='TEST']='test'
# 
# ############################################################################
# # Now I can pull out the heading...
# ############################################################################
# title_idx = str_locate_all(pattern='<TITLE>',text)
# head=vector()
# for(i in 1:(length(text))){
#   if(nrow(title_idx[[i]])<2){
#     head[i]=''
#   }else{
#     head[i]=substr(text[i], title_idx[[i]][1,2]+1,title_idx[[i]][2,1]-1)
#   }
# }
# head=gsub('&lt;','<',head)
# ############################################################################
# # ...and the raw article text. 
# ############################################################################
# body_idx = str_locate_all(pattern='<BODY>',text)
# body=vector()
# for(i in 1:(length(text))){
#   if(nrow(body_idx[[i]])<2){
#     body[i]=text[i]
#   }else{
#     body[i]=substr(text[i], body_idx[[i]][1,2]+1,body_idx[[i]][2,1]-1)
#   }
# }
# text=body
# Reuters <- Corpus(VectorSource(text))
# save(text,head,Reuters,lewissplit,datetime,file='docs/final_data_plots/RawDataRead.RData')

load('docs/final_data_plots/RawDataRead.RData')
# load('Reuters.RData')
# # # Step 0
# # # 1
# # ############################################################
# R = Reuters
# R = tm_map(R,content_transformer(tolower))
# # ############################################################
# # # 2
# # ############################################################
# R = tm_map(R,removeWords,stopwords("en"))
# R = tm_map(R,removePunctuation)
# R = tm_map(R,removeNumbers)
# R = tm_map(R,stemDocument)
# R = tm_map(R,removeWords, c('reuter', 'dlrs', 'mln', 'said','will', 'year', 'compani','pct','corp' ))
# # ############################################################
# # # 3
# # ############################################################
# tdm = TermDocumentMatrix(R)
# binary = weightBin(tdm)
# # ############################################################
# # # 4
# # ############################################################
# keep_terms = row_sums(binary)>=5
# tdm = tdm[keep_terms,]
# # ############################################################
# # # 5
# # ############################################################
# keep_docs = col_sums(tdm)>10
# R = R[keep_docs]
# tdm = tdm[,keep_docs ]
# dim(tdm)
# length(R)
# datetime = datetime[keep_docs]
# lewissplit=lewissplit[keep_docs]
# head=head[keep_docs]
# raw_text=text[keep_docs]
# #############################################################
# # add breaks for text wrapping
# #############################################################
# raw_text = gsub("(.{60,}?)\\s", "\\1<br>", raw_text)
# ############################################################
# # Save data to avoid repeat processing
# ############################################################
# save(raw_text,head,lewissplit,tdm,R,datetime, file='docs/final_data_plots/processedV2.RData')
load('docs/final_data_plots/processedV2.RData')


tfidf_tdm = weightTfIdf(tdm, normalize=F)
tfidf_tdm = apply(tfidf_tdm, 2, function(x){x/c(sqrt(t(x)%*%x))})
tfidf_tdm <- as(tfidf_tdm, "sparseMatrix") 
m =  Matrix::sparseMatrix(i=tfidf_tdm$i,
                          j=tfidf_tdm$j,
                          x=tfidf_tdm$v,
                          dims=c(tfidf_tdm$nrow, tfidf_tdm$ncol),
                          dimnames = tfidf_tdm$dimnames)
svd = irlba(m, 150)
save(svd,file='docs/final_data_plots/svd.RData')
# load('svd.RData')
df = data.frame(x=1:150,d=svd$d)
g1 = ggplot(data=df, aes(x=x, y=d, group=1)) +
  geom_line(color="red")+labs(y='Singular Values',x='index', 
                              title='Screeplot of Reuters tf-idf Matrix, vlines at 10, 25') + 
  geom_point() + 
  geom_vline(xintercept = 25, linetype="dotted",  color = "blue", size=1) + 
  geom_vline(xintercept = 10, linetype="dotted", color = "blue", size=1)
u.df = data.frame(x=svd$v[,1], y=svd$v[,2])
g2 = ggplot(data=u.df, aes(x=x, y=y)) +
  geom_point()+labs(y='Second Singular Component',x='First Singular Component',
                    title='SVD Projection of Reuters tf-idf Term-Document Matrix') 
g1

fig <- plot_ly(type = 'scatter', mode = 'markers')
fig <- fig %>%
  add_trace(
    x = svd$v[,1],
    y = svd$v[,2],
    text = ~paste('heading:', head ,"<br>text: ", raw_text  ),
    hoverinfo = 'text',
    marker = list(color='green', opacity=0.6),
    showlegend = F
  )

fig

svd_ump = umap(svd$v)
save(svd_ump, file='docs/final_data_plots/svd_ump.RData')
#load('svd_ump.RData')

fig <- plot_ly(type = 'scatter', mode = 'markers')
fig <- fig %>%
  add_trace(
    x = svd_ump[,1],
    y = svd_ump[,2],
    text = ~paste('heading:', head ,"<br>text: ", raw_text  ),
    hoverinfo = 'text',
    marker = list(color='green', opacity=0.6),
    showlegend = F
  )

fig

index_subset = abs(svd_ump$layout[,1]) <20 & abs(svd_ump$layout[,2]) <20
data_subset = svd_ump$layout[index_subset,]
raw_text_subset = raw_text[index_subset]
head_subset = head[index_subset]

fig <- plot_ly(type = 'scatter', mode = 'markers')
fig <- fig %>%
  add_trace(
    x = data_subset[,1],
    y = data_subset[,2],
    text = ~paste('heading:', head_subset ,"<br>text: ", raw_text_subset ),
    hoverinfo = 'text',
    marker = list(color='green'),
    showlegend = F
  )

fig


### clus = hdbscan(svd$v[,1:25],10)
### save(clus,file='hdbscan_clusters10.RData')
### load('hdbscan_clusters10.RData')
clus = hdbscan(svd_ump$layout,5)
save(clus,file='docs/final_data_plots/alldocs_hdbscan_of_map5.RData')
#load('alldocs_hdbscan_of_map5.RData')
n=length(clus$cluster)
(k = length(clus$cluster_scores))

top.words=list()
cluster.docs = vector()
centroids = matrix(NA,k,2)
mem=matrix(NA,nrow=n,ncol=k)

for(i in 1:k){
  mem[,i] = clus$cluster ==i
  tdmi = tdm[,mem[,i]]
  rs = row_sums(tdmi)
  top.words[[i]] = names(rs[order(rs,decreasing=T)])[1:10]
  cluster.docs[i] = paste(raw_text_subset[clus$cluster ==i], sep='', collapse=' ')
  centroids[i,]=colMeans(svd_ump$layout[clus$cluster ==i,])
}

displayWords=vector()
for(i in 1:k){displayWords[i] = paste(top.words[[i]][1:7] , sep=' ', collapse='<br>')}

clusters = factor(clus$cluster[index_subset])

fig <- plot_ly(type = 'scatter', mode = 'markers')
fig <- fig %>%
  add_trace(
    x = data_subset[,1],
    y = data_subset[,2],
    text = ~paste('Heading:', head_subset ,"$<br>Text: ", raw_text_subset ,"$<br>Cluster Number: ", clusters),
    hoverinfo = 'text',
    color=clusters,
    marker = list( opacity=0.6),
    showlegend = F
  )
fig
saveWidget(fig, "docs/final_data_plots/All_clusters_noTopics_UMAPClus_wNoise.html")

index_subset = abs(svd_ump$layout[,1]) <20 & abs(svd_ump$layout[,2]) <20 & clus$outlier_scores<0.6
data_subset = svd_ump$layout[index_subset,]
raw_text_subset = raw_text[index_subset]
head_subset = head[index_subset]
clusters = factor(clus$cluster[index_subset])

fig <- plot_ly(type = 'scatter', mode = 'markers')
fig <- fig %>%
  add_trace(
    x = data_subset[,1],
    y = data_subset[,2],
    text = ~paste('Heading:', head_subset ,"$<br>Text: ", raw_text_subset ,"$<br>Cluster Number: ", clusters),
    hoverinfo = 'text',
    markers = list(opacity=0.6),
    color = clusters,
    showlegend = F
  )
fig
saveWidget(fig, "docs/final_data_plots/All_clusters_noTopics_UMAPClus.html")


cen_clus = hdbscan(centroids, 3) # Down to 78 Clusters...Looks Pretty Good. 
# Omit the 2 outside 

fig <- plot_ly(type = 'scatter', mode = 'markers')%>%
  add_trace(x = centroids[,1], 
            y = centroids[,2], 
            text = ~paste('Key Words:', displayWords,"$<br>Cluster Number: ", cen_clus$cluster ), 
            color=factor(cen_clus$cluster),
            marker=list( opacity=0.6),
            showlegend = FALSE)
fig



remapClusters = function(cen_clus,clus){
  k = length(clus$cluster_scores)
  c=as.vector(clus$cluster)
  c[c==0]=k+1
  cc=as.vector(cen_clus$cluster)
  cc[k+1]=0
  new = cc[c]
  return(new)
}



newclusters = remapClusters(cen_clus, clus) 

newclusters = newclusters[index_subset]

fig <- plot_ly(type = 'scatter', mode = 'markers')
fig <- fig %>%
  add_trace(
    x = data_subset[,1],
    y = data_subset[,2],
    text = ~paste('Heading:', head_subset ,"$<br>Text: ", raw_text_subset ,"$<br>Cluster Number: ", clusters),
    hoverinfo = 'text',
    color = factor(newclusters),
    marker=list(opacity=0.6),
    showlegend = F
  )
fig
saveWidget(fig, "docs/final_data_plots/All_centroid_refined_clusters.html")

# Take rectangular subset on the interval x=y=[-2,2]
index_subset2=abs(svd_ump$layout[,1]) <2 & abs(svd_ump$layout[,2]) <2
tdm_subset = tdm[,index_subset2]
tdm_subset = tdm_subset[row_sums(tdm_subset)!=0, ]
tfidf_tdm_subset = weightTfIdf(tdm_subset, normalize=T)
m =  Matrix::sparseMatrix(i=tfidf_tdm_subset$i, 
                          j=tfidf_tdm_subset$j, 
                          x=tfidf_tdm_subset$v, 
                          dims=c(tfidf_tdm_subset$nrow, tfidf_tdm_subset$ncol),
                          dimnames = tfidf_tdm_subset$dimnames)
# Take SVD of the subset and compute the UMAP 
svd_subset = irlba(m,15)
svd_subset_map = umap(svd_subset$v)
# Subset raw text for visualization
raw_text_subset2 = raw_text[index_subset2]
head_subset2 = head[index_subset2]
# Cluster
clus2=hdbscan(svd_subset_map$layout,4)

fig2 <- plot_ly(type = 'scatter', mode = 'markers')
fig2 <- fig2 %>%
  add_trace(
    x = svd_subset_map$layout[,1],
    y = svd_subset_map$layout[,2],
    text = ~paste('heading:', head_subset2 ,"$<br>text: ", raw_text_subset2,"$<br>Cluster Number: ", clus2$cluster ),
    hoverinfo = 'text',
    color=factor(clus2$cluster),
    showlegend = F
  )
fig2

saveWidget(fig2, 'docs/final_data_plots/SubsettingUMAPforRepeatSVD.html')


tfi = apply(tfidf_tdm, 2, function(x){x/c(sqrt(t(x)%*%x))})
tfi <- as(tfi, "sparseMatrix") 
writeMM(tfi,file='tfidf_norm_tdm')

layout = read.csv('/Users/shaina/Library/Mobile Documents/com~apple~CloudDocs/Datasets and Code/reuters21578/UMAPofTFIDFsparse.csv')
fig2 <- plot_ly(type = 'scatter', mode = 'markers')
fig2 <- fig2 %>%
  add_trace(
    x = layout[,1],
    y = layout[,2],
    text = ~paste('heading:', head ,"$<br>text: ", raw_text, "$<br>x: ", layout[,1],"$<br>y: ", layout[,2] ),
                  #,"$<br>Cluster Number: ", clus2$cluster ),
    hoverinfo = 'text',
    #color=factor(clus2$cluster),
    showlegend = F
  )
fig2

