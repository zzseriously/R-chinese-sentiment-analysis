##載入套件
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

pkg = c('data.table','dplyr','jiebaR',
        'stringr','tmcn',
        'xlsx','readxl')

check.packages(pkg)

##讀入資料
data = read_excel('Downloads經濟日報ESGETF.xlsx')

##清理資料
temp_text =data$CONTENT
temp_text  = gsub('::',' ',temp_text)
temp_text  = gsub('\r|\n|:|[.]|???',' ',temp_text)
temp_text  = gsub("'",'',temp_text)
temp_text = gsub("[^\u4E00-\u9FFCa-zA-Z0-9]*",'',temp_text) #去除utf-8的亂碼
temp_text = gsub("[a-zA-Z]|[0-9]",'',temp_text) 
temp_text = toTrad(temp_text)

###匯入停用詞和刪除
stop_words = readLines('stopwords.txt')
set.seed(7788)
w = worker(type = 'mix')

#結巴斷詞
library(jiebaR)
library(jiebaRD)
xtext2 = NULL
for (i in 1:length(temp_text)){
  t0 = temp_text[i]
  t1 = w <= t0 
  xtext2 = c(xtext2,paste0(t1,collapse=" "))
}

text_df = data_frame(doc_id = 1:length(xtext2), text = xtext2)
text_df$text = removeWords(text_df$text,stop_words)

#載入台大字典並刪除重複值
p = data.frame(word=(readLines("ntusd-positive.txt",encoding = "UTF-8")))%>%
  unique(., by = "word")
n = data.frame(word=(readLines("ntusd-negative.txt",encoding = "UTF-8")))%>%
  unique(., by = "word")
#p跟n重複的字
intersect_Dic=intersect(p,n)
#加上情緒標籤
positive = data.frame(word = p, sentiments = "positive")
negative = data.frame(word = n, sentiments = "negative")
#合併字典
Dic_ch = rbind(positive,negative)

#每一列算一個分數
score=NULL
#取出文字
for(i in c(1:nrow(text_df))){
  dic1=text_df[i,2]%>%
  {gsub('^\\s+|\\s+$','',.)}%>%
  {gsub('\\s{2,}',' ',.)}%>%strsplit(' ','')%>%
  {tibble(word=unlist(.))}
 #統計一篇文章的正面詞數量跟負面詞數量
  plot_table=dic1 %>%
    select(word) %>%
    inner_join(Dic_ch) %>% 
    group_by(sentiments) %>%
    data.frame(.)
  p_num=plot_table%>%
    subset(.,sentiments=='positive')%>%
    nrow(.)
  n_num=plot_table%>%
    subset(.,sentiments=='negative')%>%
    nrow(.)
    #合併在同一個表格
  data$正面詞數量[i]=p_num
  data$負面詞數量[i]=n_num
}
#匯出CSV檔
write.table(data,file="經濟日報情緒.csv",sep=",",row.names = F,col.names = T)

