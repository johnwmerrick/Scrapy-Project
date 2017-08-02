
setwd("D:/Programming/Python/Bootcamp/Scrapy Project") 
rm(list=ls())

library(data.table)
library(tm)
library(SnowballC)
library(wordcloud)
library(dplyr)
library(gamlr)

postings <- read.csv('postings_trimmed.csv', stringsAsFactors = FALSE)

postings$job_description = gsub(',', ' ', postings$job_description)
postings$job_description = gsub(';', ' ', postings$job_description)
postings$job_description = gsub(':', ' ', postings$job_description)
postings$job_description = gsub('/', ' ', postings$job_description)




# Corpus of job description text:

list = unlist(postings$job_description,recursive=F)
#list2 = unlist(list,recursive=F)

list = gsub('\\n', ' ', list)
list = gsub('\\"', '', list)

list = paste(list, collapse = ' ')


inspect(corpus)

corpus <- Corpus(VectorSource(postings$job_description)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(tolower) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('english')) %>% 
  tm_map(removeWords, c('data', 'experience', 'job', 'company', 'will', 'business', 'work', 'years', 'new', 'working')) #%>% 
  #tm_map(stemDocument) #%>% 
  #tm_map(stemCompletion, c('environ', 'innov', 'advanc', 'engin', 'scienc', 'solut', 'applic', 'analyt', 'larg', 'statist')) #%>% 
  #tm_map(PlainTextDocument) 


wordcloud(corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, 
          use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2')) #rot.per=0.35, 

tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)

v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

summary(v)

findFreqTerms(tdm, lowfreq = 4)

findAssocs(tdm, terms = "python", corlimit = 0.2)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#####################################################################

top_companies <- postings %>% 
  filter(company %in% c('Amazon', 'Deloitte', 'KPMG', 'Uber', 'IBM')) 
                        #'Microsoft', 'NJF Global Holdings', 'Apple'


top_companies <- as.data.frame(cbind(company = top_companies[, "company"], 
                       job_description = top_companies[, "job_description"]))

top_companies <- top_companies[complete.cases(top_companies), ]

corpus2 <- Corpus(VectorSource(top_companies$job_description)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(tolower) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('english')) %>% 
  tm_map(removeWords, c('data', 'experience', 'job', 'company', 'will', 
                        'business', 'work', 'years', 'new', 'working', 
                        'deloitte', 'ibm', 'amazon', 'uber', 
                        'facebooks', 'ubers', 'llp', 'kpmg')) #%>% 
  #tm_map(stemDocument) #%>% 
  #tm_map(stemCompletion, c('environ', 'innov', 'advanc', 'engin', 'scienc', 'solut', 'applic', 'analyt', 'larg', 'statist')) #%>% 
  #tm_map(PlainTextDocument) 

tdm2 <- TermDocumentMatrix(corpus2)
tdm2 <- as.matrix(tdm2)
colnames(tdm2) = top_companies$company
tdm2 = t(rowsum(t(tdm2), group = rownames(t(tdm2))))
comparison.cloud(tdm2, colors=brewer.pal(11, "Paired"), 
                 scale=c(3,0.5), title.size = 1)

#######################################################################

top_companies2 <- postings %>% 
  filter(company %in% c('Facebook', 'NJF Global Holdings', 
                        'Apple', 'Google', 'GroupM')) 


top_companies2 <- as.data.frame(cbind(company = top_companies2[, "company"], 
                                     job_description = top_companies2[, "job_description"]))

top_companies2 <- top_companies2[complete.cases(top_companies2), ]

corpus3 <- Corpus(VectorSource(top_companies2$job_description)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(tolower) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('english')) %>% 
  tm_map(removeWords, c('data', 'experience', 'job', 'company', 'will', 
                        'business', 'work', 'years', 'new', 'working', 
                        'google', 'groupm', 'apple', 'microsoft', 'intuit', 
                        'resume', 'legal', 'recruitment', 'sex', 'sexual', 
                        'national', 'status', 'disability', 'consideration', 'facebook')) #%>% 
#tm_map(stemDocument) #%>% 
#tm_map(stemCompletion, c('environ', 'innov', 'advanc', 'engin', 'scienc', 'solut', 'applic', 'analyt', 'larg', 'statist')) #%>% 
#tm_map(PlainTextDocument) 

tdm3 <- TermDocumentMatrix(corpus3)
tdm3 <- as.matrix(tdm3)
colnames(tdm3) = top_companies2$company
tdm3 = t(rowsum(t(tdm3), group = rownames(t(tdm3))))
comparison.cloud(tdm3, colors=brewer.pal(11, "Paired"), 
                 scale=c(3,0.5), title.size = 1)

#######################################################################

metros <- postings %>% 
  filter(metro %in% c('SV', 'NYC', 'SF', 'CHI', 'LA')) 


metros <- as.data.frame(cbind(metro = metros[, "metro"], 
                              job_description = metros[, "job_description"]))

metros <- metros[complete.cases(metros), ]

corpus4 <- Corpus(VectorSource(metros$job_description)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(tolower) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords, stopwords('english')) %>% 
  tm_map(removeWords, c('data', 'experience', 'job', 'company', 'will', 
                        'business', 'work', 'years', 'new', 'working', 
                        'york', 'san', 'francisco', 'palo', 'chicago', 
                        'los', 'angeles', 'california', 'moreapply')) #%>% 
#tm_map(stemDocument) #%>% 
#tm_map(stemCompletion, c('environ', 'innov', 'advanc', 'engin', 'scienc', 'solut', 'applic', 'analyt', 'larg', 'statist')) #%>% 
#tm_map(PlainTextDocument) 

tdm4 <- TermDocumentMatrix(corpus4)
tdm4 <- as.matrix(tdm4)
colnames(tdm4) = metros$metro
tdm4 = t(rowsum(t(tdm4), group = rownames(t(tdm4))))
comparison.cloud(tdm4, colors=brewer.pal(11, "Paired"), 
                 scale=c(3,0.5), title.size = 1)

#######################################################################

python_y <- postings %>% 
  filter(python == "True")

python_n <- postings %>% 
  filter(python == "False")

colnames(python_y)


t.test(python_y$salary_mid, python_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  python_y$salary_mid and python_n$salary_mid
#         t = 11.58, df = 2786.1, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   12.01932 16.91934

# sample estimates:
#   mean of x   mean of y 
#   121.3846  106.9153


#######################################################################

scikit_y <- postings %>% 
  filter(scikit == "True")

scikit_n <- postings %>% 
  filter(scikit == "False")


t.test(scikit_y$salary_mid, scikit_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  scikit_y$salary_mid and scikit_n$salary_mid
#         t = 5.8155, df = 176.06, p-value = 2.788e-08
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   10.93466 22.16829

# sample estimates:
#   mean of x   mean of y 
#   131.6522  115.1007


#######################################################################

r_y <- postings %>% 
  filter(r == "True")

r_n <- postings %>% 
  filter(r == "False")


t.test(r_y$salary_mid, r_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  r_y$salary_mid and r_n$salary_mid
#         t = 2.363, df = 2895.3, p-value = 0.0182
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   0.4997764 5.3729655

# sample estimates:
#   mean of x   mean of y 
#   117.6836  114.7473 


#######################################################################

sql_y <- postings %>% 
  filter(sql == "True")

sql_n <- postings %>% 
  filter(sql == "False")


t.test(sql_y$salary_mid, sql_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  sql_y$salary_mid and sql_n$salary_mid
#         t = -5.1199, df = 3316.4, p-value = 3.231e-07
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   -8.642489 -3.856120

# sample estimates:
#   mean of x   mean of y 
#   113.2380  119.4873


#######################################################################

matlab_y <- postings %>% 
  filter(matlab == "True")

matlab_n <- postings %>% 
  filter(matlab == "False")


t.test(matlab_y$salary_mid, matlab_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  matlab_y$salary_mid and matlab_n$salary_mid
#         t = 3.8108, df = 614.76, p-value = 0.0001524
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   3.142240 9.824295

# sample estimates:
#   mean of x   mean of y 
#   121.5144  115.0311 


#######################################################################

sas_y <- postings %>% 
  filter(sas == "True")

sas_n <- postings %>% 
  filter(sas == "False")


t.test(sas_y$salary_mid, sas_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  sas_y$salary_mid and sas_n$salary_mid
#         t = 3.8108, df = 614.76, p-value = 0.0001524
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   t = -9.1011, df = 1174.5, p-value < 2.2e-16

# sample estimates:
#   mean of x   mean of y 
#   105.3230  118.3422  


#######################################################################

phd_y <- postings %>% 
  filter(phd == "True")

phd_n <- postings %>% 
  filter(phd == "False")


t.test(phd_y$salary_mid, phd_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  phd_y$salary_mid and phd_n$salary_mid
#         t = 15.848, df = 2754.9, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#    16.91474 21.69137

# sample estimates:
#   mean of x   mean of y 
#   128.2235  108.9205


#######################################################################

master_y <- postings %>% 
  filter(masters == "True")

master_n <- postings %>% 
  filter(masters == "False")


t.test(master_y$salary_mid, master_n$salary_mid, alternative = "two.sided")

# Welch Two Sample t-test

# data:  master_y$salary_mid and master_n$salary_mid
#         t = -3.0006, df = 2677.4, p-value = 0.002719
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   -6.184833 -1.296144

# sample estimates:
#   mean of x   mean of y 
#   113.3323  117.0728 


#######################################################################

statistic_y <- postings %>% 
  filter(statistic == "True")

statistic_n <- postings %>% 
  filter(statistic == "False")


t.test(statistic_y$salary_mid, statistic_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  statistic_y$salary_mid and statistic_n$salary_mid
#         t = 0.76132, df = 2669.7, p-value = 0.4465
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   -1.513290  3.434218

# sample estimates:
#   mean of x   mean of y 
#    116.1489  115.1885 


#######################################################################

visualization_y <- postings %>% 
  filter(visualization == "True")

visualization_n <- postings %>% 
  filter(visualization == "False")


t.test(visualization_y$salary_mid, visualization_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  visualization_y$salary_mid and visualization_n$salary_mid
#         t = -1.928, df = 1930.7, p-value = 0.054
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   -5.18919993  0.04424688

# sample estimates:
#   mean of x   mean of y 
#    113.9656  116.5381 


#######################################################################

tableau_y <- postings %>% 
  filter(tableau == "True")

tableau_n <- postings %>% 
  filter(tableau == "False")


t.test(tableau_y$salary_mid, tableau_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  tableau_y$salary_mid and tableau_n$salary_mid
#         t = -9.5482, df = 1309.4, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#    -15.93207 -10.50109

# sample estimates:
#   mean of x   mean of y 
#    105.3270  118.5436 


#######################################################################

ml_y <- postings %>% 
  filter(ml == "True")

ml_n <- postings %>% 
  filter(ml == "False")


t.test(ml_y$salary_mid, ml_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  ml_y$salary_mid and ml_n$salary_mid
#         t = 19.931, df = 3692.5, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   20.58634 25.07840

# sample estimates:
#   mean of x   mean of y 
#   128.1329  105.3005 


#######################################################################

nlp_y <- postings %>% 
  filter(nlp == "True")

nlp_n <- postings %>% 
  filter(nlp == "False")


t.test(nlp_y$salary_mid, nlp_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  nlp_y$salary_mid and nlp_n$salary_mid
#         t = 6.0075, df = 350.64, p-value = 4.717e-09
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   8.240121 16.261533

# sample estimates:
#   mean of x   mean of y 
#   127.1198  114.8690 


#######################################################################

hadoop_y <- postings %>% 
  filter(hadoop == "True")

hadoop_n <- postings %>% 
  filter(hadoop == "False")


t.test(hadoop_y$salary_mid, hadoop_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  hadoop_y$salary_mid and hadoop_n$salary_mid
#         t = 8.1031, df = 2595.6, p-value = 8.158e-16
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   7.583295 12.425130

# sample estimates:
#   mean of x   mean of y 
#   122.5928  112.5885


#######################################################################

spark_y <- postings %>% 
  filter(spark == "True")

spark_n <- postings %>% 
  filter(spark == "False")


t.test(spark_y$salary_mid, spark_n$salary_mid, alternative = "two.sided") 

# Welch Two Sample t-test

# data:  spark_y$salary_mid and spark_n$salary_mid
#         t = 11.335, df = 2155, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#    11.83414 16.78580

# sample estimates:
#   mean of x   mean of y 
#   126.0126  111.7026 


#######################################################################

java_y <- postings %>% 
  filter(java == "True")

java_n <- postings %>% 
  filter(java == "False")


t.test(java_y$salary_mid, java_n$salary_mid, alternative = "two.sided")

# Welch Two Sample t-test

# data:  java_y$salary_mid and java_n$salary_mid
#         t = 11.515, df = 2782.3, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#    11.71879 16.52911

# sample estimates:
#   mean of x   mean of y 
#    124.9855  110.8615 


#######################################################################

mongo_y <- postings %>% 
  filter(mongo == "True")

mongo_n <- postings %>% 
  filter(mongo == "False")


t.test(mongo_y$salary_mid, mongo_n$salary_mid, alternative = "two.sided")

# Welch Two Sample t-test

# data:  mongo_y$salary_mid and mongo_n$salary_mid
#         t = 0.7971, df = 232.14, p-value = 0.4262
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   -2.971534  7.009613

# sample estimates:
#   mean of x   mean of y 
#   117.7233  115.7043


#######################################################################

hive_y <- postings %>% 
  filter(hive == "True")

hive_n <- postings %>% 
  filter(hive == "False")


t.test(hive_y$salary_mid, hive_n$salary_mid, alternative = "two.sided")

# Welch Two Sample t-test

# data:  hive_y$salary_mid and hive_n$salary_mid
#         t = 6.8629, df = 1155.3, p-value = 1.099e-11
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#    7.120219 12.821279

# sample estimates:
#   mean of x   mean of y 
#   123.8546  113.8838 

#######################################################################

linux_y <- postings %>% 
  filter(linux == "True")

linux_n <- postings %>% 
  filter(linux == "False")


t.test(linux_y$salary_mid, linux_n$salary_mid, alternative = "two.sided")

# Welch Two Sample t-test

# data:  linux_y$salary_mid and linux_n$salary_mid
#         t = 1.7799, df = 568.38, p-value = 0.07562
# alternative hypothesis: true difference in means is not equal to 0

# 95 percent confidence interval:
#   -0.3319922  6.7469627

# sample estimates:
#   mean of x   mean of y 
#   118.6498  115.4423

#######################################################################








