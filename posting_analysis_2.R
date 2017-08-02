
library(corrplot)
library(tabplot)

tabplot_df <- postings[ , !(names(postings) %in% c("company", "job_description", "title", "state", "city", "salary_range", "salary_high", "salary_low"))]


colMtx <- matrix(names(tabplot_df)[1:length(tabplot_df)-1], nrow = 8)
for (i in 1:ncol(colMtx)) {
  tableplot(tabplot_df, 
            select_string = c(colMtx[,i], "salary_mid"), 
            sortCol = "salary_mid", decreasing = TRUE, 
            nBins = 30)
}
### need to drop the title, company, and job description columns here ###
  
  


numeric_features <- names(postings)[sapply(postings, is.numeric)]
numeric_features <- numeric_features[-length(postings)]
print(numeric_features)


corr_numtran <- cor(postings %>% 
                      select(one_of(numeric_features, "salary_mid")), 
                    method = "pearson", 
                    use = "pairwise.complete.obs")

corrplot(corr_numtran, method = "color", order="hclust")



library(lsr)

cor.cramersV <- function(data) {
  cramersV(table(data[complete.cases(data),]))
}

corr_postings <- postings



nominal_features <- setdiff(names(postings), 
                            c('salary_high', 'salary_low', 'salary_range', 
                              'job_description', 'recommend_friend',  
                              'ceo_approve', 'data_sci_indicator', 'city', 
                              'state', 'title', 'state', 'company'))

colnames(nominal_features)

test <- postings[,!(names(postings) %in% nominal_features)]


## cramers V in test dataset
corr_nomtran <- sapply(nominal_features, 
                       function(x) sapply(nominal_features,
                                          function(y) cor.cramersV(postings[, c(x, y)])))

par(xpd=TRUE)
corrplot(corr_nomtran, method = "color", order="hclust", mar = c(2, 0, 1, 0))


## coorelation between ordered categorical variables in training - spearman
cor.ordcnt <- function(data, x, y) {
  cor(as.numeric(data[[x]]), as.numeric(data[[y]]), 
      method = "spearman", 
      use = "pairwise.complete.obs")
}

corr_ordcnttran <- data.frame(Variable = nominal_features,
                              Correlation = sapply(nominal_features, 
                                                   function(x) -cor.ordcnt(postings, x, "salary_mid")))

library(ggplot2)
library(plotly)
ggplot(corr_ordcnttran, aes(reorder(Variable, Correlation), Correlation)) + 
  geom_bar(stat = "identity") +
  coord_flip()

#########################################################################################

tests <- colnames(postings)
tests <- tests[14:33]
tests <- tests[-19]

pvalues <- as.numeric(c(0.00000000000000022, 0.00000002788, 0.0182, -0.0000003231, 0.0001524, -0.0001524, 
                        0.00000000000000022, -0.002719, 0.4465, -0.054, -0.00000000000000022, 
                        0.00000000000000022, 0.000000004717, 0.0000000000000008158, 
                        0.00000000000000022, 0.00000000000000022, 0.4262, 0.00000000001099, 0.07562))

stats <- cbind.data.frame(tests, pvalues)

stats <- stats %>% 
  arrange(pvalues)

stats$tests <- factor(stats$tests, ordered = TRUE, levels = rownames(stats))


plot_ly(stats, x = ~tests, y = ~pvalues, type = 'scatter', mode = 'lines')



ggplot(stats, aes(x=tests, y=pvalues)) + 
  geom_point()


##############################################################################################

postings_model <- postings[, !(names(postings) %in% c("ceo_approve", "company", "job_description", 
                                                      "rating", "recommend_friend", "title", 
                                                      "salary_low", "salary_high", "salary_range", 
                                                      "city", "state", "data_sci_indicator"))]
summary(postings_model)
cor(postings_model)

model.saturated = lm(salary_mid ~ . -python -sql -phd -statistic - ml -r - spark - hadoop - java, data = postings_model)

summary(model.saturated)

plot(model.saturated)

influencePlot(model.saturated)

vif(model.saturated)

avPlots(model.saturated)








