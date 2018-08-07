---
title: "Word Embedding Imputation of Neuropsychological Assessment"
author: "Rob Chavez"
date: "August 7, 2018"
output: 
  html_document: 
    highlight: haddock
    keep_md: yes
    theme: cerulean
    toc: yes
    toc_float: yes
---

In clinical settings as well as experimental research, there is often a need to assess participents in a number of different dimensions using standard neuropsychological assment tools and self-report measures. However, due to time constraints and other issues, it is not feasable to collect every possible measure of interest for every participent in a given study. That said, if you have collected at least one reliable assesment, it may be possible to approximate item responses in another set of assessments using tools of natural language processing. 

The code below attempts a rudimentary approach to meeting this challenge. Here, we attempt to use the average distance between word vectors in a phrase to imputate an assessment score for an "unassesssed" questionarre. We the compare the predicted responses from this data to actual responses from a set of real subjects.   

**Spoiler alert:** It doesn't work as implimented here. However, it provides food for thought and an avenune for a more refined approach to this question going forward.  

https://github.com/robchavez/misc_analyses/blob/master/dti_selfesteem/wholebrain_dti_selfesteem.ipynb


```r
library(tidyverse)
library(text2vec)
library(stringr)
library(lme4)
library(lmerTest)
library(reshape2)
library(caret)


setwd("C:/Users/rober/Documents/MIND_2018/")

wiki <- readLines("C:/Users/rober/Documents/MIND_2018/survey_words_again.txt", n = 1, warn = FALSE)
```

## Load data and tokenize

```r
# Create iterator over tokens
tokens = space_tokenizer(wiki)

# Create vocabulary. Terms will be unigrams (simple words).
it <- itoken(tokens, progressbar = FALSE)
vocab <-  create_vocabulary(it)

# Use our filtered vocabulary
vectorizer <-  vocab_vectorizer(vocab)

# use window of 5 for context words
tcm <-  create_tcm(it, vectorizer, skip_grams_window = 5L)
```


```r
glove <-  GlobalVectors$new(word_vectors_size = 300, vocabulary = vocab, x_max = 10)

set.seed(5)
wv_main <-  glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.005)
```

```
## INFO [2018-08-07 15:28:06] 2018-08-07 15:28:06 - epoch 1, expected cost 0.1807
## INFO [2018-08-07 15:28:06] 2018-08-07 15:28:06 - epoch 2, expected cost 0.1911
## INFO [2018-08-07 15:28:06] Success: early stopping. Improvement at iterartion 2 is less then convergence_tol
```


```r
wv_context <-  glove$components
dim(wv_context)
```

```
## [1] 300 368
```

```r
word_vectors <-  wv_main + t(wv_context)
```

## Define helper function to compare questions

```r
q2vec <- function(question1, question2, method="pearson"){
  
  # question 1  
  q1_split <- as.character(str_split(string = question1, pattern = " ", simplify = TRUE))

  q1_mat <- vector()
  for(word in q1_split){
    vals <- word_vectors[word, , drop=TRUE]
    q1_mat <- cbind(q1_mat, vals, deparse.level = 0)
  }
  
  q1_vec <- apply(q1_mat,1, mean)
  

  # quesiton 2
  q2_split <- as.character(str_split(string = question2, pattern = " ", simplify = TRUE))
  
  q2_mat <- vector()
  for(word in q2_split){
    vals <- word_vectors[word, , drop=TRUE]
    q2_mat <- cbind(q2_mat, vals, deparse.level = 0)
  }
  
  q2_vec <- apply(q2_mat,1, mean)
  
  # relate question vectors
  r <- cor(q1_vec, q2_vec, method = method)
  all <- data.frame(question1, question2, r)
  
  return(all)
  
}

bfas_q <- read_lines("bfas_questions.txt")
jf_q <- read_lines("jf_questions.txt")


df <- data.frame()

for(bfas in bfas_q){
  
  for(jf in jf_q){
    
    temp <- q2vec(bfas, jf, method = "spearman")
    
    df <- rbind(df, temp)
    
  }
  
}

# write.csv(df, "bfas_jf_question_vecs.csv")
```

## Find the best questions per survey to map scores.

```r
filter_df <- data.frame()

for(jf in unique(df$question2)){
  
  cmd <- paste0("df %>% filter(question2 == '", jf ,"') %>% filter(r == max(r))" )
  
  tmp_df <- eval(parse(text = cmd))
  
  filter_df <- rbind(filter_df, tmp_df)
  
}

filter_df$question1 <- as.character(filter_df$question1)
filter_df$question2 <- as.character(filter_df$question2)
```

## Load personality data

```r
pers_bfas <- read.csv("fyself_personality_transp_bfas.csv", stringsAsFactors = FALSE, header = T)
#pers_bfas[,2:51] <- apply(pers_bfas[,2:51], 2, scale) 

pers_jf <- read.csv("fyself_personality_transp_jf.csv", stringsAsFactors = FALSE, header = T)
#pers_jf[,2:15] <- apply(pers_jf[,2:51], 2, scale) 


filter_jf_guess <- inner_join(pers_jf, filter_df)
```

```
## Joining, by = "question2"
```
## Attach personality data to predictions

```r
jf_guess_all <- vector()
jf_guess <- vector()
n <- 0
for(k in 2:51){
  for(q in 1:length(pers_bfas$question1)){
  
    for(i in 1:length(filter_df$question1))

    if(pers_bfas[q,1] == filter_df[i,1]){
      n <- n + 1
      jf_guess[n] <-  pers_bfas[q,k]
    }
  }
}
```

## Fit to data and plot results
Below we can see the confusion matrix of the model 

```r
## fit to data

melt_jf_pers <- melt(pers_jf[,2:51])
```

```
## No id variables; using all as measure variables
```

```r
melt_jf_pers$jf_guess_all <- jf_guess
melt_jf_pers$r <- rep(filter_df$r,50)
melt_jf_pers$sub <- mapply(rep,1:50,36) %>% as.vector() 

table <- table(melt_jf_pers$value,melt_jf_pers$jf_guess_all)

table_m <- melt(table)
ggplot(table_m, aes(Var1, Var2, fill=value)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma", name ="count") +
  labs(x = "predicted values", y = "actual values") + 
  theme_minimal()
```

![](scale2vec_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
chisq.test(table)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  table
## X-squared = 24.956, df = 16, p-value = 0.07061
```

```r
confusionMatrix(table)
```

```
## Confusion Matrix and Statistics
## 
##    
##       1   2   3   4   5
##   1   9  18  40  32  18
##   2  45  60  97 103  45
##   3  78 126 151 172  61
##   4  69  77 114 147  79
##   5  42  47  60  69  41
## 
## Overall Statistics
##                                           
##                Accuracy : 0.2267          
##                  95% CI : (0.2075, 0.2467)
##     No Information Rate : 0.2906          
##     P-Value [Acc > NIR] : 1               
##                                           
##                   Kappa : 9e-04           
##  Mcnemar's Test P-Value : 1.692e-10       
## 
## Statistics by Class:
## 
##                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
## Sensitivity           0.03704  0.18293  0.32684  0.28107  0.16803
## Specificity           0.93064  0.80299  0.67339  0.73453  0.85990
## Pos Pred Value        0.07692  0.17143  0.25680  0.30247  0.15830
## Neg Pred Value        0.86096  0.81517  0.74340  0.71385  0.86827
## Prevalence            0.13500  0.18222  0.25667  0.29056  0.13556
## Detection Rate        0.00500  0.03333  0.08389  0.08167  0.02278
## Detection Prevalence  0.06500  0.19444  0.32667  0.27000  0.14389
## Balanced Accuracy     0.48384  0.49296  0.50012  0.50780  0.51396
```


The plot below breaks it down for each subject to plot the relationship in each subject (keeping in mind that modeling this is continuous data probably isn't the best thing to do here.)

```r
# facet plot per subject
ggplot(melt_jf_pers,aes(jitter(value, factor = .3), jitter(jf_guess_all,factor = .3))) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'black', fill="purple", alpha = .2) +
  facet_wrap(~variable) +
  theme_bw()
```

![](scale2vec_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Fit linear models

```r
cor.test(melt_jf_pers$value, melt_jf_pers$jf_guess_all)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  melt_jf_pers$value and melt_jf_pers$jf_guess_all
## t = -0.39028, df = 1798, p-value = 0.6964
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.05538247  0.03701456
## sample estimates:
##          cor 
## -0.009203603
```

```r
fit <- lmer(value ~ jf_guess_all + (1 + jf_guess_all|variable), REML = FALSE, data = melt_jf_pers)
summary(fit)
```

```
## Linear mixed model fit by maximum likelihood . t-tests use
##   Satterthwaite's method [lmerModLmerTest]
## Formula: value ~ jf_guess_all + (1 + jf_guess_all | variable)
##    Data: melt_jf_pers
## 
##      AIC      BIC   logLik deviance df.resid 
##   5377.6   5410.5  -2682.8   5365.6     1794 
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.73773 -0.74028 -0.03614  0.75823  2.16883 
## 
## Random effects:
##  Groups   Name         Variance Std.Dev. Corr 
##  variable (Intercept)  0.197999 0.44497       
##           jf_guess_all 0.000386 0.01965  -1.00
##  Residual              1.098483 1.04809       
## Number of obs: 1800, groups:  variable, 50
## 
## Fixed effects:
##               Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)    3.30011    0.09223  65.65557  35.782   <2e-16 ***
## jf_guess_all  -0.02111    0.02035 779.10975  -1.037      0.3    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## jf_guess_ll -0.767
```

## link to brain

https://github.com/robchavez/misc_analyses/blob/master/dti_selfesteem/wholebrain_dti_selfesteem.ipynb
