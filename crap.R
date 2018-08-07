bfas_q <- read_lines("bfas_questions.txt")
jf_q <- read_lines("jf_questions.txt")


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


df <- data.frame()

for(bfas in bfas_q){
  
  for(jf in jf_q){
    
    temp <- q2vec(bfas, jf, method = "spearman")
    
    df <- rbind(df, temp)
    
  }
  
}


crap_words <- read_lines("survey_words_vector2.txt")

for(i in crap_words){
  
  print(i)
  x <- word_vectors[i ,, drop=T]
  
}





















#-----------------------------------------------------------------
test <- "C:/Users/rober/Desktop/texttest.txt"

readLines("C:/Users/rober/Desktop/texttest.txt",3, warn = F)


processFile = function(filepath) {
  con = file(filepath, "rt")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
}






for(i in 1:3){
  x <- readLines(test)[i]
  if()
  print(x)
}


crazy <- "C:/Users/rober/Documents/MIND_2018/glove.840B.300d.txt"
readLines("C:/Users/rober/Documents/MIND_2018/glove.840B.300d.txt",1)

read.table()


# ---------------------------------------------------------------------