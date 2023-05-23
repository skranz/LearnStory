# Code to choose quizes
# Uses some adaptive algorithm that gives higher probability
# to new questions and some that where wrongly answered

compute_quiz_weights = function(as.df) {
  restore.point("compute_quiz_weights")
  as.df$num = as.df$num_correct + as.df$num_wrong
  max_num = suppressWarnings(max(as.df$num, na.rm = TRUE))
  if (!is.finite(max_num)==1) max_num = 1

  as.df$time.diff = as.numeric(Sys.time())- as.numeric(as.df$time)

  as.df$share.wrong = as.df$num_wrong / as.df$num


  as.df = as.df %>%
    mutate(time_weight =
        0 +
        0.5*(time.diff > 5*60) + # more than 5 minutes ago
        0.5*(time.diff > 20*60) + # more than 20 minutes ago
        0.5*(time.diff > 60*60) + # more than one 1 hour ago
        0.5*(time.diff > 60*60*24) + # more than a day ago
        0.5*(time.diff > 60*60*24*7) + # more than a week ago
        1.0*(time.diff > 60*60*24*30)  # more than a month ago
    ) %>%
    mutate(weight = case_when(
      # New question
      num==0 ~ 5.1,
      TRUE ~ 0.1+ time_weight +
        #0.1*(max_num - num) / max_num + # overall less often asked
        1* share.wrong + # more wrong
        (4-time_weight)*(last_correct==0) # last time wrong
    ))
  as.df
}


choose_single_quiz = function(pageid, pages=getApp()$glob$quiz.df) {
  restore.point("choose_single_qui")
  page = get_page(pageid, pages)
  quiz.df = pages[pages$pagebase == page$pagebase,]
  quiz.df
}

choose_quizes = function(num_questions, chapters,qtags=NULL, quiz.df=glob$quiz.df, tags.df = glob$tags.df, as.df = getApp()$as.df, glob=getApp()$glob) {
  restore.point("choose_quizes")
  num_questions = as.integer(num_questions)
  quiz.df = quiz.df[quiz.df$chapter %in% chapters,]
  if (length(qtags)>0) {
    qtags = unlist(qtags)
    ids = unique(tags.df$quizid[tags.df$tag %in% qtags])
    quiz.df = quiz.df[quiz.df$quizid %in% ids, ]
  }

  if (num_questions > NROW(quiz.df))
    num_questions = NROW(quiz.df)

  # Get adaptive question weights
  as.df = compute_quiz_weights(as.df)
  quiz.df = remove.cols(quiz.df, "weight")
  quiz.df = left_join(quiz.df, as.df[,c("quizid","weight")], by="quizid")


  rows = sample(1:NROW(quiz.df), num_questions,replace=FALSE, prob = quiz.df$weight)
  quiz.df[rows,]
}

