example.compile_quizes = function() {
  quiz.dir = "C:/libraries/LearnStory/umwelt"
  compile_quizes(quiz.dir)
}

load_quizes = function(quiz.dir,quiz_pages_glob = "q*.md", update=TRUE) {
  restore.point("load_quizes")
  quiz.file = file.path(quiz.dir, "quizes.Rds")
  if (!file.exists(quiz.file)) {
    res = compile_quizes(quiz.dir, quiz_pages_glob, update=FALSE)
  } else if (update) {
    quiz.df = readRDS(quiz.file)
    res = compile_quizes(quiz.dir, quiz_pages_glob, quiz.df=quiz.df)
  }
  if (is.null(res$tags.df)) {
    tags.file = file.path(quiz.dir, "quiz_tags.Rds")
    if (file.exists(tags.file)) {
      res$tags.df = readRDS(tags.file)
    } else {
      res$tags.df = data.frame(quizid = character(0), tag=character(0))
    }
  }
  res
}

compile_quizes = function(quiz.dir, quiz_pages_glob = "q*.md", quiz.df = NULL, update=!is.null(quiz.df)) {
  restore.point("compile_quizes")
  pages = get_quiz_pages(quiz.dir, quiz_pages_glob)
  pages$mtime = file.mtime(pages$page.file)

  if (update) {
    del = !quiz.df$pageid %in% pages$pageid
    quiz.df = quiz.df[!del,]

    quiz.df = remove.cols(quiz.df,"old.mtime")

    quiz.df = rename(quiz.df, old.mtime = mtime)
    quiz.df = left_join(quiz.df, select(pages,pageid,mtime), by="pageid")

    new = !pages$pageid %in% quiz.df$pageid
    quiz.df = bind_rows(quiz.df, pages[new,])

    quiz.df$mod = !is.true.vec(quiz.df$old.mtime==quiz.df$mtime)


    inds = which(quiz.df$mod)
    quiz.df = remove.cols(quiz.df,c("mod", "old.mtime"))
    if (length(inds)==0) {
      return(list(changed=FALSE, quiz.df=quiz.df))
    }
  } else {
    quiz.df = pages
    inds = seq_len(NROW(quiz.df))
    if (!"p" %in% colnames(quiz.df)) {
      quiz.df$p = list(NROW(quiz.df))
    }

  }

  quiz.df$p[inds] = lapply(inds, function(ind) {
    parse_quiz_page(quiz.df$page.file[ind], quiz.dir=quiz.dir)
  })

  quiz.df$quizid = basename(quiz.df$pageid)

  saveRDS(quiz.df, file.path(quiz.dir,"quizes.Rds"))

  tags.df = lapply(seq_len(NROW(quiz.df)), function(ind) {
    qtags = quiz.df$p[[ind]]$tags
    if (length(qtags)==0) return(NULL)
    as.data.frame(list(quizid = quiz.df$quizid[ind], tag=unlist(qtags)))
  }) %>% bind_rows()

  saveRDS(tags.df, file.path(quiz.dir,"quiz_tags.Rds"))

  return(list(changed = TRUE, quiz.df=quiz.df, tags.df = tags.df))
}


get_quiz_pages = function(quiz.dir = glob$quiz.dir, quiz_pages_glob=glob$quiz_pages_glob,  glob=getApp()$glob) {
  restore.point("get_quiz_pages")
  if (is.null(quiz_pages_glob)) {
    quiz_pages_glob = "*.md"
  }
  quiz.dir = glob$quiz.dir

  setwd(quiz.dir)
  files = list.files(quiz.dir, glob2rx(quiz_pages_glob), recursive = TRUE)

  pages = tibble(pageid = tools::file_path_sans_ext(files), page.file = file.path(quiz.dir,files), chapter = str.left.of(files, "/", not.found = rep("", length(files))), pagebase = basename(files))
  pages
}
