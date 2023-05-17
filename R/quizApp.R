example = function() {
  quiz.dir = "C:/libraries/LearnStory/umwelt"

  app = quizApp(quiz.dir, pageid="q2a_02")
  #app = quizApp(quiz.dir)
  viewApp(app,url.args = list(u="test"))
}

quizApp = function(quiz.dir, db.dir = quiz.dir, pageid=NULL, develop=TRUE, userid = NULL, gameid = basename(quiz.dir), with.mathjax=TRUE) {
  restore.point("quizApp")
  app=eventsApp()

  shiny::addResourcePath("js", system.file("js", package="LearnStory"))
  glob = app$glob
  glob$develop = develop
  glob$quiz.dir = quiz.dir
  glob$fixed_userid = userid
  glob$gameid = app$gameid = gameid

  glob$userid = userid
  glob$pageid = pageid
  glob$origin = "q"

  glob$with.mathjax = with.mathjax

  glob$db.dir = db.dir
  glob$use.db = !is.null(db.dir)
  if (glob$use.db) {
    db = get.quizdb(db.dir)
  }

  spec = parse.yaml.md.file(file.path(quiz.dir, "spec_quiz.md"))
  copy.into.env(spec, glob)

  frag.dir = system.file("html", package="LearnStory")
  glob$quiz.frag = readUtf8(file.path(frag.dir, "quiz_frag.html")) %>% merge.lines()


  glob$pages = get_quiz_pages(glob)

  app$ui = fluidPage(
    tags$head(HTML('
<script src="js/check_answer.js"></script>
<script src="js/quiz.js"></script>
')),
    includeCSS(file.path(quiz.dir,"/quiz.css")),
    cookiesHeader(),
    if (develop) {
      sidebarLayout(uiOutput("developUI"), uiOutput("mainUI"))
    } else {
      uiOutput("mainUI")
    }
  )
  quiz_app_handlers()


  loadPageCookiesHandler(fun= function(cookies,...) {
    init_quiz_app(cookies)
  })

  app
}


init_quiz_app = function(cookies=NULL, app=getApp(), glob=app$glob) {
    restore.point("init_story_app")

  # Don't specify a user if glob$pageid is provided
  if (!is.null(glob$pageid)) {
    quiz.df = choose_single_quiz(glob$pageid, glob$pages)
    app$quiz.df = quiz.df
    app$qnum = 1
    set_quiz_page()
    return()
  }

  app$userid = cookies[["learn_story_userid"]][[1]]
  cat("\nuserid from cookies: ", app$userid,"\n")

  if (!is.null(glob$fixed_userid)) {
    app$userid = glob$fixed_userid
    setCookie("learn_story_userid",list(userid=app$userid))
  }

  app$new.user = is.null(app$userid)
  if (is.null(app$userid)) {
    app$userid = random.string(1,10)
    setCookie("learn_story_userid",list(userid=app$userid))
  }
  set_quiz_menu_ui()
}

set_quiz_menu_ui = function(app=getApp(), glob=app$glob) {
  kapitel = names(glob$chapters)
  names(kapitel) = unlist(glob$chapters)

  ui = tagList(
    tags$div(style="margin: 1em",
    h4(glob$title),
    selectInput("num_questions", "Anzahl Fragen",choices = c("10"=10,"20"=20,"Alle"=1000), selected=100, multiple=FALSE),
    selectInput("chapters", "Kapitel", choices=kapitel,selected = kapitel, multiple = TRUE),
    simpleButton("startQuizBtn","Starte Quiz",form.ids = c("num_questions","chapters"))
    )
  )
  setUI("mainUI",ui)
}

quiz_app_handlers = function(app=getApp()) {
  buttonHandler("startQuizBtn", start_quiz_click)
  buttonHandler("btn-next", next_quiz_click)
  eventHandler("answer_click","answer_click",answer_quiz_click)

  if (app$glob$develop) {
    buttonHandler("refreshBtn",function(...) set_quiz_page())
    buttonHandler("#showSourceBtn",function(...) show_source())
  }

}


get_quiz_pages = function(glob=getApp()$glob) {
  restore.point("get_quiz_pages")
  if (is.null(glob$quiz_pages_glob)) {
    quiz_pages_glob = "*.md"
  } else {
    quiz_pages_glob = glob$quiz_pages_glob

  }
  quiz.dir = glob$quiz.dir

  setwd(quiz.dir)
  files = list.files(quiz.dir, glob2rx(quiz_pages_glob), recursive = TRUE)

  pages = tibble(pageid = tools::file_path_sans_ext(files), page.file = file.path(quiz.dir,files), chapter = str.left.of(files, "/", not.found = rep("", length(files))), pagebase = basename(files))
  pages
}

choose_single_quiz = function(pageid, pages=getApp()$glob$pages) {
  restore.point("choose_single_qui")
  page = get_page(pageid, pages)
  quiz.df = pages[pages$pagebase == page$pagebase,]
  quiz.df
}

choose_quizes = function(num_questions, chapters, pages=getApp()$glob$pages) {
  restore.point("choose_quizes")
  quiz.df = pages[pages$chapter %in% chapters,]
  if (num_questions > NROW(quiz.df))
    num_questions = NROW(quiz.df)

  rows = sample(1:NROW(quiz.df), num_questions)
  quiz.df[rows,]
}

next_quiz_click = function(..., app=getApp()) {
  app$qnum = app$qnum+1
  set_quiz_page()
}


start_quiz_click = function(formValues, ..., app=getApp()) {
  restore.point("start_quiz_click")

  formValues$chapters = unlist(formValues$chapters)
  copy.into.env(formValues, app)
  glob = app$glob

  quiz.df = choose_quizes(app$num_questions,app$chapters, glob$pages )

  app$quiz.df = quiz.df
  app$qnum = 1
  set_quiz_page()
}

set_quiz_page = function(qnum=app$qnum, quiz.df = app$quiz.df, app=getApp()) {
  restore.point("set_quiz_page")
  glob = app$glob

  quiz = as.list(quiz.df[qnum,])
  quiz$qnum = qnum
  pquiz = try(parse_quiz_page(quiz$page.file))
  if (is(pquiz,"try-error")) {
    stop(paste0("Parsing error in ", quiz$page.file,":\n\n", as.character(pquiz)))
  }

  quiz = c(quiz, pquiz)

  quiz = set.null.fields(quiz, c("script","textdiv","hide_text"),"")

  quiz$quiz_title = paste0("Frage ", quiz$qnum, " von ", NROW(quiz.df))

  quiz$quiz.html = rmdtools::replace.whiskers(glob$quiz.frag, quiz, eval=FALSE)

  app$quiz = quiz
  show_quiz_page()
  show_quiz_develop()
}



show_quiz_page = function(quiz=app$quiz, app=getApp()) {
  restore.point("show_quiz")
  ui = HTML(quiz$quiz.html)
  if (app$glob$with.mathjax) {
    ui = shiny::withMathJax(ui)
  }
  setUI("mainUI",ui)
}


show_quiz_develop = function(app=getApp()) {
  restore.point("show_quiz_develop")
  if (!app$glob$develop) return()
  quiz = app$quiz
  ui = tagList(
    actionButton("refreshBtn",paste0("Refresh ", quiz$pageid)),
    actionButton("showSourceBtn",paste0("Show source"))
  )
  setUI("developUI",ui)
}

answer_quiz_click = function(value, ...) {
  args = list(...)
  restore.point("answer_quiz_click")
  log.answer(value$answer, value$check)

}

log.answer = function(answer, check, app=getApp()) {

  restore.point("log.answer")

  if (!isTRUE(app$glob$use.db)) return()

  origin = app$glob$origin

  if (origin == "q") {
    quiz = app$quiz
  } else {
    quiz = app$page
  }

  if (quiz$quiz_type=="numeric") {
    orgpos = ""
  } else {
    orgpos = paste0(quiz$sample.df$org_pos, collapse=";")

    # Check which of the statements a) b) was correctly
    # classified
    correct = quiz$solution$correct
    ans.correct = sapply(names(correct), function(char) has.substr(answer, char) == correct[char]) %>% as.integer %>% paste0(collapse="")
    answer = ans.correct
  }

  log = list(
    userid = app$userid,
    gameid=app$gameid,
    quizid = quiz$quizid,
    ok = check$ok,
    orgpos = orgpos,
    answer = answer,
    origin = origin,
    time = Sys.time()
  )

  db = get.quizdb()
  dbInsert(db, "answer", log)

}
