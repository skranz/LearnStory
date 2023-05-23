example = function() {
  quiz.dir = "C:/libraries/LearnStory/umwelt"
  #app = quizApp(quiz.dir, pageid="q2a_02")
  app = quizApp(quiz.dir)
  viewApp(app,url.args = list(u="test"))
}

quizApp = function(quiz.dir, db.dir = quiz.dir, pageid=NULL, develop=TRUE, userid = NULL, gameid = basename(quiz.dir), with.mathjax=TRUE, extra.html=NULL, check.update=TRUE) {
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
  frag.dir = system.file("html", package="LearnStory")
  glob$quiz.frag = readUtf8(file.path(frag.dir, "quiz_frag.html")) %>% merge.lines()

  spec = parse.yaml.md.file(file.path(quiz.dir, "spec_quiz.md"))
  copy.into.env(spec, glob)

  res = load_quizes(glob$quiz.dir, glob$quiz_pages_glob, update=check.update)
  glob$quiz.df = res$quiz.df
  glob$tags.df = res$tags.df

  app$ui = tagList(fluidPage(
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
    },
    ),
    if (!is.null(extra.html)) HTML(extra.html)
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
    quiz.df = choose_single_quiz(glob$pageid, glob$quiz.df)
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

  init_answer_stats(userid=app$userid)

  set_quiz_menu_ui()
}

set_quiz_menu_ui = function(app=getApp(), glob=app$glob, restart=FALSE) {
  restore.point("set_quiz_menu_ui")
  kapitel = names(glob$chapters)
  names(kapitel) = unlist(glob$chapters)
  all.tags = glob[["tags"]]

  info.ui = NULL
  if (restart) {
    info.ui = tagList(
      h4("Gut gemacht!"),
      p("Du hast Quiz erfolgreich beendet. Wenn du magst, kannst du gleich noch eine Runde spielen.")
    )
    vals = app$menu_values
  } else {
    vals = list(num_questions = 3, chapters=kapitel,tags=all.tags)
  }

  hinweis = "<p><br><b>Hinweis:</b> Zum Teil gibt es Wissensfragen, für die du einfach gewisse Fakten lernen solltest. Oft gibt es aber mathematische Verständnisfragen. Verständnisfragen sind nicht zum Auswendiglernen gedacht, sondern sollen dir einfach Feedback geben, wie gut du die Thematik verstanden hast.</p>"

  ui = shiny::tags$div(style="margin: 1em",
    info.ui,
    selectInput("num_questions", "Anzahl Fragen",choices = c("3"=3, "5"=5, "10"=10,  "20"=20,"Alle"=1000), selected=vals$num_questions, multiple=FALSE),
    selectInput("chapters", "Kapitel", choices=kapitel,selected = vals$chapters, multiple = TRUE),
    selectInput("tags","Kategorien", choices=all.tags, multiple=TRUE, selected=vals$tags),
    simpleButton("startQuizBtn","Starte Quiz",form.ids = c("num_questions","chapters","tags")),
    HTML(hinweis)
  )

  setUI("mainUI",ui)
}

quiz_app_handlers = function(app=getApp()) {
  buttonHandler("startQuizBtn", start_quiz_click)
  buttonHandler("btn-next", next_quiz_click)
  eventHandler("answer_click","answer_click",answer_quiz_click)

  if (app$glob$develop) {
    buttonHandler("refreshBtn",function(...) set_quiz_page())
    buttonHandler("showSourceBtn",function(...) show_source())
  }

}


next_quiz_click = function(..., app=getApp()) {
  app$qnum = app$qnum+1
  if (app$qnum > NROW(app$quiz.df)) {
    set_quiz_menu_ui(restart=TRUE)
    return()
  }
  set_quiz_page()
}


start_quiz_click = function(formValues, ..., app=getApp()) {
  restore.point("start_quiz_click")

  app$menu_values = formValues

  formValues$chapters = unlist(formValues$chapters)


  copy.into.env(formValues, app)
  glob = app$glob

  quiz.df = choose_quizes(app$num_questions,app$chapters, app$tags, glob$quiz.df)

  app$quiz.df = quiz.df
  app$qnum = 1
  set_quiz_page(quiz.df = quiz.df)
}

set_quiz_page = function(qnum=app$qnum, quiz.df = app$quiz.df, app=getApp()) {
  restore.point("set_quiz_page")
  glob = app$glob

  quiz = as.list(quiz.df[qnum,])
  quiz$qnum = qnum

  pquiz = quiz[["p"]][[1]]
  if (is.null(pquiz)) {
    pquiz = try(parse_quiz_page(quiz$page.file))
    if (is(pquiz,"try-error")) {
      stop(paste0("Parsing error in ", quiz$page.file,":\n\n", as.character(pquiz)))
    }
  }
  quiz = c(quiz, pquiz)
  quiz = set.null.fields(quiz, c("script","textdiv","hide_text"),"")

  quiz$quiz_title = paste0("Frage ", quiz$qnum, " von ", NROW(quiz.df))

  quiz$quiz.html = rmdtools::replace.whiskers(glob$quiz.frag, quiz, eval=FALSE)

  app$attempt = 1
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

answer_quiz_click = function(value, ..., app=getApp()) {
  args = list(...)
  restore.point("answer_quiz_click")
  log.answer(value$answer, value$check)
  if (is.null(app$attempt)) app$attempt = 0
  app$attempt = app$attempt+1
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
    attempt = app$attempt,
    ok = check$ok,
    orgpos = orgpos,
    answer = answer,
    origin = origin,
    time = Sys.time()
  )

  db = get.quizdb()
  dbInsert(db, "answer", log)

  # Update answer stats for first attempts only
  if (app$attempt == 1) {
    row = which(app$as.df$quizid == app$quiz$quizid)
    app$as.df$time[row] = log$time
    last_correct = if (log$ok) app$as.df$last_correct[row]+1 else 0
    if (is.na(last_correct)) last_correct = 1
    app$as.df$last_correct[row] = last_correct
    if (log$ok) {
      app$as.df$num_correct[row] = app$as.df$num_correct[row]+1
    } else {
      app$as.df$num_wrong[row] = app$as.df$num_wrong[row]+1
    }
    as = app$as.df[row,]

    dbInsert(db, "answer_stat", as, mode="replace")
  }

}

init_answer_stats = function(userid=app$userid, app=getApp()) {
  restore.point("init_answer_stats")
  db = get.quizdb()
  glob = app$glob
  as.df = dbGet(db,"answer_stat", list(userid=userid, gameid = glob$gameid))

  missing = setdiff(glob$quiz.df$quizid, as.df$quizid)


  if (length(missing)>0) {
    mas.df = tibble(userid=userid, gameid = glob$gameid, quizid=missing, time=NA, last_correct=NA, num_correct = 0, num_wrong=0)
    as.df = bind_rows(as.df, mas.df)
  }
  app$as.df = as.df
  invisible(as.df)
}
