example = function() {
  quiz.dir = "C:/libraries/LearnStory/muenster/umwelt"

  app = quizApp(quiz.dir)
  viewApp(app,url.args = list(u="H349ajndewz6"))

}

quizApp = function(quiz.dir, develop=TRUE) {
  restore.point("quizApp")
  app=eventsApp()


  shiny::addResourcePath("js", system.file("js", package="LearnStory"))

  glob = app$glob
  glob$develop = develop
  glob$quiz.dir = quiz.dir

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
  query <- parseQueryString(app$session$clientData$url_search)
  cat("query: ", app$session$clientData$url_search)
  restore.point("init_quiz_app")
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

  if (app$glob$develop) {
    buttonHandler("refreshBtn",set_quiz_page)
    buttonHandler("#showSourceBtn",show_source)
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
  setUI("mainUI", HTML(quiz$quiz.html))
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

