example = function() {
  restore.point.options(display.restore.point = TRUE)

  story.dir = "C:/libraries/LearnStory/umwelt"
  img.dir = "C:/libraries/LearnStory/muenster/img"

  #app = storyApp(story.dir, img.dir, userid="test")
  app = storyApp(story.dir, img.dir)
  viewApp(app,launch.browser = TRUE)
  viewApp(app,url.args = list(u="H349ajndewz6"))

}

storyApp = function(story.dir, img.dir, db.dir = story.dir, title="Münster Escape", start_pageid=NULL, develop=TRUE, gameid = basename(story.dir), userid=NULL) {
  restore.point("storyApp")
  app=eventsApp(add.events = NULL)

  shiny::addResourcePath("img", img.dir)
  shiny::addResourcePath("js", system.file("js", package="LearnStory"))

  glob = app$glob
  glob$fixed_userid = userid
  glob$origin = "s"
  glob$develop = develop
  glob$story.dir = story.dir
  glob$img.dir = img.dir
  glob$pages = get_story_pages(story.dir)
  glob$gameid = app$gameid = gameid

  glob$db.dir = db.dir
  glob$use.db = !is.null(db.dir)
  if (glob$use.db) {
    db = get.quizdb(db.dir)
  }

  frag.dir = system.file("html", package="LearnStory")
  head.html = readLines(file.path(frag.dir, "head_frag.html")) %>% merge.lines()
  glob$start.page.frag = readUtf8(file.path(story.dir, "story_start.html")) %>% merge.lines()

  glob$continue.page.frag = readUtf8(file.path(story.dir, "story_continue.html")) %>% merge.lines()

  glob$container.frag = readUtf8(file.path(frag.dir, "container_frag.html")) %>% merge.lines()

  glob$text.frag = readUtf8(file.path(frag.dir, "text_frag.html")) %>% merge.lines()

  glob$question.frag = readUtf8(file.path(frag.dir, "question_frag.html")) %>% merge.lines()

  app$ui = tagList(
    tags$title(title),
    tags$head(HTML(head.html)),
    #cookiesHeader(onload.cookies = "learn_story_userid"),
    cookiesHeader(onload.cookies = "learn_story_userid"),
    includeCSS(file.path(story.dir,"/story.css")),
    if (develop) {
      sidebarLayout(uiOutput("developUI"), uiOutput("mainUI"))
    } else {
      uiOutput("mainUI")
    }
  )

  #if (is.null(start_pageid))
  #  start_pageid = glob$pages$pageid[1]
  story_app_handlers()

  loadPageCookiesHandler(fun= function(cookies,...) {
    args = list(...)
    restore.point("nshdsdsf")
    init_story_app(cookies, start_pageid)
  })


  app
}


init_story_app = function(cookies=NULL, start_pageid=NULL, app=getApp(), glob=app$glob) {
  #base.url = app$session$clientData$url_search
  #query <- parseQueryString(app$session$clientData$url_search)
  #cat("query: ", app$session$clientData$url_search)
  restore.point("story_app")

  # Don't specify a user if start_pageid is provided
  if (!is.null(start_pageid)) {
    app$userid = NULL
    set_page(start_pageid)
    return()
  }

  app$userid = cookies[["learn_story_userid"]][[1]]
  cat("\nuserid from cookies: ", app$userid,"\n")


  if (!is.null(glob$fixed_userid)) {
    app$userid = glob$fixed_userid
    setCookie("learn_story_userid",list(userid=app$userid))
  }

  app$new.user = is.null(app$userid)
  if (!is.null(app$userid)) {
    res = load_story_progress()
    copy.into.env(res,app)
  } else {
    app$userid = random.string(1,14)
    setCookie("learn_story_userid",list(userid=app$userid))
  }
  set_start_page()
}

set_start_page = function(app=getApp()) {
  restore.point("set_start_page")
  glob = app$glob
  if (isTRUE(app$new.user)) {
    values = list()
    html = glob$start.page.frag
    setUI("mainUI", HTML(html))
  } else {
    values = list()
    html = glob$continue.page.frag
    setUI("mainUI", HTML(html))
  }
}

story_app_handlers = function(app=getApp()) {
  eventHandler("ls_click","imgAreaClick",image_area_click)
  eventHandler("ls_click","last_text_click",last_text_click)
  eventHandler("ls_click","btn_answer",answer_click)
  customEventHandler("btn_click",function(id, ...) {
    if (id == "btn-new-game") {
      new_game_click()
    } else if (id=="btn-continue-game") {
      continue_game_click()
    }
  },css.locator = ".menu-btn",event = "click")
  if (app$glob$develop) {
    customEventHandler("refresh",fun = function(...) {set_page()},css.locator = "#refreshBtn",event = "click")
    customEventHandler("showSource",show_source,css.locator = "#showSourceBtn",event = "click")
  }
}

new_game_click = function(..., app=getApp()) {
  restore.point("new_game_click")
  pageid = app$glob$pages$pageid[1]
  set_page(pageid)
}

continue_game_click = function(..., app=getApp()) {
  restore.point("new_game_click")
  set_page(app$pageid)
}

image_area_click = function(value,...) {
  args = list(...)
  restore.point("image_area_click")

  next_pageid = value$href
  set_page(next_pageid)
}

last_text_click = function(..., app=getApp()) {
  restore.point("last_text_click")
  page = app$page
  if (is.null(page[["next"]])) return()
  next_pageid = page[["next"]]
  set_page(next_pageid)
}

get_story_pages = function(story.dir) {
  restore.point("get_story_pages")
  setwd(story.dir)
  files = list.files(story.dir, glob2rx("*.md"), recursive = TRUE)

  pages = tibble(pageid = tools::file_path_sans_ext(files), page.file = file.path(story.dir,files), sub.dir = str.left.of(files, "/", not.found = rep("", length(files))), pagebase = basename(files))
  pages
}


get_page = function(pageid, pages = getApp()$glob$pages) {
  restore.point("get_page")
  if (has.substr(pageid,"/")) {
    row = which(startsWith(pages$pageid,pageid))
  } else {
    row = which(startsWith(pages$pagebase,pageid))
  }
  if (length(row)==0) {
    stop(paste0("Could not find page ", pageid))
  }
  row = first(row)
  as.list(pages[row,])
}

set_page = function(pageid=app$pageid, app=getApp(), text_num=1) {
  restore.point("set_page")
  glob = app$glob

  page = get_page(pageid)
  spage = parse_story_page(page$page.file, glob$img.dir)

  page = c(page, spage)
  page = set.null.fields(page, c("img_map","question","script","textdiv","hide_text"),"")

  # One or more text fields (only the first will be shown initially)
  page$textdiv_id = "textdiv"

  text.df = page$text.df

  if (NROW(text.df)==1) {
    i=1
    page$text = text.df$html[[1]]
    page$text_type = text.df$type[[1]]
    if (text.df$wide[[i]]) {
      page[["text_class"]] = paste0("wide ", page[["text_class"]])
    }
    page$textdiv = rmdtools::replace.whiskers(glob$text.frag, page, eval=FALSE)
  } else if (NROW(text.df)>1) {
    text_divs = lapply(seq_len(NROW(text.df)), function(i) {
      page$text = text.df$html[[i]]
      page$textdiv_id = paste0("textdiv_",i)
      page$text_type = text.df$type[[i]]
      page[["text_class"]] = case_when(
        i==1 ~ "click-text",
        i <NROW(text.df) | !is.null(page[["next"]]) ~ "click-text hide-me",
        i==NROW(text.df) ~ "hide-me"
      )
      if (text.df$wide[[i]]) {
        page[["text_class"]] = paste0("wide ", page[["text_class"]])
      }

      rmdtools::replace.whiskers(glob$text.frag, page, eval=FALSE)
    })
    page$textdiv = paste0(text_divs, collapse="\n")
  }



  cont = rmdtools::replace.whiskers(glob$container.frag, page, eval=FALSE)
  page$container.html = cont

  app$page = page
  app$pageid = app$page$pageid
  update_story_progress()
  show_page()
  show_develop()
}

show_page = function(page = app$page, app=getApp()) {
  restore.point("show_page")
  glob = app$glob
  ui = tagList(
    HTML(page$container.html)
  )
  setUI("mainUI",ui)
}

show_develop = function(app=getApp()) {
  restore.point("show_develop")
  if (!app$glob$develop) return()
  page = app$page
  ui = tagList(
    actionButton("refreshBtn",paste0("Refresh ", page$pageid)),
    actionButton("showSourceBtn",paste0("Show source"))
  )
  setUI("developUI",ui)
}

show_source = function(..., app=getApp()) {
  restore.point("show_source")
  page = app$page
  if (is.null(page)) page = app$quiz
  rstudioapi::navigateToFile(page$page.file)
}

answer_click = function(values, ..., app = getApp()) {
  args = list(...)
  values

  page = app$page
  if (!is.null(page[["item.df"]])) {
    orgpos = ""
  } else {
    orgpos = paste0(page$item.df$org_pos, collapse=";")
  }
  ans = list(userid=app$userid, gameid=app$gameid, quizid = page$quizid, ok = values$check$ok, orgpos=orgpos, answer = values$answer, origin = "s", time=Sys.time())

  db = get.quizdb()
  dbInsert(db, "answer", ans)

}

load_story_progress = function(userid=app$userid, gameid = glob$gameid, app=getApp(), glob=app$glob) {
  restore.point("load_story_progress")
  db = get.quizdb()
  if (!is.null(userid)) {
    res = dbGet(db,"story_progress",list(userid=userid, gameid=gameid))
  } else {
    res = NULL
  }

  if (NROW(res)==0) {
    res = list(userid=userid, gameid=gameid, pageid =glob$pages$pageid[1])
  } else {
    res = as.list(res)
  }
}

update_story_progress = function(userid=app$userid, pageid = app$pageid, app=getApp(), glob=app$glob) {
  if (is.null(userid)) return()
  dat = tibble(userid, gameid=glob$gameid, pageid=pageid)
  db = get.quizdb()
  dbInsert(db, "story_progress", dat,mode="replace")

  dat$time = Sys.time()
  dbInsert(db, "story_log", dat)
}


