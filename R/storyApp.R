example = function() {
  story.dir = "C:/libraries/LearnStory/muenster/umwelt"
  img.dir = "C:/libraries/LearnStory/muenster/img"

  app = storyApp(story.dir, img.dir, start_pageid = "2001")
  viewApp(app)

}

storyApp = function(story.dir, img.dir, title="Münster Escape", start_pageid=NULL, develop=TRUE) {
  restore.point("storyApp")
  app=eventsApp(add.events = NULL)

  shiny::addResourcePath("img", img.dir)
  shiny::addResourcePath("js", system.file("js", package="LearnStory"))

  glob = app$glob
  glob$develop = develop
  glob$story.dir = story.dir
  glob$img.dir = img.dir
  glob$pages = get_story_pages(story.dir)

  frag.dir = system.file("html", package="LearnStory")
  head.html = readLines(file.path(frag.dir, "head_frag.html")) %>% merge.lines()

  glob$container.frag = readUtf8(file.path(frag.dir, "container_frag.html")) %>% merge.lines()

  glob$text.frag = readUtf8(file.path(frag.dir, "text_frag.html")) %>% merge.lines()

  glob$question.frag = readUtf8(file.path(frag.dir, "question_frag.html")) %>% merge.lines()

  app$ui = tagList(
    tags$title(title),
    tags$head(HTML(head.html)),
    includeCSS(file.path(story.dir,"/story.css")),
    if (develop) {
      sidebarLayout(uiOutput("developUI"), uiOutput("mainUI"))
    } else {
      uiOutput("mainUI")
    }
  )

  if (is.null(start_pageid))
    start_pageid = glob$pages$pageid[1]

  set_page(start_pageid)
  story_app_handlers()
  app
}

story_app_handlers = function(app=getApp()) {
  eventHandler("ls_click","imgAreaClick",image_area_click)
  eventHandler("ls_click","last_text_click",last_text_click)
  if (app$glob$develop) {
    customEventHandler("refresh",fun = function(...) {set_page()},css.locator = "#refreshBtn",event = "click")
    customEventHandler("showSource",show_source,css.locator = "#showSourceBtn",event = "click")
  }
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

set_page = function(pageid=app$page$pageid, app=getApp(), text_num=1) {
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
  rstudioapi::navigateToFile(page$page.file)
}
