addin_show_md_page = function(...) {
  library(LearnStory)
  file = rstudioapi::getSourceEditorContext()$path
  if (!endsWith(file, ".md")) {
    cat("\nNo .md file is selected.")
    return()
  }

  restore.point("addin_show_md_page")
  try(stopApp())

  content = parse.yaml.md.file(file)

  is_story = "img" %in% names(content) | !("question" %in% names(content))

  img.dir = "C:/libraries/LearnStory/muenster/img"
  story.dir = quiz.dir = "C:/libraries/LearnStory/umwelt"
  pageid = tools::file_path_sans_ext(basename(file))

  if (is_story) {
    app = storyApp(story.dir, img.dir, start_pageid = pageid)
    viewApp(app,launch.browser = TRUE)
  } else {
    app = quizApp(quiz.dir, pageid = pageid, userid="test")
    viewApp(app)
  }


}

addin_make_gpt_prompt = function() {
  library(LearnStory)
  file = rstudioapi::getSourceEditorContext()$path
  restore.point("addin_make_gpt_prompt")
  #file = "C:/libraries/LearnStory/muenster/umwelt/chp2a/q2a_05_coase_gpt.md"

  if (!endsWith(file, ".md")) {
    cat("\nNo .md file is selected.")
    return()
  }

  page = parse.yaml.md.file(file)

  prompt = rmdtools::replace.whiskers(page$prompt, page, eval=FALSE)
  writeClipboard(prompt)
  cat(paste0("\n",prompt,"\n"))
}
