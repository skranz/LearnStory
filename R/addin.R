addin_show_md_page = function(...) {
  library(LearnStory)
  file = rstudioapi::getSourceEditorContext()$path
  if (!endsWith(file, ".md")) {
    cat("\nNo .md file is selected.")
    return()
  }

  try(stopApp())

  img.dir = "C:/libraries/LearnStory/muenster/img"
  story.dir = "C:/libraries/LearnStory/muenster/umwelt"

  pageid = tools::file_path_sans_ext(basename(file))

  app = storyApp(story.dir, img.dir, start_pageid = pageid)
  viewApp(app,launch.browser = TRUE)

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

  page = parse_story_page(file)

  prompt = rmdtools::replace.whiskers(page$prompt, page, eval=FALSE)
  writeClipboard(prompt)
  cat(paste0("\n",prompt,"\n"))
}
