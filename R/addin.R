show_md_page = function(...) {
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
  viewApp(app)

}
