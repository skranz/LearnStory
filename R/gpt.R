extract.story.text = function(dir) {
  dir = "C:/libraries/LearnStory/muenster/umwelt/chp1"

  files = list.files(dir, glob2rx("*.md"),full.names = TRUE)

  texts = lapply(files, function(file) {
    txt = readUtf8(file)

    obj = parse.page.md(txt)


    obj$question = "[FRAGE AUSGELASSEN]"
    obj$question = ""
    obj[names(obj) %in% c("text")]
  }) %>% unlist()

  txt = paste0(texts, collapse = "\n\n")
  txt = gsub("{{correct}}","",txt,fixed = TRUE)

  writeUtf8(txt, file.path(dir, "story.txt"))
}



make_prompt = function(answer) {


}
