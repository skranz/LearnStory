area.txt.to.json = function(file) {
  restore.point("area.txt.to.json")
  txt = readUtf8(file)
  txt = merge.lines(txt)
  html = rvest::read_html(txt)
  nodes = html %>% rvest::html_nodes("area")
  if (NROW(nodes)==0) return()

  df = bind_rows(html_attrs(nodes))
  df$id = paste0("area", seq_len(NROW(df)))

  df = ensure.cols(df,"title","")
  df = ensure.cols(df,"href","")
  df$link = df$href
  df = df[,c("id","link","title","shape", "coords")]

  txt = toJSON(df)
  txt = gsub("},","},\n",txt, fixed=TRUE)
  txt = gsub('"([a-zA-Z]+)":',"\\1: ",txt)
  txt

  dest.file = paste0(tools::file_path_sans_ext(file),"__areas.yaml")
  writeUtf8(txt, dest.file)

}

create.area.files = function(dir) {
  dir = "C:/libraries/LearnStory/muenster/img"
  files = list.files(dir, glob2rx("*.txt"), full.names = TRUE)

  file = files[1]
  for (file in files) {
    area.txt.to.json(file)
  }
}

