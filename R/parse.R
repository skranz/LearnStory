
parse_story_page = function(page.file, img.dir=getApp()$glob$img.dir, app=getApp()) {
  restore.point("parse_story_page")
  txt = readUtf8(page.file)
  p = parse.page.md(txt)


  # Parse image map areas
  area.df = parse_areas(p$areas)

  if (NROW(area.df)>0) {
    p$area.df = area.df
    p$img_map = paste0('
<map name="image-map">
',paste0(area.df$html,collapse="\n"),'
</map>
')
    p$script = paste0(p$script, "\n",
      "var area_tab = ", toJSON(select(area.df, id, s_show, c_show, s_title, c_title, s_link, c_link)),";\n"
    )
  } else {
    p$img_map = ""
  }

  fields = names(p)

  if (!is.null(p[["solution"]])) {
    p$solution = parse_md_solution(p$solution)


    p$script = paste0(p$script, "\n",
      "var solution = ", toJSON(p$solution),";\n"
    )
  }

  inds = which(fields %in% c("text","question"))

  txt = unlist(p[inds]) %>% trimws()

  html = rep("", length(inds))
  rows = fields[inds]=="text"
  html[rows] = sapply(txt[rows], rmdtools::md2html, fragment.only = TRUE)

  rows = fields[inds]=="question"
  html[rows] = sapply(txt[rows], function(str) {
    vals = list(
      question = rmdtools::md2html(p$question, fragment.only = TRUE),
      wrong = rmdtools::md2html(p$wrong, fragment.only = TRUE),
      correct = rmdtools::md2html(p$correct, fragment.only = TRUE)
    )
    cat(rmdtools::replace.whiskers(app$glob$question.frag, vals, eval=FALSE))
    rmdtools::replace.whiskers(app$glob$question.frag, vals, eval=FALSE)
  })




  p$text.df = tibble(pos = seq_along(inds), type = fields[inds], txt = txt, html=html)

  question.row = which(p$text.df$type=="question")




  p
}

parse_img_map = function(html, links=NULL) {
  restore.point("parse_img_map")
  html = sep.lines(html)
  rows = has.substr(html, "<area ") | has.substr(html, "< area")
  txt = html[rows]

  #txt = gsub("title[\\s]*=","title=",txt)
  #str = str.right.of(txt,"title=")
  #title = str.between(str, '"','"')
  #names(links) = tolower(names(links))
  for (r in seq_along(txt)) {
    if (length(links)>=r) {
      txt[r] = gsub('href=""',paste0('href="', tolower(links[r]),'"'),txt[r], fixed=TRUE)
    }
    #link = links[[tolower(title[[r]])]]
  }
  html[rows] = txt
  list(map.html = merge.lines(html), areas=title)
}

parse.page.md = function(txt, hashdot = "#. ",...) {
  restore.point("parse.page.md")
  if (length(txt)==1) txt = sep.lines(txt)
  df = split.text.in.startline.blocks(txt, start.with = hashdot)
  li = list()
  start.txt = df$inner[1]
  if (nchar(start.txt)>0) {
    li = read.yaml(text = start.txt)
  }
  if (NROW(df)>1) {
    df = df[-1,]
    names = str.right.of(df$head, hashdot) %>% str.left.of(" ")
    li2 = as.list(df$inner)
    names(li2) = names

    li = c(li, li2)
  }
  li
}

parse_md_solution = function(str) {
  restore.point("parse_md_solution")
  sol = yaml.load(str)
  fields = names(sol)
  if ("min" %in% fields | "max" %in% fields) {
    sol$type = "numeric"
  }
  sol
}

parse_areas = function(str) {
  restore.point("parse_areas")
  areas = yaml.load(str)

  areas = lapply(areas, function(area) {
    area$s_link = first.none.null(area$s_link, area$link)
    area$s_title = first.none.null(area$s_title, area$s_link)

    area$c_title = first.none.null(area$c_title, area$title)
    area$c_link = first.none.null(area$c_link, area$link)

    area$s_show = !is.null(c(area$s_title, area$s_link))
    area$c_show = !is.null(c(area$c_title, area$c_link))

    html.tpl = '<area id="{{id}}" target="" alt="{{s_title}}" title="{{s_title}}" href="{{s_link}}" coords="{{coords}}" shape="{{shape}}">'
    area$html =  rmdtools::replace.whiskers(html.tpl, area, eval=FALSE)


    area
  }) %>% bind_rows()


  areas

}
