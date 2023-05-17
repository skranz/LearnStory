
parse_story_page = function(page.file, img.dir=glob$img.dir, app=getApp(), story.dir  = glob$story.dir,pages = glob$pages, glob=getApp()$glob) {
  restore.point("parse_story_page")
  txt = readUtf8(page.file)
  p = parse.page.md(txt)


  # Include linked exercise
  ex_row = which(names(p)=="exercise")
  if (length(ex_row)>0) {
    ex_row = ex_row[[1]]
    ex_id = trimws(p[[ex_row]])
    ex_page = get_page(ex_id)
    ex_txt = readUtf8(ex_page$page.file)
    ex_li = parse.page.md(ex_txt)
    p = insert.into.list(p, ex_li, ex_row, overwrite.pos = TRUE)
    p$quizid = tools::file_path_sans_ext(ex_id)
  } else {
    p$quizid = tools::file_path_sans_ext(basename(page.file))
  }

  default_page = NULL
  try(default_page <- get_page("defaults"), silent=TRUE)
  if (!is.null(default_page)) {
    def_txt = readUtf8(default_page$page.file)
    defaults = parse.page.md(def_txt)
    fields = setdiff(names(defaults), names(p))
    p[fields] = defaults[fields]
  }

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

  p = parse_quiz_elements(p)
  p = prepare_page_quiz(p)

  fields = names(p)
  inds = which(fields %in% c("text","question"))

  txt = unlist(p[inds]) %>% trimws()

  # If no text element follows a question
  # generate one that contains the text shown under
  # correct
  if (isTRUE(fields[max(inds)] == "question")) {
    fields = c(fields, "text")
    inds = c(inds, NROW(fields))
    txt = c(txt, "{{correct}}")
  }



  html = rep("", length(inds))
  rows = fields[inds] %in% c("text")

  # Special replacements for text
  txt[rows] = gsub("{{correct}}",'<div id="show_correct" class="correct"></div>', txt[rows], fixed=TRUE)
  html[rows] = sapply(txt[rows], rmdtools::md2html, fragment.only = TRUE)

  rows = fields[inds]=="question"
  html[rows] = sapply(txt[rows], function(str) {
    vals = list(
      question = rmdtools::md2html(p$question, fragment.only = TRUE)
    )
    #cat(rmdtools::replace.whiskers(app$glob$question.frag, vals, eval=FALSE))
    rmdtools::replace.whiskers(app$glob$question.frag, vals, eval=FALSE)
  })

  p$text.df = tibble(pos = seq_along(inds), type = fields[inds], txt = txt, html=html, wide=nchar(txt)>700)

  if (isTRUE(p$quiz_type %in% c("abc","statements"))) {
    p$text.df$wide[p$text.df$type == "question"]= TRUE
  }

  question.row = which(p$text.df$type=="question")

  p = add_page_hidden_html(p)


  p
}

parse_quiz_page = function(page.file, app=getApp(), quiz.dir  = glob$quiz.dir, glob=getApp()$glob) {

  restore.point("parse_quiz_page")
  txt = readUtf8(page.file)
  p = parse.page.md(txt)

  default.file = file.path(quiz.dir, "defaults.md")
  if (file.exists(default.file)) {
    defaults = parse.yaml.md.file(default.file)
    fields = setdiff(names(defaults), names(p))
    p[fields] = defaults[fields]
  }


  p = parse_quiz_elements(p)
  p = prepare_page_quiz(p)

  p$question.txt  = p$question
  p$question = md2html(p$question, fragment.only=TRUE)

  p = add_page_hidden_html(p)
  p$quizid = tools::file_path_sans_ext(basename(page.file))

  p
}


add_page_hidden_html = function(page) {
  restore.point("add_page_hidden_html")
  hidden = startsWith(names(page), "correct") |
           startsWith(names(page), "wrong") |
           startsWith(names(page), "help")


  hidden = which(hidden)
  if (length(hidden)==0) return(page)

  fields = names(page)[hidden]
  correct_fields = fields[startsWith(fields,"correct")]
  if (length(correct_fields)>0 & !is.null(page$add_correct)) {
    page[correct_fields] = as.list(paste0(page[correct_fields],"\n\n", page$add_correct))
  }

  page[hidden] = lapply(page[hidden],rmdtools::md2html, fragment.only=TRUE)

  html = paste0('<div id="hidden_', fields, '">\n', page[hidden],"\n</div>", collapse="\n")

  page$hidden_html = html
  page

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

parse.yaml.md.file = function(file) {
  txt = readUtf8(file)
  parse.page.md(txt)
}

parse.page.md = function(txt, hashdot = "#. ",...) {
  restore.point("parse.page.md")
  if (length(txt)==1) txt = sep.lines(txt)
  df = split.text.in.startline.blocks(txt, start.with = hashdot)
  li = list()

  has.start = isTRUE(df$head[[1]] == "START" | df$head[[1]]=="")
  if (has.start) {
    start.txt = df$inner[1]
    if (nchar(start.txt)>0) {
      li = yaml.load(start.txt)
    }
  }
  if (NROW(df)>as.integer(has.start)) {
    df = df[-1,]
    head = str.right.of(df$head, hashdot) %>% trimws()
    arg = str.right.of(head, " ", not.found = rep("", length(head)))
    names =  str.left.of(head, " ")
    df$inner = paste0(arg," ", df$inner) %>% trimws()
    li2 = as.list(df$inner)
    names(li2) = names
    li = c(li, li2)
  }
  li
}


parse_areas = function(str) {
  restore.point("parse_areas")
  areas = yaml.load(str)

  areas = lapply(areas, function(area) {
    area$s_link = first.none.null(area$s_link, area$link, "")
    area$s_title = first.none.null(area$s_title, area$s_link,"")

    area$c_title = first.none.null(area$c_title, area$title,"")
    area$c_link = first.none.null(area$c_link, area$link,"")

    area$s_show = !all(c(area$s_title, area$s_link)=="")
    area$c_show = !all(c(area$c_title, area$c_link)=="")

    html.tpl = '<area id="{{id}}" target="" alt="{{s_title}}" title="{{s_title}}" href="{{s_link}}" coords="{{coords}}" shape="{{shape}}">'
    area$html =  rmdtools::replace.whiskers(html.tpl, area, eval=FALSE)


    area
  }) %>% bind_rows()


  areas

}


parse_quiz_elements = function(p) {
  restore.point("parse_quiz_elements")

  p$has_quiz = !is.null(p[["question"]]) | !is.null(p[["statements"]])

  if (!p$has_quiz) return(p)

  if (!is.null(p[["solution"]])) {
    sol = yaml.load(p$solution)
  } else {
    sol = list()
  }
  p$solution = sol

  qu_text = sep.lines(p$question)
  if (!is.null(sol$type)) {
    p$quiz_type = sol$type
  } else if ("statements" %in% names(p)) {
    p$quiz_type = "statements"
  } else if (any(c("min","max") %in% names(sol))) {
    p$quiz_type = "numeric"
  } else if (any(startsWith(qu_text,"a)"))) {
    p$quiz_type = "abc"
  } else {
    p$quiz_type = ""
  }
  p$solution$type = p$quiz_type

  if (!p$quiz_type %in% c("statements","abc","numeric")) {
    stop("Could not detect quiz type 'statements', 'abc' or 'numeric'")
  }

  if (p$quiz_type == "abc") {
    res = parse_quiz_items(p$question,modes="abc")
    p$question_head = res$head
    p$question_tail = res$tail
    p$item.df = res$item.df
  } else if (p$quiz_type == "statements") {
    res = parse_quiz_items(p$statements,modes="abc")
    p$question_head = p$question
    p$question_tail = ""
    p$item.df = res$item.df
  }

  p
}


# Sample quiz questions and then perform
# all preparations
prepare_page_quiz = function(p) {
  restore.point("prepare_page_quiz")
  if (!p$has_quiz) return(p)

  if (p$quiz_type == "numeric") {
    p$script = paste0(p$script, "\n",
      "var solution = ", toJSON(p$solution,auto_unbox = TRUE),";\n"
    )
    return(p)
  }

  # Sample question items
  item.df = p$item.df

  has_abc = first(item.df$org_abc != "")

  abc = item.df$org_abc
  keep_order = isTRUE(p$keep_order)
  stratified = has_abc & any(duplicated(abc))

  if (p$quiz_type == "abc") {
    num_choices = length(unique(abc))
  } else {
    num_choices = first.non.null(p$solution$num_choices, 4)
  }

  if (keep_order) {
    sample.df = item.df
  } else if (!stratified) {
    rows = sample(1:NROW(item.df), num_choices, replace=FALSE)
    sample.df = item.df[rows,]
  } else if (stratified) {
    sample_abc = sample(unique(abc), num_choices)
    sample.df = item.df[item.df$org_abc %in% sample_abc,]
    sample.df$prio = sample.int(NROW(sample.df))
    sample.df = sample.df %>%
      group_by(org_abc) %>%
      filter(prio == max(prio)) %>%
      ungroup() %>%
      select(-prio)
    rows = sample.int(NROW(sample.df))
    sample.df = sample.df[rows,]
  }
  sample.df$pos = 1:NROW(sample.df)
  sample.df$abc = letters[sample.df$pos]


  # Create item div
  sample.df = sample.df %>%
    mutate(
      div = paste0('<div class="abc-item" id="abc-',abc,'">', abc,") ", item, "</div>")
    )

  # Recreate question

  p$question = paste0(
    p$question_head,
    paste0(sample.df$div, collapse="\n"),
    p$question_tail
  )

  p$solution$value = paste0(sample.df$abc[sample.df$correct])
  p$solution$type = p$quiz_type
  p$solution$num_choices = NROW(sample.df)


  correct = rep(sample.df$correct)
  names(correct) = sample.df$abc
  p$solution$correct = correct

  p$sample.df = sample.df

  p$script = paste0(p$script, "\n",
    "var solution = ", toJSON(p$solution,auto_unbox = TRUE),";\n"
  )

  rows = which(sample.df$info != "")
  if (NROW(rows)>0) {
    p$add_correct = paste0(" zu ", sample.df$abc[rows],") ", sample.df$info[rows],collapse="\n\n")
  }

  return(p)
}


parse_quiz_items = function(txt, modes = c("abc","-")) {
  restore.point("parse_quiz_items")
  txt = sep.lines(txt,"\n")
  trim = c(trimws(txt),"")

  abc.item.rows = which(grepl("^[a-z]\\)", trim))
  hyphen.item.rows = which(startsWith(trim, "-"))

  if (length(modes)>=2) {
    mode = ifelse(length(abc.item.rows)>length(hyphen.item.rows), "abc","-")
  } else {
    mode = modes
  }



  if (mode =="abc") {
    item.rows = abc.item.rows
    abc = substr(trim[item.rows],1,1)
    trim[item.rows] = trimws(substring(trim[item.rows],3))
  } else {
    item.rows = hypthen.item.rows
    trim[item.rows] = trimws(substr(trim[item.rows],2))
    abc = ""
  }
  empty.rows = which(trim=="")

  start.rows = item.rows
  end.rows = lead(item.rows-1)
  info.rows = which(startsWith(trim, "info: "))
  if (any(empty.rows>max(c(start.rows, info.rows)))) {
    end.rows[length(end.rows)] = min(empty.rows[empty.rows>max(c(start.rows, info.rows))])
  } else {
    end.rows[length(end.rows)] = length(txt)
  }

  org.end.rows = end.rows
  info = rep("", length(item.rows))
  for (ii in seq_along(info.rows)) {
    irow = info.rows[ii]
    i = which(start.rows < irow & end.rows >= irow)
    info[i] = merge.lines(trim[irow:end.rows[i]]) %>% trimws() %>% substring(7)
    end.rows[i] = irow-1
  }
  info[info!=""] = sapply(info[info!=""], md2html, fragment.only=TRUE)

  info[info!=""] = sapply(info[info!=""], function(str) {
    str = md2html(str,fragment.only=TRUE)
    str = gsub("<p>","", str, fixed=TRUE)
    str = gsub("</p>","", str, fixed=TRUE)
    trimws(str)
  })

  items = sapply(seq_along(start.rows), function(i) {
    trimws(merge.lines(trim[start.rows[i]:end.rows[i]]))
  })

  correct = endsWith(items,"*")
  items[correct] = str.remove.ends(items[correct], right=1)

  items = sapply(items, function(item) {
    str = md2html(item,fragment.only=TRUE)
    str = gsub("<p>","", str, fixed=TRUE)
    str = gsub("</p>","", str, fixed=TRUE)
    trimws(str)
  })

  item.df = tibble(org_pos= seq_along(items), org_abc=abc, correct=correct, item=items, info=info)

  head = tail = ""
  if (start.rows[1] > 1) {
    head = merge.lines(txt[1:(start.rows[1]-1)])
  }
  if (last(org.end.rows)< length(txt)) {
    tail = merge.lines(txt[(last(org.end.rows)+1):length(txt)])
  }

  list(item.df=item.df, head=head, tail=tail)
}

