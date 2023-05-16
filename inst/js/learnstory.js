$(document).on("click","#sol-btn",function(evt) {
  var answer = $("#input-text").val();
  check = check_sol(answer, solution);
  if (check.ok) {
    show_correct(check);
    update_page_status("c");
  } else {
    show_wrong(check);
  }

	Shiny.onInputChange("ls_click",
	  {eventId: "ls_lick",id: "btn_answer", value: {answer: answer, check: check}, nonce: Math.random()}
	);

	evt.stopPropagation();

});




$(document).on("click","area",function(evt) {
  // Remove list item
  evt.preventDefault();
  var id = evt.currentTarget.id;
  var area;
  var status = page_status;

  for (var i=0; i < area_tab.length; i++) {
    if (area_tab[i].id == id) {
      area = area_tab[i];
      break;
    }
  }

  var link = area[[status+"_link"]];
  if (link==="")
    return;

  //alert("Area click!");
	Shiny.onInputChange("ls_click",
	  {eventId: "ls_click",id: "imgAreaClick", value: {linkid: id, href: link},nonce: Math.random()}
	);
});


// Click on text link
$(document).on("click",".text-div",function(evt) {
  show_next_text(true);

});


$(document).on("click",".abc-item",function(evt) {
  var el = evt.currentTarget;
  var id = el.id;
  var answer = $("#input-text").val();
  var item = id.substr(4,1);

  if (!$(el).hasClass("abc-active")) {
    $(el).addClass("abc-active");

    if (!answer.includes(item)) {
      $("#input-text").val(answer+item);
    }
  } else {
    $(el).removeClass("abc-active");
    $("#input-text").val(answer.replace(item,''));
  }
	evt.stopPropagation();
});

function show_next_text(was_click = false) {
  var text_divs = $(".text-div");
  var i = 0;
  while (i < text_divs.length-1) {
    textdiv = $(text_divs).eq(i);
    if (!textdiv.hasClass("hide-me")) {
      // Don't do anything for click on question
      if (was_click && textdiv.hasClass("type-question")) return;

      textdiv.addClass("hide-me");
      $(text_divs).eq(i+1).removeClass("hide-me");
      if (i+1 >= text_divs.length-1 && was_click) {
        update_page_status("c");
      }
      return;
    }
    i++;
  }
	// Last text element: send event to R
	Shiny.onInputChange("ls_click",
	  {eventId: "ls_lick",id: "last_text_click",nonce: Math.random()}
	);


}

function update_page_status(status) {
  page_status = status;
  set_areas_to_page_status();
}

function set_hidden_text(show_id, hidden_id, default_id, whiskers_val = null) {
  var el = document.getElementById("hidden_"+hidden_id);
  if (el === null) {
    el = document.getElementById("hidden_"+default_id);
  }
  var new_html = "";
  if (el !== null) {
    new_html = $(el).html();
  }

  if (whiskers_val !== null) {
    new_html = replace_whiskers(new_html, whiskers_val);
  }

  $("#"+show_id).html(new_html);
}

function show_wrong(check) {
  // example w_empty wrongtype = _empty
  let type = check.type;
  set_hidden_text("show_wrong", type,"wrong", check);
  $("#show_wrong").removeClass("hide-me");
}

function show_correct(check) {
  let type = check.type;
  set_hidden_text("show_correct", type,"correct", check);
  show_next_text(false);
  update_page_status("c");
}

function set_areas_to_page_status() {
  if (typeof area_tab === "undefined") {
    return;
  }
  var status = page_status;
  for (var i=0; i < area_tab.length; i++) {
    if (area_tab[i][status+"_show"]) {
      enable_area(area_tab[[i]].id, area_tab[[i]][status+"_title"],area_tab[[i]][status+"_link"]);
    } else {
      disable_area(area_tab[[i]].id, area_tab[[i]][status+"_title"],"");
    }
  }
}

function disable_area(id, title, href) {
  document.getElementById(id).setAttribute("onclick", "return false;");
  if (title !== undefined) {
    document.getElementById(id).setAttribute("title", title);
  }
  if (href !== undefined) {
    document.getElementById(id).setAttribute("href", href);
  }

}

function enable_area(id, title, href) {
  document.getElementById(id).setAttribute("onclick", "return true;");
  if (title !== undefined) {
    document.getElementById(id).setAttribute("title", title);
  }
  if (href !== undefined) {
    document.getElementById(id).setAttribute("href", href);
  }
}

// Define a function that replaces fields with object values
function replace_whiskers(str, obj) {
  // Use a regular expression to match fields in {{ }} format
  var regex = /{{(\w+)}}/g;
  // Use replaceAll method to replace matches with object values or original match
  return str.replace(regex, (match, key) => String(obj[key]) || match);
  //return str.replaceAll(regex, (match, key) => String(obj[key]) || match);
}
