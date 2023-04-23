$(document).on("click","#sol-btn",function(evt) {
  var answer = $("#input-text").val();
  check = check_sol(answer, solution);
  correct = check.substr(0,1) == "c";

  if (correct) {
    show_correct(check);
    update_page_status("c");
  } else {
    show_wrong(check);
  }

	Shiny.onInputChange("btnClick",
	  {eventId: "btnClick",id: "btn-sol",nonce: Math.random()}
	);

});


function check_sol(answer, sol) {
  var type = sol.type;
  if (answer.trim()==="") {
    return "w_empty";
  }

  if (type == "numeric") {
    var val = parseNumber(answer);

    if (isNaN(val)) {
      return "w_nan";
    }
    smaller = "";

    if (sol.hasOwnProperty('value')) {
      if (val === sol.value) {
        return("c");
      }
      smaller = val < sol.value;
    }
    if (sol.hasOwnProperty('min')) {
      if (val >= sol.min && val <= sol.max) {
        return("c");
      }
      smaller = val < sol.min;
    }
    if (sol.hasOwnProperty('broad_min')) {
      if (val >= sol.broad_min && val <= sol.broad_max) {
        return("c_broad");
      }
    }

     if (smaller === true) {
       return("w_smaller");
     } else if (smaller === false) {
       return("w_larger");
     }
  }
  return "w";
}



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
	Shiny.onInputChange("imgAreaClick",
	  {eventId: "imgAreaClick",id: "imgAreaClick", value: {linkid: id, href: link},nonce: Math.random()}
	);
});


// Click on text link
$(document).on("click",".text-div",function(evt) {
  show_next_text(true);
});

function show_next_text(was_click = false) {
  var text_divs = $(".text-div");
  var i = 0;
  while (i < text_divs.length-1) {
    textdiv = $(text_divs).eq(i);
    if (!textdiv.hasClass("hide-me")) {
      // Don't do anything for qlick on question
      if (was_click && textdiv.hasClass("type-question")) return;

      textdiv.addClass("hide-me");
      $(text_divs).eq(i+1).removeClass("hide-me");
      if (i+1 >= text_divs.length-1 && was_click) {
        update_page_status("c");
      }
      break;
    }
    i++;
  }
}

function update_page_status(status) {
  page_status = status;
  set_areas_to_page_status();
}

function set_hidden_text(show_id, hidden_id, default_id) {
  var el = document.getElementById("hidden_"+hidden_id);
  if (el === null) {
    el = document.getElementById("hidden_"+default_id);
  }
  var new_html = "";
  if (el !== null) {
    new_html = $(el).html();
  }

  $("#"+show_id).html(new_html);
}

function show_wrong(type) {
  // example w_empty wrongtype = _empty
  set_hidden_text("show_wrong", "wrong"+type.substr(1),"wrong");
  $("#show_wrong").removeClass("hide-me");
}

function show_correct(type) {
  set_hidden_text("show_correct", "correct"+type.substr(1),"correct");
  show_next_text(false);
  update_page_status("c");
}

function set_areas_to_page_status() {
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

function check_math(user, sol, vals) {
  var res_user;
  for (var i = 0; i < vals.length; i++) {
    val = vals[[i]];
    try {
      res_user = math.eval(user, val);
    } catch(error) {
      return({ok: false, error: error});
    }
    res_sol = math.eval(sol, val);
    if (res_user !== res_sol) {
      return({ok: false});
    }
  }
  return({ok: true});
}


function parseNumber(str) {
  // remove all non-numeric characters except commas and periods from the string
  const numericStr = str.replace(/[^0-9,.]/g, '');

  // replace commas with periods
  const numericStrWithPeriods = numericStr.replace(/,/g, '.');

  // parse the numeric string as a number and return it
  return parseFloat(numericStrWithPeriods);
}
