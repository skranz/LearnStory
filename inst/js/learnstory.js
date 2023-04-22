$(document).on("click","#sol-btn",function(evt) {
  var answer = $("#input-text").val();
  correct = check_sol(answer, solution);
  if (correct) {
    show_correct();
    update_page_status("c");
  } else {
    show_wrong();
  }

	Shiny.onInputChange("btnClick",
	  {eventId: "btnClick",id: "btn-sol",nonce: Math.random()}
	);

});


function check_sol(answer, sol) {
  var type = sol.type;
  if (type == "numeric") {
    var val = parseNumber(answer);
    if (sol.hasOwnProperty('min')) {
      if (val >= sol.min && val <= sol.max) {
        return(true);
      }
      if (val == sol.value) {
        return(true);
      }
    }
  }
  return false;
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
  var text_divs = $(".text-div");
  var i = 0;
  while (i < text_divs.length-1) {
    textdiv = $(text_divs).eq(i);
    if (!textdiv.hasClass("hide-me")) {
      // Don't do anything for qlick on question
      if (textdiv.hasClass("type-question")) return;

      textdiv.addClass("hide-me");
      $(text_divs).eq(i+1).removeClass("hide-me");
      if (i+1 >= text_divs.length-1) {
        update_page_status("c");
      }
      break;
    }
    i++;
  }
});

function update_page_status(status) {
  page_status = status;
  set_areas_to_page_status();
}

function show_wrong(msg) {
  if (msg !== undefined) {
    $("#wrong-answer").html(msg);
  }
  $("#wrong-answer").removeClass("hide-me");
}

function show_correct() {
  $("#wrong-answer").addClass("hide-me");
  $("#question").addClass("hide-me");
  $("#correct-answer").removeClass("hide-me");
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
  return Number(numericStrWithPeriods);
}
