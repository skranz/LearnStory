$(document).on("click","#btn-sol",function(evt) {
  var answer = $("#input-text").val();
  check = check_sol(answer, solution);
  if (check.ok) {
    show_correct(check);
  } else {
    show_wrong(check);
  }

	Shiny.onInputChange("answer_click",
	  {eventId: "answer_click",id: "answer_click", value: {answer: answer, check: check}, nonce: Math.random()}
	);


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
  $("#btn-sol").addClass("hide-me");
  $("#show_wrong").addClass("hide-me");
  $("#show_correct").removeClass("hide-me");
  $("#btn-next").removeClass("hide-me");
}

// Define a function that replaces fields with object values
function replace_whiskers(str, obj) {
  // Use a regular expression to match fields in {{ }} format
  var regex = /{{(\w+)}}/g;
  // Use replaceAll method to replace matches with object values or original match
  return str.replace(regex, (match, key) => String(obj[key]) || match);
  //return str.replaceAll(regex, (match, key) => String(obj[key]) || match);
}
