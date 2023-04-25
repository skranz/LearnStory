
function check_sol(answer, sol) {
  var type = sol.type;

  if (type === "abc") {
    return check_abc_sol(answer, sol);
  }

  if (answer.trim()==="") {
    return  {ok: false, type: "wrong_empty"};
  }

  if (type == "numeric") {
    var val = parseNumber(answer);

    if (isNaN(val)) {
      return  {ok: false, type: "wrong_nan"};
    }
    smaller = "";

    if (sol.hasOwnProperty('value')) {
      if (val === sol.value) {
        return  {ok: true, type: "correct"};
      }
      smaller = val < sol.value;
    }
    if (sol.hasOwnProperty('min')) {
      if (val >= sol.min && val <= sol.max) {
        return  {ok: true, type: "correct"};
      }
      smaller = val < sol.min;
    }
    if (sol.hasOwnProperty('broad_min')) {
      if (val >= sol.broad_min && val <= sol.broad_max) {
        return  {ok: true, type: "correct_broad"};
      }
    }

     if (smaller === true) {
       return  {ok: false, type: "wrong_smaller"};
     } else if (smaller === false) {
       return  {ok: false, type: "wrong_larger"};
     }
  }
  return  {ok: false, type: "wrong"};
}


function check_abc_sol(ans, solutions) {
  // initialize an object to store the result
  let result = {ok: false, type: "abc", num_correct: 0, num_choices: solution.num_choices};
  let letters = "abcdefghijklmnopqrstuvwxyz";
  let is_correct = solution.sol;

  for (var i=0; i < solution.num_choices; i++) {
    var letter = letters[i];
    if ((ans.includes(letter) && is_correct[i]) || (!ans.includes(letter) && !is_correct[i])) {
      // increment the number of correct choices
      result.num_correct++;
    }
  }
  result.ok = (result.num_correct == result.num_choices);
  if (result.ok) {
    result.type = "correct_abc";
  } else {
    result.type = "wrong_abc";
  }
  return result;
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


