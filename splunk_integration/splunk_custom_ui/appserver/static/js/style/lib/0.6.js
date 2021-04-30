define(function() {

  return {
    bg_color_error: function(value) {
      if (!value)
        return "";
      var class_name = "";
      switch(value) {
        case "NA":
          class_name = "light";
          break;
        case "MISSING":
        case "FAIL":
        case "FAILED":
        case "failed":
        case "F":
        case "ABORT":
        case "PARSE":
        case "LINK":
        case "ERR":
        case "ERROR":
        case "NO":
        case "DNF":
        case "NOT_RUN":
        case "nr":
          class_name = "lt_red";
        break;
        case "FOUND":
        case "PASSED":
        case "passed":
        case "p":
        case "PASS":
        case "YES":
          class_name = "lt_green";
        break;
        case "WARNING":
        case "PASS with ERRORS":
          class_name = "lt_yellow";
        break;
      }
      if (Number.isNaN(value))  {
        class_name = (Number(value) == 0 ? "lt_green" : "lt_red");
        console.log("class_name = " + class_name);
      }
      return class_name;
    }
  }
});
