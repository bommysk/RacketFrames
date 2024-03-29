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
        case 0:
          class_name = "lt_green";
          break;
        case "WARNING":
        case "PASS with ERRORS":
        case "IN_PROGRESS":
          class_name = "lt_yellow";
        break;
      }
      if (class_name === "")  {
        class_name = (Number(value) === 0 ? "lt_green" : "lt_red");
      }
      return class_name;
    },
    bg_color_warn: function(value) {
      if (!value)
        return "";
      var class_name = "";
      switch(value) {
        case "NA":
          class_name = "light";
          break;
        case 0:
          class_name = "lt_green";
        break;
      }
      if (class_name === "")  {
        class_name = (Number(value) == 0 ? "lt_green" : "lt_yellow");
      }
      return class_name;
    },
    bg_color_reverse_error: function(value) {
      if (!value)
        return "";
      var n = Number(value);
      if (n == 1)
        return "lt_green";
      else if (n == 0)
        return "lt_red";
      else if (n == -1)
        return "lt_red6";
      return "";
    }
  }
});
