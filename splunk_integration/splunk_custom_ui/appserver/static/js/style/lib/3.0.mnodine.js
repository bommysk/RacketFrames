// Translations for en_US
i18n_register({"catalog": {}, "plural": function(n) { return n == 1 ? 0 : 1; }});

define(['underscore'], function(_) {

    function isColor(strColor) {
      return /(^#[0-9A-F]{6}$)|(^#[0-9A-F]{3}$)/i.test(strColor);
    }

    function calculate_range_class(value, range, color_class) {

      if (!value)
        return "";

      var last_index = range.length - 1;

      if (value < range[0]) {
        return color_class[0];
      }
      else if (value > range[last_index]) {
        return color_class[last_index + 1];
      }

      for (var i = 0; i < last_index; i++) {
        if (value >= range[i] && value <= range[i + 1]) {
          return color_class[i + 1];
        }
      }

      return "";
    }

    function calculate_mapping_class(value, mapping, color_class) {
      if (!value)
        return "";

      var mapping_length = mapping.length;

      for (var i = 0; i < mapping_length; i++) {
        if (RegExp(mapping[i]).test(value)) {
            return color_class[i];
        }
      }
      return "";
    }

    function get_ref_field_value(field, value, ref_field, rows) {
      for (idx in rows) {
        var row = rows[idx];
        if (field in row) {
          if (row[field] == value) {
            for (r in row) {
              if (RegExp(ref_field).test(r)) {
                return row[r];
              }
            }
          }
        }
        else {
          throw new Error("Field not in table.");
        }
      }
    }

    function setColorClass(field, value, $td, resultArray, rows) {
      for (var row in resultArray) {
          var threshold_field = resultArray[row]["Field"];

          if (!threshold_field)
            continue;

          let ref_field = resultArray[row]["Ref_Field"];
          let values = JSON.parse(resultArray[row]["Values"]);
          let color_class = JSON.parse(resultArray[row]["Color_Class"]);
          let type = resultArray[row]["Type"];

          if (RegExp(threshold_field).test(field)) {
            if (ref_field) {
              let ref_value = get_ref_field_value(field, value, ref_field, rows);

              if (ref_value) {
                value = ref_value;
              }
            }

          if (type == "range") {
            var range_value = calculate_range_class(value, values, color_class);
            if (isColor(range_value)) {
              $td.css("background-color", range_value);
            }
            else {
              $td.addClass(range_value);
            }
          }
          else if (type == "mapping") {
            let mapping_value = calculate_mapping_class(value, values, color_class);
            if (isColor(mapping_value)) {
              $td.css("background-color", mapping_value);
            }
            else {
              $td.addClass(mapping_value);
            }
          }
        }
      }
    }

    function get_group (field) {
      if (field.includes("::")) {
        return field.split("::")[0];
      }
      else {
        return "";
      }
    }

    function _getGroupClasses (myField, cellRenderer) {
      if (myField.substr(0,1) == "_") {
          return "";
      }
      var fieldArray = cellRenderer.fields;
      fieldArray = fieldArray.filter(item => item.substr(0,1) != "_");
      var firstInGroup = {};
      var lastInGroup  = {};
      var lastGroup = "";
      var lastField = "";
      fieldArray.forEach(function(field) {
        var thisGroup = get_group(field);
        firstInGroup[field] = thisGroup != "" && thisGroup != lastGroup ? true : false;
        lastInGroup[field] = false;
        if ( lastGroup != "" && thisGroup != lastGroup ) {
          lastInGroup[lastField] = true;
        }
        lastGroup = thisGroup;
        lastField = field;
      });
      
      var classes = [];
  
      if (firstInGroup[myField]) {
          classes.push("first-in-group");
      }
      if (lastInGroup[myField]) {
          classes.push("last-in-group");
      }
      return classes.join(",");
    }

  var getGroupClasses = _.memoize(_getGroupClasses);

  return {
    bg_color_text: function(value) {
      if (!value)
        return "";
      let class_name = "text";
      switch(value) {
        case "NA":
          class_name = "light";
          break;
        case "EX":
          class_name = "lt_blue";
        break;
      }

      return class_name;
    },
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
        case "Failed":
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
        case "UNDEFINED":
          class_name = "lt_red";
          break;
        case "FOUND":
        case "PASSED":
        case "passed":
        case "Passed":
        case "p":
        case "PASS":
        case "YES":
        case "DV":
        case "VG":
        case "PRN":
        case 0:
          class_name = "lt_green";
          break;
        case "WARNING":
        case "PASS with ERRORS":
        case "IN_PROGRESS":
        case "FBF":
        case "SNAP":
        case "CONT":
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
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n == 1)
        return "lt_green";
      else if (n == 0)
        return "lt_red";
      else if (n == -1)
        return "lt_red6";
      return "";
    },
    bg_color_sta_mem_area: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n < 5)
        return "lt_green";
      else if (n >= 5)
        return "lt_red";
      return "";
    },
    bg_color_gated_ff: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n < 25)
        return "lt_red";
      else if (n < 60)
        return "lt_orange";
     else if (n < 85)
        return "lt_yellow";
     else if (n >= 85)
        return "lt_green";
      return "";
    },
   bg_color_mbf: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n >= 80)
        return "lt_green";
      else if (n >= 70)
        return "lt_yellow";
     else if (n < 70)
        return "lt_red";
      return "";
    },
    bg_color_mtbf: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n <= 1.00E+3)
        return "lt_red";
      else if (n < 1.00E+6)
        return "lt_orange";
      else if (n >= 1.00E+6)
        return "lt_green";
      return "";
    },
    bg_color_atpg_cov_ramseq_turks: function(value) {
    if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n < 1 )
        return "lt_yellow";
      else if (n >= 1)
        return "lt_green";
      return "";
    },
   bg_color_atpg_cov_ramseq: function(value) {
    if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n < 2 )
        return "lt_yellow";
      else if (n >= 2)
        return "lt_green";
      return "";
    },
   bg_color_atpg_cov_extest_turks: function(value) {
    if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n < 1 )
        return "lt_yellow";
      else if (n >= 1)
        return "lt_green";
      return "";
    },
   bg_color_atpg_cov_comp_turks:function(value) {
   if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n <= 87 )
        return "lt_red";
      else if (n < 92)
        return "lt_orange";
      else if (n < 97)
        return "lt_yellow";
      else if (n <= 97)
        return "lt_green";
      return "";
   },
   bg_color_atpg_cov_comp_td_turks: function(value) {
   if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n <= 77)
        return "lt_red";
      else if (n < 87)
        return "lt_yellow";
      else if (n >= 87)
        return "lt_green";
      return "";
   },
   bg_color_atpg_cov_td_turks: function(value) {
   if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n <= 77)
        return "lt_orange";
      else if (n < 87)
        return "lt_yellow";
      else if (n >= 87)
        return "lt_green";
      return "";
   },
bg_color_atpg_cov_td: function(value) {
   if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
       if (n > 0)
        return "lt_green";
      else if (n == 0)
        return "lt_red";
      return "";
   },
   bg_color_reverse_warn: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n == 1)
        return "lt_green";
      else if (n == 0)
        return "lt_yellow";
      return "";
    },
      bg_color_post_fe_genport: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n <= 0)
        return "lt_green";
      else if (n > 0)
        return "lt_red";
      return "";
    },
    bg_color_io_wns: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n <= -0.4)
        return "lt_red";
      else if (n <= -0.3)
        return "lt_orange";
     else if (n <= -0.25)
        return "lt_yellow";
     else if (n < -0.2)
        return "lt_greenyellow";
     else if (n >= -0.2)
        return "lt_green";
      return "";
    },
    bg_color_reverse_more_warn: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n > 0)
        return "lt_green";
      else if (n == 0)
        return "lt_yellow";
      return "";
    },
    bg_color_reverse_more_error: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n > 0)
        return "lt_green";
      else if (n === 0)
        return "lt_red";
      return "";
    },
    bg_color_wns: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light"
      var n = Number(value);
      if (n <= -1.0)
        return "lt_red";
      else if (n <= -0.4)
        return "lt_orange";
      else if (n <= -0.05)
        return "lt_yellow";
      else if (n < 0.0)
        return "lt_greenyellow";
      else if (n >= 0.0)
        return "lt_green";
      return "";
    },
    bg_color_tns: function(value) {
      if (!value)
        return "";
      var n = Number(value);
      if (n <= -10.0)
        return "lt_red";
      else if (n <= -5.0)
        return "lt_orange";
      else if (n < -0.100)
        return "lt_yellow";
      else if (n < 0.0)
        return "lt_greenyellow";
      else if (n >= 0.0)
        return "lt_green";
      return "";
    },
    bg_color_gated_ff: function(value) {
      if (!value)
        return "";
      var n = Number(value);
      if (n < 25)
        return "lt_red";
      else if (n < 60)
        return "lt_orange";
      else if (n < 85)
        return "lt_yellow";
      else if (n >= 85)
        return "lt_green";
      return "";
    },
    bg_color_fep: function(value) {
      if (!value)
        return "";
      var n = Number(value);
      if (n >= 10000)
        return "lt_red";
      else if (n >= 1000)
        return "lt_orange";
      else if (n > 0)
        return "lt_yellow";
      else if (n === 0)
        return "lt_green";
      return "";
    },
    bg_color_mbf: function(value) {
      if (!value)
        return "";
      var n = Number(value);
      if (n >= 80.0)
        return "lt_green";
      else if (n >= 70.0)
        return "lt_yellow";
      else if (n < 70.0)
        return "lt_red";
      return "";
    },
    bg_color_fputil: function(value) {
      if (!value)
        return "";
      var n = Number(value);
      if (n >= 100.0)
        return "lt_red";
      else if (n >= 95.0)
        return "lt_orange";
      else if (n >= 85.0)
        return "lt_yellow";
      else if (n < 85.0)
        return "lt_green";
      return "";
    },
    bg_color_atpg_cov: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light";
      var n = Number(value);
      if (n <= 90.0)
        return "lt_red";
      else if (n < 97.0)
        return "lt_orange";
      else if (n >= 97.0)
        return "lt_green";
      return "";
    },
    bg_color_atpg_cov_scan_turks: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light";
      var n = Number(value);
      if (n <= 87.0)
        return "lt_orange";
      else if (n < 97.0)
        return "lt_yellow";
      else if (n >= 97.0)
        return "lt_green";
      return "";
    },
   bg_color_atpg_cov_scan: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light";
      var n = Number(value);
      if (n < 10)
        return "lt_red";
      else if (n >= 97.0)
        return "lt_green";
      else if (n >= 10)
        return "lt_yellow";
      return "";
    },
    bg_color_atpg_patterns: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light";
      var n = Number(value);
      if (n < 5000)
        return "lt_green";
      else if (n >= 5000)
        return "lt_yellow";
      return "";
    },
    bg_color_stage: function(value) {
      if (!value)
        return "";
      if (value == "NA")
        return "light";
      if (value == "init_design")
        return "stage1";
      if (value == "initdesign")
        return "stage1";
      if (value == "init")
        return "stage1";
      if (value == "init_icc2")
        return "stage1";
      if (value == "floorplan")
        return "stage2";
      if (value == "fp_filler")
        return "stage2";
      if (value.match(/fpfiller*/))
        return "stage2";
      if (value == "feedthrough")
        return "stage2";
      if (value == "place_opt")
        return "stage3";
      if (value == "placeopt")
        return "stage3";
      if (value == "clock_opt_cts")
        return "stage4";
      if (value == "clock_opt")
        return "stage4";
      if (value == "ccopt")
        return "stage4";
      if (value == "clock")
        return "stage4";
      if (value == "clock_opt_psyn")
        return "stage5";
      if (value == "clockopt")
        return "stage5";
      if (value == "clock_opt_route")
        return "stage5";
      if (value == "route")
        return "stage6";
      if (value == "route_opt")
        return "stage7";
      if (value == "routeopt")
        return "stage7";
      if (value == "route_opt2")
        return "stage8";
      if (value == "chip_finish")
        return "stage9";
      if (value == "finish")
        return "stage9";
      if (value.match(/chip_finish_eco*/))
        return "ios_skyeblue";
      if (value.match(/finisheco*/))
        return "ios_skyeblue";
      if (value.match(/finish_eco*/))
        return "ios_skyeblue";
      if (value.match(/eco*/))
        return "ios_skyeblue";
      if (value.match(/dp*/))
        return "ios_navyblue";
    },
    bg_lookup: function(cell, $td, resultArray, rows) {
        var field = cell.field;
        var value = cell.value;

        if (!value)
            return "";

        setColorClass(field, value, $td, resultArray, rows);

        return "";
    },
    calculate_group_classes: function(cellRenderer, cell) {
        return getGroupClasses(cell.field, cellRenderer);
    },

    isColor,
    calculate_range_class,
    calculate_mapping_class
  }

});
