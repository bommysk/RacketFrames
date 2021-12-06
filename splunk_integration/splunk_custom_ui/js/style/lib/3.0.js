define(['underscore','splunkjs/mvc'], function(_, mvc) {

    function isColor(strColor) {
      return /(^#[0-9A-F]{6}$)|(^#[0-9A-F]{3}$)/i.test(strColor);
    }

    function calculate_range(value, range, range_dict) {

      if (!value)
        return "";

      var last_index = range.length - 1;

      if (value < range[0]) {
        return range_dict[0];
      }
      else if (value > range[last_index]) {
        return range_dict[last_index + 1];
      }

      for (var i = 0; i < last_index; i++) {
        if (value >= range[i] && value <= range[i + 1]) {
          return range_dict[i + 1];
        }
      }

      return "";
    }

    // backwards compatibility
    let calculate_range_class = calculate_range;

    function calculate_mapping(value, mapping, mapping_dict) {
      if (!value)
        return "";

      var mapping_length = mapping.length;

      for (var i = 0; i < mapping_length; i++) {
        if (RegExp(mapping[i]).test(value)) {
            return mapping_dict[i];
        }
      }
      return "";
    }

    // backwards compatibility
    let calculate_mapping_class = calculate_mapping;

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
      try {
        for (let row in resultArray) {
          let threshold_field = resultArray[row]["Field"];

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

            if (type && type === "range") {
              let range_value = calculate_range(value, values, color_class);
              if (isColor(range_value)) {
                $td.css("background-color", range_value);
              }
              else {
                $td.addClass(range_value);
              }
            }
            else if (type && type === "mapping") {
              let mapping_value = calculate_mapping(value, values, color_class);
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
      catch (err) {
        console.log(err);
        return false;
      }
    }

    function getTooltip(field, value, resultHash, rows) {
      try {
        for (let threshold_field in resultHash) {
          let tooltip_obj = resultHash[threshold_field];
          let ref_field = tooltip_obj["Ref_Field"];
          let values = tooltip_obj["Values"];
          if (values) {
            values = JSON.parse(values);
          }          
          let tooltip = tooltip_obj["Tooltip"];     
          let type = tooltip_obj["Type"];

          if (RegExp(threshold_field).test(field)) {
            if (ref_field) {
              let ref_value = get_ref_field_value(field, value, ref_field, rows);

              if (ref_value) {
                value = ref_value;
              }
            }

            if (type && type === "range") {
              let range_value = calculate_range(value, values, tooltip);
              return range_value;
            }
            else if (type && type === "mapping") {
              let mapping_value = calculate_mapping(value, values, tooltip);
              return mapping_value;
            }

            return tooltip;
          }
        }
      }
      catch (err) {
        console.log(err);
        return err;
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

    let set_drilldown = (cell, $td, link) => {        
      if (link && cell &&
        (cell.value && cell.value.toString().toLowerCase() !== "na" && cell.value.toString().toLowerCase() !== "ex")) {
        let a;          

        $td.addClass('table-link');

        // tooltip has been applied 
        if ($td.find('[data-toggle="tooltip"]').length) {            
          a = $td.find('a');
          a.attr("href", link).attr("target", "_blank");            
        }
        else {          
          a = $('<a>').attr("href", link).attr("target", "_blank").text(cell.value);
          $td.empty().append(a);
        }

        a.click(function(e) {
          e.preventDefault();
          //window.location = $(e.currentTarget).attr('href');
          // or for popup:
          window.open($(e.currentTarget).attr('href'));
        });
      }
    }

    let set_tooltip = ($td, tip, tooltip_placement, full_value) => {        
      if (tip && tooltip_placement &&
        (full_value && full_value.toString().toLowerCase() !== "na" && full_value.toString().toLowerCase() !== "ex")) {
        // custom drilldown has been applied 
        if ($td.find('a').length) {            
          let a = $td.find('a');
          let href = a.attr('href');

          let html_value = `<a href="${href}" target="_blank" data-toggle="tooltip" data-placement="${tooltip_placement}" data-original-title="${tip}">${full_value}</a>`;            

          $td.html(html_value);
          $td.find('a').click(function(e) {
            e.preventDefault();
            //window.location = $(e.currentTarget).attr('href');
            // or for popup:
            window.open($(e.currentTarget).attr('href'));
          });
        }
        else {            
          html_value = `<a href="#" data-toggle="tooltip" data-placement="${tooltip_placement}" data-original-title="${tip}">${full_value}</a>`;  
          $td.html(html_value);
        }                
        
        // This line wires up the Bootstrap tooltip to the cell markup
        // This won't do anything if tooltips weren't created above
        $td.find('[data-toggle="tooltip"]').tooltip({
          html: true,
          container: 'body'
        });          
      }
    } 

  function eval_link_expr(eval_token_dict, link_expr, row) {
    if (! link_expr.includes("$")) {
      return link_expr;
    }

    const re = /\$([^=]+?)\$/g;
    let m, token_name_extracted,token_name_extracted_orig;
    let default_token_model = mvc.Components.get("default");

    do {
        m = re.exec(link_expr);
        if (m) {
          token_name_extracted = m[1];
          if (token_name_extracted.startsWith("row.")) {
            // escape period because it is a special character in regex
            token_name_extracted_orig = token_name_extracted;
            token_name_extracted = 'row\\.' + token_name_extracted_orig.split('.')[1];
            let row_token_value = row[token_name_extracted_orig.split('.')[1]];
            //if (default_token_model.get('developer_mode_enabled')) console.log(`Token replace: $${token_name_extracted}$ with: ` + eval(token_name_extracted_orig));
            link_expr = link_expr.replace(new RegExp(`\\$${token_name_extracted}\\$`, 'g'), row_token_value);
          }
          else {
            let eval_token_dict_value = eval_token_dict[`$${token_name_extracted}$`];

            if (! eval_token_dict_value) {
              eval_token_dict_value = default_token_model.get(token_name_extracted);
            }
            //console.log(`Token replace: $${token_name_extracted}$ with: ` + eval_token_dict_value);
            link_expr = link_expr.replace(new RegExp(`\\$${token_name_extracted}\\$`, 'g'), eval_token_dict_value);
          }
        }
        //console.log(`Link Expr: ${link_expr}`);
    } while (m);

    return link_expr;
  }

  function eval_token_value_expr(eval_token_dict, token_name, token_value_expr, row) {
    if (! token_value_expr.includes("$")) {
      return eval(token_value_expr);
    }

    const re = /\s*\$([^=]+)\$\s*/g;
    let m, token_name_extracted,token_name_extracted_orig;

    do {
        m = re.exec(token_value_expr);
        if (m) {
          token_name_extracted = m[1];
          if (token_name_extracted.startsWith("row.")) {
            // escape period because it is a special character in regex
            token_name_extracted_orig = token_name_extracted;
            token_name_extracted = 'row\\.' + token_name_extracted_orig.split('.')[1];
            let row_token_value = row[token_name_extracted_orig.split('.')[1]];
            //console.log(`Token replace: $${token_name_extracted}$ with: ` + eval(token_name_extracted_orig));
            token_value_expr = token_value_expr.replace(new RegExp(`\\$${token_name_extracted}\\$`, 'g'), row_token_value);
          }
          else {
            //console.log(`Token replace: $${token_name_extracted}$ with: ` + eval_token_dict[`$${token_name_extracted}$`]);
            token_value_expr = token_value_expr.replace(new RegExp(`\\$${token_name_extracted}\\$`, 'g'), eval_token_dict[`$${token_name_extracted}$`]);
          }
        }
        //console.log(`Token Value Expr: ${token_value_expr}`);
    } while (m);

    eval_token_dict[`$${token_name}$`] = eval(token_value_expr);

    return eval_token_dict[`$${token_name}$`];
  }

  // drilldown_lookup_obj maps to a row in the custom drilldown lookup and has all the information
  // needed to construct an anchor link
  function construct_drilldown_link(cell, $td, drilldown_lookup_obj, row) {
    let field = cell.field;
    let value = cell.value;

    if (!value)
        return "";

    // construct link
    // check if field regex matches cell.field
    // if not return ""
    // if yes then construct link
    try {
      // [{Field, Eval_Tokens, Link_Expression}...]
      for (let drilldown_lookup_row in drilldown_lookup_obj) {
        let threshold_field = drilldown_lookup_obj[drilldown_lookup_row]["Field"];

        if (!threshold_field)
          continue;

        if (RegExp(threshold_field).test(field)) {
          let custom_link = "";
          let eval_token_dict = {};
          // array of string expressions ["$token$=value","$token$=value","$token$=value"...]
          let eval_tokens = eval(drilldown_lookup_obj[drilldown_lookup_row]["Eval_Tokens"]);

          if (! eval_tokens) {
            eval_tokens = [];
          }

          //eval_tokens format: [[token_name,token_value],[token_name,token_value],[token_name,token_value]...]
          for (let eval_token of eval_tokens) {
            let [token_name, token_value] = eval_token;
            token_name = token_name.trim();
            token_value = token_value.trim();

            eval_token_dict[token_name] = eval_token_value_expr(eval_token_dict, token_name, token_value, row);
          }

          let link_expression = new URL(drilldown_lookup_obj[drilldown_lookup_row]["Link_Expression"], document.baseURI).href.replace(/amp;/g, '');

          if (! link_expression) {
            return false;
          }

          link_expression = eval_link_expr(eval_token_dict, link_expression, row);          

          if (! link_expression) {
            return false;
          }

          return link_expression;
        }
      }
    } catch  (err) {
      console.log(err);
      return false;
    }

    return "";
  }
 

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
    tooltip_lookup: function(cell, resultHash, rows) {
        var field = cell.field;
        var value = cell.value;

        if (!value)
            return "";

        return getTooltip(field, value, resultHash, rows);        
    },
    calculate_group_classes: function(cellRenderer, cell) {
        return getGroupClasses(cell.field, cellRenderer);
    },

    isColor,
    calculate_range,
    calculate_range_class,
    calculate_mapping,
    calculate_mapping_class,
    set_drilldown,
    set_tooltip,
    eval_link_expr,
    eval_token_value_expr,
    construct_drilldown_link
  }
});
