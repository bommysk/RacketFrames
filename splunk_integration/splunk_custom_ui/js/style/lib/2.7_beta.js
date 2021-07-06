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
    bg_color_power_collector: function(value) {
        console.log("HRE");
      if (!value)
        return "";
      if (value == "NA")
        return "light";
      var n = Number(value);
      if (n < 0.7)
        return "green";
      else if (n < 1.05)
        return "lt_green";
      else if (n >= 1.05 && n < 1.2)
        return "lt_red";
      else if (n >= 1.2)
        return "red";

      return "";
    }
  }
});
