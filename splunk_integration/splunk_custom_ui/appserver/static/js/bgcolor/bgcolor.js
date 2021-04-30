require([
  'jquery',
  'splunkjs/mvc',
  'splunkjs/mvc/tableview',
  'splunkjs/mvc/simplexml/ready!'],
function($, mvc, TableView) {
  var BGColorRenderer = TableView.BaseCellRenderer.extend({
    canRender: function(cell) {
      var returnVal = (cell.value != null && cell.value.startsWith("BGCOLOR_"));
      return returnVal;
    },
    render: function($td, cell) {
      var value = cell.value;
      value = value.replace("BGCOLOR_","");
      var process = value.substring(0, value.indexOf("_"));
      value = value.replace(process+"_","");
      var type = typeof(value);
      var type = isNaN(value)? "string" : "number";
      $td.text(value);

      if( type == "string" ) {
        var failures = ["MISSING", "FAIL", "FAILED", "F", "ABORT", "PARSE", "LINK", "ERR", "ERROR", "NO", "DNF", "DID NOT FINISH", "NOT RUN", "nr"];
        var successes = ["FOUND", "PASS", "PASSED", "P", "YES"];
        var warnings = ["WARNING", "PASS with ERRORS"];

        if( process == "ERROR" ) {
          if( failures.indexOf(value) != -1 ){
            $td.addClass('lt_red');
          } else if( successes.indexOf(value) != -1 ) {
            $td.addClass('lt_green');
          } else if ( warnings.indexOf(value) != -1 ) {
            $td.addClass('lt_yellow');
          }
        } else if( process == "ERROR.REVERSE" ) {
          if( failures.indexOf(value) != -1 ){
            $td.addClass('lt_green');
          } else if( successes.indexOf(value) != -1 ) {
            $td.addClass('lt_red');
          } else if ( warnings.indexOf(value) != -1 ) {
            $td.addClass('lt_yellow');
          }
        } else if( process == "WARN" ) {
          if( failures.indexOf(value) != -1 ){
            $td.addClass('lt_yelow');
          } else if( successes.indexOf(value) != -1 ) {
            $td.addClass('lt_green');
          } else if ( warnings.indexOf(value) != -1 ) {
            $td.addClass('lt_yellow');
          }
        } else if( process == "WARN.REVERSE" ) {
          if( failures.indexOf(value) != -1 ){
            $td.addClass('lt_green');
          } else if( successes.indexOf(value) != -1 ) {
            $td.addClass('lt_yellow');
          } else if ( warnings.indexOf(value) != -1 ) {
            $td.addClass('lt_yellow');
          }
        }
      } else if( type == "number" ) {
        if( process == "ERROR" ) {
          if( value == 0 ) {
            $td.addClass('lt_green');
          } else {
            $td.addClass('lt_red');
          }
        } else if( process == "ERROR.REVERSE" ) {
          if( value != 0 ) {
            $td.addClass('lt_green');
          } else {
            $td.addClass('lt_red');
          }
        } else if( process == "WARN" ) {
          if( value == 0 ) {
            $td.addClass('lt_green');
          } else {
            $td.addClass('lt_yellow');
          }
        } else if( process == "WARN.REVERSE" ) {
          if( value != 0 ) {
            $td.addClass('lt_green');
          } else {
            $td.addClass('lt_yellow');
          }
        } else if( process == "NEGSLACK" ) {
          if( value <= -1.0 ) {
            $td.addClass('lt_red');
          } else if( value <= -0.4 ) {
            $td.addClass('lt_orange');
          } else if( value <= -0.05 ) {
            $td.addClass('lt_yellow');
          } else if( value < 0.0 ) {
            $td.addClass('lt_greenyellow');
          } else if( value >= 0.0 ) {
            $td.addClass('lt_green');
          }
        }
      }
    }
  });

  mvc.Components.get('mxdHighlight').getVisualization(function(tableView) {
    tableView.addCellRenderer(new BGColorRenderer());
  });
});
