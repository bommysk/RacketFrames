var libs = [
  "underscore",
  "jquery",
  "splunkjs/mvc",
  "splunkjs/mvc/simplexml/element/table",
  "splunkjs/mvc/simplexml/ready!",
];

var calculate_rowspan_maxdepth = function (table_headers) {
  let maxdepth = 0;
  table_headers.each(function (i, th) {
    if (th.innerText.includes("::")) {
      let depth = th.innerText.split("::").length;
      if (depth > maxdepth) {
        maxdepth = depth;
      }
    }
  });

  return maxdepth;
};

var get_path = function (str, level) {
  if (!str.includes("::")) {
    return str;
  }
  let nodes = str.split("::");
  let len = nodes.length;
  nodes = nodes.slice(0, level + 1);
  return nodes.join("::");
}

var format_newlines = function (table_headers) {
  table_headers.each(function (i, th) {
    let str = th.innerText;
    if (str.length === 0) {
      return;
    }
    $(th).find("a").get(0).childNodes[0].nodeValue = str;
    $(th).find("a").html($(th).find("a").html().replace(/\\n/g,'<br>'));
  });
}

var get_name = function (str, level) {
  if (!str.includes("::")) {
    return str;
  }
  
  var nodes = str.split("::");
  return nodes[level];
}

var get_rowspan = function (str, level, maxdepth) {
  let depth = str.split("::").length;
  if (depth == maxdepth || level + 1 != depth) {
    return 0;
  }
  return maxdepth - depth + 1;
}

var format_counter = 0;
var timedCount = function (subview) {  
  format_headers(subview);  
}

var is_formatted = function (subview) {
  let view_id = $(subview).attr("id");
  let table_headers = $("#" + view_id + " table thead tr th");

  let result = true;
  table_headers.each(function (i, th) {
    if (th.innerText.includes("::") || th.innerText.match(/\\n/g)) {      
      result = false;
    }
  });

  return result;
}

var triggered_events = {};
var format_headers = function (subview) {
  try {
    if (!is_formatted(subview)) {      
      var view_id = $(subview).attr("id");
      var table_headers = $("#" + view_id + " table thead tr th");
      var rowspan_maxdepth = calculate_rowspan_maxdepth(table_headers);

      // single row now double colons, just need to format newlines
      if (rowspan_maxdepth === 0) {
        format_newlines(table_headers);
      }
      else {
        var trs = [];
        for (var i = 0; i < rowspan_maxdepth - 1; i++) {
          var tr = $("<tr></tr>");
          trs.push(tr)
        }

        table_headers.each(function (i, th) {
          if (th.innerText.includes("::")) {
            var myText = th.innerText;
            var rowspan_depth = myText.split("::").length;
            var top = myText.split("::")[0];
            if (i > 0 && top != get_name(table_headers[i-1].innerText,0)) {
              $(th).addClass("first-in-group");
            }
            if (i == table_headers.length - 1 ||
                i < table_headers.length - 1 &&
                top != get_name(table_headers[i+1].innerText,0)) {
                    $(th).addClass("last-in-group");
            }
            for (var i = 1; i < rowspan_depth; i++) {
              var bottom = th.innerText.split("::")[i];
              var cloned_th = $(th).clone();
              var tr = trs[i - 1];
              $(tr).append(cloned_th);
            }
          } else {
            $(th).attr("rowspan", rowspan_maxdepth);
            $(th).css("vertical-align", "middle")
          }
        });
        for (var i = 0; i < rowspan_maxdepth; i++) {
          var tr = trs[i];
          $("#" + view_id + " table thead").append(tr);
        }
        var trs = $("#" + view_id + " table thead tr");
        for (var level = 0; level < trs.length; level++) {
          var tr = $(trs[level]);
          var arr = tr.find("th");
          var headersLength = arr.length;
          arr.each(function (i, th) {
            var count = 1;
            for (var j = i + 1; j < headersLength; j++) {
              if ((get_path(arr[j].innerText, level) == get_path(th.innerText, level)) && (j == i + count)) {
                if (arr[j].className.includes("last-in-group")) {
                  $(arr[i]).addClass("last-in-group");
                }
                arr[j].remove();
                count++;
              }
            }
            if (count > 1) {
              $(th).attr("colspan", count);
              $(th).css("text-align", "center");
            }
          });
          arr = tr.find("th");

          arr.each(function (i, th) {            
            if (th.innerText.includes("::")) {              
              var rowspan = get_rowspan(th.innerText, level, rowspan_maxdepth);
              if (rowspan > 0) {
                $(th).attr("rowspan", rowspan);
              }              
              if (level < trs.length - 1 && rowspan === 0) {
                $(th).find("a").get(0).childNodes[0].nodeValue = get_name(th.innerText,level);              
                $(th).find("a").html($(th).find("a").html().replace(/\\n/g,'<br>'));
                $(th).css("vertical-align", "middle")
                $(th).css("text-align", "center");
              } else {              
                $(th).find("a").get(0).childNodes[0].nodeValue = get_name(th.innerText,level);
                $(th).find("a").html($(th).find("a").html().replace(/\\n/g,'<br>'));
              }
            } 
          });
        }
      }
    }    
  } catch (err) {
    console.log("ERR", err.message);
  }

  if (format_counter < 5) {
    console.log("running formatter");
    console.log(format_counter);
    setTimeout(function () { timedCount(subview) }, 1000);
    format_counter = format_counter + 1;
  }
  else {    
    console.log("done running formatter");
    console.log(format_counter);

    console.log("Triggering nested_headers_formatted");
    console.log(subview.id + "_nested_headers_formatted");

    triggered_events[subview.id] = true;

    $(document).trigger(subview.id + "_nested_headers_formatted", { "table_headers": table_headers });
  }
}

define(libs, function (_, $, mvc, TableElement) {
  "use strict";
  $.each(mvc.Components.getInstances(), function (i, view) {
    if (typeof view.getVisualization != "undefined") {
      view.getVisualization(function (subview) {
        subview.on("rendered", function() {
          format_counter = 0;                      
          _.once(timedCount(subview));
        });
      });
    }
  });

  return {
    is_formatted,    
    format_headers
  };
});
