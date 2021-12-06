var libs = [
  "jquery",
  "splunkjs/mvc",
  "splunkjs/mvc/simplexml/element/table",
  "splunkjs/mvc/simplexml/ready!",
];

var calculate_rowspan_maxdepth = function (table_headers) {
  var maxdepth = 0;
  table_headers.each(function (i, th) {
    if (th.innerText.includes("::")) {
      var depth = th.innerText.split("::").length;
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
  var nodes = str.split("::");
  var len = nodes.length;
  nodes = nodes.slice(0, level + 1);
  return nodes.join("::");
}


var get_name = function (str, level) {

  if (!str.includes("::")) {
     str = str.replace(/\\n/g,"<br>");
    return str;
  }
  
  var nodes = str.split("::");
  var name = nodes[level];
  if (name)
  name = name.replace(/\\n/g,"<br>");
  return name;
}

var get_rowspan = function (str, level, maxdepth) {
  var depth = str.split("::").length;
  if (depth == maxdepth || level + 1 != depth) {
    return 0;
  }
  return maxdepth - depth + 1;
}
var format_counter = 0;
var timedCount = function (subview) {
  format_headers(subview);
  format_counter = format_counter + 1;
}

var is_formatted = function (subview) {

  var view_id = $(subview).attr("id");
  var table_headers = $("#" + view_id + " table thead tr th");

  var result = true;
  table_headers.each(function (i, th) {
    if (th.innerText.includes("::")) {
      result = false;
    }
  });

  return result;
}


var format_headers = function (subview) {
  
  try {
    if (!is_formatted(subview)) {
	  
      var view_id = $(subview).attr("id");
      var table_headers = $("#" + view_id + " table thead tr th");
      var rowspan_maxdepth = calculate_rowspan_maxdepth(table_headers)
      var trs = [];
      for (var i = 0; i < rowspan_maxdepth - 1; i++) {
        var tr = $("<tr></tr>");
        trs.push(tr)
      }


      table_headers.each(function (i, th) {
        if (th.innerText.includes("::")) {
          var rowspan_depth = th.innerText.split("::").length;
          var top = th.innerText.split("::")[0];
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
            if (level < trs.length - 1 && rowspan == 0) {
              //$(th).find("a").text(get_name(th.innerText, level));
			  
              $(th).find("a").html(get_name(th.innerText, level));
              $(th).css("vertical-align", "middle")
              $(th).css("text-align", "center");
            } else {
              //$(th).find("a").html(get_name(th.innerText, level));
              $(th).find("a").get(0).childNodes[0].nodeValue = get_name(th.innerText,level);
			  //$($(th).find("a").get(0).childNodes[0]).html("<a href='#'>" + get_name(th.innerText,level) + "</a>");
			  //$($(th).find("a").get(0).childNodes[0]).text( get_name(th.innerText,level) );
            }
          } else {
              //$(th).find("a").html(get_name(th.innerText, level));
              $(th).find("a").text(get_name(th.innerText, level) + "!");
              //$(th).find("a").get(0).childNodes[0].nodeValue = get_name(th.innerText,level);
          }
        });
      }
    }
  } catch (err) {
    console.log("ERR", err.message);
  }

  if (format_counter < 5)
    setTimeout(function () { timedCount(subview) }, 1000);

}

require(libs, function ($, mvc, TableElement) {
  "use strict";
  $.each(mvc.Components.getInstances(), function (i, view) {
	
    if (typeof view.getVisualization != "undefined") {
	  
      view.getVisualization(function (subview) {
		


        subview.on("rendered", function () {
          format_counter = 0;
          timedCount(subview);
          //console.log("#" + view_id + " table thead");
          //console.log(view);
        });




      });
    }
  });
});

