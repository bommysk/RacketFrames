require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomRangeRenderer = TableView.BaseCellRenderer.extend({
        canRender: function(cell) {
            return true;
        },
        render: function($td, cell) {
            var value = cell.value;



var search_id = "search" + Math.floor((Math.random()*10000000000000)).toString();
    var tokens = mvc.Components.get('default');
    var uid  = tokens.get('UID');
    var search = new SearchManager({
        id: search_id,
        earliest_time :"-7d@d" ,
        latest_time: "now",
        autostart: true,
        search: `$row.IP$`
    });
      var mvaDataObj = splunkjs.mvc.Components.getInstance(search_id);
     mvaDataObj.on('search:progress', function(properties) {
         console.log("IN PROGRESS:"); //  properties)
     });
     mvaDataObj.on('search:done', function(properties) {
         console.log("DONE:"); //, properties)
     });
     mvaDataObj.on('search:failed', function(properties) {
         console.log("FAIL:", properties)

     });
var myResults = mvaDataObj.data("results") ;
       myResults.on("data", function() {
            console.log("Has data? ", myResults.hasData());
            var res = null;
            if (myResults.hasData()) {
                res = myResults.data().rows.filter( array => {
                        return array.length;
                });
                if (res[0]) {
			console.log(res[0]);
			}
		}
	    });

            // Add a class to the cell based on the returned value
            if (cell && cell.value && cell.value.includes(","))  {
                value = cell.value.split(",")[0];
                var arr = cell.value.split(",");
                arr.shift();
                arr.forEach(function(class_name) {
                    $td.addClass(class_name);
               });
            }
            $td.text(value);
        }
    });
require(libs, function ($, mvc, TableElement) {
  "use strict";
$.each(mvc.Components.getInstances(), function (i, view) {

    if (typeof view.getVisualization != "undefined") {

        console.log("CHECK");
      view.getVisualization(function (subview) {



        console.log("TEST");
        subview.on("rendered", function () {
           subview.addCellRenderer(new CustomRangeRenderer());
          //console.log("#" + view_id + " table thead");
          //console.log(view);
        });




      });
    }
  });
  });


   // mvc.Components.get('soft_ip').getVisualization(function(tableView) {
        // Add custom cell renderer, the table will re-render automatically.
     //   tableView.addCellRenderer(new CustomRangeRenderer());
   // });
   // mvc.Components.get('hard_ip').getVisualization(function(tableView) {
        // Add custom cell renderer, the table will re-render automatically.
    //    tableView.addCellRenderer(new CustomRangeRenderer());
  //  });
});
