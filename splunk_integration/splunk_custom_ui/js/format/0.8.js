require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var AdvancedFormatRenderer = TableView.BaseCellRenderer.extend({
        data: {},
        canRender: function(cell) {
            return true;
        },
        render: function($td, cell) {
            var value = cell.value;
            this.data[cell.field] = cell.value;
            console.log(cell);
            if (!this.data.includes(cell.field + ".format"))
            
	    }
            // Add a class to the cell based on the returned value
            if (cell && cell.value && this.data.includes(cell.field + ".format"))  {
                //value = cell.value.split(",")[0];
                var format_value = this.data[cell.field + ".format"];
                var arr = format_value.split(",");
                arr.forEach(function(class_name) {
                    $td.addClass(class_name);
               });
            }
            //$td.text(value);
        }
    });
require(libs, function ($, mvc, TableElement) {
  "use strict";
$.each(mvc.Components.getInstances(), function (i, view) {

    if (typeof view.getVisualization != "undefined") {

      view.getVisualization(function (subview) {
        subview.on("rendered", function () {
           subview.addCellRenderer(new AdvancedFormatRenderer());
        });




      });
    }
  });
  });

});
