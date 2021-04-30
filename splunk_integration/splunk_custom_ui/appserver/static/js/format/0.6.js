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
            console.log("data",this.data);

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

      view.getVisualization(function (subview) {
        subview.on("rendered", function () {
           subview.addCellRenderer(new AdvancedFormatRenderer());
        });




      });
    }
  });
  });

});
