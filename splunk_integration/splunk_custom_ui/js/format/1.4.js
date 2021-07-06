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
console.log("HERE");
   // mvc.Components.get('hard_ip').getVisualization(function(tableView) {
        // Add custom cell renderer, the table will re-render automatically.
    //    tableView.addCellRenderer(new CustomRangeRenderer());
  //  });
});
