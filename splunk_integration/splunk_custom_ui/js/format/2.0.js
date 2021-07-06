require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomRangeRenderer = TableView.BaseCellRenderer.extend({
        data: {},
        canRender: function(cell) {
            return true;
        },
        render: function($td, cell) {
            console.log("A");
            var value = cell.value;
            this.data[cell.field] = cell.value;
            console.log("B");
            if (this.data.hasOwnProperty(cell.field + ".format"))
                console.log("includes",this.data[cell.field + ".format"]);
            
            // Add a class to the cell based on the returned value
            if (cell && cell.value && this.data.hasOwnProperrty(cell.field + ".format"))  {
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
