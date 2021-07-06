require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomRangeRenderer = TableView.BaseCellRenderer.extend({
        data: {},
onetime: true,
        canRender: function(cell) {
            return true;
        },
        render: function($td, cell) {
            var value = cell.value;
            this.data[cell.field] = cell.value;
if (this.onetime){
console.log(cell);
this.onetime = false;
}
            if (cell.field.includes(".format"))  {

                 $td.css("display", "none");
            }
/*            console.log("B");
            if (this.data.hasOwnProperty(cell.field + ".format"))
                console.log("includes",this.data[cell.field + ".format"]);
  */          
            // Add a class to the cell based on the returned value
            if (cell && cell.value && this.data.hasOwnProperty(cell.field + ".format"))  {
                //value = cell.value.split(",")[0];
                var format_value = this.data[cell.field + ".format"];
                if (format_value)  {
                    var arr = format_value.split(",");
                    arr.forEach(function(class_name) {
                        $td.addClass(class_name);
                   });
               }
            }
            $td.text(cell.value);
        }
    });
$.each(mvc.Components.getInstances(), function (i, view) {

    if (typeof view.getVisualization != "undefined") {

      view.getVisualization(function (subview) {
        subview.addCellRenderer(new CustomRangeRenderer());
        subview.on("rendered", function () {
     var elements = [];
     /*$('th', this.container).each(function(i, el) {
         if(/^.*\.format\s*$/.test($(el).text())) {
             elements.push($(el).text());
             $(el).hide();
         }
     });*/
     
     for(n = 0; n < elements.length; n++) {
         //$("td[field|=" + elements[n].trim() + "]", this.container).hide();
     }
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
