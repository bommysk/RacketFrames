function dompath( element )
{
    var path = '';
    for ( ; element && element.nodeType == 1; element = element.parentNode )
    {
        var inner = $(element).children().length == 0 ? $(element).text() : '';
        var eleSelector = element.tagName.toLowerCase() + 
           ((inner.length > 0) ? ':contains(\'' + inner + '\')' : '');
        path = ' ' + eleSelector + path;
    }
    return path;
}

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
            var value = cell.value;
            this.data[cell.field] = cell.value;
            if (cell.field.includes(".format"))  {
                 console.log("td",$($td.parentNode).html());
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
                var arr = format_value.split(",");
                arr.forEach(function(class_name) {
                    $td.addClass(class_name);
               });
            }
            $td.text(cell.value);
        }
    });
$.each(mvc.Components.getInstances(), function (i, view) {

    if (typeof view.getVisualization != "undefined") {

      view.getVisualization(function (subview) {
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
