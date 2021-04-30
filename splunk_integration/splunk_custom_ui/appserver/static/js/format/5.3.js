require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomRangeRenderer = TableView.BaseCellRenderer.extend({
        data: [],
onetime: true,
        canRender: function(cell) {
            return true;
        },
        render: function($td, cell) {
            var value = cell.value;
console.log("TEST")
            if (this.data.length <= cell.index)
                this.data[cell.index] = {};
            this.data[cell.index][cell.field] = cell.value;
if (this.onetime){
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
        var soft_ip_search = mvc.Components.getInstance("soft_ip_search");

  var searchResults = soft_ip_search.data('results', {
       output_mode: 'json_rows',
       count: 0
     });
 
     searchResults.on("data", function() {
       if (searchResults.hasData()) {
          var results = searchResults.data().rows;
          var length = results.length;
          //var column1 = searchResults.data().fields.indexOf("IP");
          //var medianColumn = searchResults.data().fields.indexOf("Errors");
          //var searchTerm = "The Column1 VALUE YOU ARE LOOKING FOR";
 
         for (var j = 0; j < length; j++) {
           console.log(results[j]);
           //if (results[j][column1] === searchTerm) {
           //  submittedTokenModel.set("median_token", parseFlow(results[j][medianColumn]));
           //  break;
           //}
       }
    }
 });

        subview.addCellRenderer(new CustomRangeRenderer());
        
        subview.on("rendered", function () {
console.log("RENDERED");
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
   // mvc.Components.get('hard_ip').getVisualization(function(tableView) {
        // Add custom cell renderer, the table will re-render automatically.
    //    tableView.addCellRenderer(new CustomRangeRenderer());
  //  });
});
