require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomRangeRenderer = TableView.BaseCellRenderer.extend({
onetime: true,
        canRender: function(cell) {
            return true;
        },
        render: function($td, cell) {
            console.log("render");
            var value = cell.value;
            var results = this.searchResults.data().rows()l
            console.log(results[cell.index]);
            var class_name = "lt_red";
            $td.addClass(class_name);
            $td.text(cell.value);
        }
    });
$.each(mvc.Components.getInstances(), function (i, view) {
    if (typeof view.getVisualization != "undefined") {
console.log("HERE");
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
console.log("Data is ready");
          //var column1 = searchResults.data().fields.indexOf("IP");
          //var medianColumn = searchResults.data().fields.indexOf("Errors");
          //var searchTerm = "The Column1 VALUE YOU ARE LOOKING FOR";
 
         for (var j = 0; j < length; j++) {
           //console.log(results[j]);
           //if (results[j][column1] === searchTerm) {
           //  submittedTokenModel.set("median_token", parseFlow(results[j][medianColumn]));
           //  break;
           //}
       }
    }
 });
        console.log("REGISTERING");
        var customRangeRenderer = new CustomRangeRenderer();
        customRangeRenderer.searchResults = searchResults;
        subview.addCellRenderer(customRangeRenderer);
        
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
