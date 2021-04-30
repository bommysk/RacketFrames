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
            var value = cell.value;
            var columnIndex = this.searchResults.data().fields.indexOf(cell.field);
            var results = this.searchResults.data().rows;
            console.log(results[cell.index][columnIndex]);
            var class_name = this.tokens.get("my_class");
            $td.addClass(class_name);
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
           //console.log(results[j]);
           //if (results[j][column1] === searchTerm) {
           //  submittedTokenModel.set("median_token", parseFlow(results[j][medianColumn]));
           //  break;
           //}
       }
    }
 });
        var tokens = mvc.Components.get("default");

        var customRangeRenderer = new CustomRangeRenderer();
        customRangeRenderer.searchResults = searchResults;
        customRangeRenderer.tokens = tokens;
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
