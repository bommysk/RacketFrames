require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomRangeRenderer = TableView.BaseCellRenderer.extend({
        row_index: -1,
        row_count: 0,
        canRender: function(cell) {
            return true;
        },
       setup: function($container, cell) {
            if (cell.index == 0) {
              this.row_index++;
              this.row_count++;
            }
        },
        teardown: function($container, cell) {
            if (cell.index == 0) {
              this.row_count--;
            }
            if (this.row_count == 0)  {
                this.row_index=-1;
            }
            
        },
        render: function($td, cell) {
            var value = cell.value;
            var columnIndex = this.searchResults.data().fields.indexOf(cell.field);
            var results = this.searchResults.data().rows;
            var row = {};
            for(var i =0 ; i < this.searchResults.data().fields.length; i++)  {
                var field = this.searchResults.data().fields[i];
                var value = results[this.row_index][i];
                row[field] = value;
            }
            var token_value = this.tokens.get("format." + cell.field);
            if (token_value)  {
                try  {
                    var format_value = eval(token_value);
                    var arr = format_value.split(",");
                    arr.forEach(function(class_name) {
                        $td.addClass(class_name);
                    });
                }
                catch(err)  {
                    console.log("Failed evaluating format for field " + cell.field +  ",Error:" , err);
                }
            }
            //console.log(cell.field , eval(class_name), row["Vendor"],cell.index);
            //console.log(this);
            $td.text(cell.value);
        }
    });
$.each(mvc.Components.getInstances(), function (i, view) {
      console.log(view);
    if (typeof view.getVisualization != "undefined") {

      view.getVisualization(function (subview) {
        var soft_ip_search = mvc.Components.getInstance("soft_ip_search");

        var searchResults = soft_ip_search.data('results', {
            output_mode: 'json_rows',
            count: 0
       });
 
        var tokens = mvc.Components.get("default");
        var customRangeRenderer = new CustomRangeRenderer();
        customRangeRenderer.searchResults = searchResults;
        customRangeRenderer.tokens = tokens;
        subview.addCellRenderer(customRangeRenderer);
        
      });
    }
  });
});
