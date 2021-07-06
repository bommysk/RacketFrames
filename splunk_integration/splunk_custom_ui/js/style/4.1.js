require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    '../app/search/js/style/lib/latest',
    'splunkjs/mvc/simplexml/ready!'

], function(_, $, mvc, TableView, lib) {
    var CustomCellRenderer = TableView.BaseCellRenderer.extend({
        row_index: -1,
        row_count: 0,
        canRender: function(cell) {
            return true;
        },
        // monitor the current row and column index
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
            if (cell && cell.value && cell.value.toString().toLowerCase() == "na" ) {
                $td.addClass("light");
                $td.text(cell.value);
                return;
            }
            var value = cell.value;
            var columnIndex = this.tableView.resultsModel._data.fields.indexOf(cell.field);
            var results = this.tableView.resultsModel._data.rows;
            // build up a data structure to be available in the Dashboard code
            var row = {};
            for(var i =0 ; i < this.tableView.resultsModel._data.fields.length; i++)  {
                var field = this.tableView.resultsModel._data.fields[i].name;
                var value = results[this.row_index][i];
                row[field] = value;
            }
            
            // get the value of the token
            var token_value = this.tokens.get("style." + this.dashboard_id + "." +  cell.field);
            if (!token_value)  {
                token_value = this.tokens.get("style." + cell.field);
            }
            if (!token_value)  {
                for (var token in this.tokens.attributes)  {
                    var isRegexToken = /style\.\//
                    if (isRegexToken.exec(token))  {
                        var myRegex = new RegExp(token.replace("/","").replace("/",""));
                        if (myRegex.exec("style." + cell.field))  {
                            token_value = this.tokens.get(token);
                        }
                    }
                }
            }
            if (!token_value)  { // Check for default style as literal "style.*"
                token_value = this.tokens.get("style.*");
            }
            if (token_value)  {
                try  {
                    // execute the eval to determine the classes names
                    var style_value = eval(token_value);
                    // apply CSS classes onto the cell
                    var arr = style_value.split(",");
                    arr.forEach(function(class_name) {
                        $td.addClass(class_name);
                    });
                }
                catch(err)  {
                    console.error("Failed evaluating style for field " + cell.field +  ",Error:" , err);
                }
            }
            /*if (value == "NA")  {
               $td.addClass("light");
            }*/
            $td.text(cell.value);
        }
    });
$.each(mvc.Components.getInstances(), function (i, view) {
    if (typeof view.getVisualization != "undefined") {
      // Apply only on Data Tables
      view.getVisualization(function (subview) {
        // Obtain a reference to the search Manager of the table
        var searchManager = mvc.Components.getInstance(view.manager.id);
        var searchResults = searchManager.data('results', {
            output_mode: 'json_rows',
            count: 0
       });
     // Reigster a callback for when the data is being loaded
     searchResults.on("data", function() {
       if (searchResults.hasData()) {
         // All of the data have been loaded
         // Obtain a reference to the tokens service
         var tokens = mvc.Components.get("default");
         // Create a new instance of the new custom cell renderer
         var customCellRenderer = new CustomCellRenderer();
         customCellRenderer.dashboard_id = view.id;
         customCellRenderer.tokens = tokens;
         customCellRenderer.tableView = subview;
         // Assigning all of the style functions extensions
         Object.assign(customCellRenderer, lib);
         // Adding the Cell Renderer to the table
         subview.addCellRenderer(customCellRenderer);
        }
       });
      });
    }
  });
});
