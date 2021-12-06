require([
  'underscore',
  'jquery',
  'splunkjs/mvc',
  'splunkjs/mvc/tableview',
  '../app/mxd3/js/double_link/latest',
  '../app/mxd3/js/style/latest',
  '../app/mxd3/js/style/lib/latest',
  '../app/mxd3/js/images/latest',
  'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView, double_link, style, lib, images) {
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
      if (this.row_count == 0) {
        this.row_index = -1;
      }
    },
    render: function($td, cell) {
      var value = cell.value;
      var columnIndex = this.searchResults.data().fields.indexOf(cell.field);
      var results = this.searchResults.data().rows;
      // build up a data structure to be available in the Dashboard code
      var row = {};
      for (var i = 0; i < this.searchResults.data().fields.length; i++) {
        var field = this.searchResults.data().fields[i];
        var value = results[this.row_index][i];
        row[field] = value;
      }

//      var token_value = this.tokens.get('style.' + this.dashboard_id + '.' + cell.field);
//
//      if (!token_value) {
//        token_value = this.tokens.get('style.' + cell.field);
//      }
//
//      if (token_value) {
//        try {
//          // execute the eval to determine the classes names
//          var style_value = eval(token_value);
//          // apply CSS classes onto the cell
//          if (style_value) {
//            var arr = style_value.split(',');
//            arr.forEach(function(class_name) {
//              $td.addClass(class_name);
//            });
//          }
//        } catch (err) {
//          console.log('Failed evaluating style for field ' + cell.field + ',Error:', err);
//        }
//      }

      var result = false;

      if (this.cellRenders) {
        for (var i = 1; i < this.cellRenders.length; i++) {
          try {
            result = this.cellRenders[i].exec($td, cell, row, this.tokens, this.dashboard_id) || result;
          } catch (e) {
            console.error('not able to run custom cell render' + i, e);
          }
        }
        //no body change the dom, then put back the value
        if (!result) $td.text(cell.value);
      } else {
        $td.text(cell.value);
      }
    }
  });

  $.each(mvc.Components.getInstances(), function(i, view) {
    if (typeof view.getVisualization != 'undefined') {
      // Apply only on Data Tables
      view.getVisualization(function(subview) {
        // Obtain a reference to the search Manager of the table
        var searchManager = mvc.Components.getInstance(view.manager.id);
        var searchResults = searchManager.data('results', {
          output_mode: 'json_rows',
          count: 0
        });
        // Reigster a callback for when the data is being loaded
        searchResults.on('data', function() {
          if (searchResults.hasData()) {
            // All of the data have been loaded
            // Obtain a reference to the tokens service
            var tokens = mvc.Components.get('default');

            // Create a new instance of the new custom cell renderer
            var customCellRenderer = new CustomCellRenderer();
            customCellRenderer.dashboard_id = view.id;
            customCellRenderer.searchResults = searchResults;
            customCellRenderer.tokens = tokens;

            var customCellRenderList = ['style'];
            if (tokens.get('custom_cell_renders')) {
              var t = tokens.get('custom_cell_renders').split(',');
              customCellRenderList.push.apply(customCellRenderList, t);
            }

            if (customCellRenderList && Array.isArray(customCellRenderList)) {
              try {
                customCellRenderList.forEach(function(part, index, theArray) {
                  theArray[index] = eval(theArray[index]);
                });
                customCellRenderer.cellRenders = customCellRenderList;
                Object.assign(customCellRenderer, lib);
              } catch (e) {
                console.error('e', e);
              }
            }
            subview.addCellRenderer(customCellRenderer);
          }
        });
      });
    }
  });
});
