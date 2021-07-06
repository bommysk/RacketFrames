define(function() {
  return {
    exec: function($td, cell, row, tokens, dashboard_id) {
      var token_value = tokens.get('style.' + dashboard_id + '.' + cell.field);
      if (!token_value) {
        token_value = tokens.get('style.' + cell.field);
      }

      if (token_value) {
        try {
          // execute the eval to determine the classes names
          var style_value = eval(token_value);
          // apply CSS classes onto the cell
          if (style_value) {

            var arr = style_value.split(',');
            arr.forEach(function(class_name) {
              $td.addClass(class_name);
            });
          }
        } catch (err) {
          console.log('Failed evaluating style for field ' + cell.field + ',Error:', err);
        }
      }
      return false;
    }
  };
});
