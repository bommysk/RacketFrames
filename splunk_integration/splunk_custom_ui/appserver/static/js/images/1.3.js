define(function() {
  return {
    exec: function($td, cell, row, tokens, dashboard_id) {
      var token_value = tokens.get('image.' + dashboard_id + '.' + cell.field);
      if (!token_value) {
        token_value = tokens.get('image.' + cell.field);
      }

      if (token_value) {
        try {
          // Retrieve the site_server from the tokens
          var site_server = tokens.get("site_server");
          // Compute the image url base on the cell's value
          var url = 'http://' + site_server + '/view.php?file=' + cell.value + '&thumbnail';
          // Create the img element and add it to the table cell
          $td.html(_.template('<object style="cursor:pointer;max-width:250px" data="<%-url%>"> <img src=https://comps.canstockphoto.com/login-button-stock-photo_csp16990723.jpg" width="100" height="18"/>  </object>', {
              url: url
          }));
          // Returning true means that we have updated the outer html/text of the cell
          return true;

        } catch (err) {
          console.error('Failed evaluating style for field ' + cell.field + ',Error:', err);
        }
      }
      // Returning false means that we haven't updated the outer html/text of the cell, e.g. adding a call
      return false;
    }
  };
});

