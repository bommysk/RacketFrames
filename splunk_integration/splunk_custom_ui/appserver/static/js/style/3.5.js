define(function() {
  return {
    exec: function($td, cell, row, tokens, dashboard_id) {
            var value = cell.value;
            // get the value of the token
            var token_value = tokens.get("style." + dashboard_id + "." +  cell.field);
            if (!token_value)  {
                token_value = tokens.get("style." + cell.field);
            }
            if (!token_value)  {
                for (var token in tokens.attributes)  {
                    var isRegexToken = /style\.\//
                    if (isRegexToken.exec(token))  {
                        var myRegex = new RegExp(token.replace("/","").replace("/",""));
                        if (myRegex.exec("style." + cell.field))  {
                            token_value = tokens.get(token);
                        }
                    }
                }
            }
            if (!token_value)  { // Check for default style as literal "style.*"
                token_value = tokens.get("style.*");
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
      // Returning false means that we haven't updated the outer html/text of the cell, e.g. adding a call
      return true;
    }
  };
});

