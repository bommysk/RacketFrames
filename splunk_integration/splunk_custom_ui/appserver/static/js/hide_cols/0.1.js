// 
// https://answers.splunk.com/answers/96446/hide-custom-named-column-from-simpleresultstable.html
// Modified by Danny Durio (ddurio@apple.com), Mar 15 2018
//

require([
  'jquery',
  'splunkjs/mvc',
  'splunkjs/mvc/simplexml/ready!'
], function( $, mvc) {
  $.each( mvc.Components.getInstances(), function(i, view) {
    if( typeof(view.getVisualization) != "undefined" ) {
      var tokens = mvc.Components.get("default");
      var token_value = "false";
      var field = "";

console.log("Before Each");
console.log(view);
console.log(view.children);
console.log(view.children.body);
console.log(view.children.body.el);
console.log($('th', view.children.body.el));
      $('th', view.children.body.el).each(function(i, el) {
console.log("In TH");
        field = $(el).text().trim();
        token_value = tokens.get("hidden." + view.id + "." + field);
        if( !token_value ) {
          token_value = tokens.get("hidden." + field);
        }

        if( token_value && token_value == "true" ) {
          $("td[field|=" + field + "]", view).hide();
          $(el).hide();
        }
      });
console.log("After Each");
    }
  });
});
