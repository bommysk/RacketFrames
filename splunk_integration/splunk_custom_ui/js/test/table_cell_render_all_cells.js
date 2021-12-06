require([
    "underscore",
    "jquery",
    "splunkjs/mvc",
    "splunkjs/mvc/searchmanager",
    "splunkjs/mvc/tableview",
    "splunkjs/mvc/simplexml/ready!"
], function(_, $, mvc, SearchManager, TableView) {

    var mySearch = splunkjs.mvc.Components.getInstance("myTableSearch");
    var myResults = mySearch.data("results", {count:0});
    var allFields,filteredFields;
    myResults.on("data", function(){
        if (myResults.hasData()) {
            allFields=myResults.data().fields;
            filteredFields=allFields.filter(filterFields);
            console.log("Filtered Fields:",filteredFields);   
        }        
    });
    function filterFields(field) {
        return field !== "_time";
    }
    // Row Coloring Example with custom, client-side range interpretation
    var CustomRangeRenderer = TableView.BaseCellRenderer.extend({
        canRender: function(cell) {
            // Enable this custom cell renderer for all fields except _time
            return _(filteredFields).contains(cell.field);
        },
        render: function($td, cell) {
            console.log("RENDERING");
            // Add a class to the cell based on the returned value
            var value = parseInt(cell.value);

            // Apply interpretation based on count of errors per _time for each field
            if(cell.field !== "_time"){
                if (value >= 500) {
                    $td.addClass("range-cell").addClass("range-severe");
                }
                else if (value >= 200 && value < 500) {
                    $td.addClass("range-cell").addClass("range-elevated");
                }
                else if (value < 200) {
                    $td.addClass("range-cell").addClass("range-low");
                }
                // Update the cell content
                $td.text(value).addClass("numeric");
            }
        }
    });
    
    let all_view_instances = mvc.Components.getInstances().filter(function (view) {
        return (typeof view.getVisualization != "undefined");
    });

    $.each(all_view_instances, function (i, view) {
      // Apply only on Data Tables
      view.getVisualization(function (tableView) {
          console.log(tableView);
        tableView.addCellRenderer(new CustomRangeRenderer());
      });
    });
});