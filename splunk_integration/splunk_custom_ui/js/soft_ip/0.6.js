require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomRangeRenderer = TableView.BaseCellRenderer.extend({
        canRender: function(cell) {
            return cell.field.includes("::");
        },
        render: function($td, cell) {
            var value = cell.value;
            // Add a class to the cell based on the returned value
            if (cell && cell.value && cell.value.includes(","))  {
                value = cell.value.split(",")[0];
                var arr = cell.value.split(",");
                arr.shift();
                arr.forEach(function(class_name) {
                    $td.addClass(class_name);
               });
            }
            $td.text(value);
        }
    });
    mvc.Components.get('soft_ip').getVisualization(function(tableView) {
        // Add custom cell renderer, the table will re-render automatically.
        tableView.addCellRenderer(new CustomRangeRenderer());
    });
    mvc.Components.get('hard_ip').getVisualization(function(tableView) {
        // Add custom cell renderer, the table will re-render automatically.
        tableView.addCellRenderer(new CustomRangeRenderer());
    });
});
