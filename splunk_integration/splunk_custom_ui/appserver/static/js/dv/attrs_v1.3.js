require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomRangeRenderer = TableView.BaseCellRenderer.extend({
        canRender: function(cell) {
            return cell.field.includes("Implemented::Yes") || cell.field.includes("Verified::Yes") | cell.field.includes("Impl::Yes") || cell.field == "Total";
        },
        render: function($td, cell) {
            var value = cell.value;
            // Add a class to the cell based on the returned value
            if (cell.value.includes(","))  {
                value = cell.value.split(",")[0];
                var reference = cell.value.split(",")[1];
                if (reference > 0) {
                    $td.css('background-color','#FB7E7E');
                    //$td.css('color','#1e93c6');
                    $td.css('color','#000000');
                }
                else {
                    $td.css('background-color','#B6ECB5');
                    //$td.css('color','#1e93c6');
                    $td.css('color','#000000');
                }
            }
            $td.text(value).addClass('numeric');
        }
    });
    mvc.Components.get('attributes').getVisualization(function(tableView) {
        // Add custom cell renderer, the table will re-render automatically.
        tableView.addCellRenderer(new CustomRangeRenderer());
    });
});
