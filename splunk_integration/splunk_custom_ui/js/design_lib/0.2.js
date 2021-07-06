require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomRangeRenderer = TableView.BaseCellRenderer.extend({
        canRender: function(cell) {
            return cell.field.includes("Diff Non-RTL files to") || cell.field.includes("Diff RTL files to");
        },
        render: function($td, cell) {
            var value = cell.value;
            if (!cell.value)
                return;
            // Add a class to the cell based on the returned value
            if (cell.value.includes(",")) {
                value = cell.value.split(",")[0];
            }
            if (value > 0) {
                if (cell.field.includes("Diff Non-RTL files to")) {
                    $td.css('background-color','#FBF1BA');
                    //$td.css('color','#1e93c6');
                    $td.css('color','#000000');
                }
                else {
                    $td.css('background-color','#FB7E7E');
                    //$td.css('color','#1e93c6');
                    $td.css('color','#000000');
                }
            }
            else {
                $td.css('background-color','#B6ECB5');
                //$td.css('color','#1e93c6');
                $td.css('color','#000000');
            }
            $td.text(value).addClass('numeric');
        }
    });
    mvc.Components.get('design_lib').getVisualization(function(tableView) {
        // Add custom cell renderer, the table will re-render automatically.
        tableView.addCellRenderer(new CustomRangeRenderer());
    });
});
