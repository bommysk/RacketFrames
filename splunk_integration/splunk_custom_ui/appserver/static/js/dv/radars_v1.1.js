require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomIconRenderer = TableView.BaseCellRenderer.extend({
        canRender: function(cell) {
            return cell.field === 'Radar Chart' || cell.field === 'Open/Closed Chart';
        },
        render: function($td, cell) {
            var file = cell.value;
            // Compute the image url base on the file (cell's value)
            var url = 'https://scv-mxd.csg.apple.com/view.php?file=' + file + '&thumbnail';
            // Create the img element and add it to the table cell
            $td.html(_.template('<img style="cursor:pointer" src="<%-url%>"></img>', {
                url: url
            }));}
    });
    mvc.Components.get('radars').getVisualization(function(tableView){
        // Register custom cell renderer, the table will re-render automatically
        tableView.addCellRenderer(new CustomIconRenderer());
    });
});
