require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomIconRenderer = TableView.BaseCellRenderer.extend({
        canRender: function(cell) {
            return true;
        },
        render: function($td, cell) {
            // Retrieve the site_server from the tokens
            var tokens = mvc.Components.get("default");
            var site_server = tokens.get("site_server");
            site_server = 'scv-mxd.csg.apple.com';
            var file = cell.value;
            // Compute the image url base on the file (cell's value)
            var url = 'https://' + site_server + '/view.php?file=' + file + '&thumbnail';
            // Create the img element and add it to the table cell
            $td.html(_.template('<img style="cursor:pointer" src="<%-url%>"></img>', {
                url: url
            }));}
    });
    mvc.Components.get('images').getVisualization(function(tableView){
        // Register custom cell renderer, the table will re-render automatically
        tableView.addCellRenderer(new CustomIconRenderer());
    });
});
