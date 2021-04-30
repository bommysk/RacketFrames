require([
    'underscore',
    'jquery',
    'splunkjs/mvc',
    'splunkjs/mvc/tableview',
    'splunkjs/mvc/simplexml/ready!'
], function(_, $, mvc, TableView) {
    var CustomIconRenderer = TableView.BaseCellRenderer.extend({
        canRender: function(cell) {
            return cell.field.includes('Chart');
        },
        render: function($td, cell) {
            // Retrieve the site_server from the tokens
            var tokens = mvc.Components.get("default");
            var site_server = tokens.get("site_server");
            site_server = 'scv-mxd.csg.apple.com';
            var file = cell.value;
            // Compute the image url base on the file (cell's value)
            var url = 'https://' + site_server + '/view.php?file=' + file + '&no-cache';
            // Create the img element and add it to the table cell
            $td.html(_.template('<object style="cursor:pointer" data="<%-url%>"> <img src=https://comps.canstockphoto.com/login-button-stock-photo_csp16990723.jpg" width="100" height="18"/>  </object>', {
                url: url
            }));}
    });
    mvc.Components.get('attributes').getVisualization(function(tableView){
        // Register custom cell renderer, the table will re-render automatically
        tableView.addCellRenderer(new CustomIconRenderer());
    });
    mvc.Components.get('attributes_project').getVisualization(function(tableView){
        // Register custom cell renderer, the table will re-render automatically
        tableView.addCellRenderer(new CustomIconRenderer());
    });
});
