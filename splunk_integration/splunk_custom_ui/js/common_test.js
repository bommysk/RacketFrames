// Translations for en_US
i18n_register({"plural": function(n) { return n == 1 ? 0 : 1; }, "catalog": {}});

// disable timeout so our libraries have plenty of time to load
require.config({
  waitSeconds: 0,  
  paths: {
    'test_style_renderer': '../app/search/js/test/table_cell_render_all_cells',
  }
});

require([
	'test_style_renderer'
  ],
  function() {}
);
