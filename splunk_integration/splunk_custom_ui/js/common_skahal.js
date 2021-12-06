// Translations for en_US
i18n_register({"plural": function(n) { return n == 1 ? 0 : 1; }, "catalog": {}});

// disable timeout so our libraries have plenty of time to load
require.config({
  waitSeconds: 0,
  paths: {
    'tabs': '../app/mxd3/js/tabs/latest',
    'session_storage': '../app/mxd3/js/session_storage/latest',
    'caching': '../app/mxd3/js/caching/latest',
    'style':  '../app/mxd3/js/style/latest',
    'short_url': '../app/mxd3/js/short_url/latest'
  }
});

require([
	'tabs',
  'session_storage',
  'caching',
  'style',
  'short_url'
  ],
  function() {}
);
