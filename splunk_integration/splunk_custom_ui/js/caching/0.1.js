
// Translations for en_US
i18n_register({"plural": function(n) { return n == 1 ? 0 : 1; }, "catalog": {}});

// Translations for en_US
i18n_register({ "catalog": {}, "plural": function(n) { return n == 1 ? 0 : 1; } });

require([
    'underscore',
    'jquery',
    'splunkjs/mvc/utils',
], function(_, $, utils) {

    let page_info = utils.getPageInfo();
    let app = page_info.app;
    let page = page_info.page;
    let dashboard_name = `app_${app}_${page}`;
    
    function get_cache_page(url) { 
      return $.ajax({
        url: url,
        type: 'GET'
      });
    };

    async function cache_redirect() {
      try {
        let dashboard_cache_id = window.location.href.split('en-US/')[1];
        dashboard_cache_id = dashboard_cache_id.replace("/[^a-zA-Z0-9]/gi", "_");
        dashboard_cache_id = `${dashboard_name}_${dashboard_cache_id}`;

        console.log(`dashboard_cache_id: ${dashboard_cache_id}`);

        const res = await get_cache_page(`/app/${app}/${dashboard_cache_id}`);
        window.location.href = `/app/${app}/${dashboard_cache_id}`;
      }
      catch(err) {
        console.log(err);
        console.log("cache doesn't exist continuing normally");
      }
    }

    cache_redirect();
});
