// Translations for en_US
i18n_register({"plural": function(n) { return n == 1 ? 0 : 1; }, "catalog": {}});

require.config({
  waitSeconds: 0,  
  paths: {
    'spinner': '../app/search/gizmos/js/spin.min'
  }
});

require([
    'underscore',
    'jquery',
    'splunkjs/mvc/utils',
    'spinner',
    'splunkjs/ready!',
    'splunkjs/mvc/simplexml/ready!',    
], function(_, $, utils, Spinner) {
  /**
   * Generate elements for viewing links within inset page modal
   **/
  $(document).ready(function() {
    let modal = '<!-- Modal for Splunk Lightbox -->'
              + '<div id="splunk_modal" class="modal fade" tabindex="-1" role="dialog">'
              + '  <div class="modal-dialog" style="height: 95%; width: 95%;">'
              + '    <div class="modal-content" style="height: 100%;">'
              + '      <div class="modal-header">'
              + '        <div class="pull-right">'              
              + '          <button class="btn btn-default btn-sm glyphicon glyphicon-refresh"'
              + '                  id="splunk_refresh_btn"'
              + '                  title="Refresh">'
              + '          </button>'
              + '          <button class="btn btn-default btn-sm glyphicon glyphicon-new-window"'
              + '                  id="splunk_tab_btn"'
              + '                  title="Open in a new tab">'
              + '          </button>'
              + '          <button class="btn btn-default btn-sm glyphicon glyphicon-remove"'
              + '                  id="splunk_close_btn"'
              + '                  title="Close"'
              + '                  data-dismiss="modal">'
              + '          </button>'
              + '        </div>'
              + '        <h4 style="text-align: left; word-wrap: break-word;">'
              + '          <span id="splunk_title"></span>'
              + '        </h4>'
              + '      </div>'
              + '      <div class="modal-body">'
              + '        <div class="container" style="height: 100%; width: 100%">'
              + '          <div id="splunk_content" style="height: 100%; width: 100%"></div>'
              + '          <div id="splunk_overlay"></div>'
              + '        </div>'
              + '      </div>'
              + '    </div>'
              + '  </div>'
              + '</div>';

    $(document.body).append(modal);

    let SplunkLightbox = {};

    SplunkLightbox.getUriParams = function(url) {
      let params = {};
      let url_parts = decodeURIComponent(url).split('?',2);
      if ( url_parts.length > 1 ) {
        url_parts.pop().split('&').map(function(res) {
          let pairs = res.split('=',2);
          let key   = pairs[0];
          let value = pairs[1];

          params[key] = value
        });
      }

      return params;
    }

    SplunkLightbox.open_tab = function() {
      let win = window.open(SplunkLightbox.url, '_blank');

      if (win) {
        win.focus();
      } else {
        alert('Please allow popups for this website');
      }
    }

    SplunkLightbox.build= function(event) {
      let spinner    = new Spinner().spin();
      let title      = 'View file';
      let php_re     = /\/(mxd_report_parser|view)\.php/;
      let php_script = php_re.exec(SplunkLightbox.url);

      if ( php_script !== null ) {
        let delim = ( SplunkLightbox.url.lastIndexOf('?') == -1 ) ? '?' : '&';
        SplunkLightbox.url += delim + 'href_target=false';

        let params = SplunkLightbox.getUriParams(SplunkLightbox.url);
        if ( php_script[1] === 'mxd_report_parser' ) {
          title = params.report.split(/[\\/]/).pop();
        } else if ( php_script[1] === 'view' ) {
          title = params.file.split(/[\\/]/).pop();
        }
      }

      let content_el = $('#splunk_content');
      let overlay_el = $('#splunk_overlay');

      let remote_obj = '<object id="splunk_object" type="text/html"'
                     + '        data="' + SplunkLightbox.url  + '"'
                     + '        width="100%" height="100%"'
                     + '        style="overflow:auto; border: 1px ridge lightblue">'
                     + '</object>';

      let error_msg  = '<div style="text-align: center;">'
                     + '  <span class="glyphicon glyphicon-eye-close"'
                     + '        style="font-size: 50px;">'
                     + '  </span><br/>'
                     + '  <h2>Unable to load</h2>'
                     + '  <h2 style="word-wrap: break-word;">' + SplunkLightbox.url
                     + '  </h2>'
                     + '</div>'

      let load_timout_fn = setTimeout(function() {
        content_el.html(error_msg);
        spinner.stop();
        overlay_el.hide();
      }, 30000);

      $('#splunk_title').html(title);
      content_el.html(remote_obj);
      overlay_el.html(spinner.el);
      overlay_el.show();

      $('#splunk_object')[0].addEventListener('load', function() {
        clearTimeout(load_timout_fn);
        spinner.stop();
        overlay_el.hide();
      });

      $('#splunk_modal').on('hidden.bs.modal', function () {
        clearTimeout(load_timout_fn);
        overlay_el.hide();
        spinner.stop();
        $('#splunk_object').detach();
        SplunkLightbox.modal_open = false;
      });
    }

    SplunkLightbox.refresh = function(event) {
      if ( !SplunkLightbox.modal_open ) {
        return false;
      }

      SplunkLightbox.build();

      return false;
    }

    SplunkLightbox.show = function(event) {
      if ( SplunkLightbox.modal_open ) {
        return false;
      }

      SplunkLightbox.url = event.target.href || event.target.closest('a').href;

      if ( !SplunkLightbox.url ) {
        return false;
      }

      SplunkLightbox.modal_open = true;

      SplunkLightbox.build();

      $('#splunk_modal').modal();

      return false;
    }

    $(document).on('click', "a.open-popup-link", function(e) {
      e.preventDefault();      
      SplunkLightbox.show(e);
    });


    $('#splunk_tab_btn').click(function(e) {
      e.preventDefault();
      SplunkLightbox.open_tab(e);
    });

    $('#splunk_refresh_btn').click(function(e) {
      e.preventDefault();
      SplunkLightbox.refresh(e);
    }); 
  });
});

