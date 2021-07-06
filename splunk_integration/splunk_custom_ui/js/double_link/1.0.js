define(function() {
  return {
    exec: function($td, cell, row, tokens) {
      var tokenValue = tokens.get('double_link_fields');
      var tokenValueList = tokenValue ? tokenValue.split(',') : [];
      if (tokenValueList && (tokenValueList.includes(cell.field) || tokenValueList.includes('"' + cell.field + '"'))) {
        var value = cell.value;
        var urlField = cell.field + '_link';

        var url = row[urlField];
        var res = url ? url.split('|') : [];

        if (res.length === 2) {
          var link1 = $('<a>')
            .attr('href', res[0])
            .text(value);
          var link2;

          var d = res[1].match(/(http(s)?:\/\/.)?(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)/g);
          var validPicUrl = false;
          if (d != null) {
            validPicUrl = true;
          }

          if (validPicUrl) {
            link2 = $('<a>')
              .attr('href', res[1])
              .addClass('icon-document');
          }

          $td
            .addClass('table-link')
            .append($('<span style="float:left">').wrapInner(link1))
            .append($('<span style="float:right">').wrapInner(link2));

          link1.click(function(e) {
            e.preventDefault();
            window.open($(e.currentTarget).attr('href'), '_blank');
          });

          if (validPicUrl) {
            link2.click(function(e) {
              e.preventDefault();
              window.open($(e.currentTarget).attr('href'), '_blank');
            });
          }
          return true;
        } else {
          return false;
        }
      } else {
        return false;
      }
    }
  };
});
