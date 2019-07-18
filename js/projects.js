(function() {
  function addHandler() {
    if (typeof jQuery !== 'undefined') {
      var hidden = false;
      jQuery("dt em").on("click", function() {
        if (hidden) {
          jQuery("dt, dd").fadeIn();
          hidden = false;
        } else {
          var tag = jQuery(this).text();
          var dts = jQuery("dt em:not(:contains('" + tag + "'))").parent()
                    .not(jQuery("dt em:contains('" + tag + "')").parent());
          dts.fadeOut();
          dts.next("dd").fadeOut();
          hidden = true;
        }
      });
    } else {
      window.setTimeout(addHandler, 1000);
    }
  }

  addHandler();
}());