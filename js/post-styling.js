(function() {
  function restyleOnResize() {
    clearTimeout(window.resizedFinished);
    window.resizedFinished = setTimeout(function(){
        doStyle();
    }, 250);
  }

  function unstyleCode() {
    var jq = jQuery;
    jq("article pre").each(function() {
      if (jq(this).parents("section.footnotes").length == 0) {
        jq(this).css("left", "auto");
        jq(this).css("width", "auto");
      }
    });
    jq("article pre code").each(function() {
      if (jq(this).parents("section.footnotes").length == 0) {
        jq(this).css("margin-left", "auto");
        jq(this).css("margin-right", "auto");
      }
    });
  }

  function styleCode() {
    var jq = jQuery;

    var ww = jq(window).width();
    var margin = jq("main article").offset().left;
    var bw = jQuery("main article").width();

    jq("article pre").each(function() {
      if (jq(this).parents("section.footnotes").length == 0) {
        jq(this).css("left", "-" + margin + "px");
        jq(this).css("width", ww + "px");
      }
    });
    jq("article pre code").each(function() {
      if (jq(this).parents("section.footnotes").length == 0) {
        jq(this).css("margin-left", margin + "px");
        jq(this).css("margin-right", (ww - margin - bw) + "px");
      }
    });
  }

  function sidenotesEnabled() {
    return jQuery(".footnotes").length != 0
      && window.location.search.indexOf("nosidenotes") == -1;
  }

  function unstyleFootnotes() {
    if (sidenotesEnabled()) {
      jQuery("body").removeClass("sidenotes");
      jQuery(".footnote-ref").css("display", "initial");
    }
  }

  function styleFootnotes() {
    if (sidenotesEnabled()) {
      var jq = jQuery;

      if (jq(window).width() > 1500) {
        jq("body").addClass("sidenotes");
        jq(".footnote-ref").css("display", "initial");

        jq(".footnotes li").each(function() {
          var fn = jq(this);
          var id = parseInt(fn.attr("id").substr(2));
          var top = jq("#fnref" + id).offset().top - 10;
          fn.css("top", top);
          if (id > 1) {
            var prevId = id - 1;
            var prevFn = jq("#fn" + prevId);
            var prevY = prevFn.offset().top + prevFn.outerHeight();
            if (prevY > top) {
              fn.css("top", prevY);
            }
          }
        });

        jq(".footnote-ref").css("display", "none");
      } else {
        unstyleFootnotes();
      }
    }
  }

  function doStyle() {
    styleFootnotes();
    styleCode();
    styleFootnotes();
  }

  function style() {
    if (typeof jQuery !== 'undefined') {
      if (window.__absr__loaded) {
        doStyle();
      } else {
        jQuery(document).ready(function() {
          doStyle();
          window.__absr__loaded = true;
          jQuery(window).resize(restyleOnResize);
        });
      }
    } else {
      window.setTimeout(style, 1000);
    }
  }

  function unstyle() {
    unstyleCode();
    unstyleFootnotes();
  }

  window.onbeforeprint = unstyle;
  window.onafterprint = style;
  window.__absr__style = style;
  window.__absr__loaded = false;

  style();
}());

(function() {
  function highlight() {
    if (typeof loadStyleSheet !== 'undefined') {
      loadStyleSheet("https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/atom-one-light.min.css");
    } else {
      window.setTimeout(highlight, 500);
    }
  }

  function setupFeatherlight() {
    if (typeof jQuery !== 'undefined' && typeof loadStyleSheet !== 'undefined') {
      if (jQuery("a.img-link").length > 0) {
        loadStyleSheet("https://cdn.jsdelivr.net/npm/featherlight@1.7.13/release/featherlight.min.css");
        jQuery.getScript("https://cdn.jsdelivr.net/npm/featherlight@1.7.13/release/featherlight.min.js", function() {
          jQuery("a.img-link").attr("data-featherlight", "image");
        });
      }
    } else {
      window.setTimeout(setupFeatherlight, 500);
    }
  }

  highlight();
  setupFeatherlight();
}());
