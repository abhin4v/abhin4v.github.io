(function setupPhotoGallery(){
  function getPhotos() {
    var imgs = jQuery('.photocol').toArray().map(function(e) {
      return jQuery(e).find("a.img-link").toArray();
    });

    var reImgs = [];
    for (var i = 0; i < Math.max(...imgs.map(function(a) { return a.length; })); i++) {
      for (var j = 0; j < imgs.length; j++) {
        if (imgs[j].length > i) {
          reImgs.push(imgs[j][i]);
        }
      }
    }

    return jQuery(reImgs);
  }

  function navigateToPhotoHash(photos) {
    var hash = window.location.hash;
    if (hash !== "") {
      hash = hash.substring(1);
      photos.each(function() {
        if (this.href.indexOf(hash) != -1) {
          window.history.replaceState({}, "", window.location.pathname);
          jQuery(this).click();
        }
      });
    }
  }

  if (typeof jQuery !== 'undefined' && typeof loadStyleSheet !== 'undefined') {
    loadStyleSheet("https://cdn.jsdelivr.net/npm/featherlight@1.7.13/release/featherlight.min.css");
    loadStyleSheet("https://cdn.jsdelivr.net/npm/featherlight@1.7.13/release/featherlight.gallery.min.css");

    jQuery.getScript("https://cdn.jsdelivr.net/npm/featherlight@1.7.13/release/featherlight.min.js", function() {
      jQuery.getScript("https://cdnjs.cloudflare.com/ajax/libs/jquery-touch-events/2.0.0/jquery.mobile-events.min.js", function() {
        jQuery.getScript("https://cdn.jsdelivr.net/npm/featherlight@1.7.13/release/featherlight.gallery.min.js", function() {
          var photos = getPhotos();
          photos.featherlightGallery({
            previousIcon: '&#171;',
            nextIcon: '&#187;',
            galleryFadeIn: 300,
            openSpeed: 300,
            loading: 'Loading ...'
          });

          navigateToPhotoHash(photos);
        });
      });
    });
  } else {
    window.setTimeout(setupPhotoGallery, 500);
  }
}());