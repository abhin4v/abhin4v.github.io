(function() {
  var slidesWindow = document.getElementById("slides").contentWindow;
  slidesWindow.addEventListener("load", function() {
    var slideShow = slidesWindow.slideshow;
    var slideCount = slideShow.getSlideCount();

    var progressBar = document.getElementById("slides-progress");
    progressBar.addEventListener("click", function(e) { 
      var percent = e.offsetX / e.srcElement.offsetWidth;
      slideShow.gotoSlide(Math.max(1, Math.round(percent * slideCount)));
    });

    progressBar.max = slideCount;
    progressBar.value = slideShow.getCurrentSlideIndex() + 1;

    slideShow.on('showSlide', function (slide) {
      var idx = slide.getSlideIndex();
      progressBar.value = idx + 1;
      _paq.push(['trackEvent', 'Slide', 'Show', '$slides_path$-' + (idx + 1)]);
    });

    document.querySelector(".notes-button").addEventListener("click", function() {
      slideShow.togglePresenterMode();
      slidesWindow.focus();
      _paq.push(['trackEvent', 'Slide', 'ToggleNotes', '$slides_path$']);
    });

    document.querySelector(".fullscreen-button").addEventListener("click", function() {
      slideShow.toggleFullscreen();
      slidesWindow.focus();
      _paq.push(['trackEvent', 'Slide', 'ToggleFullScreen', '$slides_path$']);
    });
  });
})();
