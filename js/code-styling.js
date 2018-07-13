(function() {
  function styleCodeOnResize() {
    clearTimeout(window.resizedFinished);
    window.resizedFinished = setTimeout(function(){
        styleCode();
    }, 250);
  }

  function unstyleCode() {
    var jq = jQuery;
    jq("article pre").css("left", "auto");
    jq("article pre code").css("margin-left", "auto");
    jq("article pre code").css("margin-right", "auto");
    jq("article pre").css("width", "auto");
  }

  function styleCode() {
    if (typeof jQuery !== 'undefined') {
      var jq = jQuery;

      var ww = jq(window).width();
      var bw = jq("main").width();
      var margin = (ww - bw)/2;
      jq("article pre").css("left", "-" + margin + "px");
      jq("article pre code").css("margin-left", margin + "px");
      jq("article pre code").css("margin-right", margin + "px");
      jq("article pre").css("width", ww + "px");

      jq(window).resize(styleCodeOnResize);
      window.onbeforeprint = unstyleCode;
      window.onafterprint = styleCode;
    } else {
      window.setTimeout(styleCode, 1000);
    }
  }

  styleCode();
}());
