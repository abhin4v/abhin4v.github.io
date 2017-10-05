(function() {
  function addCommentSubmitHandler() {
    if (typeof jQuery !== 'undefined') {
      var jq = jQuery;
      jq("#comment-redirect").remove();
      jq("#comment-form").on("submit", function(event) {
        event.preventDefault();
        var form = jq(this);
        var reqBody = form.serialize();
        jq("#comment-form fieldset").attr("disabled", "disabled");

        var formSubmit = jq("#comment-form-submit");
        var origVal = formSubmit.val();
        formSubmit.val("Posting ...");

        fetch(form.attr("action"), {
          method: form.attr("method"),
          body: reqBody,
          headers: {
            "Content-Type": "application/x-www-form-urlencoded"
          }})
        .then(function(response) {
          var formMessage = jq("#comment-form-message");
          formSubmit.val(origVal);
          jq("#comment-form fieldset").removeAttr("disabled");

          if (!response.ok) {
            formMessage.html("Sorry, there was an error in posting the comment.").fadeIn("slow");
          } else {
            form.trigger("reset");
            formMessage.html("Thanks for your comment. It will show on the site once it has been approved.").fadeIn("slow");
          }
          window.setTimeout(function() { formMessage.fadeOut("slow"); }, 5000);
        });
        return false;
      });
    } else {
      window.setTimeout(addCommentSubmitHandler, 1000);
    }
  }

  addCommentSubmitHandler();
}());
