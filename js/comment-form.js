(function() {
  function addCommentSubmitHandler() {
    if (typeof jQuery !== 'undefined') {
      var jq = jQuery;

      function timeout(ms, promise) {
        return new Promise(function(resolve, reject) {
          window.setTimeout(function() {
            reject(new Error("timeout"))
          }, ms)
          promise.then(resolve, reject)
        })
      }

      var formMessage = jq("#comment-form-message");
      var toutId = null;
      function showMsg(msg) {
        formMessage.html(msg).fadeIn("slow");
        if (toutId !== null) {
          window.clearTimeout(toutId);
        }
        toutId = window.setTimeout(function() { formMessage.fadeOut("slow"); }, 5000);
      }

      jq("#comment-redirect").remove();
      jq("#comment-form").on("submit", function(event) {
        event.preventDefault();
        if (grecaptcha.getResponse() === "") {
          showMsg("Please tick the reCAPTCHA");
          return false;
        }
        var form = jq(this);
        var fieldSet = jq("#comment-form fieldset");
        var reqBody = form.serialize();
        fieldSet.attr("disabled", "disabled");

        var formSubmit = jq("#comment-form-submit");
        var origVal = formSubmit.val();
        formSubmit.val("Posting ...");

        timeout(15000, fetch(form.attr("action"), {
          method: form.attr("method"),
          body: reqBody,
          headers: {
            "Content-Type": "application/x-www-form-urlencoded"
          }}))
        .then(function(response) {
          formSubmit.val(origVal);
          fieldSet.removeAttr("disabled");

          if (!response.ok) {
            showMsg("Sorry, there was an error in posting the comment. Please try again.");
          } else {
            form.trigger("reset");
            grecaptcha.reset();
            showMsg("Thanks for your comment. It will show on the site once it has been approved.");
          }
        })
        .catch(function(error) {
          formSubmit.val(origVal);
          fieldSet.removeAttr("disabled");
          grecaptcha.reset();
          showMsg("Sorry, there was an error in posting the comment. Please try again.");
        });
        return false;
      });
    } else {
      window.setTimeout(addCommentSubmitHandler, 1000);
    }
  }

  addCommentSubmitHandler();
}());
