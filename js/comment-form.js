(function(){function addCommentSubmitHandler(){if(typeof jQuery!=='undefined'){var jq=jQuery,formMessage=jq("#comment-form-message"),toutId=null;function showMsg(msg){formMessage.html(msg).fadeIn("slow");if(toutId!==null)window.clearTimeout(toutId);toutId=window.setTimeout(function(){formMessage.fadeOut("slow")},5000)};jq("#comment-redirect").remove();jq("#comment-form").on("submit",function(event){event.preventDefault();if(grecaptcha.getResponse()===""){showMsg("Please tick the reCAPTCHA");return false};var form=jq(this),reqBody=form.serialize();jq("#comment-form fieldset").attr("disabled","disabled");var formSubmit=jq("#comment-form-submit"),origVal=formSubmit.val();formSubmit.val("Posting ...");fetch(form.attr("action"),{method:form.attr("method"),body:reqBody,headers:{"Content-Type":"application/x-www-form-urlencoded"}}).then(function(response){formSubmit.val(origVal);jq("#comment-form fieldset").removeAttr("disabled");if(!response.ok){showMsg("Sorry, there was an error in posting the comment.")}else{form.trigger("reset");grecaptcha.reset();showMsg("Thanks for your comment. It will show on the site once it has been approved.")}});return false})}else window.setTimeout(addCommentSubmitHandler,1000)};addCommentSubmitHandler()}())