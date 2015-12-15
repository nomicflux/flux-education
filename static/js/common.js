"use strict"
var console, $, Elm;

function goBack() {
  window.history.back();
}

function startApp(module) {
  var elmApp = document.getElementById("elmApp");
  var elmObj = Elm[module];
  var app = Elm.embed(elmObj, elmApp);
  app.ports.signalCompletion.subscribe(switchCompletionDivs);
}

function lessonToEl(lesson) {
  console.log(lesson);
  var id = lesson[0];
  var data = lesson[1];
  return "<li><a href='/page/"+String(id)+"'>"+ data.goon+"</a></li>";
};

function loadNextFiles(lessonId) {
  var completionDiv = $("#completionDiv");
  var url = "/lessons/requiredby/" + String(lessonId);
 // console.log("Trying to get: ", lessonId, url);
  $.get(url, function(lessons, status) {
    var html = "<div id='nowCompleted' class='hidden'>";
    if(lessons.length != 0) {
      html += "<section><h4>Up next:</h4><ul>";
      for(var i = 0, _is = lessons.length; i < _is; i++) {
        html += lessonToEl(lessons[i]);
      }
      html += "</ul></section>";
    } else {
      html += "<section><h4>You've reached the current end of that branch!</h4></section>";
    }
    html += "</div><div id='notCompletedYet'>Complete all questions in order to continue.</div>";
    if(lessonId > 3) {
      html += "<section><h4>Take a different path?</h4><a onClick='goBack()'>Go Back</a></section>";
    }
    completionDiv.html(html);
  });
}

function switchCompletionDivs(val) {
  $("#notCompletedYet").fadeOut();
  $("#nowCompleted").removeClass("hidden").fadeIn();
}