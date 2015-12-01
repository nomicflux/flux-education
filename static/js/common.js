"use strict"
var console, $, Elm;

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
    var html = "<div id='nowCompleted' class='hidden'><ul>";
    for(var i = 0, _is = lessons.length; i < _is; i++) {
      html += lessonToEl(lessons[i]);
    }
    html += "</ul></div>";
    html += "<div id='notCompletedYet'>Complete all questions in order to continue.</div>";
    completionDiv.html(html);
  });
}

function switchCompletionDivs(val) {
  $("#notCompletedYet").fadeOut();
  $("#nowCompleted").removeClass("hidden").fadeIn();
}