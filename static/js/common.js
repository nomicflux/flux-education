"use strict"
var console, $;

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
    var html = "<ul>";
    for(var i = 0, _is = lessons.length; i < _is; i++) {
      html += lessonToEl(lessons[i]);
    }
    html += "</ul>";
    completionDiv.html(html);
  });
}