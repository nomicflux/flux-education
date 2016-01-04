"use strict"
var console, $, Elm;

function goBack() {
  window.history.back();
}

function startApp(module) {
    var elmApp = document.getElementById("elmApp");
    var elmObj = Elm["Lesson"]; // Elm[module];
    var app = Elm.embed(elmObj, elmApp, {sendLesson: null});

    var equations = [
        ["2 + 3 = x"],
        ["5 + 7 = x"],
        ["2 + 40 = x"],
        ["2 - 1 = x",
         "x + 3 = y",
         "y - x = z"]
        ]

    var colors = [
        [0, 1, 0.5, 0.7],
        [120, 1, 0.5, 0.7],
        [240, 1, 0.5, 0.7]
    ];

    var lesson = {
        equations: equations,
        colors: colors,
        visual: "NumberLine"
    };

    app.ports.sendLesson.send(
            {
                equations: equations,
                mcolors: null,
                visual: "NumberLine"
            }
        );

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
