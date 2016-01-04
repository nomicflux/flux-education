"use strict";
var console, $, Elm;

function pageTracker() {
    var pages = [];
    var temp = [];
    return {
        recordPage: function(info) {
        addToSet(info, pages);
        },
        recordProvisionally: function(info) {
        addToSet(info, temp);
        },
        writeProvisional: function() {
            for(var i = 0, _is = temp.length; i < _is; i++) {
                addToSet(temp[i], pages);
            }
            temp = [];
        },
        getPages: function() {
            return pages;
        }
    };
}
var visited = pageTracker();
var prereqqed = pageTracker();

function addToSet(item, set) {
    for(var i = 0, _is = set.length; i < _is; i++) {
        if(set[i] == item) {
            return set;
        }
    }
    return set.push(item);
}

function goBack() {
    window.history.back();
}

function startApp(equations) {
    var elmApp = document.getElementById("elmApp");
    var elmObj = Elm["Lesson"]; // Elm[module];
    var app = Elm.embed(elmObj, elmApp, {
        sendLesson: null
    });

    app.ports.sendLesson.send({
        equations: equations,
        mcolors: null,
        visual: "NumberLine"
    });

    app.ports.signalCompletion.subscribe(switchCompletionDivs);
}

function lessonToEl(lesson) {
    var id = lesson[0];
    var data = lesson[1];
    prereqqed.recordProvisionally({id: id, label: data.keyphrase});
    return "<li><a href='javascript:void(0)' onClick='loadPage("+String(id)+")'>" + data.goon + "</a></li>";
};

function sortIntoCompleted(currentId) {
    var ableToDo = prereqqed.getPages(),
        alreadyDone = visited.getPages(),
        uncompleted = [],
        completed = currentId == 1 ? [] : [{id: 1, label: "Back to the beginning"}];
    for(var i = 0, _is = ableToDo.length; i < _is; i++) {
        var found = false,
            able = ableToDo[i];
        if(able.id != currentId) {
            for(var j = 0, _js = alreadyDone.length; j < _js; j++) {
                if(alreadyDone[j] === able.id) {
                    found = true;
                    break;
                }
            }
            if(found == true) {
                completed.push(able);
            } else {
                uncompleted.push(able);
            }
        }
    }
    return {
        completed: completed,
        uncompleted: uncompleted
    };
}

function loadNextFiles(lessonId) {
    var completionDiv = $("#completionDiv");
    var url = "/lessons/requiredby/" + String(lessonId);

    $.get(url, function(lessons, status) {
        var html = "<div id='nowCompleted' class='hidden'><span>";
        if (lessons.length != 0) {
            html += "<section><h4>Up next:</h4><ul>";
            for (var i = 0, _is = lessons.length; i < _is; i++) {
                html += lessonToEl(lessons[i]);
            }
            html += "</ul></section>";
        } else {
            html += "<section><h4>You've reached the current end of that branch!</h4></section>";
        }
        html += "</span>";
        var completion = sortIntoCompleted(lessonId);
        var numUncompleted = completion.uncompleted.length,
            numCompleted = completion.completed.length;
        if(numUncompleted > 0) {
            html += "<span id='uncompletedPages'><h4>You can also do:</h4><ul>";
            for(var i = 0; i < numUncompleted; i++) {
                var page = completion.uncompleted[i];
                html += "<li><a href='javascript:void(0)' onClick='loadPage("+String(page.id)+")'>" + page.label + "</a></li>";
            }
            html += "</ul></span>";
        }
        if(numCompleted > 0) {
            html += "<span id='completedPages'><h4>Or you can go back and visit:</h4><ul>";
            for(var i = 0; i < numCompleted; i++) {
                var page = completion.completed[i];
                html += "<li><a href='javascript:void(0)' onClick='loadPage("+String(page.id)+")'>" + page.label + "</a></li>";
            }
            html += "</ul></span>";
        }
        html += "</div><div id='notCompletedYet'>Complete all questions in order to continue.</div>";
        completionDiv.html(html);
    });
}

function switchCompletionDivs(val) {
    $("#notCompletedYet").fadeOut();
    $("#nowCompleted").removeClass("hidden").fadeIn();
    prereqqed.writeProvisional();
    visited.writeProvisional();
}

var waitingPage = "<div class='alert alert-warning'><i class='fa fa-spinner fa-pulse'></i> Loading Lesson...</div>";

function loadPage(n) {
    $("#page").html(waitingPage);
    $.get("/page/"+String(n), function(data) {
        $("#page").html(data);
        visited.recordProvisionally(n);
    });
}

$(function() {
    $("#startAlgebra").click(function(e) {
        loadPage(1);
    });
});
