function extractAttrs({ id, offsetTop, offsetLeft, clientWidth, clientHeight }) {
    return { id, offsetTop, offsetLeft, clientWidth, clientHeight };
}

app.ports.elemFromTo.subscribe(function(startEnd) {
    var elemStart = document.elementFromPoint(
        startEnd[0][0],
        startEnd[0][1]
    );
    var elemEnd = document.elementFromPoint(
        startEnd[1][0],
        startEnd[1][1]
    );
    app.ports.elemFromToUpdate.send(JSON.stringify({
        start: extractAttrs(elemStart),
        end: extractAttrs(elemEnd)
    }));
});

app.ports.updateCanvas.subscribe(function() {
    var canvas = document.getElementById("canvas");
    app.ports.canvasUpdate.send(JSON.stringify(extractAttrs(canvas)));
});

app.ports.saveState.subscribe(function(json) {
    localStorage.setItem('state', json);
});

app.ports.loadState.subscribe(function() {
    app.ports.loadStateUpdate.send(localStorage.getItem('state'));
});
