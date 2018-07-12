function canvasOnImg(img) {
    var canvas = document.createElement('canvas');
    canvas.width = img.width;
    canvas.height = img.height;
    let context = canvas.getContext('2d');
    context.drawImage(img, 0, 0, img.width, img.height);
    return canvas
}

function countBlack(data) {
    var blackCount = 0;
    let pixelCount = (400 * 400);
    for (var i = 0; i < pixelCount; i++) {
        var offset = (i * 4);
        if (data[offset] === 1 && data[offset + 1] === 1 && data[offset + 2] === 1) {
            blackCount++
        }
    }
    return blackCount
}

function drawCircle(context, centerX, centerY, radius) {
    context.beginPath();
    context.arc(centerX, centerY, radius, 0, 2 * Math.PI, false);
    context.stroke()

}



function forEachHtmlCollection(htmlCollection, func) {
    for (var i = 0; i < htmlCollection.length; i++) {
        var element = htmlCollection[i]
        func(element)
    }
}

function testCircle(x, y, img, testCanvas, testContext, circleSize, count) {
    var offset = (x * 4) + y
    var data = testContext.getImageData(0,0,400,400).data
    if (data[offset] === 1 && data[offset + 1] === 1 && data[offset + 2] === 1) {
        return false;
    }
    drawCircle(testContext, x, y, circleSize)
    let counted = countBlack(testContext.getImageData(0,0,400,400).data);
    if (counted === count) {
        return testCanvas
    }
    else {
        testContext.clearRect(0, 0, 400, 400);
        testContext.drawImage(img, 0, 0, 400, 400);
        return false;
    }
}

window.onload = function () {
    forEachHtmlCollection(document.getElementById("playground").getElementsByTagName('img'), img => {
            var originalCanvas = canvasOnImg(img);
            var originaldata = originalCanvas.getContext('2d').getImageData(0, 0, 400, 400).data
            var testCanvas = canvasOnImg(img)

            var testContext =testCanvas.getContext('2d')
            testContext.lineWidth = 1;
            testContext.strokeStyle = '#f11';
            var count = countBlack(originaldata);
            var foundCanvas = false;
            for (var i = 0; i < 200 && !foundCanvas; i=i+0.5) {
                console.log(i)
                var circleSize = 200 - i;
                for (var x = -i; x <= i && !foundCanvas; x = x + 0.5) {
                    for (var y = -i ; y <= i && !foundCanvas; y = y + 0.5) {
                        foundCanvas = testCircle(x + 200, y+200, img, testCanvas, testContext,circleSize, count)
                    }
                }
            }
            if (foundCanvas) {
                img.src = foundCanvas.toDataURL()
            }
        }
    )


}