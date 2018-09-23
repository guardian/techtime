const CIRCLE_RADIUS = 0.5;

function getPlacement(context) {
    for (x = 1; x <= 400; x++) {
        for (y = 1; y <= 400; y++) {
            const pixelData = context.getImageData(x, y, 1, 1).data;

            const pixelColour = Array.from(pixelData).slice(0, 3);

            if (Math.max(...pixelColour) > 200) {
                return [x, y];
            }
        }
    }
}

window.onload = function() { 
    var images = Array.from(document.getElementsByTagName('img'));

    console.log(images);

    function test(element) {
        var canvas = document.createElement('canvas');
        canvas.setAttribute("style", "z-index: 999; position: absolute;");
        canvas.width = element.width;
        canvas.height = element.height;
        canvas.getContext('2d').drawImage(element, 0, 0, element.width, element.height);
      
        const context = canvas.getContext('2d');
        const placement = getPlacement(context);

        context.strokeStyle = 'red';
        context.beginPath();
        context.arc(placement[0],placement[1],CIRCLE_RADIUS,0,2*Math.PI);
        context.stroke();

        element.parentNode.insertBefore(canvas, element);
    }

    images.forEach(test);
};
