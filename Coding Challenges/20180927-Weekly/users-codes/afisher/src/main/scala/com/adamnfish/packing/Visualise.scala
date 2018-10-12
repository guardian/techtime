package com.adamnfish.packing


object Visualise {
  def generateHtml(map: Map, discs: List[Disc]): String = {
    val pointEls = map.points.map { point =>
      s"""<circle class="point" cx='${point.x}' cy='${1000 - point.y}' r='2' fill='black' data-label='${point.label}'
         |onmouseover="highlight(this);"
         |onmouseout="unHighlight(this);"
         |onclick="pin(this);"
         |/>""".stripMargin
    }
    val discEls = discs.map { disc =>
      s"""<circle class="disc" cx='${disc.point.x}' cy='${1000 - disc.point.y}' r='${disc.radius}'
         |onmouseover="highlight(this);"
         |onmouseout="unHighlight(this);"
         |onclick="pin(this);"
         |fill='blue' fill-opacity="0.4" stroke="blue" stroke-width="1" data-label='${disc.point.label}' />""".stripMargin
    }
    s"""<html>
       |<head>
       |  <title>Result ${map.mapId}</title>
       |  <style>
       |    input.current-label {
       |      padding: 5px;
       |      border: solid 1px #006666;
       |      background-color: #f7f7f7;
       |    }
       |  </style>
       |  <script>
       |    function highlight(el) {
       |      if (el.getAttribute('data-pinned') !== 'true') {
       |        el.setAttribute('fill', 'green');
       |        el.setAttribute('stroke', 'green');
       |      }
       |    }
       |    function unHighlight(el) {
       |      var colour = el.classList.contains("disc") ? "blue" : "black";
       |      if (el.getAttribute('data-pinned') !== 'true') {
       |        el.setAttribute('fill', colour);
       |        el.setAttribute('stroke', colour);
       |      }
       |    }
       |    function pin(el) {
       |      unPinAll();
       |      el.setAttribute('data-pinned', 'true');
       |      el.setAttribute('fill', 'red');
       |      el.setAttribute('stroke', 'red');
       |      document.querySelector('.current-label').value = el.getAttribute("data-label");
       |    }
       |    function unPin(el) {
       |      var colour = el.classList.contains("disc") ? "blue" : "black";
       |      el.removeAttribute('data-pinned');
       |      el.setAttribute('fill', colour);
       |      el.setAttribute('stroke', colour);
       |    }
       |    function unPinAll() {
       |      document.querySelector('.current-label').value = '';
       |      [].forEach.call(document.querySelectorAll("circle"), function(el) {
       |        unPin(el);
       |      });
       |    }
       |  </script>
       |</head>
       |<body>
       |  <p>${map.mapId} @ ${map.timestamp}</p>
       |  <p>
       |    <input type="text" class="current-label" value="" />
       |  </p>
       |  <svg style="margin: 10px; background-colour: #f7f7f7; border: solid 1px #ccc;" height="1000" width="1000">
       |    <rect width="100%" height="100%" fill="#f7f7f7"/>
       |    ${discEls.mkString("\n")}
       |    ${pointEls.mkString("\n")}
       |  </svg>
       |</body>
       |</html>
     """.stripMargin
  }
}
