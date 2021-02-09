// uses trenary coordiantes on an equilateral triangle
// see https://en.wikipedia.org/wiki/Barycentric_coordinate_system
// Created by Gary H. McClelland, Professor Emeritus | University of Colorado Boulder
// modified by Jan Simson
function createTriangle(id, options) {
  var canvas = d3.select('#' + id);

  var height = canvas.attr('height');
  var width = canvas.attr('width');

  var topLabel = options[0];
  var leftLabel = options[1];
  var rightLabel = options[2];

  var margin = { 'left': 30, 'right': 30, 'top': 30, 'bottom': 30 };

  var xScale = d3.scale.linear()
    .domain([0, 1])
    .range([margin.left, width - margin.right]);

  var yScale = d3.scale.linear()
    .domain([0, 1])
    .range([height - margin.bottom, margin.top]);

  var pA = .3333;
  var pB = .3333;
  var pC = 1 - pA - pB;

  var dataProb = [ pA, pB, pC ];
  var inputs = [
    d3.select('input[name="' + id + '_' + options[0] + '"]'),
    d3.select('input[name="' + id + '_' + options[1] + '"]'),
    d3.select('input[name="' + id + '_' + options[2] + '"]'),
  ];

  function ternary(a, b) {
    c = 1 - a - b;
    x = a * (0) + b * (.5) + c * (1);
    y = a * (0) + b * (Math.sqrt(3) / 2) + c * (0);
    return [x, y];
  }

  ternaryX = ternary(pA, pB)[0];
  ternaryY = ternary(pA, pB)[1];

  function invertTernary(x, y) {
    probA = ((Math.sqrt(3) / 2 - 0) * (x - 1) + (1 - .5) * (y - 0)) / ((Math.sqrt(3) / 2 - 0) * (0 - 1) + (1 - .5) * (0 - 0));
    probB = ((0 - 0) * (x - 1) + (0 - 1) * (y - 0)) / ((Math.sqrt(3) / 2 - 0) * (0 - 1) + (1 - .5) * (0 - 0));
    probC = 1 - probA - probB;
    return [probA, probB, probC];
  }


  var side1 = canvas.append('line')
    .attr('x1', xScale(0))
    .attr('y1', yScale(0))
    .attr('x2', xScale(0.5))
    .attr('y2', yScale(Math.sqrt(3) / 2))
    .style('stroke', 'black').style('stroke-width', 2);

  var side2 = canvas.append('line')
    .attr('x1', xScale(0.5))
    .attr('y1', yScale(Math.sqrt(3) / 2))
    .attr('x2', xScale(1))
    .attr('y2', yScale(0))
    .style('stroke', 'black').style('stroke-width', 2);

  var side3 = canvas.append('line')
    .attr('x1', xScale(0))
    .attr('y1', yScale(0))
    .attr('x2', xScale(1))
    .attr('y2', yScale(0))
    .style('stroke', 'black').style('stroke-width', 2);


  var cursor = canvas.append('circle')
    .attr('cx', xScale(ternaryX))
    .attr('cy', yScale(ternaryY))
    .attr('r', 10)
    .style('fill', d3.rgb("#B03F3B"));

  var aText = canvas.append('text')
    .attr('x', 0)
    .attr('y', height - 10)
    .style('text-anchor', 'start')
    .text(topLabel + ' = ' + d3.format('.2f')(pA));

  var bText = canvas.append('text')
    .attr('x', xScale(0.5))
    .attr('y', yScale(Math.sqrt(3) / 2) - 5)
    .style('text-anchor', 'middle')
    .text(leftLabel + ' = ' + d3.format('.2f')(pB));

  var cText = canvas.append('text')
    .attr('x', width)
    .attr('y', height - 10)
    .style('text-anchor', 'end')
    .text(rightLabel + ' = ' + d3.format('.2f')(pC));


  // Allow dragging on the whole canvas
  canvas.call(d3.behavior.drag().on('drag', dragmove));

  var formatNums = d3.format('.2f');
  function dragmove(d) {
    tempX = d3.mouse(this)[0];
    tempY = d3.mouse(this)[1];
    tempTernary = invertTernary(xScale.invert(tempX), yScale.invert(tempY));

    if ((tempTernary[0] >= 0) && (tempTernary[1] >= 0) && (tempTernary[2] >= 0)) {

      cursor
        .attr('cx', tempX)
        .attr('cy', tempY);

      aText.text(topLabel + ' = ' + formatNums(tempTernary[0]));
      bText.text(leftLabel + ' = ' + formatNums(tempTernary[1]));
      cText.text(rightLabel + ' = ' + formatNums(tempTernary[2]));

      dataProb = [ tempTernary[0], tempTernary[1], tempTernary[2] ];
      for (var i = dataProb.length - 1; i >= 0; i--) {
        if (inputs[i]) {
          inputs[i].attr('value', formatNums(dataProb[i]));
        }
      }
    }
  }
}