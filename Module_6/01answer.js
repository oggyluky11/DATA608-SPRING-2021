d3.csv('ue_industry.csv', data => {

    // Define your scales and generator here.

    let xScale = d3.scaleLinear()
        .domain(d3.extent(data,d=>+d.index))
        .range([1180,20]);
    let yScale = d3.scaleLinear()
        .domain(d3.extent(data,d=>+d.Agriculture))
        .range([580,20]);

    let line3 = d3.line()
        .x(d=>xScale(d.index))
        .y(d=>yScale(d.Agriculture));

    d3.select('#answer1')
        // append more elements here
        .append('path')
        .attr('d', line3(data))
        .attr('stroke', '#2e2928');

});