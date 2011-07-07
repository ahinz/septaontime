/**
 * Create new grapah by execute an async ajax request to
 * load historic data from the web services
 *
 * Parameter elements:
 * @param element string element to render to
 * @param start,end  start and end times to render (decimal hours)
 * @param offset how much data to look backwards at (decimal hours)
 * @param incr the amount to increment each model (decimal hours)
 * @param date Date object specifying the date to run the model with
 * @param startStation the id of the starting station
 * @param startStationName the name of the starting station
 * @param endStation the id of the ending station
 * @param endStationName the name of the ending station
 * @param direction the direction
 */
function createGraph(params) {

    var startdate = params.date.getTime();
    var enddate = startdate + 1000*60*60*25;
    var n = (params.end - params.start)/params.incr;

    $.ajax({
	url: baseURL + "station/" + params.startStation + 
	    "/to/" + params.endStation + "/" + params.direction + "?endDate=" + enddate +
	    "&startDate=" + startdate + "&startTime=" + params.start + "&endTime=" + 
	    (parseFloat(params.start)+parseFloat(params.offset)) + "&" + "seriesTimeIncrement=" + params.incr + 
	    "&numberOfRuns=" + n,
	dataType: 'jsonp',
	timeout: 1000*60,
	success: function( content ) {
	    if (params.callback !== undefined) {
		params.callback();
	    }

	    var data = [];
	    var count = 0;
	    for(var i=parseFloat(params.start);i<parseFloat(params.end);i+=parseFloat(params.incr)) {
		data.push([i + params.offset, content[count]]);
		count += 1;
	    }
	    
	    renderChart(data, 
			"Time to " + params.startStationName + 
			" to " + params.endStationName,
			params.element);
	}
    });
}

/**
 * Render the data into a chart in the given container
 *
 * @param array of pairs to render
 * @param title the title of the chart
 * @param container a string id of the container to render to
 */
function renderChart( data, title, container ) {
    chart1 = new Highcharts.Chart({
	chart: {
	    renderTo: container,
	    defaultSeriesType: 'spline'
	},
	title: {
	    text: title
	},
	xAxis: {
	    //categories: $.map(data, function ( datum ) { return datum[0]; }),
	    plotBands: [{ 
		from: 7.5,
		to: 9.5,
		color: 'rgba(68, 170, 213, 0.1)',
		label: {
		    text: 'Morning Rush',
		    style: {
			color: '#606060'
		    }
		}
            },{
		from: 16.0,
		to: 19.0,
		color: 'rgba(68, 170, 213, 0.1)',
		label: {
		    text: 'Afternoon Rush',
		    style: {
			color: '#606060'
		    }
		}
	    }]
	},
	tooltip: {
	    formatter: function() {
		return '<b>'+ this.series.name +'</b><br/>'+
		    this.x  +': '+ this.y;
	    }
	},
	series: [{
	    name: 'Route Time',
	    data: data 
	}]
    });
}

/**
 * Perform an async AJAX call to determine the bus route between two
 * lat/lon pts.
 *
 * @param lat1,lon1     First point
 * @param lat2,lon2     Second point
 * @param route         Route to draw
 * @param map           Map to draw on
 * @param bounds        Current map bounds
 * @param busLines      List of current lines representing the route on the map
 *
 * @mutates bound       Upon ajax async return bounds will include this line
 * @mutates busLines    Upon ajax async return this array will contain a ref to the bus
 */
function drawLine(lat1,lon1,lat2,lon2,route,map,bounds,busLines,color) {
    $.ajax({
	url: baseURL + "map/" + route + "/" + lat1 + "," + lon1 + "/to/" + lat2 + "," + lon2 + "/",
	dataType: 'jsonp',
	success: function( points ) {
	    busLines.push(
		new google.maps.Polyline({
		    path: points.map(function( point ) {
			var ll = new google.maps.LatLng(point.lat, point.lon);
			if (bounds == null) {
			    bounds = new google.maps.LatLngBounds(ll,ll);
			} else {
			    bounds = bounds.extend(ll);
			}
			
			return ll;
		    }),
		    map: map,
		    strokeColor: color || "#FF0000",
		    strokeOpacity: 1.0,
		    strokeWeight: 2
		}));

		map.fitBounds(bounds);
	    }
	});
}

/**
 * Completely render the results table
 *
 * @param table String id of the table to render to
 * @param busses array of bus est objects
 * @param timeToDest (Optional) time offset to destination
 */
function renderTable(table, busses, timeToDest) {
    $("#" + table + " tr").map(function( index, dom ) {
	if (index > 0) {
	    $(dom).remove();
	}
    });

    var table = $("#" + table);
    busses.map(function( bus ) {
	table.append(createResultRow(bus, timeToDest));
    });

    if ((busses == null || busses.length == 0) && typeof timeToDest !== "undefined") {
	table.append("<tr><td colspan=\"4\">Estimate time to arrival: " + timeToDest + "</td></tr>");
    }
}

function dateToDecimalHours(date) {
    return parseFloat(date.getHours()) + parseFloat(date.getMinutes() / 60.0);
}

function createResultRow(busest, timeToDest) {
    var timeStr = ""
    if(typeof timeToDest !== "undefined") {
	timeStr = timeToDest;
    }

    return "<tr class=\"result\"><td>" + busest.busId + "</td>" +
	"<td>" + busest.blockId + "</td>" +
	"<td>" + busest.arrival[0] + "</td>" + 
	"<td>" + timeStr + "</td></tr>";
}

function decimalHoursToTime(dhours) {
    var hours = parseInt(dhours);
    var minutes = (dhours - hours)*60;

    if (minutes == 0) {
	minutes = "00";
    } else if (minutes < 10) {
	minutes = "0" + minutes;
    } else {
	minutes = "" + minutes;
    }

    var ampm = "am";

    if (hours >= 12) {
	ampm = "pm";
    }
    if (hours >= 13) {
	hours -= 12;
    }


    return hours + ":" + minutes.substring(0,2) + " " + ampm;
}

function fillTimeSelect(sel, from, to, incr) {
    var select = $("#" + sel);
    for(var j=from;j<=to;j+=incr) {
	select.append('<option value="' + j + '">' + decimalHoursToTime(j) + "</option>");
    }
}

function mk_color( mph ) {
    var greenpt = 55; // Point at which all values above are green
    var midpt = greenpt / 2.0;
    if (mph < midpt) {
	return "#FF" + pad2(Math.floor((mph/midpt)*255.0).toString(16)) + "00";
    } else if (mph > greenpt) {
	return "#00FF00";
    } else {
	var clr = (1.0 - mph/greenpt)*255.0;
	return "#" + pad2(Math.floor(clr).toString(16)) + "FF00";
    }
}

function pad2(str) {
    if (str.length < 2) {
	return "0" + str;
    } else {
	return str;
    }
}

function createRouteVelocityBand(tgtDiv, selectedRoute,selectedDirection,slatlng,date,stime,etime,cb) {
    var incr = 0.25;
    var offset = 2.0;
    var numModels = parseInt((etime - stime + offset)/incr);
    var div = $("#" + tgtDiv);

    div.empty();

    $.ajax({
	url: baseURL + "map/intervals/" + selectedRoute + "/" + selectedDirection + "/" + slatlng.lat() + "," + slatlng.lng() + "/?date=" + date.getTime() + "&time=" + (stime - offset) + "&offset=" + offset +"&map=0&incr=0.25&n=" + numModels,
		dataType: 'jsonp',
		success: function( ests ) {
		    if (cb !== undefined) {
			cb();
		    }

		    var width = 680.0;

		    var max = 0.0;
		    var nRows = 0;

		    ests.map(function( est ) {
			var dist = est.endDist;
			if (dist > max) {
			    max = dist;
			}
			if (est.v.length > nRows) {
			    nRows = est.v.length;
			}
		    });

		    var pixelsPerKm = width / max;

		  
		    var t = stime;
		    for(var i=0;i<nRows;i+=1) {
			var divs = "<div>";
			ests.map(function( est ) {
			    divs  += '<div style="height: 6px; width:' + ((est.endDist - est.startDist)*pixelsPerKm) + 'px; background-color: ' + mk_color(est.v[i] * 2.23693629) + '; float:left;">&nbsp;</div>';
			});
			if (i % 4 == 0) {
			    divs += '<div style="float: left; height: 6px;">' +  decimalHoursToTime(stime) + '</div>';
			}
			stime += incr;
			divs += '<div style="clear: both;"></div></div>';
			div.append(divs);
		    }
		    
		}
	    });
}

function createRouteVelocityGraph(mapElement,selectedRoute,selectedDirection,slatlng,date,time,cb) {
    $("#" + mapElement).empty();

    var latlng = new google.maps.LatLng(39.9522222, -75.1641667);
    var myOptions = {
	zoom: 12,
	center: latlng,
	mapTypeId: google.maps.MapTypeId.ROADMAP
    };

    $.ajax({
	url: baseURL + "map/intervals/" + selectedRoute + "/" + selectedDirection + "/" + slatlng.lat() + "," + slatlng.lng() + "/?date=" + date.getTime() + "&time=" + time + "&offset=2.0",
		dataType: 'jsonp',
		success: function( ests ) {
		    if (cb !== undefined) {
			cb();
		    }

		    var real_map = new google.maps.Map(document.getElementById(mapElement), myOptions);
		    var bounds = null; 
		    ests.map( function( est ) {
			var pts = est.pts.map(function( ll ) {
			    var gll = new google.maps.LatLng(ll.lat, ll.lon);
			    if (bounds == null) {
				bounds = new google.maps.LatLngBounds(gll);
			    } else {
				bounds = bounds.extend(gll);
			    }

			    return gll;
			});

			var mph = est.ival.v[0] * 2.23693629;

			new google.maps.Polyline({
			    path: pts,
			    strokeColor: mk_color(mph),
			    strokeOpacity: '1.0',
			    strokeWeight: 5,
			    title: mph + " mph",
			    map: real_map
			});
		    });
		    real_map.fitBounds(bounds);
		}
	    });
}
