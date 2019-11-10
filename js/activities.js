(function renderActivityPage() {
  function aggregateActivities(activities, type, period) {
    const chartData = [];
    var i = -1;
    var lastDate = null;
    activities.forEach(function(a) {
      if (a.t !== type) {
        return;
      }

      const date = moment(a.s).startOf(period).toDate();
      const count = a.d;

      if ((lastDate != null) && (lastDate.getTime() === date.getTime())) {
        chartData[i].count += count;
      } else {
        chartData.push({ date: date, count: count });
        i += 1;
        lastDate = date;
      }
    });

    chartData.forEach(function(d) {
      d.count = Math.round(d.count/1000);
    });
    chartData.sort(function(a, b) { return a.date.getTime() - b.date.getTime(); });

    padEarliest(chartData, period);
    padLatest(chartData, period);

    return chartData;
  }

  function padEarliest(chartData, period) {
    const startOfPeriodOneYearAgo = moment().subtract(366, 'days').startOf(period).toDate();
    while (true) {
      const first = chartData[0].date;
      if (first.getTime() > startOfPeriodOneYearAgo.getTime()) {
        const d = moment(first).subtract(1, period).toDate();
        chartData.unshift({ date: d, count: 0 });
      } else {
        break;
      }
    }
  }

  function padLatest(chartData, period) {
    const startOfPeriodToday = moment().startOf(period).toDate();
    while (true) {
      const last = chartData[chartData.length - 1].date;
      if (last.getTime() < startOfPeriodToday.getTime()) {
        const d = moment(last).add(1, period).toDate();
        chartData.push({ date: d, count: 0 });
      } else {
        break;
      }
    }
  }

  if (typeof jQuery === "undefined") {
    window.setTimeout(renderActivityPage, 1000);
  } else {
    function renderCalendar(activities, type, colorRange, containerId) {
      const chartData = aggregateActivities(activities, type, "day");
      const activitiesHeatmap = calendarHeatmap()
                                .data(chartData)
                                .selector(containerId)
                                .colorRange(colorRange)
                                .tooltipEnabled(true)
                                .tooltipUnit([
                                  {min: 0, unit: type.toLowerCase() + "s"},
                                  {min: 1, max: 1, unit: "km"},
                                  {min: 2, max: "Infinity", unit: "kms"}
                                ]);
      activitiesHeatmap();
      jQuery(containerId).css("display", "block");
    }

    function renderBarChart(activities, type, color, containerId) {
      const chartData = aggregateActivities(activities, type, "week");

      const margin = ({top: 10, right: 10, bottom: 30, left: 10});
      const width = 500;
      const height = 100;
      const barWidth = 8;

      const y = d3.scaleLinear()
        .domain([0, d3.max(chartData, function(d) { return d.count; })])
        .nice()
        .range([height - margin.bottom, margin.top]);

      const yAxis = function (g) {
        g.attr("transform", "translate(" + margin.left + ",0)")
          .call(d3.axisLeft(y).ticks(5).tickSizeInner(0))
          .call(function(g) { g.select(".domain").remove(); });
      };

      const x = d3.scaleTime()
        .domain([chartData[0].date,
          moment(chartData[chartData.length - 1].date).add(1, 'w').toDate()])
        .range([margin.left, width - margin.right + barWidth]);

      const xAxis = function(g) {
        g.attr("transform", "translate(0," + (height - margin.bottom) + ")")
          .call(d3.axisBottom(x)
                  .ticks(d3.timeMonth.every(1))
                  .tickFormat(d3.timeFormat("%b"))
                  .tickSizeInner(4)
                  .tickSizeOuter(0));
      };

      const svg = d3.select(containerId)
        .attr("viewBox", [0, 0, width, height]);

      svg.append("g")
        .attr("fill", color)
        .selectAll("rect")
        .data(chartData)
        .join("rect")
          .attr("x", function(d) { return x(d.date); })
          .attr("y", function(d) { return y(d.count); })
          .attr("height", function(d) { return y(0) - y(d.count); })
          .attr("width", barWidth)
          .append("title")
            .text(function(d) {
              return moment(d.date).startOf("week").format("MMM Do") + " - " + d.count + " km";
            });

      svg.append("g").call(xAxis);
      svg.append("g").call(yAxis);

      jQuery(containerId).css("display", "block");
    }

    function renderCalendars() {
      loadStyleSheet("/css/calendar-heatmap.css");

      renderCalendar(window.stravaActivities, "Ride", ["#FFF7F4", "#FF4B08"], "#ride-calendar");
      renderCalendar(window.stravaActivities, "Run", ["#F9F5FF", "#6B42C0"], "#run-calendar");

      jQuery("svg.calendar-heatmap")
        .attr("viewBox", "-15 -15 720 140")
        .css("padding", "1em 0px 0px 0px")
        .removeAttr("width")
        .removeAttr("height");
    }

    function renderBarCharts() {
      renderBarChart(window.stravaActivities, "Ride", "#FF8C61", "#ride-bar-chart");
      renderBarChart(window.stravaActivities, "Run", "#9E85D2", "#run-bar-chart");
    }

    function listenToActivitySelector() {
      const activitySelector = jQuery("#activity-select");
      activitySelector.on("change", function(event) {
        switch(event.target.value) {
          case "all":
            jQuery(".activity").fadeIn();
            activitySelector.get(0).scrollIntoView();
            break;
          case "run":
            jQuery.when(jQuery(".activity.ride").fadeOut(),
                        jQuery(".activity.walk").fadeOut()).done(function() {
              jQuery(".activity.run").fadeIn();
              activitySelector.get(0).scrollIntoView();
            });
            break;
          case "ride":
            jQuery.when(jQuery(".activity.run").fadeOut(),
                        jQuery(".activity.walk").fadeOut()).done(function() {
              jQuery(".activity.ride").fadeIn();
              activitySelector.get(0).scrollIntoView();
            });
            break;
          case "walk":
            jQuery.when(jQuery(".activity.ride").fadeOut(),
                        jQuery(".activity.run").fadeOut()).done(function() {
              jQuery(".activity.walk").fadeIn();
              activitySelector.get(0).scrollIntoView();
            });
            break;
        }
      });
    }

    jQuery(document).ready(function() {
      listenToActivitySelector();
      renderCalendars();
      renderBarCharts();
    });
  }
}());
