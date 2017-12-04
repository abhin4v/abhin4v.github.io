(function() {
  function showActivitiesCalendar() {
    if (typeof jQuery !== "undefined") {
      function renderCalendar(activities, type, colorRange, containerId) {
        var chartData = [];
        var i = -1;
        var lastDate = null;
        activities.forEach(function(a) {
          if (a.type !== type) {
            return;
          }

          var date = moment(a.date).startOf("day").toDate();
          var count = a.distance;

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

        var activitiesHeatmap = calendarHeatmap()
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

      jQuery(document).ready(function() {
        loadStyleSheet("https://cdn.rawgit.com/DKirwan/calendar-heatmap/7d1b971f/src/calendar-heatmap.css");

        fetch("/files/activities.json")
        .then(function(response) {
          return response.json();
        })
        .then(function(activities) {
          renderCalendar(activities, "Ride", ["#FFF7F4", "#FF4B08"], "#ride-calendar");
          renderCalendar(activities, "Run", ["#F9F5FF", "#6B42C0"], "#run-calendar");

          jQuery("svg.calendar-heatmap")
          .attr("viewBox", "-15 -15 720 140")
          .css("padding", "1em 0px")
          .removeAttr("width")
          .removeAttr("height");
        })
      });
    } else {
      window.setTimeout(showActivitiesCalendar, 1000);
    }
  }

  showActivitiesCalendar();
}());
