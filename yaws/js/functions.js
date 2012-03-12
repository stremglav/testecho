//'{"method": "add_records", "data": [["echo", 1, 1, 1], ["echo", 2, 2, 2], ["apple", 3, 3, 3], ["echo", 4, 4, 4]]}'
function rand_between(min, max) {
    var random = function(x) {
        return Math.floor(x * (Math.random() % 1));
    }

    return min + random(max - min + 1);
}

function gen_add_data(papersId, countId, resultId, importButtonId) {
    var timeSection = 60*60*24*30; //~ One month
    var sep = ','; // Separator for papers
    var costLeftBound = 1;
    var costRightBound = 1000;
    var volumeLeftBound = 1;
    var volumeRightBound = 1000;

    var papers = jQuery('#' + papersId).val().split(sep);
    var count = parseInt(jQuery('#' + countId).val(), 10);

    if (count <= 0) {
        return;
    }

    var curTime = new Date();
    var timeRightBound = Math.round(curTime.getTime() / 1000);
    var timeLeftBound = timeRightBound - timeSection;

    var data = [];
    jQuery.each(papers, function(key, value) {
        var processed = jQuery.trim(value);
        if (processed) {
            var paper = [];
            for(var i = 0; i < count; i++) {
                var time = rand_between(timeLeftBound, timeRightBound);
                var cost = rand_between(costLeftBound, costRightBound);
                var volume = rand_between(volumeLeftBound, volumeRightBound);
                paper.push([processed, time, cost, volume]);
            }
            data = data.concat(paper);
        }
    });

    var jsonText = JSON.stringify({"method": "add_records", "data": data}, null);
    jQuery('#' + resultId).val(jsonText);
    jQuery('#' + importButtonId).removeClass('disabled').removeAttr('disabled');
}

function gen_fetch_data(paperId, scaleId, startTimeId, finishTimeId, resultId, fetchButtonId) {
    var paper = jQuery.trim(jQuery('#' + paperId).val());
    var scale = jQuery('#' + scaleId).val();
    var startTime = new Date(jQuery('#' + startTimeId).val());
    var finishTime = new Date(jQuery('#' + finishTimeId).val());

    startTime = Math.round(startTime.getTime() / 1000);
    finishTime = Math.round(finishTime.getTime() / 1000);

    if (!startTime || !finishTime || finishTime < startTime) {
        alert('Временые границы выборки заполнены не верно!');
        return;
    }

    var jsonText = JSON.stringify({"method": "get_records", "data": [paper, startTime, finishTime, scale]}, null);
    jQuery('#' + resultId).val(jsonText);
    jQuery('#' + fetchButtonId).removeClass('disabled').removeAttr('disabled');
}

function import_data(data_id, notice_id) {
    var data = jQuery('#' + data_id).val();
    var cb = function(d){
        if (!d) {
            return;
        }
        if (typeof(d) == 'object') {
            d = JSON.stringify(d, null, 1);
        }
        jQuery('#' + notice_id).html("<div class='notice'>" + d + "</div>");
    }
    if (!data) {
        cb("Сгенерируйте пожалуйста данные для импорта");
        return;
    }
    send_request(data, cb);
}

function fetch(data_id, notice_id) {
    var data = jQuery('#' + data_id).val();
    var cb = function(d){
        if (!d) {
            return;
        }
        if (typeof(d) == 'object') {
            d = JSON.stringify(d, null, 1);
        }
        data = JSON.parse(d);
        var tableData =
            '<table class="table table-striped table-bordered table-condensed">'+
                '<thead><tr>'+
                    '<th>Время открытия</th>'+
                    '<th>Цена открытия</th>'+
                    '<th>Цена закрытия</th>'+
                    '<th>Минимальная цена</th>'+
                    '<th>Максимальная цена</th>'+
                    '<th>Общий обьем сделок</th>'+
                '</tr></thead>';

        jQuery.each(data, function(key, value) {
            var datetime  = new Date(parseInt(value[0], 10) * 1000);
            var date = jQuery.datepicker.formatDate('dd.mm.yy ', datetime);
            var time = datetime.getHours() + ":" + datetime.getMinutes();
            tableData += 
                '<tr><td>' + date + time +
                '</td><td>' + value[1] +
                '</td><td>' + value[2] +
                '</td><td>' + value[3] +
                '</td><td>' + value[4] +
                '</td><td>' + value[5] +
                '</td></tr>';
        });

        tableData += '</table>';

        jQuery('#' + notice_id).html(tableData);
    }
    if (!data) {
        cb("Сгенерируйте пожалуйста данные для выборки");
        return;
    }
    send_request(data, cb);

}

function send_request(request, cb) {
    jQuery.post(
        '/data.yaws', 
        {'request' : request},
        function(data) {
            cb(data);
    });
}
