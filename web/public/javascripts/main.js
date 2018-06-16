'use strict';

window.chartColors = {
	red: 'rgb(255, 99, 132)',
	redlight: 'rgb(255, 129, 162)',
	orange: 'rgb(255, 159, 64)',
	orangelight: 'rgb(255, 189, 94)',
	yellow: 'rgb(255, 205, 86)',
	yellowlight: 'rgb(255, 235, 116)',
	green: 'rgb(75, 192, 192)',
	greenlight: 'rgb(105, 222, 222)',
	blue: 'rgb(54, 162, 235)',
	bluelight: 'rgb(84, 192, 235)',
	purple: 'rgb(153, 102, 255)',
	purplelight: 'rgb(183, 132, 255)',
	grey: 'rgb(201, 203, 207)',
	greylight: 'rgb(231, 233, 237)'
};

$(function() {
    $('#side-menu').metisMenu();
});

//Loads the correct sidebar on window load,
//collapses the sidebar on window resize.
// Sets the min-height of #page-wrapper to window size
$(function() {
    $(window).bind("load resize", function() {
        var topOffset = 50;
        var width = (this.window.innerWidth > 0) ? this.window.innerWidth : this.screen.width;
        if (width < 768) {
            $('div.navbar-collapse').addClass('collapse');
            topOffset = 100; // 2-row-menu
        } else {
            $('div.navbar-collapse').removeClass('collapse');
        }

        var height = ((this.window.innerHeight > 0) ? this.window.innerHeight : this.screen.height) - 1;
        height = height - topOffset*2;
        if (height < 1) height = 1;
        if (height > topOffset) {
            $("#page-wrapper").css("min-height", (height) + "px");
        }
    });

    var url = window.location;
    // var element = $('ul.nav a').filter(function() {
    //     return this.href == url;
    // }).addClass('active').parent().parent().addClass('in').parent();
    var element = $('ul.nav a').filter(function() {
        return this.href == url;
    }).addClass('active').parent();

    while (true) {
        if (element.is('li')) {
            element = element.parent().addClass('in').parent();
        } else {
            break;
        }
    }
});