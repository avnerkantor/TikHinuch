//Buttons Logic

$(document).ready(function () {
    $("#generalBtn").addClass('active');
    $("#lowBtn, #mediumBtn, #highBtn").addClass('general');
});

$(document).on('click', '#generalBtn', function (e) {
  //, #lowBtn, #mediumBtn, #highBtn"
    $("#maleBtn, #femaleBtn").removeClass('active');
    $("#lowBtn, #mediumBtn, #highBtn").addClass('general').removeClass('male').removeClass('female');
});
//, #lowBtn, #mediumBtn, #highBtn
$(document).on('click', '#maleBtn, #femaleBtn', function (e) {
   $("#generalBtn").removeClass('active');
});

$(document).on('click', '#maleBtn', function (e) {
    $("#lowBtn, #mediumBtn, #highBtn").toggleClass('male');
});

$(document).on('click', '#femaleBtn', function (e) {
    $("#lowBtn, #mediumBtn, #highBtn").toggleClass('female');
});

//Sections Logic
$(function () {
    $("#surveyDashboard").hide();
    $("#subjectsDashboard").show();
    //$("#headerBar").hide();
    $("#page-top").scrollspy({target: "#mainNav"});
    $("#mainNav").on("activate.bs.scrollspy", function () {
        var x = $(".nav li.active > a").attr('href');
        switch (x) {
          case "#scores":
                $("#surveyDashboard").hide();
                $("#subjectsDashboard").show();
                $("#headerBar").show();
                break;
            case "#expertise":
                $("#surveyDashboard").hide();
                $("#subjectsDashboard").show();
                $("#headerBar").hide();
                break;
            case "#survey":
                $("#surveyDashboard").show();
                $("#subjectsDashboard").hide();
                $("#headerBar").hide();
                break;
              case "#analyze":
                $("#surveyDashboard").show();
                $("#subjectsDashboard").show();
                $("#headerBar").hide();
                break;
            case "#about":
               $("#headerBar").hide();
                $("#surveyDashboard").show();
                 $("#subjectsDashboard").show();
                break;
            case "#mainNav":
               $("#surveyDashboard").hide();
                $("#subjectsDashboard").show();
                $("#headerBar").show();
                break;
            default:
                $("#surveyDashboard").hide();
              $("#subjectsDashboard").show();
                 $("#headerBar").show();
                break;
        }
    });
});

//Set countries width the same width as plots width
$(document).ready(function () {
  $("#Country1, #Country2, #Country3, #Country4, #SurveyYear, #SurveySubject, #SurveyCategory, #SurveySubCategory, #modelId, #analyzeVariables" ).css('width', ($("#Country1Plot").width()+'px'));
});

//jQuery to collapse the navbar on scroll
//$(window).scroll(function() {
//    if ($(".navbar").offset().top > 50) {
//        $(".navbar-fixed-top").addClass("top-nav-collapse");
//    } else {
//        $(".navbar-fixed-top").removeClass("top-nav-collapse");
//    }
//});

//jQuery for page scrolling feature - requires jQuery Easing plugin
$(function() {
    $('a.page-scroll').bind('click', function(event) {
    // Store hash
    var hash = this.hash;
    
    var $anchor = $(this);
        $('html, body').stop().animate({
            scrollTop: $($anchor.attr('href')).offset().top
        }, 1500, 'easeInOutExpo');
    // Prevent default anchor click behavior
    event.preventDefault();
   // Add hash (#) to URL when done scrolling (default click behavior)
      window.location.hash = hash;
    });
});

  // $(document).ready(function() {
    //    $('#pisaScoresTable').dataTable( {
      //      "language": {
        //        "url": "dataTables.hebrew.lang"
          //  }
//        } );
//    } );

