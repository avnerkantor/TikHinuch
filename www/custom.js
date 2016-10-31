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

//Set countries width the same width as plots width
$(window).on('resize', function(){
  $("#Country1, #Country2, #Country3, #Country4, #SurveyYear, #SurveySubject, #SurveyCategory, #SurveySubCategory, #modelId, #analyzeVariables" ).css('width', ($("#Country1Plot").width()+'px'));
}).resize();

//jQuery for page scrolling feature - requires jQuery Easing plugin
$(document).ready(function () {
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

$(document).on('activate.bs.scrollspy', function(e) {
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
                 //$("#headerBar").show();
                break;
        }
        
  var $hash, $node;
  $hash = $("a[href^='#']", e.target).attr("href").replace(/^#/, '');
  $node = $('#' + $hash);
  if ($node.length) {
    $node.attr('id', '');
  }
  document.location.hash = $hash;
  if ($node.length) {
    return $node.attr('id', $hash);
  }
});

  // $(document).ready(function() {
    //    $('#pisaScoresTable').dataTable( {
      //      "language": {
        //        "url": "dataTables.hebrew.lang"
          //  }
//        } );
//    } );



// This recieves messages from the server.
Shiny.addCustomMessageHandler('updateSelections',
  function(data) {
    window.history.pushState('','', data);
    //alert("url: ", data);                 
});
                   
