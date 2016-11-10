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


$(document).on('change', function() {
   //alert($('input[name="Subject"]:checked').val()); 
  $('input[name="Subject"]:not(:checked)').parent().removeClass("active");
  $('input[name="Subject"]:checked').parent().addClass("active");
  
  $('input[name="worldOrIsrael"]:not(:checked)').parent().removeClass("active");
  $('input[name="worldOrIsrael"]:checked').parent().addClass("active");
  
  //TODO: gender and escs
});


$(function() {
    $('#dashboard').affix({
        offset: { 
          top: $('#dashboard').offset().top-50,
          bottom: $('#dashboard--wrap').height($("#dashboard").height())
        }
    });
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
            scrollTop: $($anchor.attr('href')).offset().top - 325
        }, 1500, 'easeInOutExpo');
    // Prevent default anchor click behavior
    event.preventDefault();
   // Add hash (#) to URL when done scrolling (default click behavior)
      window.location.hash = hash;
    });
});

$(document).ready(function () {
   $("#surveyDashboard").hide();
});





$(document).on('activate.bs.scrollspy', function(e) {
   var x = $(".nav li.active > a").attr('href');
        switch (x) {
          case "#scores":
                $("#surveyDashboard").hide();
                $("#Subject").show();
                break;
            case "#expertise":
                $("#surveyDashboard").hide();
                $("#Subject").show();
                break;
            case "#survey":
                $("#surveyDashboard").show();
                $("#Subject").hide();
                break;
             // case "#analyze":
               // $("#surveyDashboard").show();
              //  $("#Subject").show();
              //  break;
            case "#about":
                $("#surveyDashboard").show();
                 $("#Subject").hide();
                break;
           // case "#mainNav":
             //  $("#surveyDashboard").hide();
               // $("#Subject").show();
                //break;
            default:
                $("#surveyDashboard").hide();
              $("#Subject").show();
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
                   
$(document).ready(function(){
    $('[data-toggle="tooltip"]').tooltip({
        placement : 'top'
    });
});


//$('#survey').on('click', 'li > a', function() {
 //$("#surveyDashboard").show();
   //             $("#Subject").hide();
//});

(function($) {
    $.fn.textfill = function(maxFontSize) {
        maxFontSize = parseInt(maxFontSize, 10);
        return this.each(function(){
            var ourText = $("span", this),
                parent = ourText.parent(),
                maxHeight = parent.height(),
                maxWidth = parent.width(),
                fontSize = parseInt(ourText.css("fontSize"), 10),
                multiplier = maxWidth/ourText.width(),
                newSize = (fontSize*(multiplier-0.1));
            ourText.css(
                "fontSize", 
                (maxFontSize > 0 && newSize > maxFontSize) ? 
                    maxFontSize : 
                    newSize
            );
        });
    };
})(jQuery);