var checkExist = setInterval(function() {
// Ugly solution to wait for jslider-pointer to load before listening for cliccks
// Did this instead of monkeying w/ the slider.js which is embedded in the shiny package
// slider.js takes an input, then applies a template to it to build the slider (pointer, scale, etc)
// hence the need for this janky listene (vs document.ready etc)
   if ($(".jslider-pointer").length) {
      var sliders = $(".jslider-pointer")
      clearInterval(checkExist);
        for (var i = 0; i < sliders.length; i++) {
        var slider = sliders[i];
        if(slider.addEventListener){
            slider.addEventListener("click",
            function() {
                var closestInput = $(this).parents('span').siblings('input');
                var inputID = closestInput[0].name
                if(closestInput[0].getAttribute("data-nulldefault") === "TRUE"){
                    $("#"+inputID+"_hidden").attr("value",1);
                    $("#"+inputID+"_hidden").change();
                }
            },
            false);
        } else if(slider.attachEvent){
            //for IE
            slider.attachEvent("onclick", 
            function() {
               console.log("bar")
            });
        }
    }
   }
}, 10)