function remove_elements(){
var checked = $(this).context.childNodes[1].checked
	        	var parentDiv = $(this).closest('.span3')[0];
	        	var siblings = parentDiv.childNodes;
        		for(var i = 0; i < siblings.length; i++){
        			var sibling = siblings[i]
        			var thisTag = sibling.tagName;
        			var thisClass = sibling.className;
        			if($.inArray(thisTag, hideTags) != -1 && thisClass != "checkbox" && thisClass != "selectized shiny-bound-input"){
        				if(checked){
        					$(sibling).css("display", "none")
        				}
        				else{
        					if (thisClass != "selectize-control single"){
        						$(sibling).css("display", "inline-block");
        					}
        					else{$(sibling).css("display", "inherit"); }
        				}
        			}
        		}
	        }

function init() {
    var checkboxes = document.getElementsByClassName("checkbox");
    var hideTags = ["LABEL","SELECT","INPUT","DIV"]
    for (var i = 0; i < checkboxes.length; i++) {
	    var checkbox = checkboxes[i];
	    if(checkbox.addEventListener){
	        checkbox.addEventListener("click",
	        function() {
			    var checked = $(this).context.childNodes[1].checked
	        	var parentDiv = $(this).closest('.span3')[0];
	        	var siblings = parentDiv.childNodes;
        		for(var i = 0; i < siblings.length; i++){
        			var sibling = siblings[i]
        			var thisTag = sibling.tagName;
        			var thisClass = sibling.className;
        			if($.inArray(thisTag, hideTags) != -1 && thisClass != "checkbox" && thisClass != "selectized shiny-bound-input"){
        				if(checked){
        					$(sibling).css("display", "none")
        				}
        				else{
        					if (thisClass != "selectize-control single"){
        						$(sibling).css("display", "inline-block");
        					}
        					else{$(sibling).css("display", "inherit"); }
        				}
        			}
        		}
	        },
	        false);
	    } else if(checkbox.attachEvent){
	        checkbox.attachEvent("onclick", 
	        function() {
			    var checked = $(this).context.childNodes[1].checked
	        	var parentDiv = $(this).closest('.span3')[0];
	        	var siblings = parentDiv.childNodes;
        		for(var i = 0; i < siblings.length; i++){
        			var sibling = siblings[i]
        			var thisTag = sibling.tagName;
        			var thisClass = sibling.className;
        			if($.inArray(thisTag, hideTags) != -1 && thisClass != "checkbox" && thisClass != "selectized shiny-bound-input"){
        				if(checked){
        					$(sibling).css("display", "none")
        				}
        				else{
        					if (thisClass != "selectize-control single"){
        						$(sibling).css("display", "inline-block");
        					}
        					else{$(sibling).css("display", "inherit"); }
        				}
        			}
        		}
	        });
	    }
	}


// Start of slider code
    var sliders = $(".jslider-pointer");
    for (var i = 0; i < sliders.length; i++) {
	    var slider = sliders[i];
	    if(slider.addEventListener){
	        slider.addEventListener("click",
	        function() {
                console.log("foo")
			   
	        },
	        false);
	    } else if(slider.attachEvent){
	        slider.attachEvent("onclick", 
	        function() {
			   console.log("bar")
	        });
	    }
	}


};

if(window.addEventListener){
    window.addEventListener("load", init, false);
} else if(window.attachEvent){
    window.attachEvent("onload", init);
} else{
   document.addEventListener("load", init, false);
}
