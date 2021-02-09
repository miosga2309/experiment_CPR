function fill_value(item_name,val) {
  for (var i=0; i<document.pumpernickel.length; i++) {
    if (document.pumpernickel.elements[i].name == item_name) {
      document.pumpernickel.elements[i].value = val;
    }
  }
}

function get_variable_value(variable) {
  var value = "";
  var input = unescape(location.search.substring(1));
  var searchArray = input.split("&");
  var tempArray = new Array();
  for (i=0; i<searchArray.length; i++) {
    tempArray = searchArray[i].split("=");
    if (tempArray[0] == variable) {
      var value = tempArray[1];
    }
  }
  return value;
}

var inhalt="&";

//js-funktion get_forms
//liest alle form-namen und -werte und �bermittelt sie per get-methode an den server
function get_forms() {
  for (var i=0; i<document.pumpernickel.length; i++) {
    if (document.pumpernickel.elements[i].type == "checkbox") {
      if (document.pumpernickel.elements[i].checked == true) {
        inhalt += escape(document.pumpernickel.elements[i].name) + "=" + escape(document.pumpernickel.elements[i].value) + "&";
      }
    }
    if (document.pumpernickel.elements[i].type == "radio") {
      if (document.pumpernickel.elements[i].checked == true) {
        inhalt += escape(document.pumpernickel.elements[i].name) + "=" + escape(document.pumpernickel.elements[i].value) + "&";
      }
    }
    if (document.pumpernickel.elements[i].type == "select-one") {
      for (var j=0; j<document.pumpernickel.elements[i].length; j++) {
        if (document.pumpernickel.elements[i].options[j].selected == true) {
          inhalt += escape(document.pumpernickel.elements[i].name) + "=" + escape(document.pumpernickel.elements[i].options[j].value) + "&";
        }
      }
    }
    if (document.pumpernickel.elements[i].type == "hidden") {
      inhalt += escape(document.pumpernickel.elements[i].name) + "=" + escape(document.pumpernickel.elements[i].value) + "&";
    }
    if (document.pumpernickel.elements[i].type == "text" || document.pumpernickel.elements[i].type == "textarea") {
      inhalt += escape(document.pumpernickel.elements[i].name) + "=" + escape(document.pumpernickel.elements[i].value) + "&";
    }
  }
  //das letzte ampersand l�schen
  inhalt = inhalt.substring(0,(inhalt.length-1));
  return inhalt; 
}
function all_answered (items) {
  var items_answered = [];
  for (i=0; i<items.length; i++) {
    if (items[i] == '') {
      return true;
    }
    var item = document.getElementsByName(items[i])
    switch (item[0].type) {
      case "check_box":
      case "radio":
        var answered = false;
        for (j=0; j<item.length; j++) {
          if (item[j].checked) {
            answered = true;
          }
        }
        items_answered.push(answered);
        break;
      case "select-one":
        var answered = false;
        for (j=0; j<item[0].options.length; j++) {
          if (item[0].options[j].selected) {
            answered = item[0].options[j].value != "Please choose here...";
          }
        }
        items_answered.push(answered);
        break;
      case "hidden":
      case "text":
      case "text-area":
        var answered = item[0].value != "";
        items_answered.push(answered);
        break;
    }
  }
  var all_items_answered = true;
  for (var i=0; i < items_answered.length; i++) {
    all_items_answered = all_items_answered && items_answered[i]
  };
  return all_items_answered;
}