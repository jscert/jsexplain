
(function () {
  var abr = 1;
  
var bli = 4;
   
  return (function () {
    
      if (true) {
      return  abr;
      } else {
      return  bli;
      }
  })();}())var app = function (x) {
             return x;
           };
           var app2 = function (x) {
             return x;
           };
           
var affiche = function (x) {
  return (function () {
    switch (x.type) {
    case "As": 
               return "As";
    case "Petite": var n = x.petite;
                   return "Petite";
    }
  }())
  ;
};
var pet = {type: "Petite", petite: 5};

var cinq = 5;
