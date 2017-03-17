let stylesheet = "stylesheets/stylesheet.css"
let img_dir = "images/"
let sources = "src/html"

let main_lang = "fr"

let index = "index.html"

let page_order = [
  index, 1;
  "table.html", 2;
  "zone.html", 3;
  "temoignages.html",4;
  "tarifs.html", 5;
  "contact.html",6
]

let map = "https://umap.openstreetmap.fr/fr/map/zones-dintervention_133043"
let map_options =
  let first = ref true in
  let symb () =
    if !first then (first:= false; "?") else "&"
  in
  List.fold_left (fun s (t,b) ->
                    s^(symb ())^t^"="^b)
    map ["scaleControl","true";
         "miniMap","false";
         "scrollWheelZoom","false";
         "zoomControl","true";
         "allowEdit","false";
         "moreControl","false";
         "searchControl","true";
         "tilelayersControl","null";
         "embedControl","null";
         "datalayersControl","true";
         "onLoadPanel","undefined";
         "captionBar","true"]

(* This construction is intended to avoid spam.*)
let email = "math"^"hainaut@"^"fr"^"ee.fr"
