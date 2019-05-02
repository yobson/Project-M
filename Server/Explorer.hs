import ProjectM

type State = (Double, Double, Double, Int) -- Lat, Long, Dist, ID

jsMin = "(function(id, e){return id + \" (\" + m_location.lati + \", \" + m_location.longi + \", \" + m_location.distance + \")\" ;})" -- JS goes here

genPage :: State -> String
genPage (lat,long,dist,id) = concat ["<head><meta http-equiv=\"refresh\" content=\"30\"><script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js\"></script>",
  "<link href=\"https://fonts.googleapis.com/css?family=Lato:400,700%7CMerriweather:300,700%7CSource+Code+Pro:400,700\" rel=\"stylesheet\">",
  "<link rel=\"stylesheet\" href=\"https://use.fontawesome.com/releases/v5.3.1/css/all.css\" integrity=\"sha384-mzrmE5qonljUremFsqc01SB46JvROS7bZs3IO2EmfFsd15uHvIt+Y8vEf7N7fWAU\" crossorigin=\"anonymous\" />",
  "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.0/normalize.min.css\" integrity=\"sha256-oSrCnRYXvHG31SBifqP2PM1uje7SJUyX0nTwO2RJV54=\" crossorigin=\"anonymous\" />",
  "<link  rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Fira+Mono:400,700\">",
  "<link rel=\"stylesheet\" href=\"/css/coder.min.85c75a0ebdf10c1ec15300bbf20ec1026e1189031c0d930ce6cdc86eb9a441e9.css\" integrity=\"sha256-hcdaDr3xDB7BUwC78g7BAm4RiQMcDZMM5s3IbrmkQek=\" crossorigin=\"anonymous\" media=\"screen\">",
  "<script>var name = \"\"; jQuery.getJSON(\"/cgi-bin/main.cgi?action=GetUser&id=", show id, "\",{format: \"json\"}).done(function (json) {name = json.firstName + \" \" + json.lastName; $(\"#finder\").html(\"Finder: \" + name)});</script>",
  "</head><body><h2>Our greatest explorer has adventured:", show dist, " meters!</h2>",
  "<h3 id=\"finder\">Explorer: ", show id,
  "</h3>",
  "<p>This page is designed to highlight possible ethical issues with our app. Here it displays the exact location of the student when they ventured further than any other CS student</p>",
  "<iframe width=\"400\" height=\"300\" frameborder=\"0\" scrolling=\"no\" marginheight=\"0\" marginwidth=\"0\"",
  "src=\"https://maps.google.com/maps?q=",show lat,"%2C%20",show long,"&t=&z=13&ie=UTF8&iwloc=&output=embed\"/>",
  "<div style=\"white-space: nowrap; text-align: center; width: 500px; padding: 6px 0;\">",
  "</div>",
  "</div>",
--  "<iframe src=\"https://ox.ac.uk\"></iframe>",
  "</body>"]

permissions = [Location]

updater :: Updater State (Double, Double, Double)
updater state                     RequestJS      = (state, Send jsMin)
updater state@(lat,long,dist,id)  RequestInput   = (state, Send "Empty")
updater state@(lat,long,dist,id) (ReturnAns n (la,lo,di)) | di > dist = ((la,lo,di,n), Yeet)
                                                          | otherwise = (state, Yeet) 
updater state@(lat,long,dist,id)  ShowProject    = (state, Send $ genPage state)
updater state             RequestPermissions = (state, formatPermissions permissions)

main = runSite "explorer" updater (0,0,0,1000)
