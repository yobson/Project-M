import ProjectM

type State = (Integer, Integer, Int) -- Largest, Last Checked, ID of largest

jsMin = "(function(id, number){var n = parseInt(number); for (var i = 2; i <= Math.sqrt(n); i++) {if (n % i == 0){return id + \" \" + 0}} return id + \" \" + n;})" -- JS goes here

genPage :: Integer -> Int -> String
genPage prime id = concat ["<head><meta http-equiv=\"refresh\" content=\"30\"><script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js\"></script>",
  "<link href=\"https://fonts.googleapis.com/css?family=Lato:400,700%7CMerriweather:300,700%7CSource+Code+Pro:400,700\" rel=\"stylesheet\">",
  "<link rel=\"stylesheet\" href=\"https://use.fontawesome.com/releases/v5.3.1/css/all.css\" integrity=\"sha384-mzrmE5qonljUremFsqc01SB46JvROS7bZs3IO2EmfFsd15uHvIt+Y8vEf7N7fWAU\" crossorigin=\"anonymous\" />",
  "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.0/normalize.min.css\" integrity=\"sha256-oSrCnRYXvHG31SBifqP2PM1uje7SJUyX0nTwO2RJV54=\" crossorigin=\"anonymous\" />",
  "<link  rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Fira+Mono:400,700\">",
  "<link rel=\"stylesheet\" href=\"/css/coder.min.85c75a0ebdf10c1ec15300bbf20ec1026e1189031c0d930ce6cdc86eb9a441e9.css\" integrity=\"sha256-hcdaDr3xDB7BUwC78g7BAm4RiQMcDZMM5s3IbrmkQek=\" crossorigin=\"anonymous\" media=\"screen\">",
  "<script>var name = \"\"; jQuery.getJSON(\"/cgi-bin/main.cgi?action=GetUser&id=", show id, "\",{format: \"json\"}).done(function (json) {name = json.firstName + \" \" + json.lastName; $(\"#finder\").html(\"Finder: \" + name)});</script>",
  "</head><body><h2>Largest prime found is ", show prime, "</h2>",
  "<h3 id=\"finder\">Finder: ", show id,
  "</h3></body>"]

nextOdd :: (Integral i) => i -> i -> i
nextOdd p i | i < p          = p + 2
            | i `mod` 2 == 0 = i + 1
            | otherwise      = i + 2

updater :: Updater State Integer
updater state             RequestJS      = (state, Send jsMin)
updater state@(lg,ch,id)  RequestInput   = ((lg,nextOdd lg ch ,id), Send $ show (nextOdd lg ch))
updater state@(lg,ch,id) (ReturnAns n i) | i > lg    = ((i,ch,n), Yeet)
                                         | otherwise = (state, Yeet) 
updater state@(lg,ch,id)  ShowProject    = (state, Send $ genPage lg id)

main = runSite "primes" updater (2,2,1000)
