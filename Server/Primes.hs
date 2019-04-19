import ProjectM

type State = (Int, Int, Int) -- Largest, Last Checked, ID of largest

jsMin = "(function(id, number){var n = parseInt(number); for (var i = 2; i <= Math.sqrt(n); i++) {if (n % i == 0){return id + \" \" + 0}} return id + \" \" + n;})" -- JS goes here

genPage :: Int -> Int -> String
genPage prime id = concat ["<head><meta http-equiv=\"refresh\" content=\"5\"><script src=\"https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js\"></script>",
  "<script>var name = \"\"; jQuery.getJSON(\"/cgi-bin/main.cgi?action=GetUser&id=", show id, "\",{format: \"json\"}).done(function (json) {name = json.firstName + \" \" + json.lastName; $(\"#finder\").html(\"Finder: \" + name)});</script>",
  "</head><body><h2>Largest prime found is ", show prime, "</h2>",
  "<h3 id=\"finder\">Finder: ", show id,
  "</h3></body>"]

updater :: Updater State Int
updater state             RequestJS      = (state, Send jsMin)
updater state@(lg,ch,id)  RequestInput   = ((lg,ch+1,id), Send $ show (ch+1))
updater state@(lg,ch,id) (ReturnAns n i) | i > lg    = ((i,ch,n), Yeet)
                                         | otherwise = (state, Yeet) 
updater state@(lg,ch,id)  ShowProject    = (state, Send $ genPage lg id)

main = runSite "primes" updater (2,2,1000)
