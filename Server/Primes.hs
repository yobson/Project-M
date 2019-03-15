import ProjectM

type State = (Int, Int, Int) -- Largest, Last Checked, ID of largest

jsMin = "testJS" -- JS goes here

updater :: Updater State Int
updater state             RequestJS      = (state, Send jsMin)
updater state@(lg,ch,id)  RequestInput   = ((lg,ch+1,id), Send $ show (ch+1))
updater state@(lg,ch,id) (ReturnAns n i) | i > lg    = ((i,ch,n), Yeet)
                                         | otherwise = (state, Yeet) 
updater state@(lg,ch,id)  ShowProject    = (state, Send $ concat ["Largest prime is ", show lg, ", found by user ", show id])

main = runSite "primes" updater (2,2,1000)
