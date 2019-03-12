-- This is a library for building projects. The idea is that this code will handle all IO and state 
-- so that each project simply provides a transformation from state to state dependne on event.
module ProjectM (
      Event (..),
      runSite
) where


data Event = RequestJS | RequestInput | ShowProject deriving (read, show)

runSite :: (Read a) => (a -> Event -> a) -> a -> IO ()
runSite update init = 
