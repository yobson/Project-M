# Project M

## Quick Build
Make sure you have autoconf and Qt installed.
You will need to run it to get the configure script:
```bash
autoconf
```
Now we need to configure the app. Here are the important config options:

| Option | Use |
| ------ | --- |
| `--disable-app` | Don't build app |
| `--disbale-server` | Don't build server |
| `--enable-autodep` | Automatically install haskell dependencies |
| `--with-qt-path=<path>` | Path to Qt installation. You will probably want to set this |
| `--with-arch=<arch>` | Set android build architecture. Options are: arm64\_v8a, armv7, x86 |
| `--with-qt-version=<version>` | Set Qt version. If not set, will choose newest version installed |
| `--with-live-address=<web address>` | Sets server address for app. f unset, will default to the test server address |
| `--with-projects-list` | Sets path to list file (for server). Deafults to `/tmp/projectList.txt`
| `ANDROID_NDK_ROOT=<path>` | Specify path to android NDK |
| `ANDROID_SDK_ROOT=<path>` | Specify path to android SDK |
| `JAVA_HOME=<path>` | Specify path to jdk |

To see an example set of argument, the arguments I pass are:
```bash
./configure --with-qt-path=$HOME/Qt --with-arch=x86 ANDROID_NDK_ROOT=/Users/jameshobson/Downloads/android-ndk-r19b/ ANDROID_SDK_ROOT=/Users/jameshobson/Library/Android/sdk/ JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_191.jdk/Contents/Home/ --enable-autodep --with-live-address=www.hobson.space
```
now we simply run
```bash
make
```
This should produce `ProjectM.apk` and `main.cgi`. If you want the primes example, type `make Primes.cgi`

> ### Project M: Put your phone to work
>
> When people browse social media sites on their phones for hours every day, most of the CPU power goes unused. The old desktop equivalent of this problem was the screensaver, which did little of value until it was co-opted for distributed computing projects such as SETI@home. Your task is to make a platform that can perform useful computation in the background on a large number of mobile phones, while the owners are on social media - or even while they are asleep. It will have to run cross-platform, perhaps using JavaScript, but must also give the appropriate incentives to users - will it drain batteries or incur network charges? If so, what kind of application would customers pay to run on such a platform? Would phone sensors offer any specific value? You need to demonstrate an end-to-end solution including servers, mobile clients and an example application, keeping in mind the security implications if either customers or phone owners try to cheat the system.
>
> [Project in collaboration with G-Research]

## Our Approach

Our approach is to build a mobile app (potentially cross platform) that requests and executes tasks and a web server that distributes them. This is a textbook example of the bag of tasks architecture. In this document I will go into detail into the composite parts and how to extend them/write projects to be executed on this platform.

## The Server

The server is written in Haskell (so that projects can be easily written in Haskell) and uses Redis as a backend. You will need a Redis database installed, as well as the `hedis` and `cgi` (I know... old school) cabal modules installed. To build, simply run:

```bash
make
```

And to build with the primes test experiment, run:

```bash
make Primes
```

If you want to test the server, install `lighttpd` , and modify the `testsvr.conf` file to give the absolute path to the root directory (line 1, hopefully will fix in future). To run the server in test mode, simply run

```bash
make test
```

### Writing Projects

The Haskell library is designed so that it is really easy to add new projects. The best way to learn how to write a new project is to look at an example:

```haskell
import ProjectM

type State = (Int, Int, Int) -- Largest, Last Checked, ID of largest

jsMin = "(function(id, number){var n = parseInt(number); for (var i = 2; i <= Math.sqrt(n); i++) {if (n % i == 0){return id + \" \" + 0}} return id + \" \" + n;})" -- JS goes here

updater :: Updater State Int
updater state             RequestJS      = (state, Send jsMin)
updater state@(lg,ch,id)  RequestInput   = ((lg,ch+1,id), Send $ show (ch+1))
updater state@(lg,ch,id) (ReturnAns n i) | i > lg    = ((i,ch,n), Yeet)
                                         | otherwise = (state, Yeet)
updater state@(lg,ch,id)  ShowProject    = (state, Send $ concat ["Largest prime is ",
                                                                   show lg,
                                                                   ", found by user ",
                                                                   show id])

main = runSite "primes" updater (2,2,1000)
```

I'll go through this bit by bit

```haskell
import ProjectM
```

Imports the data types `Event a `, `Result`, `Updater a b` and the function `runSite`.

```haskell
type State = (Int, Int, Int) -- Largest, Last Checked, ID of largest
```

This is the user defined state type. It can be as complicated as you like, but it has to be readable and showable (explanation later). I have chosen for this prime number solver, that the state should be a product type with the largest prime number, the last number checked and the ID of the user that found it.

```haskell
jsMin = "(function(id, number){var n = parseInt(number); for (var i = 2; i <= Math.sqrt(n); i++) {if (n % i == 0){return id + \" \" + 0}} return id + \" \" + n;})" -- JS goes here
```

This line sets a string with the JS code. The JS must be a single, anonymous function with a single input. It must return a specifically formatted string, `(UserID, result)`

```haskell
updater :: Updater State Int
updater state             RequestJS      = (state, Send jsMin)
updater state@(lg,ch,id)  RequestInput   = ((lg,ch+1,id), Send $ show (ch+1))
updater state@(lg,ch,id) (ReturnAns n i) | i > lg    = ((i,ch,n), Yeet)
                                         | otherwise = (state, Yeet)
updater state@(lg,ch,id)  ShowProject    = (state, Send $ concat ["Largest prime is ",
                                                                   show lg,
                                                                   ", found by user ",
                                                                   show id])
```

This is where all the magic happens! The user must define an updater function. This is of type `Updater a b` (which expands to `a -> Event b -> (a, Result)` ) Where `a` is the state and `b` is the type that the JS returns!

The Updater simply takes an event and a state and transforms it into a new state and something to sent back to the phone. The next time the function is run, the new state will be passed to it. In this example I show how our user defined state tuple is transformed to update prime numbers. The event type (and thus, possible events) and return type are defined as: 
```haskell
data Event a = RequestJS | RequestInput | ReturnAns Int a | ShowProject deriving (Read, Show)
data Result = Send String | Yeet deriving (Show)
```

And finally

```haskell
main = runSite "primes" updater (2,2,1000)
```

In this last line, set our main function to host our project. We must first give the `runSite` a unique name (for data base reasons), the updater function and an initial state.

Team 9
