# Red Eclipse Server Browser

This package contains library and executable to access current state of [Red Eclipse](http://redeclipse.net)'s game servers.
The name was shamelessly stolen from [Redflare](http://redflare.ofthings.net/).

Note: neither the library nor the executable access original [Redflare](http://redflare.ofthings.net/).

## Usage

```
> redflare --help
Usage: redflare COMMAND
Get current state of Red Eclipse game servers.

Available options:
    -h,--help                Show this help text

Available commands:
    master                   Receive list of connected servers from master server
                             and poll them to get their current state.
    single                   Get current state of a specified server.
```

### Master Command

```
> redflare master --help
Usage: redflare master [HOST] [PORT] [-e|--show-empty] [-f|--show-failed]
Receive list of connected servers from master server and poll them to get
their current state.

Available options:
    -h,--help                Show this help text
    HOST                     Master server's host (default: "play.redeclipse.net")
    PORT                     Master server's port (default: 28800)
    -e,--show-empty          If passed output will include reports from empty
                             servers
    -f,--show-failed         If passed output will include errors for servers that
                             redflare failed to recieve reports from

# Example:
> redflare master
[{"status":"success"
 ,"report":{"serverDesc":"DOOM USA Server (St. Louis)"
           ,"numGameMods":5
           ,"mapName":"Garden5"
           ,"playerNames":["Kal-El"]
           ,"masterMode":"Veto"
           ,"mutators":["FFA","Classic"]
           ,"verInfo":{"versionPatch":3
                      ,"versionArch":64
                      ,"versionPlatform":"LinuxBSD"
                      ,"versionMinor":5
                      ,"versionMajor":1}
           ,"serverClients":16
           ,"version":226
           ,"playerCnt":1
           ,"gameMode":"Edit"
           ,"timeLeft":255
           ,"numGameVars":4267
           ,"handles":["gipsydanger"]
           ,"gameState":"Playing"
           ,"timeRemaining":255}
 ,"host":"173.28.173.201"
 ,"port":28801}]

> redflare master behaservers.zapto.org 28800
[{"status":"success"
 ,"report":{"serverDesc":"Ghosts' Haunt [ghostclanre.tk]"
           ,"numGameMods":0
           ,"mapName":"ghost"
           ,"playerNames":["Django"]
           ,"masterMode":"Open"
           ,"mutators":[]
           ,"serverClients":16
           ,"version":220
           ,"playerCnt":1
           ,"gameMode":"Deathmatch"
           ,"numGameVars":3456
           ,"timeRemaining":312}
 ,"host":"76.179.121.138"
 ,"port":28810}]
```

### Single Command

```
> redflare single --help
Usage: redflare single HOST [PORT]
Get current state of a specified server.

Available options:
    -h,--help                Show this help text
    HOST                     Server's host.
    PORT                     Server's port. (default: 28801)

# Example
> redflare single aceclan.tk 28800
{"status":"success"
,"report":{"serverDesc":"ACE's server"
          ,"numGameMods":3
          ,"mapName":"affluence"
          ,"playerNames":[]
          ,"masterMode":"Open"
          ,"mutators":[]
          ,"verInfo":{"versionPatch":3
                     ,"versionArch":64
                     ,"versionPlatform":"LinuxBSD"
                     ,"versionMinor":5
                     ,"versionMajor":1}
          ,"serverClients":16
          ,"version":226
          ,"playerCnt":0
          ,"gameMode":"Deathmatch"
          ,"timeLeft":0
          ,"numGameVars":4267
          ,"handles":[]
          ,"gameState":"Waiting"
          ,"timeRemaining":600}
,"host":"aceclan.tk"
,"port":28800}
```
