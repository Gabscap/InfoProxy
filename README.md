# InfoProxy

InfoProxy is a fake Minecraft Server displaying a user-defined Motd and kicking connecting players with a custom kick message. This can be useful if you're doing a maintenance/regular backup and don't want your server to appear offline.

## Installation
Install [Stack](https://www.haskellstack.org/), then
```
stack build
```

## Config

Default config
```
servers:
- host: 127.0.0.1
  port: 25565
  motd: §cDefault motd
  kickMessage: §cDefault kick message
  maxplayers: 100
  serverIcon: server-icon.png
  protocol: 340
```

* *host* The address to listen on
* *port* The port to listen on
* *motd* Message of the day (displayed in Multiplayer Browser)
* *kickMessage* Message a connecting user gets
* *maxplayer* Max slots (displayed in Multiplayer Browser)
* *serverIcon* path to server icon PNG file (displayed in Multiplayer Browser)
* *protocol* [Version Number](http://wiki.vg/Protocol_version_numbers)
