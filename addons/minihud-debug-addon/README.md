# MiniHUD Debug Addon (v1.0)

MiniHUD Debug Addon is a client-only Fabric add-on that sends vanilla debug subscription requests to the server based on MiniHUD debug toggles. This enables MiniHUD’s debug renderers (brain, pathfinding, neighbor updates, redstone order, structures, etc.) on multiplayer servers that support debug subscriptions.

Minecraft version: 1.21.11

Edits and add-on by lyntermint.

## Requirements
- Minecraft 1.21.11 (client)
- Fabric Loader
- Fabric API
- MiniHUD 0.38.2
- malilib 0.27.5

## Install
- Put this add-on jar in your client `mods` folder.
- Make sure MiniHUD and malilib are also in the client `mods` folder.
- Join a server and toggle the MiniHUD debug options you want to render.

Note: The server requires OP (or equivalent permissions) to accept debug subscriptions.

## Build
- The build uses `libs/minihud-fabric-1.21.11-0.38.2.jar` and `libs/malilib-fabric-1.21.11-0.27.5.jar`.
- Run `./gradlew build`.

## Credits
- MiniHUD by maruohon: [GitHub](https://github.com/maruohon/minihud)
- malilib by maruohon: [GitHub](https://github.com/maruohon/malilib)
