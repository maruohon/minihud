[![Curseforge](http://cf.way2muchnoise.eu/full_minihud_downloads.svg)](https://minecraft.curseforge.com/projects/minihud) [![Curseforge](http://cf.way2muchnoise.eu/versions/For%20MC_minihud_all.svg)](https://minecraft.curseforge.com/projects/minihud)

## MiniHUD
MiniHUD is a client-side information and overlay rendering mod for Minecraft.

MiniHUD provides a "configurable mini-F3" info line section with configurable info line types, font size and color and line order etc.
The mod also contains various "overlay renderers", such as spawn chunks, slime chunks, random tick range, region file boundaries,
heightmap, structure bounding boxes, and various generic configurable shapes such as spheres, cylinders/circles etc.

MiniHUD is primarily developed on MC 1.12.2 for LiteLoader. It has also been ported to Rift on MC 1.13.2,
and for Fabric on MC 1.14 and later and for Forge on all the recent major MC versions.
(The 1.12.2+ Forge versions are more or less equivalent in features to the following description
and the other mod loader versions. The older Forge versions going back to 1.8 are a lot simpler with less features.)

For compiled builds (= downloads), see:
* CurseForge: http://minecraft.curseforge.com/projects/minihud
* For more up-to-date development builds: https://masa.dy.fi/mcmods/client_mods/
* **Note:** MiniHUD also requires the malilib library mod! But on the other hand Fabric API is not needed.

## Compiling
* Clone the repository
* Open a command prompt/terminal to the repository directory
* On 1.12.x you will first need to run `gradlew setupDecompWorkspace`
  (unless you have already done it once for another project on the same 1.12.x MC version
  and mappings and the same mod loader, Forge or LiteLoader)
* Run `gradlew build` to build the mod
* The built jar file will be inside `build/libs/`

## YourKit
![](https://www.yourkit.com/images/yklogo.png)

We appreciate YourKit for providing the project developers licenses of its profiler to help us improve performance! 

YourKit supports open source projects with innovative and intelligent tools
for monitoring and profiling Java and .NET applications.
YourKit is the creator of [YourKit Java Profiler](https://www.yourkit.com/java/profiler/),
[YourKit .NET Profiler](https://www.yourkit.com/.net/profiler/) and
[YourKit YouMonitor](https://www.yourkit.com/youmonitor), tools for profiling Java and .NET applications.
