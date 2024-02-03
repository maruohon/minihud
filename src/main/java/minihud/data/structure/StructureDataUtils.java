package minihud.data.structure;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import javax.annotation.Nullable;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import it.unimi.dsi.fastutil.longs.Long2ObjectMap;

import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.world.WorldServer;
import net.minecraft.world.gen.ChunkGeneratorEnd;
import net.minecraft.world.gen.ChunkGeneratorFlat;
import net.minecraft.world.gen.ChunkGeneratorHell;
import net.minecraft.world.gen.ChunkGeneratorOverworld;
import net.minecraft.world.gen.IChunkGenerator;
import net.minecraft.world.gen.structure.MapGenScatteredFeature;
import net.minecraft.world.gen.structure.MapGenStronghold;
import net.minecraft.world.gen.structure.MapGenStructure;
import net.minecraft.world.gen.structure.MapGenStructureIO;
import net.minecraft.world.gen.structure.MapGenVillage;
import net.minecraft.world.gen.structure.StructureComponent;
import net.minecraft.world.gen.structure.StructureOceanMonument;
import net.minecraft.world.gen.structure.StructureStart;

import malilib.config.util.ConfigUtils;
import malilib.registry.Registry;
import malilib.util.StringUtils;
import malilib.util.data.Constants;
import malilib.util.game.wrap.GameUtils;
import malilib.util.game.wrap.NbtWrap;
import malilib.util.nbt.NbtUtils;
import malilib.util.position.BlockPos;
import malilib.util.position.IntBoundingBox;
import minihud.MiniHud;
import minihud.Reference;
import minihud.config.RendererToggle;
import minihud.mixin.structure.ChunkGeneratorEndMixin;
import minihud.mixin.structure.ChunkGeneratorFlatMixin;
import minihud.mixin.structure.ChunkGeneratorHellMixin;
import minihud.mixin.structure.ChunkGeneratorOverworldMixin;
import minihud.mixin.structure.ChunkProviderServerMixin;
import minihud.mixin.structure.MapGenStructureMixin;
import minihud.network.carpet.CarpetStructurePacketHandler;
import minihud.network.servux.ServuxStructurePacketHandler;
import minihud.util.MiscUtils;

public class StructureDataUtils
{
    public static void requestStructureDataUpdates()
    {
        if (GameUtils.getClientWorld() != null)
        {
            boolean enabled = RendererToggle.STRUCTURE_BOUNDING_BOXES.isRendererEnabled();

            if (GameUtils.isSinglePlayer() == false)
            {
                if (enabled)
                {
                    MiniHud.debugLog("Attempting to register structure packet handlers to the server");

                    // TODO on Servux servers this re-register shouldn't be done when the player just moves around.
                    // But on 1.12.x Carpet server I think a re-request is needed, it doesn't seem like the server
                    // would auto-send structures when the player moves around?
                    Registry.CLIENT_PACKET_CHANNEL_HANDLER.registerClientChannelHandler(CarpetStructurePacketHandler.INSTANCE);
                    Registry.CLIENT_PACKET_CHANNEL_HANDLER.registerClientChannelHandler(ServuxStructurePacketHandler.INSTANCE);
                    CarpetStructurePacketHandler.INSTANCE.sendBoundingBoxRequest();
                }
                else
                {
                    Registry.CLIENT_PACKET_CHANNEL_HANDLER.unregisterClientChannelHandler(CarpetStructurePacketHandler.INSTANCE);
                    Registry.CLIENT_PACKET_CHANNEL_HANDLER.unregisterClientChannelHandler(ServuxStructurePacketHandler.INSTANCE);
                }
            }
            else if (enabled)
            {
                StructureStorage.INSTANCE.setStructuresNeedUpdating();
            }
        }
    }

    public static class IntegratedServer
    {
        public static void updateStructureDataFromIntegratedServer(BlockPos playerPos)
        {
            WorldServer world = GameUtils.getClientPlayersServerWorld();

            if (world != null)
            {
                int maxRange = (GameUtils.getRenderDistanceChunks() + 4) * 16;
                world.addScheduledTask(() -> addStructureDataFromGenerator(world, playerPos, maxRange));
            }
        }

        private static void addStructureDataFromGenerator(WorldServer world, BlockPos playerPos, int maxRange)
        {
            IChunkGenerator chunkGenerator = ((ChunkProviderServerMixin) world.getChunkProvider()).minihud_getChunkGenerator();
            ArrayListMultimap<StructureType, StructureData> mapOut = ArrayListMultimap.create();

            if (chunkGenerator instanceof ChunkGeneratorOverworld)
            {
                MapGenStructure mapGen;

                mapGen = ((ChunkGeneratorOverworldMixin) chunkGenerator).minihud_getOceanMonumentGenerator();
                addStructuresWithinRange(StructureType.OCEAN_MONUMENT, mapOut, mapGen, playerPos, maxRange);

                mapGen = ((ChunkGeneratorOverworldMixin) chunkGenerator).minihud_getScatteredFeatureGenerator();
                addTempleStructuresWithinRange(mapOut, mapGen, playerPos, maxRange);

                mapGen = ((ChunkGeneratorOverworldMixin) chunkGenerator).minihud_getStrongholdGenerator();
                addStructuresWithinRange(StructureType.STRONGHOLD, mapOut, mapGen, playerPos, maxRange);

                mapGen = ((ChunkGeneratorOverworldMixin) chunkGenerator).minihud_getVillageGenerator();
                addStructuresWithinRange(StructureType.VILLAGE, mapOut, mapGen, playerPos, maxRange);

                mapGen = ((ChunkGeneratorOverworldMixin) chunkGenerator).minihud_getWoodlandMansionGenerator();
                addStructuresWithinRange(StructureType.MANSION, mapOut, mapGen, playerPos, maxRange);
            }
            else if (chunkGenerator instanceof ChunkGeneratorHell)
            {
                MapGenStructure mapGen = ((ChunkGeneratorHellMixin) chunkGenerator).minihud_getFortressGenerator();
                addStructuresWithinRange(StructureType.NETHER_FORTRESS, mapOut, mapGen, playerPos, maxRange);
            }
            else if (chunkGenerator instanceof ChunkGeneratorEnd)
            {
                MapGenStructure mapGen = ((ChunkGeneratorEndMixin) chunkGenerator).minihud_getEndCityGenerator();
                addStructuresWithinRange(StructureType.END_CITY, mapOut, mapGen, playerPos, maxRange);
            }
            else if (chunkGenerator instanceof ChunkGeneratorFlat)
            {
                Map<String, MapGenStructure> map = ((ChunkGeneratorFlatMixin) chunkGenerator).minihud_getStructureGenerators();

                for (MapGenStructure mapGen : map.values())
                {
                    if (mapGen instanceof StructureOceanMonument)
                    {
                        addStructuresWithinRange(StructureType.OCEAN_MONUMENT, mapOut, mapGen, playerPos, maxRange);
                    }
                    else if (mapGen instanceof MapGenScatteredFeature)
                    {
                        addTempleStructuresWithinRange(mapOut, mapGen, playerPos, maxRange);
                    }
                    else if (mapGen instanceof MapGenStronghold)
                    {
                        addStructuresWithinRange(StructureType.STRONGHOLD, mapOut, mapGen, playerPos, maxRange);
                    }
                    else if (mapGen instanceof MapGenVillage)
                    {
                        addStructuresWithinRange(StructureType.VILLAGE, mapOut, mapGen, playerPos, maxRange);
                    }
                }
            }

            GameUtils.scheduleToClientThread(() -> StructureStorage.INSTANCE.addStructureDataFromIntegratedServer(mapOut));
        }

        private static void addStructuresWithinRange(StructureType type,
                                                     ArrayListMultimap<StructureType, StructureData> mapOut,
                                                     MapGenStructure mapGen,
                                                     BlockPos playerPos,
                                                     int maxRange)
        {
            if (type.isEnabled() == false)
            {
                return;
            }

            Long2ObjectMap<StructureStart> structureMap = ((MapGenStructureMixin) mapGen).minihud_getStructureMap();

            for (StructureStart start : structureMap.values())
            {
                if (MiscUtils.isStructureWithinRange(start.getBoundingBox(), playerPos, maxRange))
                {
                    mapOut.put(type, StructureData.fromStructure(start));
                }
            }
        }

        private static void addTempleStructuresWithinRange(ArrayListMultimap<StructureType, StructureData> mapOut,
                                                           MapGenStructure mapGen,
                                                           BlockPos playerPos,
                                                           int maxRange)
        {
            Long2ObjectMap<StructureStart> structureMap = ((MapGenStructureMixin) mapGen).minihud_getStructureMap();

            for (StructureStart start : structureMap.values())
            {
                List<StructureComponent> components = start.getComponents();

                if (components.size() == 1 &&
                    MiscUtils.isStructureWithinRange(start.getBoundingBox(), playerPos, maxRange))
                {
                    String id = MapGenStructureIO.getStructureComponentName(components.get(0));
                    StructureType type = StructureType.fromStructureId("Temple." + id);

                    if (type.isEnabled())
                    {
                        mapOut.put(type, StructureData.fromStructure(start));
                    }
                }
            }
        }
    }

    public static class StructureFileUtils
    {
        @Nullable
        public static Path getLocalStructureFileDirectory()
        {
            String dirName = StringUtils.getWorldOrServerName();

            if (dirName != null)
            {
                return ConfigUtils.getConfigDirectory().resolve(Reference.MOD_ID)
                               .resolve("structures").resolve(dirName);
            }

            return null;
        }

        public static ArrayListMultimap<StructureType, StructureData> getEnabledStructuresFromNbtFiles()
        {
            ArrayListMultimap<StructureType, StructureData> map = ArrayListMultimap.create();
            Path dir = getLocalStructureFileDirectory();

            if (dir != null && Files.isDirectory(dir))
            {
                for (StructureType type : StructureType.VALUES)
                {
                    if (type.isEnabled() && type.isTemple() == false)
                    {
                        Path file = dir.resolve(type.getStructureName() + ".dat");
                        NBTTagCompound nbt = NbtUtils.readNbtFromFile(file);

                        if (nbt != null)
                        {
                            readAndAddStructuresToMap(type, map, nbt);
                        }
                    }
                }

                NBTTagCompound nbt = NbtUtils.readNbtFromFile(dir.resolve("Temple.dat"));

                if (nbt != null)
                {
                    readAndAddTemplesToMap(map, nbt);
                }
            }

            return map;
        }

        /**
         * Reads structures from the vanilla 1.12 and below structure files,
         * and adds any structures of the provided StructureType <b>type</b> to the provided map.
         */
        private static void readAndAddStructuresToMap(StructureType type,
                                                      ArrayListMultimap<StructureType, StructureData> map,
                                                      NBTTagCompound rootCompound)
        {
            if (NbtWrap.containsCompound(rootCompound, "data") == false)
            {
                return;
            }

            rootCompound = NbtWrap.getCompound(rootCompound, "data");

            if (NbtWrap.containsCompound(rootCompound, "Features") == false)
            {
                return;
            }

            rootCompound = NbtWrap.getCompound(rootCompound, "Features");

            for (String key : NbtWrap.getKeys(rootCompound))
            {
                NBTBase nbtBase = NbtWrap.getTag(rootCompound, key);

                if (NbtWrap.getTypeId(nbtBase) != Constants.NBT.TAG_COMPOUND)
                {
                    continue;
                }

                NBTTagCompound tag = (NBTTagCompound) nbtBase;
                String id = NbtWrap.getString(tag, "id");

                if (type.getStructureName().equals(id))
                {
                    StructureData data = StructureData.fromTag(tag);

                    if (data != null)
                    {
                        map.put(type, data);
                    }
                }
            }
        }

        /**
         * Reads Temple structures from the vanilla 1.12 and below structure files,
         * and adds them to the provided map. The structure type is read from the child component. 
         */
        public static void readAndAddTemplesToMap(ArrayListMultimap<StructureType, StructureData> map, NBTTagCompound rootTag)
        {
            if (NbtWrap.containsCompound(rootTag, "data") == false)
            {
                return;
            }

            NBTTagCompound dataTag = NbtWrap.getCompound(rootTag, "data");

            if (NbtWrap.containsCompound(dataTag, "Features") == false)
            {
                return;
            }

            NBTTagCompound featureTag = NbtWrap.getCompound(dataTag, "Features");

            for (String key : NbtWrap.getKeys(featureTag))
            {
                NBTBase nbtBase = NbtWrap.getTag(featureTag, key);

                if (NbtWrap.getTypeId(nbtBase) != Constants.NBT.TAG_COMPOUND)
                {
                    continue;
                }

                NBTTagCompound tag = (NBTTagCompound) nbtBase;

                if (NbtWrap.contains(tag, "ChunkX", Constants.NBT.TAG_ANY_NUMERIC) &&
                    NbtWrap.contains(tag, "ChunkZ", Constants.NBT.TAG_ANY_NUMERIC) &&
                    NbtWrap.containsIntArray(tag, "BB") &&
                    NbtWrap.getString(tag, "id").equals("Temple"))
                {
                    NBTTagList tagList = NbtWrap.getListOfCompounds(tag, "Children");

                    if (NbtWrap.getListSize(tagList) != 1)
                    {
                        continue;
                    }

                    NBTTagCompound componentTag = NbtWrap.getCompoundAt(tagList, 0);

                    if (NbtWrap.containsString(componentTag, "id") &&
                        NbtWrap.containsIntArray(componentTag, "BB"))
                    {
                        String id = NbtWrap.getString(componentTag, "id");
                        StructureType type = StructureType.fromStructureId("Temple." + id);

                        if (type.isEnabled())
                        {
                            IntBoundingBox bb = IntBoundingBox.fromArray(NbtWrap.getIntArray(componentTag, "BB"));
                            map.put(type, new StructureData(IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "BB")), ImmutableList.of(bb)));
                        }
                    }
                }
            }
        }
    }
}
