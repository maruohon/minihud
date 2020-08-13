package fi.dy.masa.minihud.data;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import javax.annotation.Nullable;
import com.google.common.collect.ArrayListMultimap;
import com.mumfrey.liteloader.core.ClientPluginChannels;
import com.mumfrey.liteloader.core.PluginChannels.ChannelPolicy;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
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
import fi.dy.masa.malilib.network.ClientPacketChannelHandler;
import fi.dy.masa.malilib.network.PacketSplitter;
import fi.dy.masa.malilib.util.FileUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.malilib.util.data.Constants;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.mixin.IMixinChunkGeneratorEnd;
import fi.dy.masa.minihud.mixin.IMixinChunkGeneratorFlat;
import fi.dy.masa.minihud.mixin.IMixinChunkGeneratorHell;
import fi.dy.masa.minihud.mixin.IMixinChunkGeneratorOverworld;
import fi.dy.masa.minihud.mixin.IMixinChunkProviderServer;
import fi.dy.masa.minihud.mixin.IMixinMapGenStructure;
import fi.dy.masa.minihud.network.CarpetStructurePacketHandler;
import fi.dy.masa.minihud.util.MiscUtils;
import io.netty.buffer.Unpooled;
import it.unimi.dsi.fastutil.longs.Long2ObjectMap;

public class StructureStorage
{
    public static final int CARPET_ID_BOUNDINGBOX_MARKERS = 3;
    public static final int CARPET_ID_LARGE_BOUNDINGBOX_MARKERS_START = 7;
    public static final int CARPET_ID_LARGE_BOUNDINGBOX_MARKERS = 8;

    private final Minecraft mc = Minecraft.getMinecraft();
    private final ArrayListMultimap<StructureType, StructureData> structureMap = ArrayListMultimap.create();
    @Nullable private BlockPos lastStructureUpdatePos;
    private boolean hasStructureDataFromServer;
    private boolean structuresDirty;
    private boolean structuresNeedUpdating;

    public void clear()
    {
        this.structuresNeedUpdating = true;
        this.hasStructureDataFromServer = false;
        this.structuresDirty = false;

        this.lastStructureUpdatePos = null;
        this.structureMap.clear();
    }

    public boolean hasStructureDataChanged()
    {
        return this.structuresDirty;
    }

    public void setStructuresNeedUpdating()
    {
        this.structuresNeedUpdating = true;
    }

    public void setStructuresDirty()
    {
        this.structuresDirty = true;
    }

    @Nullable
    private File getLocalStructureFileDirectory()
    {
        String dirName = StringUtils.getWorldOrServerName();

        if (dirName != null)
        {
            File dir = new File(new File(FileUtils.getConfigDirectory(), Reference.MOD_ID), "structures");
            return new File(dir, dirName);
        }

        return null;
    }

    /**
     * Gets a copy of the structure data map, and clears the dirty flag
     * @return
     */
    public ArrayListMultimap<StructureType, StructureData> getCopyOfStructureData()
    {
        ArrayListMultimap<StructureType, StructureData> copy = ArrayListMultimap.create();

        synchronized (this.structureMap)
        {
            for (StructureType type : StructureType.values())
            {
                Collection<StructureData> values = this.structureMap.get(type);

                if (values.isEmpty() == false)
                {
                    copy.putAll(type, values);
                }
            }

            this.structuresDirty = false;
        }

        return copy;
    }

    public void updateStructureDataIfNeeded()
    {
        if (this.mc.world != null && this.mc.player != null)
        {
            final BlockPos playerPos = new BlockPos(this.mc.player);

            if (this.mc.isSingleplayer())
            {
                if (this.structuresNeedUpdating(playerPos, 32))
                {
                    this.updateStructureDataFromIntegratedServer(playerPos);
                }
            }
            else if (this.structuresNeedUpdating(playerPos, 128))
            {
                if (this.hasStructureDataFromServer == false)
                {
                    this.updateStructureDataFromNBTFiles(playerPos);
                }
                else
                {
                    this.requestStructureDataUpdates();
                }
            }
        }
    }

    public void requestStructureDataUpdates()
    {
        Minecraft mc = Minecraft.getMinecraft();

        if (mc.world != null)
        {
            boolean enabled = RendererToggle.OVERLAY_STRUCTURE_MAIN_TOGGLE.isRendererEnabled();

            if (mc.isSingleplayer() == false)
            {
                if (enabled)
                {
                    ClientPacketChannelHandler.INSTANCE.registerClientChannelHandler(CarpetStructurePacketHandler.INSTANCE);

                    // Request the data using both the old and the new protocol/channel name
                    PacketBuffer data = new PacketBuffer(Unpooled.buffer());
                    data.writeInt(CARPET_ID_BOUNDINGBOX_MARKERS);
                    ClientPluginChannels.sendMessage(LiteModMiniHud.CHANNEL_CARPET_CLIENT_OLD, data, ChannelPolicy.DISPATCH_ALWAYS);

                    data = new PacketBuffer(Unpooled.buffer());
                    data.writeInt(CARPET_ID_BOUNDINGBOX_MARKERS);
                    PacketSplitter.send(mc.getConnection(), new ResourceLocation(LiteModMiniHud.CHANNEL_CARPET_CLIENT_NEW), data);

                    LiteModMiniHud.logger.info("Requesting structure data from Carpet server");
                }
                else
                {
                    ClientPacketChannelHandler.INSTANCE.unregisterClientChannelHandler(CarpetStructurePacketHandler.INSTANCE);
                }
            }
            else if (enabled)
            {
                this.setStructuresNeedUpdating();
            }

            this.setStructuresDirty();
        }
    }

    private boolean structuresNeedUpdating(BlockPos playerPos, int hysteresis)
    {
        return this.structuresNeedUpdating || this.lastStructureUpdatePos == null ||
                Math.abs(playerPos.getX() - this.lastStructureUpdatePos.getX()) >= hysteresis ||
                Math.abs(playerPos.getY() - this.lastStructureUpdatePos.getY()) >= hysteresis ||
                Math.abs(playerPos.getZ() - this.lastStructureUpdatePos.getZ()) >= hysteresis;
    }

    private void updateStructureDataFromIntegratedServer(final BlockPos playerPos)
    {
        final int dimension = this.mc.player.dimension;
        final WorldServer world = this.mc.getIntegratedServer().getWorld(dimension);

        synchronized (this.structureMap)
        {
            this.structureMap.clear();
        }

        if (world != null)
        {
            final IChunkGenerator chunkGenerator = ((IMixinChunkProviderServer) world.getChunkProvider()).getChunkGenerator();
            final int maxRange = (this.mc.gameSettings.renderDistanceChunks + 4) * 16;

            world.addScheduledTask(() -> this.addStructureDataFromGenerator(chunkGenerator, playerPos, maxRange));
        }

        this.lastStructureUpdatePos = playerPos;
        this.structuresNeedUpdating = false;
    }

    private void updateStructureDataFromNBTFiles(final BlockPos playerPos)
    {
        synchronized (this.structureMap)
        {
            this.structureMap.clear();

            File dir = this.getLocalStructureFileDirectory();

            if (dir != null && dir.exists() && dir.isDirectory())
            {
                for (StructureType type : StructureType.values())
                {
                    if (type.isTemple() == false)
                    {
                        NBTTagCompound nbt = FileUtils.readNBTFile(new File(dir, type.getStructureName() + ".dat"));

                        if (nbt != null)
                        {
                            StructureData.readAndAddStructuresToMap(this.structureMap, nbt, type);
                        }
                    }
                }

                NBTTagCompound nbt = FileUtils.readNBTFile(new File(dir, "Temple.dat"));

                if (nbt != null)
                {
                    StructureData.readAndAddTemplesToMap(this.structureMap, nbt);
                }

                if (this.structureMap.size() > 0)
                {
                    this.lastStructureUpdatePos = playerPos;
                    this.structuresDirty = true;
                    this.structuresNeedUpdating = false;
                }

                LiteModMiniHud.logger.info("Structure data updated from local structure files, structures: {}", this.structureMap.size());
            }
        }
    }

    public void updateStructureDataFromCarpetServer(PacketBuffer data)
    {
        try
        {
            data.readerIndex(0);

            if (data.readerIndex() < data.writerIndex() - 4)
            {
                int type = data.readInt();

                if (type == CARPET_ID_BOUNDINGBOX_MARKERS)
                {
                    this.readStructureDataCarpetAll(data.readCompoundTag());
                }
                else if (type == CARPET_ID_LARGE_BOUNDINGBOX_MARKERS_START)
                {
                    NBTTagCompound nbt = data.readCompoundTag();
                    int boxCount = data.readVarInt();
                    this.readStructureDataCarpetSplitHeader(nbt, boxCount);
                }
                else if (type == CARPET_ID_LARGE_BOUNDINGBOX_MARKERS)
                {
                    int boxCount = data.readByte();
                    this.readStructureDataCarpetSplitBoxes(data, boxCount);
                }
            }

            data.readerIndex(0);
        }
        catch (Exception e)
        {
            LiteModMiniHud.logger.warn("Failed to read structure data from Carpet mod packet", e);
        }
    }

    private void readStructureDataCarpetAll(NBTTagCompound nbt)
    {
        NBTTagList tagList = nbt.getTagList("Boxes", Constants.NBT.TAG_LIST);
        DataStorage.getInstance().setWorldSeed(nbt.getLong("Seed"));

        synchronized (this.structureMap)
        {
            this.structureMap.clear();
            StructureData.readStructureDataCarpetAllBoxes(this.structureMap, tagList);
            this.hasStructureDataFromServer = true;
            this.structuresDirty = true;
            this.structuresNeedUpdating = false;

            EntityPlayer player = Minecraft.getMinecraft().player;

            if (player != null)
            {
                this.lastStructureUpdatePos = new BlockPos(player);
            }

            LiteModMiniHud.logger.info("Structure data updated from Carpet server (all), structures: {}", this.structureMap.size());
        }
    }

    private void readStructureDataCarpetSplitHeader(NBTTagCompound nbt, int boxCount)
    {
        DataStorage.getInstance().setWorldSeed(nbt.getLong("Seed"));

        synchronized (this.structureMap)
        {
            this.structureMap.clear();
            StructureData.readStructureDataCarpetIndividualBoxesHeader(boxCount);
        }

        LiteModMiniHud.logger.info("Structure data header received from Carpet server, expecting {} boxes", boxCount);
    }

    private void readStructureDataCarpetSplitBoxes(PacketBuffer data, int boxCount) throws IOException
    {
        synchronized (this.structureMap)
        {
            for (int i = 0; i < boxCount; ++i)
            {
                NBTTagCompound nbt = data.readCompoundTag();
                StructureData.readStructureDataCarpetIndividualBoxes(this.structureMap, nbt);
            }

            this.hasStructureDataFromServer = true;
            this.structuresDirty = true;
            this.structuresNeedUpdating = false;

            EntityPlayer player = Minecraft.getMinecraft().player;

            if (player != null)
            {
                this.lastStructureUpdatePos = new BlockPos(player);
            }

            LiteModMiniHud.logger.info("Structure data received from Carpet server (split boxes), received {} boxes", boxCount);
        }
    }

    private void addStructureDataFromGenerator(IChunkGenerator chunkGenerator, BlockPos playerPos, int maxRange)
    {
        if (chunkGenerator instanceof ChunkGeneratorOverworld)
        {
            MapGenStructure mapGen;

            mapGen = ((IMixinChunkGeneratorOverworld) chunkGenerator).getOceanMonumentGenerator();
            this.addStructuresWithinRange(StructureType.OCEAN_MONUMENT, mapGen, playerPos, maxRange);

            mapGen = ((IMixinChunkGeneratorOverworld) chunkGenerator).getScatteredFeatureGenerator();
            this.addTempleStructuresWithinRange(mapGen, playerPos, maxRange);

            mapGen = ((IMixinChunkGeneratorOverworld) chunkGenerator).getStrongholdGenerator();
            this.addStructuresWithinRange(StructureType.STRONGHOLD, mapGen, playerPos, maxRange);

            mapGen = ((IMixinChunkGeneratorOverworld) chunkGenerator).getVillageGenerator();
            this.addStructuresWithinRange(StructureType.VILLAGE, mapGen, playerPos, maxRange);

            mapGen = ((IMixinChunkGeneratorOverworld) chunkGenerator).getWoodlandMansionGenerator();
            this.addStructuresWithinRange(StructureType.MANSION, mapGen, playerPos, maxRange);
        }
        else if (chunkGenerator instanceof ChunkGeneratorHell)
        {
            MapGenStructure mapGen = ((IMixinChunkGeneratorHell) chunkGenerator).getFortressGenerator();
            this.addStructuresWithinRange(StructureType.NETHER_FORTRESS, mapGen, playerPos, maxRange);
        }
        else if (chunkGenerator instanceof ChunkGeneratorEnd)
        {
            MapGenStructure mapGen = ((IMixinChunkGeneratorEnd) chunkGenerator).getEndCityGenerator();
            this.addStructuresWithinRange(StructureType.END_CITY, mapGen, playerPos, maxRange);
        }
        else if (chunkGenerator instanceof ChunkGeneratorFlat)
        {
            Map<String, MapGenStructure> map = ((IMixinChunkGeneratorFlat) chunkGenerator).getStructureGenerators();

            for (MapGenStructure mapGen : map.values())
            {
                if (mapGen instanceof StructureOceanMonument)
                {
                    this.addStructuresWithinRange(StructureType.OCEAN_MONUMENT, mapGen, playerPos, maxRange);
                }
                else if (mapGen instanceof MapGenScatteredFeature)
                {
                    this.addTempleStructuresWithinRange(mapGen, playerPos, maxRange);
                }
                else if (mapGen instanceof MapGenStronghold)
                {
                    this.addStructuresWithinRange(StructureType.STRONGHOLD, mapGen, playerPos, maxRange);
                }
                else if (mapGen instanceof MapGenVillage)
                {
                    this.addStructuresWithinRange(StructureType.VILLAGE, mapGen, playerPos, maxRange);
                }
            }
        }

        this.structuresDirty = true;
        LiteModMiniHud.logger.info("Structure data updated from the integrated server");
    }

    private void addStructuresWithinRange(StructureType type, MapGenStructure mapGen, BlockPos playerPos, int maxRange)
    {
        Long2ObjectMap<StructureStart> structureMap = ((IMixinMapGenStructure) mapGen).getStructureMap();

        for (StructureStart start : structureMap.values())
        {
            if (MiscUtils.isStructureWithinRange(start.getBoundingBox(), playerPos, maxRange))
            {
                this.structureMap.put(type, StructureData.fromStructure(start));
            }
        }
    }

    private void addTempleStructuresWithinRange(MapGenStructure mapGen, BlockPos playerPos, int maxRange)
    {
        Long2ObjectMap<StructureStart> structureMap = ((IMixinMapGenStructure) mapGen).getStructureMap();

        for (StructureStart start : structureMap.values())
        {
            List<StructureComponent> components = start.getComponents();

            if (components.size() == 1 && MiscUtils.isStructureWithinRange(start.getBoundingBox(), playerPos, maxRange))
            {
                String id = MapGenStructureIO.getStructureComponentName(components.get(0));
                StructureType type = StructureType.templeTypeFromComponentId(id);

                if (type != null)
                {
                    this.structureMap.put(type, StructureData.fromStructure(start));
                }
            }
        }
    }
}
