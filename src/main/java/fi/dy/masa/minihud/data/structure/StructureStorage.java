package fi.dy.masa.minihud.data.structure;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import javax.annotation.Nullable;
import com.google.common.collect.ArrayListMultimap;
import it.unimi.dsi.fastutil.longs.Long2ObjectMap;
import net.minecraft.entity.Entity;
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
import fi.dy.masa.malilib.util.game.wrap.EntityWrap;
import fi.dy.masa.malilib.util.game.wrap.GameUtils;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.data.structure.StructureDataUtils.StructureFileUtils;
import fi.dy.masa.minihud.mixin.structure.ChunkGeneratorEndMixin;
import fi.dy.masa.minihud.mixin.structure.ChunkGeneratorFlatMixin;
import fi.dy.masa.minihud.mixin.structure.ChunkGeneratorHellMixin;
import fi.dy.masa.minihud.mixin.structure.ChunkGeneratorOverworldMixin;
import fi.dy.masa.minihud.mixin.structure.ChunkProviderServerMixin;
import fi.dy.masa.minihud.mixin.structure.MapGenStructureMixin;
import fi.dy.masa.minihud.util.MiscUtils;

public class StructureStorage
{
    public static final StructureStorage INSTANCE = new StructureStorage();

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

    /**
     * Gets a copy of the structure data map, and clears the dirty flag
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

    public void addStructureDataFromServer(Consumer<ArrayListMultimap<StructureType, StructureData>> dataSource)
    {
        this.addStructureData(dataSource);
        this.hasStructureDataFromServer = true;
    }

    public void addStructureDataFromLocalStructureFiles(Consumer<ArrayListMultimap<StructureType, StructureData>> dataSource)
    {
        this.addStructureData(dataSource);
        MiniHUD.debugLog("Structure data updated from local structure files, structures: {}", this.structureMap.size());
    }

    protected void addStructureData(Consumer<ArrayListMultimap<StructureType, StructureData>> dataSource)
    {
        synchronized (this.structureMap)
        {
            this.structureMap.clear();

            dataSource.accept(this.structureMap);

            this.structuresDirty = true;
            this.structuresNeedUpdating = false;

            Entity player = GameUtils.getClientPlayer();

            if (player != null)
            {
                this.lastStructureUpdatePos = EntityWrap.getEntityBlockPos(player);
            }
        }
    }

    public void updateStructureDataIfNeeded()
    {
        if (GameUtils.getClientPlayer() != null)
        {
            BlockPos playerPos = EntityWrap.getPlayerBlockPos();

            if (GameUtils.isSinglePlayer())
            {
                if (this.structuresNeedUpdating(playerPos, 32))
                {
                    this.updateStructureDataFromIntegratedServer(playerPos);
                }
            }
            else if (this.structuresNeedUpdating(playerPos, 128))
            {
                if (this.hasStructureDataFromServer)
                {
                    StructureDataUtils.requestStructureDataUpdates();
                }
                else
                {
                    this.addStructureDataFromLocalStructureFiles(StructureFileUtils::addStructureDataFromNbtFiles);
                }
            }
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
        synchronized (this.structureMap)
        {
            this.structureMap.clear();
        }

        final WorldServer world = GameUtils.getClientPlayersServerWorld();

        if (world != null)
        {
            final IChunkGenerator chunkGenerator = ((ChunkProviderServerMixin) world.getChunkProvider()).minihud_getChunkGenerator();
            final int maxRange = (GameUtils.getRenderDistanceChunks() + 4) * 16;

            world.addScheduledTask(() -> this.addStructureDataFromGenerator(chunkGenerator, playerPos, maxRange));
        }

        this.lastStructureUpdatePos = playerPos;
        this.structuresNeedUpdating = false;
    }

    private void addStructureDataFromGenerator(IChunkGenerator chunkGenerator, BlockPos playerPos, int maxRange)
    {
        if (chunkGenerator instanceof ChunkGeneratorOverworld)
        {
            MapGenStructure mapGen;

            mapGen = ((ChunkGeneratorOverworldMixin) chunkGenerator).minihud_getOceanMonumentGenerator();
            this.addStructuresWithinRange(StructureType.OCEAN_MONUMENT, mapGen, playerPos, maxRange);

            mapGen = ((ChunkGeneratorOverworldMixin) chunkGenerator).minihud_getScatteredFeatureGenerator();
            this.addTempleStructuresWithinRange(mapGen, playerPos, maxRange);

            mapGen = ((ChunkGeneratorOverworldMixin) chunkGenerator).minihud_getStrongholdGenerator();
            this.addStructuresWithinRange(StructureType.STRONGHOLD, mapGen, playerPos, maxRange);

            mapGen = ((ChunkGeneratorOverworldMixin) chunkGenerator).minihud_getVillageGenerator();
            this.addStructuresWithinRange(StructureType.VILLAGE, mapGen, playerPos, maxRange);

            mapGen = ((ChunkGeneratorOverworldMixin) chunkGenerator).minihud_getWoodlandMansionGenerator();
            this.addStructuresWithinRange(StructureType.MANSION, mapGen, playerPos, maxRange);
        }
        else if (chunkGenerator instanceof ChunkGeneratorHell)
        {
            MapGenStructure mapGen = ((ChunkGeneratorHellMixin) chunkGenerator).minihud_getFortressGenerator();
            this.addStructuresWithinRange(StructureType.NETHER_FORTRESS, mapGen, playerPos, maxRange);
        }
        else if (chunkGenerator instanceof ChunkGeneratorEnd)
        {
            MapGenStructure mapGen = ((ChunkGeneratorEndMixin) chunkGenerator).minihud_getEndCityGenerator();
            this.addStructuresWithinRange(StructureType.END_CITY, mapGen, playerPos, maxRange);
        }
        else if (chunkGenerator instanceof ChunkGeneratorFlat)
        {
            Map<String, MapGenStructure> map = ((ChunkGeneratorFlatMixin) chunkGenerator).minihud_getStructureGenerators();

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

        MiniHUD.debugLog("Structure data updated from the integrated server");
    }

    private void addStructuresWithinRange(StructureType type, MapGenStructure mapGen, BlockPos playerPos, int maxRange)
    {
        Long2ObjectMap<StructureStart> structureMap = ((MapGenStructureMixin) mapGen).minihud_getStructureMap();

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
        Long2ObjectMap<StructureStart> structureMap = ((MapGenStructureMixin) mapGen).minihud_getStructureMap();

        for (StructureStart start : structureMap.values())
        {
            List<StructureComponent> components = start.getComponents();

            if (components.size() == 1 && MiscUtils.isStructureWithinRange(start.getBoundingBox(), playerPos, maxRange))
            {
                String id = MapGenStructureIO.getStructureComponentName(components.get(0));
                StructureType type = StructureType.fromStructureId("Temple." + id);
                this.structureMap.put(type, StructureData.fromStructure(start));
            }
        }
    }
}
