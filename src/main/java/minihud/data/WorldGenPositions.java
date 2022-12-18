package minihud.data;

import java.util.ArrayList;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;

import net.minecraft.util.math.BlockPos;

import minihud.MiniHud;
import minihud.config.Configs;

public class WorldGenPositions
{
    private final WorldProperties worldProperties;
    private boolean spawnerPositionsDirty;
    private boolean waterFallPositionsDirty;

    public WorldGenPositions(WorldProperties worldProperties)
    {
        this.worldProperties = worldProperties;
    }

    public boolean areSpawnerPositionsDirty()
    {
        return this.spawnerPositionsDirty;
    }

    public boolean areWaterFallPositionsDirty()
    {
        return this.waterFallPositionsDirty;
    }

    protected void addPosition(BlockPos pos, Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> map)
    {
        int cx = pos.getX() >> 4;
        int cz = pos.getZ() >> 4;
        long cp = (long) cz << 32 | (((long) cx) & 0xFFFFFFFFL);
        ArrayList<OrderedBlockPosLong> list = map.computeIfAbsent(cp, c -> new ArrayList<>());
        int order = list.size();
        list.add(OrderedBlockPosLong.of(pos, order));
    }

    public void addDungeonSpawnerPosition(BlockPos pos)
    {
        synchronized (this.worldProperties.spawnerPositions)
        {
            this.addPosition(pos, this.worldProperties.spawnerPositions);
            this.spawnerPositionsDirty = true;
        }

        if (Configs.Generic.SPAWNER_POSITION_PRINT.getBooleanValue())
        {
            MiniHud.LOGGER.info("Spawner gen attempt: Chunk: [{}, {}] pos: [{}, {}, {}]",
                                pos.getX() >> 4, pos.getZ() >> 4, pos.getX(), pos.getY(), pos.getZ());
        }
    }

    public void addWaterFallPosition(BlockPos pos)
    {
        synchronized (this.worldProperties.waterFallPositions)
        {
            this.addPosition(pos, this.worldProperties.waterFallPositions);
            this.waterFallPositionsDirty = true;
        }
    }

    public Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> getSpawnerPositions()
    {
        Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> map = new Long2ObjectOpenHashMap<>();

        synchronized (this.worldProperties.spawnerPositions)
        {
            map.putAll(this.worldProperties.spawnerPositions);
            this.spawnerPositionsDirty = false;
        }

        return map;
    }

    public Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> getWaterFallPositions()
    {
        Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> map = new Long2ObjectOpenHashMap<>();

        synchronized (this.worldProperties.waterFallPositions)
        {
            map.putAll(this.worldProperties.waterFallPositions);
            this.waterFallPositionsDirty = false;
        }

        return map;
    }
}
