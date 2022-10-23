package fi.dy.masa.minihud.data;

import java.util.ArrayList;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.OptionalLong;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import net.minecraft.util.math.BlockPos;
import fi.dy.masa.minihud.config.Configs;

public class WorldProperties
{
    public final Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> spawnerPositions = new Long2ObjectOpenHashMap<>();
    public final Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> waterFallPositions = new Long2ObjectOpenHashMap<>();
    public OptionalInt droppedChunksHashSize = OptionalInt.empty();
    public OptionalLong worldSeed = OptionalLong.empty();
    public Optional<BlockPos> worldSpawn = Optional.empty();

    public void clearSimple()
    {
        this.droppedChunksHashSize = OptionalInt.empty();
        this.worldSpawn = Optional.empty();
        this.worldSeed = OptionalLong.empty();
    }

    public void clearOnWorldChange(boolean isLogout)
    {
        this.droppedChunksHashSize = OptionalInt.empty();
        this.worldSpawn = Optional.empty();

        if (isLogout || Configs.Generic.USE_PER_DIMENSION_SEED.getBooleanValue())
        {
            this.worldSeed = OptionalLong.empty();
        }

        if (isLogout)
        {
            this.clearGenerationPositions();
        }
    }

    public void clearGenerationPositions()
    {
        this.spawnerPositions.clear();
        this.waterFallPositions.clear();
    }
}
