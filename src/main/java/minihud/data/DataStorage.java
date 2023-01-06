package minihud.data;

import java.util.Optional;
import java.util.OptionalInt;
import java.util.OptionalLong;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;

import malilib.overlay.message.MessageUtils;
import malilib.util.data.json.JsonUtils;
import malilib.util.game.WorldUtils;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import minihud.data.structure.StructureDataUtils;
import minihud.data.structure.StructureStorage;
import minihud.event.RenderHandler;
import minihud.network.carpet.CarpetPubsubPacketHandler;
import minihud.network.servux.ServuxInfoSubDataPacketHandler;
import minihud.renderer.RenderContainer;
import minihud.renderer.shapes.ShapeManager;
import minihud.util.MiscUtils;

public class DataStorage
{
    public static final DataStorage INSTANCE = new DataStorage();

    protected final WorldProperties worldProperties = new WorldProperties();
    public final WorldGenPositions worldGenPositions;
    private final int[] blockBreakCounter = new int[100];
    private Vec3d distanceReferencePoint = Vec3d.ZERO;

    public DataStorage()
    {
        this.worldGenPositions = new WorldGenPositions(this.worldProperties);
    }

    public static DataStorage getInstance()
    {
        return INSTANCE;
    }

    public void clear(boolean isLogout)
    {
        this.worldProperties.clearOnWorldChange(isLogout);
        RenderHandler.INSTANCE.setReady(false);

        MobCapDataHandler.INSTANCE.clear();
        ShapeManager.INSTANCE.clear();
        StructureStorage.INSTANCE.clear();
        TpsDataManager.INSTANCE.clear();
        WoolCounters.INSTANCE.clear();

        RenderContainer.BEACON_OVERLAY.clear();

        if (isLogout)
        {
            CarpetPubsubPacketHandler.INSTANCE.unsubscribeAll();
            ServuxInfoSubDataPacketHandler.INSTANCE.onLogout();
        }
        else
        {
            CarpetPubsubPacketHandler.INSTANCE.updatePubSubSubscriptions();
            ServuxInfoSubDataPacketHandler.INSTANCE.updateSubscriptions();
            StructureDataUtils.requestStructureDataUpdates();
        }
    }

    public void afterWorldLoad()
    {
        RenderContainer.BEACON_OVERLAY.setNeedsUpdate();
        RenderHandler.INSTANCE.setReady(true);
    }

    public void setWorldSeed(long seed)
    {
        this.worldProperties.worldSeed = OptionalLong.of(seed);
    }

    public void setWorldSpawn(BlockPos spawn)
    {
        this.worldProperties.worldSpawn = Optional.of(spawn);
    }

    public void setWorldSpawnIfUnknown(BlockPos spawn)
    {
        if (this.worldProperties.worldSpawn.isPresent() == false)
        {
            this.setWorldSpawn(spawn);
        }
    }

    public void setServerDroppedChunksHashSize(int size)
    {
        this.worldProperties.droppedChunksHashSize = OptionalInt.of(size);
    }

    public boolean isWorldSeedKnown(World world)
    {
        if (this.worldProperties.worldSeed.isPresent())
        {
            return true;
        }
        else if (GameUtils.isSinglePlayer())
        {
            return GameUtils.getIntegratedServer().getWorld(WorldUtils.getDimensionId(world)) != null;
        }

        return false;
    }

    public boolean hasStoredWorldSeed()
    {
        return this.worldProperties.worldSeed.isPresent();
    }

    public long getStoredWorldSeed()
    {
        return this.worldProperties.worldSeed.isPresent() ? this.worldProperties.worldSeed.getAsLong() : 0;
    }

    public long getWorldSeed(World world)
    {
        if (this.hasStoredWorldSeed() == false && GameUtils.isSinglePlayer())
        {
            World worldTmp = WorldUtils.getServerWorldForClientWorld(world);

            if (worldTmp != null)
            {
                this.setWorldSeed(worldTmp.getSeed());
            }
        }

        return this.worldProperties.worldSeed.isPresent() ? this.worldProperties.worldSeed.getAsLong() : 0;
    }

    public boolean isWorldSpawnKnown()
    {
        return this.worldProperties.worldSpawn.isPresent();
    }

    public BlockPos getWorldSpawn()
    {
        return this.worldProperties.worldSpawn.isPresent() ? this.worldProperties.worldSpawn.get() : BlockPos.ORIGIN;
    }

    public Vec3d getDistanceReferencePoint()
    {
        return this.distanceReferencePoint;
    }

    public boolean setDistanceReferencePoint()
    {
        Vec3d pos = EntityWrap.getCameraEntityPosition();
        this.distanceReferencePoint = pos;
        String x = String.format("%.2f", pos.x);
        String y = String.format("%.2f", pos.y);
        String z = String.format("%.2f", pos.z);
        MessageUtils.printCustomActionbarMessage("minihud.message.info.distance_reference_point_set", x, y, z);
        return true;
    }

    public void onBlocksChangedInChunk(int chunkX, int chunkZ)
    {
        RenderContainer.LIGHT_LEVEL_OVERLAY.setNeedsUpdate();
        RenderContainer.SPAWNABLE_COLUMN_HEIGHTS_OVERLAY.markChunkChanged(chunkX, chunkZ);
    }

    public void clearBlockBreakCounter()
    {
        if (GameUtils.getClientWorld() != null)
        {
            int tick = (int) (GameUtils.getCurrentWorldTick() % this.blockBreakCounter.length);
            this.blockBreakCounter[tick] = 0;
        }
    }

    public void onPlayerBlockBreak()
    {
        if (GameUtils.getClientWorld() != null)
        {
            int tick = (int) (GameUtils.getCurrentWorldTick() % this.blockBreakCounter.length);
            ++this.blockBreakCounter[tick];
        }
    }

    public double getBlockBreakingSpeed()
    {
        return MiscUtils.intAverage(this.blockBreakCounter) * 20;
    }

    public JsonObject toJson()
    {
        JsonObject obj = new JsonObject();

        obj.add("distance_pos", JsonUtils.vec3dToJson(this.distanceReferencePoint));

        if (this.worldProperties.worldSeed.isPresent())
        {
            long seed = this.worldProperties.worldSeed.getAsLong();
            obj.add("seed", new JsonPrimitive(seed));
        }

        return obj;
    }

    public void fromJson(JsonObject obj)
    {
        Vec3d pos = JsonUtils.getVec3d(obj, "distance_pos");

        if (pos != null)
        {
            this.distanceReferencePoint = pos;
        }
        else
        {
            this.distanceReferencePoint = Vec3d.ZERO;
        }

        if (JsonUtils.hasLong(obj, "seed"))
        {
            this.worldProperties.worldSeed = OptionalLong.of(JsonUtils.getLong(obj, "seed"));
        }
    }
}
