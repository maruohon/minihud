package fi.dy.masa.minihud.data;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextComponentTranslation;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import fi.dy.masa.malilib.overlay.message.MessageUtils;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.GameUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.WorldUtils;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.network.CarpetPubsubPacketHandler;
import fi.dy.masa.minihud.network.ServuxInfoSubDataPacketHandler;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnableColumnHeights;
import fi.dy.masa.minihud.renderer.RenderContainer;
import fi.dy.masa.minihud.util.MiscUtils;

public class DataStorage
{
    private static final DataStorage INSTANCE = new DataStorage();

    private final Minecraft mc = GameUtils.getClient();

    private final MobCapDataHolder mobcapData = new MobCapDataHolder();
    private final StructureStorage structureStorage = new StructureStorage();
    private final TpsData tpsData = new TpsData();
    private final WoolCounters woolCounters = new WoolCounters();

    private final Set<ChunkPos> chunkHeightmapsToCheck = new HashSet<>();
    private final Map<ChunkPos, Integer> spawnableSubChunks = new HashMap<>();
    private final Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> spawnerPositions = new Long2ObjectOpenHashMap<>();
    private final Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> waterFallPositions = new Long2ObjectOpenHashMap<>();
    private final int[] blockBreakCounter = new int[100];
    private BlockPos worldSpawn = BlockPos.ORIGIN;
    private Vec3d distanceReferencePoint = Vec3d.ZERO;
    private long worldSeed;
    private int serverDroppedChunksHashSize;
    private boolean worldSeedValid;
    private boolean worldSpawnValid;
    private boolean hasServerDroppedChunksHashSize;
    private boolean spawnerPositionsDirty;
    private boolean waterFallPositionsDirty;

    public static DataStorage getInstance()
    {
        return INSTANCE;
    }

    public MobCapDataHolder getMobcapData()
    {
        return this.mobcapData;
    }

    public StructureStorage getStructureStorage()
    {
        return this.structureStorage;
    }

    public TpsData getTpsData()
    {
        return this.tpsData;
    }

    public WoolCounters getWoolCounters()
    {
        return this.woolCounters;
    }

    public void reset()
    {
        this.worldSeedValid = false;
        this.worldSpawnValid = false;
        this.hasServerDroppedChunksHashSize = false;

        this.mobcapData.clear();
        this.structureStorage.clear();
        this.tpsData.clear();
        this.woolCounters.clear();
        this.serverDroppedChunksHashSize = 0;
        this.worldSeed = 0;
        this.worldSpawn = BlockPos.ORIGIN;

        RenderContainer.BEACON_OVERLAY.clear();

        if (this.mc.world != null)
        {
            this.structureStorage.requestStructureDataUpdates();
            CarpetPubsubPacketHandler.updatePubsubSubscriptions();
        }
        else
        {
            this.spawnerPositions.clear();
            this.waterFallPositions.clear();
            CarpetPubsubPacketHandler.unsubscribeAll();
        }
    }

    public void onLogout()
    {
        ServuxInfoSubDataPacketHandler.INSTANCE.onLogout();
    }

    public void onWorldLoad()
    {
        RenderContainer.BEACON_OVERLAY.setNeedsUpdate();
    }

    public void setWorldSeed(long seed)
    {
        this.worldSeed = seed;
        this.worldSeedValid = true;
    }

    public void setWorldSpawn(BlockPos spawn)
    {
        this.worldSpawn = spawn;
        this.worldSpawnValid = true;
    }

    public void setServerDroppedChunksHashSize(int size)
    {
        this.serverDroppedChunksHashSize = size;
        this.hasServerDroppedChunksHashSize = true;
    }

    public void setWorldSpawnIfUnknown(BlockPos spawn)
    {
        if (this.worldSpawnValid == false)
        {
            this.setWorldSpawn(spawn);
        }
    }

    public boolean isWorldSeedKnown(int dimension)
    {
        if (this.worldSeedValid)
        {
            return true;
        }
        else if (this.mc.isSingleplayer())
        {
            return this.mc.getIntegratedServer().getWorld(dimension) != null;
        }

        return false;
    }

    public boolean hasStoredWorldSeed()
    {
        return this.worldSeedValid;
    }

    public boolean getHasDroppedChunksHashSizeFromServer()
    {
        return this.hasServerDroppedChunksHashSize;
    }

    public long getWorldSeed(int dimension)
    {
        if (this.worldSeedValid == false && this.mc.isSingleplayer())
        {
            World worldTmp = this.mc.getIntegratedServer().getWorld(dimension);

            if (worldTmp != null)
            {
                this.setWorldSeed(worldTmp.getSeed());
            }
        }

        return this.worldSeed;
    }

    public boolean isWorldSpawnKnown()
    {
        return this.worldSpawnValid;
    }

    public BlockPos getWorldSpawn()
    {
        return this.worldSpawn;
    }

    public Vec3d getDistanceReferencePoint()
    {
        return this.distanceReferencePoint;
    }

    public boolean areSpawnerPositionsDirty()
    {
        return this.spawnerPositionsDirty;
    }

    public boolean areWaterFallPositionsDirty()
    {
        return this.waterFallPositionsDirty;
    }

    public void addDungeonSpawnerPosition(BlockPos pos)
    {
        int cx = pos.getX() >> 4;
        int cz = pos.getZ() >> 4;
        long cp = (long) cz << 32 | (((long) cx) & 0xFFFFFFFFL);

        synchronized (this.spawnerPositions)
        {
            ArrayList<OrderedBlockPosLong> list = this.spawnerPositions.computeIfAbsent(cp, c -> new ArrayList<>());
            int order = list.size();
            list.add(OrderedBlockPosLong.of(pos, order));
            this.spawnerPositionsDirty = true;
        }

        if (Configs.Generic.SPAWNER_POSITION_PRINT.getBooleanValue())
        {
            LiteModMiniHud.logger.info(String.format("Spawner gen attempt: Chunk: [%4d, %4d] pos: [%d, %d, %d]",
                                                     cx, cz, pos.getX(), pos.getY(), pos.getZ()));
        }
    }

    public void addWaterFallPosition(BlockPos pos)
    {
        int cx = pos.getX() >> 4;
        int cz = pos.getZ() >> 4;
        long cp = (long) cz << 32 | (((long) cx) & 0xFFFFFFFFL);

        synchronized (this.waterFallPositions)
        {
            ArrayList<OrderedBlockPosLong> list = this.waterFallPositions.computeIfAbsent(cp, c -> new ArrayList<>());
            int order = list.size();
            list.add(OrderedBlockPosLong.of(pos, order));
            this.waterFallPositionsDirty = true;
        }
    }

    public Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> getSpawnerPositions()
    {
        Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> map = new Long2ObjectOpenHashMap<>();

        synchronized (this.spawnerPositions)
        {
            map.putAll(this.spawnerPositions);
            this.spawnerPositionsDirty = false;
        }

        return map;
    }

    public Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> getWaterFallPositions()
    {
        Long2ObjectOpenHashMap<ArrayList<OrderedBlockPosLong>> map = new Long2ObjectOpenHashMap<>();

        synchronized (this.waterFallPositions)
        {
            map.putAll(this.waterFallPositions);
            this.waterFallPositionsDirty = false;
        }

        return map;
    }

    public boolean setDistanceReferencePoint()
    {
        Vec3d pos = EntityUtils.getCameraEntityPosition();
        this.distanceReferencePoint = pos;
        String x = String.format("%.2f", pos.x);
        String y = String.format("%.2f", pos.y);
        String z = String.format("%.2f", pos.z);
        MessageUtils.printCustomActionbarMessage("minihud.message.info.distance_reference_point_set", x, y, z);
        return true;
    }

    public void markChunkForHeightmapCheck(int chunkX, int chunkZ)
    {
        OverlayRendererSpawnableColumnHeights.markChunkChanged(chunkX, chunkZ);
        this.chunkHeightmapsToCheck.add(new ChunkPos(chunkX, chunkZ));
        RenderContainer.LIGHT_LEVEL_OVERLAY.setNeedsUpdate();
    }

    public HashSizeType getDroppedChunksHashSizeType()
    {
        int size = Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue();

        if (size != -1)
        {
            return HashSizeType.CONFIG;
        }

        if (this.hasServerDroppedChunksHashSize)
        {
            return HashSizeType.CARPET;
        }

        if (this.mc.isSingleplayer() && this.mc.world != null)
        {
            return HashSizeType.SINGLE_PLAYER;
        }

        return HashSizeType.FALLBACK;
    }

    public int getDroppedChunksHashSize()
    {
        HashSizeType type = this.getDroppedChunksHashSizeType();

        switch (type)
        {
            case CONFIG:
                return Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue();

            case CARPET:
                return this.serverDroppedChunksHashSize;

            case SINGLE_PLAYER:
                return MiscUtils.getCurrentHashSize(WorldUtils.getServerWorldForClientWorld(this.mc));

            case FALLBACK:
            default:
                return 0xFFFF;
        }
    }

    public void checkQueuedDirtyChunkHeightmaps()
    {
        WorldClient world = this.mc.world;

        if (world != null)
        {
            if (this.chunkHeightmapsToCheck.isEmpty() == false)
            {
                for (ChunkPos pos : this.chunkHeightmapsToCheck)
                {
                    Chunk chunk = world.getChunk(pos.x, pos.z);
                    int[] heightMap = chunk.getHeightMap();
                    int maxHeight = -1;

                    for (int j : heightMap)
                    {
                        if (j > maxHeight)
                        {
                            maxHeight = j;
                        }
                    }

                    int subChunks;

                    if (maxHeight >= 0)
                    {
                        subChunks = MathHelper.clamp((maxHeight / 16) + 1, 1, 16);
                    }
                    // Void world? Use the topFilledSegment, see WorldEntitySpawner.getRandomChunkPosition()
                    else
                    {
                        subChunks = MathHelper.clamp((chunk.getTopFilledSegment() + 16) / 16, 1, 16);
                    }

                    //System.out.printf("@ %d, %d - subChunks: %d, maxHeight: %d\n", pos.x, pos.z, subChunks, maxHeight);

                    this.spawnableSubChunks.put(pos, subChunks);
                }
            }
        }
        else
        {
            this.spawnableSubChunks.clear();
        }

        this.chunkHeightmapsToCheck.clear();
    }

    public void clearBlockBreakCounter(Minecraft mc)
    {
        if (mc.world != null)
        {
            int tick = (int) (mc.world.getTotalWorldTime() % this.blockBreakCounter.length);
            this.blockBreakCounter[tick] = 0;
        }
    }

    public void onPlayerBlockBreak(Minecraft mc)
    {
        if (mc.world != null)
        {
            int tick = (int) (mc.world.getTotalWorldTime() % this.blockBreakCounter.length);
            ++this.blockBreakCounter[tick];
        }
    }

    public double getBlockBreakingSpeed()
    {
        return MiscUtils.intAverage(this.blockBreakCounter) * 20;
    }

    public void onChunkUnload(int chunkX, int chunkZ)
    {
        ChunkPos pos = new ChunkPos(chunkX, chunkZ);
        this.chunkHeightmapsToCheck.remove(pos);
        this.spawnableSubChunks.remove(pos);
    }

    public int getSpawnableSubChunkCountFor(int chunkX, int chunkZ)
    {
        Integer count = this.spawnableSubChunks.get(new ChunkPos(chunkX, chunkZ));
        return count != null ? count.intValue() : -1;
    }

    public boolean onSendChatMessage(String message)
    {
        String[] parts = message.split(" ");

        if (parts[0].equals("minihud-seed"))
        {
            if (parts.length == 2)
            {
                try
                {
                    this.setWorldSeed(Long.parseLong(parts[1]));
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set", this.worldSeed);
                }
                catch (NumberFormatException e)
                {
                    MessageUtils.printCustomActionbarMessage("minihud.message.error.failed_to_parse_seed_from_chat");
                }
            }
            else if (this.worldSeedValid && parts.length == 1)
            {
                MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set", this.worldSeed);
            }

            return true;
        }
        else if (parts[0].equals("minihud-dropped-chunks-hash-size"))
        {
            if (parts.length == 2)
            {
                try
                {
                    int size = Integer.parseInt(parts[1]);
                    Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.setValue(size);
                    // Fetch it again from the config, to take the bounds clamping into account
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.dropped_chunks_hash_size_set_to", Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue());
                }
                catch (NumberFormatException e)
                {
                    MessageUtils.printCustomActionbarMessage("minihud.message.error.invalid_dropped_chunks_hash_size");
                }
            }
            else if (parts.length == 1)
            {
                MessageUtils.printCustomActionbarMessage("minihud.message.info.dropped_chunks_hash_size_get", this.getDroppedChunksHashSize());
            }

            return true;
        }

        return false;
    }

    public void onChatMessage(ITextComponent message)
    {
        if (message instanceof TextComponentTranslation)
        {
            TextComponentTranslation text = (TextComponentTranslation) message;

            // The vanilla "/seed" command
            if ("commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    this.setWorldSeed(Long.parseLong(text.getFormatArgs()[0].toString()));
                    LiteModMiniHud.logger.info("Received world seed from the vanilla /seed command: {}", this.worldSeed);
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set", this.worldSeed);
                }
                catch (Exception e)
                {
                    LiteModMiniHud.logger.warn("Failed to read the world seed from '{}'", text.getFormatArgs()[0], e);
                }
            }
            // The "/jed seed" command
            else if ("jed.commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    this.setWorldSeed(Long.parseLong(text.getFormatArgs()[1].toString()));
                    LiteModMiniHud.logger.info("Received world seed from the JED '/jed seed' command: {}", this.worldSeed);
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set", this.worldSeed);
                }
                catch (Exception e)
                {
                    LiteModMiniHud.logger.warn("Failed to read the world seed from '{}'", text.getFormatArgs()[1], e);
                }
            }
            else if ("commands.setworldspawn.success".equals(text.getKey()) && text.getFormatArgs().length == 3)
            {
                try
                {
                    Object[] o = text.getFormatArgs();
                    int x = Integer.parseInt(o[0].toString());
                    int y = Integer.parseInt(o[1].toString());
                    int z = Integer.parseInt(o[2].toString());

                    this.setWorldSpawn(new BlockPos(x, y, z));

                    String spawnStr = String.format("x: %d, y: %d, z: %d", this.worldSpawn.getX(), this.worldSpawn.getY(), this.worldSpawn.getZ());
                    LiteModMiniHud.logger.info("Received world spawn from the vanilla /setworldspawn command: {}", spawnStr);
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.spawn_set", spawnStr);
                }
                catch (Exception e)
                {
                    LiteModMiniHud.logger.warn("Failed to read the world spawn point from '{}'", text.getFormatArgs(), e);
                }
            }
        }
    }

    public JsonObject toJson()
    {
        JsonObject obj = new JsonObject();

        obj.add("distance_pos", JsonUtils.vec3dToJson(this.distanceReferencePoint));

        if (this.worldSeedValid)
        {
            obj.add("seed", new JsonPrimitive(this.worldSeed));
        }

        return obj;
    }

    public void fromJson(JsonObject obj)
    {
        Vec3d pos = JsonUtils.vec3dFromJson(obj, "distance_pos");

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
            this.worldSeed = JsonUtils.getLong(obj, "seed");
            this.worldSeedValid = true;
        }
    }

    public enum HashSizeType
    {
        CARPET          ("Carpet"),
        CONFIG          ("Config"),
        SINGLE_PLAYER   ("SP"),
        FALLBACK        ("0xFFFF");

        private final String displayName;

        HashSizeType(String displayName)
        {
            this.displayName = displayName;
        }

        public String getDisplayName()
        {
            return this.displayName;
        }
    }

}
