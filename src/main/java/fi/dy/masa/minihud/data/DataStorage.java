package fi.dy.masa.minihud.data;

import java.util.ArrayList;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.OptionalLong;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import it.unimi.dsi.fastutil.longs.Long2IntOpenHashMap;
import it.unimi.dsi.fastutil.longs.Long2ObjectOpenHashMap;
import it.unimi.dsi.fastutil.longs.LongOpenHashSet;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextComponentTranslation;
import net.minecraft.world.World;
import net.minecraft.world.WorldServer;
import net.minecraft.world.chunk.Chunk;
import fi.dy.masa.malilib.overlay.message.MessageUtils;
import fi.dy.masa.malilib.util.data.json.JsonUtils;
import fi.dy.masa.malilib.util.game.WorldUtils;
import fi.dy.masa.malilib.util.game.wrap.EntityWrap;
import fi.dy.masa.malilib.util.game.wrap.GameUtils;
import fi.dy.masa.malilib.util.position.PositionUtils;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.data.structure.StructureDataUtils;
import fi.dy.masa.minihud.data.structure.StructureStorage;
import fi.dy.masa.minihud.event.RenderHandler;
import fi.dy.masa.minihud.network.carpet.CarpetPubsubPacketHandler;
import fi.dy.masa.minihud.network.servux.ServuxInfoSubDataPacketHandler;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnableColumnHeights;
import fi.dy.masa.minihud.renderer.RenderContainer;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.util.MiscUtils;

public class DataStorage
{
    public static final DataStorage INSTANCE = new DataStorage();

    protected final WorldProperties worldProperties = new WorldProperties();
    private final LongOpenHashSet chunkHeightmapsToCheck = new LongOpenHashSet();
    private final Long2IntOpenHashMap spawnableSubChunks = new Long2IntOpenHashMap();
    private final int[] blockBreakCounter = new int[100];
    private Vec3d distanceReferencePoint = Vec3d.ZERO;
    private boolean spawnerPositionsDirty;
    private boolean waterFallPositionsDirty;

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

        synchronized (this.worldProperties.spawnerPositions)
        {
            ArrayList<OrderedBlockPosLong> list = this.worldProperties.spawnerPositions.computeIfAbsent(cp, c -> new ArrayList<>());
            int order = list.size();
            list.add(OrderedBlockPosLong.of(pos, order));
            this.spawnerPositionsDirty = true;
        }

        if (Configs.Generic.SPAWNER_POSITION_PRINT.getBooleanValue())
        {
            MiniHUD.LOGGER.info("Spawner gen attempt: Chunk: [{}, {}] pos: [{}, {}, {}]",
                                cx, cz, pos.getX(), pos.getY(), pos.getZ());
        }
    }

    public void addWaterFallPosition(BlockPos pos)
    {
        int cx = pos.getX() >> 4;
        int cz = pos.getZ() >> 4;
        long cp = (long) cz << 32 | (((long) cx) & 0xFFFFFFFFL);

        synchronized (this.worldProperties.waterFallPositions)
        {
            ArrayList<OrderedBlockPosLong> list = this.worldProperties.waterFallPositions.computeIfAbsent(cp, c -> new ArrayList<>());
            int order = list.size();
            list.add(OrderedBlockPosLong.of(pos, order));
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

    public void markChunkForHeightmapCheck(int chunkX, int chunkZ)
    {
        OverlayRendererSpawnableColumnHeights.markChunkChanged(chunkX, chunkZ);
        this.chunkHeightmapsToCheck.add(ChunkPos.asLong(chunkX, chunkZ));
        RenderContainer.LIGHT_LEVEL_OVERLAY.setNeedsUpdate();
    }

    public HashSizeType getDroppedChunksHashSizeType()
    {
        int size = Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue();

        if (size != -1)
        {
            return HashSizeType.CONFIG;
        }

        if (this.worldProperties.droppedChunksHashSize.isPresent())
        {
            return HashSizeType.CARPET;
        }

        if (GameUtils.isSinglePlayer() && GameUtils.getClientWorld() != null)
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

            case SINGLE_PLAYER:
                WorldServer world = GameUtils.getClientPlayersServerWorld();
                return world != null ? MiscUtils.getCurrentHashSize(world) : 0xFFFF;

            case CARPET:
                if (this.worldProperties.droppedChunksHashSize.isPresent())
                {
                    return this.worldProperties.droppedChunksHashSize.getAsInt();
                }

            case FALLBACK:
            default:
                return 0xFFFF;
        }
    }

    public void checkQueuedDirtyChunkHeightmaps()
    {
        WorldClient world = GameUtils.getClientWorld();

        if (world != null)
        {
            if (this.chunkHeightmapsToCheck.isEmpty() == false)
            {
                for (long posLong : this.chunkHeightmapsToCheck)
                {
                    Chunk chunk = world.getChunk(PositionUtils.getChunkPosX(posLong),
                                                 PositionUtils.getChunkPosZ(posLong));
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

                    this.spawnableSubChunks.put(posLong, subChunks);
                }
            }
        }
        else
        {
            this.spawnableSubChunks.clear();
        }

        this.chunkHeightmapsToCheck.clear();
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

    public void onChunkUnload(int chunkX, int chunkZ)
    {
        long posLong = ChunkPos.asLong(chunkX, chunkZ);
        this.chunkHeightmapsToCheck.remove(posLong);
        this.spawnableSubChunks.remove(posLong);
    }

    public int getSpawnableSubChunkCountFor(int chunkX, int chunkZ)
    {
        return this.spawnableSubChunks.getOrDefault(ChunkPos.asLong(chunkX, chunkZ), -1);
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
                    long seed = Long.parseLong(parts[1]);
                    this.setWorldSeed(seed);
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set", seed);
                }
                catch (NumberFormatException e)
                {
                    MessageUtils.printCustomActionbarMessage("minihud.message.error.failed_to_parse_seed_from_chat");
                }
            }
            else if (this.worldProperties.worldSeed.isPresent() && parts.length == 1)
            {
                MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set",
                                                         this.worldProperties.worldSeed.getAsLong());
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
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.dropped_chunks_hash_size_set_to",
                                                             Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue());
                }
                catch (NumberFormatException e)
                {
                    MessageUtils.printCustomActionbarMessage("minihud.message.error.invalid_dropped_chunks_hash_size");
                }
            }
            else if (parts.length == 1)
            {
                MessageUtils.printCustomActionbarMessage("minihud.message.info.dropped_chunks_hash_size_get",
                                                         this.getDroppedChunksHashSize());
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
                    long seed = Long.parseLong(text.getFormatArgs()[0].toString());
                    this.setWorldSeed(seed);
                    LiteModMiniHud.logger.info("Received world seed from the vanilla /seed command: {}", seed);
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set", seed);
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
                    long seed = Long.parseLong(text.getFormatArgs()[1].toString());
                    this.setWorldSeed(seed);
                    LiteModMiniHud.logger.info("Received world seed from the JED '/jed seed' command: {}", seed);
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set", seed);
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

                    BlockPos spawn = new BlockPos(x, y, z);
                    this.setWorldSpawn(spawn);

                    String spawnStr = String.format("x: %d, y: %d, z: %d", spawn.getX(), spawn.getY(), spawn.getZ());
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

        if (this.worldProperties.worldSeed.isPresent())
        {
            long seed = this.worldProperties.worldSeed.getAsLong();
            obj.add("seed", new JsonPrimitive(seed));
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
            this.worldProperties.worldSeed = OptionalLong.of(JsonUtils.getLong(obj, "seed"));
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
