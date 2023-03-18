package fi.dy.masa.minihud.util;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Queues;
import com.google.common.util.concurrent.ThreadFactoryBuilder;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.client.MinecraftClient;
import net.minecraft.entity.Entity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtList;
import net.minecraft.registry.RegistryKey;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.ServerTask;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.structure.StructureStart;
import net.minecraft.text.LiteralTextContent;
import net.minecraft.text.MutableText;
import net.minecraft.text.Text;
import net.minecraft.text.TranslatableTextContent;
import net.minecraft.util.Formatting;
import net.minecraft.util.Identifier;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.util.math.Vec3i;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraft.world.chunk.ChunkStatus;
import net.minecraft.world.gen.structure.Structure;
import fi.dy.masa.malilib.network.ClientPacketChannelHandler;
import fi.dy.masa.malilib.util.Constants;
import fi.dy.masa.malilib.util.InfoUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.MobCapDataHandler;
import fi.dy.masa.minihud.network.StructurePacketHandlerCarpet;
import fi.dy.masa.minihud.network.StructurePacketHandlerServux;
import fi.dy.masa.minihud.renderer.OverlayRendererBeaconRange;
import fi.dy.masa.minihud.renderer.OverlayRendererBiomeBorders;
import fi.dy.masa.minihud.renderer.OverlayRendererConduitRange;
import fi.dy.masa.minihud.renderer.OverlayRendererLightLevel;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnableColumnHeights;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.renderer.worker.ChunkTask;
import fi.dy.masa.minihud.renderer.worker.ThreadWorker;
import fi.dy.masa.minihud.util.MiscUtils;

public class DataStorage
{
    private static final ThreadFactory THREAD_FACTORY = (new ThreadFactoryBuilder()).setNameFormat("MiniHUD Worker Thread %d").setDaemon(true).build();
    private static final Pattern PATTERN_CARPET_TPS = Pattern.compile("TPS: (?<tps>[0-9]+[\\.,][0-9]) MSPT: (?<mspt>[0-9]+[\\.,][0-9])");

    public static final DataStorage INSTANCE = new DataStorage();

    public static final int CARPET_ID_BOUNDINGBOX_MARKERS = 3;
    public static final int CARPET_ID_LARGE_BOUNDINGBOX_MARKERS_START = 7;
    public static final int CARPET_ID_LARGE_BOUNDINGBOX_MARKERS = 8;

    private final MobCapDataHandler mobCapData = new MobCapDataHandler();
    private boolean worldSeedValid;
    private boolean serverTPSValid;
    private boolean hasSyncedTime;
    private boolean carpetServer;
    private boolean servuxServer;
    private boolean worldSpawnValid;
    private boolean hasStructureDataFromServer;
    private boolean structureRendererNeedsUpdate;
    private boolean structuresNeedUpdating;
    private boolean shouldRegisterStructureChannel;
    private int structureDataTimeout = 30 * 20;
    private long worldSeed;
    private long lastServerTick;
    private long lastServerTimeUpdate;
    private BlockPos lastStructureUpdatePos;
    private double serverTPS;
    private double serverMSPT;
    private BlockPos worldSpawn = BlockPos.ORIGIN;
    private Vec3d distanceReferencePoint = Vec3d.ZERO;
    private final int[] blockBreakCounter = new int[100];
    private final ArrayListMultimap<StructureType, StructureData> structures = ArrayListMultimap.create();
    private final MinecraftClient mc = MinecraftClient.getInstance();

    private final PriorityBlockingQueue<ChunkTask> taskQueue = Queues.newPriorityBlockingQueue();
    private final Thread workerThread;
    private final ThreadWorker worker;

    private DataStorage()
    {
        this.worker = new ThreadWorker();
        this.workerThread = THREAD_FACTORY.newThread(this.worker);
        this.workerThread.start();
    }

    public static DataStorage getInstance()
    {
        return INSTANCE;
    }

    public MobCapDataHandler getMobCapData()
    {
        return this.mobCapData;
    }

    public void reset(boolean isLogout)
    {
        if (isLogout)
        {
            MiniHUD.printDebug("DataStorage#reset() - log-out");
            /*
            this.worker.stopThread();

            try
            {
                this.workerThread.interrupt();
                this.workerThread.join();
            }
            catch (InterruptedException e)
            {
                MiniHUD.logger.warn("Interrupted whilst waiting for worker thread to die", e);
            }
            */
        }
        else
        {
            MiniHUD.printDebug("DataStorage#reset() - dimension change or log-in");
        }

        this.mobCapData.clear();
        this.serverTPSValid = false;
        this.hasSyncedTime = false;
        this.carpetServer = false;
        this.worldSpawnValid = false;
        this.structuresNeedUpdating = true;
        this.hasStructureDataFromServer = false;
        this.structureRendererNeedsUpdate = true;

        this.lastStructureUpdatePos = null;
        this.structures.clear();
        this.worldSpawn = BlockPos.ORIGIN;
        this.clearTasks();

        StructurePacketHandlerCarpet.INSTANCE.reset();
        StructurePacketHandlerServux.INSTANCE.reset();
        ShapeManager.INSTANCE.clear();
        OverlayRendererBeaconRange.INSTANCE.clear();
        OverlayRendererConduitRange.INSTANCE.clear();
        OverlayRendererBiomeBorders.INSTANCE.clear();
        OverlayRendererLightLevel.reset();

        if (isLogout || Configs.Generic.DONT_RESET_SEED_ON_DIMENSION_CHANGE.getBooleanValue() == false)
        {
            this.worldSeedValid = false;
            this.worldSeed = 0;
        }

        if (isLogout)
        {
            this.servuxServer = false;
            this.structureDataTimeout = 30 * 20;
        }
    }

    public void clearTasks()
    {
        this.taskQueue.clear();
    }

    public ChunkTask getNextTask() throws InterruptedException
    {
        return this.taskQueue.take();
    }

    public void addTask(Runnable task, ChunkPos pos, Vec3i playerPos)
    {
        if (this.taskQueue.size() < 64000)
        {
            this.taskQueue.offer(new ChunkTask(task, pos, playerPos));
        }
    }

    public void setIsServuxServer()
    {
        MiniHUD.printDebug("DataStorage#setIsServuxServer()");
        this.servuxServer = true;
        ClientPacketChannelHandler.getInstance().unregisterClientChannelHandler(StructurePacketHandlerCarpet.INSTANCE);
    }

    public void onWorldJoin()
    {
        MiniHUD.printDebug("DataStorage#onWorldJoin()");
        OverlayRendererBeaconRange.INSTANCE.setNeedsUpdate();
        OverlayRendererConduitRange.INSTANCE.setNeedsUpdate();

        if (this.mc.isIntegratedServerRunning() == false && RendererToggle.OVERLAY_STRUCTURE_MAIN_TOGGLE.getBooleanValue())
        {
            this.shouldRegisterStructureChannel = true;
        }
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

    public void setWorldSpawnIfUnknown(BlockPos spawn)
    {
        if (this.worldSpawnValid == false)
        {
            this.setWorldSpawn(spawn);
        }
    }

    public boolean isWorldSeedKnown(World world)
    {
        if (this.worldSeedValid)
        {
            return true;
        }
        else if (this.mc.isIntegratedServerRunning())
        {
            MinecraftServer server = this.mc.getServer();
            World worldTmp = server.getWorld(world.getRegistryKey());
            return worldTmp != null;
        }

        return false;
    }

    public boolean hasStoredWorldSeed()
    {
        return this.worldSeedValid;
    }

    public long getWorldSeed(World world)
    {
        if (this.worldSeedValid == false && this.mc.isIntegratedServerRunning())
        {
            MinecraftServer server = this.mc.getServer();
            ServerWorld worldTmp = server.getWorld(world.getRegistryKey());

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

    public boolean hasTPSData()
    {
        return this.serverTPSValid;
    }

    public boolean isCarpetServer()
    {
        return this.carpetServer;
    }

    public double getServerTPS()
    {
        return this.serverTPS;
    }

    public double getServerMSPT()
    {
        return this.serverMSPT;
    }

    public boolean structureRendererNeedsUpdate()
    {
        return this.structureRendererNeedsUpdate;
    }

    public void setStructuresNeedUpdating()
    {
        this.structuresNeedUpdating = true;
    }

    public void setStructureRendererNeedsUpdate()
    {
        this.structureRendererNeedsUpdate = true;
    }

    public Vec3d getDistanceReferencePoint()
    {
        return this.distanceReferencePoint;
    }

    public void setDistanceReferencePoint(Vec3d pos)
    {
        this.distanceReferencePoint = pos;
        String str = String.format("x: %.2f, y: %.2f, z: %.2f", pos.x, pos.y, pos.z);
        InfoUtils.printActionbarMessage("minihud.message.distance_reference_point_set", str);
    }

    public void markChunkForHeightmapCheck(int chunkX, int chunkZ)
    {
        Entity entity = MinecraftClient.getInstance().getCameraEntity();

        // Only update the renderers when blocks change near the camera
        if (entity != null)
        {
            Vec3d pos = entity.getPos();

            if (Math.abs(pos.x - (chunkX << 4) - 8) <= 48D || Math.abs(pos.z - (chunkZ << 4) - 8) <= 48D)
            {
                OverlayRendererSpawnableColumnHeights.markChunkChanged(chunkX, chunkZ);
                OverlayRendererLightLevel.setNeedsUpdate();
            }
        }
    }

    public void onClientTickPre(MinecraftClient mc)
    {
        if (mc.world != null)
        {
            int tick = (int) (mc.world.getTime() % this.blockBreakCounter.length);
            this.blockBreakCounter[tick] = 0;
        }
    }

    public void onPlayerBlockBreak(MinecraftClient mc)
    {
        if (mc.world != null)
        {
            int tick = (int) (mc.world.getTime() % this.blockBreakCounter.length);
            ++this.blockBreakCounter[tick];
        }
    }

    public double getBlockBreakingSpeed()
    {
        return MiscUtils.intAverage(this.blockBreakCounter) * 20;
    }

    public boolean onSendChatMessage(String message)
    {
        String[] parts = message.split(" ");

        if (parts.length > 0 && (parts[0].equals("minihud-seed") || parts[0].equals("/minihud-seed")))
        {
            if (parts.length == 2)
            {
                try
                {
                    this.setWorldSeed(Long.parseLong(parts[1]));
                    InfoUtils.printActionbarMessage("minihud.message.seed_set", this.worldSeed);
                }
                catch (NumberFormatException e)
                {
                    InfoUtils.printActionbarMessage("minihud.message.error.invalid_seed");
                }
            }
            else if (parts.length == 1)
            {
                if (this.worldSeedValid)
                {
                    InfoUtils.printActionbarMessage("minihud.message.seed_is", this.worldSeed);
                }
                else
                {
                    InfoUtils.printActionbarMessage("minihud.message.no_seed");
                }
            }

            return true;
        }

        return false;
    }

    public void onChatMessage(Text message)
    {
        if (message instanceof MutableText mutableText &&
            mutableText.getContent() instanceof TranslatableTextContent text)
        {
            // The vanilla "/seed" command
            if ("commands.seed.success".equals(text.getKey()) && text.getArgs().length == 1)
            {
                try
                {
                    //String str = message.getString();
                    //int i1 = str.indexOf("[");
                    //int i2 = str.indexOf("]");
                    MutableText m = (MutableText) text.getArgs()[0];
                    TranslatableTextContent t = (TranslatableTextContent) m.getContent();
                    LiteralTextContent l = (LiteralTextContent) ((MutableText) t.getArgs()[0]).getContent();
                    String str = l.string();

                    //if (i1 != -1 && i2 != -1)
                    {
                        //this.setWorldSeed(Long.parseLong(str.substring(i1 + 1, i2)));
                        this.setWorldSeed(Long.parseLong(str));
                        MiniHUD.logger.info("Received world seed from the vanilla /seed command: {}", this.worldSeed);
                        InfoUtils.printActionbarMessage("minihud.message.seed_set", this.worldSeed);
                    }
                }
                catch (Exception e)
                {
                    MiniHUD.logger.warn("Failed to read the world seed from '{}'", text.getArgs()[0]);
                }
            }
            // The "/jed seed" command
            else if ("jed.commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    this.setWorldSeed(Long.parseLong(text.getArgs()[1].toString()));
                    MiniHUD.logger.info("Received world seed from the JED '/jed seed' command: {}", this.worldSeed);
                    InfoUtils.printActionbarMessage("minihud.message.seed_set", this.worldSeed);
                }
                catch (Exception e)
                {
                    MiniHUD.logger.warn("Failed to read the world seed from '{}'", text.getArgs()[1], e);
                }
            }
            else if ("commands.setworldspawn.success".equals(text.getKey()) && text.getArgs().length == 3)
            {
                try
                {
                    Object[] o = text.getArgs();
                    int x = Integer.parseInt(o[0].toString());
                    int y = Integer.parseInt(o[1].toString());
                    int z = Integer.parseInt(o[2].toString());

                    this.setWorldSpawn(new BlockPos(x, y, z));

                    String spawnStr = String.format("x: %d, y: %d, z: %d", this.worldSpawn.getX(), this.worldSpawn.getY(), this.worldSpawn.getZ());
                    MiniHUD.logger.info("Received world spawn from the vanilla /setworldspawn command: {}", spawnStr);
                    InfoUtils.printActionbarMessage("minihud.message.spawn_set", spawnStr);
                }
                catch (Exception e)
                {
                    MiniHUD.logger.warn("Failed to read the world spawn point from '{}'", text.getArgs(), e);
                }
            }
        }
    }

    public void onServerTimeUpdate(long totalWorldTime)
    {
        // Carpet server sends the TPS and MSPT values via the player list footer data,
        // and for single player the data is grabbed directly from the integrated server.
        if (this.carpetServer == false && this.mc.isInSingleplayer() == false)
        {
            long currentTime = System.nanoTime();

            if (this.hasSyncedTime)
            {
                long elapsedTicks = totalWorldTime - this.lastServerTick;

                if (elapsedTicks > 0)
                {
                    this.serverMSPT = ((double) (currentTime - this.lastServerTimeUpdate) / (double) elapsedTicks) / 1000000D;
                    this.serverTPS = this.serverMSPT <= 50 ? 20D : (1000D / this.serverMSPT);
                    this.serverTPSValid = true;
                }
            }

            this.lastServerTick = totalWorldTime;
            this.lastServerTimeUpdate = currentTime;
            this.hasSyncedTime = true;
        }
    }

    public void updateIntegratedServerTPS()
    {
        if (this.mc != null && this.mc.player != null && this.mc.getServer() != null)
        {
            this.serverMSPT = MiscUtils.longAverage(this.mc.getServer().lastTickLengths) / 1000000D;
            this.serverTPS = this.serverMSPT <= 50 ? 20D : (1000D / this.serverMSPT);
            this.serverTPSValid = true;
        }
    }

    /**
     * Gets a copy of the structure data map, and clears the dirty flag
     */
    public ArrayListMultimap<StructureType, StructureData> getCopyOfStructureData()
    {
        ArrayListMultimap<StructureType, StructureData> copy = ArrayListMultimap.create();

        if (RendererToggle.OVERLAY_STRUCTURE_MAIN_TOGGLE.getBooleanValue() == false)
        {
            return copy;
        }

        synchronized (this.structures)
        {
            for (StructureType type : StructureType.VALUES)
            {
                Collection<StructureData> values = this.structures.get(type);

                if (values.isEmpty() == false)
                {
                    copy.putAll(type, values);
                }
            }

            this.structureRendererNeedsUpdate = false;
        }

        return copy;
    }

    public void updateStructureData()
    {
        if (this.mc != null && this.mc.world != null && this.mc.player != null)
        {
            long currentTime = this.mc.world.getTime();

            if ((currentTime % 20) == 0)
            {
                if (this.mc.isIntegratedServerRunning())
                {
                    if (RendererToggle.OVERLAY_STRUCTURE_MAIN_TOGGLE.getBooleanValue())
                    {
                        BlockPos playerPos = PositionUtils.getEntityBlockPos(this.mc.player);

                        if (this.structuresNeedUpdating(playerPos, 32))
                        {
                            this.updateStructureDataFromIntegratedServer(playerPos);
                        }
                    }
                }
                else if (this.hasStructureDataFromServer)
                {
                    this.removeExpiredStructures(currentTime, this.structureDataTimeout);
                }
                else if (this.shouldRegisterStructureChannel && this.mc.getNetworkHandler() != null)
                {
                    if (RendererToggle.OVERLAY_STRUCTURE_MAIN_TOGGLE.getBooleanValue())
                    {
                        MiniHUD.printDebug("DataStorage#updateStructureData(): Unregister channels");
                        // (re-)register the structure packet handlers
                        ClientPacketChannelHandler.getInstance().unregisterClientChannelHandler(StructurePacketHandlerCarpet.INSTANCE);
                        ClientPacketChannelHandler.getInstance().unregisterClientChannelHandler(StructurePacketHandlerServux.INSTANCE);

                        this.registerStructureChannel();
                    }

                    this.shouldRegisterStructureChannel = false;
                }
            }
        }
    }

    public void registerStructureChannel()
    {
        MiniHUD.printDebug("DataStorage#registerStructureChannel(): Servux");
        ClientPacketChannelHandler.getInstance().registerClientChannelHandler(StructurePacketHandlerServux.INSTANCE);

        // Don't register the Carpet structure channel if the server is known to have the Servux mod
        if (this.servuxServer == false)
        {
            MiniHUD.printDebug("DataStorage#registerStructureChannel(): Carpet");
            ClientPacketChannelHandler.getInstance().registerClientChannelHandler(StructurePacketHandlerCarpet.INSTANCE);
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
        final RegistryKey<World> worldId = this.mc.player.getEntityWorld().getRegistryKey();
        final ServerWorld world = this.mc.getServer().getWorld(worldId);

        if (world != null)
        {
            MinecraftServer server = this.mc.getServer();
            final int maxChunkRange = this.mc.options.getViewDistance().getValue() + 2;

            server.send(new ServerTask(server.getTicks(), () ->
            {
                synchronized (this.structures)
                {
                    this.addStructureDataFromGenerator(world, playerPos, maxChunkRange);
                }
            }));
        }
        else
        {
            synchronized (this.structures)
            {
                this.structures.clear();
            }
        }

        this.lastStructureUpdatePos = playerPos;
        this.structuresNeedUpdating = false;
    }

    public void addOrUpdateStructuresFromServer(NbtList structures, int timeout, boolean isServux)
    {
        MiniHUD.printDebug("DataStorage#addOrUpdateStructuresFromServer(): start");

        // Ignore the data from QuickCarpet if the Servux mod is also present
        if (this.servuxServer && isServux == false)
        {
            return;
        }

        if (structures.getHeldType() == Constants.NBT.TAG_COMPOUND)
        {
            MiniHUD.printDebug("DataStorage#addOrUpdateStructuresFromServer(): count: " + structures.size());
            this.structureDataTimeout = timeout + 200;

            long currentTime = this.mc.world.getTime();
            final int count = structures.size();

            this.removeExpiredStructures(currentTime, this.structureDataTimeout);

            for (int i = 0; i < count; ++i)
            {
                NbtCompound tag = structures.getCompound(i);
                StructureData data = StructureData.fromStructureStartTag(tag, currentTime);

                if (data != null)
                {
                    // Remove the old entry and replace it with the new entry with the current refresh time
                    if (this.structures.containsEntry(data.getStructureType(), data))
                    {
                        this.structures.remove(data.getStructureType(), data);
                    }

                    this.structures.put(data.getStructureType(), data);
                }
            }

            this.structureRendererNeedsUpdate = true;
            this.hasStructureDataFromServer = true;
        }
    }

    private void removeExpiredStructures(long currentTime, int timeout)
    {
        long maxAge = timeout;
        this.structures.values().removeIf(data -> currentTime > (data.getRefreshTime() + maxAge));
    }

    private void addStructureDataFromGenerator(ServerWorld world, BlockPos playerPos, int maxChunkRange)
    {
        this.structures.clear();

        int minCX = (playerPos.getX() >> 4) - maxChunkRange;
        int minCZ = (playerPos.getZ() >> 4) - maxChunkRange;
        int maxCX = (playerPos.getX() >> 4) + maxChunkRange;
        int maxCZ = (playerPos.getZ() >> 4) + maxChunkRange;

        for (int cz = minCZ; cz <= maxCZ; ++cz)
        {
            for (int cx = minCX; cx <= maxCX; ++cx)
            {
                // Don't load the chunk
                Chunk chunk = world.getChunk(cx, cz, ChunkStatus.FULL, false);

                if (chunk == null)
                {
                    continue;
                }

                for (Map.Entry<Structure, StructureStart> entry : chunk.getStructureStarts().entrySet())
                {
                    Structure structure = entry.getKey();
                    StructureStart start = entry.getValue();
                    Identifier id = world.getRegistryManager().get(RegistryKeys.STRUCTURE).getId(structure);
                    StructureType type = StructureType.fromStructureId(id != null ? id.toString() : "?");

                    if (type.isEnabled() &&
                        start.hasChildren() &&
                        MiscUtils.isStructureWithinRange(start.getBoundingBox(), playerPos, maxChunkRange << 4))
                    {
                        this.structures.put(type, StructureData.fromStructureStart(type, start));
                    }
                }
            }
        }

        this.structureRendererNeedsUpdate = true;

        //MiniHUD.printDebug("Structure data updated from the integrated server ({} structures)", this.structures.size());
    }

    public void handleCarpetServerTPSData(Text textComponent)
    {
        if (textComponent.getString().isEmpty() == false)
        {
            String text = Formatting.strip(textComponent.getString());
            String[] lines = text.split("\n");

            for (String line : lines)
            {
                Matcher matcher = PATTERN_CARPET_TPS.matcher(line);

                if (matcher.matches())
                {
                    try
                    {
                        this.serverTPS = Double.parseDouble(matcher.group("tps"));
                        this.serverMSPT = Double.parseDouble(matcher.group("mspt"));
                        this.serverTPSValid = true;
                        this.carpetServer = true;
                        return;
                    }
                    catch (NumberFormatException ignore)
                    {
                    }
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
}
