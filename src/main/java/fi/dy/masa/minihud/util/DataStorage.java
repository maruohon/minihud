package fi.dy.masa.minihud.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.MultimapBuilder;
import com.google.gson.JsonObject;
import net.minecraft.client.MinecraftClient;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.ServerTask;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.structure.StructureStart;
import net.minecraft.text.Text;
import net.minecraft.text.TranslatableText;
import net.minecraft.util.Formatting;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraft.world.chunk.ChunkStatus;
import net.minecraft.world.dimension.DimensionType;
import fi.dy.masa.malilib.network.ClientPacketChannelHandler;
import fi.dy.masa.malilib.util.Constants;
import fi.dy.masa.malilib.util.InfoUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.network.StructurePacketHandler;
import fi.dy.masa.minihud.renderer.OverlayRendererLightLevel;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnableColumnHeights;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.util.StructureTypes.StructureType;

public class DataStorage
{
    private static final Pattern PATTERN_CARPET_TPS = Pattern.compile("TPS: (?<tps>[0-9]+[\\.,][0-9]) MSPT: (?<mspt>[0-9]+[\\.,][0-9])");
    private static final DataStorage INSTANCE = new DataStorage();

    public static final int CARPET_ID_BOUNDINGBOX_MARKERS = 3;
    public static final int CARPET_ID_LARGE_BOUNDINGBOX_MARKERS_START = 7;
    public static final int CARPET_ID_LARGE_BOUNDINGBOX_MARKERS = 8;

    private boolean worldSeedValid;
    private boolean serverTPSValid;
    private boolean carpetServer;
    private boolean worldSpawnValid;
    private boolean hasStructureDataFromServer;
    private boolean structureRendererNeedsUpdate;
    private boolean structuresNeedUpdating;
    private boolean shouldRegisterStructureChannel;
    private int structureDataTimeout = 800;
    private long worldSeed;
    private long lastServerTick;
    private long lastServerTimeUpdate;
    private BlockPos lastStructureUpdatePos;
    private double serverTPS;
    private double serverMSPT;
    private BlockPos worldSpawn = BlockPos.ORIGIN;
    private Vec3d distanceReferencePoint = Vec3d.ZERO;
    private final Multimap<StructureType, StructureData> structures = MultimapBuilder.hashKeys().hashSetValues().build();
    private final MinecraftClient mc = MinecraftClient.getInstance();

    public static DataStorage getInstance()
    {
        return INSTANCE;
    }

    public void reset()
    {
        this.worldSeedValid = false;
        this.serverTPSValid = false;
        this.carpetServer = false;
        this.worldSpawnValid = false;
        this.structuresNeedUpdating = true;
        this.hasStructureDataFromServer = false;
        this.structureRendererNeedsUpdate = false;

        this.lastStructureUpdatePos = null;
        this.structures.clear();
        this.structureDataTimeout = 800;
        this.worldSeed = 0;
        this.worldSpawn = BlockPos.ORIGIN;

        StructurePacketHandler.INSTANCE.reset();
        ShapeManager.INSTANCE.clear();
    }

    public void onWorldJoin()
    {
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

    public boolean isWorldSeedKnown(DimensionType dimension)
    {
        if (this.worldSeedValid)
        {
            return true;
        }
        else if (this.mc.isIntegratedServerRunning())
        {
            MinecraftServer server = this.mc.getServer();
            World worldTmp = server.getWorld(dimension);
            return worldTmp != null;
        }

        return false;
    }

    public long getWorldSeed(DimensionType dimension)
    {
        if (this.worldSeedValid == false && this.mc.isIntegratedServerRunning())
        {
            MinecraftServer server = this.mc.getServer();
            World worldTmp = server.getWorld(dimension);

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

    public boolean isServerTPSValid()
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

    public boolean onSendChatMessage(PlayerEntity player, String message)
    {
        String[] parts = message.split(" ");

        if (parts[0].equals("minihud-seed"))
        {
            if (parts.length == 2)
            {
                try
                {
                    this.setWorldSeed(Long.parseLong(parts[1]));
                    InfoUtils.printActionbarMessage("minihud.message.seed_set", Long.valueOf(this.worldSeed));
                }
                catch (NumberFormatException e)
                {
                    InfoUtils.printActionbarMessage("minihud.message.error.invalid_seed");
                }
            }
            else if (this.worldSeedValid && parts.length == 1)
            {
                InfoUtils.printActionbarMessage("minihud.message.seed_set", Long.valueOf(this.worldSeed));
            }

            return true;
        }

        return false;
    }

    public void onChatMessage(Text message)
    {
        if (message instanceof TranslatableText)
        {
            TranslatableText text = (TranslatableText) message;

            // The vanilla "/seed" command
            if ("commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    String str = text.getString();
                    int i1 = str.indexOf("[");
                    int i2 = str.indexOf("]");

                    if (i1 != -1 && i2 != -1)
                    {
                        this.setWorldSeed(Long.parseLong(str.substring(i1 + 1, i2)));
                        MiniHUD.logger.info("Received world seed from the vanilla /seed command: {}", this.worldSeed);
                        InfoUtils.printActionbarMessage("minihud.message.seed_set", Long.valueOf(this.worldSeed));
                    }
                }
                catch (Exception e)
                {
                    MiniHUD.logger.warn("Failed to read the world seed from '{}'", text.getArgs()[0], e);
                }
            }
            // The "/jed seed" command
            else if ("jed.commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    this.setWorldSeed(Long.parseLong(text.getArgs()[1].toString()));
                    MiniHUD.logger.info("Received world seed from the JED '/jed seed' command: {}", this.worldSeed);
                    InfoUtils.printActionbarMessage("minihud.message.seed_set", Long.valueOf(this.worldSeed));
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

            if (this.serverTPSValid)
            {
                long elapsedTicks = totalWorldTime - this.lastServerTick;

                if (elapsedTicks > 0)
                {
                    this.serverMSPT = ((double) (currentTime - this.lastServerTimeUpdate) / (double) elapsedTicks) / 1000000D;
                    this.serverTPS = this.serverMSPT <= 50 ? 20D : (1000D / this.serverMSPT);
                }
            }

            this.lastServerTick = totalWorldTime;
            this.lastServerTimeUpdate = currentTime;
            this.serverTPSValid = true;
        }
    }

    public void updateIntegratedServerTPS()
    {
        if (this.mc != null && this.mc.player != null && this.mc.getServer() != null)
        {
            this.serverMSPT = (double) MathHelper.average(this.mc.getServer().lastTickLengths) / 1000000D;
            this.serverTPS = this.serverMSPT <= 50 ? 20D : (1000D / this.serverMSPT);
            this.serverTPSValid = true;
        }
    }

    /**
     * Gets a copy of the structure data map, and clears the dirty flag
     * @return
     */
    public ArrayListMultimap<StructureType, StructureData> getCopyOfStructureData()
    {
        ArrayListMultimap<StructureType, StructureData> copy = ArrayListMultimap.create();

        synchronized (this.structures)
        {
            for (StructureType type : StructureType.values())
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
                    BlockPos playerPos = PositionUtils.getEntityBlockPos(this.mc.player);

                    if (this.structuresNeedUpdating(playerPos, 32))
                    {
                        this.updateStructureDataFromIntegratedServer(playerPos);
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
                        // (re-)register the structure packet handler
                        ClientPacketChannelHandler.getInstance().unregisterClientChannelHandler(StructurePacketHandler.INSTANCE);
                        ClientPacketChannelHandler.getInstance().registerClientChannelHandler(StructurePacketHandler.INSTANCE);
                    }

                    this.shouldRegisterStructureChannel = false;
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
        final DimensionType dimension = this.mc.player.dimension;
        final ServerWorld world = this.mc.getServer().getWorld(dimension);

        if (world != null)
        {
            MinecraftServer server = this.mc.getServer();
            final int maxChunkRange = this.mc.options.viewDistance + 2;

            server.send(new ServerTask(server.getTicks(), () ->
            {
                synchronized (this.structures)
                {
                    this.addStructureDataFromGenerator(world, dimension, playerPos, maxChunkRange);
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

    public void addOrUpdateStructuresFromServer(ListTag structures, int timeout)
    {
        if (structures.getElementType() == Constants.NBT.TAG_COMPOUND)
        {
            this.structureDataTimeout = timeout;

            long currentTime = this.mc.world.getTime();
            final int count = structures.size();

            this.removeExpiredStructures(currentTime, timeout);

            for (int i = 0; i < count; ++i)
            {
                CompoundTag tag = structures.getCompound(i);
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
        long maxAge = timeout + 200;
        Iterator<StructureData> iter = this.structures.values().iterator();

        while (iter.hasNext())
        {
            StructureData data = iter.next();

            if (currentTime > (data.getRefreshTime() + maxAge))
            {
                iter.remove();
            }
        }
    }

    private void addStructureDataFromGenerator(ServerWorld world, DimensionType dimensionType, BlockPos playerPos, int maxChunkRange)
    {
        this.structures.clear();

        List<StructureType> enabledTypes = new ArrayList<>();

        for (StructureType type : StructureType.values())
        {
            if (type.isEnabled() && type.existsInDimension(dimensionType))
            {
                enabledTypes.add(type);
            }
        }

        if (enabledTypes.isEmpty() == false)
        {
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

                    if (chunk != null)
                    {
                        for (StructureType type : enabledTypes)
                        {
                            StructureStart start = chunk.getStructureStart(type.getStructureName());

                            if (start != null)
                            {
                                if (MiscUtils.isStructureWithinRange(start.getBoundingBox(), playerPos, maxChunkRange << 4))
                                {
                                    this.structures.put(type, StructureData.fromStructureStart(type, start));
                                }
                            }
                        }
                    }
                }
            }
        }

        this.structureRendererNeedsUpdate = true;

        //MiniHUD.logger.info("Structure data updated from the integrated server");
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
                    catch (NumberFormatException e)
                    {
                    }
                }
            }
        }

        this.serverTPSValid = false;
    }

    public JsonObject toJson()
    {
        JsonObject obj = new JsonObject();

        obj.add("distance_pos", JsonUtils.vec3dToJson(this.distanceReferencePoint));

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
    }
}
