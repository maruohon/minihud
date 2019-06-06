package fi.dy.masa.minihud.util;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.annotation.Nullable;
import com.google.common.collect.ArrayListMultimap;
import com.google.gson.JsonObject;
import fi.dy.masa.malilib.util.FileUtils;
import fi.dy.masa.malilib.util.InfoUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.renderer.OverlayRendererLightLevel;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnableColumnHeights;
import net.minecraft.client.MinecraftClient;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.ServerTask;
import net.minecraft.server.world.ServerWorld;
import net.minecraft.structure.StructureStart;
import net.minecraft.text.TextComponent;
import net.minecraft.text.TextFormat;
import net.minecraft.text.TranslatableTextComponent;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraft.world.dimension.DimensionType;

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
    //private boolean hasStructureDataFromServer;
    private boolean structuresDirty;
    private boolean structuresNeedUpdating;
    private long worldSeed;
    private long lastServerTick;
    private long lastServerTimeUpdate;
    private BlockPos lastStructureUpdatePos;
    private double serverTPS;
    private double serverMSPT;
    private BlockPos worldSpawn = BlockPos.ORIGIN;
    private Vec3d distanceReferencePoint = Vec3d.ZERO;
    private final ArrayListMultimap<StructureType, StructureData> structures = ArrayListMultimap.create();
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
        //this.hasStructureDataFromServer = false;
        this.structuresDirty = false;

        this.lastStructureUpdatePos = null;
        this.structures.clear();
        this.worldSeed = 0;
        this.worldSpawn = BlockPos.ORIGIN;
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
        OverlayRendererSpawnableColumnHeights.markChunkChanged(chunkX, chunkZ);
        OverlayRendererLightLevel.setNeedsUpdate();
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

    public void onChatMessage(TextComponent message)
    {
        if (message instanceof TranslatableTextComponent)
        {
            TranslatableTextComponent text = (TranslatableTextComponent) message;

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
                    MiniHUD.logger.warn("Failed to read the world seed from '{}'", text.getParams()[0], e);
                }
            }
            // The "/jed seed" command
            else if ("jed.commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    this.setWorldSeed(Long.parseLong(text.getParams()[1].toString()));
                    MiniHUD.logger.info("Received world seed from the JED '/jed seed' command: {}", this.worldSeed);
                    InfoUtils.printActionbarMessage("minihud.message.seed_set", Long.valueOf(this.worldSeed));
                }
                catch (Exception e)
                {
                    MiniHUD.logger.warn("Failed to read the world seed from '{}'", text.getParams()[1], e);
                }
            }
            else if ("commands.setworldspawn.success".equals(text.getKey()) && text.getParams().length == 3)
            {
                try
                {
                    Object[] o = text.getParams();
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
                    MiniHUD.logger.warn("Failed to read the world spawn point from '{}'", text.getParams(), e);
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

            this.structuresDirty = false;
        }

        return copy;
    }

    public void requestStructureDataFromServer()
    {
    }

    public void updateStructureData()
    {
        if (this.mc != null && this.mc.world != null && this.mc.player != null)
        {
            final BlockPos playerPos = new BlockPos(this.mc.player);

            if (this.mc.isIntegratedServerRunning())
            {
                if (this.structuresNeedUpdating(playerPos, 32))
                {
                    this.updateStructureDataFromIntegratedServer(playerPos);
                }
            }
            else if (this.structuresNeedUpdating(playerPos, 256))
            {
                this.requestStructureDataFromServer();
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

        synchronized (this.structures)
        {
            this.structures.clear();
        }

        if (world != null)
        {
            MinecraftServer server = this.mc.getServer();
            final int maxRange = (this.mc.options.viewDistance) * 16;

            server.method_18858(new ServerTask(server.getTicks(), () ->
            {
                synchronized (this.structures)
                {
                    this.addStructureDataFromGenerator(world, dimension, playerPos, maxRange);
                }
            }));
        }

        this.lastStructureUpdatePos = playerPos;
        this.structuresNeedUpdating = false;
    }

    /*
    public void updateStructureDataFromServer(PacketBuffer data)
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
            MiniHUD.logger.warn("Failed to read structure data from Carpet mod packet", e);
        }
    }

    private void readStructureDataCarpetAll(NBTTagCompound nbt)
    {
        NBTTagList tagList = nbt.getList("Boxes", Constants.NBT.TAG_LIST);
        this.setWorldSeed(nbt.getLong("Seed"));

        synchronized (this.structures)
        {
            this.structures.clear();
            StructureData.readStructureDataCarpetAllBoxes(this.structures, tagList);
            this.hasStructureDataFromServer = true;
            this.structuresDirty = true;
            this.structuresNeedUpdating = false;

            EntityPlayer player = Minecraft.getInstance().player;

            if (player != null)
            {
                this.lastStructureUpdatePos = new BlockPos(player);
            }

            MiniHUD.logger.info("Structure data updated from Carpet server (all), structures: {}", this.structures.size());
        }
    }

    private void readStructureDataCarpetSplitHeader(NBTTagCompound nbt, int boxCount)
    {
        this.setWorldSeed(nbt.getLong("Seed"));

        synchronized (this.structures)
        {
            this.structures.clear();
            StructureData.readStructureDataCarpetIndividualBoxesHeader(boxCount);
        }

        MiniHUD.logger.info("Structure data header received from Carpet server, expecting {} boxes", boxCount);
    }

    private void readStructureDataCarpetSplitBoxes(PacketBuffer data, int boxCount) throws IOException
    {
        synchronized (this.structures)
        {
            for (int i = 0; i < boxCount; ++i)
            {
                NBTTagCompound nbt = data.readCompoundTag();
                StructureData.readStructureDataCarpetIndividualBoxes(this.structures, nbt);
            }

            this.hasStructureDataFromServer = true;
            this.structuresDirty = true;
            this.structuresNeedUpdating = false;

            EntityPlayer player = Minecraft.getInstance().player;

            if (player != null)
            {
                this.lastStructureUpdatePos = new BlockPos(player);
            }

            MiniHUD.logger.info("Structure data received from Carpet server (split boxes), received {} boxes", boxCount);
        }
    }
    */

    private void addStructureDataFromGenerator(ServerWorld world, DimensionType dimensionType, BlockPos playerPos, int maxRange)
    {
        for (StructureType type : StructureType.values())
        {
            if (type.isEnabled() && type.existsInDimension(dimensionType))
            {
                this.addStructuresWithinRange(type, world, playerPos, maxRange);
            }
        }

        this.structuresDirty = true;
        //MiniHUD.logger.info("Structure data updated from the integrated server");
    }

    private void addStructuresWithinRange(StructureType type, ServerWorld world, BlockPos playerPos, int maxRange)
    {
        int minCX = (playerPos.getX() >> 4) - (maxRange >> 4);
        int minCZ = (playerPos.getZ() >> 4) - (maxRange >> 4);
        int maxCX = (playerPos.getX() >> 4) + (maxRange >> 4);
        int maxCZ = (playerPos.getZ() >> 4) + (maxRange >> 4);
        HashMap<BlockPos, StructureStart> structures = new HashMap<>();

        for (int cz = minCZ; cz <= maxCZ; ++cz)
        {
            for (int cx = minCX; cx <= maxCX; ++cx)
            {
                Chunk chunk = world.getChunk(cx, cz);

                StructureStart start = chunk.getStructureStart(type.getStructureName());

                if (start != null)
                {
                    structures.put(start.getPos(), start);
                }
            }
        }

        for (StructureStart start : structures.values())
        {
            if (MiscUtils.isStructureWithinRange(start.getBoundingBox(), playerPos, maxRange))
            {
                this.structures.put(type, StructureData.fromStructure(start));
            }
        }
    }

    public void handleCarpetServerTPSData(TextComponent textComponent)
    {
        if (textComponent.getFormattedText().isEmpty() == false)
        {
            String text = TextFormat.stripFormatting(textComponent.getString());
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
