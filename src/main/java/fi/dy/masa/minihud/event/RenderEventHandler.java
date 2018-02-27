package fi.dy.masa.minihud.event;

import java.lang.invoke.MethodHandle;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map.Entry;
import java.util.Random;
import net.minecraft.block.Block;
import net.minecraft.block.properties.IProperty;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.RenderGlobal;
import net.minecraft.client.renderer.chunk.RenderChunk;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextComponentTranslation;
import net.minecraft.util.text.TextFormatting;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.World;
import net.minecraft.world.WorldType;
import net.minecraft.world.chunk.Chunk;
import net.minecraftforge.client.event.ClientChatReceivedEvent;
import net.minecraftforge.client.event.RenderGameOverlayEvent;
import net.minecraftforge.client.event.RenderGameOverlayEvent.ElementType;
import net.minecraftforge.common.DimensionManager;
import net.minecraftforge.event.world.WorldEvent;
import net.minecraftforge.fml.common.FMLCommonHandler;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import fi.dy.masa.minihud.MiniHud;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.util.MethodHandleUtils;
import fi.dy.masa.minihud.util.MethodHandleUtils.UnableToFindMethodHandleException;

public class RenderEventHandler
{
    public static final int MASK_FPS                        = 0x00000001;
    public static final int MASK_TIME_REAL                  = 0x00000002;
    public static final int MASK_TIME_TICKS                 = 0x00000004;
    public static final int MASK_TIME_MC                    = 0x00000008;
    public static final int MASK_COORDINATES                = 0x00000010;
    public static final int MASK_DIMENSION                  = 0x00000020;
    public static final int MASK_BLOCK                      = 0x00000040;
    public static final int MASK_CHUNK                      = 0x00000080;
    public static final int MASK_FACING                     = 0x00000100;
    public static final int MASK_LIGHT                      = 0x00000200;
    public static final int MASK_YAW                        = 0x00000400;
    public static final int MASK_PITCH                      = 0x00000800;
    public static final int MASK_SPEED                      = 0x00001000;
    public static final int MASK_CHUNK_SECTIONS             = 0x00002000;
    public static final int MASK_CHUNK_SECTIONS_LINE        = 0x00004000;
    public static final int MASK_CHUNK_UPDATES              = 0x00008000;
    public static final int MASK_PARTICLE_COUNT             = 0x00010000;
    public static final int MASK_DIFFICULTY                 = 0x00020000;
    public static final int MASK_BIOME                      = 0x00040000;
    public static final int MASK_BIOME_REGISTRY_NAME        = 0x00080000;
    public static final int MASK_ENTITIES                   = 0x00100000;
    public static final int MASK_SLIME_CHUNK                = 0x00200000;
    public static final int MASK_LOOKING_AT_ENTITY          = 0x00400000;
    public static final int MASK_LOOKING_AT_ENTITY_REGNAME  = 0x00800000;
    public static final int MASK_LOOKING_AT_BLOCK           = 0x01000000;
    public static final int MASK_LOOKING_AT_BLOCK_CHUNK     = 0x02000000;
    public static final int MASK_BLOCK_PROPERTIES           = 0x04000000;

    private static RenderEventHandler instance;
    private final MethodHandle methodHandle_RenderGlobal_getRenderedChunks;
    private final Minecraft mc;
    private final Date date;
    private final Random rand = new Random();
    private boolean enabled;
    private int mask;
    private int fps;
    private int fpsCounter;
    private long fpsUpdateTime = Minecraft.getSystemTime();
    private long infoUpdateTime;
    private long serverSeed;
    private boolean serverSeedValid;
    private int addedTypes;
    private final List<StringHolder> lines = new ArrayList<StringHolder>();

    public RenderEventHandler()
    {
        this.mc = Minecraft.getMinecraft();
        this.date = new Date();
        this.methodHandle_RenderGlobal_getRenderedChunks = this.getMethodHandle_getRenderedChunks();
    }

    private MethodHandle getMethodHandle_getRenderedChunks()
    {
        try
        {
            return MethodHandleUtils.getMethodHandleVirtual(RenderGlobal.class, new String[] { "func_184382_g", "getRenderedChunks" });
        }
        catch (UnableToFindMethodHandleException e)
        {
            MiniHud.logger.error("Failed to get a MethodHandle for RenderGlobal#getRenderedChunks()", e);
            return null;
        }
    }

    @SubscribeEvent
    public void onRenderGameOverlay(RenderGameOverlayEvent.Post event)
    {
        if (this.enabled == false || event.getType() != ElementType.ALL ||
            this.mc.gameSettings.showDebugInfo || this.mc.player == null ||
            (Configs.requireSneak && this.mc.player.isSneaking() == false) ||
            (Configs.requireHoldingKey && InputEventHandler.isRequiredKeyActive(Configs.requiredKey) == false))
        {
            return;
        }

        if ((this.mask & MASK_FPS) != 0)
        {
            this.updateFps();
        }

        long currentTime = System.currentTimeMillis();

        // Only update the text once per game tick
        if (currentTime - this.infoUpdateTime >= 50)
        {
            this.updateLines(this.mask);
            this.infoUpdateTime = currentTime;
        }

        this.renderText(Configs.textPosX, Configs.textPosY, this.lines);
    }

    @SubscribeEvent
    public void onWorldLoad(WorldEvent.Load event)
    {
        this.serverSeedValid = false;
    }

    @SubscribeEvent
    public void onChatMessage(ClientChatReceivedEvent event)
    {
        ITextComponent message = event.getMessage();

        if (message instanceof TextComponentTranslation)
        {
            TextComponentTranslation text = (TextComponentTranslation) message;

            // The vanilla "/seed" command
            if ("commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    this.serverSeed = Long.parseLong(text.getFormatArgs()[0].toString());
                    this.serverSeedValid = true;
                    MiniHud.logger.info("Received world seed from the vanilla /seed command: {}", this.serverSeed);
                }
                catch (Exception e)
                {
                    MiniHud.logger.warn("Failed to read the world seed from '{}'", text.getFormatArgs()[0], e);
                }
            }
            // The "/jed seed" command
            else if ("jed.commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    this.serverSeed = Long.parseLong(text.getFormatArgs()[1].toString());
                    this.serverSeedValid = true;
                    MiniHud.logger.info("Received world seed from the JED '/jed seed' command: {}", this.serverSeed);
                }
                catch (Exception e)
                {
                    MiniHud.logger.warn("Failed to read the world seed from '{}'", text.getFormatArgs()[1], e);
                }
            }
        }
    }

    public static RenderEventHandler getInstance()
    {
        if (instance == null)
        {
            instance = new RenderEventHandler();
        }

        return instance;
    }

    public void setEnabledMask(int mask)
    {
        this.mask = mask;
    }

    public void xorEnabledMask(int mask)
    {
        this.mask ^= mask;
    }

    public void setEnabled(boolean enabled)
    {
        this.enabled = enabled;
    }

    public void toggleEnabled()
    {
        this.enabled = ! this.enabled;
    }

    private void updateFps()
    {
        this.fpsCounter++;

        if (Minecraft.getSystemTime() >= (this.fpsUpdateTime + 1000L))
        {
            this.fpsUpdateTime = Minecraft.getSystemTime();
            this.fps = this.fpsCounter;
            this.fpsCounter = 0;
        }
    }

    private void updateLines(int enabledMask)
    {
        this.lines.clear();
        this.addedTypes = 0;

        // Get the info line order based on the configs
        List<LinePos> positions = new ArrayList<LinePos>();

        for (int i = 0; i < 32; i++)
        {
            int mask = (1 << i);

            if ((enabledMask & mask) != 0)
            {
                positions.add(new LinePos(Configs.getLinePositionFor(mask), mask));
            }
        }

        Collections.sort(positions);

        for (LinePos pos : positions)
        {
            this.addLine(pos.type);
        }

        if (Configs.sortLinesByLength)
        {
            Collections.sort(this.lines);

            if (Configs.sortLinesReversed)
            {
                Collections.reverse(this.lines);
            }
        }
    }

    private void addLine(String text)
    {
        this.lines.add(new StringHolder(text));
    }

    private void addLine(int type)
    {
        Entity entity = this.mc.getRenderViewEntity();
        BlockPos pos = new BlockPos(entity.posX, entity.getEntityBoundingBox().minY, entity.posZ);

        switch (type)
        {
            case MASK_FPS:
                this.addLine(String.format("%d fps", this.fps));
                break;

            case MASK_TIME_REAL:
                try
                {
                    SimpleDateFormat sdf = new SimpleDateFormat(Configs.dateFormatReal);
                    this.date.setTime(System.currentTimeMillis());
                    this.addLine(sdf.format(this.date));
                }
                catch (Exception e)
                {
                    this.addLine("Date formatting failed - Invalid date format string?");
                }
                break;

            case MASK_TIME_TICKS:
                long current = entity.getEntityWorld().getWorldTime();
                long total = entity.getEntityWorld().getTotalWorldTime();
                this.addLine(String.format("World time: %5d - total: %d", current, total));
                break;

            case MASK_TIME_MC:
                try
                {
                    long timeDay = (int) entity.getEntityWorld().getWorldTime();
                    int day = (int) (timeDay / 24000) + 1;
                    // 1 tick = 3.6 seconds in MC (0.2777... seconds IRL)
                    int hour = (int) ((timeDay / 1000) + 6) % 24;
                    int min = (int) (timeDay / 16.666666) % 60;
                    int sec = (int) (timeDay / 0.277777) % 60;

                    String str = Configs.dateFormatMinecraft;
                    str = str.replace("{DAY}",  String.format("%d", day));
                    str = str.replace("{HOUR}", String.format("%02d", hour));
                    str = str.replace("{MIN}",  String.format("%02d", min));
                    str = str.replace("{SEC}",  String.format("%02d", sec));

                    this.addLine(str);
                }
                catch (Exception e)
                {
                    this.addLine("Date formatting failed - Invalid date format string?");
                }
                break;

            case MASK_COORDINATES:
            case MASK_DIMENSION:
            {
                // Don't add the same line multiple times
                if ((this.addedTypes & (MASK_COORDINATES | MASK_DIMENSION)) != 0)
                {
                    break;
                }

                String pre = "";
                StringBuilder str = new StringBuilder(128);

                if ((this.mask & MASK_COORDINATES) != 0)
                {
                    if (Configs.coordinateFormatCustomized)
                    {
                        try
                        {
                            str.append(String.format(Configs.coordinateFormat,
                                entity.posX, entity.getEntityBoundingBox().minY, entity.posZ));
                        }
                        // Uh oh, someone done goofed their format string... :P
                        catch (Exception e)
                        {
                            str.append("broken coordinate format string!");
                        }
                    }
                    else
                    {
                        str.append(String.format("XYZ: %.2f / %.4f / %.2f",
                            entity.posX, entity.getEntityBoundingBox().minY, entity.posZ));
                    }

                    pre = " / ";
                }

                if ((this.mask & MASK_DIMENSION) != 0)
                {
                    str.append(String.format(String.format("%sdim: %d", pre, entity.getEntityWorld().provider.getDimension())));
                }

                this.addLine(str.toString());
                this.addedTypes |= (MASK_COORDINATES | MASK_DIMENSION);
                break;
            }

            case MASK_BLOCK:
                this.addLine(String.format("Block: %d / %d / %d", pos.getX(), pos.getY(), pos.getZ()));
                break;

            case MASK_CHUNK:
                this.addLine(String.format("Block: %d %d %d in Chunk: %d %d %d",
                        pos.getX() & 0xF, pos.getY() & 0xF, pos.getZ() & 0xF,
                        pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4));
                break;

            case MASK_FACING:
            {
                EnumFacing facing = entity.getHorizontalFacing();
                String str = "Invalid";

                switch (facing)
                {
                    case NORTH: str = "Negative Z"; break;
                    case SOUTH: str = "Positive Z"; break;
                    case WEST:  str = "Negative X"; break;
                    case EAST:  str = "Positive X"; break;
                    default:
                }

                this.addLine(String.format("Facing: %s (%s)", facing, str));
                break;
            }

            case MASK_LIGHT:
                // Prevent a crash when outside of world
                if (pos.getY() >= 0 && pos.getY() < 256 && this.mc.world.isBlockLoaded(pos))
                {
                    Chunk chunk = this.mc.world.getChunkFromBlockCoords(pos);

                    if (chunk.isEmpty() == false)
                    {
                        this.addLine(String.format("Light: %d (block: %d, sky: %d)",
                                chunk.getLightSubtracted(pos, 0),
                                chunk.getLightFor(EnumSkyBlock.BLOCK, pos),
                                chunk.getLightFor(EnumSkyBlock.SKY, pos)));
                    }
                }
                break;

            case MASK_YAW:
            case MASK_PITCH:
            case MASK_SPEED:
            {
                // Don't add the same line multiple times
                if ((this.addedTypes & (MASK_YAW | MASK_PITCH | MASK_SPEED)) != 0)
                {
                    break;
                }

                String pre = "";
                StringBuilder str = new StringBuilder(128);

                if ((this.mask & MASK_YAW) != 0)
                {
                    str.append(String.format("%syaw: %.1f", pre, MathHelper.wrapDegrees(entity.rotationYaw)));
                    pre = " / ";
                }

                if ((this.mask & MASK_PITCH) != 0)
                {
                    str.append(String.format("%spitch: %.1f", pre, MathHelper.wrapDegrees(entity.rotationPitch)));
                    pre = " / ";
                }

                if ((this.mask & MASK_SPEED) != 0)
                {
                    double dx = entity.posX - entity.lastTickPosX;
                    double dy = entity.posY - entity.lastTickPosY;
                    double dz = entity.posZ - entity.lastTickPosZ;
                    double dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
                    str.append(String.format("%sspeed: %.3f m/s", pre, dist * 20));
                    pre = " / ";
                }

                this.addLine(str.toString());
                this.addedTypes |= (MASK_YAW | MASK_PITCH | MASK_SPEED);
                break;
            }

            case MASK_CHUNK_SECTIONS:
                this.addLine(String.format("C: %d", this.getRenderedChunks()));
                break;

            case MASK_CHUNK_SECTIONS_LINE:
                this.addLine(this.mc.renderGlobal.getDebugInfoRenders());
                break;

            case MASK_CHUNK_UPDATES:
                this.addLine(String.format("Chunk updates: %d", RenderChunk.renderChunksUpdated));
                break;

            case MASK_PARTICLE_COUNT:
                this.addLine(String.format("P: %s", this.mc.effectRenderer.getStatistics()));
                break;

            case MASK_DIFFICULTY:
                if (this.mc.world.isBlockLoaded(pos))
                {
                    DifficultyInstance diff = this.mc.world.getDifficultyForLocation(pos);

                    if (this.mc.isIntegratedServerRunning() && this.mc.getIntegratedServer() != null)
                    {
                        EntityPlayerMP player = this.mc.getIntegratedServer().getPlayerList().getPlayerByUUID(this.mc.player.getUniqueID());

                        if (player != null)
                        {
                            diff = player.world.getDifficultyForLocation(new BlockPos(player));
                        }
                    }

                    this.addLine(String.format("Local Difficulty: %.2f // %.2f (Day %d)",
                            diff.getAdditionalDifficulty(), diff.getClampedAdditionalDifficulty(), this.mc.world.getWorldTime() / 24000L));
                }
                break;

            case MASK_BIOME:
                // Prevent a crash when outside of world
                if (pos.getY() >= 0 && pos.getY() < 256 && this.mc.world.isBlockLoaded(pos))
                {
                    Chunk chunk = this.mc.world.getChunkFromBlockCoords(pos);

                    if (chunk.isEmpty() == false)
                    {
                        this.addLine("Biome: " + chunk.getBiome(pos, this.mc.world.getBiomeProvider()).getBiomeName());
                    }
                }
                break;

            case MASK_BIOME_REGISTRY_NAME:
                // Prevent a crash when outside of world
                if (pos.getY() >= 0 && pos.getY() < 256 && this.mc.world.isBlockLoaded(pos))
                {
                    Chunk chunk = this.mc.world.getChunkFromBlockCoords(pos);

                    if (chunk.isEmpty() == false)
                    {
                        this.addLine("Biome reg name: " +
                                chunk.getBiome(pos, this.mc.world.getBiomeProvider()).getRegistryName().toString());
                    }
                }
                break;

            case MASK_ENTITIES:
                String ent = this.mc.renderGlobal.getDebugInfoEntities();

                int p = ent.indexOf(",");

                if (p != -1)
                {
                    ent = ent.substring(0, p);
                }

                this.addLine(ent);
                break;

            case MASK_SLIME_CHUNK:
                boolean valid = false;
                long seed = 0;
                String result;
                MinecraftServer server = FMLCommonHandler.instance().getMinecraftServerInstance();

                if (server != null && server.isSinglePlayer())
                {
                    World world = DimensionManager.getWorld(entity.getEntityWorld().provider.getDimension());
                    seed = world != null ? world.getSeed() : 0;
                    valid = world != null;
                }
                else if (this.serverSeedValid)
                {
                    seed = this.serverSeed;
                    valid = true;
                }

                if (valid)
                {
                    if (this.canSlimeSpawnAt(entity.posX, entity.posZ, seed))
                    {
                        result = TextFormatting.GREEN.toString() + "YES" + TextFormatting.RESET.toString() + TextFormatting.WHITE.toString();
                    }
                    else
                    {
                        result = TextFormatting.RED.toString() + "NO" + TextFormatting.RESET.toString() + TextFormatting.WHITE.toString();
                    }
                }
                else
                {
                    result = "<world seed not known>";
                }

                this.addLine("Slime chunk: " + result);
                break;

            case MASK_LOOKING_AT_ENTITY:
                if (this.mc.objectMouseOver != null &&
                    this.mc.objectMouseOver.typeOfHit == RayTraceResult.Type.ENTITY &&
                    this.mc.objectMouseOver.entityHit != null)
                {
                    Entity target = this.mc.objectMouseOver.entityHit;

                    if (target instanceof EntityLivingBase)
                    {
                        EntityLivingBase living = (EntityLivingBase) target;
                        this.addLine(String.format("Entity: %s - HP: %.1f / %.1f",
                                target.getName(), living.getHealth(), living.getMaxHealth()));
                    }
                    else
                    {
                        this.addLine(String.format("Entity: %s", target.getName()));
                    }
                }
                break;

            case MASK_LOOKING_AT_ENTITY_REGNAME:
                if (this.mc.objectMouseOver != null &&
                    this.mc.objectMouseOver.typeOfHit == RayTraceResult.Type.ENTITY &&
                    this.mc.objectMouseOver.entityHit != null)
                {
                    ResourceLocation regName = EntityList.getKey(this.mc.objectMouseOver.entityHit);

                    if (regName != null)
                    {
                        this.addLine(String.format("Entity reg name: %s", regName.toString()));
                    }
                }
                break;

            case MASK_LOOKING_AT_BLOCK:
            case MASK_LOOKING_AT_BLOCK_CHUNK:
                // Don't add the same line multiple times
                if ((this.addedTypes & (MASK_LOOKING_AT_BLOCK | MASK_LOOKING_AT_BLOCK_CHUNK)) != 0)
                {
                    break;
                }

                if (this.mc.objectMouseOver != null &&
                    this.mc.objectMouseOver.typeOfHit == RayTraceResult.Type.BLOCK &&
                    this.mc.objectMouseOver.getBlockPos() != null)
                {
                    BlockPos lookPos = this.mc.objectMouseOver.getBlockPos();
                    String pre = "";
                    StringBuilder str = new StringBuilder(128);

                    if ((this.mask & MASK_LOOKING_AT_BLOCK) != 0)
                    {
                        str.append(String.format("Looking at block: %d %d %d", lookPos.getX(), lookPos.getY(), lookPos.getZ()));
                        pre = " // ";
                    }

                    if ((this.mask & MASK_LOOKING_AT_BLOCK_CHUNK) != 0)
                    {
                        str.append(pre).append(String.format("Block: %d %d %d within chunk section: %d %d %d",
                                lookPos.getX() & 0xF, lookPos.getY() & 0xF, lookPos.getZ() & 0xF,
                                lookPos.getX() >> 4, lookPos.getY() >> 4, lookPos.getZ() >> 4));
                    }

                    this.addLine(str.toString());
                    this.addedTypes |= (MASK_LOOKING_AT_BLOCK | MASK_LOOKING_AT_BLOCK_CHUNK);
                }
                break;

            case MASK_BLOCK_PROPERTIES:
                this.getBlockProperties();
                break;
        }
    }

    @SuppressWarnings("unchecked")
    private <T extends Comparable<T>> void getBlockProperties()
    {
        if (this.mc.objectMouseOver != null &&
                this.mc.objectMouseOver.typeOfHit == RayTraceResult.Type.BLOCK &&
                this.mc.objectMouseOver.getBlockPos() != null)
        {
            BlockPos posLooking = this.mc.objectMouseOver.getBlockPos();
            IBlockState state = this.mc.world.getBlockState(posLooking);

            if (this.mc.world.getWorldType() != WorldType.DEBUG_ALL_BLOCK_STATES)
            {
                state = state.getActualState(this.mc.world, posLooking);
            }

            this.lines.add(new StringHolder(String.valueOf(Block.REGISTRY.getNameForObject(state.getBlock()))));

            for (Entry <IProperty<?>, Comparable<?>> entry : state.getProperties().entrySet())
            {
                IProperty<T> property = (IProperty<T>) entry.getKey();
                T value = (T) entry.getValue();
                String valueName = property.getName(value);

                if (Boolean.TRUE.equals(value))
                {
                    valueName = TextFormatting.GREEN + valueName;
                }
                else if (Boolean.FALSE.equals(value))
                {
                    valueName = TextFormatting.RED + valueName;
                }
                else if (Integer.class.equals(property.getValueClass()))
                {
                    valueName = TextFormatting.AQUA + valueName;
                }

                this.lines.add(new StringHolder(property.getName() + ": " + valueName));
            }
        }
    }

    private void renderText(int xOff, int yOff, List<StringHolder> lines)
    {
        boolean scale = Configs.useScaledFont;

        if (scale)
        {
            GlStateManager.pushMatrix();
            GlStateManager.scale(0.5, 0.5, 0.5);
        }

        FontRenderer fontRenderer = this.mc.fontRenderer;

        for (StringHolder holder : lines)
        {
            String line = holder.str;

            if (Configs.useTextBackground)
            {
                Gui.drawRect(xOff - 2, yOff - 2, xOff + fontRenderer.getStringWidth(line) + 2, yOff + fontRenderer.FONT_HEIGHT, Configs.textBackgroundColor);
            }

            if (Configs.useFontShadow)
            {
                fontRenderer.drawStringWithShadow(line, xOff, yOff, Configs.fontColor);
            }
            else
            {
                fontRenderer.drawString(line, xOff, yOff, Configs.fontColor);
            }

            yOff += fontRenderer.FONT_HEIGHT + 2;
        }

        if (scale)
        {
            GlStateManager.popMatrix();
        }
    }

    private boolean canSlimeSpawnAt(double posX, double posZ, long worldSeed)
    {
        int chunkX = (int) Math.floor(posX / 16d);
        int chunkZ = (int) Math.floor(posZ / 16d);
        long slimeSeed = 987234911L;
        long rngSeed = worldSeed +
                       (long) (chunkX * chunkX *  4987142) + (long) (chunkX * 5947611) +
                       (long) (chunkZ * chunkZ) * 4392871L + (long) (chunkZ * 389711) ^ slimeSeed;

        this.rand.setSeed(rngSeed);

        return this.rand.nextInt(10) == 0;
    }

    private int getRenderedChunks()
    {
        try
        {
            return (int) methodHandle_RenderGlobal_getRenderedChunks.invokeExact(this.mc.renderGlobal);
        }
        catch (Throwable t)
        {
            MiniHud.logger.error("Error while trying invoke RenderGlobal#getRenderedChunks()", t);
            return -1;
        }
    }

    private class StringHolder implements Comparable<StringHolder>
    {
        public final String str;

        public StringHolder(String str)
        {
            this.str = str;
        }

        @Override
        public int compareTo(StringHolder other)
        {
            int lenThis = this.str.length();
            int lenOther = other.str.length();

            if (lenThis == lenOther)
            {
                return 0;
            }

            return this.str.length() > other.str.length() ? -1 : 1;
        }
    }

    private static class LinePos implements Comparable<LinePos>
    {
        private final int position;
        private final int type;

        private LinePos(int position, int type)
        {
            this.position = position;
            this.type = type;
        }

        @Override
        public int compareTo(LinePos other)
        {
            if (this.position < 0)
            {
                return other.position >= 0 ? 1 : 0;
            }
            else if (other.position < 0 && this.position >= 0)
            {
                return -1;
            }

            return this.position < other.position ? -1 : (this.position > other.position ? 1 : 0);
        }
    }
}
