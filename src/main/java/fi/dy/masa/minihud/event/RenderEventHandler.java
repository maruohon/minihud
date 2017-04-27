package fi.dy.masa.minihud.event;

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
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextComponentTranslation;
import net.minecraft.util.text.TextFormatting;
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

public class RenderEventHandler
{
    public static final int MASK_COORDINATES    = 0x00000001;
    public static final int MASK_YAW            = 0x00000002;
    public static final int MASK_PITCH          = 0x00000004;
    public static final int MASK_SPEED          = 0x00000008;
    public static final int MASK_BIOME          = 0x00000010;
    public static final int MASK_LIGHT          = 0x00000020;
    public static final int MASK_FACING         = 0x00000040;
    public static final int MASK_BLOCK          = 0x00000080;
    public static final int MASK_CHUNK          = 0x00000100;
    public static final int MASK_LOOKINGAT      = 0x00000200;
    public static final int MASK_FPS            = 0x00000400;
    public static final int MASK_ENTITIES       = 0x00000800;
    public static final int MASK_DIMENSION      = 0x00001000;
    public static final int MASK_TIME_TICKS     = 0x00002000;
    public static final int MASK_TIME_MC        = 0x00004000;
    public static final int MASK_TIME_REAL      = 0x00008000;
    public static final int MASK_LOOKING_AT_ENTITY  = 0x00010000;
    public static final int MASK_SLIME_CHUNK        = 0x00020000;
    public static final int MASK_BLOCK_PROPERTIES   = 0x00040000;

    private static RenderEventHandler instance;
    private final Minecraft mc;
    private final Date date;
    private boolean enabled;
    private int mask;
    private int fps;
    private int fpsCounter;
    private long fpsUpdateTime = Minecraft.getSystemTime();
    private float partialTicksLast;
    private long serverSeed;
    private boolean serverSeedValid;
    List<StringHolder> lines = new ArrayList<StringHolder>();

    public RenderEventHandler()
    {
        this.mc = Minecraft.getMinecraft();
        this.date = new Date();
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

        // Only update the text once per game tick
        if (event.getPartialTicks() < this.partialTicksLast)
        {
            this.lines = this.getLines(this.mask);
        }

        this.renderText(Configs.textPosX, Configs.textPosY, this.lines);
        this.partialTicksLast = event.getPartialTicks();
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

    private List<StringHolder> getLines(int enabledMask)
    {
        List<StringHolder> lines = new ArrayList<StringHolder>();
        Entity entity = this.mc.getRenderViewEntity();
        BlockPos pos = new BlockPos(entity.posX, entity.getEntityBoundingBox().minY, entity.posZ);

        if ((enabledMask & MASK_TIME_REAL) != 0)
        {
            try
            {
                SimpleDateFormat sdf = new SimpleDateFormat(Configs.dateFormatReal);
                this.date.setTime(System.currentTimeMillis());
                lines.add(new StringHolder(sdf.format(this.date)));
            }
            catch (Exception e)
            {
                lines.add(new StringHolder("Date formatting failed - Invalid date format string?"));
            }
        }

        if ((enabledMask & MASK_FPS) != 0)
        {
            lines.add(new StringHolder(String.format("%d fps", this.fps)));
        }

        int coordsDim = enabledMask & (MASK_COORDINATES | MASK_DIMENSION);

        if (coordsDim != 0)
        {
            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if ((coordsDim & MASK_COORDINATES) != 0)
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

            if ((coordsDim & MASK_DIMENSION) != 0)
            {
                str.append(String.format(String.format("%sdim: %d", pre, entity.getEntityWorld().provider.getDimension())));
            }

            lines.add(new StringHolder(str.toString()));
        }

        if ((enabledMask & MASK_TIME_TICKS) != 0)
        {
            long current = entity.getEntityWorld().getWorldTime();
            long total = entity.getEntityWorld().getTotalWorldTime();
            lines.add(new StringHolder(String.format("World time: %5d - total: %d", current, total)));
        }

        if ((enabledMask & MASK_TIME_MC) != 0)
        {
            try
            {
                long timeDay = (int) entity.getEntityWorld().getWorldTime();
                int day = (int) (timeDay / 24000) + 1;
                // 1 tick = 3.6 seconds in MC (0.2777... seconds IRL)
                int hour = (int) ((timeDay / 1000) + 6) % 24;
                int min = (int) (timeDay / 16.666666) % 60;
                int sec = (int) (timeDay / 0.277777) % 60;

                lines.add(new StringHolder(String.format("Time: %02d:%02d:%02d (day %d)", hour, min, sec, day)));
            }
            catch (Exception e)
            {
                lines.add(new StringHolder("Date formatting failed - Invalid date format string?"));
            }
        }

        if ((enabledMask & MASK_BLOCK) != 0)
        {
            lines.add(new StringHolder(String.format("Block: %d / %d / %d", pos.getX(), pos.getY(), pos.getZ())));
        }

        if ((enabledMask & MASK_CHUNK) != 0)
        {
            lines.add(new StringHolder(String.format("Block: %d %d %d in Chunk: %d %d %d",
                    pos.getX() & 0xF, pos.getY() & 0xF, pos.getZ() & 0xF,
                    pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4)));
        }

        int yawPitchSpeed = enabledMask & (MASK_PITCH | MASK_YAW | MASK_SPEED);

        if (yawPitchSpeed != 0)
        {
            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if ((yawPitchSpeed & MASK_YAW) != 0)
            {
                str.append(String.format("%syaw: %.1f", pre, MathHelper.wrapDegrees(entity.rotationYaw)));
                pre = " / ";
            }

            if ((yawPitchSpeed & MASK_PITCH) != 0)
            {
                str.append(String.format("%spitch: %.1f", pre, MathHelper.wrapDegrees(entity.rotationPitch)));
                pre = " / ";
            }

            if ((yawPitchSpeed & MASK_SPEED) != 0)
            {
                double dx = entity.posX - entity.lastTickPosX;
                double dy = entity.posY - entity.lastTickPosY;
                double dz = entity.posZ - entity.lastTickPosZ;
                double dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
                str.append(String.format("%sspeed: %.3f m/s", pre, dist * 20));
                pre = " / ";
            }

            lines.add(new StringHolder(str.toString()));
        }

        if ((enabledMask & MASK_FACING) != 0)
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

            lines.add(new StringHolder(String.format("Facing: %s (%s)", facing, str)));
        }

        if ((enabledMask & (MASK_BIOME | MASK_LIGHT)) != 0)
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && this.mc.world.isBlockLoaded(pos))
            {
                Chunk chunk = this.mc.world.getChunkFromBlockCoords(pos);

                if (chunk.isEmpty() == false)
                {
                    if ((enabledMask & MASK_BIOME) != 0)
                    {
                        lines.add(new StringHolder("Biome: " + chunk.getBiome(pos, this.mc.world.getBiomeProvider()).getBiomeName()));
                    }

                    if ((enabledMask & MASK_LIGHT) != 0)
                    {
                        lines.add(new StringHolder("Light: " + chunk.getLightSubtracted(pos, 0) +
                                " (" + chunk.getLightFor(EnumSkyBlock.SKY, pos) + " sky, " +
                                chunk.getLightFor(EnumSkyBlock.BLOCK, pos) + " block)"));
                    }
                }
            }
        }

        if ((enabledMask & MASK_ENTITIES) != 0)
        {
            String ent = this.mc.renderGlobal.getDebugInfoEntities();

            int p = ent.indexOf(",");
            if (p != -1)
            {
                ent = ent.substring(0, p);
            }

            lines.add(new StringHolder(ent));
        }

        if ((enabledMask & MASK_LOOKINGAT) != 0)
        {
            if (this.mc.objectMouseOver != null &&
                this.mc.objectMouseOver.typeOfHit == RayTraceResult.Type.BLOCK &&
                this.mc.objectMouseOver.getBlockPos() != null)
            {
                BlockPos lookPos = this.mc.objectMouseOver.getBlockPos();
                lines.add(new StringHolder(String.format("Looking at: %d %d %d", lookPos.getX(), lookPos.getY(), lookPos.getZ())));
            }
        }

        if ((enabledMask & MASK_LOOKING_AT_ENTITY) != 0)
        {
            if (this.mc.objectMouseOver != null &&
                this.mc.objectMouseOver.typeOfHit == RayTraceResult.Type.ENTITY &&
                this.mc.objectMouseOver.entityHit != null && this.mc.objectMouseOver.entityHit instanceof EntityLivingBase)
            {
                EntityLivingBase target = (EntityLivingBase) this.mc.objectMouseOver.entityHit;
                lines.add(new StringHolder(String.format("%s - HP: %.1f / %.1f", target.getName(), target.getHealth(), target.getMaxHealth())));
            }
        }

        if ((enabledMask & MASK_SLIME_CHUNK) != 0)
        {
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

            lines.add(new StringHolder("Slime chunk: " + result));
        }

        if ((enabledMask & MASK_BLOCK_PROPERTIES) != 0)
        {
            this.getBlockProperties(lines);
        }

        if (Configs.sortLinesByLength)
        {
            Collections.sort(lines);

            if (Configs.sortLinesReversed)
            {
                Collections.reverse(lines);
            }
        }

        return lines;
    }

    @SuppressWarnings("unchecked")
    private <T extends Comparable<T>> void getBlockProperties(List<StringHolder> lines)
    {
        if (this.mc.objectMouseOver != null &&
                this.mc.objectMouseOver.typeOfHit == RayTraceResult.Type.BLOCK &&
                this.mc.objectMouseOver.getBlockPos() != null)
        {
            BlockPos posLooking = this.mc.objectMouseOver.getBlockPos();
            IBlockState state = this.mc.world.getBlockState(posLooking);

            if (this.mc.world.getWorldType() != WorldType.DEBUG_WORLD)
            {
                state = state.getActualState(this.mc.world, posLooking);
            }

            lines.add(new StringHolder(String.valueOf(Block.REGISTRY.getNameForObject(state.getBlock()))));

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

                lines.add(new StringHolder(property.getName() + ": " + valueName));
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
        return (new Random(rngSeed)).nextInt(10) == 0;
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
}
