package fi.dy.masa.minihud.event;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map.Entry;
import java.util.Random;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.mixin.IMixinRenderGlobal;
import net.minecraft.block.Block;
import net.minecraft.block.properties.IProperty;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.renderer.GlStateManager;
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
import net.minecraft.world.biome.Biome;
import net.minecraft.world.chunk.Chunk;

public class RenderEventHandler
{
    private static final RenderEventHandler INSTANCE = new RenderEventHandler();
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
        this.date = new Date();
    }

    public static RenderEventHandler getInstance()
    {
        return INSTANCE;
    }

    public void onRenderGameOverlayPost(float partialTicks)
    {
        Minecraft mc = Minecraft.getMinecraft();

        if (this.enabled &&
            mc.gameSettings.showDebugInfo == false &&
            mc.player != null &&
            (Configs.Generic.REQUIRE_SNEAK.getBooleanValue() == false || mc.player.isSneaking()) &&
            (Configs.Generic.REQUIRE_HOLDING_KEY.getBooleanValue() == false || InputEventHandler.isRequiredKeyActive()))
        {
            if ((this.mask & Configs.InfoToggle.FPS.getBitMask()) != 0)
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

            this.renderText(mc, Configs.Generic.TEXT_POS_X.getIntegerValue(), Configs.Generic.TEXT_POS_Y.getIntegerValue(), this.lines);
        }
    }

    public void onWorldLoad()
    {
        this.serverSeedValid = false;
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
                    this.serverSeed = Long.parseLong(text.getFormatArgs()[0].toString());
                    this.serverSeedValid = true;
                    LiteModMiniHud.logger.info("Received world seed from the vanilla /seed command: {}", this.serverSeed);
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
                    this.serverSeed = Long.parseLong(text.getFormatArgs()[1].toString());
                    this.serverSeedValid = true;
                    LiteModMiniHud.logger.info("Received world seed from the JED '/jed seed' command: {}", this.serverSeed);
                }
                catch (Exception e)
                {
                    LiteModMiniHud.logger.warn("Failed to read the world seed from '{}'", text.getFormatArgs()[1], e);
                }
            }
        }
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

        if (Configs.Generic.SORT_LINES_BY_LENGTH.getBooleanValue())
        {
            Collections.sort(this.lines);

            if (Configs.Generic.SORT_LINES_REVERSED.getBooleanValue())
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
        Minecraft mc = Minecraft.getMinecraft();
        Entity entity = mc.getRenderViewEntity();
        BlockPos pos = new BlockPos(entity.posX, entity.getEntityBoundingBox().minY, entity.posZ);

        if (type == Configs.InfoToggle.FPS.getBitMask())
        {
            this.addLine(String.format("%d fps", this.fps));
        }
        else if (type == Configs.InfoToggle.TIME_REAL.getBitMask())
        {
            try
            {
                SimpleDateFormat sdf = new SimpleDateFormat(Configs.Generic.DATE_FORMAT_REAL.getStringValue());
                this.date.setTime(System.currentTimeMillis());
                this.addLine(sdf.format(this.date));
            }
            catch (Exception e)
            {
                this.addLine("Date formatting failed - Invalid date format string?");
            }
        }
        else if (type == Configs.InfoToggle.TIME_WORLD.getBitMask())
        {
            long current = entity.getEntityWorld().getWorldTime();
            long total = entity.getEntityWorld().getTotalWorldTime();
            this.addLine(String.format("World time: %5d - total: %d", current, total));
        }
        else if (type == Configs.InfoToggle.TIME_WORLD_FORMATTED.getBitMask())
        {
            try
            {
                long timeDay = (int) entity.getEntityWorld().getWorldTime();
                int day = (int) (timeDay / 24000) + 1;
                // 1 tick = 3.6 seconds in MC (0.2777... seconds IRL)
                int hour = (int) ((timeDay / 1000) + 6) % 24;
                int min = (int) (timeDay / 16.666666) % 60;
                int sec = (int) (timeDay / 0.277777) % 60;

                String str = Configs.Generic.DATE_FORMAT_MINECRAFT.getStringValue();
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
        }
        else if (type == Configs.InfoToggle.COORDINATES.getBitMask() ||
                 type == Configs.InfoToggle.DIMENSION.getBitMask())
        {
            // Don't add the same line multiple times
            if ((this.addedTypes & (Configs.InfoToggle.COORDINATES.getBitMask() | Configs.InfoToggle.DIMENSION.getBitMask())) != 0)
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if ((this.mask & Configs.InfoToggle.COORDINATES.getBitMask()) != 0)
            {
                if (Configs.Generic.USE_CUSTOMIZED_COORDINATES.getBooleanValue())
                {
                    try
                    {
                        str.append(String.format(Configs.Generic.COORDINATE_FORMAT_STRING.getStringValue(),
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

            if ((this.mask & Configs.InfoToggle.DIMENSION.getBitMask()) != 0)
            {
                int dimension = entity.getEntityWorld().provider.getDimensionType().getId();
                str.append(String.format(String.format("%sDimensionType ID: %d", pre, dimension)));
            }

            this.addLine(str.toString());
            this.addedTypes |= (Configs.InfoToggle.COORDINATES.getBitMask() | Configs.InfoToggle.DIMENSION.getBitMask());
        }
        else if (type == Configs.InfoToggle.BLOCK_POS.getBitMask())
        {
            this.addLine(String.format("Block: %d / %d / %d", pos.getX(), pos.getY(), pos.getZ()));
        }
        else if (type == Configs.InfoToggle.CHUNK_POS.getBitMask())
        {
            this.addLine(String.format("Block: %d %d %d in Chunk: %d %d %d",
                        pos.getX() & 0xF, pos.getY() & 0xF, pos.getZ() & 0xF,
                        pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4));
        }
        else if (type == Configs.InfoToggle.FACING.getBitMask())
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
        }
        else if (type == Configs.InfoToggle.LIGHT_LEVEL.getBitMask())
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && mc.world.isBlockLoaded(pos))
            {
                Chunk chunk = mc.world.getChunkFromBlockCoords(pos);

                if (chunk.isEmpty() == false)
                {
                    this.addLine(String.format("Light: %d (block: %d, sky: %d)",
                            chunk.getLightSubtracted(pos, 0),
                            chunk.getLightFor(EnumSkyBlock.BLOCK, pos),
                            chunk.getLightFor(EnumSkyBlock.SKY, pos)));
                }
            }
        }
        else if (type == Configs.InfoToggle.ROTATION_YAW.getBitMask() ||
                 type == Configs.InfoToggle.ROTATION_PITCH.getBitMask() ||
                 type == Configs.InfoToggle.SPEED.getBitMask())
        {
            // Don't add the same line multiple times
            if ((this.addedTypes & (Configs.InfoToggle.ROTATION_YAW.getBitMask() |
                                    Configs.InfoToggle.ROTATION_PITCH.getBitMask() |
                                    Configs.InfoToggle.SPEED.getBitMask())) != 0)
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if ((this.mask & Configs.InfoToggle.ROTATION_YAW.getBitMask()) != 0)
            {
                str.append(String.format("%syaw: %.1f", pre, MathHelper.wrapDegrees(entity.rotationYaw)));
                pre = " / ";
            }

            if ((this.mask & Configs.InfoToggle.ROTATION_PITCH.getBitMask()) != 0)
            {
                str.append(String.format("%spitch: %.1f", pre, MathHelper.wrapDegrees(entity.rotationPitch)));
                pre = " / ";
            }

            if ((this.mask & Configs.InfoToggle.SPEED.getBitMask()) != 0)
            {
                double dx = entity.posX - entity.lastTickPosX;
                double dy = entity.posY - entity.lastTickPosY;
                double dz = entity.posZ - entity.lastTickPosZ;
                double dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
                str.append(String.format("%sspeed: %.3f m/s", pre, dist * 20));
                pre = " / ";
            }

            this.addLine(str.toString());
            this.addedTypes |= (Configs.InfoToggle.ROTATION_YAW.getBitMask() |
                                Configs.InfoToggle.ROTATION_PITCH.getBitMask() |
                                Configs.InfoToggle.SPEED.getBitMask());
        }
        else if (type == Configs.InfoToggle.CHUNK_SECTIONS.getBitMask())
        {
            this.addLine(String.format("C: %d", ((IMixinRenderGlobal) mc.renderGlobal).getRenderedChunksInvoker()));
        }
        else if (type == Configs.InfoToggle.CHUNK_SECTIONS_FULL.getBitMask())
        {
            this.addLine(mc.renderGlobal.getDebugInfoRenders());
        }
        else if (type == Configs.InfoToggle.CHUNK_UPDATES.getBitMask())
        {
            this.addLine(String.format("Chunk updates: %d", RenderChunk.renderChunksUpdated));
        }
        else if (type == Configs.InfoToggle.PARTICLE_COUNT.getBitMask())
        {
            this.addLine(String.format("P: %s", mc.effectRenderer.getStatistics()));
        }
        else if (type == Configs.InfoToggle.DIFFICULTY.getBitMask())
        {
            if (mc.world.isBlockLoaded(pos))
            {
                DifficultyInstance diff = mc.world.getDifficultyForLocation(pos);

                if (mc.isIntegratedServerRunning() && mc.getIntegratedServer() != null)
                {
                    EntityPlayerMP player = mc.getIntegratedServer().getPlayerList().getPlayerByUUID(mc.player.getUniqueID());

                    if (player != null)
                    {
                        diff = player.world.getDifficultyForLocation(new BlockPos(player));
                    }
                }

                this.addLine(String.format("Local Difficulty: %.2f // %.2f (Day %d)",
                        diff.getAdditionalDifficulty(), diff.getClampedAdditionalDifficulty(), mc.world.getWorldTime() / 24000L));
            }
        }
        else if (type == Configs.InfoToggle.BIOME.getBitMask())
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && mc.world.isBlockLoaded(pos))
            {
                Chunk chunk = mc.world.getChunkFromBlockCoords(pos);

                if (chunk.isEmpty() == false)
                {
                    this.addLine("Biome: " + chunk.getBiome(pos, mc.world.getBiomeProvider()).getBiomeName());
                }
            }
        }
        else if (type == Configs.InfoToggle.BIOME_REG_NAME.getBitMask())
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && mc.world.isBlockLoaded(pos))
            {
                Chunk chunk = mc.world.getChunkFromBlockCoords(pos);

                if (chunk.isEmpty() == false)
                {
                    Biome biome = chunk.getBiome(pos, mc.world.getBiomeProvider());
                    ResourceLocation rl = Biome.REGISTRY.getNameForObject(biome);
                    String name = rl != null ? rl.toString() : "?";
                    this.addLine("Biome reg name: " + name);
                }
            }
        }
        else if (type == Configs.InfoToggle.ENTITIES.getBitMask())
        {
            String ent = mc.renderGlobal.getDebugInfoEntities();

            int p = ent.indexOf(",");

            if (p != -1)
            {
                ent = ent.substring(0, p);
            }

            this.addLine(ent);
        }
        else if (type == Configs.InfoToggle.SLIME_CHUNK.getBitMask())
        {
            boolean valid = false;
            long seed = 0;
            String result;
            MinecraftServer server = entity.getServer();

            if (server != null && server.isSinglePlayer())
            {
                World world = server.getWorld(entity.dimension);
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
        }
        else if (type == Configs.InfoToggle.LOOKING_AT_ENTITY.getBitMask())
        {
            if (mc.objectMouseOver != null &&
                mc.objectMouseOver.typeOfHit == RayTraceResult.Type.ENTITY &&
                mc.objectMouseOver.entityHit != null)
            {
                Entity target = mc.objectMouseOver.entityHit;

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
        }
        else if (type == Configs.InfoToggle.ENTITY_REG_NAME.getBitMask())
        {
            if (mc.objectMouseOver != null &&
                mc.objectMouseOver.typeOfHit == RayTraceResult.Type.ENTITY &&
                mc.objectMouseOver.entityHit != null)
            {
                ResourceLocation regName = EntityList.getKey(mc.objectMouseOver.entityHit);

                if (regName != null)
                {
                    this.addLine(String.format("Entity reg name: %s", regName.toString()));
                }
            }
        }
        else if (type == Configs.InfoToggle.LOOKING_AT_BLOCK.getBitMask() ||
                 type == Configs.InfoToggle.LOOKING_AT_BLOCK_CHUNK.getBitMask())
        {
            // Don't add the same line multiple times
            if ((this.addedTypes & (Configs.InfoToggle.LOOKING_AT_BLOCK.getBitMask() | Configs.InfoToggle.LOOKING_AT_BLOCK_CHUNK.getBitMask())) != 0)
            {
                return;
            }

            if (mc.objectMouseOver != null &&
                mc.objectMouseOver.typeOfHit == RayTraceResult.Type.BLOCK &&
                mc.objectMouseOver.getBlockPos() != null)
            {
                BlockPos lookPos = mc.objectMouseOver.getBlockPos();
                String pre = "";
                StringBuilder str = new StringBuilder(128);

                if ((this.mask & Configs.InfoToggle.LOOKING_AT_BLOCK.getBitMask()) != 0)
                {
                    str.append(String.format("Looking at block: %d %d %d", lookPos.getX(), lookPos.getY(), lookPos.getZ()));
                    pre = " // ";
                }

                if ((this.mask & Configs.InfoToggle.LOOKING_AT_BLOCK_CHUNK.getBitMask()) != 0)
                {
                    str.append(pre).append(String.format("Block: %d %d %d within chunk section: %d %d %d",
                            lookPos.getX() & 0xF, lookPos.getY() & 0xF, lookPos.getZ() & 0xF,
                            lookPos.getX() >> 4, lookPos.getY() >> 4, lookPos.getZ() >> 4));
                }

                this.addLine(str.toString());
                this.addedTypes |= (Configs.InfoToggle.LOOKING_AT_BLOCK.getBitMask() | Configs.InfoToggle.LOOKING_AT_BLOCK_CHUNK.getBitMask());
            }
        }
        else if (type == Configs.InfoToggle.BLOCK_PROPS.getBitMask())
        {
            this.getBlockProperties(mc);
        }
    }

    @SuppressWarnings("unchecked")
    private <T extends Comparable<T>> void getBlockProperties(Minecraft mc)
    {
        if (mc.objectMouseOver != null &&
            mc.objectMouseOver.typeOfHit == RayTraceResult.Type.BLOCK &&
            mc.objectMouseOver.getBlockPos() != null)
        {
            BlockPos posLooking = mc.objectMouseOver.getBlockPos();
            IBlockState state = mc.world.getBlockState(posLooking);

            if (mc.world.getWorldType() != WorldType.DEBUG_ALL_BLOCK_STATES)
            {
                state = state.getActualState(mc.world, posLooking);
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

    private void renderText(Minecraft mc, int xOff, int yOff, List<StringHolder> lines)
    {
        boolean scale = Configs.Generic.USE_SCALED_FONT.getBooleanValue();

        if (scale)
        {
            GlStateManager.pushMatrix();
            GlStateManager.scale(0.5, 0.5, 0.5);
        }

        FontRenderer fontRenderer = mc.fontRenderer;

        for (StringHolder holder : lines)
        {
            String line = holder.str;

            if (Configs.Generic.USE_TEXT_BACKGROUND.getBooleanValue())
            {
                Gui.drawRect(xOff - 2, yOff - 2, xOff + fontRenderer.getStringWidth(line) + 2, yOff + fontRenderer.FONT_HEIGHT, Configs.Generic.TEXT_BACKGROUND_COLOR.getIntegerValue());
            }

            if (Configs.Generic.USE_FONT_SHADOW.getBooleanValue())
            {
                fontRenderer.drawStringWithShadow(line, xOff, yOff, Configs.Generic.FONT_COLOR.getIntegerValue());
            }
            else
            {
                fontRenderer.drawString(line, xOff, yOff, Configs.Generic.FONT_COLOR.getIntegerValue());
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
