package fi.dy.masa.minihud.event;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map.Entry;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.ConfigsGeneric;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.OverlayHotkeys;
import fi.dy.masa.minihud.config.interfaces.IConfigOptionListEntry;
import fi.dy.masa.minihud.mixin.IMixinRenderGlobal;
import fi.dy.masa.minihud.renderer.OverlayRenderer;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;
import net.minecraft.block.Block;
import net.minecraft.block.properties.IProperty;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.gui.ScaledResolution;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.chunk.RenderChunk;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.potion.Potion;
import net.minecraft.potion.PotionEffect;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.text.TextComponentString;
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
    private final DataStorage data;
    private final Date date;
    private boolean enabled;
    private int infoLineMask;
    private int overlayMask;
    private int fps;
    private int fpsCounter;
    private long fpsUpdateTime = Minecraft.getSystemTime();
    private long infoUpdateTime;
    private int addedTypes;
    private double fontScale = 0.5d;

    private final List<StringHolder> lines = new ArrayList<StringHolder>();

    public RenderEventHandler()
    {
        this.data = DataStorage.getInstance();
        this.date = new Date();
    }

    public static RenderEventHandler getInstance()
    {
        return INSTANCE;
    }

    public static void fixDebugRendererState()
    {
        if (ConfigsGeneric.FIX_VANILLA_DEBUG_RENDERERS.getBooleanValue())
        {
            GlStateManager.disableLighting();
            //GlStateManager.color(1, 1, 1, 1);
            //OpenGlHelper.setLightmapTextureCoords(OpenGlHelper.lightmapTexUnit, 240f, 240f);
        }
    }

    public void onRenderGameOverlayPost(float partialTicks)
    {
        Minecraft mc = Minecraft.getMinecraft();

        if (this.enabled &&
            mc.gameSettings.showDebugInfo == false &&
            mc.player != null &&
            (ConfigsGeneric.REQUIRE_SNEAK.getBooleanValue() == false || mc.player.isSneaking()) &&
            (ConfigsGeneric.REQUIRE_HOLDING_KEY.getBooleanValue() == false || InputEventHandler.isRequiredKeyActive()))
        {
            if ((this.infoLineMask & InfoToggle.FPS.getBitMask()) != 0)
            {
                this.updateFps();
            }

            long currentTime = System.currentTimeMillis();

            // Only update the text once per game tick
            if (currentTime - this.infoUpdateTime >= 50)
            {
                this.updateLines(this.infoLineMask);
                this.infoUpdateTime = currentTime;
            }

            this.renderText(mc, ConfigsGeneric.TEXT_POS_X.getIntegerValue(), ConfigsGeneric.TEXT_POS_Y.getIntegerValue(), this.lines);
        }
    }

    public void onRenderWorldLast(float partialTicks)
    {
        Minecraft mc = Minecraft.getMinecraft();

        if (this.enabled && mc.player != null)
        {
            OverlayRenderer.renderOverlays(this.overlayMask, mc, partialTicks);
        }
    }

    public void setFontScale(double scale)
    {
        this.fontScale = scale;
    }


    public int getSubtitleOffset()
    {
        HudAlignment align = (HudAlignment) ConfigsGeneric.HUD_ALIGNMENT.getOptionListValue();

        if (align == HudAlignment.BOTTOM_RIGHT)
        {
            Minecraft mc = Minecraft.getMinecraft();
            int offset = (int) (this.lines.size() * (mc.fontRenderer.FONT_HEIGHT + 2) * this.fontScale);

            return -(offset - 16);
        }

        return 0;
    }

    public void setEnabledMask(int mask)
    {
        this.infoLineMask = mask;
    }

    public void xorInfoLineEnabledMask(int mask)
    {
        this.infoLineMask ^= mask;
    }

    public void xorOverlayRendererEnabledMask(int mask)
    {
        this.overlayMask ^= mask;
        Minecraft mc = Minecraft.getMinecraft();

        if (mc != null && mc.player != null)
        {
            // Flipped on the Chunk unload bucket rendering, store the player's current y-position
            if ((mask & OverlayHotkeys.CHUNK_UNLOAD_BUCKET.getBitMask()) != 0 &&
                (this.overlayMask & OverlayHotkeys.CHUNK_UNLOAD_BUCKET.getBitMask()) != 0)
            {
                OverlayRenderer.chunkUnloadBucketOverlayY = mc.player.posY;
            }
            // Flipped on the slime chunk overlay rendering, store the player's current y-position
            else if ((mask & OverlayHotkeys.SLIME_CHUNKS_OVERLAY.getBitMask()) != 0 &&
                (this.overlayMask & OverlayHotkeys.SLIME_CHUNKS_OVERLAY.getBitMask()) != 0)
            {
                OverlayRenderer.slimeChunkOverlayTopY = mc.player.posY;
            }
            // Flipped on the real spawn point rendering, store the player's current position
            else if ((mask & OverlayHotkeys.SPAWN_CHUNK_OVERLAY_REAL.getBitMask()) != 0 &&
                     (this.overlayMask & OverlayHotkeys.SPAWN_CHUNK_OVERLAY_REAL.getBitMask()) != 0)
            {
                BlockPos spawn = mc.world.getSpawnPoint();
                this.data.setWorldSpawn(spawn);
                String str = String.format("Using the world spawn x = %d, y = %d, z = %d", spawn.getX(), spawn.getY(), spawn.getZ());
                mc.player.sendStatusMessage(new TextComponentString(str), true);
            }
        }
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
            this.addLine(pos.type, enabledMask);
        }

        if (ConfigsGeneric.SORT_LINES_BY_LENGTH.getBooleanValue())
        {
            Collections.sort(this.lines);

            if (ConfigsGeneric.SORT_LINES_REVERSED.getBooleanValue())
            {
                Collections.reverse(this.lines);
            }
        }
    }

    private void addLine(String text)
    {
        this.lines.add(new StringHolder(text));
    }

    private void addLine(int type, int enabledMask)
    {
        Minecraft mc = Minecraft.getMinecraft();
        Entity entity = mc.getRenderViewEntity();
        World world = entity.getEntityWorld();
        BlockPos pos = new BlockPos(entity.posX, entity.getEntityBoundingBox().minY, entity.posZ);

        if (type == InfoToggle.FPS.getBitMask())
        {
            this.addLine(String.format("%d fps", this.fps));
        }
        else if (type == InfoToggle.TIME_REAL.getBitMask())
        {
            try
            {
                SimpleDateFormat sdf = new SimpleDateFormat(ConfigsGeneric.DATE_FORMAT_REAL.getStringValue());
                this.date.setTime(System.currentTimeMillis());
                this.addLine(sdf.format(this.date));
            }
            catch (Exception e)
            {
                this.addLine("Date formatting failed - Invalid date format string?");
            }
        }
        else if (type == InfoToggle.TIME_WORLD.getBitMask())
        {
            long current = world.getWorldTime();
            long total = world.getTotalWorldTime();
            this.addLine(String.format("World time: %5d - total: %d", current, total));
        }
        else if (type == InfoToggle.TIME_WORLD_FORMATTED.getBitMask())
        {
            try
            {
                long timeDay = (int) world.getWorldTime();
                int day = (int) (timeDay / 24000) + 1;
                // 1 tick = 3.6 seconds in MC (0.2777... seconds IRL)
                int hour = (int) ((timeDay / 1000) + 6) % 24;
                int min = (int) (timeDay / 16.666666) % 60;
                int sec = (int) (timeDay / 0.277777) % 60;

                String str = ConfigsGeneric.DATE_FORMAT_MINECRAFT.getStringValue();
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
        else if (type == InfoToggle.SERVER_TPS.getBitMask())
        {
            if (mc.isSingleplayer() && (mc.getIntegratedServer().getTickCounter() % 10) == 0)
            {
                this.data.updateIntegratedServerTPS();
            }

            if (this.data.isServerTPSValid())
            {
                double tps = this.data.getServerTPS();
                double mspt = this.data.getServerMSPT();
                String rst = TextFormatting.RESET.toString();
                String preTps = tps >= 20.0D ? TextFormatting.GREEN.toString() : TextFormatting.RED.toString();
                String preMspt;

                // Carpet server and integrated server have actual meaningful MSPT data available
                if (this.data.isCarpetServer() || mc.isSingleplayer())
                {
                    if      (mspt <= 40) { preMspt = TextFormatting.GREEN.toString(); }
                    else if (mspt <= 45) { preMspt = TextFormatting.YELLOW.toString(); }
                    else if (mspt <= 50) { preMspt = TextFormatting.GOLD.toString(); }
                    else                 { preMspt = TextFormatting.RED.toString(); }

                    this.addLine(String.format("Server TPS: %s%.1f%s MSPT: %s%.1f%s", preTps, tps, rst, preMspt, mspt, rst));
                }
                else
                {
                    if (mspt <= 51) { preMspt = TextFormatting.GREEN.toString(); }
                    else            { preMspt = TextFormatting.RED.toString(); }

                    this.addLine(String.format("Server TPS: %s%.1f%s (MSPT*: %s%.1f%s)", preTps, tps, rst, preMspt, mspt, rst));
                }
            }
            else
            {
                this.addLine("Server TPS: <no valid data>");
            }
        }
        else if (type == InfoToggle.COORDINATES.getBitMask() ||
                 type == InfoToggle.DIMENSION.getBitMask())
        {
            // Don't add the same line multiple times
            if ((this.addedTypes & (InfoToggle.COORDINATES.getBitMask() | InfoToggle.DIMENSION.getBitMask())) != 0)
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if ((enabledMask & InfoToggle.COORDINATES.getBitMask()) != 0)
            {
                if (ConfigsGeneric.USE_CUSTOMIZED_COORDINATES.getBooleanValue())
                {
                    try
                    {
                        str.append(String.format(ConfigsGeneric.COORDINATE_FORMAT_STRING.getStringValue(),
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

            if ((enabledMask & InfoToggle.DIMENSION.getBitMask()) != 0)
            {
                int dimension = world.provider.getDimensionType().getId();
                str.append(String.format(String.format("%sDimensionType ID: %d", pre, dimension)));
            }

            this.addLine(str.toString());
            this.addedTypes |= (InfoToggle.COORDINATES.getBitMask() | InfoToggle.DIMENSION.getBitMask());
        }
        else if (type == InfoToggle.BLOCK_POS.getBitMask() ||
                 type == InfoToggle.CHUNK_POS.getBitMask() ||
                 type == InfoToggle.REGION_FILE.getBitMask())
        {
            // Don't add the same line multiple times
            if ((this.addedTypes & (InfoToggle.BLOCK_POS.getBitMask() |
                                    InfoToggle.CHUNK_POS.getBitMask() |
                                    InfoToggle.REGION_FILE.getBitMask())) != 0)
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(256);

            if ((enabledMask & InfoToggle.BLOCK_POS.getBitMask()) != 0)
            {
                str.append(String.format("Block: %d, %d, %d", pos.getX(), pos.getY(), pos.getZ()));
                pre = " / ";
            }

            if ((enabledMask & InfoToggle.CHUNK_POS.getBitMask()) != 0)
            {
                str.append(pre).append(String.format("Sub-Chunk: %d, %d, %d", pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4));
                pre = " / ";
            }

            if ((enabledMask & InfoToggle.REGION_FILE.getBitMask()) != 0)
            {
                str.append(pre).append(String.format("Region: r.%d.%d", pos.getX() >> 9, pos.getZ() >> 9));
            }

            this.addLine(str.toString());
            this.addedTypes |= (InfoToggle.BLOCK_POS.getBitMask() |
                                InfoToggle.CHUNK_POS.getBitMask() |
                                InfoToggle.REGION_FILE.getBitMask());
        }
        else if (type == InfoToggle.BLOCK_IN_CHUNK.getBitMask())
        {
            this.addLine(String.format("Block: %d, %d, %d within Sub-Chunk: %d, %d, %d",
                        pos.getX() & 0xF, pos.getY() & 0xF, pos.getZ() & 0xF,
                        pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4));
        }
        else if (type == InfoToggle.FACING.getBitMask())
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
        else if (type == InfoToggle.LIGHT_LEVEL.getBitMask())
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
        else if (type == InfoToggle.ROTATION_YAW.getBitMask() ||
                 type == InfoToggle.ROTATION_PITCH.getBitMask() ||
                 type == InfoToggle.SPEED.getBitMask())
        {
            // Don't add the same line multiple times
            if ((this.addedTypes & (InfoToggle.ROTATION_YAW.getBitMask() |
                                    InfoToggle.ROTATION_PITCH.getBitMask() |
                                    InfoToggle.SPEED.getBitMask())) != 0)
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if ((enabledMask & InfoToggle.ROTATION_YAW.getBitMask()) != 0)
            {
                str.append(String.format("yaw: %.1f", MathHelper.wrapDegrees(entity.rotationYaw)));
                pre = " / ";
            }

            if ((enabledMask & InfoToggle.ROTATION_PITCH.getBitMask()) != 0)
            {
                str.append(pre).append(String.format("pitch: %.1f", MathHelper.wrapDegrees(entity.rotationPitch)));
                pre = " / ";
            }

            if ((enabledMask & InfoToggle.SPEED.getBitMask()) != 0)
            {
                double dx = entity.posX - entity.lastTickPosX;
                double dy = entity.posY - entity.lastTickPosY;
                double dz = entity.posZ - entity.lastTickPosZ;
                double dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
                str.append(pre).append(String.format("speed: %.3f m/s", dist * 20));
            }

            this.addLine(str.toString());
            this.addedTypes |= (InfoToggle.ROTATION_YAW.getBitMask() |
                                InfoToggle.ROTATION_PITCH.getBitMask() |
                                InfoToggle.SPEED.getBitMask());
        }
        else if (type == InfoToggle.CHUNK_SECTIONS.getBitMask())
        {
            this.addLine(String.format("C: %d", ((IMixinRenderGlobal) mc.renderGlobal).getRenderedChunksInvoker()));
        }
        else if (type == InfoToggle.CHUNK_SECTIONS_FULL.getBitMask())
        {
            this.addLine(mc.renderGlobal.getDebugInfoRenders());
        }
        else if (type == InfoToggle.CHUNK_UPDATES.getBitMask())
        {
            this.addLine(String.format("Chunk updates: %d", RenderChunk.renderChunksUpdated));
        }
        else if (type == InfoToggle.CHUNK_UNLOAD_ORDER.getBitMask())
        {
            int bucket = MiscUtils.getChunkUnloadBucket(pos.getX() >> 4, pos.getZ() >> 4);
            this.addLine(String.format("Chunk unload bucket: %d", bucket));
        }
        else if (type == InfoToggle.PARTICLE_COUNT.getBitMask())
        {
            this.addLine(String.format("P: %s", mc.effectRenderer.getStatistics()));
        }
        else if (type == InfoToggle.DIFFICULTY.getBitMask())
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
        else if (type == InfoToggle.BIOME.getBitMask())
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
        else if (type == InfoToggle.BIOME_REG_NAME.getBitMask())
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
        else if (type == InfoToggle.ENTITIES.getBitMask())
        {
            String ent = mc.renderGlobal.getDebugInfoEntities();

            int p = ent.indexOf(",");

            if (p != -1)
            {
                ent = ent.substring(0, p);
            }

            this.addLine(ent);
        }
        else if (type == InfoToggle.SLIME_CHUNK.getBitMask())
        {
            if (world.provider.isSurfaceWorld() == false)
            {
                return;
            }

            String result;
            int dimension = entity.dimension;

            if (this.data.isWorldSeedKnown(dimension))
            {
                long seed = this.data.getWorldSeed(dimension);

                if (MiscUtils.canSlimeSpawnAt(pos.getX(), pos.getZ(), seed))
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
        else if (type == InfoToggle.LOOKING_AT_ENTITY.getBitMask())
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
        else if (type == InfoToggle.ENTITY_REG_NAME.getBitMask())
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
        else if (type == InfoToggle.LOOKING_AT_BLOCK.getBitMask() ||
                 type == InfoToggle.LOOKING_AT_BLOCK_CHUNK.getBitMask())
        {
            // Don't add the same line multiple times
            if ((this.addedTypes & (InfoToggle.LOOKING_AT_BLOCK.getBitMask() |
                                    InfoToggle.LOOKING_AT_BLOCK_CHUNK.getBitMask())) != 0)
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

                if ((enabledMask & InfoToggle.LOOKING_AT_BLOCK.getBitMask()) != 0)
                {
                    str.append(String.format("Looking at block: %d, %d, %d", lookPos.getX(), lookPos.getY(), lookPos.getZ()));
                    pre = " // ";
                }

                if ((enabledMask & InfoToggle.LOOKING_AT_BLOCK_CHUNK.getBitMask()) != 0)
                {
                    str.append(pre).append(String.format("Block: %d, %d, %d in Sub-Chunk: %d, %d, %d",
                            lookPos.getX() & 0xF, lookPos.getY() & 0xF, lookPos.getZ() & 0xF,
                            lookPos.getX() >> 4, lookPos.getY() >> 4, lookPos.getZ() >> 4));
                }

                this.addLine(str.toString());
                this.addedTypes |= (InfoToggle.LOOKING_AT_BLOCK.getBitMask() |
                                    InfoToggle.LOOKING_AT_BLOCK_CHUNK.getBitMask());
            }
        }
        else if (type == InfoToggle.BLOCK_PROPS.getBitMask())
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
        FontRenderer fontRenderer = mc.fontRenderer;
        ScaledResolution res = new ScaledResolution(mc);
        final int lineHeight = fontRenderer.FONT_HEIGHT + 2;
        final double scale = this.fontScale;
        final int bgMargin = 2;
        final int bgColor = ConfigsGeneric.TEXT_BACKGROUND_COLOR.getIntegerValue();
        HudAlignment align = (HudAlignment) ConfigsGeneric.HUD_ALIGNMENT.getOptionListValue();
        double posX = xOff + bgMargin;
        double posY = yOff + bgMargin;

        // Only Chuck Norris can divide by zero
        if (scale == 0d)
        {
            return;
        }

        if (align == HudAlignment.TOP_RIGHT)
        {
            Collection<PotionEffect> effects = mc.player.getActivePotionEffects();

            if (effects.isEmpty() == false)
            {
                int y1 = 0;
                int y2 = 0;

                for (PotionEffect effect : effects)
                {
                    Potion potion = effect.getPotion();

                    if (effect.doesShowParticles() && potion.hasStatusIcon())
                    {
                        if (potion.isBeneficial())
                        {
                            y1 = 26;
                        }
                        else
                        {
                            y2 = 52;
                            break;
                        }
                    }
                }

                posY += Math.max(y1, y2) / scale;
            }
        }

        switch (align)
        {
            case BOTTOM_LEFT:
            case BOTTOM_RIGHT:
                posY = res.getScaledHeight() / scale - (lines.size() * lineHeight) - yOff + 2;
                break;
            case CENTER:
                posY = (res.getScaledHeight() / scale / 2.0d) - (lines.size() * lineHeight / 2.0d) + yOff;
                break;
            default:
        }

        if (scale != 1d)
        {
            GlStateManager.pushMatrix();
            GlStateManager.scale(scale, scale, 0);
        }

        for (StringHolder holder : lines)
        {
            String line = holder.str;
            final int width = fontRenderer.getStringWidth(line);

            switch (align)
            {
                case TOP_RIGHT:
                case BOTTOM_RIGHT:
                    posX = (res.getScaledWidth() / scale) - width - xOff - bgMargin;
                    break;
                case CENTER:
                    posX = (res.getScaledWidth() / scale / 2) - (width / 2) - xOff;
                    break;
                default:
            }

            final int x = (int) posX;
            final int y = (int) posY;
            posY += (double) lineHeight;

            if (ConfigsGeneric.USE_TEXT_BACKGROUND.getBooleanValue())
            {
                Gui.drawRect(x - bgMargin, y - bgMargin, x + width + bgMargin, y + fontRenderer.FONT_HEIGHT, bgColor);
            }

            if (ConfigsGeneric.USE_FONT_SHADOW.getBooleanValue())
            {
                fontRenderer.drawStringWithShadow(line, x, y, ConfigsGeneric.FONT_COLOR.getIntegerValue());
            }
            else
            {
                fontRenderer.drawString(line, x, y, ConfigsGeneric.FONT_COLOR.getIntegerValue());
            }
        }

        if (scale != 1d)
        {
            GlStateManager.popMatrix();
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

    public enum HudAlignment implements IConfigOptionListEntry
    {
        TOP_LEFT        ("Top Left"),
        TOP_RIGHT       ("Top Right"),
        BOTTOM_LEFT     ("Bottom Left"),
        BOTTOM_RIGHT    ("Bottom Right"),
        CENTER          ("Center");

        private final String displayName;

        private HudAlignment(String displayName)
        {
            this.displayName = displayName;
        }

        @Override
        public String getStringValue()
        {
            return this.name().toLowerCase();
        }

        @Override
        public String getDisplayName()
        {
            return this.displayName;
        }

        @Override
        public int getOrdinalValue()
        {
            return this.ordinal();
        }

        @Override
        public IConfigOptionListEntry cycle(boolean forward)
        {
            int id = this.ordinal();

            if (forward)
            {
                if (++id >= values().length)
                {
                    id = 0;
                }
            }
            else
            {
                if (--id < 0)
                {
                    id = values().length - 1;
                }
            }

            return values()[id % values().length];
        }

        @Override
        public HudAlignment fromString(String name)
        {
            return fromStringStatic(name);
        }

        public static HudAlignment fromStringStatic(String name)
        {
            for (HudAlignment al : HudAlignment.values())
            {
                if (al.name().equalsIgnoreCase(name))
                {
                    return al;
                }
            }

            return HudAlignment.TOP_LEFT;
        }
    }
}
