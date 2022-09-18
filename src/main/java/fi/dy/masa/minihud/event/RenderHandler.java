package fi.dy.masa.minihud.event;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;
import javax.annotation.Nullable;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.network.NetworkPlayerInfo;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.chunk.RenderChunk;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityList;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.item.ItemMap;
import net.minecraft.item.ItemStack;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.DifficultyInstance;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.World;
import net.minecraft.world.WorldServer;
import net.minecraft.world.WorldType;
import net.minecraft.world.biome.Biome;
import net.minecraft.world.chunk.Chunk;
import fi.dy.masa.malilib.config.value.ScreenLocation;
import fi.dy.masa.malilib.event.PostGameOverlayRenderer;
import fi.dy.masa.malilib.event.PostItemTooltipRenderer;
import fi.dy.masa.malilib.event.PostWorldRenderer;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.overlay.InfoArea;
import fi.dy.masa.malilib.overlay.widget.StringListRendererWidget;
import fi.dy.masa.malilib.registry.Registry;
import fi.dy.masa.malilib.render.inventory.InventoryRenderUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.malilib.util.game.BlockUtils;
import fi.dy.masa.malilib.util.game.WorldUtils;
import fi.dy.masa.malilib.util.game.wrap.EntityWrap;
import fi.dy.masa.malilib.util.game.wrap.GameUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoLine;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.data.DataStorage.HashSizeType;
import fi.dy.masa.minihud.data.MobCapDataHandler;
import fi.dy.masa.minihud.data.TpsData;
import fi.dy.masa.minihud.mixin.info_lines.RenderGlobalMixin;
import fi.dy.masa.minihud.renderer.OverlayRenderer;
import fi.dy.masa.minihud.util.MiscUtils;

public class RenderHandler implements PostGameOverlayRenderer, PostItemTooltipRenderer, PostWorldRenderer
{
    public static final RenderHandler INSTANCE = new RenderHandler();
    private final Supplier<String> profilerSectionSupplier = () -> "MiniHUD_RenderHandler";
    private final Set<InfoLine> addedTypes = new HashSet<>();
    private final Date date = new Date();
    private StringListRendererWidget stringListRenderer;
    private int fps;
    private int fpsCounter;
    private long fpsUpdateTime = Minecraft.getSystemTime();
    private long infoUpdateTime;
    private boolean enabled;
    private boolean ready;

    private final List<StringHolder> lineWrappers = new ArrayList<>();
    private final List<String> lines = new ArrayList<>();

    private RenderHandler()
    {
    }

    @Nullable
    public StringListRendererWidget getStringListRenderer()
    {
        // The ready flag will be set from the world load event handler.
        // That event is fired after malilib has loaded the InfoWidgetManager contents from the save file
        // (see: ClientWorldChangeEventDispatcherImpl#onWorldLoadPost()).
        // Thus we will not try to create a new StringListRendererWidget before malilib has had the
        // chance to load the existing ones from file.
        if (this.stringListRenderer == null && this.ready)
        {
            this.stringListRenderer = this.createStringListRenderer();
        }

        return this.stringListRenderer;
    }

    private StringListRendererWidget createStringListRenderer()
    {
        ScreenLocation location = Configs.Internal.HUD_LOCATION.getValue();
        InfoArea area = Registry.INFO_OVERLAY.getOrCreateInfoArea(location);
        String marker = Reference.MOD_ID;
        StringListRendererWidget widget = area.findWidget(StringListRendererWidget.class, (w) -> w.getMarkerManager().matchesMarker(marker));

        if (widget == null)
        {
            widget = new StringListRendererWidget();
            widget.getMarkerManager().addMarker(marker);
            widget.setName(Reference.MOD_NAME);
            widget.setSortIndex(90);
            widget.setScale(0.5);
            widget.getTextSettings().setTextColor(0xFFFFFFFF);
            widget.getTextSettings().setBackgroundColor(0x80505050);
            widget.getTextSettings().setBackgroundEnabled(true);
            widget.getMargin().setAll(1, 0, 0, 1);
            widget.getPadding().setAll(2, 2, 0, 2);
            Registry.INFO_WIDGET_MANAGER.addWidget(widget);
        }

        widget.addLocationChangeListener(Configs.Internal.HUD_LOCATION::setValue);
        widget.setStringListProvider(Reference.MOD_ID, this::getInfoLines, 99);

        return widget;
    }

    private List<String> getInfoLines()
    {
        return this.enabled ? this.lines : Collections.emptyList();
    }

    /*
    public void updateHudSettings()
    {
        if (this.stringListRenderer != null)
        {
            TextRenderSettings settings = new TextRenderSettings();
            settings.setTextColor(Configs.Colors.HUD_TEXT.getIntegerValue());
            settings.setBackgroundColor(Configs.Colors.HUD_TEXT_BACKGROUND.getIntegerValue());
            settings.setUseTextShadow(Configs.Generic.USE_FONT_SHADOW.getBooleanValue());
            settings.setUseBackground(Configs.Generic.USE_TEXT_BACKGROUND.getBooleanValue());

            EdgeInt offset = new EdgeInt();
            offset.setLeftRight(Configs.Generic.HUD_TEXT_POS_X.getIntegerValue());
            offset.setTopBottom(Configs.Generic.HUD_TEXT_POS_Y.getIntegerValue());

            this.stringListRenderer.setTextSettings(settings);
            this.stringListRenderer.setPadding(offset);
            this.stringListRenderer.setScale(Configs.Generic.HUD_FONT_SCALE.getDoubleValue());
        }
    }
    */

    public void setReady(boolean ready)
    {
        this.ready = ready;

        if (ready == false)
        {
            this.stringListRenderer = null;
        }
    }

    public static void fixDebugRendererState()
    {
        if (Configs.Generic.FIX_VANILLA_DEBUG_RENDERERS.getBooleanValue())
        {
            GlStateManager.disableLighting();
            //RenderUtils.color(1, 1, 1, 1);
            //OpenGlHelper.setLightmapTextureCoords(OpenGlHelper.lightmapTexUnit, 240f, 240f);
        }
    }

    @Override
    public Supplier<String> getProfilerSectionSupplier()
    {
        return this.profilerSectionSupplier;
    }

    @Override
    public void onPostGameOverlayRender()
    {
        if (this.enabled)
        {
            if (InfoLine.FPS.getBooleanValue())
            {
                this.updateFps();
            }

            long currentTime = System.currentTimeMillis();

            // Only update the text once per game tick
            if (currentTime - this.infoUpdateTime >= 50)
            {
                this.updateLines();
                this.infoUpdateTime = currentTime;
            }
        }
    }

    @Override
    public void onPostRenderItemTooltip(ItemStack stack, int x, int y)
    {
        float z = Configs.Generic.ITEM_PREVIEW_Z.getIntegerValue();

        if (stack.getItem() instanceof ItemMap)
        {
            if (Configs.Generic.MAP_PREVIEW.getBooleanValue())
            {
                boolean render = Configs.Generic.MAP_PREVIEW_REQUIRE_SHIFT.getBooleanValue() == false || BaseScreen.isShiftDown();

                if (render)
                {
                    int dimensions = Configs.Generic.MAP_PREVIEW_SIZE.getIntegerValue();
                    fi.dy.masa.malilib.render.RenderUtils.renderMapPreview(stack, x, y, z, dimensions);
                }
            }
        }
        else if (Configs.Generic.SHULKER_BOX_PREVIEW.getBooleanValue())
        {
            boolean render = Configs.Generic.SHULKER_DISPLAY_REQUIRE_SHIFT.getBooleanValue() == false || BaseScreen.isShiftDown();

            if (render)
            {
                boolean background = Configs.Generic.SHULKER_DISPLAY_BACKGROUND_COLOR.getBooleanValue();
                x += 8;
                y -= 10;
                InventoryRenderUtils.renderItemInventoryPreview(stack, x, y, z, background);
            }
        }
    }

    @Override
    public void onPostWorldRender(float tickDelta)
    {
        if (Configs.Generic.OVERLAYS_RENDERING_TOGGLE.getBooleanValue())
        {
            OverlayRenderer.renderOverlays(tickDelta);
        }
    }

    public int getSubtitleOffset()
    {
        if (this.stringListRenderer != null && this.stringListRenderer.getScreenLocation() == ScreenLocation.BOTTOM_RIGHT)
        {
            double scale = this.stringListRenderer.getScale();
            int offset = (int) (this.lineWrappers.size() * (StringUtils.getFontHeight() + 2) * scale);

            return -(offset - 16);
        }

        return 0;
    }

    private void updateFps()
    {
        ++this.fpsCounter;

        long currentTime = Minecraft.getSystemTime();

        if (currentTime >= (this.fpsUpdateTime + 1000L))
        {
            this.fpsUpdateTime = currentTime;
            this.fps = this.fpsCounter;
            this.fpsCounter = 0;
        }
    }

    public void onClientTick(Minecraft mc)
    {
        boolean wasEnabled = this.enabled;

        this.enabled = Configs.Generic.INFO_LINES_RENDERING_TOGGLE.getBooleanValue() &&
                       mc.gameSettings.showDebugInfo == false &&
                       mc.player != null && mc.world != null && GameUtils.Options.hideGui() == false &&
                       (Configs.Generic.REQUIRE_SNEAK.getBooleanValue() == false || mc.player.isSneaking()) &&
                        Configs.Hotkeys.REQUIRED_KEY.getKeyBind().isKeyBindHeld();

        // Update the string list renderer to remove MiniHUD's info lines when the HUD is disabled
        if (wasEnabled && this.enabled == false && this.stringListRenderer != null)
        {
            this.stringListRenderer.notifyStringListChanged();
        }

        if (mc.world != null)
        {
            long worldTick = mc.world.getTotalWorldTime();

            if (InfoLine.SPAWNABLE_SUB_CHUNKS.getBooleanValue() &&
                worldTick % Configs.Generic.SPAWNABLE_SUB_CHUNK_CHECK_INTERVAL.getIntegerValue() == 0)
            {
                DataStorage.getInstance().checkQueuedDirtyChunkHeightmaps();
            }

            if ((worldTick % 20) == 0)
            {
                if (InfoLine.MOB_CAPS.getBooleanValue())
                {
                    DataStorage.getInstance().getMobCapData().updateIntegratedServerMobCaps();
                }

                if (RendererToggle.STRUCTURE_BOUNDING_BOXES.isRendererEnabled())
                {
                    DataStorage.getInstance().getStructureStorage().updateStructureDataIfNeeded();
                }
            }
        }
    }

    private void updateLines()
    {
        StringListRendererWidget widget = this.getStringListRenderer();

        if (widget == null)
        {
            return;
        }

        this.lineWrappers.clear();
        this.addedTypes.clear();

        // Get the info line order based on the configs
        List<LinePos> positions = new ArrayList<>();

        for (InfoLine toggle : InfoLine.values())
        {
            if (toggle.getBooleanValue())
            {
                positions.add(new LinePos(toggle.getLineOrder(), toggle));
            }
        }

        Collections.sort(positions);

        for (LinePos pos : positions)
        {
            try
            {
                this.addLine(pos.type);
            }
            catch (Exception e)
            {
                this.addLine(pos.type.getName() + ": exception");
            }
        }

        if (Configs.Generic.SORT_LINES_BY_LENGTH.getBooleanValue())
        {
            Collections.sort(this.lineWrappers);

            if (Configs.Generic.SORT_LINES_REVERSED.getBooleanValue())
            {
                Collections.reverse(this.lineWrappers);
            }
        }

        this.lines.clear();

        for (StringHolder holder : this.lineWrappers)
        {
            this.lines.add(holder.str);
        }

        widget.notifyStringListChanged();
    }

    private void addLine(String text)
    {
        this.lineWrappers.add(new StringHolder(text));
    }

    private void addLine(InfoLine type)
    {
        Minecraft mc = GameUtils.getClient();
        Entity entity = mc.getRenderViewEntity();
        World world = entity.getEntityWorld();
        RayTraceResult hitResult = GameUtils.getHitResult();
        double x = EntityWrap.getX(entity);
        double y = EntityWrap.getY(entity);
        double z = EntityWrap.getZ(entity);
        double bbY = entity.getEntityBoundingBox().minY;
        BlockPos pos = new BlockPos(x, bbY, z);
        DataStorage data = DataStorage.getInstance();

        if (type == InfoLine.FPS)
        {
            this.addLine(String.format("%d fps", this.fps));
        }
        else if (type == InfoLine.MEMORY_USAGE)
        {
            long memMax = Runtime.getRuntime().maxMemory();
            long memTotal = Runtime.getRuntime().totalMemory();
            long memFree = Runtime.getRuntime().freeMemory();
            long memUsed = memTotal - memFree;

            this.addLine(String.format("Mem: % 2d%% %03d/%03dMB | Allocated: % 2d%% %03dMB",
                    memUsed * 100L / memMax,
                    MiscUtils.bytesToMb(memUsed),
                    MiscUtils.bytesToMb(memMax),
                    memTotal * 100L / memMax,
                    MiscUtils.bytesToMb(memTotal)));
        }
        else if (type == InfoLine.TIME_REAL)
        {
            try
            {
                SimpleDateFormat sdf = new SimpleDateFormat(Configs.Generic.REAL_TIME_FORMAT.getValue());
                this.date.setTime(System.currentTimeMillis());
                this.addLine(sdf.format(this.date));
            }
            catch (Exception e)
            {
                this.addLine("Date formatting failed - Invalid date format string?");
            }
        }
        else if (type == InfoLine.TIME_WORLD)
        {
            long current = world.getWorldTime();
            long total = world.getTotalWorldTime();
            this.addLine(String.format("World time: %5d - total: %d", current, total));
        }
        else if (type == InfoLine.TIME_WORLD_FORMATTED)
        {
            try
            {
                long timeDay = world.getWorldTime();
                long day = (int) (timeDay / 24000);
                // 1 tick = 3.6 seconds in MC (0.2777... seconds IRL)
                int dayTicks = (int) (timeDay % 24000);
                int hour = ((dayTicks / 1000) + 6) % 24;
                int min = (int) (dayTicks / 16.666666) % 60;
                int sec = (int) (dayTicks / 0.277777) % 60;

                String str = Configs.Generic.MC_TIME_FORMAT.getValue();
                str = str.replace("{DAY}",  String.format("%d", day));
                str = str.replace("{DAY_1}",String.format("%d", day + 1));
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
        else if (type == InfoLine.TIME_DAY_MODULO)
        {
            int mod = Configs.Generic.TIME_DAY_DIVISOR.getIntegerValue();
            long current = world.getWorldTime() % mod;
            this.addLine(String.format("Day time %% %d: %5d", mod, current));
        }
        else if (type == InfoLine.TIME_TOTAL_MODULO)
        {
            int mod = Configs.Generic.TIME_TOTAL_DIVISOR.getIntegerValue();
            long current = world.getTotalWorldTime() % mod;
            this.addLine(String.format("Total time %% %d: %5d", mod, current));
        }
        else if (type == InfoLine.SERVER_TPS)
        {
            TpsData tpsData = data.getTpsData();

            if (mc.isSingleplayer() && (mc.getIntegratedServer().getTickCounter() % 10) == 0)
            {
                tpsData.updateIntegratedServerTps();
            }

            if (tpsData.getHasValidData())
            {
                this.addLine(tpsData.getFormattedInfoLine());
            }
        }
        else if (type == InfoLine.MOB_CAPS)
        {
            MobCapDataHandler mobCapData = data.getMobCapData();

            if (mc.isSingleplayer() && (mc.getIntegratedServer().getTickCounter() % 100) == 0)
            {
                mobCapData.updateIntegratedServerMobCaps();
            }

            if (mobCapData.getHasValidData())
            {
                this.addLine(mobCapData.getFormattedInfoLine());
            }   
        }
        else if (type == InfoLine.PING)
        {
            // The ping is useless in single player
            if (mc.isSingleplayer() == false)
            {
                NetworkPlayerInfo info = mc.player.connection.getPlayerInfo(mc.player.getUniqueID());

                if (info != null)
                {
                    this.addLine("Ping: " + info.getResponseTime() + " ms");
                }
            }
        }
        else if (type == InfoLine.COORDINATES ||
                 type == InfoLine.DIMENSION)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoLine.COORDINATES) || this.addedTypes.contains(InfoLine.DIMENSION))
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if (InfoLine.COORDINATES.getBooleanValue())
            {
                if (Configs.Generic.COORDINATE_FORMAT_CUSTOMIZED.getBooleanValue())
                {
                    try
                    {
                        str.append(String.format(Configs.Generic.COORDINATE_FORMAT_STRING.getValue(),
                            x, bbY, z));
                    }
                    // Uh oh, someone done goofed their format string... :P
                    catch (Exception e)
                    {
                        str.append("broken coordinate format string!");
                    }
                }
                else
                {
                    str.append(String.format("x: %.1f y: %.1f z: %.1f",
                        x, bbY, z));
                }

                pre = " / ";
            }

            if (InfoLine.DIMENSION.getBooleanValue())
            {
                str.append(String.format("%sDimType ID: %s", pre, WorldUtils.getDimensionAsString(world)));
            }

            this.addLine(str.toString());

            this.addedTypes.add(InfoLine.COORDINATES);
            this.addedTypes.add(InfoLine.DIMENSION);
        }
        else if (type == InfoLine.BLOCK_POS ||
                 type == InfoLine.CHUNK_POS ||
                 type == InfoLine.REGION_FILE)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoLine.BLOCK_POS) ||
                this.addedTypes.contains(InfoLine.CHUNK_POS) ||
                this.addedTypes.contains(InfoLine.REGION_FILE))
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(256);

            if (InfoLine.BLOCK_POS.getBooleanValue())
            {
                str.append(String.format("Block: %d, %d, %d", pos.getX(), pos.getY(), pos.getZ()));
                pre = " / ";
            }

            if (InfoLine.CHUNK_POS.getBooleanValue())
            {
                str.append(pre).append(String.format("Sub-Chunk: %d, %d, %d", pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4));
                pre = " / ";
            }

            if (InfoLine.REGION_FILE.getBooleanValue())
            {
                str.append(pre).append(String.format("Region: r.%d.%d", pos.getX() >> 9, pos.getZ() >> 9));
            }

            this.addLine(str.toString());

            this.addedTypes.add(InfoLine.BLOCK_POS);
            this.addedTypes.add(InfoLine.CHUNK_POS);
            this.addedTypes.add(InfoLine.REGION_FILE);
        }
        else if (type == InfoLine.BLOCK_IN_CHUNK)
        {
            this.addLine(String.format("Block: %d, %d, %d within Sub-Chunk: %d, %d, %d",
                        pos.getX() & 0xF, pos.getY() & 0xF, pos.getZ() & 0xF,
                        pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4));
        }
        else if (type == InfoLine.BLOCK_BREAK_SPEED)
        {
            this.addLine(String.format("BBS: %.2f", DataStorage.getInstance().getBlockBreakingSpeed()));
        }
        else if (type == InfoLine.DISTANCE)
        {
            Vec3d ref = DataStorage.getInstance().getDistanceReferencePoint();
            double dist = MathHelper.sqrt(ref.squareDistanceTo(x, y, z));
            this.addLine(String.format("Distance: %.2f (x: %.2f y: %.2f z: %.2f) [to x: %.2f y: %.2f z: %.2f]",
                    dist, x - ref.x, y - ref.y, z - ref.z, ref.x, ref.y, ref.z));
        }
        else if (type == InfoLine.PLAYER_FACING)
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
        else if (type == InfoLine.LIGHT_LEVEL)
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && mc.world.isBlockLoaded(pos))
            {
                Chunk chunk = mc.world.getChunk(pos);

                if (chunk.isEmpty() == false)
                {
                    this.addLine(String.format("Light: %d (block: %d, sky: %d)",
                            chunk.getLightSubtracted(pos, 0),
                            chunk.getLightFor(EnumSkyBlock.BLOCK, pos),
                            chunk.getLightFor(EnumSkyBlock.SKY, pos)));
                }
            }
        }
        else if (type == InfoLine.PLAYER_YAW_ROTATION ||
                 type == InfoLine.PLAYER_PITCH_ROTATION ||
                 type == InfoLine.SPEED)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoLine.PLAYER_YAW_ROTATION) ||
                this.addedTypes.contains(InfoLine.PLAYER_PITCH_ROTATION) ||
                this.addedTypes.contains(InfoLine.SPEED))
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if (InfoLine.PLAYER_YAW_ROTATION.getBooleanValue())
            {
                str.append(String.format("yaw: %.1f", MathHelper.wrapDegrees(EntityWrap.getYaw(entity))));
                pre = " / ";
            }

            if (InfoLine.PLAYER_PITCH_ROTATION.getBooleanValue())
            {
                str.append(pre).append(String.format("pitch: %.1f", MathHelper.wrapDegrees(EntityWrap.getPitch(entity))));
                pre = " / ";
            }

            if (InfoLine.SPEED.getBooleanValue())
            {
                double dx = x - entity.lastTickPosX;
                double dy = y - entity.lastTickPosY;
                double dz = z - entity.lastTickPosZ;
                double dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
                str.append(pre).append(String.format("speed: %.3f m/s", dist * 20));
            }

            this.addLine(str.toString());

            this.addedTypes.add(InfoLine.PLAYER_YAW_ROTATION);
            this.addedTypes.add(InfoLine.PLAYER_PITCH_ROTATION);
            this.addedTypes.add(InfoLine.SPEED);
        }
        else if (type == InfoLine.SPEED_AXIS)
        {
            double dx = x - entity.lastTickPosX;
            double dy = y - entity.lastTickPosY;
            double dz = z - entity.lastTickPosZ;
            this.addLine(String.format("speed: x: %.3f y: %.3f z: %.3f m/s", dx * 20, dy * 20, dz * 20));
        }
        else if (type == InfoLine.CARPET_WOOL_COUNTERS)
        {
            List<String> lines = DataStorage.getInstance().getWoolCounters().getInfoLines();

            if (lines.isEmpty() == false)
            {
                lines.forEach(this::addLine);
            }
        }
        else if (type == InfoLine.CHUNK_SECTIONS)
        {
            this.addLine(String.format("C: %d", ((RenderGlobalMixin) mc.renderGlobal).minihud_getRenderedChunks()));
        }
        else if (type == InfoLine.CHUNK_SECTIONS_FULL)
        {
            this.addLine(mc.renderGlobal.getDebugInfoRenders());
        }
        else if (type == InfoLine.CHUNK_UPDATES)
        {
            this.addLine(String.format("Chunk updates: %d", RenderChunk.renderChunksUpdated));
        }
        else if (type == InfoLine.CHUNK_UNLOAD_ORDER)
        {
            int bucket = MiscUtils.getChunkUnloadBucket(pos.getX() >> 4, pos.getZ() >> 4);
            String str1 = String.format("Chunk unload bucket: %d", bucket);

            if (Configs.Generic.CHUNK_UNLOAD_BUCKET_HASH_SIZE.getBooleanValue())
            {
                HashSizeType hashType = data.getDroppedChunksHashSizeType();
                str1 += String.format(" - Hash size [%s]: %d", hashType.getDisplayName(), data.getDroppedChunksHashSize());
            }

            this.addLine(str1);
        }
        else if (type == InfoLine.LOADED_CHUNKS_COUNT)
        {
            String chunksClient = mc.world.getProviderName();
            World worldServer = WorldUtils.getBestWorld(mc);

            if (worldServer != null && worldServer != mc.world)
            {
                String chunksServer = worldServer.getChunkProvider().makeString();
                this.addLine(String.format("Server: %s - Client: %s", chunksServer, chunksClient));
            }
            else
            {
                this.addLine(chunksClient);
            }
        }
        else if (type == InfoLine.PARTICLE_COUNT)
        {
            this.addLine(String.format("P: %s", mc.effectRenderer.getStatistics()));
        }
        else if (type == InfoLine.DIFFICULTY)
        {
            if (mc.world.isBlockLoaded(pos))
            {
                DifficultyInstance diff = mc.world.getDifficultyForLocation(pos);

                if (mc.isIntegratedServerRunning() && mc.getIntegratedServer() != null)
                {
                    EntityPlayerMP player = mc.getIntegratedServer().getPlayerList().getPlayerByUUID(mc.player.getUniqueID());

                    if (player != null)
                    {
                        diff = player.world.getDifficultyForLocation(EntityWrap.getEntityBlockPos(player));
                    }
                }

                this.addLine(String.format("Local Difficulty: %.2f // %.2f (Day %d)",
                        diff.getAdditionalDifficulty(), diff.getClampedAdditionalDifficulty(), mc.world.getWorldTime() / 24000L));
            }
        }
        else if (type == InfoLine.BIOME)
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && mc.world.isBlockLoaded(pos))
            {
                Chunk chunk = mc.world.getChunk(pos);

                if (chunk.isEmpty() == false)
                {
                    this.addLine("Biome: " + chunk.getBiome(pos, mc.world.getBiomeProvider()).getBiomeName());
                }
            }
        }
        else if (type == InfoLine.BIOME_REG_NAME)
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && mc.world.isBlockLoaded(pos))
            {
                Chunk chunk = mc.world.getChunk(pos);

                if (chunk.isEmpty() == false)
                {
                    Biome biome = chunk.getBiome(pos, mc.world.getBiomeProvider());
                    ResourceLocation rl = Biome.REGISTRY.getNameForObject(biome);
                    String name = rl != null ? rl.toString() : "?";
                    this.addLine("Biome reg name: " + name);
                }
            }
        }
        else if (type == InfoLine.ENTITIES)
        {
            String ent = mc.renderGlobal.getDebugInfoEntities();

            int p = ent.indexOf(",");

            if (p != -1)
            {
                ent = ent.substring(0, p);
            }

            this.addLine(ent);
        }
        else if (type == InfoLine.BLOCK_ENTITIES)
        {
            this.addLine(String.format("Client world TE - L: %d, T: %d", mc.world.loadedTileEntityList.size(), mc.world.tickableTileEntities.size()));
        }
        else if (type == InfoLine.ENTITIES_CLIENT_WORLD)
        {
            int countClient = mc.world.loadedEntityList.size();

            if (mc.isIntegratedServerRunning())
            {
                World serverWorld = WorldUtils.getBestWorld(mc);

                if (serverWorld instanceof WorldServer)
                {
                    int countServer = serverWorld.loadedEntityList.size();
                    this.addLine(String.format("Entities - Client: %d, Server: %d", countClient, countServer));
                    return;
                }
            }

            this.addLine(String.format("Entities - Client: %d", countClient));
        }
        else if (type == InfoLine.SLIME_CHUNK)
        {
            if (world.provider.isSurfaceWorld() == false)
            {
                return;
            }

            String result;
            int dimension = entity.dimension;

            if (data.isWorldSeedKnown(dimension))
            {
                long seed = data.getWorldSeed(dimension);

                if (MiscUtils.canSlimeSpawnAt(pos.getX(), pos.getZ(), seed))
                {
                    result = "minihud.info_line.slime_chunk.yes";
                }
                else
                {
                    result = "minihud.info_line.slime_chunk.no";
                }
            }
            else
            {
                result = "minihud.info_line.slime_chunk.no_seed";
            }

            this.addLine(StringUtils.translate(result));
        }
        else if (type == InfoLine.LOOKING_AT_ENTITY)
        {
            if (hitResult != null &&
                hitResult.typeOfHit == RayTraceResult.Type.ENTITY &&
                hitResult.entityHit != null)
            {
                Entity target = hitResult.entityHit;

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
        else if (type == InfoLine.ENTITY_REG_NAME)
        {
            if (hitResult != null &&
                hitResult.typeOfHit == RayTraceResult.Type.ENTITY &&
                hitResult.entityHit != null)
            {
                ResourceLocation regName = EntityList.getKey(hitResult.entityHit);

                if (regName != null)
                {
                    this.addLine(String.format("Entity reg name: %s", regName.toString()));
                }
            }
        }
        else if (type == InfoLine.LOOKING_AT_BLOCK ||
                 type == InfoLine.LOOKING_AT_BLOCK_CHUNK)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoLine.LOOKING_AT_BLOCK) ||
                this.addedTypes.contains(InfoLine.LOOKING_AT_BLOCK_CHUNK))
            {
                return;
            }

            if (hitResult != null &&
                hitResult.typeOfHit == RayTraceResult.Type.BLOCK &&
                hitResult.getBlockPos() != null)
            {
                BlockPos lookPos = hitResult.getBlockPos();
                String pre = "";
                StringBuilder str = new StringBuilder(128);

                if (InfoLine.LOOKING_AT_BLOCK.getBooleanValue())
                {
                    str.append(String.format("Looking at block: %d, %d, %d", lookPos.getX(), lookPos.getY(), lookPos.getZ()));
                    pre = " // ";
                }

                if (InfoLine.LOOKING_AT_BLOCK_CHUNK.getBooleanValue())
                {
                    str.append(pre).append(String.format("Block: %d, %d, %d in Sub-Chunk: %d, %d, %d",
                            lookPos.getX() & 0xF, lookPos.getY() & 0xF, lookPos.getZ() & 0xF,
                            lookPos.getX() >> 4, lookPos.getY() >> 4, lookPos.getZ() >> 4));
                }

                this.addLine(str.toString());

                this.addedTypes.add(InfoLine.LOOKING_AT_BLOCK);
                this.addedTypes.add(InfoLine.LOOKING_AT_BLOCK_CHUNK);
            }
        }
        else if (type == InfoLine.BLOCK_PROPS)
        {
            this.getBlockProperties(mc);
        }
        else if (type == InfoLine.SPAWNABLE_SUB_CHUNKS)
        {
            int value = DataStorage.getInstance().getSpawnableSubChunkCountFor(pos.getX() >> 4, pos.getZ() >> 4);

            if (value >= 0)
            {
                this.addLine(String.format("Spawnable sub-chunks: %d (y: 0 - %d)", value, value * 16 - 1));
            }
            else
            {
                this.addLine("Spawnable sub-chunks: <no data>");
            }
        }
    }

    private void getBlockProperties(Minecraft mc)
    {
        RayTraceResult hitResult = GameUtils.getHitResult();

        if (hitResult != null &&
            hitResult.typeOfHit == RayTraceResult.Type.BLOCK &&
            hitResult.getBlockPos() != null)
        {
            BlockPos posLooking = hitResult.getBlockPos();
            IBlockState state = mc.world.getBlockState(posLooking);

            if (mc.world.getWorldType() != WorldType.DEBUG_ALL_BLOCK_STATES)
            {
                state = state.getActualState(mc.world, posLooking);
            }

            this.addLine(String.valueOf(Block.REGISTRY.getNameForObject(state.getBlock())));

            for (String line : BlockUtils.getFormattedBlockStateProperties(state))
            {
                this.addLine(line);
            }
        }
    }

    private static class StringHolder implements Comparable<StringHolder>
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
        private final InfoLine type;

        private LinePos(int position, InfoLine type)
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

            return Integer.compare(this.position, other.position);
        }
    }
}
