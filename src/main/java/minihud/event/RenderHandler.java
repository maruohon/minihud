package minihud.event;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Supplier;
import javax.annotation.Nullable;

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

import malilib.config.value.ScreenLocation;
import malilib.event.PostGameOverlayRenderer;
import malilib.event.PostItemTooltipRenderer;
import malilib.event.PostWorldRenderer;
import malilib.gui.BaseScreen;
import malilib.overlay.InfoArea;
import malilib.overlay.widget.StringListRendererWidget;
import malilib.registry.Registry;
import malilib.render.RenderContext;
import malilib.render.RenderUtils;
import malilib.render.inventory.InventoryRenderUtils;
import malilib.util.StringUtils;
import malilib.util.game.BlockUtils;
import malilib.util.game.WorldUtils;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import malilib.util.game.wrap.RegistryUtils;
import minihud.Reference;
import minihud.config.Configs;
import minihud.config.InfoLineToggle;
import minihud.config.RendererToggle;
import minihud.data.DataStorage;
import minihud.data.DroppedChunks;
import minihud.data.DroppedChunks.HashSizeType;
import minihud.data.MobCapDataHandler;
import minihud.data.TpsDataManager;
import minihud.data.WoolCounters;
import minihud.data.structure.StructureStorage;
import minihud.mixin.info_lines.RenderGlobalMixin;
import minihud.renderer.OverlayRenderer;
import minihud.util.MiscUtils;

public class RenderHandler implements PostGameOverlayRenderer, PostItemTooltipRenderer, PostWorldRenderer
{
    public static final RenderHandler INSTANCE = new RenderHandler();
    private final Supplier<String> profilerSectionSupplier = () -> "MiniHUD_RenderHandler";
    private final Set<InfoLineToggle> addedTypes = new HashSet<>();
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
    public void onPostGameOverlayRender(RenderContext ctx)
    {
        if (this.enabled)
        {
            if (InfoLineToggle.FPS.getBooleanValue())
            {
                this.updateFps();
            }

            long currentTime = System.nanoTime();

            // Only update the text once per game tick
            if (currentTime - this.infoUpdateTime >= 50000000L)
            {
                this.updateLines();
                this.infoUpdateTime = currentTime;
            }
        }
    }

    @Override
    public void onPostRenderItemTooltip(ItemStack stack, int x, int y, RenderContext ctx)
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
                    RenderUtils.renderMapPreview(stack, x, y, z, dimensions, ctx);
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
                InventoryRenderUtils.renderItemInventoryPreview(stack, x, y, z, background, ctx);
            }
        }
    }

    @Override
    public void onPostWorldRender(RenderContext ctx, float tickDelta)
    {
        if (Configs.Generic.OVERLAYS_RENDERING_TOGGLE.getBooleanValue())
        {
            OverlayRenderer.renderOverlays(ctx, tickDelta);
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

            if ((worldTick % 20) == 0)
            {
                if (InfoLineToggle.MOB_CAPS.getBooleanValue())
                {
                    MobCapDataHandler.INSTANCE.updateIntegratedServerMobCaps();
                }

                if (RendererToggle.STRUCTURE_BOUNDING_BOXES.isRendererEnabled())
                {
                    StructureStorage.INSTANCE.updateStructureDataIfNeeded();
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

        for (InfoLineToggle toggle : InfoLineToggle.values())
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

    private void addLine(InfoLineToggle type)
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

        if (type == InfoLineToggle.FPS)
        {
            this.addLine(String.format("%d fps", this.fps));
        }
        else if (type == InfoLineToggle.MEMORY_USAGE)
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
        else if (type == InfoLineToggle.TIME_REAL)
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
        else if (type == InfoLineToggle.TIME_WORLD)
        {
            long current = world.getWorldTime();
            long total = world.getTotalWorldTime();
            this.addLine(String.format("World time: %5d - total: %d", current, total));
        }
        else if (type == InfoLineToggle.TIME_WORLD_FORMATTED)
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
        else if (type == InfoLineToggle.TIME_DAY_MODULO)
        {
            int mod = Configs.Generic.TIME_DAY_DIVISOR.getIntegerValue();
            long current = world.getWorldTime() % mod;
            this.addLine(String.format("Day time %% %d: %5d", mod, current));
        }
        else if (type == InfoLineToggle.TIME_TOTAL_MODULO)
        {
            int mod = Configs.Generic.TIME_TOTAL_DIVISOR.getIntegerValue();
            long current = world.getTotalWorldTime() % mod;
            this.addLine(String.format("Total time %% %d: %5d", mod, current));
        }
        else if (type == InfoLineToggle.SERVER_TPS)
        {
            TpsDataManager tpsData = TpsDataManager.INSTANCE;

            if (mc.isSingleplayer() && (mc.getIntegratedServer().getTickCounter() % 10) == 0)
            {
                tpsData.updateIntegratedServerTps();
            }

            if (tpsData.getHasValidData())
            {
                this.addLine(tpsData.getFormattedInfoLine());
            }
        }
        else if (type == InfoLineToggle.MOB_CAPS)
        {
            MobCapDataHandler mobCapData = MobCapDataHandler.INSTANCE;

            if (mc.isSingleplayer() && (mc.getIntegratedServer().getTickCounter() % 100) == 0)
            {
                mobCapData.updateIntegratedServerMobCaps();
            }

            if (mobCapData.getHasValidData())
            {
                this.addLine(mobCapData.getFormattedInfoLine());
            }
        }
        else if (type == InfoLineToggle.PING)
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
        else if (type == InfoLineToggle.COORDINATES ||
                 type == InfoLineToggle.DIMENSION)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoLineToggle.COORDINATES) || this.addedTypes.contains(InfoLineToggle.DIMENSION))
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if (InfoLineToggle.COORDINATES.getBooleanValue())
            {
                if (Configs.Generic.COORDINATE_FORMAT_CUSTOMIZED.getBooleanValue())
                {
                    try
                    {
                        // Follow a python like formatted string input, parse then make it java
                        StringBuilder javaFormat = new StringBuilder(128);
                        //                                      aka stuff between {}
                        Matcher matcher = Pattern.compile("\\{([^\\}]+)\\}").matcher(line);

                        int from = 0, to = 0;
                        float[] vals = {x, bbY, z};

                        for (int i = 0; i < 3; i++)
                        {
                            if (!matcher.find(from)) 
                            {
                                break;
                            }
                            
                            to = matcher.start();
                            // Add the content between the {x:%.2f}
                            javaFormat.append(line.substring(from, to));

                            String[] parts = matcher.group(1).split(":");
                            String varname = parts[0], format = parts[1];

                            if (varname.equals("x"))
                            {
                                vals[i] = x;
                            }
                            else if (varname.equals("y"))
                            {
                                vals[i] = bbY;
                            } else if (varname.equals("z"))
                            {
                                vals[i] = z;
                            }
                            javaFormat.append(format);

                            from = matcher.end();
                        }

                        str.append(String.format(javaFormat.toString(), vals[0], vals[1], vals[2]));
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

            if (InfoLineToggle.DIMENSION.getBooleanValue())
            {
                str.append(String.format("%sDimType ID: %s", pre, WorldUtils.getDimensionIdAsString(world)));
            }

            this.addLine(str.toString());

            this.addedTypes.add(InfoLineToggle.COORDINATES);
            this.addedTypes.add(InfoLineToggle.DIMENSION);
        }
        else if (type == InfoLineToggle.BLOCK_POS ||
                 type == InfoLineToggle.CHUNK_POS ||
                 type == InfoLineToggle.REGION_FILE)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoLineToggle.BLOCK_POS) ||
                this.addedTypes.contains(InfoLineToggle.CHUNK_POS) ||
                this.addedTypes.contains(InfoLineToggle.REGION_FILE))
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(256);

            if (InfoLineToggle.BLOCK_POS.getBooleanValue())
            {
                str.append(String.format("Block: %d, %d, %d", pos.getX(), pos.getY(), pos.getZ()));
                pre = " / ";
            }

            if (InfoLineToggle.CHUNK_POS.getBooleanValue())
            {
                str.append(pre).append(String.format("Sub-Chunk: %d, %d, %d", pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4));
                pre = " / ";
            }

            if (InfoLineToggle.REGION_FILE.getBooleanValue())
            {
                str.append(pre).append(String.format("Region: r.%d.%d", pos.getX() >> 9, pos.getZ() >> 9));
            }

            this.addLine(str.toString());

            this.addedTypes.add(InfoLineToggle.BLOCK_POS);
            this.addedTypes.add(InfoLineToggle.CHUNK_POS);
            this.addedTypes.add(InfoLineToggle.REGION_FILE);
        }
        else if (type == InfoLineToggle.BLOCK_IN_CHUNK)
        {
            this.addLine(String.format("Block: %d, %d, %d within Sub-Chunk: %d, %d, %d",
                        pos.getX() & 0xF, pos.getY() & 0xF, pos.getZ() & 0xF,
                        pos.getX() >> 4, pos.getY() >> 4, pos.getZ() >> 4));
        }
        else if (type == InfoLineToggle.BLOCK_BREAK_SPEED)
        {
            this.addLine(String.format("BBS: %.2f", DataStorage.getInstance().getBlockBreakingSpeed()));
        }
        else if (type == InfoLineToggle.DISTANCE)
        {
            Vec3d ref = DataStorage.getInstance().getDistanceReferencePoint();
            double dist = MathHelper.sqrt(ref.squareDistanceTo(x, y, z));
            this.addLine(String.format("Distance: %.2f (x: %.2f y: %.2f z: %.2f) [to x: %.2f y: %.2f z: %.2f]",
                    dist, x - ref.x, y - ref.y, z - ref.z, ref.x, ref.y, ref.z));
        }
        else if (type == InfoLineToggle.PLAYER_FACING)
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
        else if (type == InfoLineToggle.LIGHT_LEVEL)
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
        else if (type == InfoLineToggle.PLAYER_YAW_ROTATION ||
                 type == InfoLineToggle.PLAYER_PITCH_ROTATION ||
                 type == InfoLineToggle.SPEED)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoLineToggle.PLAYER_YAW_ROTATION) ||
                this.addedTypes.contains(InfoLineToggle.PLAYER_PITCH_ROTATION) ||
                this.addedTypes.contains(InfoLineToggle.SPEED))
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if (InfoLineToggle.PLAYER_YAW_ROTATION.getBooleanValue())
            {
                str.append(String.format("yaw: %.1f", MathHelper.wrapDegrees(EntityWrap.getYaw(entity))));
                pre = " / ";
            }

            if (InfoLineToggle.PLAYER_PITCH_ROTATION.getBooleanValue())
            {
                str.append(pre).append(String.format("pitch: %.1f", MathHelper.wrapDegrees(EntityWrap.getPitch(entity))));
                pre = " / ";
            }

            if (InfoLineToggle.SPEED.getBooleanValue())
            {
                double dx = x - entity.lastTickPosX;
                double dy = y - entity.lastTickPosY;
                double dz = z - entity.lastTickPosZ;
                double dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
                str.append(pre).append(String.format("speed: %.3f m/s", dist * 20));
            }

            this.addLine(str.toString());

            this.addedTypes.add(InfoLineToggle.PLAYER_YAW_ROTATION);
            this.addedTypes.add(InfoLineToggle.PLAYER_PITCH_ROTATION);
            this.addedTypes.add(InfoLineToggle.SPEED);
        }
        else if (type == InfoLineToggle.SPEED_AXIS)
        {
            double dx = x - entity.lastTickPosX;
            double dy = y - entity.lastTickPosY;
            double dz = z - entity.lastTickPosZ;
            this.addLine(String.format("speed: x: %.3f y: %.3f z: %.3f m/s", dx * 20, dy * 20, dz * 20));
        }
        else if (type == InfoLineToggle.CARPET_WOOL_COUNTERS)
        {
            List<String> lines = WoolCounters.INSTANCE.getInfoLines();

            if (lines.isEmpty() == false)
            {
                lines.forEach(this::addLine);
            }
        }
        else if (type == InfoLineToggle.CHUNK_SECTIONS)
        {
            this.addLine(String.format("C: %d", ((RenderGlobalMixin) mc.renderGlobal).minihud_getRenderedChunks()));
        }
        else if (type == InfoLineToggle.CHUNK_SECTIONS_FULL)
        {
            this.addLine(mc.renderGlobal.getDebugInfoRenders());
        }
        else if (type == InfoLineToggle.CHUNK_UPDATES)
        {
            this.addLine(String.format("Chunk updates: %d", RenderChunk.renderChunksUpdated));
        }
        else if (type == InfoLineToggle.CHUNK_UNLOAD_ORDER)
        {
            int bucket = MiscUtils.getChunkUnloadBucket(pos.getX() >> 4, pos.getZ() >> 4);
            String str1 = String.format("Chunk unload bucket: %d", bucket);

            if (Configs.Generic.CHUNK_UNLOAD_BUCKET_HASH_SIZE.getBooleanValue())
            {
                HashSizeType hashType = DroppedChunks.getDroppedChunksHashSizeType();
                str1 += String.format(" - Hash size [%s]: %d", hashType.getDisplayName(), DroppedChunks.getDroppedChunksHashSize());
            }

            this.addLine(str1);
        }
        else if (type == InfoLineToggle.LOADED_CHUNKS_COUNT)
        {
            String chunksClient = mc.world.getProviderName();
            World worldServer = WorldUtils.getBestWorld();

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
        else if (type == InfoLineToggle.PARTICLE_COUNT)
        {
            this.addLine(String.format("P: %s", mc.effectRenderer.getStatistics()));
        }
        else if (type == InfoLineToggle.DIFFICULTY)
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
        else if (type == InfoLineToggle.BIOME)
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
        else if (type == InfoLineToggle.BIOME_REG_NAME)
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
        else if (type == InfoLineToggle.ENTITIES)
        {
            String ent = mc.renderGlobal.getDebugInfoEntities();

            int p = ent.indexOf(",");

            if (p != -1)
            {
                ent = ent.substring(0, p);
            }

            this.addLine(ent);
        }
        else if (type == InfoLineToggle.BLOCK_ENTITIES)
        {
            this.addLine(String.format("Client world TE - L: %d, T: %d", mc.world.loadedTileEntityList.size(), mc.world.tickableTileEntities.size()));
        }
        else if (type == InfoLineToggle.ENTITIES_CLIENT_WORLD)
        {
            int countClient = mc.world.loadedEntityList.size();

            if (mc.isIntegratedServerRunning())
            {
                World serverWorld = WorldUtils.getBestWorld();

                if (serverWorld instanceof WorldServer)
                {
                    int countServer = serverWorld.loadedEntityList.size();
                    this.addLine(String.format("Entities - Client: %d, Server: %d", countClient, countServer));
                    return;
                }
            }

            this.addLine(String.format("Entities - Client: %d", countClient));
        }
        else if (type == InfoLineToggle.SLIME_CHUNK)
        {
            if (world.provider.isSurfaceWorld() == false)
            {
                return;
            }

            String result;

            if (data.isWorldSeedKnown(world))
            {
                long seed = data.getWorldSeed(world);

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
        else if (type == InfoLineToggle.LOOKING_AT_ENTITY)
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
        else if (type == InfoLineToggle.ENTITY_REG_NAME)
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
        else if (type == InfoLineToggle.LOOKING_AT_BLOCK ||
                 type == InfoLineToggle.LOOKING_AT_BLOCK_CHUNK)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoLineToggle.LOOKING_AT_BLOCK) ||
                this.addedTypes.contains(InfoLineToggle.LOOKING_AT_BLOCK_CHUNK))
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

                if (InfoLineToggle.LOOKING_AT_BLOCK.getBooleanValue())
                {
                    str.append(String.format("Looking at block: %d, %d, %d", lookPos.getX(), lookPos.getY(), lookPos.getZ()));
                    pre = " // ";
                }

                if (InfoLineToggle.LOOKING_AT_BLOCK_CHUNK.getBooleanValue())
                {
                    str.append(pre).append(String.format("Block: %d, %d, %d in Sub-Chunk: %d, %d, %d",
                            lookPos.getX() & 0xF, lookPos.getY() & 0xF, lookPos.getZ() & 0xF,
                            lookPos.getX() >> 4, lookPos.getY() >> 4, lookPos.getZ() >> 4));
                }

                this.addLine(str.toString());

                this.addedTypes.add(InfoLineToggle.LOOKING_AT_BLOCK);
                this.addedTypes.add(InfoLineToggle.LOOKING_AT_BLOCK_CHUNK);
            }
        }
        else if (type == InfoLineToggle.BLOCK_PROPS)
        {
            this.getBlockProperties(mc);
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

            this.addLine(RegistryUtils.getBlockIdStr(state.getBlock()));

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
        private final InfoLineToggle type;

        private LinePos(int position, InfoLineToggle type)
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
