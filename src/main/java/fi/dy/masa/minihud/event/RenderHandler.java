package fi.dy.masa.minihud.event;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import com.mojang.blaze3d.platform.GlStateManager;
import fi.dy.masa.malilib.config.HudAlignment;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.interfaces.IRenderer;
import fi.dy.masa.malilib.render.RenderUtils;
import fi.dy.masa.malilib.util.WorldUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.mixin.IMixinWorldRenderer;
import fi.dy.masa.minihud.renderer.OverlayRenderer;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;
import net.minecraft.block.BlockState;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.chunk.ChunkRenderer;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.state.property.DirectionProperty;
import net.minecraft.state.property.Property;
import net.minecraft.util.BlockHitResult;
import net.minecraft.util.EntityHitResult;
import net.minecraft.util.HitResult;
import net.minecraft.util.Identifier;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.MathHelper;
import net.minecraft.util.registry.Registry;
import net.minecraft.world.LightType;
import net.minecraft.world.LocalDifficulty;
import net.minecraft.world.World;
import net.minecraft.world.biome.Biome;
import net.minecraft.world.chunk.ChunkPos;
import net.minecraft.world.chunk.WorldChunk;
import net.minecraft.world.chunk.light.LightingProvider;
import net.minecraft.world.dimension.DimensionType;

public class RenderHandler implements IRenderer
{
    private static final RenderHandler INSTANCE = new RenderHandler();
    private final DataStorage data;
    private final Date date;
    private int fps;
    private int fpsCounter;
    private long fpsUpdateTime = System.currentTimeMillis();
    private long infoUpdateTime;
    private double fontScale = 0.5d;
    private Set<InfoToggle> addedTypes = new HashSet<>();

    private final List<StringHolder> lineWrappers = new ArrayList<>();
    private final List<String> lines = new ArrayList<>();

    public RenderHandler()
    {
        this.data = DataStorage.getInstance();
        this.date = new Date();
    }

    public static RenderHandler getInstance()
    {
        return INSTANCE;
    }

    public DataStorage getDataStorage()
    {
        return this.data;
    }

    public static void fixDebugRendererState()
    {
        if (Configs.Generic.FIX_VANILLA_DEBUG_RENDERERS.getBooleanValue())
        {
            GlStateManager.disableLighting();
            //GlStateManager.color(1, 1, 1, 1);
            //OpenGlHelper.setLightmapTextureCoords(OpenGlHelper.lightmapTexUnit, 240f, 240f);
        }
    }

    @Override
    public void onRenderGameOverlayPost(float partialTicks)
    {
        MinecraftClient mc = MinecraftClient.getInstance();

        if (Configs.Generic.ENABLED.getBooleanValue() &&
            mc.options.debugEnabled == false &&
            mc.player != null &&
            (Configs.Generic.REQUIRE_SNEAK.getBooleanValue() == false || mc.player.isSneaking()) &&
            (Configs.Generic.REQUIRED_KEY.getKeybind().isValid() == false || Configs.Generic.REQUIRED_KEY.getKeybind().isKeybindHeld()))
        {
            if (InfoToggle.FPS.getBooleanValue())
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

            int x = Configs.Generic.TEXT_POS_X.getIntegerValue();
            int y = Configs.Generic.TEXT_POS_Y.getIntegerValue();
            int textColor = Configs.Colors.TEXT_COLOR.getIntegerValue();
            int bgColor = Configs.Colors.TEXT_BACKGROUND_COLOR.getIntegerValue();
            HudAlignment alignment = (HudAlignment) Configs.Generic.HUD_ALIGNMENT.getOptionListValue();
            boolean useBackground = Configs.Generic.USE_TEXT_BACKGROUND.getBooleanValue();
            boolean useShadow = Configs.Generic.USE_FONT_SHADOW.getBooleanValue();

            RenderUtils.renderText(mc, x, y, this.fontScale, textColor, bgColor, alignment, useBackground, useShadow, this.lines);
        }
    }

    @Override
    public void onRenderWorldLast(float partialTicks)
    {
        MinecraftClient mc = MinecraftClient.getInstance();

        if (Configs.Generic.ENABLED.getBooleanValue() && mc.player != null)
        {
            OverlayRenderer.renderOverlays(mc, partialTicks);
        }
    }

    public void setFontScale(double scale)
    {
        this.fontScale = MathHelper.clamp(scale, 0, 10D);
    }

    public int getSubtitleOffset()
    {
        HudAlignment align = (HudAlignment) Configs.Generic.HUD_ALIGNMENT.getOptionListValue();

        if (align == HudAlignment.BOTTOM_RIGHT)
        {
            MinecraftClient mc = MinecraftClient.getInstance();
            int offset = (int) (this.lineWrappers.size() * (mc.fontRenderer.fontHeight + 2) * this.fontScale);

            return -(offset - 16);
        }

        return 0;
    }

    private void updateFps()
    {
        this.fpsCounter++;
        long time = System.currentTimeMillis();

        if (time >= (this.fpsUpdateTime + 1000L))
        {
            this.fpsUpdateTime = time;
            this.fps = this.fpsCounter;
            this.fpsCounter = 0;
        }
    }

    private void updateLines()
    {
        this.lineWrappers.clear();
        this.addedTypes.clear();

        // Get the info line order based on the configs
        List<LinePos> positions = new ArrayList<LinePos>();

        for (InfoToggle toggle : InfoToggle.values())
        {
            if (toggle.getBooleanValue())
            {
                positions.add(new LinePos(toggle.getIntegerValue(), toggle));
            }
        }

        Collections.sort(positions);

        for (LinePos pos : positions)
        {
            this.addLine(pos.type);
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
    }

    private void addLine(String text)
    {
        this.lineWrappers.add(new StringHolder(text));
    }

    private void addLine(InfoToggle type)
    {
        MinecraftClient mc = MinecraftClient.getInstance();
        Entity entity = mc.getCameraEntity();
        World world = entity.getEntityWorld();
        BlockPos pos = new BlockPos(entity.x, entity.getBoundingBox().minY, entity.z);
        ChunkPos chunkPos = new ChunkPos(pos);

        if (type == InfoToggle.FPS)
        {
            this.addLine(String.format("%d fps", this.fps));
        }
        else if (type == InfoToggle.MEMORY_USAGE)
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
        else if (type == InfoToggle.TIME_REAL)
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
        else if (type == InfoToggle.TIME_WORLD)
        {
            long current = world.getTimeOfDay();
            long total = world.getTime();
            this.addLine(String.format("World time: %5d - total: %d", current, total));
        }
        else if (type == InfoToggle.TIME_WORLD_FORMATTED)
        {
            try
            {
                long timeDay = (int) world.getTimeOfDay();
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
        else if (type == InfoToggle.SERVER_TPS)
        {
            if (mc.isIntegratedServerRunning() && (mc.getServer().getTicks() % 10) == 0)
            {
                this.data.updateIntegratedServerTPS();
            }

            if (this.data.isServerTPSValid())
            {
                double tps = this.data.getServerTPS();
                double mspt = this.data.getServerMSPT();
                String rst = GuiBase.TXT_RST;
                String preTps = tps >= 20.0D ? GuiBase.TXT_GREEN : GuiBase.TXT_RED;
                String preMspt;

                // Carpet server and integrated server have actual meaningful MSPT data available
                if (this.data.isCarpetServer() || mc.isInSingleplayer())
                {
                    if      (mspt <= 40) { preMspt = GuiBase.TXT_GREEN; }
                    else if (mspt <= 45) { preMspt = GuiBase.TXT_YELLOW; }
                    else if (mspt <= 50) { preMspt = GuiBase.TXT_GOLD; }
                    else                 { preMspt = GuiBase.TXT_RED; }

                    this.addLine(String.format("Server TPS: %s%.1f%s MSPT: %s%.1f%s", preTps, tps, rst, preMspt, mspt, rst));
                }
                else
                {
                    if (mspt <= 51) { preMspt = GuiBase.TXT_GREEN; }
                    else            { preMspt = GuiBase.TXT_RED; }

                    this.addLine(String.format("Server TPS: %s%.1f%s (MSPT*: %s%.1f%s)", preTps, tps, rst, preMspt, mspt, rst));
                }
            }
            else
            {
                this.addLine("Server TPS: <no valid data>");
            }
        }
        else if (type == InfoToggle.COORDINATES ||
                 type == InfoToggle.DIMENSION)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoToggle.COORDINATES) || this.addedTypes.contains(InfoToggle.DIMENSION))
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if (InfoToggle.COORDINATES.getBooleanValue())
            {
                if (Configs.Generic.USE_CUSTOMIZED_COORDINATES.getBooleanValue())
                {
                    try
                    {
                        str.append(String.format(Configs.Generic.COORDINATE_FORMAT_STRING.getStringValue(),
                            entity.x, entity.getBoundingBox().minY, entity.z));
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
                        entity.x, entity.getBoundingBox().minY, entity.z));
                }

                pre = " / ";
            }

            if (InfoToggle.DIMENSION.getBooleanValue())
            {
                int dimension = world.dimension.getType().getRawId();
                str.append(String.format(String.format("%sDimensionType ID: %d", pre, dimension)));
            }

            this.addLine(str.toString());

            this.addedTypes.add(InfoToggle.COORDINATES);
            this.addedTypes.add(InfoToggle.DIMENSION);
        }
        else if (type == InfoToggle.BLOCK_POS ||
                 type == InfoToggle.CHUNK_POS ||
                 type == InfoToggle.REGION_FILE)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoToggle.BLOCK_POS) ||
                this.addedTypes.contains(InfoToggle.CHUNK_POS) ||
                this.addedTypes.contains(InfoToggle.REGION_FILE))
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(256);

            if (InfoToggle.BLOCK_POS.getBooleanValue())
            {
                str.append(String.format("Block: %d, %d, %d", pos.getX(), pos.getY(), pos.getZ()));
                pre = " / ";
            }

            if (InfoToggle.CHUNK_POS.getBooleanValue())
            {
                str.append(pre).append(String.format("Sub-Chunk: %d, %d, %d", chunkPos.x, pos.getY() >> 4, chunkPos.z));
                pre = " / ";
            }

            if (InfoToggle.REGION_FILE.getBooleanValue())
            {
                str.append(pre).append(String.format("Region: r.%d.%d", pos.getX() >> 9, pos.getZ() >> 9));
            }

            this.addLine(str.toString());

            this.addedTypes.add(InfoToggle.BLOCK_POS);
            this.addedTypes.add(InfoToggle.CHUNK_POS);
            this.addedTypes.add(InfoToggle.REGION_FILE);
        }
        else if (type == InfoToggle.BLOCK_IN_CHUNK)
        {
            this.addLine(String.format("Block: %d, %d, %d within Sub-Chunk: %d, %d, %d",
                        pos.getX() & 0xF, pos.getY() & 0xF, pos.getZ() & 0xF,
                        chunkPos.x, pos.getY() >> 4, chunkPos.z));
        }
        else if (type == InfoToggle.FACING)
        {
            Direction facing = entity.getHorizontalFacing();
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
        else if (type == InfoToggle.LIGHT_LEVEL)
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && mc.world.isBlockLoaded(pos))
            {
                WorldChunk chunk = mc.world.getWorldChunk(pos);

                if (chunk.isEmpty() == false)
                {
                    this.addLine(String.format("Client Light: %d (block: %d, sky: %d)",
                            chunk.getLightLevel(pos, 0),
                            mc.world.getLightLevel(LightType.BLOCK_LIGHT, pos),
                            mc.world.getLightLevel(LightType.SKY_LIGHT, pos)));

                    World bestWorld = WorldUtils.getBestWorld(mc);
                    WorldChunk serverChunk = WorldUtils.getBestChunk(chunkPos.x, chunkPos.z, mc);

                    if (serverChunk != null)
                    {
                        LightingProvider lightingProvider = bestWorld.getChunkManager().getLightingProvider();
                        this.addLine(String.format("Server Light: (%d sky, %d block)", lightingProvider.get(LightType.SKY_LIGHT).getLightLevel(pos), lightingProvider.get(LightType.BLOCK_LIGHT).getLightLevel(pos)));
                    }
                }
            }
        }
        else if (type == InfoToggle.ROTATION_YAW ||
                 type == InfoToggle.ROTATION_PITCH ||
                 type == InfoToggle.SPEED)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoToggle.ROTATION_YAW) ||
                this.addedTypes.contains(InfoToggle.ROTATION_PITCH) ||
                this.addedTypes.contains(InfoToggle.SPEED))
            {
                return;
            }

            String pre = "";
            StringBuilder str = new StringBuilder(128);

            if (InfoToggle.ROTATION_YAW.getBooleanValue())
            {
                str.append(String.format("yaw: %.1f", MathHelper.wrapDegrees(entity.yaw)));
                pre = " / ";
            }

            if (InfoToggle.ROTATION_PITCH.getBooleanValue())
            {
                str.append(pre).append(String.format("pitch: %.1f", MathHelper.wrapDegrees(entity.pitch)));
                pre = " / ";
            }

            if (InfoToggle.SPEED.getBooleanValue())
            {
                double dx = entity.x - entity.prevRenderX;
                double dy = entity.y - entity.prevRenderY;
                double dz = entity.z - entity.prevRenderZ;
                double dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
                str.append(pre).append(String.format("speed: %.3f m/s", dist * 20));
            }

            this.addLine(str.toString());

            this.addedTypes.add(InfoToggle.ROTATION_YAW);
            this.addedTypes.add(InfoToggle.ROTATION_PITCH);
            this.addedTypes.add(InfoToggle.SPEED);
        }
        else if (type == InfoToggle.SPEED_AXIS)
        {
            double dx = entity.x - entity.prevRenderX;
            double dy = entity.y - entity.prevRenderY;
            double dz = entity.z - entity.prevRenderZ;
            this.addLine(String.format("speed: x: %.3f y: %.3f z: %.3f m/s", dx * 20, dy * 20, dz * 20));
        }
        else if (type == InfoToggle.CHUNK_SECTIONS)
        {
            this.addLine(String.format("C: %d", ((IMixinWorldRenderer) mc.worldRenderer).getRenderedChunksInvoker()));
        }
        else if (type == InfoToggle.CHUNK_SECTIONS_FULL)
        {
            this.addLine(mc.worldRenderer.getChunksDebugString());
        }
        else if (type == InfoToggle.CHUNK_UPDATES)
        {
            this.addLine(String.format("Chunk updates: %d", ChunkRenderer.chunkUpdateCount));
        }
        else if (type == InfoToggle.MP_CHUNK_CACHE)
        {
            this.addLine(mc.world.getChunkProviderStatus());
        }
        else if (type == InfoToggle.PARTICLE_COUNT)
        {
            this.addLine(String.format("P: %s", mc.particleManager.getDebugString()));
        }
        else if (type == InfoToggle.DIFFICULTY)
        {
            if (mc.world.isBlockLoaded(pos))
            {
                long chunkInhabitedTime = 0L;
                float moonPhaseFactor = 0.0F;
                WorldChunk serverChunk = WorldUtils.getBestChunk(chunkPos.x, chunkPos.z, mc);

                if (serverChunk != null)
                {
                    moonPhaseFactor = mc.world.method_8391();
                    chunkInhabitedTime = serverChunk.getInhabitedTime();
                }

                LocalDifficulty diff = new LocalDifficulty(mc.world.getDifficulty(), mc.world.getTimeOfDay(), chunkInhabitedTime, moonPhaseFactor);
                this.addLine(String.format("Local Difficulty: %.2f // %.2f (Day %d)",
                        diff.getLocalDifficulty(), diff.getClampedLocalDifficulty(), mc.world.getTimeOfDay() / 24000L));
            }
        }
        else if (type == InfoToggle.BIOME)
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && mc.world.isBlockLoaded(pos))
            {
                WorldChunk chunk = mc.world.getWorldChunk(pos);

                if (chunk.isEmpty() == false)
                {
                    this.addLine("Biome: " + chunk.getBiome(pos).getTextComponent().getString());
                }
            }
        }
        else if (type == InfoToggle.BIOME_REG_NAME)
        {
            // Prevent a crash when outside of world
            if (pos.getY() >= 0 && pos.getY() < 256 && mc.world.isBlockLoaded(pos))
            {
                WorldChunk chunk = mc.world.getWorldChunk(pos);

                if (chunk.isEmpty() == false)
                {
                    Biome biome = chunk.getBiome(pos);
                    Identifier rl = Registry.BIOME.getId(biome);
                    String name = rl != null ? rl.toString() : "?";
                    this.addLine("Biome reg name: " + name);
                }
            }
        }
        else if (type == InfoToggle.ENTITIES)
        {
            String ent = mc.worldRenderer.getEntitiesDebugString();

            int p = ent.indexOf(",");

            if (p != -1)
            {
                ent = ent.substring(0, p);
            }

            this.addLine(ent);
        }
        else if (type == InfoToggle.SLIME_CHUNK)
        {
            if (world.dimension.hasVisibleSky() == false)
            {
                return;
            }

            String result;
            DimensionType dimension = entity.dimension;

            if (this.data.isWorldSeedKnown(dimension))
            {
                long seed = this.data.getWorldSeed(dimension);

                if (MiscUtils.canSlimeSpawnAt(pos.getX(), pos.getZ(), seed))
                {
                    result = GuiBase.TXT_GREEN + "YES" + GuiBase.TXT_RST;
                }
                else
                {
                    result = GuiBase.TXT_RED + "NO" + GuiBase.TXT_RST;
                }
            }
            else
            {
                result = "<world seed not known>";
            }

            this.addLine("Slime chunk: " + result);
        }
        else if (type == InfoToggle.LOOKING_AT_ENTITY)
        {
            if (mc.hitResult != null && mc.hitResult.getType() == HitResult.Type.ENTITY)
            {
                Entity lookedEntity = ((EntityHitResult) mc.hitResult).getEntity();

                if (lookedEntity instanceof LivingEntity)
                {
                    LivingEntity living = (LivingEntity) lookedEntity;
                    this.addLine(String.format("Entity: %s - HP: %.1f / %.1f",
                            living.getName().getString(), living.getHealth(), living.getHealthMaximum()));
                }
                else
                {
                    this.addLine(String.format("Entity: %s", lookedEntity.getName().getString()));
                }
            }
        }
        else if (type == InfoToggle.ENTITY_REG_NAME)
        {
            if (mc.hitResult != null && mc.hitResult.getType() == HitResult.Type.ENTITY)
            {
                Entity lookedEntity = ((EntityHitResult) mc.hitResult).getEntity();
                Identifier regName = EntityType.getId(lookedEntity.getType());

                if (regName != null)
                {
                    this.addLine(String.format("Entity reg name: %s", regName.toString()));
                }
            }
        }
        else if (type == InfoToggle.LOOKING_AT_BLOCK ||
                 type == InfoToggle.LOOKING_AT_BLOCK_CHUNK)
        {
            // Don't add the same line multiple times
            if (this.addedTypes.contains(InfoToggle.LOOKING_AT_BLOCK) ||
                this.addedTypes.contains(InfoToggle.LOOKING_AT_BLOCK_CHUNK))
            {
                return;
            }

            if (mc.hitResult != null && mc.hitResult.getType() == HitResult.Type.BLOCK)
            {
                BlockPos lookPos = ((BlockHitResult) mc.hitResult).getBlockPos();
                String pre = "";
                StringBuilder str = new StringBuilder(128);

                if (InfoToggle.LOOKING_AT_BLOCK.getBooleanValue())
                {
                    str.append(String.format("Looking at block: %d, %d, %d", lookPos.getX(), lookPos.getY(), lookPos.getZ()));
                    pre = " // ";
                }

                if (InfoToggle.LOOKING_AT_BLOCK_CHUNK.getBooleanValue())
                {
                    str.append(pre).append(String.format("Block: %d, %d, %d in Sub-Chunk: %d, %d, %d",
                            lookPos.getX() & 0xF, lookPos.getY() & 0xF, lookPos.getZ() & 0xF,
                            lookPos.getX() >> 4, lookPos.getY() >> 4, lookPos.getZ() >> 4));
                }

                this.addLine(str.toString());

                this.addedTypes.add(InfoToggle.LOOKING_AT_BLOCK);
                this.addedTypes.add(InfoToggle.LOOKING_AT_BLOCK_CHUNK);
            }
        }
        else if (type == InfoToggle.BLOCK_PROPS)
        {
            this.getBlockProperties(mc);
        }
    }

    @SuppressWarnings("unchecked")
    private <T extends Comparable<T>> void getBlockProperties(MinecraftClient mc)
    {
        if (mc.hitResult != null && mc.hitResult.getType() == HitResult.Type.BLOCK)
        {
            BlockPos posLooking = ((BlockHitResult) mc.hitResult).getBlockPos();
            BlockState state = mc.world.getBlockState(posLooking);
            Identifier rl = Registry.BLOCK.getId(state.getBlock());

            this.addLine(rl != null ? rl.toString() : "<null>");

            for (Entry <Property<?>, Comparable<?>> entry : state.getEntries().entrySet())
            {
                Property<T> property = (Property<T>) entry.getKey();
                T value = (T) entry.getValue();
                String valueName = property.getValueAsString(value);

                if (property instanceof DirectionProperty)
                {
                    valueName = GuiBase.TXT_GOLD + valueName;
                }
                else if (Boolean.TRUE.equals(value))
                {
                    valueName = GuiBase.TXT_GREEN + valueName;
                }
                else if (Boolean.FALSE.equals(value))
                {
                    valueName = GuiBase.TXT_RED + valueName;
                }
                else if (Integer.class.equals(property.getValueClass()))
                {
                    valueName = GuiBase.TXT_GREEN + valueName;
                }

                this.addLine(property.getName() + ": " + valueName);
            }
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
        private final InfoToggle type;

        private LinePos(int position, InfoToggle type)
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
