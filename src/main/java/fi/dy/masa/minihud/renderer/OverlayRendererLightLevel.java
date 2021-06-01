package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.block.BlockState;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.GameRenderer;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.fluid.FluidState;
import net.minecraft.tag.FluidTags;
import net.minecraft.util.Identifier;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.BlockView;
import net.minecraft.world.LightType;
import net.minecraft.world.SpawnHelper;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraft.world.chunk.light.LightingProvider;
import fi.dy.masa.malilib.config.IConfigDouble;
import fi.dy.masa.malilib.config.options.ConfigColor;
import fi.dy.masa.malilib.gui.Message;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.malilib.util.InfoUtils;
import fi.dy.masa.malilib.util.PositionUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.LightLevelMarkerMode;
import fi.dy.masa.minihud.util.LightLevelNumberMode;

public class OverlayRendererLightLevel extends OverlayRendererBase
{
    private static final Identifier TEXTURE_NUMBERS = new Identifier(Reference.MOD_ID, "textures/misc/light_level_numbers.png");

    private final List<LightLevelInfo> lightInfos = new ArrayList<>();
    private final BlockPos.Mutable mutablePos = new BlockPos.Mutable();
    private Direction lastDirection = Direction.NORTH;

    private static boolean tagsBroken;
    private static boolean needsUpdate;

    public static void setNeedsUpdate()
    {
        needsUpdate = true;
    }

    public static void reset()
    {
        tagsBroken = false;
    }

    @Override
    public boolean shouldRender(MinecraftClient mc)
    {
        return RendererToggle.OVERLAY_LIGHT_LEVEL.getBooleanValue();
    }

    @Override
    public boolean needsUpdate(Entity entity, MinecraftClient mc)
    {
        return needsUpdate || this.lastUpdatePos == null ||
                Math.abs(entity.getX() - this.lastUpdatePos.getX()) > 4 ||
                Math.abs(entity.getY() - this.lastUpdatePos.getY()) > 4 ||
                Math.abs(entity.getZ() - this.lastUpdatePos.getZ()) > 4 ||
                (Configs.Generic.LIGHT_LEVEL_NUMBER_ROTATION.getBooleanValue() && this.lastDirection != entity.getHorizontalFacing());
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity, MinecraftClient mc)
    {
        BlockPos pos = PositionUtils.getEntityBlockPos(entity);
        RenderObjectBase renderQuads = this.renderObjects.get(0);
        RenderObjectBase renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), VertexFormats.POSITION_TEXTURE_COLOR);
        BUFFER_2.begin(renderLines.getGlMode(), VertexFormats.POSITION_COLOR);

        //long pre = System.nanoTime();
        this.updateLightLevels(mc.world, pos);
        //System.out.printf("LL markers: %d, time: %.3f s\n", LIGHT_INFOS.size(), (double) (System.nanoTime() - pre) / 1000000000D);
        this.renderLightLevels(cameraPos, mc);

        BUFFER_1.end();
        BUFFER_2.end();
        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.lastUpdatePos = pos;
        this.lastDirection = entity.getHorizontalFacing();
        needsUpdate = false;
    }

    @Override
    protected void preRender()
    {
        super.preRender();

        fi.dy.masa.malilib.render.RenderUtils.bindTexture(TEXTURE_NUMBERS);
        RenderSystem.enableTexture();
    }

    @Override
    protected void postRender()
    {
        RenderSystem.disableTexture();
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(VertexFormat.DrawMode.QUADS, VertexFormats.POSITION_TEXTURE_COLOR, GameRenderer::getPositionTexColorShader);
        this.allocateBuffer(VertexFormat.DrawMode.DEBUG_LINES, VertexFormats.POSITION_COLOR, GameRenderer::getPositionColorShader);
    }

    private void renderLightLevels(Vec3d cameraPos, MinecraftClient mc)
    {
        final int count = this.lightInfos.size();

        if (count > 0)
        {
            BufferBuilder bufferQuads = BUFFER_1;
            BufferBuilder bufferLines = BUFFER_2;
            Direction numberFacing = Configs.Generic.LIGHT_LEVEL_NUMBER_ROTATION.getBooleanValue() ? mc.player.getHorizontalFacing() : Direction.NORTH;
            LightLevelNumberMode numberMode = (LightLevelNumberMode) Configs.Generic.LIGHT_LEVEL_NUMBER_MODE.getOptionListValue();
            LightLevelMarkerMode markerMode = (LightLevelMarkerMode) Configs.Generic.LIGHT_LEVEL_MARKER_MODE.getOptionListValue();
            boolean useColoredNumbers = Configs.Generic.LIGHT_LEVEL_COLORED_NUMBERS.getBooleanValue();
            int lightThreshold = Configs.Generic.LIGHT_LEVEL_THRESHOLD.getIntegerValue();

            if (numberMode == LightLevelNumberMode.BLOCK || numberMode == LightLevelNumberMode.BOTH)
            {
                this.renderNumbers(cameraPos, LightLevelNumberMode.BLOCK,
                        Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_X,
                        Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_Y,
                        Configs.Colors.LIGHT_LEVEL_NUMBER_BLOCK_LIT,
                        Configs.Colors.LIGHT_LEVEL_NUMBER_BLOCK_DARK,
                        useColoredNumbers, lightThreshold, numberFacing, bufferQuads);
            }

            if (numberMode == LightLevelNumberMode.SKY || numberMode == LightLevelNumberMode.BOTH)
            {
                this.renderNumbers(cameraPos, LightLevelNumberMode.SKY,
                        Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_SKY_X,
                        Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_SKY_Y,
                        Configs.Colors.LIGHT_LEVEL_NUMBER_SKY_LIT,
                        Configs.Colors.LIGHT_LEVEL_NUMBER_SKY_DARK,
                        useColoredNumbers, lightThreshold, numberFacing, bufferQuads);
            }

            if (markerMode == LightLevelMarkerMode.SQUARE)
            {
                this.renderMarkers(this::renderLightLevelSquare, cameraPos, lightThreshold, bufferLines);
            }
            else if (markerMode == LightLevelMarkerMode.CROSS)
            {
                this.renderMarkers(this::renderLightLevelCross, cameraPos, lightThreshold, bufferLines);
            }
        }
    }

    private void renderNumbers(Vec3d cameraPos, LightLevelNumberMode mode, IConfigDouble cfgOffX, IConfigDouble cfgOffZ,
            ConfigColor cfgColorLit, ConfigColor cfgColorDark, boolean useColoredNumbers,
            int lightThreshold, Direction numberFacing, BufferBuilder buffer)
    {
        double ox = cfgOffX.getDoubleValue();
        double oz = cfgOffZ.getDoubleValue();
        double tmpX, tmpZ;
        double offsetY = Configs.Generic.LIGHT_LEVEL_Z_OFFSET.getDoubleValue();
        Color4f colorLit, colorDark;

        switch (numberFacing)
        {
            case NORTH: tmpX = -ox; tmpZ = -oz; break;
            case SOUTH: tmpX =  ox; tmpZ =  oz; break;
            case WEST:  tmpX = -oz; tmpZ =  ox; break;
            case EAST:  tmpX =  oz; tmpZ = -ox; break;
            default:    tmpX = -ox; tmpZ = -oz; break;
        }

        if (useColoredNumbers)
        {
            colorLit = cfgColorLit.getColor();
            colorDark = cfgColorDark.getColor();
        }
        else
        {
            colorLit = Color4f.fromColor(0xFFFFFFFF);
            colorDark = Color4f.fromColor(0xFFFFFFFF);
        }

        this.renderLightLevelNumbers(tmpX + cameraPos.x, cameraPos.y - offsetY, tmpZ + cameraPos.z, numberFacing, lightThreshold, mode, colorLit, colorDark, buffer);
    }

    private void renderMarkers(IMarkerRenderer renderer, Vec3d cameraPos, int lightThreshold, BufferBuilder buffer)
    {
        double markerSize = Configs.Generic.LIGHT_LEVEL_MARKER_SIZE.getDoubleValue();
        Color4f colorLit = Configs.Colors.LIGHT_LEVEL_MARKER_LIT.getColor();
        Color4f colorDark = Configs.Colors.LIGHT_LEVEL_MARKER_DARK.getColor();
        double offsetX = cameraPos.x;
        double offsetY = cameraPos.y - Configs.Generic.LIGHT_LEVEL_Z_OFFSET.getDoubleValue();
        double offsetZ = cameraPos.z;
        double offset1 = (1.0 - markerSize) / 2.0;
        double offset2 = (1.0 - offset1);
        final int count = this.lightInfos.size();

        for (int i = 0; i < count; ++i)
        {
            LightLevelInfo info = this.lightInfos.get(i);

            if (info.block < lightThreshold)
            {
                BlockPos pos = info.pos;
                Color4f color = info.sky >= lightThreshold ? colorLit : colorDark;
                renderer.render(pos.getX() - offsetX, pos.getY() - offsetY, pos.getZ() - offsetZ, color, offset1, offset2, buffer);
            }
        }
    }

    private void renderLightLevelNumbers(double dx, double dy, double dz, Direction facing,
            int lightThreshold, LightLevelNumberMode numberMode,
            Color4f colorLit, Color4f colorDark, BufferBuilder buffer)
    {
        final int count = this.lightInfos.size();

        for (int i = 0; i < count; ++i)
        {
            LightLevelInfo info = this.lightInfos.get(i);
            BlockPos pos = info.pos;
            double x = pos.getX() - dx;
            double y = pos.getY() - dy;
            double z = pos.getZ() - dz;
            int lightLevel = numberMode == LightLevelNumberMode.BLOCK ? info.block : info.sky;
            Color4f color = lightLevel >= lightThreshold ? colorLit : colorDark;

            this.renderLightLevelTextureColor(x, y, z, facing, lightLevel, color, buffer);
        }
    }

    private void renderLightLevelTextureColor(double x, double y, double z, Direction facing, int lightLevel, Color4f color, BufferBuilder buffer)
    {
        float w = 0.25f;
        float u = (lightLevel & 0x3) * w;
        float v = (lightLevel >> 2) * w;
        y += 0.005;

        switch (facing)
        {
            case NORTH:
                buffer.vertex(x    , y, z    ).texture(u    , v    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z + 1).texture(u    , v + w).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z + 1).texture(u + w, v + w).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z    ).texture(u + w, v    ).color(color.r, color.g, color.b, color.a).next();
                break;

            case SOUTH:
                buffer.vertex(x + 1, y, z + 1).texture(u    , v    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z    ).texture(u    , v + w).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z    ).texture(u + w, v + w).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z + 1).texture(u + w, v    ).color(color.r, color.g, color.b, color.a).next();
                break;

            case EAST:
                buffer.vertex(x + 1, y, z    ).texture(u    , v    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z    ).texture(u    , v + w).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z + 1).texture(u + w, v + w).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z + 1).texture(u + w, v    ).color(color.r, color.g, color.b, color.a).next();
                break;

            case WEST:
                buffer.vertex(x    , y, z + 1).texture(u    , v    ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z + 1).texture(u    , v + w).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z    ).texture(u + w, v + w).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z    ).texture(u + w, v    ).color(color.r, color.g, color.b, color.a).next();
                break;

            default:
        }
    }

    private void renderLightLevelCross(double x, double y, double z, Color4f color, double offset1, double offset2, BufferBuilder buffer)
    {
        y += 0.005;

        buffer.vertex(x + offset1, y, z + offset1).color(color.r, color.g, color.b, color.a).next();
        buffer.vertex(x + offset2, y, z + offset2).color(color.r, color.g, color.b, color.a).next();

        buffer.vertex(x + offset1, y, z + offset2).color(color.r, color.g, color.b, color.a).next();
        buffer.vertex(x + offset2, y, z + offset1).color(color.r, color.g, color.b, color.a).next();
    }

    private void renderLightLevelSquare(double x, double y, double z, Color4f color, double offset1, double offset2, BufferBuilder buffer)
    {
        y += 0.005;

        buffer.vertex(x + offset1, y, z + offset1).color(color.r, color.g, color.b, color.a).next();
        buffer.vertex(x + offset1, y, z + offset2).color(color.r, color.g, color.b, color.a).next();

        buffer.vertex(x + offset1, y, z + offset2).color(color.r, color.g, color.b, color.a).next();
        buffer.vertex(x + offset2, y, z + offset2).color(color.r, color.g, color.b, color.a).next();

        buffer.vertex(x + offset2, y, z + offset2).color(color.r, color.g, color.b, color.a).next();
        buffer.vertex(x + offset2, y, z + offset1).color(color.r, color.g, color.b, color.a).next();

        buffer.vertex(x + offset2, y, z + offset1).color(color.r, color.g, color.b, color.a).next();
        buffer.vertex(x + offset1, y, z + offset1).color(color.r, color.g, color.b, color.a).next();
    }

    private void updateLightLevels(World world, BlockPos center)
    {
        this.lightInfos.clear();

        int radius = Configs.Generic.LIGHT_LEVEL_RANGE.getIntegerValue();
        final int minX = center.getX() - radius;
        final int minY = center.getY() - radius;
        final int minZ = center.getZ() - radius;
        final int maxX = center.getX() + radius;
        final int maxY = center.getY() + radius;
        final int maxZ = center.getZ() + radius;
        final int minCX = (minX >> 4);
        final int minCZ = (minZ >> 4);
        final int maxCX = (maxX >> 4);
        final int maxCZ = (maxZ >> 4);
        LightingProvider lightingProvider = world.getChunkManager().getLightingProvider();
        final int worldTopHeight = world.getTopY();

        for (int cx = minCX; cx <= maxCX; ++cx)
        {
            final int startX = Math.max( cx << 4      , minX);
            final int endX   = Math.min((cx << 4) + 15, maxX);

            for (int cz = minCZ; cz <= maxCZ; ++cz)
            {
                final int startZ = Math.max( cz << 4      , minZ);
                final int endZ   = Math.min((cz << 4) + 15, maxZ);
                Chunk chunk = world.getChunk(cx, cz);

                for (int x = startX; x <= endX; ++x)
                {
                    for (int z = startZ; z <= endZ; ++z)
                    {
                        final int startY = Math.max(minY, world.getBottomY());
                        final int endY   = Math.min(maxY, chunk.getHighestNonEmptySectionYOffset() + 15 + 1);

                        for (int y = startY; y <= endY; ++y)
                        {
                            if (this.canSpawnAtWrapper(x, y, z, chunk, world))
                            {
                                BlockPos pos = new BlockPos(x, y, z);
                                int block = y < worldTopHeight ? lightingProvider.get(LightType.BLOCK).getLightLevel(pos) : 0;
                                int sky   = y < worldTopHeight ? lightingProvider.get(LightType.SKY).getLightLevel(pos) : 15;

                                this.lightInfos.add(new LightLevelInfo(pos, block, sky));

                                //y += 2; // if the spot is spawnable, that means the next spawnable spot can be the third block up
                            }
                        }
                    }
                }
            }
        }
    }

    private boolean canSpawnAtWrapper(int x, int y, int z, Chunk chunk, World world)
    {
        try
        {
            return this.canSpawnAt(x, y, z, chunk, world);
        }
        catch (Exception e)
        {
            InfoUtils.showGuiOrInGameMessage(Message.MessageType.WARNING, 8000, "This dimension seems to have missing block tag data, the light level will not use the normal block spawnability checks in this dimension. This is known to happen on some Waterfall/BungeeCord/ViaVersion/whatever setups that have an older MC version at the back end.");
            tagsBroken = true;

            return false;
        }
    }

    /**
     * This method mimics the one from WorldEntitySpawner, but takes in the Chunk to avoid that lookup
     */
    private boolean canSpawnAt(int x, int y, int z, Chunk chunk, World world)
    {
        this.mutablePos.set(x, y - 1, z);
        BlockState stateDown = chunk.getBlockState(this.mutablePos);

        if (stateDown.allowsSpawning(world, this.mutablePos, EntityType.CREEPER))
        {
            this.mutablePos.set(x, y, z);
            BlockState state = chunk.getBlockState(this.mutablePos);

            if (isClearForSpawnWrapper(world, this.mutablePos, state, state.getFluidState(), EntityType.WITHER_SKELETON))
            {
                this.mutablePos.set(x, y + 1, z);
                BlockState stateUp1 = chunk.getBlockState(this.mutablePos);

                return isClearForSpawnWrapper(world, this.mutablePos, stateUp1, state.getFluidState(), EntityType.WITHER_SKELETON);
            }

            if (state.getFluidState().isIn(FluidTags.WATER))
            {
                this.mutablePos.set(x, y + 1, z);
                BlockState stateUp1 = chunk.getBlockState(this.mutablePos);

                return stateUp1.getFluidState().isIn(FluidTags.WATER) &&
                       chunk.getBlockState(this.mutablePos.set(x, y + 2, z)).isSolidBlock(world, this.mutablePos) == false;
            }
        }

        return false;
    }

    public static boolean isClearForSpawnWrapper(BlockView blockView, BlockPos pos, BlockState state, FluidState fluidState, EntityType<?> entityType)
    {
        return tagsBroken ? isClearForSpawnStripped(blockView, pos, state, fluidState, entityType) : SpawnHelper.isClearForSpawn(blockView, pos, state, fluidState, entityType);
    }

    /**
     * This method is basically a copy of SpawnHelper.isClearForSpawn(), except that
     * it removes any calls to BlockState.isIn(), which causes an exception on certain
     * ViaVersion servers that have old 1.12.2 worlds.
     * (or possibly newer versions as well, but older than 1.16 or 1.15 or whenever the tag syncing was added)
     */
    public static boolean isClearForSpawnStripped(BlockView blockView, BlockPos pos, BlockState state, FluidState fluidState, EntityType<?> entityType)
    {
        if (state.isFullCube(blockView, pos) || state.emitsRedstonePower() || fluidState.isEmpty() == false)
        {
            return false;
        }
        /*
        else if (state.isIn(BlockTags.PREVENT_MOB_SPAWNING_INSIDE))
        {
            return false;
        }

        // this also calls BlockState isIn()
        return entityType.method_29496(state) == false;
        */

        return true;
    }

    public static class LightLevelInfo
    {
        public final BlockPos pos;
        public final int block;
        public final int sky;

        public LightLevelInfo(BlockPos pos, int block, int sky)
        {
            this.pos = pos;
            this.block = block;
            this.sky = sky;
        }
    }

    private interface IMarkerRenderer
    {
        void render(double x, double y, double z, Color4f color, double offset1, double offset2, BufferBuilder buffer);
    }
}
