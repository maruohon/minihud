package minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import org.lwjgl.opengl.GL11;

import net.minecraft.block.material.Material;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.init.Blocks;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.World;
import net.minecraft.world.WorldEntitySpawner;
import net.minecraft.world.chunk.Chunk;

import malilib.config.option.ColorConfig;
import malilib.config.option.Vec2dConfig;
import malilib.render.RenderUtils;
import malilib.render.overlay.BaseRenderObject;
import malilib.render.overlay.VboRenderObject;
import malilib.util.data.Color4f;
import malilib.util.game.wrap.EntityWrap;
import malilib.util.game.wrap.GameUtils;
import minihud.Reference;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.util.value.LightLevelMarkerMode;
import minihud.util.value.LightLevelNumberMode;

public class OverlayRendererLightLevel extends MiniHudOverlayRenderer
{
    private static final ResourceLocation NUMBER_TEXTURE = new ResourceLocation(Reference.MOD_ID, "textures/misc/light_level_numbers.png");

    private final List<LightLevelInfo> lightInfoList = new ArrayList<>();
    private EnumFacing lastDirection = EnumFacing.NORTH;

    @Override
    public boolean shouldRender()
    {
        return RendererToggle.LIGHT_LEVEL.isRendererEnabled();
    }

    @Override
    public boolean needsUpdate(Entity entity)
    {
        return this.needsUpdate || this.lastUpdatePos == null ||
               Math.abs(EntityWrap.getX(entity) - this.lastUpdatePos.getX()) > 4 ||
               Math.abs(EntityWrap.getY(entity) - this.lastUpdatePos.getY()) > 4 ||
               Math.abs(EntityWrap.getZ(entity) - this.lastUpdatePos.getZ()) > 4 ||
               (Configs.Generic.LIGHT_LEVEL_NUMBER_ROTATION.getBooleanValue() &&
                   this.lastDirection != entity.getHorizontalFacing());
    }

    @Override
    public void update(Vec3d cameraPos, Entity entity)
    {
        BlockPos pos = EntityWrap.getEntityBlockPos(entity);
        BaseRenderObject renderQuads = this.renderObjects.get(0);
        BaseRenderObject renderLines = this.renderObjects.get(1);
        BUFFER_1.begin(renderQuads.getGlMode(), renderQuads.getVertexFormat());
        BUFFER_2.begin(renderLines.getGlMode(), renderLines.getVertexFormat());

        //long pre = System.nanoTime();
        this.updateLightLevels(GameUtils.getClientWorld(), pos);
        //System.out.printf("LL markers: %d, time: %.3f s\n", LIGHT_INFOS.size(), (double) (System.nanoTime() - pre) / 1000000000D);
        this.renderLightLevels(cameraPos, BUFFER_1, BUFFER_2);

        BUFFER_1.finishDrawing();
        BUFFER_2.finishDrawing();
        renderQuads.uploadData(BUFFER_1);
        renderLines.uploadData(BUFFER_2);

        this.lastDirection = entity.getHorizontalFacing();
        this.needsUpdate = false;
    }

    @Override
    protected void preRender()
    {
        super.preRender();

        RenderUtils.bindTexture(NUMBER_TEXTURE);
    }

    @Override
    public void allocateGlResources()
    {
        this.allocateBuffer(GL11.GL_QUADS, DefaultVertexFormats.POSITION_TEX_COLOR, VboRenderObject::setupArrayPointersPosUvColor);
        this.allocateBuffer(GL11.GL_LINES);
    }

    private void renderLightLevels(Vec3d cameraPos, BufferBuilder bufferQuads, BufferBuilder bufferLines)
    {
        final int count = this.lightInfoList.size();
        Entity entity = GameUtils.getCameraEntity();

        if (count > 0)
        {
            EnumFacing numberFacing = Configs.Generic.LIGHT_LEVEL_NUMBER_ROTATION.getBooleanValue() ? entity.getHorizontalFacing() : EnumFacing.NORTH;
            LightLevelNumberMode numberMode = Configs.Generic.LIGHT_LEVEL_NUMBER_MODE.getValue();
            LightLevelMarkerMode markerMode = Configs.Generic.LIGHT_LEVEL_MARKER_MODE.getValue();
            boolean useColoredNumbers = Configs.Generic.LIGHT_LEVEL_COLORED_NUMBERS.getBooleanValue();
            int lightThreshold = Configs.Generic.LIGHT_LEVEL_THRESHOLD.getIntegerValue();

            if (numberMode == LightLevelNumberMode.BLOCK || numberMode == LightLevelNumberMode.BOTH)
            {
                this.renderNumbers(cameraPos, LightLevelNumberMode.BLOCK,
                                   Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_BLOCK,
                                   Configs.Colors.LIGHT_LEVEL_NUMBER_BLOCK_LIT,
                                   Configs.Colors.LIGHT_LEVEL_NUMBER_BLOCK_DARK,
                                   useColoredNumbers, lightThreshold, numberFacing, bufferQuads);
            }

            if (numberMode == LightLevelNumberMode.SKY || numberMode == LightLevelNumberMode.BOTH)
            {
                this.renderNumbers(cameraPos, LightLevelNumberMode.SKY,
                                   Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_SKY,
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

    private void renderNumbers(Vec3d cameraPos, LightLevelNumberMode mode, Vec2dConfig cfgOff,
                               ColorConfig cfgColorLit, ColorConfig cfgColorDark, boolean useColoredNumbers,
                               int lightThreshold, EnumFacing numberFacing, BufferBuilder buffer)
    {
        double ox = cfgOff.getValue().x;
        double oz = cfgOff.getValue().y;
        double tmpX, tmpZ;
        Color4f colorLit, colorDark;
        double offsetY = Configs.Generic.LIGHT_LEVEL_Z_OFFSET.getDoubleValue();

        switch (numberFacing)
        {
            case SOUTH: tmpX =  ox; tmpZ =  oz; break;
            case WEST:  tmpX = -oz; tmpZ =  ox; break;
            case EAST:  tmpX =  oz; tmpZ = -ox; break;
            case NORTH:
            default:    tmpX = -ox; tmpZ = -oz;
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

        for (LightLevelInfo info : this.lightInfoList)
        {
            if (info.block < lightThreshold)
            {
                BlockPos pos = info.pos;
                Color4f color = info.sky >= lightThreshold ? colorLit : colorDark;
                renderer.render(pos.getX() - offsetX, pos.getY() - offsetY, pos.getZ() - offsetZ, color, offset1, offset2, buffer);
            }
        }
    }

    private void renderLightLevelNumbers(double dx, double dy, double dz, EnumFacing facing,
                                         int lightThreshold, LightLevelNumberMode numberMode,
                                         Color4f colorLit, Color4f colorDark, BufferBuilder buffer)
    {
        for (LightLevelInfo info : this.lightInfoList)
        {
            int lightLevel = numberMode == LightLevelNumberMode.BLOCK ? info.block : info.sky;
            Color4f color = lightLevel >= lightThreshold ? colorLit : colorDark;
            BlockPos pos = info.pos;
            double x = pos.getX() - dx;
            double y = pos.getY() - dy;
            double z = pos.getZ() - dz;

            this.renderLightLevelTextureColor(x, y, z, facing, lightLevel, color, buffer);
        }
    }

    private void renderLightLevelTextureColor(double x, double y, double z, EnumFacing facing, int lightLevel, Color4f color, BufferBuilder buffer)
    {
        float w = 0.25f;
        float u = (lightLevel & 0x3) * w;
        float v = (lightLevel >> 2) * w;

        switch (facing)
        {
            case NORTH:
                buffer.pos(x    , y, z    ).tex(u    , v    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z + 1).tex(u    , v + w).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z + 1).tex(u + w, v + w).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z    ).tex(u + w, v    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;

            case SOUTH:
                buffer.pos(x + 1, y, z + 1).tex(u    , v    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z    ).tex(u    , v + w).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z    ).tex(u + w, v + w).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z + 1).tex(u + w, v    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;

            case EAST:
                buffer.pos(x + 1, y, z    ).tex(u    , v    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z    ).tex(u    , v + w).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z + 1).tex(u + w, v + w).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z + 1).tex(u + w, v    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;

            case WEST:
                buffer.pos(x    , y, z + 1).tex(u    , v    ).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z + 1).tex(u    , v + w).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x + 1, y, z    ).tex(u + w, v + w).color(color.r, color.g, color.b, color.a).endVertex();
                buffer.pos(x    , y, z    ).tex(u + w, v    ).color(color.r, color.g, color.b, color.a).endVertex();
                break;

            default:
        }
    }

    private void renderLightLevelCross(double x, double y, double z, Color4f color, double offset1, double offset2, BufferBuilder buffer)
    {
        buffer.pos(x + offset1, y, z + offset1).color(color.r, color.g, color.b, color.a).endVertex();
        buffer.pos(x + offset2, y, z + offset2).color(color.r, color.g, color.b, color.a).endVertex();

        buffer.pos(x + offset1, y, z + offset2).color(color.r, color.g, color.b, color.a).endVertex();
        buffer.pos(x + offset2, y, z + offset1).color(color.r, color.g, color.b, color.a).endVertex();
    }

    private void renderLightLevelSquare(double x, double y, double z, Color4f color, double offset1, double offset2, BufferBuilder buffer)
    {
        buffer.pos(x + offset1, y, z + offset1).color(color.r, color.g, color.b, color.a).endVertex();
        buffer.pos(x + offset1, y, z + offset2).color(color.r, color.g, color.b, color.a).endVertex();

        buffer.pos(x + offset1, y, z + offset2).color(color.r, color.g, color.b, color.a).endVertex();
        buffer.pos(x + offset2, y, z + offset2).color(color.r, color.g, color.b, color.a).endVertex();

        buffer.pos(x + offset2, y, z + offset2).color(color.r, color.g, color.b, color.a).endVertex();
        buffer.pos(x + offset2, y, z + offset1).color(color.r, color.g, color.b, color.a).endVertex();

        buffer.pos(x + offset2, y, z + offset1).color(color.r, color.g, color.b, color.a).endVertex();
        buffer.pos(x + offset1, y, z + offset1).color(color.r, color.g, color.b, color.a).endVertex();
    }

    private void updateLightLevels(World world, BlockPos center)
    {
        this.lightInfoList.clear();

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
                        final int startY = Math.max(minY, 0);
                        final int endY   = Math.min(maxY, chunk.getTopFilledSegment() + 15 + 1);
                        IBlockState stateDown = chunk.getBlockState(x, startY - 1, z);
                        IBlockState state    = chunk.getBlockState(x, startY, z);
                        IBlockState stateUp  = chunk.getBlockState(x, startY + 1, z);
                        IBlockState stateUp2 = chunk.getBlockState(x, startY + 2, z);

                        for (int y = startY; y <= endY; ++y)
                        {
                            if (canSpawnAt(stateDown, state, stateUp, stateUp2))
                            {
                                BlockPos pos = new BlockPos(x, y, z);
                                int block = y < 256 ? chunk.getLightFor(EnumSkyBlock.BLOCK, pos) : 0;
                                int sky   = y < 256 ? chunk.getLightFor(EnumSkyBlock.SKY, pos) : 15;

                                this.lightInfoList.add(new LightLevelInfo(pos, block, sky));

                                //y += 2; // if the spot is spawnable, that means the next spawnable spot can be the third block up
                            }

                            stateDown = state;
                            state = stateUp;
                            stateUp = stateUp2;
                            stateUp2 = chunk.getBlockState(x, y + 3, z);
                        }
                    }
                }
            }
        }
    }

    /**
     * This method mimics the one from WorldEntitySpawner, but takes in the Chunk to avoid that lookup
     */
    public static boolean canSpawnAt(IBlockState stateDown, IBlockState state, IBlockState stateUp, IBlockState stateUp2)
    {
        if (stateDown.isTopSolid() == false ||
            stateDown.getBlock() == Blocks.BEDROCK ||
            stateDown.getBlock() == Blocks.BARRIER)
        {
            return false;
        }
        else
        {
            if (state.getMaterial() == Material.WATER)
            {
                return stateUp.getMaterial() == Material.WATER &&
                       stateUp2.isNormalCube() == false;
            }

            return WorldEntitySpawner.isValidEmptySpawnBlock(state) &&
                   WorldEntitySpawner.isValidEmptySpawnBlock(stateUp);
        }
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
