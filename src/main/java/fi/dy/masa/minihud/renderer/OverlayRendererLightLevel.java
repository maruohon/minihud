package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import org.lwjgl.opengl.GL11;
import com.mojang.blaze3d.platform.GlStateManager;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.Blocks;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.Tessellator;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.client.render.entity.EntityRenderDispatcher;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.util.Identifier;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.world.LightType;
import net.minecraft.world.SpawnHelper;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;
import net.minecraft.world.chunk.light.LightingProvider;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.util.LightLevelMarkerMode;
import fi.dy.masa.minihud.util.LightLevelNumberMode;

public class OverlayRendererLightLevel
{
    private static final Identifier TEXTURE_NUMBERS = new Identifier(Reference.MOD_ID, "textures/misc/light_level_numbers.png");
    private static final List<LightLevelInfo> LIGHT_INFOS = new ArrayList<>();
    private static final BlockPos.Mutable MUTABLE_POS = new BlockPos.Mutable();

    private static boolean needsUpdate;
    private static BlockPos lastUpdatePos = null;

    public static void setNeedsUpdate()
    {
        needsUpdate = true;
    }

    public static void render(double x, double y, double z, Entity entity, MinecraftClient mc, net.minecraft.client.util.math.MatrixStack matrixStack)
    {
        if (needsUpdate || lastUpdatePos == null ||
            Math.abs(entity.getX() - lastUpdatePos.getX()) > 4 ||
            Math.abs(entity.getY() - lastUpdatePos.getY()) > 4 ||
            Math.abs(entity.getZ() - lastUpdatePos.getZ()) > 4)
        {
            //long pre = System.nanoTime();
            updateLightLevels(mc.world, new BlockPos(entity));
            //System.out.printf("LL markers: %d, time: %.3f s\n", LIGHT_INFOS.size(), (double) (System.nanoTime() - pre) / 1000000000D);
        }

        RenderSystem.pushMatrix();
        EntityRenderDispatcher disp = mc.getEntityRenderManager();
        RenderSystem.rotatef(disp.camera.getPitch(), 1.0F, 0.0F, 0.0F);
        RenderSystem.rotatef(disp.camera.getYaw(), 0.0F, 1.0F, 0.0F);
        RenderSystem.scalef(-1, 1, -1);

        renderLightLevels(x, y, z, mc);

        RenderSystem.popMatrix();
    }

    private static void renderLightLevels(double dx, double dy, double dz, MinecraftClient mc)
    {
        final int count = LIGHT_INFOS.size();

        if (count > 0)
        {
            fi.dy.masa.malilib.render.RenderUtils.bindTexture(TEXTURE_NUMBERS);

            GlStateManager.enableAlphaTest();
            GlStateManager.alphaFunc(GL11.GL_GREATER, 0.01F);
            GlStateManager.disableLighting();
            fi.dy.masa.malilib.render.RenderUtils.setupBlend();
            fi.dy.masa.malilib.render.RenderUtils.color(1f, 1f, 1f, 1f);

            Tessellator tessellator = Tessellator.getInstance();
            BufferBuilder buffer = tessellator.getBuffer();
            Direction numberFacing = Configs.Generic.LIGHT_LEVEL_NUMBER_ROTATION.getBooleanValue() ? mc.player.getHorizontalFacing() : Direction.NORTH;
            LightLevelNumberMode numberMode = (LightLevelNumberMode) Configs.Generic.LIGHT_LEVEL_NUMBER_MODE.getOptionListValue();
            LightLevelMarkerMode markerMode = (LightLevelMarkerMode) Configs.Generic.LIGHT_LEVEL_MARKER_MODE.getOptionListValue();
            boolean useColoredNumbers = Configs.Generic.LIGHT_LEVEL_COLORED_NUMBERS.getBooleanValue();
            int lightThreshold = Configs.Generic.LIGHT_LEVEL_THRESHOLD.getIntegerValue();

            if (numberMode == LightLevelNumberMode.BLOCK || numberMode == LightLevelNumberMode.BOTH)
            {
                double ox = Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_X.getDoubleValue();
                double oz = Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_Y.getDoubleValue();
                double tmpX, tmpZ;
                Color4f colorLit = null, colorDark = null;

                switch (numberFacing)
                {
                    case NORTH: tmpX = dx - ox; tmpZ = dz - oz; break;
                    case SOUTH: tmpX = dx + ox; tmpZ = dz + oz; break;
                    case WEST:  tmpX = dx - oz; tmpZ = dz + ox; break;
                    case EAST:  tmpX = dx + oz; tmpZ = dz - ox; break;
                    default:    tmpX = dx - ox; tmpZ = dz - oz; break;
                }

                if (useColoredNumbers)
                {
                    colorLit = Configs.Colors.LIGHT_LEVEL_NUMBER_BLOCK_LIT.getColor();
                    colorDark = Configs.Colors.LIGHT_LEVEL_NUMBER_BLOCK_DARK.getColor();
                }

                renderLightLevelNumbers(tmpX, dy, tmpZ, numberFacing, lightThreshold, LightLevelNumberMode.BLOCK, colorLit, colorDark, buffer);
                tessellator.draw();
            }

            if (numberMode == LightLevelNumberMode.SKY || numberMode == LightLevelNumberMode.BOTH)
            {
                double ox = Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_SKY_X.getDoubleValue();
                double oz = Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_SKY_Y.getDoubleValue();
                double tmpX, tmpZ;
                Color4f colorLit = null, colorDark = null;

                switch (numberFacing)
                {
                    case NORTH: tmpX = dx - ox; tmpZ = dz - oz; break;
                    case SOUTH: tmpX = dx + ox; tmpZ = dz + oz; break;
                    case WEST:  tmpX = dx - oz; tmpZ = dz + ox; break;
                    case EAST:  tmpX = dx + oz; tmpZ = dz - ox; break;
                    default:    tmpX = dx - ox; tmpZ = dz - oz; break;
                }

                if (useColoredNumbers)
                {
                    colorLit = Configs.Colors.LIGHT_LEVEL_NUMBER_SKY_LIT.getColor();
                    colorDark = Configs.Colors.LIGHT_LEVEL_NUMBER_SKY_DARK.getColor();
                }

                renderLightLevelNumbers(tmpX, dy, tmpZ, numberFacing, lightThreshold, LightLevelNumberMode.SKY, colorLit, colorDark, buffer);
                tessellator.draw();
            }

            if (markerMode == LightLevelMarkerMode.SQUARE)
            {
                double markerSize = Configs.Generic.LIGHT_LEVEL_MARKER_SIZE.getDoubleValue();
                Color4f colorLit = Configs.Colors.LIGHT_LEVEL_MARKER_LIT.getColor();
                Color4f colorDark = Configs.Colors.LIGHT_LEVEL_MARKER_DARK.getColor();
                double offset1 = (1.0 - markerSize) / 2.0;
                double offset2 = (1.0 - offset1);

                GlStateManager.disableTexture();

                buffer.begin(GL11.GL_LINES, VertexFormats.POSITION_COLOR);

                for (int i = 0; i < count; ++i)
                {
                    LightLevelInfo info = LIGHT_INFOS.get(i);

                    if (info.block < lightThreshold)
                    {
                        BlockPos pos = info.pos;
                        Color4f color = info.sky >= lightThreshold ? colorLit : colorDark;
                        renderLightLevelSquare(pos.getX() - dx, pos.getY() - dy, pos.getZ() - dz, color, offset1, offset2, buffer);
                    }
                }

                tessellator.draw();

                GlStateManager.enableTexture();
            }
            else if (markerMode == LightLevelMarkerMode.CROSS)
            {
                double markerSize = Configs.Generic.LIGHT_LEVEL_MARKER_SIZE.getDoubleValue();
                Color4f colorLit = Configs.Colors.LIGHT_LEVEL_MARKER_LIT.getColor();
                Color4f colorDark = Configs.Colors.LIGHT_LEVEL_MARKER_DARK.getColor();
                double offset1 = (1.0 - markerSize) / 2.0;
                double offset2 = (1.0 - offset1);

                GlStateManager.disableTexture();

                buffer.begin(GL11.GL_LINES, VertexFormats.POSITION_COLOR);

                for (int i = 0; i < count; ++i)
                {
                    LightLevelInfo info = LIGHT_INFOS.get(i);

                    if (info.block < lightThreshold)
                    {
                        BlockPos pos = info.pos;
                        Color4f color = info.sky >= lightThreshold ? colorLit : colorDark;
                        renderLightLevelCross(pos.getX() - dx, pos.getY() - dy, pos.getZ() - dz, color, offset1, offset2, buffer);
                    }
                }

                tessellator.draw();

                GlStateManager.enableTexture();
            }

            GlStateManager.disableBlend();
            GlStateManager.enableLighting();
        }
    }

    private static void renderLightLevelNumbers(double dx, double dy, double dz, Direction facing,
            int lightThreshold, LightLevelNumberMode numberMode,
            @Nullable Color4f colorLit, @Nullable Color4f colorDark, BufferBuilder buffer)
    {
        final int count = LIGHT_INFOS.size();

        if (colorLit != null)
        {
            buffer.begin(GL11.GL_QUADS, VertexFormats.POSITION_TEXTURE_COLOR);

            for (int i = 0; i < count; ++i)
            {
                LightLevelInfo info = LIGHT_INFOS.get(i);
                BlockPos pos = info.pos;
                double x = pos.getX() - dx;
                double z = pos.getZ() - dz;
                int lightLevel = numberMode == LightLevelNumberMode.BLOCK ? info.block : info.sky;
                Color4f color = lightLevel >= lightThreshold ? colorLit : colorDark;

                renderLightLevelTextureColor(x, pos.getY() - dy, z, facing, lightLevel, color, buffer);
            }
        }
        else
        {
            buffer.begin(GL11.GL_QUADS, VertexFormats.POSITION_TEXTURE);

            for (int i = 0; i < count; ++i)
            {
                LightLevelInfo info = LIGHT_INFOS.get(i);
                BlockPos pos = info.pos;
                double x = pos.getX() - dx;
                double z = pos.getZ() - dz;
                int lightLevel = numberMode == LightLevelNumberMode.BLOCK ? info.block : info.sky;

                renderLightLevelTexture(x, pos.getY() - dy, z, facing, lightLevel, buffer);
            }
        }
    }

    private static void renderLightLevelTexture(double x, double y, double z, Direction facing, int lightLevel, BufferBuilder buffer)
    {
        float w = 0.25f;
        float u = (lightLevel & 0x3) * w;
        float v = (lightLevel >> 2) * w;

        y += 0.005;

        switch (facing)
        {
            case NORTH:
                buffer.vertex(x    , y, z    ).texture(u    , v    ).next();
                buffer.vertex(x    , y, z + 1).texture(u    , v + w).next();
                buffer.vertex(x + 1, y, z + 1).texture(u + w, v + w).next();
                buffer.vertex(x + 1, y, z    ).texture(u + w, v    ).next();
                break;

            case SOUTH:
                buffer.vertex(x + 1, y, z + 1).texture(u    , v    ).next();
                buffer.vertex(x + 1, y, z    ).texture(u    , v + w).next();
                buffer.vertex(x    , y, z    ).texture(u + w, v + w).next();
                buffer.vertex(x    , y, z + 1).texture(u + w, v    ).next();
                break;

            case EAST:
                buffer.vertex(x + 1, y, z    ).texture(u    , v    ).next();
                buffer.vertex(x    , y, z    ).texture(u    , v + w).next();
                buffer.vertex(x    , y, z + 1).texture(u + w, v + w).next();
                buffer.vertex(x + 1, y, z + 1).texture(u + w, v    ).next();
                break;

            case WEST:
                buffer.vertex(x    , y, z + 1).texture(u    , v    ).next();
                buffer.vertex(x + 1, y, z + 1).texture(u    , v + w).next();
                buffer.vertex(x + 1, y, z    ).texture(u + w, v + w).next();
                buffer.vertex(x    , y, z    ).texture(u + w, v    ).next();
                break;

            default:
        }
    }

    private static void renderLightLevelTextureColor(double x, double y, double z, Direction facing, int lightLevel, Color4f color, BufferBuilder buffer)
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

    private static void renderLightLevelCross(double x, double y, double z, Color4f color, double offset1, double offset2, BufferBuilder buffer)
    {
        y += 0.005;

        buffer.vertex(x + offset1, y, z + offset1).color(color.r, color.g, color.b, color.a).next();
        buffer.vertex(x + offset2, y, z + offset2).color(color.r, color.g, color.b, color.a).next();

        buffer.vertex(x + offset1, y, z + offset2).color(color.r, color.g, color.b, color.a).next();
        buffer.vertex(x + offset2, y, z + offset1).color(color.r, color.g, color.b, color.a).next();
    }

    private static void renderLightLevelSquare(double x, double y, double z, Color4f color, double offset1, double offset2, BufferBuilder buffer)
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

    private static void updateLightLevels(World world, BlockPos center)
    {
        LIGHT_INFOS.clear();

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
                        final int endY   = Math.min(maxY, chunk.getHighestNonEmptySectionYOffset() + 15);

                        for (int y = startY; y <= endY; ++y)
                        {
                            if (canSpawnAt(x, y, z, chunk, world))
                            {
                                MUTABLE_POS.set(x, y, z);

                                int block = lightingProvider.get(LightType.BLOCK).getLightLevel(MUTABLE_POS);
                                int sky = lightingProvider.get(LightType.SKY).getLightLevel(MUTABLE_POS);

                                LIGHT_INFOS.add(new LightLevelInfo(new BlockPos(x, y, z), block, sky));

                                //y += 2; // if the spot is spawnable, that means the next spawnable spot can be the third block up
                            }
                        }
                    }
                }
            }
        }

        needsUpdate = false;
        lastUpdatePos = center;
    }

    /**
     * This method mimics the one from WorldEntitySpawner, but takes in the Chunk to avoid that lookup
     * @param spawnPlacementTypeIn
     * @param worldIn
     * @param pos
     * @return
     */
    public static boolean canSpawnAt(int x, int y, int z, Chunk chunk, World world)
    {
        MUTABLE_POS.set(x, y - 1, z);
        BlockState state = chunk.getBlockState(MUTABLE_POS);

        if (state.allowsSpawning(world, MUTABLE_POS, EntityType.SKELETON) == false)
        {
            return false;
        }
        else
        {
            Block block = state.getBlock();
            boolean spawnable = block != Blocks.BEDROCK && block != Blocks.BARRIER;

            if (spawnable)
            {
                MUTABLE_POS.set(x, y , z);
                state = chunk.getBlockState(MUTABLE_POS);

                if (SpawnHelper.isClearForSpawn(world, MUTABLE_POS, state, state.getFluidState()))
                {
                    MUTABLE_POS.set(x, y + 1, z);
                    state = chunk.getBlockState(MUTABLE_POS);
                    return SpawnHelper.isClearForSpawn(world, MUTABLE_POS, state, state.getFluidState());
                }
            }

            return false;
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
}
