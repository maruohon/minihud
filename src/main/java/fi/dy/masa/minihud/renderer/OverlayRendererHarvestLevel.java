package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import org.lwjgl.opengl.GL11;
import com.mojang.blaze3d.platform.GlStateManager;
import fi.dy.masa.malilib.util.Color4f;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.util.HarvestLevelMarkerMode;
import fi.dy.masa.minihud.util.HarvestLevelNumberMode;
import net.minecraft.block.BlockState;
import net.minecraft.block.CropBlock;
import net.minecraft.block.FarmlandBlock;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.Tessellator;
import net.minecraft.client.render.VertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.Identifier;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.world.World;
import net.minecraft.world.chunk.Chunk;

public class OverlayRendererHarvestLevel {
	private static final Identifier TEXTURE_NUMBERS = new Identifier(Reference.MOD_ID, "textures/misc/light_level_numbers.png");
    private static final List<HarvestLevelInfo> HARVEST_INFOS = new ArrayList<>();
    private static final BlockPos.Mutable MUTABLE_POS = new BlockPos.Mutable();

    private static boolean needsUpdate;
    private static BlockPos lastUpdatePos = null;

    public static void setNeedsUpdate()
    {
        needsUpdate = true;
    }

    public static void render(double dx, double dy, double dz, Entity entity, MinecraftClient mc)
    {
        if (needsUpdate || lastUpdatePos == null ||
            Math.abs(entity.x - lastUpdatePos.getX()) > 4 ||
            Math.abs(entity.y - lastUpdatePos.getY()) > 4 ||
            Math.abs(entity.z - lastUpdatePos.getZ()) > 4)
        {
            //long pre = System.nanoTime();
            updateHarvestLevels(mc.world, new BlockPos(entity));
            //System.out.printf("LL markers: %d, time: %.3f s\n", LIGHT_INFOS.size(), (double) (System.nanoTime() - pre) / 1000000000D);
        }
        
        renderHarvestLevels(dx, dy, dz, mc);
    }

    private static void renderHarvestLevels(double dx, double dy, double dz, MinecraftClient mc)
    {
        final int count = HARVEST_INFOS.size();

        if (count > 0)
        {
            mc.getTextureManager().bindTexture(TEXTURE_NUMBERS);

            GlStateManager.enableAlphaTest();
            GlStateManager.alphaFunc(GL11.GL_GREATER, 0.01F);
            GlStateManager.disableLighting();
            fi.dy.masa.malilib.render.RenderUtils.setupBlend();
            fi.dy.masa.malilib.render.RenderUtils.color(1f, 1f, 1f, 1f);

            Tessellator tessellator = Tessellator.getInstance();
            BufferBuilder buffer = tessellator.getBufferBuilder();
            Direction numberFacing = Configs.Generic.HARVEST_LEVEL_NUMBER_ROTATION.getBooleanValue() ? mc.player.getHorizontalFacing() : Direction.NORTH;
            HarvestLevelNumberMode numberMode = (HarvestLevelNumberMode) Configs.Generic.HARVEST_LEVEL_NUMBER_MODE.getOptionListValue();
            HarvestLevelMarkerMode markerMode = (HarvestLevelMarkerMode) Configs.Generic.HARVEST_LEVEL_MARKER_MODE.getOptionListValue();
            boolean useColoredNumbers = Configs.Generic.HARVEST_LEVEL_COLORED_NUMBERS.getBooleanValue();

            if (numberMode == HarvestLevelNumberMode.BLOCK)
            {
                double ox = Configs.Generic.HARVEST_LEVEL_NUMBER_OFFSET_BLOCK_X.getDoubleValue();
                double oz = Configs.Generic.HARVEST_LEVEL_NUMBER_OFFSET_BLOCK_Y.getDoubleValue();
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
                    colorLit = Configs.Colors.HARVEST_LEVEL_NUMBER_BLOCK_LIT.getColor();
                    colorDark = Configs.Colors.HARVEST_LEVEL_NUMBER_BLOCK_DARK.getColor();
                }
                
	            renderHarvestLevelNumbers(tmpX, dy, tmpZ, numberFacing, colorLit, colorDark, buffer, mc);
	            tessellator.draw();
            }

            if (markerMode == HarvestLevelMarkerMode.SQUARE)
            {
                double markerSize = Configs.Generic.HARVEST_LEVEL_MARKER_SIZE.getDoubleValue();
                Color4f colorLit = Configs.Colors.HARVEST_LEVEL_MARKER_LIT.getColor();
                Color4f colorDark = Configs.Colors.HARVEST_LEVEL_MARKER_DARK.getColor();
                double offset1 = (1.0 - markerSize) / 2.0;
                double offset2 = (1.0 - offset1);

                GlStateManager.disableTexture();

                buffer.begin(GL11.GL_LINES, VertexFormats.POSITION_COLOR);

                for (int i = 0; i < count; ++i)
                {
                    HarvestLevelInfo info = HARVEST_INFOS.get(i);
                    
                    MUTABLE_POS.set(info.pos.getX(), info.pos.getY(), info.pos.getZ());
                    int maxAge = 0;
                    if(mc.player.world.getBlockState(MUTABLE_POS).getBlock() instanceof CropBlock)
                    {
                    	maxAge = ((CropBlock)mc.player.world.getBlockState(MUTABLE_POS).getBlock()).getMaxAge();
                    
	                    //TODO look wheat harvest
	                    if (info.block < maxAge)
	                    {
	                        BlockPos pos = info.pos;
	                        Color4f color = info.block >= maxAge ? colorLit : colorDark;
	                        renderHarvestLevelSquare(pos.getX() - dx, pos.getY() - dy, pos.getZ() - dz, color, offset1, offset2, buffer);
	                    }
                    }
                }

                tessellator.draw();

                GlStateManager.enableTexture();
            }
            else if (markerMode == HarvestLevelMarkerMode.CROSS)
            {
                double markerSize = Configs.Generic.HARVEST_LEVEL_MARKER_SIZE.getDoubleValue();
                Color4f colorLit = Configs.Colors.HARVEST_LEVEL_MARKER_LIT.getColor();
                Color4f colorDark = Configs.Colors.HARVEST_LEVEL_MARKER_DARK.getColor();
                double offset1 = (1.0 - markerSize) / 2.0;
                double offset2 = (1.0 - offset1);

                GlStateManager.disableTexture();

                buffer.begin(GL11.GL_LINES, VertexFormats.POSITION_COLOR);

                for (int i = 0; i < count; ++i)
                {
                    HarvestLevelInfo info = HARVEST_INFOS.get(i);
                    
                    MUTABLE_POS.set(info.pos.getX(), info.pos.getY(), info.pos.getZ());
                    int maxAge = 0;
                    if(mc.player.world.getBlockState(MUTABLE_POS).getBlock() instanceof CropBlock) {
                    	maxAge = ((CropBlock)mc.player.world.getBlockState(MUTABLE_POS).getBlock()).getMaxAge();
                    	
                        if (info.block < maxAge)
                        {
                            BlockPos pos = info.pos;
                            Color4f color = info.block >= maxAge ? colorLit : colorDark;
                            renderHarvestLevelCross(pos.getX() - dx, pos.getY() - dy, pos.getZ() - dz, color, offset1, offset2, buffer);
                        }
                    }
                }

                tessellator.draw();

                GlStateManager.enableTexture();
            }

            GlStateManager.disableBlend();
            GlStateManager.enableLighting();
        }
    }

    private static void renderHarvestLevelNumbers(double dx, double dy, double dz, Direction facing,
            @Nullable Color4f colorLit, @Nullable Color4f colorDark, BufferBuilder buffer, MinecraftClient mc)
    {
        final int count = HARVEST_INFOS.size();

        if (colorLit != null)
        {
            buffer.begin(GL11.GL_QUADS, VertexFormats.POSITION_UV_COLOR);

            for (int i = 0; i < count; ++i)
            {
                HarvestLevelInfo info = HARVEST_INFOS.get(i);
                BlockPos pos = info.pos;
                double x = pos.getX() - dx;
                double z = pos.getZ() - dz;
                int harvestLevel = info.block;
                MUTABLE_POS.set(info.pos.getX(), info.pos.getY(), info.pos.getZ());
                int maxAge = 0;
                if(mc.player.world.getBlockState(MUTABLE_POS).getBlock() instanceof CropBlock) {
                	maxAge = ((CropBlock)mc.player.world.getBlockState(MUTABLE_POS).getBlock()).getMaxAge();
                	Color4f color = harvestLevel >= maxAge ? colorLit : colorDark;
                	renderHarvestLevelTextureColor(x, pos.getY() - dy + 1, z, facing, harvestLevel, color, buffer);
                }
            }
        }
        else
        {
            buffer.begin(GL11.GL_QUADS, VertexFormats.POSITION_UV);

            for (int i = 0; i < count; ++i)
            {
                HarvestLevelInfo info = HARVEST_INFOS.get(i);
                BlockPos pos = info.pos;
                double x = pos.getX() - dx;
                double z = pos.getZ() - dz;
                int harvestLevel = info.block;

                renderHarvestLevelTexture(x, pos.getY() - dy + 1, z, facing, harvestLevel, buffer);
            }
        }
    }

    private static void renderHarvestLevelTexture(double x, double y, double z, Direction facing, int harvestLevel, BufferBuilder buffer)
    {
        double u = (harvestLevel & 0x3) * 0.25;
        double v = (harvestLevel >> 2) * 0.25;

        y += 0.005;

        switch (facing)
        {
            case NORTH:
                buffer.vertex(x    , y, z    ).texture(u       , v       ).next();
                buffer.vertex(x    , y, z + 1).texture(u       , v + 0.25).next();
                buffer.vertex(x + 1, y, z + 1).texture(u + 0.25, v + 0.25).next();
                buffer.vertex(x + 1, y, z    ).texture(u + 0.25, v       ).next();
                break;

            case SOUTH:
                buffer.vertex(x + 1, y, z + 1).texture(u       , v       ).next();
                buffer.vertex(x + 1, y, z    ).texture(u       , v + 0.25).next();
                buffer.vertex(x    , y, z    ).texture(u + 0.25, v + 0.25).next();
                buffer.vertex(x    , y, z + 1).texture(u + 0.25, v       ).next();
                break;

            case EAST:
                buffer.vertex(x + 1, y, z    ).texture(u       , v       ).next();
                buffer.vertex(x    , y, z    ).texture(u       , v + 0.25).next();
                buffer.vertex(x    , y, z + 1).texture(u + 0.25, v + 0.25).next();
                buffer.vertex(x + 1, y, z + 1).texture(u + 0.25, v       ).next();
                break;

            case WEST:
                buffer.vertex(x    , y, z + 1).texture(u       , v       ).next();
                buffer.vertex(x + 1, y, z + 1).texture(u       , v + 0.25).next();
                buffer.vertex(x + 1, y, z    ).texture(u + 0.25, v + 0.25).next();
                buffer.vertex(x    , y, z    ).texture(u + 0.25, v       ).next();
                break;

            default:
        }
    }

    private static void renderHarvestLevelTextureColor(double x, double y, double z, Direction facing, int harvestLevel, Color4f color, BufferBuilder buffer)
    {
        double u = (harvestLevel & 0x3) * 0.25;
        double v = (harvestLevel >> 2) * 0.25;
        y += 0.005;

        switch (facing)
        {
            case NORTH:
                buffer.vertex(x    , y, z    ).texture(u       , v       ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z + 1).texture(u       , v + 0.25).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z + 1).texture(u + 0.25, v + 0.25).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z    ).texture(u + 0.25, v       ).color(color.r, color.g, color.b, color.a).next();
                break;

            case SOUTH:
                buffer.vertex(x + 1, y, z + 1).texture(u       , v       ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z    ).texture(u       , v + 0.25).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z    ).texture(u + 0.25, v + 0.25).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z + 1).texture(u + 0.25, v       ).color(color.r, color.g, color.b, color.a).next();
                break;

            case EAST:
                buffer.vertex(x + 1, y, z    ).texture(u       , v       ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z    ).texture(u       , v + 0.25).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z + 1).texture(u + 0.25, v + 0.25).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z + 1).texture(u + 0.25, v       ).color(color.r, color.g, color.b, color.a).next();
                break;

            case WEST:
                buffer.vertex(x    , y, z + 1).texture(u       , v       ).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z + 1).texture(u       , v + 0.25).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x + 1, y, z    ).texture(u + 0.25, v + 0.25).color(color.r, color.g, color.b, color.a).next();
                buffer.vertex(x    , y, z    ).texture(u + 0.25, v       ).color(color.r, color.g, color.b, color.a).next();
                break;

            default:
        }
    }

    private static void renderHarvestLevelCross(double x, double y, double z, Color4f color, double offset1, double offset2, BufferBuilder buffer)
    {
        y += 0.005;

        buffer.vertex(x + offset1, y, z + offset1).color(color.r, color.g, color.b, color.a).next();
        buffer.vertex(x + offset2, y, z + offset2).color(color.r, color.g, color.b, color.a).next();

        buffer.vertex(x + offset1, y, z + offset2).color(color.r, color.g, color.b, color.a).next();
        buffer.vertex(x + offset2, y, z + offset1).color(color.r, color.g, color.b, color.a).next();
    }

    private static void renderHarvestLevelSquare(double x, double y, double z, Color4f color, double offset1, double offset2, BufferBuilder buffer)
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

    private static void updateHarvestLevels(World world, BlockPos center)
    {
        HARVEST_INFOS.clear();

        int radius = Configs.Generic.HARVEST_LEVEL_RANGE.getIntegerValue();
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
                        final int endY   = Math.min(maxY, chunk.getHighestNonEmptySectionYOffset() + 15);

                        for (int y = startY; y <= endY; ++y)
                        {
                            if (canHarvestAt(x, y, z, chunk, world))
                            {
                                MUTABLE_POS.set(x, y, z);
                                int block = (chunk.getBlockState(MUTABLE_POS)).get(((CropBlock)chunk.getBlockState(MUTABLE_POS).getBlock()).getAgeProperty());

                                HARVEST_INFOS.add(new HarvestLevelInfo(new BlockPos(x, y, z), block));
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
     * This method checks if there is a crop above farmland, but takes in the Chunk to avoid that lookup
     * @param cropPlacementIn
     * @param worldIn
     * @param pos
     * @return
     */
    public static boolean canHarvestAt(int x, int y, int z, Chunk chunk, World world)
    {
        MUTABLE_POS.set(x, y - 1, z);
        BlockState state = chunk.getBlockState(MUTABLE_POS);

        if ((state.getBlock() instanceof FarmlandBlock) == false)
        {
            return false;
        }
        else
        {
            return (world.getBlockState(MUTABLE_POS.up()).getBlock()) instanceof CropBlock;

        }
    }

    public static class HarvestLevelInfo
    {
        public final BlockPos pos;
        public final int block;

        public HarvestLevelInfo(BlockPos pos, int block)
        {
            this.pos = pos;
            this.block = block;
        }
    }
}
