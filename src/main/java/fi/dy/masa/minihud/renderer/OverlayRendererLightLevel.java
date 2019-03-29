package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import org.lwjgl.opengl.GL11;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import net.minecraft.block.Block;
import net.minecraft.block.state.IBlockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.init.Blocks;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.EnumSkyBlock;
import net.minecraft.world.World;
import net.minecraft.world.WorldEntitySpawner;
import net.minecraft.world.chunk.Chunk;

public class OverlayRendererLightLevel
{
    private static final ResourceLocation TEXTURE_NUMBERS = new ResourceLocation(Reference.MOD_ID, "textures/misc/light_level_numbers.png");
    private static final List<LightLevelInfo> LIGHT_INFOS = new ArrayList<>();

    private static boolean needsUpdate;
    private static BlockPos lastUpdatePos = null;

    public static void setNeedsUpdate()
    {
        needsUpdate = true;
    }

    public static void render(double dx, double dy, double dz, Entity entity, Minecraft mc)
    {
        if (needsUpdate || lastUpdatePos == null ||
            Math.abs(entity.posX - lastUpdatePos.getX()) > 4 ||
            Math.abs(entity.posY - lastUpdatePos.getY()) > 4 ||
            Math.abs(entity.posZ - lastUpdatePos.getZ()) > 4)
        {
            //long pre = System.nanoTime();
            updateLightLevels(mc.world, new BlockPos(entity));
            //System.out.printf("LL markers: %d, time: %.3f s\n", LIGHT_INFOS.size(), (double) (System.nanoTime() - pre) / 1000000000D);
        }

        renderLightLevels(dx, dy, dz, mc);
    }

    private static void renderLightLevels(double dx, double dy, double dz, Minecraft mc)
    {
        final int count = LIGHT_INFOS.size();

        if (count > 0)
        {
            mc.getTextureManager().bindTexture(TEXTURE_NUMBERS);

            GlStateManager.enableAlpha();
            GlStateManager.alphaFunc(GL11.GL_GREATER, 0.01F);
            GlStateManager.enableBlend();
            GlStateManager.tryBlendFuncSeparate(GlStateManager.SourceFactor.SRC_ALPHA, GlStateManager.DestFactor.ONE_MINUS_SRC_ALPHA, GlStateManager.SourceFactor.ONE, GlStateManager.DestFactor.ZERO);
            GlStateManager.disableLighting();
            GlStateManager.color(1f, 1f, 1f, 1f);

            Tessellator tessellator = Tessellator.getInstance();
            BufferBuilder buffer = tessellator.getBuffer();
            boolean useColoredNumbers = Configs.Generic.LIGHT_LEVEL_COLORED_NUMBERS.getBooleanValue();

            if (Configs.Generic.LIGHT_LEVEL_NUMBERS.getBooleanValue())
            {
                if (useColoredNumbers)
                {
                    buffer.begin(GL11.GL_QUADS, DefaultVertexFormats.POSITION_TEX_COLOR);

                    for (int i = 0; i < count; ++i)
                    {
                        LightLevelInfo info = LIGHT_INFOS.get(i);
                        BlockPos pos = info.pos;
                        renderLightLevelTextureColor(info, pos.getX() - dx, pos.getY() - dy, pos.getZ() - dz, buffer);
                    }

                    tessellator.draw();
                }
                else
                {
                    buffer.begin(GL11.GL_QUADS, DefaultVertexFormats.POSITION_TEX);

                    for (int i = 0; i < count; ++i)
                    {
                        LightLevelInfo info = LIGHT_INFOS.get(i);
                        BlockPos pos = info.pos;
                        renderLightLevelTexture(info, pos.getX() - dx, pos.getY() - dy, pos.getZ() - dz, buffer);
                    }

                    tessellator.draw();
                }
            }
            else
            {
                GlStateManager.disableTexture2D();

                buffer.begin(GL11.GL_LINES, DefaultVertexFormats.POSITION_COLOR);

                for (int i = 0; i < count; ++i)
                {
                    LightLevelInfo info = LIGHT_INFOS.get(i);

                    if (info.block < 8)
                    {
                        BlockPos pos = info.pos;
                        renderLightLevelCross(info, pos.getX() - dx, pos.getY() - dy, pos.getZ() - dz, buffer);
                    }
                }

                tessellator.draw();

                GlStateManager.enableTexture2D();
            }

            GlStateManager.disableBlend();
            GlStateManager.enableLighting();
        }
    }

    private static void renderLightLevelTexture(LightLevelInfo info, double x, double y, double z, BufferBuilder buffer)
    {
        double u = (info.block & 0x3) * 0.25;
        double v = (info.block >> 2) * 0.25;
        y += 0.005;

        buffer.pos(x    , y, z    ).tex(u       , v       ).endVertex();
        buffer.pos(x    , y, z + 1).tex(u       , v + 0.25).endVertex();
        buffer.pos(x + 1, y, z + 1).tex(u + 0.25, v + 0.25).endVertex();
        buffer.pos(x + 1, y, z    ).tex(u + 0.25, v       ).endVertex();
    }

    private static void renderLightLevelTextureColor(LightLevelInfo info, double x, double y, double z, BufferBuilder buffer)
    {
        double u = (info.block & 0x3) * 0.25;
        double v = (info.block >> 2) * 0.25;
        float r, g, b;
        y += 0.005;

        if (info.block >= 8)
        {
            r = 0x20 / 255f;
            g = 0x70 / 255f;
            b = 0x40 / 255f;
        }
        else
        {
            r = 0xC0 / 255f;
            g = 0x30 / 255f;
            b = 0x30 / 255f;
        }

        buffer.pos(x    , y, z    ).tex(u       , v       ).color(r, g, b, 1f).endVertex();
        buffer.pos(x    , y, z + 1).tex(u       , v + 0.25).color(r, g, b, 1f).endVertex();
        buffer.pos(x + 1, y, z + 1).tex(u + 0.25, v + 0.25).color(r, g, b, 1f).endVertex();
        buffer.pos(x + 1, y, z    ).tex(u + 0.25, v       ).color(r, g, b, 1f).endVertex();
    }

    private static void renderLightLevelCross(LightLevelInfo info, double x, double y, double z, BufferBuilder buffer)
    {
        float r, g, b;

        y += 0.005;

        if (info.sky >= 8)
        {
            r = 1f;
            g = 1f;
            b = 0.2f;
        }
        else
        {
            r = 1f;
            g = 0.3f;
            b = 0.3f;
        }

        buffer.pos(x    , y, z    ).color(r, g, b, 1f).endVertex();
        buffer.pos(x + 1, y, z + 1).color(r, g, b, 1f).endVertex();

        buffer.pos(x    , y, z + 1).color(r, g, b, 1f).endVertex();
        buffer.pos(x + 1, y, z    ).color(r, g, b, 1f).endVertex();
    }

    private static void updateLightLevels(World world, BlockPos center)
    {
        LIGHT_INFOS.clear();

        int radius = 24;
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
        BlockPos.MutableBlockPos posMutable = new BlockPos.MutableBlockPos();

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
                        final int endY   = Math.min(maxY, chunk.getTopFilledSegment() + 15);

                        for (int y = startY; y <= endY; ++y)
                        {
                            if (canSpawnAt(x, y, z, chunk))
                            {
                                posMutable.setPos(x, y, z);

                                int block = chunk.getLightFor(EnumSkyBlock.BLOCK, posMutable);
                                int sky = chunk.getLightFor(EnumSkyBlock.SKY, posMutable);

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
    public static boolean canSpawnAt(int x, int y, int z, Chunk chunk)
    {
        IBlockState state = chunk.getBlockState(x, y - 1, z);

        if (state.isTopSolid() == false)
        {
            return false;
        }
        else
        {
            Block block = state.getBlock();
            boolean spawnable = block != Blocks.BEDROCK && block != Blocks.BARRIER;

            return spawnable &&
                   WorldEntitySpawner.isValidEmptySpawnBlock(chunk.getBlockState(x, y    , z)) &&
                   WorldEntitySpawner.isValidEmptySpawnBlock(chunk.getBlockState(x, y + 1, z));
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
