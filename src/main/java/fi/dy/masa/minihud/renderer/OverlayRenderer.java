package fi.dy.masa.minihud.renderer;

import org.apache.commons.lang3.tuple.Pair;
import org.lwjgl.opengl.GL11;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockPos.PooledMutableBlockPos;
import net.minecraft.util.math.MathHelper;

public class OverlayRenderer
{
    public static double chunkUnloadBucketOverlayY;
    public static double slimeChunkOverlayTopY;

    public static void renderOverlays(Minecraft mc, float partialTicks)
    {
        Entity entity = mc.player;
        DataStorage data = DataStorage.getInstance();
        GlStateManager.depthMask(false);
        GlStateManager.disableLighting();
        GlStateManager.disableCull();
        GlStateManager.enableBlend();
        GlStateManager.disableTexture2D();
        double dx = entity.lastTickPosX + (entity.posX - entity.lastTickPosX) * partialTicks;
        double dy = entity.lastTickPosY + (entity.posY - entity.lastTickPosY) * partialTicks;
        double dz = entity.lastTickPosZ + (entity.posZ - entity.lastTickPosZ) * partialTicks;

        if (RendererToggle.OVERLAY_REGION_FILE.getBooleanValue())
        {
            renderRegionOverlay(mc, entity, dx, dy, dz);
        }

        if (RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getBooleanValue())
        {
            renderChunkUnloadBuckets(mc, entity, dx, dy, dz, chunkUnloadBucketOverlayY);
        }

        if (RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanValue() && mc.world.provider.isSurfaceWorld())
        {
            renderSlimeChunkOverlay(mc, entity, dx, dy, dz, slimeChunkOverlayTopY);
        }

        if (RendererToggle.OVERLAY_SPAWNABLE_SUB_CHUNKS.getBooleanValue())
        {
            renderSpawnableSubChunksOverlay(mc, entity, dx, dy, dz);
        }

        if (RendererToggle.OVERLAY_SPAWNABLE_COLUMN_HEIGHTS.getBooleanValue())
        {
            renderSpawnableColumnHeightsOverlay(mc, entity, dx, dy, dz);
        }

        if (RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER.getBooleanValue())
        {
            int colorLazy = Configs.Colors.SPAWN_PLAYER_LAZY_OVERLAY_COLOR.getIntegerValue();
            int colorProcessing = Configs.Colors.SPAWN_PLAYER_ENTITY_OVERLAY_COLOR.getIntegerValue();
            BlockPos pos = new BlockPos(entity.posX, 0, entity.posZ);
            renderSpawnChunksOverlay(mc, entity, dx, dy, dz, pos, colorLazy, colorProcessing);
        }

        if (RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL.getBooleanValue() && data.isWorldSpawnKnown())
        {
            int colorLazy = Configs.Colors.SPAWN_REAL_LAZY_OVERLAY_COLOR.getIntegerValue();
            int colorProcessing = Configs.Colors.SPAWN_REAL_ENTITY_OVERLAY_COLOR.getIntegerValue();
            renderSpawnChunksOverlay(mc, entity, dx, dy, dz, data.getWorldSpawn(), colorLazy, colorProcessing);
        }

        GlStateManager.enableTexture2D();
        GlStateManager.disableBlend();
        GlStateManager.enableCull();
        GlStateManager.depthMask(true);
    }

    private static void renderRegionOverlay(Minecraft mc, Entity entity, double dx, double dy, double dz)
    {
        int rx = MathHelper.floor(entity.posX) & ~0x1FF;
        int rz = MathHelper.floor(entity.posZ) & ~0x1FF;
        PooledMutableBlockPos pos1 = PooledMutableBlockPos.retain();
        PooledMutableBlockPos pos2 = PooledMutableBlockPos.retain();
        pos1.setPos(rx,         0, rz      );
        pos2.setPos(rx + 511, 256, rz + 511);
        int rangeH = (mc.gameSettings.renderDistanceChunks + 1) * 16;
        int color = Configs.Colors.REGION_OVERLAY_COLOR.getIntegerValue();

        GlStateManager.glLineWidth(1.6f);

        RenderUtils.renderVerticalWallsOfLinesWithinRange(pos1, pos2, rangeH, 256, 16, 16, entity, dx, dy, dz, color);
        pos1.release();
        pos2.release();
    }

    private static void renderChunkUnloadBuckets(Minecraft mc, Entity entity, double dx, double dy, double dz, double chunkOverlayY)
    {
        final int centerX = ((int) MathHelper.floor(entity.posX)) >> 4;
        final int centerZ = ((int) MathHelper.floor(entity.posZ)) >> 4;
        int r = MathHelper.clamp(Configs.Generic.CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS.getIntegerValue(), -1, 40);
        if (r == -1)
        {
            r = mc.gameSettings.renderDistanceChunks;
        }
        final float y = (float) chunkOverlayY;
        final float scale = MathHelper.clamp((float) Configs.Generic.CHUNK_UNLOAD_BUCKET_FONT_SCALE.getDoubleValue(), 0.01f, 1f);

        for (int xOff = -r; xOff <= r; xOff++)
        {
            for (int zOff = -r; zOff <= r; zOff++)
            {
                int cx = centerX + xOff;
                int cz = centerZ + zOff;
                int bucket = MiscUtils.getChunkUnloadBucket(cx, cz);
                String str = String.valueOf(bucket);
                RenderUtils.drawTextPlate(str, (cx << 4) + 8.5d - dx, y - dy, (cz << 4) + 8.5D - dz, scale, mc);
            }
        }
    }

    private static void renderSlimeChunkOverlay(Minecraft mc, Entity entity, double dx, double dy, double dz, double overlayTopY)
    {
        DataStorage data = DataStorage.getInstance();

        if (data.isWorldSeedKnown(entity.dimension))
        {
            final int centerX = ((int) MathHelper.floor(entity.posX)) >> 4;
            final int centerZ = ((int) MathHelper.floor(entity.posZ)) >> 4;
            int r = MathHelper.clamp(Configs.Generic.SLIME_CHUNK_OVERLAY_RADIUS.getIntegerValue(), -1, 40);
            if (r == -1)
            {
                r = mc.gameSettings.renderDistanceChunks;
            }
            final long worldSeed = data.getWorldSeed(entity.dimension);
            final int color = Configs.Colors.SLIME_CHUNKS_OVERLAY_COLOR.getIntegerValue();
            PooledMutableBlockPos pos1 = PooledMutableBlockPos.retain();
            PooledMutableBlockPos pos2 = PooledMutableBlockPos.retain();

            for (int xOff = -r; xOff <= r; xOff++)
            {
                for (int zOff = -r; zOff <= r; zOff++)
                {
                    int cx = centerX + xOff;
                    int cz = centerZ + zOff;

                    if (MiscUtils.canSlimeSpawnInChunk(cx, cz, worldSeed))
                    {
                        pos1.setPos( cx << 4,                 0,  cz << 4);
                        pos2.setPos((cx << 4) + 16, overlayTopY, (cz << 4) + 16);
                        RenderUtils.renderBoxWithEdges(pos1, pos2, dx, dy, dz, color);
                    }
                }
            }

            pos1.release();
            pos2.release();
        }
    }

    private static void renderSpawnChunksOverlay(Minecraft mc, Entity entity, double dx, double dy, double dz,
            BlockPos worldSpawn, int colorLazy, int colorProcessing)
    {
        GlStateManager.glLineWidth(1.6f);

        int rangeH = (mc.gameSettings.renderDistanceChunks + 1) * 16;
        Pair<BlockPos, BlockPos> corners = getSpawnChunkCorners(worldSpawn, 128);
        RenderUtils.renderVerticalWallsOfLinesWithinRange(corners.getLeft(), corners.getRight(), rangeH, 256, 16, 16, entity, dx, dy, dz, colorLazy);

        corners = getSpawnChunkCorners(worldSpawn, 128 - 32);
        RenderUtils.renderVerticalWallsOfLinesWithinRange(corners.getLeft(), corners.getRight(), rangeH, 256, 16, 16, entity, dx, dy, dz, colorProcessing);
    }

    private static void renderSpawnableSubChunksOverlay(Minecraft mc, Entity entity, double dx, double dy, double dz)
    {
        final int centerX = ((int) MathHelper.floor(entity.posX)) >> 4;
        final int centerZ = ((int) MathHelper.floor(entity.posZ)) >> 4;
        final int color = Configs.Colors.SPAWNABLE_SUB_CHUNKS_OVERLAY_COLOR.getIntegerValue();
        int r = MathHelper.clamp(Configs.Generic.SPAWNABLE_SUB_CHUNKS_OVERLAY_RADIUS.getIntegerValue(), -1, 40);

        if (r == -1)
        {
            r = mc.gameSettings.renderDistanceChunks;
        }

        PooledMutableBlockPos pos1 = PooledMutableBlockPos.retain();
        PooledMutableBlockPos pos2 = PooledMutableBlockPos.retain();
        DataStorage data = DataStorage.getInstance();

        for (int xOff = -r; xOff <= r; xOff++)
        {
            for (int zOff = -r; zOff <= r; zOff++)
            {
                int cx = centerX + xOff;
                int cz = centerZ + zOff;
                int topY = data.getSpawnableSubChunkCountFor(cx, cz);

                if (topY >= 0)
                {
                    topY <<= 4;
                    pos1.setPos( cx << 4      ,    0,  cz << 4      );
                    pos2.setPos((cx << 4) + 15, topY, (cz << 4) + 15);
                    RenderUtils.renderVerticalWallsOfLinesWithinRange(pos1, pos2, 256, 256, 16, 16, entity, dx, dy, dz, color);
                    RenderUtils.renderChunkOverlayTopQuad(cx, cz, topY - dy, dx, dz, color);
                }
            }
        }

        pos1.release();
        pos2.release();
    }

    private static void renderSpawnableColumnHeightsOverlay(Minecraft mc, Entity entity, double dx, double dy, double dz)
    {
        final int color = Configs.Colors.SPAWNABLE_COLUMNS_OVERLAY_COLOR.getIntegerValue();
        final int radius = MathHelper.clamp(Configs.Generic.SPAWNABLE_COLUMNS_OVERLAY_RADIUS.getIntegerValue(), 0, 128);

        GlStateManager.pushMatrix();
        GlStateManager.disableLighting();
        GlStateManager.enableBlend();
        GlStateManager.tryBlendFuncSeparate(GlStateManager.SourceFactor.SRC_ALPHA, GlStateManager.DestFactor.ONE_MINUS_SRC_ALPHA, GlStateManager.SourceFactor.ONE, GlStateManager.DestFactor.ZERO);
        GlStateManager.disableTexture2D();

        Tessellator tessellator = Tessellator.getInstance();
        BufferBuilder buffer = tessellator.getBuffer();
        buffer.begin(GL11.GL_TRIANGLE_STRIP, DefaultVertexFormats.POSITION_COLOR);
        final float a = ((color >>> 24) & 0xFF) / 255f;
        final float r = ((color >>> 16) & 0xFF) / 255f;
        final float g = ((color >>>  8) & 0xFF) / 255f;
        final float b = ((color       ) & 0xFF) / 255f;

        final int xStart = (int) entity.posX - radius;
        final int zStart = (int) entity.posZ - radius;
        final int xEnd = (int) entity.posX + radius;
        final int zEnd = (int) entity.posZ + radius;
        final WorldClient world = mc.world;

        for (int x = xStart; x <= xEnd; ++x)
        {
            for (int z = zStart; z <= zEnd; ++z)
            {
                // See WorldEntitySpawner.getRandomChunkPosition()
                int height = MathHelper.roundUp(world.getHeight(x, z) + 1, 16);

                if (height == 0)
                {
                    height = world.getChunkFromChunkCoords(x << 4, z << 4).getTopFilledSegment() + 15;
                }

                final double boxX = x - dx;
                final double boxZ = z - dz;
                final double boxY = height - dy;

                RenderUtils.addChainedSidesAndTopBoxVertices(buffer,
                        boxX + 0.25F, boxY           , boxZ + 0.25F,
                        boxX + 0.75F, boxY + 0.09375D, boxZ + 0.75F, r, g, b, a);
            }
        }

        tessellator.draw();

        GlStateManager.enableTexture2D();
        GlStateManager.popMatrix();
    }

    private static Pair<BlockPos, BlockPos> getSpawnChunkCorners(BlockPos worldSpawn, int spawnChunkRange)
    {
        int x;
        int z;
        x = (worldSpawn.getX() - (spawnChunkRange - 7)) & ~0xF;
        z = (worldSpawn.getZ() - (spawnChunkRange - 7)) & ~0xF;
        BlockPos pos1 = new BlockPos(x, 0, z);

        x = ((worldSpawn.getX() + (spawnChunkRange - 8)) & ~0xF) + 16 - 1;
        z = ((worldSpawn.getZ() + (spawnChunkRange - 8)) & ~0xF) + 16 - 1;
        BlockPos pos2 = new BlockPos(x, 256, z);

        return Pair.of(pos1, pos2);
    }

    /*
    public boolean isSpawnChunk(int chunkX, int chunkZ)
    {
        BlockPos spawn = this.getSpawnPoint();
        int dx = chunkX * 16 + 8 - spawn.getX();
        int dz = chunkZ * 16 + 8 - spawn.getZ();
        return dx >= -128 && dx <= 128 && dz >= -128 && dz <= 128;
    }
    */
}
