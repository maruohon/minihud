package fi.dy.masa.minihud.renderer;

import org.apache.commons.lang3.tuple.Pair;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.MiscUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.GlStateManager;
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
            renderRegionOverlay(mc, entity, dx, dy, dz, partialTicks);
        }

        if (RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getBooleanValue())
        {
            renderChunkUnloadBuckets(mc, entity, dx, dy, dz, chunkUnloadBucketOverlayY, partialTicks);
        }

        if (RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanValue() && mc.world.provider.isSurfaceWorld())
        {
            renderSlimeChunkOverlay(mc, entity, dx, dy, dz, slimeChunkOverlayTopY, partialTicks);
        }

        if (RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER.getBooleanValue())
        {
            int colorLazy = Configs.Generic.SPAWN_PLAYER_LAZY_OVERLAY_COLOR.getIntegerValue();
            int colorProcessing = Configs.Generic.SPAWN_PLAYER_ENTITY_OVERLAY_COLOR.getIntegerValue();
            BlockPos pos = new BlockPos(entity.posX, 0, entity.posZ);
            renderSpawnChunksOverlay(mc, entity, dx, dy, dz, pos, colorLazy, colorProcessing, partialTicks);
        }

        if (RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL.getBooleanValue() && data.isWorldSpawnKnown())
        {
            int colorLazy = Configs.Generic.SPAWN_REAL_LAZY_OVERLAY_COLOR.getIntegerValue();
            int colorProcessing = Configs.Generic.SPAWN_REAL_ENTITY_OVERLAY_COLOR.getIntegerValue();
            renderSpawnChunksOverlay(mc, entity, dx, dy, dz, data.getWorldSpawn(), colorLazy, colorProcessing, partialTicks);
        }

        GlStateManager.enableTexture2D();
        GlStateManager.disableBlend();
        GlStateManager.enableCull();
        GlStateManager.depthMask(true);
    }

    private static void renderRegionOverlay(Minecraft mc, Entity entity, double dx, double dy, double dz, float partialTicks)
    {
        int rx = MathHelper.floor(entity.posX) & ~0x1FF;
        int rz = MathHelper.floor(entity.posZ) & ~0x1FF;
        PooledMutableBlockPos pos1 = PooledMutableBlockPos.retain();
        PooledMutableBlockPos pos2 = PooledMutableBlockPos.retain();
        pos1.setPos(rx,         0, rz      );
        pos2.setPos(rx + 511, 256, rz + 511);
        int rangeH = (mc.gameSettings.renderDistanceChunks + 1) * 16;
        int color = Configs.Generic.REGION_OVERLAY_COLOR.getIntegerValue();

        GlStateManager.glLineWidth(1.6f);

        RenderUtils.renderVerticalWallsOfLinesWithinRange(pos1, pos2, rangeH, 256, 16, 16, entity, dx, dy, dz, color, partialTicks);
        pos1.release();
        pos2.release();
    }

    private static void renderChunkUnloadBuckets(Minecraft mc, Entity entity, double dx, double dy, double dz, double chunkOverlayY, float partialTicks)
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

    private static void renderSlimeChunkOverlay(Minecraft mc, Entity entity, double dx, double dy, double dz, double overlayTopY, float partialTicks)
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
            final int color = Configs.Generic.SLIME_CHUNKS_OVERLAY_COLOR.getIntegerValue();
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
                        RenderUtils.renderBoxWithEdges(pos1, pos2, dx, dy, dz, color, partialTicks);
                    }
                }
            }

            pos1.release();
            pos2.release();
        }
    }

    private static void renderSpawnChunksOverlay(Minecraft mc, Entity entity, double dx, double dy, double dz,
            BlockPos worldSpawn, int colorLazy, int colorProcessing, float partialTicks)
    {
        GlStateManager.glLineWidth(1.6f);

        int rangeH = (mc.gameSettings.renderDistanceChunks + 1) * 16;
        Pair<BlockPos, BlockPos> corners = getSpawnChunkCorners(worldSpawn, 128);
        RenderUtils.renderVerticalWallsOfLinesWithinRange(corners.getLeft(), corners.getRight(), rangeH, 256, 16, 16, entity, dx, dy, dz, colorLazy, partialTicks);

        corners = getSpawnChunkCorners(worldSpawn, 128 - 32);
        RenderUtils.renderVerticalWallsOfLinesWithinRange(corners.getLeft(), corners.getRight(), rangeH, 256, 16, 16, entity, dx, dy, dz, colorProcessing, partialTicks);
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
        BlockPos pos2 = new BlockPos(x, 0, z);

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
