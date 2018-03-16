package fi.dy.masa.minihud.event;

import org.apache.commons.lang3.tuple.Pair;
import fi.dy.masa.minihud.config.ConfigsGeneric;
import fi.dy.masa.minihud.config.OverlayHotkeys;
import fi.dy.masa.minihud.renderer.RenderUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;

public class OverlayRenderer
{
    public static BlockPos worldSpawn = BlockPos.ORIGIN;
    public static double chunkUnloadBucketOverlayY;
    public static boolean worldSpawnValid;

    public static void renderOverlays(int mask, Minecraft mc, float partialTicks)
    {
        Entity entity = mc.player;
        GlStateManager.depthMask(false);
        GlStateManager.disableLighting();
        GlStateManager.disableCull();
        GlStateManager.enableBlend();
        //GlStateManager.pushMatrix();
        GlStateManager.disableTexture2D();
        double dx = entity.lastTickPosX + (entity.posX - entity.lastTickPosX) * partialTicks;
        double dy = entity.lastTickPosY + (entity.posY - entity.lastTickPosY) * partialTicks;
        double dz = entity.lastTickPosZ + (entity.posZ - entity.lastTickPosZ) * partialTicks;

        if ((mask & OverlayHotkeys.REGION_FILE.getBitMask()) != 0)
        {
            renderRegionOverlay(mc, entity, dx, dy, dz, partialTicks);
        }

        if ((mask & OverlayHotkeys.CHUNK_UNLOAD_BUCKET.getBitMask()) != 0)
        {
            renderChunkUnloadBuckets(mc, entity, dx, dy, dz, chunkUnloadBucketOverlayY, partialTicks);
        }

        if ((mask & OverlayHotkeys.SPAWN_CHUNK_OVERLAY_PLAYER.getBitMask()) != 0)
        {
            renderSpawnChunksOverlay(mc, entity, dx, dy, dz, new BlockPos(entity.posX, 0, entity.posZ), partialTicks);
        }
        else if ((mask & OverlayHotkeys.SPAWN_CHUNK_OVERLAY_REAL.getBitMask()) != 0 && worldSpawnValid)
        {
            renderSpawnChunksOverlay(mc, entity, dx, dy, dz, worldSpawn, partialTicks);
        }

        GlStateManager.enableTexture2D();
        //GlStateManager.popMatrix();
        GlStateManager.disableBlend();
        GlStateManager.enableCull();
        GlStateManager.depthMask(true);
    }

    private static void renderRegionOverlay(Minecraft mc, Entity entity, double dx, double dy, double dz, float partialTicks)
    {
        int rx = MathHelper.floor(entity.posX) & ~0x1FF;
        int rz = MathHelper.floor(entity.posZ) & ~0x1FF;
        BlockPos pos1 = new BlockPos(rx,         0, rz      );
        BlockPos pos2 = new BlockPos(rx + 511, 256, rz + 511);
        int rangeH = (mc.gameSettings.renderDistanceChunks + 1) * 16;
        int color = ConfigsGeneric.REGION_OVERLAY_COLOR.getIntegerValue();

        GlStateManager.glLineWidth(1.6f);

        RenderUtils.renderVerticalWallsOfLinesWithinRange(pos1, pos2, rangeH, 256, 16, 16, entity, dx, dy, dz, color, partialTicks);
    }

    private static void renderChunkUnloadBuckets(Minecraft mc, Entity entity, double dx, double dy, double dz, double chunkOverlayY, float partialTicks)
    {
        final int centerX = ((int) MathHelper.floor(entity.posX)) >> 4;
        final int centerZ = ((int) MathHelper.floor(entity.posZ)) >> 4;
        final int r = MathHelper.clamp(ConfigsGeneric.CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS.getIntegerValue(), 0, 10);
        final float y = (float) chunkOverlayY;
        final float scale = MathHelper.clamp((float) ConfigsGeneric.CHUNK_UNLOAD_BUCKET_FONT_SCALE.getDoubleValue(), 0.01f, 1f);

        for (int xOff = -r; xOff <= r; xOff++)
        {
            for (int zOff = -r; zOff <= r; zOff++)
            {
                int cx = centerX + xOff;
                int cz = centerZ + zOff;
                int bucket = RenderEventHandler.getChunkUnloadBucket(cx, cz);
                String str = String.valueOf(bucket);
                RenderUtils.drawTextPlate(str, (cx << 4) + 8.5d - dx, y - dy, (cz << 4) + 8.5D - dz, scale, mc);
            }
        }
    }

    private static void renderSpawnChunksOverlay(Minecraft mc, Entity entity, double dx, double dy, double dz, BlockPos worldSpawn, float partialTicks)
    {
        GlStateManager.glLineWidth(1.6f);

        int rangeH = (mc.gameSettings.renderDistanceChunks + 1) * 16;
        Pair<BlockPos, BlockPos> corners = getSpawnChunkCorners(worldSpawn, 128);
        int color = ConfigsGeneric.SPAWN_OVERLAY_COLOR_LAZY.getIntegerValue();
        RenderUtils.renderVerticalWallsOfLinesWithinRange(corners.getLeft(), corners.getRight(), rangeH, 256, 16, 16, entity, dx, dy, dz, color, partialTicks);

        corners = getSpawnChunkCorners(worldSpawn, 128 - 32);
        color = ConfigsGeneric.SPAWN_OVERLAY_COLOR_ENTITY.getIntegerValue();
        RenderUtils.renderVerticalWallsOfLinesWithinRange(corners.getLeft(), corners.getRight(), rangeH, 256, 16, 16, entity, dx, dy, dz, color, partialTicks);
    }

    private static Pair<BlockPos, BlockPos> getSpawnChunkCorners(BlockPos worldSpawn, int spawnChunkRange)
    {
        int x;
        int z;
        x = (worldSpawn.getX() - (spawnChunkRange - 8)) & ~0xF;
        z = (worldSpawn.getZ() - (spawnChunkRange - 8)) & ~0xF;
        BlockPos pos1 = new BlockPos(x, 0, z);

        x = ((worldSpawn.getX() + (spawnChunkRange - 8)) & ~0xF) + 16 - 1;
        z = ((worldSpawn.getZ() + (spawnChunkRange - 8)) & ~0xF) + 16 - 1;
        BlockPos pos2 = new BlockPos(x, 0, z);

        return Pair.of(pos1, pos2);
    }

    /*
    public boolean isSpawnChunk(int x, int z)
    {
        BlockPos blockpos = this.getSpawnPoint();
        int i = x * 16 + 8 - blockpos.getX();
        int j = z * 16 + 8 - blockpos.getZ();
        int k = 128;
        return i >= -128 && i <= 128 && j >= -128 && j <= 128;
    }
    */
}
