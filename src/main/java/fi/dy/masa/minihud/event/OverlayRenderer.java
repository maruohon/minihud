package fi.dy.masa.minihud.event;

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
    public static double chunkUnloadBucketOverlayY;

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
        float a = ((color >>> 24) & 0xFF) / 255f;
        float r = ((color >>> 16) & 0xFF) / 255f;
        float g = ((color >>>  8) & 0xFF) / 255f;
        float b = ((color       ) & 0xFF) / 255f;

        GlStateManager.glLineWidth(1.6f);

        RenderUtils.renderVerticalWallsOfLinesWithinRange(pos1, pos2, rangeH, 256, 16, 16, entity, dx, dy, dz, r, g, b, a, partialTicks);
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
}
