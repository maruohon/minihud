package fi.dy.masa.minihud.renderer;

import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.MiscUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.MathHelper;

public class OverlayRenderer
{
    public static double chunkUnloadBucketOverlayY;

    public static void renderOverlays(Minecraft mc, float partialTicks)
    {
        Entity entity = mc.player;
        double dx = entity.lastTickPosX + (entity.posX - entity.lastTickPosX) * partialTicks;
        double dy = entity.lastTickPosY + (entity.posY - entity.lastTickPosY) * partialTicks;
        double dz = entity.lastTickPosZ + (entity.posZ - entity.lastTickPosZ) * partialTicks;

        if (RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getBooleanValue())
        {
            renderChunkUnloadBuckets(mc, entity, dx, dy, dz, chunkUnloadBucketOverlayY);
        }

        RenderContainer.INSTANCE.render(entity, mc, partialTicks);
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
}
