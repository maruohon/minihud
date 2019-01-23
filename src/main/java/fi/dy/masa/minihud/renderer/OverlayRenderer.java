package fi.dy.masa.minihud.renderer;

import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.MiscUtils;
import net.minecraft.client.MinecraftClient;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.MathHelper;

public class OverlayRenderer
{
    private static final RenderContainer RC = new RenderContainer();
    public static double chunkUnloadBucketOverlayY;

    public static void renderOverlays(MinecraftClient mc, float partialTicks)
    {
        Entity entity = mc.player;
        double dx = entity.prevRenderX + (entity.x - entity.prevRenderX) * partialTicks;
        double dy = entity.prevRenderY + (entity.y - entity.prevRenderY) * partialTicks;
        double dz = entity.prevRenderZ + (entity.z - entity.prevRenderZ) * partialTicks;

        if (RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getBooleanValue())
        {
            renderChunkUnloadBuckets(mc, entity, dx, dy, dz, chunkUnloadBucketOverlayY);
        }

        RC.render(entity, mc, partialTicks);
    }

    private static void renderChunkUnloadBuckets(MinecraftClient mc, Entity entity, double dx, double dy, double dz, double chunkOverlayY)
    {
        final int centerX = ((int) MathHelper.floor(entity.x)) >> 4;
        final int centerZ = ((int) MathHelper.floor(entity.z)) >> 4;
        int r = MathHelper.clamp(Configs.Generic.CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS.getIntegerValue(), -1, 40);
        if (r == -1)
        {
            r = mc.options.viewDistance;
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
