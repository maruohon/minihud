package fi.dy.masa.minihud.renderer;

import java.util.Collections;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.MathHelper;
import fi.dy.masa.malilib.render.TextRenderUtils;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.GameUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.MiscUtils;

public class OverlayRenderer
{
    private static long loginTime;
    private static boolean canRender;

    public static void resetRenderTimeout()
    {
        canRender = false;
        loginTime = System.currentTimeMillis();
    }

    public static void renderOverlays(Minecraft mc, float partialTicks)
    {
        Entity entity = EntityUtils.getCameraEntity();

        if (entity == null)
        {
            return;
        }

        if (canRender == false)
        {
            // Don't render before the player has been placed in the actual proper position,
            // otherwise some of the renderers mess up.
            // The magic 8.5, 65, 8.5 comes from the WorldClient constructor
            if (System.currentTimeMillis() - loginTime >= 5000 || entity.posX != 8.5 || entity.posY != 65 || entity.posZ != 8.5)
            {
                canRender = true;
            }
            else
            {
                return;
            }
        }

        double dx = entity.lastTickPosX + (entity.posX - entity.lastTickPosX) * partialTicks;
        double dy = entity.lastTickPosY + (entity.posY - entity.lastTickPosY) * partialTicks;
        double dz = entity.lastTickPosZ + (entity.posZ - entity.lastTickPosZ) * partialTicks;

        if (RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.isRendererEnabled())
        {
            renderChunkUnloadBuckets(mc, entity, dx, dy, dz, Configs.Internal.CHUNK_UNLOAD_BUCKET_OVERLAY_Y.getDoubleValue());
        }

        if (RendererToggle.OVERLAY_BEACON_RANGE.isRendererEnabled())
        {
            OverlayRendererBeaconRange.renderBeaconBoxForPlayerIfHoldingItem(mc.player, dx, dy, dz, partialTicks);
        }

        if (RendererToggle.OVERLAY_SPAWNER_POSITIONS.isRendererEnabled())
        {
            RenderContainer.SPAWNER_RENDERER.renderPositionText(dx, dy, dz);
        }

        if (RendererToggle.OVERLAY_WATER_FALLS.isRendererEnabled())
        {
            RenderContainer.WATER_FALL_RENDERER.renderPositionText(dx, dy, dz);
        }

        RenderContainer.INSTANCE.render(entity, mc, partialTicks);
    }

    private static void renderChunkUnloadBuckets(Minecraft mc, Entity entity, double dx, double dy, double dz, double chunkOverlayY)
    {
        final int centerX = MathHelper.floor(entity.posX) >> 4;
        final int centerZ = MathHelper.floor(entity.posZ) >> 4;
        final float y = (float) chunkOverlayY;
        final float scale = Configs.Generic.CHUNK_UNLOAD_BUCKET_FONT_SCALE.getFloatValue();
        int r = Configs.Generic.CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS.getIntegerValue();

        if (r == -1)
        {
            r = GameUtils.getRenderDistanceChunks();
        }

        for (int xOff = -r; xOff <= r; xOff++)
        {
            for (int zOff = -r; zOff <= r; zOff++)
            {
                int cx = centerX + xOff;
                int cz = centerZ + zOff;
                int bucket = MiscUtils.getChunkUnloadBucket(cx, cz);
                String str = String.valueOf(bucket);
                TextRenderUtils.renderTextPlate(Collections.singletonList(str), (cx << 4) + 8.5d - dx, y - dy, (cz << 4) + 8.5D - dz, scale);
            }
        }
    }
}
