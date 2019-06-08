package fi.dy.masa.minihud.renderer;

import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;

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
        Entity entity = mc.getRenderViewEntity();

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

        if (RendererToggle.OVERLAY_LIGHT_LEVEL.getBooleanValue())
        {
            OverlayRendererLightLevel.render(dx, dy, dz, entity, mc);
        }

        RenderContainer.INSTANCE.render(entity, mc, partialTicks);
    }
}
