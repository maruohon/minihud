package fi.dy.masa.minihud.renderer;

import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.MinecraftClient;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.Vec3d;

public class OverlayRenderer
{
    private static long loginTime;
    private static boolean canRender;

    public static void resetRenderTimeout()
    {
        canRender = false;
        loginTime = System.currentTimeMillis();
    }

    public static void renderOverlays(MinecraftClient mc, float partialTicks)
    {
        Entity entity = mc.getCameraEntity();

        if (canRender == false)
        {
            // Don't render before the player has been placed in the actual proper position,
            // otherwise some of the renderers mess up.
            // The magic 8.5, 65, 8.5 comes from the WorldClient constructor
            if (System.currentTimeMillis() - loginTime >= 5000 || entity.x != 8.5 || entity.y != 65 || entity.z != 8.5)
            {
                canRender = true;
            }
            else
            {
                return;
            }
        }

        if (RendererToggle.OVERLAY_LIGHT_LEVEL.getBooleanValue())
        {
            Vec3d cameraPos = mc.gameRenderer.getCamera().getPos();
            OverlayRendererLightLevel.render(cameraPos.x, cameraPos.y, cameraPos.z, entity, mc);
        }
        else if (RendererToggle.OVERLAY_HARVEST_LEVEL.getBooleanValue()) 
        {
        	Vec3d cameraPos = mc.gameRenderer.getCamera().getPos();
        	OverlayRendererHarvestLevel.render(cameraPos.x, cameraPos.y, cameraPos.z, entity, mc);
        }

        RenderContainer.INSTANCE.render(entity, mc, partialTicks);
    }
}
