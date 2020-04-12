package fi.dy.masa.minihud.renderer;

import com.mojang.blaze3d.matrix.MatrixStack;
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

    public static void renderOverlays(Minecraft mc, float partialTicks, MatrixStack matrixStack)
    {
        Entity entity = mc.getRenderViewEntity();

        if (canRender == false)
        {
            // Don't render before the player has been placed in the actual proper position,
            // otherwise some of the renderers mess up.
            // The magic 8.5, 65, 8.5 comes from the WorldClient constructor
            if (System.currentTimeMillis() - loginTime >= 5000 || entity.getPosX() != 8.5 || entity.getPosY() != 65 || entity.getPosZ() != 8.5)
            {
                canRender = true;
            }
            else
            {
                return;
            }
        }

        RenderContainer.INSTANCE.render(entity, mc, partialTicks, matrixStack);
    }
}
