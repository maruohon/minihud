package fi.dy.masa.minihud.renderer;

import fi.dy.masa.minihud.config.RendererToggle;
import net.minecraft.client.MinecraftClient;
import net.minecraft.util.math.Vec3d;

public class OverlayRenderer
{
    private static final RenderContainer RC = new RenderContainer();

    public static void renderOverlays(MinecraftClient mc, float partialTicks)
    {
        if (RendererToggle.OVERLAY_LIGHT_LEVEL.getBooleanValue())
        {
            Vec3d cameraPos = mc.gameRenderer.getCamera().getPos();
            OverlayRendererLightLevel.render(cameraPos.x, cameraPos.y, cameraPos.z, mc.getCameraEntity(), mc);
        }

        RC.render(mc.getCameraEntity(), mc, partialTicks);
    }
}
