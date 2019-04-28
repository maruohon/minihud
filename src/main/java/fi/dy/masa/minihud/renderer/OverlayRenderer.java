package fi.dy.masa.minihud.renderer;

import net.minecraft.client.MinecraftClient;

public class OverlayRenderer
{
    private static final RenderContainer RC = new RenderContainer();

    public static void renderOverlays(MinecraftClient mc, float partialTicks)
    {
        RC.render(mc.getCameraEntity(), mc, partialTicks);
    }
}
