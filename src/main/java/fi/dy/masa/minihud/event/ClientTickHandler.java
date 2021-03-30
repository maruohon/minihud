package fi.dy.masa.minihud.event;

import net.minecraft.client.Minecraft;

public class ClientTickHandler implements fi.dy.masa.malilib.event.ClientTickHandler
{
    @Override
    public void onClientTick(Minecraft mc)
    {
        RenderHandler.INSTANCE.onClientTick(mc);
    }
}
