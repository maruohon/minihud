package fi.dy.masa.minihud.event;

import fi.dy.masa.malilib.interfaces.IClientTickHandler;
import net.minecraft.client.Minecraft;

public class ClientTickHandler implements IClientTickHandler
{
    @Override
    public void onClientTick(Minecraft mc)
    {
        if (mc.world != null && mc.player != null)
        {
            RenderHandler.getInstance().updateData(mc);
        }
    }
}
