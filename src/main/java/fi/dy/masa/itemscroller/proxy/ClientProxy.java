package fi.dy.masa.itemscroller.proxy;

import net.minecraftforge.common.MinecraftForge;

import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.event.InputEventHandler;

public class ClientProxy extends CommonProxy
{
    @Override
    public void registerEventHandlers()
    {
        MinecraftForge.EVENT_BUS.register(new InputEventHandler());
        MinecraftForge.EVENT_BUS.register(new Configs());
    }
}
