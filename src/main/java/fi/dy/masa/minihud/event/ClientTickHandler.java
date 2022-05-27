package fi.dy.masa.minihud.event;

import fi.dy.masa.malilib.util.game.wrap.GameUtils;

public class ClientTickHandler implements fi.dy.masa.malilib.event.ClientTickHandler
{
    @Override
    public void onClientTick()
    {
        RenderHandler.INSTANCE.onClientTick(GameUtils.getClient());
    }
}
