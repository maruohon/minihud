package minihud.event;

import malilib.util.game.wrap.GameUtils;

public class ClientTickHandler implements malilib.event.ClientTickHandler
{
    @Override
    public void onClientTick()
    {
        RenderHandler.INSTANCE.onClientTick(GameUtils.getClient());
    }
}
