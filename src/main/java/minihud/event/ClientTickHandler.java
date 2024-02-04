package minihud.event;

import malilib.util.game.wrap.GameWrap;

public class ClientTickHandler implements malilib.event.ClientTickHandler
{
    @Override
    public void onClientTick()
    {
        RenderHandler.INSTANCE.onClientTick(GameWrap.getClient());
    }
}
