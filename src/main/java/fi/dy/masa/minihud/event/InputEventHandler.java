package fi.dy.masa.minihud.event;

import org.lwjgl.input.Keyboard;

import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.gameevent.InputEvent.KeyInputEvent;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;

import fi.dy.masa.minihud.proxy.ClientProxy;

@SideOnly(Side.CLIENT)
public class InputEventHandler
{
    private int numKey;

    @SubscribeEvent
    public void onKeyInputEvent(KeyInputEvent event)
    {
        int key = Keyboard.getEventKey();
        boolean state = Keyboard.getEventKeyState();

        if (state == true && key == ClientProxy.keyToggleMode.getKeyCode())
        {
            if (this.numKey != 0)
            {
                RenderEventHandler.mask ^= this.numKey;
            }
            else
            {
                RenderEventHandler.enabled = ! RenderEventHandler.enabled;
            }
        }
        else if (key >= Keyboard.KEY_1 && key <= Keyboard.KEY_9)
        {
            if (state == true)
            {
                this.numKey |= (1 << (key - Keyboard.KEY_1));
            }
            else
            {
                this.numKey &= ~(1 << (key - Keyboard.KEY_1));
            }
        }
    }
}
