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
                RenderEventHandler.getInstance().xorEnabledMask(this.numKey);
            }
            else
            {
                RenderEventHandler.getInstance().toggleEnabled();
            }
        }
        else
        {
            int bit = this.getBitForKey(key);

            if (bit != 0)
            {
                this.numKey = state == true ? this.numKey | bit : this.numKey & ~bit;
            }
        }
    }

    private int getBitForKey(int key)
    {
        switch (key)
        {
            case Keyboard.KEY_0: return 1 << 9;
            case Keyboard.KEY_1: return 1 << 0;
            case Keyboard.KEY_2: return 1 << 1;
            case Keyboard.KEY_3: return 1 << 2;
            case Keyboard.KEY_4: return 1 << 3;
            case Keyboard.KEY_5: return 1 << 4;
            case Keyboard.KEY_6: return 1 << 5;
            case Keyboard.KEY_7: return 1 << 6;
            case Keyboard.KEY_8: return 1 << 7;
            case Keyboard.KEY_9: return 1 << 8;
            case Keyboard.KEY_A: return 1 << 10;
            case Keyboard.KEY_B: return 1 << 11;
        }

        return 0;
    }
}
