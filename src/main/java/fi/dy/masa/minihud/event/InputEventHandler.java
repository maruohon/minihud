package fi.dy.masa.minihud.event;

import org.lwjgl.input.Keyboard;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.settings.KeyBinding;
import net.minecraftforge.client.settings.KeyModifier;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.gameevent.InputEvent.KeyInputEvent;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.proxy.ClientProxy;

@SideOnly(Side.CLIENT)
public class InputEventHandler
{
    private boolean toggledInfo;

    @SubscribeEvent
    public void onKeyInputEvent(KeyInputEvent event)
    {
        int toggleKey = ClientProxy.keyToggleMode.getKeyCode();
        int key = Keyboard.getEventKey();
        boolean state = Keyboard.getEventKeyState();
        int bit = Configs.getBitmaskForKey(key);

        // Toggle the HUD when releasing the toggle key, if no infos were toggled while it was down
        if (state == false && key == toggleKey)
        {
            if (this.toggledInfo == false)
            {
                RenderEventHandler.getInstance().toggleEnabled();
            }

            this.toggledInfo = false;
        }
        else if (state && bit != 0 && Keyboard.isKeyDown(toggleKey))
        {
            RenderEventHandler.getInstance().xorEnabledMask(bit);
            this.toggledInfo = true;
            KeyBinding.unPressAllKeys();
        }
    }

    public static boolean isRequiredKeyActive(KeyModifier key)
    {
        if (key == KeyModifier.NONE)    { return true;                       }
        if (key == KeyModifier.ALT)     { return GuiScreen.isAltKeyDown();   }
        if (key == KeyModifier.CONTROL) { return GuiScreen.isCtrlKeyDown();  }
        if (key == KeyModifier.SHIFT)   { return GuiScreen.isShiftKeyDown(); }
        return false;
    }
}
