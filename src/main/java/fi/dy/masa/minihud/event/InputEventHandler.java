package fi.dy.masa.minihud.event;

import java.util.HashSet;
import java.util.Set;
import org.lwjgl.input.Keyboard;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.util.DebugInfoUtils;
import fi.dy.masa.minihud.util.IMinecraftAccessor;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.settings.KeyBinding;

public class InputEventHandler
{
    private static final InputEventHandler INSTANCE = new InputEventHandler();

    private final Set<Integer> modifierKeys = new HashSet<>();
    private boolean toggledInfo;

    private InputEventHandler()
    {
        this.modifierKeys.add(Keyboard.KEY_LSHIFT);
        this.modifierKeys.add(Keyboard.KEY_RSHIFT);
        this.modifierKeys.add(Keyboard.KEY_LCONTROL);
        this.modifierKeys.add(Keyboard.KEY_RCONTROL);
        this.modifierKeys.add(Keyboard.KEY_LMENU);
        this.modifierKeys.add(Keyboard.KEY_RMENU);
    }

    public static InputEventHandler getInstance()
    {
        return INSTANCE;
    }

    public boolean onKeyInput()
    {
        Minecraft mc = Minecraft.getMinecraft();

        // Keybinds shouldn't work inside GUIs
        if (mc.currentScreen != null)
        {
            return false;
        }

        int eventKey = Keyboard.getEventKey();
        boolean eventKeyState = Keyboard.getEventKeyState();
        int bitMaskForEventKey = Configs.getBitmaskForDebugKey(eventKey);
        boolean cancel = false;

        if (eventKeyState && Keyboard.isKeyDown(Keyboard.KEY_F3) && bitMaskForEventKey != 0)
        {
            DebugInfoUtils.toggleDebugRenderers(mc, bitMaskForEventKey);
            KeyBinding.setKeyBindState(eventKey, false);

            // This prevent the F3 screen from opening after releasing the F3 key
            ((IMinecraftAccessor) mc).setActionKeyF3(true);
            cancel = true;
        }

        int toggleKey = LiteModMiniHud.KEY_TOGGLE_MODE.getKeyCode();

        // Toggle the HUD when releasing the toggle key, if no info types were toggled while it was down
        if (eventKeyState == false && eventKey == toggleKey)
        {
            if (this.toggledInfo == false)
            {
                RenderEventHandler.getInstance().toggleEnabled();
            }

            this.toggledInfo = false;
            cancel = true;
        }
        else if (eventKeyState && Keyboard.isKeyDown(toggleKey))
        {
            bitMaskForEventKey = Configs.getBitmaskForInfoKey(eventKey);

            if (bitMaskForEventKey != 0)
            {
                RenderEventHandler.getInstance().xorInfoLineEnabledMask(bitMaskForEventKey);
                this.toggledInfo = true;
                cancel = true;
            }

            bitMaskForEventKey = Configs.getBitmaskForOverlayKey(eventKey);

            if (bitMaskForEventKey != 0)
            {
                RenderEventHandler.getInstance().xorOverlayRendererEnabledMask(bitMaskForEventKey);
                this.toggledInfo = true;
                cancel = true;
            }
        }

        return cancel && this.modifierKeys.contains(eventKey) == false;
    }

    public static boolean isRequiredKeyActive()
    {
        KeyModifier key = Configs.requiredKey;

        if (key == KeyModifier.NONE)    { return true;                       }
        if (key == KeyModifier.ALT)     { return GuiScreen.isAltKeyDown();   }
        if (key == KeyModifier.CONTROL) { return GuiScreen.isCtrlKeyDown();  }
        if (key == KeyModifier.SHIFT)   { return GuiScreen.isShiftKeyDown(); }
        return false;
    }

    public enum KeyModifier
    {
        NONE,
        ALT,
        CONTROL,
        SHIFT;
    }
}
