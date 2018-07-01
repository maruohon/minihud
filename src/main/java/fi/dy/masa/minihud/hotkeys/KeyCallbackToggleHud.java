package fi.dy.masa.minihud.hotkeys;

import fi.dy.masa.malilib.hotkeys.IHotkeyCallback;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.minihud.event.RenderEventHandler;

public class KeyCallbackToggleHud implements IHotkeyCallback
{
    private static boolean toggledSomething;

    public static void setToggledSomething()
    {
        toggledSomething = true;
    }

    @Override
    public boolean onKeyAction(KeyAction action, IKeybind key)
    {
        if (action == KeyAction.RELEASE)
        {
            if (toggledSomething == false)
            {
                RenderEventHandler.getInstance().toggleEnabled();
                return true;
            }

            toggledSomething = false;
        }

        return false;
    }
}
