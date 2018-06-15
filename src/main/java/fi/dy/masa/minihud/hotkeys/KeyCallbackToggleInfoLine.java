package fi.dy.masa.minihud.hotkeys;

import fi.dy.masa.malilib.hotkeys.IHotkeyCallback;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.minihud.config.InfoToggle;

public class KeyCallbackToggleInfoLine implements IHotkeyCallback
{
    protected final InfoToggle toggle;

    public KeyCallbackToggleInfoLine(InfoToggle toggle)
    {
        this.toggle = toggle;
    }

    @Override
    public boolean onKeyAction(KeyAction action, IKeybind key)
    {
        if (action == KeyAction.PRESS)
        {
            this.toggle.setBooleanValue(! this.toggle.getBooleanValue());
            KeyCallbackToggleHud.toggledSomething = true;
            return true;
        }

        return false;
    }
}
