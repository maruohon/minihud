package fi.dy.masa.minihud.hotkeys;

import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.malilib.hotkeys.KeyCallbackToggleBooleanConfigWithMessage;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DebugInfoUtils;

public class KeyCallbackToggleDebugRenderer extends KeyCallbackToggleBooleanConfigWithMessage
{
    protected final RendererToggle rendererConfig;

    public KeyCallbackToggleDebugRenderer(RendererToggle config)
    {
        super(config);

        this.rendererConfig = config;
    }

    @Override
    public boolean onKeyAction(KeyAction action, IKeybind key)
    {
        if (super.onKeyAction(action, key))
        {
            DebugInfoUtils.toggleDebugRenderer(this.rendererConfig);
            KeyCallbackToggleHud.setToggledSomething();
            return true;
        }

        return false;
    }
}
