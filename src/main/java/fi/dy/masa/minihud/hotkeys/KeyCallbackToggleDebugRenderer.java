package fi.dy.masa.minihud.hotkeys;

import fi.dy.masa.malilib.input.KeyBind;
import fi.dy.masa.malilib.input.KeyAction;
import fi.dy.masa.malilib.input.callback.ToggleBooleanWithMessageKeyCallback;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.util.DebugInfoUtils;

public class KeyCallbackToggleDebugRenderer extends ToggleBooleanWithMessageKeyCallback
{
    protected final RendererToggle rendererConfig;

    public KeyCallbackToggleDebugRenderer(RendererToggle config)
    {
        super(config);

        this.rendererConfig = config;
    }

    @Override
    public boolean onKeyAction(KeyAction action, KeyBind key)
    {
        super.onKeyAction(action, key);
        DebugInfoUtils.toggleDebugRenderer(this.rendererConfig);
        return true;
    }
}
