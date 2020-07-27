package fi.dy.masa.minihud.hotkeys;

import fi.dy.masa.malilib.input.IKeyBind;
import fi.dy.masa.malilib.input.KeyAction;
import fi.dy.masa.malilib.input.KeyCallbackToggleBooleanConfigWithMessage;
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
    public boolean onKeyAction(KeyAction action, IKeyBind key)
    {
        super.onKeyAction(action, key);
        DebugInfoUtils.toggleDebugRenderer(this.rendererConfig);
        return true;
    }
}
