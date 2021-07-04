package fi.dy.masa.minihud.gui.widget.info;

import fi.dy.masa.malilib.overlay.widget.sub.HotkeyedBooleanConfigStatusWidget;
import fi.dy.masa.malilib.util.data.ConfigOnTab;
import fi.dy.masa.minihud.config.RendererToggle;

public class RendererToggleConfigStatusWidget extends HotkeyedBooleanConfigStatusWidget
{
    public RendererToggleConfigStatusWidget(RendererToggle config, ConfigOnTab configOnTab)
    {
        super(config.getBooleanConfig(), config::getKeyBind, configOnTab, "minihud:csi_value_renderer_toggle");
    }
}
