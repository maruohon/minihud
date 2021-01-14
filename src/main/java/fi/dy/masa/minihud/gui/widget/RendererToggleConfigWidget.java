package fi.dy.masa.minihud.gui.widget;

import fi.dy.masa.malilib.gui.config.ConfigWidgetContext;
import fi.dy.masa.malilib.gui.widget.list.entry.config.BaseHotkeyedBooleanConfigWidget;
import fi.dy.masa.minihud.config.RendererToggle;

public class RendererToggleConfigWidget extends BaseHotkeyedBooleanConfigWidget
{
    public RendererToggleConfigWidget(int x, int y, int width, int height, int listIndex,
                                     int originalListIndex, RendererToggle config, ConfigWidgetContext ctx)
    {
        super(x, y, width, 22, listIndex, originalListIndex, config, config.getBooleanConfig(), config.getKeyBind(), ctx);
    }
}
