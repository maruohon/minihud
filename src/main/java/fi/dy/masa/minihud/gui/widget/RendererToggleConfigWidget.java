package fi.dy.masa.minihud.gui.widget;

import fi.dy.masa.malilib.gui.config.ConfigWidgetContext;
import fi.dy.masa.malilib.gui.widget.list.entry.DataListEntryWidgetData;
import fi.dy.masa.malilib.gui.widget.list.entry.config.BaseHotkeyedBooleanConfigWidget;
import fi.dy.masa.minihud.config.RendererToggle;

public class RendererToggleConfigWidget extends BaseHotkeyedBooleanConfigWidget<RendererToggle>
{
    public RendererToggleConfigWidget(RendererToggle config,
                                      DataListEntryWidgetData constructData,
                                      ConfigWidgetContext ctx)
    {
        super(config, config.getBooleanConfig(), config.getKeyBind(), constructData, ctx);
    }
}
