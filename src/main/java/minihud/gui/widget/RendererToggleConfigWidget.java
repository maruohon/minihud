package minihud.gui.widget;

import malilib.gui.config.ConfigWidgetContext;
import malilib.gui.widget.list.entry.DataListEntryWidgetData;
import malilib.gui.widget.list.entry.config.BaseHotkeyedBooleanConfigWidget;
import minihud.config.RendererToggle;

public class RendererToggleConfigWidget extends BaseHotkeyedBooleanConfigWidget<RendererToggle>
{
    public RendererToggleConfigWidget(RendererToggle config,
                                      DataListEntryWidgetData constructData,
                                      ConfigWidgetContext ctx)
    {
        super(config, config.getBooleanConfig(), config.getKeyBind(), constructData, ctx);
    }
}
