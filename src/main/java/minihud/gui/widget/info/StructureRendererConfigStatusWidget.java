package minihud.gui.widget.info;

import malilib.overlay.widget.sub.HotkeyedBooleanConfigStatusWidget;
import malilib.util.data.ConfigOnTab;
import minihud.config.StructureToggle;

public class StructureRendererConfigStatusWidget extends HotkeyedBooleanConfigStatusWidget
{
    public StructureRendererConfigStatusWidget(StructureToggle config, ConfigOnTab configOnTab)
    {
        super(config.getBooleanConfig(), config::getKeyBind, configOnTab, "minihud:csi_value_structure_toggle");
    }
}
