package fi.dy.masa.itemscroller.config;

import fi.dy.masa.itemscroller.Reference;
import fi.dy.masa.malilib.config.gui.ConfigPanelBase;
import fi.dy.masa.malilib.config.gui.GuiModConfigs;

public class ItemScrollerConfigPanel extends ConfigPanelBase
{
    @Override
    protected String getPanelTitlePrefix()
    {
        return Reference.MOD_NAME + " options";
    }

    @Override
    protected void createSubPanels()
    {
        String modId = Reference.MOD_ID;

        this.addSubPanel((new GuiModConfigs(modId, "Toggles", Configs.Toggles.OPTIONS)).setConfigWidth(100));
        this.addSubPanel((new GuiModConfigs(modId, "Generic", Configs.Generic.OPTIONS)).setConfigWidth(160));
        this.addSubPanel((new GuiModConfigs(modId, "Hotkeys", Hotkeys.HOTKEY_LIST)).setConfigWidth(210));
    }
}
