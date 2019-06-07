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

        this.addSubPanel((new GuiModConfigs(modId, Configs.Toggles.OPTIONS, "itemscroller.gui.button.config_gui.toggles")).setConfigWidth(100));
        this.addSubPanel((new GuiModConfigs(modId, Configs.Generic.OPTIONS, "itemscroller.gui.button.config_gui.generic")).setConfigWidth(160));
        this.addSubPanel((new GuiModConfigs(modId, Hotkeys.HOTKEY_LIST, "itemscroller.gui.button.config_gui.hotkeys")).setConfigWidth(210));
    }
}
