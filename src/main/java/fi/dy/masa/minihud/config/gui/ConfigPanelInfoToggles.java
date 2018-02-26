package fi.dy.masa.minihud.config.gui;

import fi.dy.masa.minihud.config.Configs;

public class ConfigPanelInfoToggles extends ConfigPanelSub
{
    public ConfigPanelInfoToggles(MiniHudConfigPanel parent)
    {
        super("Info Toggles", parent);
    }

    @Override
    protected Configs.InfoToggle[] getConfigs()
    {
        return Configs.InfoToggle.values();
    }
}
