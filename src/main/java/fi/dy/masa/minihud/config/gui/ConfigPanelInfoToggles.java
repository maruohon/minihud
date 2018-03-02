package fi.dy.masa.minihud.config.gui;

import fi.dy.masa.minihud.config.InfoToggle;

public class ConfigPanelInfoToggles extends ConfigPanelSub
{
    public ConfigPanelInfoToggles(MiniHudConfigPanel parent)
    {
        super("Info Toggles", parent);
    }

    @Override
    protected InfoToggle[] getConfigs()
    {
        return InfoToggle.values();
    }
}
