package fi.dy.masa.minihud.config.gui;

import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.IConfig;

public class ConfigPanelGeneric extends ConfigPanelSub
{
    public ConfigPanelGeneric(MiniHudConfigPanel parent)
    {
        super("Generic", parent);
    }

    @Override
    protected IConfig[] getConfigs()
    {
        return Configs.Generic.values();
    }
}
