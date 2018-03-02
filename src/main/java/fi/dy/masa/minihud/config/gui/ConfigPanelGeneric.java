package fi.dy.masa.minihud.config.gui;

import fi.dy.masa.minihud.config.ConfigsGeneric;
import fi.dy.masa.minihud.config.interfaces.IConfig;

public class ConfigPanelGeneric extends ConfigPanelSub
{
    public ConfigPanelGeneric(MiniHudConfigPanel parent)
    {
        super("Generic", parent);
    }

    @Override
    protected IConfig[] getConfigs()
    {
        return ConfigsGeneric.values();
    }
}
