package fi.dy.masa.minihud.config.gui;

import fi.dy.masa.malilib.config.IConfigValue;
import fi.dy.masa.malilib.config.gui.ConfigPanelBase;
import fi.dy.masa.malilib.config.gui.ConfigPanelSub;
import fi.dy.masa.minihud.config.Configs;

public class ConfigPanelSubMinihud extends ConfigPanelSub
{
    public ConfigPanelSubMinihud(String title, IConfigValue[] configs, ConfigPanelBase parent)
    {
        super(title, parent);

        this.configs = configs;
    }

    @Override
    protected void onSettingsChanged()
    {
        super.onSettingsChanged();

        Configs.save();
        Configs.load();
    }
}
