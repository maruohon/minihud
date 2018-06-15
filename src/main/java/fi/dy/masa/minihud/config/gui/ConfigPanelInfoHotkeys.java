package fi.dy.masa.minihud.config.gui;

import fi.dy.masa.malilib.config.IConfigBoolean;
import fi.dy.masa.malilib.config.gui.ConfigPanelHotkeysBase;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;

public class ConfigPanelInfoHotkeys extends ConfigPanelHotkeysBase
{
    public ConfigPanelInfoHotkeys(MiniHudConfigPanel parent)
    {
        super("Info Hotkeys", InfoToggle.values(), parent);
    }

    @Override
    protected String getHotkeyComment(IHotkey hotkey)
    {
        return "Hotkey to toggle the '" + ((IConfigBoolean) hotkey).getPrettyName() + "' info line";
    }

    @Override
    protected void onSettingsChanged()
    {
        super.onSettingsChanged();

        Configs.save();
        Configs.load();
    }
}
