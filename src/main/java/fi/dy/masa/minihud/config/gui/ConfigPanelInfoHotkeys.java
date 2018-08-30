package fi.dy.masa.minihud.config.gui;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.gui.ConfigPanelHotkeysBase;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.minihud.config.InfoToggle;

public class ConfigPanelInfoHotkeys extends ConfigPanelHotkeysBase
{
    public ConfigPanelInfoHotkeys(String modId, MiniHudConfigPanel parent)
    {
        super(modId, "Info Hotkeys", ImmutableList.copyOf(InfoToggle.values()), parent);
    }

    @Override
    protected String getHotkeyComment(IHotkey hotkey)
    {
        return "Hotkey to toggle the '" + hotkey.getPrettyName() + "' info line";
    }
}
