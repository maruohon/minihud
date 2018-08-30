package fi.dy.masa.minihud.config.gui;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.gui.ConfigPanelHotkeysBase;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.minihud.config.RendererToggle;

public class ConfigPanelRendererHotkeys extends ConfigPanelHotkeysBase
{
    public ConfigPanelRendererHotkeys(String modId, MiniHudConfigPanel parent)
    {
        super(modId, "Renderer Hotkeys", ImmutableList.copyOf(RendererToggle.values()), parent);
    }

    @Override
    protected String getHotkeyComment(IHotkey hotkey)
    {
        return "Hotkey to toggle the '" + hotkey.getPrettyName() + "' renderer";
    }
}
