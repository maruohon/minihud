package fi.dy.masa.minihud.config.gui;

import com.mumfrey.liteloader.modconfig.ConfigPanelHost;
import fi.dy.masa.minihud.config.DebugHotkeys;
import fi.dy.masa.minihud.config.interfaces.IConfigHotkey;

public class ConfigPanelDebugHotkeys extends ConfigPanelHotkeysBase
{
    public ConfigPanelDebugHotkeys(MiniHudConfigPanel parent)
    {
        super("Vanilla Debug Renderer Hotkeys", parent);
    }

    @Override
    protected DebugHotkeys[] getConfigs()
    {
        return DebugHotkeys.values();
    }

    @Override
    public void addOptions(ConfigPanelHost host)
    {
        this.clearOptions();

        int x = 10;
        int y = 10;
        int i = 0;
        int labelWidth = this.getMaxLabelWidth(this.getConfigs()) + 10;

        for (IConfigHotkey hotkey : this.getConfigs())
        {
            this.addLabel(i, x, y + 7, labelWidth, 8, 0xFFFFFFFF, hotkey.getName());
            this.addConfigComment(x, y + 2, labelWidth, 10, "Hotkey to toggle the " + hotkey.getName() + " debug renderer");

            this.addLabel(i, x + labelWidth + 10, y + 7, 24, 8, 0xFFFFFFFF, "F3 +");
            this.addButton(new ConfigButtonHotkey(i + 1, x + labelWidth + 34, y, 50, 20, hotkey, this), this.getConfigListener());

            i += 2;
            y += 21;
        }
    }
}
