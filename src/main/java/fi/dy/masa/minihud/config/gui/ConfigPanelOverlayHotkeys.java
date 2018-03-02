package fi.dy.masa.minihud.config.gui;

import org.lwjgl.input.Keyboard;
import com.mumfrey.liteloader.modconfig.ConfigPanelHost;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.config.OverlayHotkeys;
import fi.dy.masa.minihud.config.interfaces.IConfigHotkey;

public class ConfigPanelOverlayHotkeys extends ConfigPanelHotkeysBase
{
    public ConfigPanelOverlayHotkeys(MiniHudConfigPanel parent)
    {
        super("Overlay Hotkeys", parent);
    }

    @Override
    protected OverlayHotkeys[] getConfigs()
    {
        return OverlayHotkeys.values();
    }

    @Override
    public void addOptions(ConfigPanelHost host)
    {
        this.clearOptions();

        int x = 10;
        int y = 10;
        int i = 0;
        int labelWidth = this.getMaxLabelWidth(this.getConfigs()) + 10;
        String toggleKey = Keyboard.getKeyName(LiteModMiniHud.KEY_TOGGLE_MODE.getKeyCode());

        for (IConfigHotkey hotkey : this.getConfigs())
        {
            this.addLabel(i, x, y + 7, labelWidth, 8, 0xFFFFFFFF, hotkey.getName());
            this.addConfigComment(x, y + 2, labelWidth, 10, "Hotkey to toggle the " + hotkey.getName() + " renderer");

            this.addLabel(i, x + labelWidth + 10, y + 7, 20, 8, 0xFFFFFFFF, toggleKey + " +");
            this.addControl(new ConfigButtonHotkey(i + 1, x + labelWidth + 30, y, 50, 20, hotkey, this), this.getConfigListener());

            i += 2;
            y += 21;
        }
    }
}
