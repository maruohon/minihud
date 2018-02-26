package fi.dy.masa.minihud.config.gui;

import org.apache.commons.lang3.StringUtils;
import org.lwjgl.input.Keyboard;
import com.mumfrey.liteloader.modconfig.ConfigPanelHost;
import fi.dy.masa.minihud.config.IConfigHotkey;
import net.minecraft.util.text.TextFormatting;

public abstract class ConfigPanelHotkeysBase extends ConfigPanelSub
{
    private ConfigButtonHotkey activeButton;

    public ConfigPanelHotkeysBase(String title, MiniHudConfigPanel parent)
    {
        super(title, parent);
    }

    protected void setActiveButton(ConfigButtonHotkey button)
    {
        if (this.activeButton != null)
        {
            this.activeButton.clearSelection();
        }

        this.activeButton = button;
    }

    @Override
    public void keyPressed(ConfigPanelHost host, char keyChar, int keyCode)
    {
        if (this.activeButton != null)
        {
            this.activeButton.setHotkey(keyCode != Keyboard.KEY_ESCAPE ? Keyboard.getKeyName(keyCode) : "");
            this.setActiveButton(null);
        }
        else
        {
            super.keyPressed(host, keyChar, keyCode);
        }
    }

    protected static class ConfigButtonHotkey extends ConfigButtonBase
    {
        private final ConfigPanelHotkeysBase host;
        private final IConfigHotkey hotkey;
        private boolean selected;

        public ConfigButtonHotkey(int id, int x, int y, int width, int height, IConfigHotkey hotkey, ConfigPanelHotkeysBase host)
        {
            super(id, x, y, width, height);

            this.host = host;
            this.hotkey = hotkey;

            this.updateDisplayString();
        }

        @Override
        public void onMouseClicked()
        {
            this.selected = true;
            this.updateDisplayString();
            this.host.setActiveButton(this);
        }

        public void setHotkey(String value)
        {
            this.hotkey.setHotkey(value);
            this.updateDisplayString();
        }

        public void clearSelection()
        {
            this.selected = false;
            this.updateDisplayString();
        }

        private void updateDisplayString()
        {
            String valueStr = this.hotkey.getHotkey();

            if (StringUtils.isBlank(valueStr))
            {
                valueStr = "NONE";
            }

            if (this.selected)
            {
                this.displayString = "> " + TextFormatting.YELLOW + valueStr + TextFormatting.RESET + " <";
            }
            else
            {
                this.displayString = valueStr;
            }
        }
    }
}
