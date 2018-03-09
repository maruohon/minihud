package fi.dy.masa.minihud.config.gui;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.lwjgl.input.Keyboard;
import com.mumfrey.liteloader.modconfig.AbstractConfigPanel;
import com.mumfrey.liteloader.modconfig.ConfigPanelHost;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.interfaces.ConfigType;
import fi.dy.masa.minihud.config.interfaces.IConfig;
import fi.dy.masa.minihud.config.interfaces.IConfigBoolean;
import fi.dy.masa.minihud.config.interfaces.IConfigGeneric;
import fi.dy.masa.minihud.config.interfaces.IConfigOptionList;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiButton;
import net.minecraft.client.resources.I18n;
import net.minecraft.util.text.TextFormatting;

public abstract class ConfigPanelSub extends AbstractConfigPanel
{
    private final MiniHudConfigPanel parent;
    private final Map<IConfig, ConfigTextField> textFields = new HashMap<>();
    private final ConfigOptionListenerGeneric<ConfigButtonBase> listener = new ConfigOptionListenerGeneric<>();
    private final List<ConfigButtonBase> buttons = new ArrayList<>();
    private final List<HoverInfo> configComments = new ArrayList<>();
    private final String title;

    public ConfigPanelSub(String title, MiniHudConfigPanel parent)
    {
        this.title = title;
        this.parent = parent;
    }

    @Override
    public String getPanelTitle()
    {
        return this.title;
    }

    @Override
    public void onPanelHidden()
    {
        boolean dirty = false;

        if (this.listener.isDirty())
        {
            dirty = true;
            this.listener.resetDirty();
        }

        dirty |= this.handleTextFields();

        if (dirty)
        {
            Configs.save();
            Configs.load();
        }
    }

    @Override
    public void mousePressed(ConfigPanelHost host, int mouseX, int mouseY, int mouseButton)
    {
        for (ConfigButtonBase button : this.buttons)
        {
            if (button.mousePressed(this.mc, mouseX, mouseY))
            {
                button.onMouseButtonClicked(mouseButton);
                this.listener.actionPerformed(button);
                // Don't call super if the button got handled
                return;
            }
        }

        super.mousePressed(host, mouseX, mouseY, mouseButton);
    }

    protected <T extends ConfigButtonBase> void addButton(T button, ConfigOptionListener<T> listener)
    {
        this.buttons.add(button);
        this.addControl(button, listener);
    }

    protected boolean handleTextFields()
    {
        boolean dirty = false;

        for (IConfig config : this.getConfigs())
        {
            ConfigType type = config.getType();

            if (type == ConfigType.STRING ||
                type == ConfigType.HEX_STRING ||
                type == ConfigType.INTEGER ||
                type == ConfigType.DOUBLE)
            {
                ConfigTextField field = this.getTextFieldFor(config);

                if (field != null && config instanceof IConfigGeneric)
                {
                    String newValue = field.getText();

                    if (newValue.equals(config.getStringValue()) == false)
                    {
                        ((IConfigGeneric) config).setValueFromString(newValue);
                        dirty = true;
                    }
                }
            }
        }

        return dirty;
    }

    protected abstract IConfig[] getConfigs();

    protected ConfigOptionListenerGeneric<ConfigButtonBase> getConfigListener()
    {
        return this.listener;
    }

    @Override
    public void addOptions(ConfigPanelHost host)
    {
        this.clearOptions();

        int x = 10;
        int y = 10;
        int configHeight = 20;
        int labelWidth = this.getMaxLabelWidth(this.getConfigs()) + 10;

        for (IConfig config : this.getConfigs())
        {
            this.addLabel(0, x, y + 7, labelWidth, 8, 0xFFFFFFFF, config.getName());

            String comment = config.getComment();
            ConfigType type = config.getType();

            if (comment != null)
            {
                this.addConfigComment(x, y + 2, labelWidth, 10, comment);
            }

            if (type == ConfigType.BOOLEAN)
            {
                this.addButton(new ConfigButtonBoolean(0, x + labelWidth, y, 204, configHeight, (IConfigBoolean) config), this.listener);
            }
            else if (type == ConfigType.OPTION_LIST)
            {
                this.addButton(new ConfigButtonOptionList(0, x + labelWidth, y, 204, configHeight, (IConfigOptionList) config), this.listener);
            }
            else if (type == ConfigType.STRING ||
                     type == ConfigType.HEX_STRING ||
                     type == ConfigType.INTEGER ||
                     type == ConfigType.DOUBLE)
            {
                ConfigTextField field = this.addTextField(0, x + labelWidth, y + 1, 200, configHeight - 3);
                field.setText(config.getStringValue());
                field.getNativeTextField().setMaxStringLength(128);
                this.addTextField(config, field);
            }

            y += configHeight + 1;
        }
    }

    @Override
    public void clearOptions()
    {
        super.clearOptions();
        this.buttons.clear();
    }

    @Override
    public void drawPanel(ConfigPanelHost host, int mouseX, int mouseY, float partialTicks)
    {
        super.drawPanel(host, mouseX, mouseY, partialTicks);

        for (HoverInfo label : this.configComments)
        {
            if (label.isMouseOver(mouseX, mouseY))
            {
                this.drawHoveringText(label.getLines(), label.x, label.y + 30);
                break;
            }
        }
    }

    protected void addTextField(IConfig config, ConfigTextField field)
    {
        this.textFields.put(config, field);
    }

    protected ConfigTextField getTextFieldFor(IConfig config)
    {
        return this.textFields.get(config);
    }

    protected void addConfigComment(int x, int y, int width, int height, String comment)
    {
        HoverInfo info = new HoverInfo(x, y, width, height);
        info.addLines(comment);
        this.configComments.add(info);
    }

    protected int getMaxLabelWidth(IConfig[] configs)
    {
        int maxWidth = 0;

        for (IConfig config : configs)
        {
            maxWidth = Math.max(maxWidth, this.mc.fontRenderer.getStringWidth(config.getName()));
        }

        return maxWidth;
    }

    @Override
    public void keyPressed(ConfigPanelHost host, char keyChar, int keyCode)
    {
        if (keyCode == Keyboard.KEY_ESCAPE)
        {
            this.parent.setSelectedSubPanel(-1);
            return;
        }

        super.keyPressed(host, keyChar, keyCode);
    }

    /**
     * Returns true if some of the options in this panel were modified
     * @return
     */
    public boolean hasModifications()
    {
        return false;
    }

    public static class ConfigOptionListenerGeneric<T extends ConfigButtonBase> implements ConfigOptionListener<T>
    {
        private boolean dirty;

        @Override
        public void actionPerformed(T control)
        {
            this.dirty = true;
        }

        public boolean isDirty()
        {
            return this.dirty;
        }

        public void resetDirty()
        {
            this.dirty = false;
        }
    }

    public static abstract class ConfigButtonBase extends GuiButton
    {
        public ConfigButtonBase(int id, int x, int y, int width, int height)
        {
            super(id, x, y, width, height, "");
        }

        public abstract void onMouseClicked();

        public abstract void onMouseButtonClicked(int mouseButton);
    }

    public static class ConfigButtonBoolean extends ConfigButtonBase
    {
        private final IConfigBoolean config;

        public ConfigButtonBoolean(int id, int x, int y, int width, int height, IConfigBoolean config)
        {
            super(id, x, y, width, height);
            this.config = config;

            this.updateDisplayString();
        }

        @Override
        public void onMouseClicked()
        {
        }

        @Override
        public void onMouseButtonClicked(int mouseButton)
        {
            this.config.setBooleanValue(! this.config.getBooleanValue());
            this.updateDisplayString();
            this.playPressSound(Minecraft.getMinecraft().getSoundHandler());
        }

        private void updateDisplayString()
        {
            String valueStr = String.valueOf(this.config.getBooleanValue());

            if (this.config.getBooleanValue())
            {
                this.displayString = TextFormatting.DARK_GREEN + valueStr + TextFormatting.RESET;
            }
            else
            {
                this.displayString = TextFormatting.DARK_RED + valueStr + TextFormatting.RESET;
            }
        }
    }

    public static class ConfigButtonOptionList extends ConfigButtonBase
    {
        private final IConfigOptionList config;

        public ConfigButtonOptionList(int id, int x, int y, int width, int height, IConfigOptionList config)
        {
            super(id, x, y, width, height);
            this.config = config;

            this.updateDisplayString();
        }

        @Override
        public void onMouseClicked()
        {
        }

        @Override
        public void onMouseButtonClicked(int mouseButton)
        {
            this.config.setOptionListValue(this.config.getOptionListValue().cycle(mouseButton == 0));
            this.updateDisplayString();
            this.playPressSound(Minecraft.getMinecraft().getSoundHandler());
        }

        private void updateDisplayString()
        {
            this.displayString = String.valueOf(this.config.getOptionListValue().getDisplayName());
        }
    }

    public static class HoverInfo
    {
        protected final List<String> lines;
        protected int x;
        protected int y;
        protected int width;
        protected int height;

        public HoverInfo(int x, int y, int width, int height)
        {
            this.lines = new ArrayList<>();
            this.x = x;
            this.y = y;
            this.width = width;
            this.height = height;
        }

        public void addLines(String... lines)
        {
            for (String line : lines)
            {
                line = I18n.format(line);
                String[] split = line.split("\n");

                for (String str : split)
                {
                    this.lines.add(str);
                }
            }
        }

        public List<String> getLines()
        {
            return this.lines;
        }

        public boolean isMouseOver(int mouseX, int mouseY)
        {
            return mouseX >= this.x && mouseX <= (this.x + this.width) && mouseY >= this.y && mouseY <= (this.y + this.height);
        }
    }
}
