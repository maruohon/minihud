package fi.dy.masa.minihud.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.config.IConfigBase;
import fi.dy.masa.malilib.gui.GuiConfigsBase;
import fi.dy.masa.malilib.gui.button.ButtonGeneric;
import fi.dy.masa.malilib.gui.button.IButtonActionListener;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;
import net.minecraft.client.resources.I18n;

public class GuiConfigs extends GuiConfigsBase
{
    private static ConfigGuiTab tab = ConfigGuiTab.INFO_TOGGLES;
    private int id;

    public GuiConfigs()
    {
        super(10, 50, Reference.MOD_ID, null);

        this.title = I18n.format("minihud.gui.title.configs");
    }

    @Override
    public void initGui()
    {
        super.initGui();
        this.clearOptions();

        this.id = 0;
        int x = 10;
        int y = 26;

        for (ConfigGuiTab tab : ConfigGuiTab.values())
        {
            x += this.createButton(x, y, -1, tab) + 4;
        }
    }

    private int createButton(int x, int y, int width, ConfigGuiTab tab)
    {
        ButtonListener listener = new ButtonListener(tab, this);
        boolean enabled = GuiConfigs.tab != tab;
        String label = tab.getDisplayName();

        if (width < 0)
        {
            width = this.mc.fontRenderer.getStringWidth(label) + 10;
        }

        ButtonGeneric button = new ButtonGeneric(this.id++, x, y, width, 20, label);
        button.enabled = enabled;
        this.addButton(button, listener);

        return width;
    }

    @Override
    protected int getConfigWidth()
    {
        ConfigGuiTab tab = GuiConfigs.tab;

        if (tab == ConfigGuiTab.GENERIC || tab == ConfigGuiTab.STRUCTURES)
        {
            return 200;
        }
        else if (tab == ConfigGuiTab.COLORS ||
                 tab == ConfigGuiTab.INFO_LINE_ORDER ||
                 tab == ConfigGuiTab.INFO_TOGGLES)
        {
            return 100;
        }

        return super.getConfigWidth();
    }

    @Override
    protected boolean useKeybindSearch()
    {
        return GuiConfigs.tab == ConfigGuiTab.INFO_HOTKEYS || GuiConfigs.tab == ConfigGuiTab.RENDERER_HOTKEYS;
    }

    @Override
    public List<ConfigOptionWrapper> getConfigs()
    {
        List<? extends IConfigBase> configs;
        ConfigGuiTab tab = GuiConfigs.tab;

        if (tab == ConfigGuiTab.GENERIC)
        {
            configs = Configs.Generic.OPTIONS;
        }
        else if (tab == ConfigGuiTab.COLORS)
        {
            configs = Configs.Colors.OPTIONS;
        }
        else if (tab == ConfigGuiTab.INFO_TOGGLES)
        {
            configs = ConfigUtils.createConfigWrapperForType(ConfigType.BOOLEAN, ImmutableList.copyOf(InfoToggle.values()));
        }
        else if (tab == ConfigGuiTab.INFO_LINE_ORDER)
        {
            configs = ConfigUtils.createConfigWrapperForType(ConfigType.INTEGER, ImmutableList.copyOf(InfoToggle.values()));
        }
        else if (tab == ConfigGuiTab.INFO_HOTKEYS)
        {
            configs = ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(InfoToggle.values()));
        }
        else if (tab == ConfigGuiTab.STRUCTURES)
        {
            List<IConfigBase> list = new ArrayList<>();
            list.addAll(StructureToggle.getToggleConfigs());
            list.addAll(StructureToggle.getHotkeys());
            list.addAll(StructureToggle.getColorConfigs());
            return ConfigOptionWrapper.createFor(list);
        }
        else if (tab == ConfigGuiTab.RENDERER_HOTKEYS)
        {
            configs = ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(RendererToggle.values()));
        }
        else
        {
            return Collections.emptyList();
        }

        return ConfigOptionWrapper.createFor(configs);
    }

    private static class ButtonListener implements IButtonActionListener<ButtonGeneric>
    {
        private final GuiConfigs parent;
        private final ConfigGuiTab tab;

        public ButtonListener(ConfigGuiTab tab, GuiConfigs parent)
        {
            this.tab = tab;
            this.parent = parent;
        }

        @Override
        public void actionPerformed(ButtonGeneric control)
        {
        }

        @Override
        public void actionPerformedWithButton(ButtonGeneric control, int mouseButton)
        {
            GuiConfigs.tab = this.tab;

            this.parent.reCreateListWidget(); // apply the new config width
            this.parent.getListWidget().resetScrollbarPosition();
            this.parent.initGui();
        }
    }

    public enum ConfigGuiTab
    {
        GENERIC             ("minihud.gui.button.config_gui.generic"),
        COLORS              ("minihud.gui.button.config_gui.colors"),
        INFO_TOGGLES        ("minihud.gui.button.config_gui.info_toggles"),
        INFO_LINE_ORDER     ("minihud.gui.button.config_gui.info_line_order"),
        INFO_HOTKEYS        ("minihud.gui.button.config_gui.info_hotkeys"),
        STRUCTURES          ("minihud.gui.button.config_gui.structures"),
        RENDERER_HOTKEYS    ("minihud.gui.button.config_gui.renderer_hotkeys");

        private final String translationKey;

        private ConfigGuiTab(String translationKey)
        {
            this.translationKey = translationKey;
        }

        public String getDisplayName()
        {
            return I18n.format(this.translationKey);
        }
    }
}
