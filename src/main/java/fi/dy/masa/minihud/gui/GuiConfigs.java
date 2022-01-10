package fi.dy.masa.minihud.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.config.IConfigBase;
import fi.dy.masa.malilib.config.options.ConfigTypeWrapper;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.GuiConfigsBase;
import fi.dy.masa.malilib.gui.button.ButtonBase;
import fi.dy.masa.malilib.gui.button.ButtonGeneric;
import fi.dy.masa.malilib.gui.button.IButtonActionListener;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;

public class GuiConfigs extends GuiConfigsBase
{
    public static ConfigGuiTab tab = ConfigGuiTab.INFO_LINES;

    public GuiConfigs()
    {
        super(10, 50, Reference.MOD_ID, null, "minihud.gui.title.configs");
    }

    @Override
    public void initGui()
    {
        if (GuiConfigs.tab == ConfigGuiTab.SHAPES)
        {
            GuiBase.openGui(new GuiShapeManager());
            return;
        }

        super.initGui();
        this.clearOptions();

        int x = 10;
        int y = 26;
        int rows = 1;

        for (ConfigGuiTab tab : ConfigGuiTab.values())
        {
            int width = this.getStringWidth(tab.getDisplayName()) + 10;

            if (x >= this.width - width - 10)
            {
                x = 10;
                y += 22;
                rows++;
            }

            x += this.createButton(x, y, width, tab);
        }

        if (rows > 1)
        {
            int scrollbarPosition = this.getListWidget().getScrollbar().getValue();
            this.setListPosition(this.getListX(), 50 + (rows - 1) * 22);
            this.reCreateListWidget();
            this.getListWidget().getScrollbar().setValue(scrollbarPosition);
            this.getListWidget().refreshEntries();
        }
    }

    private int createButton(int x, int y, int width, ConfigGuiTab tab)
    {
        ButtonGeneric button = new ButtonGeneric(x, y, width, 20, tab.getDisplayName());
        button.setEnabled(GuiConfigs.tab != tab);
        this.addButton(button, new ButtonListenerConfigTabs(tab, this));

        return button.getWidth() + 2;
    }

    @Override
    protected int getConfigWidth()
    {
        ConfigGuiTab tab = GuiConfigs.tab;

        if (tab == ConfigGuiTab.GENERIC ||
            tab == ConfigGuiTab.STRUCTURES)
        {
            return 200;
        }
        if (tab == ConfigGuiTab.RENDERERS)
        {
            return 220;
        }
        else if (tab == ConfigGuiTab.INFO_LINES)
        {
            return 120;
        }
        else if (tab == ConfigGuiTab.COLORS)
        {
            return 100;
        }

        return super.getConfigWidth();
    }

    @Override
    protected boolean useKeybindSearch()
    {
        return GuiConfigs.tab == ConfigGuiTab.INFO_LINES || GuiConfigs.tab == ConfigGuiTab.RENDERERS;
    }

    @Override
    public List<ConfigOptionWrapper> getConfigs()
    {
        ConfigGuiTab tab = GuiConfigs.tab;

        if (tab == ConfigGuiTab.GENERIC)
        {
            return ConfigOptionWrapper.createFor(Configs.Generic.OPTIONS);
        }
        else if (tab == ConfigGuiTab.COLORS)
        {
            return ConfigOptionWrapper.createFor(Configs.Colors.OPTIONS);
        }
        else if (tab == ConfigGuiTab.INFO_LINES)
        {
            List<IConfigBase> list = new ArrayList<>();
            list.addAll(ConfigUtils.createConfigWrapperForType(ConfigType.BOOLEAN, ImmutableList.copyOf(InfoToggle.values())));
            list.addAll(ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(InfoToggle.values())));
            list.addAll(ConfigUtils.createConfigWrapperForType(ConfigType.INTEGER, ImmutableList.copyOf(InfoToggle.values())));
            return ConfigOptionWrapper.createFor(list);
        }
        else if (tab == ConfigGuiTab.STRUCTURES)
        {
            List<IConfigBase> list = new ArrayList<>();
            list.add(new ConfigTypeWrapper(ConfigType.BOOLEAN, RendererToggle.OVERLAY_STRUCTURE_MAIN_TOGGLE));
            list.addAll(StructureToggle.getToggleConfigs());
            list.addAll(StructureToggle.getHotkeys());
            list.addAll(StructureToggle.getColorConfigs());
            return ConfigOptionWrapper.createFor(list);
        }
        else if (tab == ConfigGuiTab.RENDERERS)
        {
            List<IConfigBase> list = new ArrayList<>();
            list.addAll(ConfigUtils.createConfigWrapperForType(ConfigType.BOOLEAN, ImmutableList.copyOf(RendererToggle.values())));
            list.addAll(ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(RendererToggle.values())));
            return ConfigOptionWrapper.createFor(list);
        }

        return Collections.emptyList();
    }

    private static class ButtonListenerConfigTabs implements IButtonActionListener
    {
        private final GuiConfigs parent;
        private final ConfigGuiTab tab;

        public ButtonListenerConfigTabs(ConfigGuiTab tab, GuiConfigs parent)
        {
            this.tab = tab;
            this.parent = parent;
        }

        @Override
        public void actionPerformedWithButton(ButtonBase button, int mouseButton)
        {
            GuiConfigs.tab = this.tab;

            if (this.tab == ConfigGuiTab.SHAPES)
            {
                GuiBase.openGui(new GuiShapeManager());
            }
            else
            {
                this.parent.reCreateListWidget(); // apply the new config width
                this.parent.getListWidget().resetScrollbarPosition();
                this.parent.initGui();
            }
        }
    }

    public enum ConfigGuiTab
    {
        GENERIC             ("minihud.gui.button.config_gui.generic"),
        COLORS              ("minihud.gui.button.config_gui.colors"),
        INFO_LINES          ("minihud.gui.button.config_gui.info_lines"),
        STRUCTURES          ("minihud.gui.button.config_gui.structures"),
        RENDERERS           ("minihud.gui.button.config_gui.renderers"),
        SHAPES              ("minihud.gui.button.config_gui.shapes");

        private final String translationKey;

        ConfigGuiTab(String translationKey)
        {
            this.translationKey = translationKey;
        }

        public String getDisplayName()
        {
            return StringUtils.translate(this.translationKey);
        }
    }
}
