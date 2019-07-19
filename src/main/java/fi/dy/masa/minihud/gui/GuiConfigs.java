package fi.dy.masa.minihud.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.config.gui.ConfigGuiTabBase;
import fi.dy.masa.malilib.config.options.IConfigBase;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.gui.GuiConfigsBase;
import fi.dy.masa.malilib.gui.interfaces.IConfigGuiTab;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;

public class GuiConfigs extends GuiConfigsBase
{
    private static final ConfigGuiTabBase GENERIC           = new ConfigGuiTabBase("minihud.gui.button.config_gui.generic",          120, false, Configs.Generic.OPTIONS);
    private static final ConfigGuiTabBase COLORS            = new ConfigGuiTabBase("minihud.gui.button.config_gui.colors",           100, false, Configs.Colors.OPTIONS);
    private static final ConfigGuiTabBase INFO_TOGGLES      = new ConfigGuiTabBase("minihud.gui.button.config_gui.info_toggles",     100, false, ConfigUtils.createConfigWrapperForType(ConfigType.BOOLEAN, ImmutableList.copyOf(InfoToggle.values())));
    private static final ConfigGuiTabBase INFO_LINE_ORDER   = new ConfigGuiTabBase("minihud.gui.button.config_gui.info_line_order",  100, false, ConfigUtils.createConfigWrapperForType(ConfigType.INTEGER, ImmutableList.copyOf(InfoToggle.values())));
    private static final ConfigGuiTabBase INFO_HOTKEYS      = new ConfigGuiTabBase("minihud.gui.button.config_gui.info_hotkeys",     204, true,  ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(InfoToggle.values())));
    private static final ConfigGuiTabBase STRUCTURES        = new ConfigGuiTabBase("minihud.gui.button.config_gui.structures",       160, false, getStructureConfigs());
    private static final ConfigGuiTabBase RENDERER_HOTKEYS  = new ConfigGuiTabBase("minihud.gui.button.config_gui.renderer_hotkeys", 204, true,  ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(RendererToggle.values())));
    public  static final ConfigGuiTabBase SHAPES            = new ConfigGuiTabBase("minihud.gui.button.config_gui.shapes",           204, false, Collections.emptyList(),
            (tab, gui) -> (button, mouseButton) -> { GuiBase.openGui(new GuiShapeManager()); });

    public static final ImmutableList<IConfigGuiTab> TABS = ImmutableList.of(
            GENERIC,
            COLORS,
            INFO_TOGGLES,
            INFO_LINE_ORDER,
            INFO_HOTKEYS,
            STRUCTURES,
            RENDERER_HOTKEYS,
            SHAPES
    );

    public static IConfigGuiTab tab = INFO_TOGGLES;

    private static List<IConfigBase> getStructureConfigs()
    {
        List<IConfigBase> list = new ArrayList<>();
        list.addAll(StructureToggle.getToggleConfigs());
        list.addAll(StructureToggle.getHotkeys());
        list.addAll(StructureToggle.getColorConfigs());
        return list;
    }

    public GuiConfigs()
    {
        super(10, 50, Reference.MOD_ID, null, TABS, "minihud.gui.title.configs");
    }

    @Override
    public IConfigGuiTab getCurrentTab()
    {
        return tab;
    }

    @Override
    public void setCurrentTab(IConfigGuiTab tab)
    {
        GuiConfigs.tab = tab;
    }

    @Override
    public void initGui()
    {
        if (GuiConfigs.tab == SHAPES)
        {
            GuiBase.openGui(new GuiShapeManager());
            return;
        }

        super.initGui();
    }
}
