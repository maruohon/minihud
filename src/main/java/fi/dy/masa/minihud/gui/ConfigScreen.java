package fi.dy.masa.minihud.gui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.gui.config.BaseConfigTab;
import fi.dy.masa.malilib.config.option.ConfigOption;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.gui.config.BaseConfigScreen;
import fi.dy.masa.malilib.gui.config.ConfigTab;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;

public class ConfigScreen extends BaseConfigScreen
{
    private static final BaseConfigTab GENERIC           = new BaseConfigTab("minihud.gui.button.config_gui.generic", 120, false, Configs.Generic.OPTIONS);
    private static final BaseConfigTab COLORS            = new BaseConfigTab("minihud.gui.button.config_gui.colors", 100, false, Configs.Colors.OPTIONS);
    private static final BaseConfigTab INFO_TOGGLES      = new BaseConfigTab("minihud.gui.button.config_gui.info_toggles", 100, false, ConfigUtils.createConfigWrapperForType(ConfigType.BOOLEAN, ImmutableList.copyOf(InfoToggle.values())));
    private static final BaseConfigTab INFO_LINE_ORDER   = new BaseConfigTab("minihud.gui.button.config_gui.info_line_order", 100, false, ConfigUtils.createConfigWrapperForType(ConfigType.INTEGER, ImmutableList.copyOf(InfoToggle.values())));
    private static final BaseConfigTab INFO_HOTKEYS      = new BaseConfigTab("minihud.gui.button.config_gui.info_hotkeys", 204, true, ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(InfoToggle.values())));
    private static final BaseConfigTab STRUCTURES        = new BaseConfigTab("minihud.gui.button.config_gui.structures", 160, false, getStructureConfigs());
    private static final BaseConfigTab RENDERER_HOTKEYS  = new BaseConfigTab("minihud.gui.button.config_gui.renderer_hotkeys", 204, true, ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(RendererToggle.values())));
    public  static final BaseConfigTab SHAPES            = new BaseConfigTab("minihud.gui.button.config_gui.shapes", 204, false, Collections.emptyList(),
                                                                             (tab, gui) -> (button, mouseButton) -> { BaseScreen.openGui(new GuiShapeManager()); });

    public static final ImmutableList<ConfigTab> TABS = ImmutableList.of(
            GENERIC,
            COLORS,
            INFO_TOGGLES,
            INFO_LINE_ORDER,
            INFO_HOTKEYS,
            STRUCTURES,
            RENDERER_HOTKEYS,
            SHAPES
    );

    public static ConfigTab tab = INFO_TOGGLES;

    private static List<ConfigOption> getStructureConfigs()
    {
        List<ConfigOption> list = new ArrayList<>();
        list.addAll(StructureToggle.getToggleConfigs());
        list.addAll(StructureToggle.getHotkeys());
        list.addAll(StructureToggle.getColorConfigs());
        return list;
    }

    public ConfigScreen()
    {
        super(10, 50, Reference.MOD_ID, null, TABS, "minihud.gui.title.configs");
    }

    @Override
    public ConfigTab getCurrentTab()
    {
        return tab;
    }

    @Override
    public void setCurrentTab(ConfigTab tab)
    {
        ConfigScreen.tab = tab;
    }

    @Override
    public void initGui()
    {
        if (ConfigScreen.tab == SHAPES)
        {
            BaseScreen.openGui(new GuiShapeManager());
            return;
        }

        super.initGui();
    }
}
