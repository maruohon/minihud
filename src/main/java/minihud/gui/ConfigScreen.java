package minihud.gui;

import java.util.ArrayList;
import com.google.common.collect.ImmutableList;

import malilib.config.group.ExpandableConfigGroup;
import malilib.config.option.ConfigInfo;
import malilib.config.option.GenericButtonConfig;
import malilib.config.util.ConfigUtils;
import malilib.gui.BaseScreen;
import malilib.gui.config.BaseConfigScreen;
import malilib.gui.config.BaseConfigTab;
import malilib.gui.config.ConfigTab;
import malilib.gui.tab.BaseScreenTab;
import malilib.gui.tab.ScreenTab;
import malilib.overlay.widget.StringListRendererWidget;
import malilib.util.ListUtils;
import malilib.util.data.ModInfo;
import minihud.Reference;
import minihud.config.Configs;
import minihud.config.InfoLineToggle;
import minihud.config.RendererToggle;
import minihud.config.StructureToggle;
import minihud.event.RenderHandler;

public class ConfigScreen
{
    public static final ModInfo MOD_INFO = Reference.MOD_INFO;

    private static final BaseConfigTab GENERIC              = new BaseConfigTab(MOD_INFO, "generic",    160, getGenericOptions(),       ConfigScreen::create);
    private static final BaseConfigTab COLORS               = new BaseConfigTab(MOD_INFO, "colors", 100, Configs.Colors.OPTIONS, ConfigScreen::create);
    private static final BaseConfigTab HOTKEYS              = new BaseConfigTab(MOD_INFO, "hotkeys",    200, getHotkeys(),              ConfigScreen::create);
    private static final BaseConfigTab INFO_LINES           = new BaseConfigTab(MOD_INFO, "info_lines", 200, getInfoLinesOptions(),     ConfigScreen::create);
    private static final BaseConfigTab OVERLAY_RENDERERS    = new BaseConfigTab(MOD_INFO, "renderers",  200, getRendererOptions(),      ConfigScreen::create);
    private static final BaseConfigTab STRUCTURES           = new BaseConfigTab(MOD_INFO, "structures", 200, getStructureOptions(),     ConfigScreen::create);
    public  static final BaseScreenTab SHAPES               = new BaseScreenTab(MOD_INFO, "shapes", ShapeManagerScreen::screenValidator, ShapeManagerScreen::openShapeManagerScreen);

    public static final ImmutableList<ConfigTab> CONFIG_TABS = ImmutableList.of(
            GENERIC,
            COLORS,
            HOTKEYS,
            INFO_LINES,
            OVERLAY_RENDERERS,
            STRUCTURES
    );

    public static final ImmutableList<ScreenTab> ALL_TABS = ImmutableList.of(
            GENERIC,
            COLORS,
            HOTKEYS,
            INFO_LINES,
            OVERLAY_RENDERERS,
            STRUCTURES,
            SHAPES
    );

    public static void open()
    {
        BaseScreen.openScreen(create());
    }

    public static BaseConfigScreen create()
    {
        // The parent screen should not be set here, to prevent infinite recursion via
        // the call to the parent's setWorldAndResolution -> initScreen -> switch tab -> etc.
        return BaseConfigScreen.withExtensionModTabs(MOD_INFO, ALL_TABS, INFO_LINES,
                                                     "minihud.title.screen.configs", Reference.MOD_VERSION);
    }

    public static ImmutableList<ConfigTab> getConfigTabs()
    {
        return CONFIG_TABS;
    }

    private static ImmutableList<ConfigInfo> getGenericOptions()
    {
        ArrayList<ConfigInfo> genericOptions = new ArrayList<>(Configs.Generic.OPTIONS);
        ArrayList<ConfigInfo> lightOptions = new ArrayList<>();

        ListUtils.extractEntriesToSecondList(genericOptions, lightOptions, (c) -> c.getName().startsWith("lightLevel"), true);

        genericOptions.add(new GenericButtonConfig("minihud_generic_info_lines_hud_settings",
                                                   "minihud.button.config.open_info_hud_settings",
                                                   ConfigScreen::openHudSettingScreen,
                                                   "minihud.config.name.info_lines_hud_settings",
                                                   "minihud.config.comment.info_lines_hud_settings"));
        genericOptions.add(new ExpandableConfigGroup(MOD_INFO, "light_level", lightOptions));
        ConfigUtils.sortConfigsByDisplayName(genericOptions);

        return ImmutableList.copyOf(genericOptions);
    }

    private static ImmutableList<ConfigInfo> getHotkeys()
    {
        ArrayList<ConfigInfo> list = new ArrayList<>(Configs.Hotkeys.HOTKEYS);

        ConfigUtils.sortConfigsByDisplayName(list);
        list.add(0, Configs.Generic.INFO_LINES_RENDERING_TOGGLE);
        list.add(1, Configs.Generic.OVERLAYS_RENDERING_TOGGLE);

        return ImmutableList.copyOf(list);
    }

    private static ImmutableList<ConfigInfo> getInfoLinesOptions()
    {
        ImmutableList.Builder<ConfigInfo> builder = ImmutableList.builder();

        builder.add(Configs.Generic.INFO_LINES_RENDERING_TOGGLE);
        builder.addAll(InfoLineToggle.VALUES);

        return builder.build();
    }

    private static ImmutableList<ConfigInfo> getRendererOptions()
    {
        ImmutableList.Builder<ConfigInfo> builder = ImmutableList.builder();

        builder.add(Configs.Generic.OVERLAYS_RENDERING_TOGGLE);
        builder.addAll(RendererToggle.VALUES);

        return builder.build();
    }

    private static ImmutableList<ConfigInfo> getStructureOptions()
    {
        ImmutableList.Builder<ConfigInfo> builder = ImmutableList.builder();

        builder.add(Configs.Generic.OVERLAYS_RENDERING_TOGGLE);
        builder.add(RendererToggle.STRUCTURE_BOUNDING_BOXES);
        builder.addAll(StructureToggle.VALUES);

        return builder.build();
    }

    private static boolean openHudSettingScreen(int mouseButton)
    {
        StringListRendererWidget widget = RenderHandler.INSTANCE.getStringListRenderer();

        if (widget != null)
        {
            widget.openEditScreen();
        }

        return true;
    }
}
