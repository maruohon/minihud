package fi.dy.masa.minihud.gui;

import java.util.Collections;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.option.ConfigInfo;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.gui.config.BaseConfigScreen;
import fi.dy.masa.malilib.gui.config.BaseConfigTab;
import fi.dy.masa.malilib.gui.config.ConfigTab;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoLine;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;

public class ConfigScreen
{
    private static final BaseConfigTab GENERIC              = new BaseConfigTab("minihud.gui.button.config_gui.generic",            Reference.MOD_NAME, 160, Configs.Generic.OPTIONS);
    private static final BaseConfigTab COLORS               = new BaseConfigTab("minihud.gui.button.config_gui.colors",             Reference.MOD_NAME, 100, Configs.Colors.OPTIONS);
    private static final BaseConfigTab INFO_LINES           = new BaseConfigTab("minihud.gui.button.config_gui.info_lines",         Reference.MOD_NAME, 200, InfoLine.VALUES);
    private static final BaseConfigTab OVERLAY_RENDERERS    = new BaseConfigTab("minihud.gui.button.config_gui.overlay_renderers",  Reference.MOD_NAME, 200, getRendererOptions());
    private static final BaseConfigTab STRUCTURES           = new BaseConfigTab("minihud.gui.button.config_gui.structures",         Reference.MOD_NAME, 200, getStructureOptions());
    public  static final BaseConfigTab SHAPES               = new BaseConfigTab("minihud.gui.button.config_gui.shapes",             Reference.MOD_NAME, 200, Collections.emptyList(),
                                                                                (tab, gui) -> (button, mouseButton) -> openShapeEditor(gui));

    public static final ImmutableList<ConfigTab> TABS = ImmutableList.of(
            GENERIC,
            COLORS,
            INFO_LINES,
            OVERLAY_RENDERERS,
            STRUCTURES,
            SHAPES
    );

    public static BaseConfigScreen create()
    {
        return new BaseConfigScreen(10, 50, Reference.MOD_ID, null, TABS, INFO_LINES, "minihud.gui.title.configs");
    }

    public static BaseConfigScreen createOnTab(ConfigTab tab)
    {
        BaseConfigScreen screen = new BaseConfigScreen(10, 50, Reference.MOD_ID, null, TABS, INFO_LINES, "minihud.gui.title.configs");
        screen.setCurrentTab(tab);
        return screen;
    }

    public static ImmutableList<ConfigTab> getConfigTabs()
    {
        return TABS;
    }

    private static ImmutableList<ConfigInfo> getRendererOptions()
    {
        ImmutableList.Builder<ConfigInfo> builder = ImmutableList.builder();

        builder.add(Configs.Generic.MAIN_RENDERING_TOGGLE);
        builder.addAll(RendererToggle.VALUES);

        return builder.build();
    }

    private static ImmutableList<ConfigInfo> getStructureOptions()
    {
        ImmutableList.Builder<ConfigInfo> builder = ImmutableList.builder();

        builder.add(Configs.Generic.MAIN_RENDERING_TOGGLE);
        builder.add(RendererToggle.OVERLAY_STRUCTURE_MAIN_TOGGLE);
        builder.addAll(StructureToggle.VALUES);

        return builder.build();
    }

    private static boolean openShapeEditor(BaseConfigScreen screen)
    {
        screen.setCurrentTab(SHAPES);
        BaseScreen.openGui(new GuiShapeManager());
        return true;
    }
}
