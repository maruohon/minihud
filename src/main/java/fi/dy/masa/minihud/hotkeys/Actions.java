package fi.dy.masa.minihud.hotkeys;

import fi.dy.masa.malilib.action.NamedAction;
import fi.dy.masa.malilib.config.option.HotkeyedBooleanConfig;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.listener.EventListener;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoLine;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.gui.ConfigScreen;
import fi.dy.masa.minihud.gui.GuiShapeEditor;
import fi.dy.masa.minihud.gui.GuiShapeManager;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;

public class Actions
{
    public static final NamedAction OPEN_CONFIG_SCREEN              = register("openConfigScreen",      ConfigScreen::open);
    public static final NamedAction OPEN_SHAPE_EDITOR               = register("openShapeEditor",       () -> {
        ShapeBase shape = ShapeManager.INSTANCE.getSelectedShape();
        BaseScreen.openScreen(shape != null ? new GuiShapeEditor(shape) : new GuiShapeManager());
    });
    public static final NamedAction SET_DISTANCE_REFERENCE_POINT    = register("setDistanceReferencePoint", DataStorage.getInstance()::setDistanceReferencePoint);

    public static void init()
    {
        register("toggleInfoLinesRendering",  Configs.Generic.INFO_LINES_RENDERING_TOGGLE);
        register("toggleOverlaysRendering",   Configs.Generic.OVERLAYS_RENDERING_TOGGLE);

        for (InfoLine line : InfoLine.VALUES)
        {
            NamedAction.registerToggle(Reference.MOD_INFO, line.getName(), line.getBooleanConfig());
        }

        for (RendererToggle toggle : RendererToggle.VALUES)
        {
            NamedAction.registerToggle(Reference.MOD_INFO, toggle.getName(), toggle.getBooleanConfig());
        }

        for (StructureToggle toggle : StructureToggle.VALUES)
        {
            NamedAction.registerToggle(Reference.MOD_INFO, toggle.getName(), toggle.getBooleanConfig());
        }
    }

    private static NamedAction register(String name, EventListener action)
    {
        return NamedAction.register(Reference.MOD_INFO, name, action);
    }

    private static NamedAction register(String name, HotkeyedBooleanConfig config)
    {
        return NamedAction.registerToggleKey(Reference.MOD_INFO, name, config);
    }
}
