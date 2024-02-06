package minihud;

import malilib.action.NamedAction;
import malilib.action.util.ActionUtils;
import malilib.listener.EventListener;
import minihud.config.Configs;
import minihud.config.InfoLineToggle;
import minihud.config.RendererToggle;
import minihud.config.StructureToggle;
import minihud.data.DataStorage;
import minihud.gui.ConfigScreen;
import minihud.gui.GuiShapeEditor;
import minihud.gui.ShapeManagerScreen;

public class MiniHudActions
{
    public static final NamedAction OPEN_CONFIG_SCREEN              = register("openConfigScreen", ConfigScreen::open);
    public static final NamedAction OPEN_SHAPE_EDITOR               = register("openShapeEditor", GuiShapeEditor::openShapeEditor);
    public static final NamedAction OPEN_SHAPE_MANAGER              = register("openShapeManager", ShapeManagerScreen::openShapeManager);
    public static final NamedAction SET_DISTANCE_REFERENCE_POINT    = register("setDistanceReferencePoint", DataStorage.getInstance()::setDistanceReferencePoint);

    public static void init()
    {
        for (InfoLineToggle line : InfoLineToggle.VALUES)
        {
            ActionUtils.registerBooleanConfigActions(Reference.MOD_INFO, line.getBooleanConfig(), line.getKeyBind());
        }

        for (RendererToggle toggle : RendererToggle.VALUES)
        {
            ActionUtils.registerBooleanConfigActions(Reference.MOD_INFO, toggle.getBooleanConfig(), toggle.getKeyBind());
        }

        for (StructureToggle toggle : StructureToggle.VALUES)
        {
            ActionUtils.registerBooleanConfigActions(Reference.MOD_INFO, toggle.getBooleanConfig(), toggle.getKeyBind());
        }

        ActionUtils.registerBooleanConfigActions(Configs.Generic.OPTIONS);
    }

    private static NamedAction register(String name, EventListener action)
    {
        return ActionUtils.register(Reference.MOD_INFO, name, action);
    }
}
