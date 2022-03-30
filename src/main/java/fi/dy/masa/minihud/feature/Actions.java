package fi.dy.masa.minihud.feature;

import fi.dy.masa.malilib.action.ActionUtils;
import fi.dy.masa.malilib.action.NamedAction;
import fi.dy.masa.malilib.listener.EventListener;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoLine;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.config.StructureToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.gui.ConfigScreen;
import fi.dy.masa.minihud.gui.GuiShapeEditor;
import fi.dy.masa.minihud.gui.ShapeManagerScreen;

public class Actions
{
    public static final NamedAction OPEN_CONFIG_SCREEN              = register("openConfigScreen", ConfigScreen::open);
    public static final NamedAction OPEN_SHAPE_EDITOR               = register("openShapeEditor", GuiShapeEditor::openShapeEditor);
    public static final NamedAction OPEN_SHAPE_MANAGER              = register("openShapeManager", ShapeManagerScreen::openShapeManager);
    public static final NamedAction SET_DISTANCE_REFERENCE_POINT    = register("setDistanceReferencePoint", DataStorage.getInstance()::setDistanceReferencePoint);

    public static void init()
    {
        for (InfoLine line : InfoLine.VALUES)
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
