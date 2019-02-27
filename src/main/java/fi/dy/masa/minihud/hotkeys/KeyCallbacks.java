package fi.dy.masa.minihud.hotkeys;

import fi.dy.masa.malilib.hotkeys.IHotkeyCallback;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.malilib.hotkeys.KeyCallbackToggleBoolean;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.gui.GuiConfigs;
import fi.dy.masa.minihud.gui.GuiConfigs.ConfigGuiTab;
import fi.dy.masa.minihud.gui.GuiShapeEditor;
import fi.dy.masa.minihud.gui.GuiShapeManager;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.Minecraft;

public class KeyCallbacks
{
    public static void init()
    {
        Callbacks callback = new Callbacks();
        Configs.Generic.SET_DISTANCE_REFERENCE_POINT.getKeybind().setCallback(callback);
        Configs.Generic.OPEN_CONFIG_GUI.getKeybind().setCallback(callback);
        Configs.Generic.SHAPE_EDITOR.getKeybind().setCallback(callback);
        Configs.Generic.TOGGLE_KEY.getKeybind().setCallback(new KeyCallbackToggleBoolean(Configs.Generic.ENABLED));
    }

    public static class Callbacks implements IHotkeyCallback
    {
        @Override
        public boolean onKeyAction(KeyAction action, IKeybind key)
        {
            Minecraft mc = Minecraft.getMinecraft();

            if (mc.player == null)
            {
                return false;
            }

            if (key == Configs.Generic.OPEN_CONFIG_GUI.getKeybind())
            {
                mc.displayGuiScreen(new GuiConfigs());
            }
            else if (key == Configs.Generic.SET_DISTANCE_REFERENCE_POINT.getKeybind())
            {
                DataStorage.getInstance().setDistanceReferencePoint(mc.player.getPositionVector());
            }
            else if (key == Configs.Generic.SHAPE_EDITOR.getKeybind())
            {
                ShapeBase shape = ShapeManager.INSTANCE.getSelectedShape();

                if (shape != null)
                {
                    mc.displayGuiScreen(new GuiShapeEditor(shape));
                }
                else
                {
                    GuiConfigs.tab = ConfigGuiTab.SHAPES;
                    mc.displayGuiScreen(new GuiShapeManager());
                }
            }

            return true;
        }
    }
}
