package fi.dy.masa.minihud.hotkeys;

import net.minecraft.client.MinecraftClient;
import net.minecraft.entity.Entity;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.hotkeys.IHotkeyCallback;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.malilib.hotkeys.KeyCallbackAdjustable;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.gui.GuiConfigs;
import fi.dy.masa.minihud.gui.GuiConfigs.ConfigGuiTab;
import fi.dy.masa.minihud.gui.GuiShapeEditor;
import fi.dy.masa.minihud.gui.GuiShapeManager;
import fi.dy.masa.minihud.renderer.OverlayRendererBeaconRange;
import fi.dy.masa.minihud.renderer.OverlayRendererBiomeBorders;
import fi.dy.masa.minihud.renderer.OverlayRendererConduitRange;
import fi.dy.masa.minihud.renderer.OverlayRendererLightLevel;
import fi.dy.masa.minihud.renderer.OverlayRendererStructures;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.DebugInfoUtils;

public class KeyCallbacks
{
    public static void init()
    {
        Callbacks callback = new Callbacks();

        Configs.Generic.SET_DISTANCE_REFERENCE_POINT.getKeybind().setCallback(callback);
        Configs.Generic.OPEN_CONFIG_GUI.getKeybind().setCallback(callback);
        Configs.Generic.SHAPE_EDITOR.getKeybind().setCallback(callback);

        Configs.Colors.BEACON_RANGE_LVL1_OVERLAY_COLOR.setValueChangeCallback((config) -> updateBeaconOverlay());
        Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.setValueChangeCallback((config) -> updateBeaconOverlay());
        Configs.Colors.BEACON_RANGE_LVL3_OVERLAY_COLOR.setValueChangeCallback((config) -> updateBeaconOverlay());
        Configs.Colors.BEACON_RANGE_LVL4_OVERLAY_COLOR.setValueChangeCallback((config) -> updateBeaconOverlay());
        Configs.Colors.CONDUIT_RANGE_OVERLAY_COLOR.setValueChangeCallback((config) -> updateConduitOverlay());

        Configs.Generic.LIGHT_LEVEL_RANGE.setValueChangeCallback((config) -> OverlayRendererLightLevel.setNeedsUpdate());
        Configs.Generic.LIGHT_LEVEL_RENDER_THROUGH.setValueChangeCallback((config) -> OverlayRendererLightLevel.INSTANCE.setRenderThrough(config.getBooleanValue()));
        Configs.Generic.STRUCTURES_RENDER_THROUGH.setValueChangeCallback((config) -> OverlayRendererStructures.INSTANCE.setRenderThrough(config.getBooleanValue()));

        RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeybind().setCallback(new KeyCallbackAdjustable(RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY, new KeyCallbackToggleRenderer(RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY)));

        RendererToggle.OVERLAY_BEACON_RANGE.setValueChangeCallback((config) -> updateBeaconOverlay());
        RendererToggle.OVERLAY_CONDUIT_RANGE.setValueChangeCallback((config) -> updateConduitOverlay());
        RendererToggle.OVERLAY_BIOME_BORDER.setValueChangeCallback((config) -> OverlayRendererBiomeBorders.INSTANCE.setNeedsUpdate());
        RendererToggle.DEBUG_CHUNK_BORDER.setValueChangeCallback(DebugInfoUtils::toggleDebugRenderer);
        RendererToggle.DEBUG_CHUNK_INFO.setValueChangeCallback(DebugInfoUtils::toggleDebugRenderer);
        RendererToggle.DEBUG_CHUNK_OCCLUSION.setValueChangeCallback(DebugInfoUtils::toggleDebugRenderer);
        RendererToggle.DEBUG_NEIGHBOR_UPDATES.setValueChangeCallback(DebugInfoUtils::toggleDebugRenderer);
        RendererToggle.DEBUG_PATH_FINDING.setValueChangeCallback(DebugInfoUtils::toggleDebugRenderer);
    }

    private static void updateBeaconOverlay()
    {
        OverlayRendererBeaconRange.INSTANCE.setNeedsUpdate();
    }

    private static void updateConduitOverlay()
    {
        OverlayRendererConduitRange.INSTANCE.setNeedsUpdate();
    }

    public static class Callbacks implements IHotkeyCallback
    {
        @Override
        public boolean onKeyAction(KeyAction action, IKeybind key)
        {
            MinecraftClient mc = MinecraftClient.getInstance();

            if (mc.player == null)
            {
                return false;
            }

            if (key == Configs.Generic.OPEN_CONFIG_GUI.getKeybind())
            {
                GuiBase.openGui(new GuiConfigs());
            }
            else if (key == Configs.Generic.SET_DISTANCE_REFERENCE_POINT.getKeybind())
            {
                Entity entity = mc.getCameraEntity() != null ? mc.getCameraEntity() : mc.player;
                DataStorage.getInstance().setDistanceReferencePoint(entity.getPos());
            }
            else if (key == Configs.Generic.SHAPE_EDITOR.getKeybind())
            {
                ShapeBase shape = ShapeManager.INSTANCE.getSelectedShape();

                if (shape != null)
                {
                    GuiBase.openGui(new GuiShapeEditor(shape));
                }
                else
                {
                    GuiConfigs.tab = ConfigGuiTab.SHAPES;
                    GuiBase.openGui(new GuiShapeManager());
                }
            }

            return true;
        }
    }
}
