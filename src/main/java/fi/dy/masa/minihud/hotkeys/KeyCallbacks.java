package fi.dy.masa.minihud.hotkeys;

import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.hotkeys.IHotkeyCallback;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.malilib.hotkeys.KeyCallbackAdjustable;
import fi.dy.masa.malilib.hotkeys.KeyCallbackToggleBoolean;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoToggle;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.gui.GuiConfigs;
import fi.dy.masa.minihud.gui.GuiShapeEditor;
import fi.dy.masa.minihud.gui.GuiShapeManager;
import fi.dy.masa.minihud.network.CarpetPubsubPacketHandler;
import fi.dy.masa.minihud.renderer.OverlayRendererBeaconRange;
import fi.dy.masa.minihud.renderer.OverlayRendererLightLevel;
import fi.dy.masa.minihud.renderer.OverlayRendererStructures;
import fi.dy.masa.minihud.renderer.shapes.ShapeBase;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;

public class KeyCallbacks
{
    public static void init()
    {
        Callbacks callback = new Callbacks();

        Configs.Generic.OPEN_CONFIG_GUI.getKeybind().setCallback(callback);
        Configs.Generic.SET_DISTANCE_REFERENCE_POINT.getKeybind().setCallback(callback);
        Configs.Generic.SHAPE_EDITOR.getKeybind().setCallback(callback);
        Configs.Generic.TOGGLE_KEY.getKeybind().setCallback(new KeyCallbackToggleBoolean(Configs.Generic.ENABLED));

        Configs.Colors.BEACON_RANGE_LVL1_OVERLAY_COLOR.setValueChangeCallback((config) -> OverlayRendererBeaconRange.setNeedsUpdate());
        Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.setValueChangeCallback((config) -> OverlayRendererBeaconRange.setNeedsUpdate());
        Configs.Colors.BEACON_RANGE_LVL3_OVERLAY_COLOR.setValueChangeCallback((config) -> OverlayRendererBeaconRange.setNeedsUpdate());
        Configs.Colors.BEACON_RANGE_LVL4_OVERLAY_COLOR.setValueChangeCallback((config) -> OverlayRendererBeaconRange.setNeedsUpdate());
        Configs.Generic.LIGHT_LEVEL_RANGE.setValueChangeCallback((config) -> { OverlayRendererLightLevel.setNeedsUpdate(); });
        Configs.Generic.STRUCTURES_RENDER_THROUGH.setValueChangeCallback((config) -> { OverlayRendererStructures.instance.setRenderThrough(config.getBooleanValue()); });
        Configs.Generic.WOOL_COUNTER_ENABLE_ALL.setValueChangeCallback((config) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());
        Configs.Generic.WOOL_COUNTER_TYPES.setValueChangeCallback((config) -> {
            DataStorage.getInstance().getWoolCounters().updateEnabledCounters(config.getStringValue());
            CarpetPubsubPacketHandler.updatePubsubSubscriptions();
        });

        InfoToggle.CARPET_WOOL_COUNTERS.setValueChangeCallback((config) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());
        InfoToggle.CHUNK_UNLOAD_ORDER.getKeybind().setCallback(new KeyCallbackAdjustable(InfoToggle.CHUNK_UNLOAD_ORDER, null));
        InfoToggle.CHUNK_UNLOAD_ORDER.setValueChangeCallback((config) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());
        InfoToggle.MOB_CAPS.setValueChangeCallback((config) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());
        InfoToggle.SERVER_TPS.setValueChangeCallback((config) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());

        RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getKeybind().setCallback(new KeyCallbackAdjustable(RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET, new KeyCallbackToggleRenderer(RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET)));
        RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.setValueChangeCallback((config) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());
        RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeybind().setCallback(new KeyCallbackAdjustable(RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY, new KeyCallbackToggleRenderer(RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY)));
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
                GuiBase.openGui(new GuiConfigs());
            }
            else if (key == Configs.Generic.SET_DISTANCE_REFERENCE_POINT.getKeybind())
            {
                Entity entity = mc.getRenderViewEntity() != null ? mc.getRenderViewEntity() : mc.player;
                DataStorage.getInstance().setDistanceReferencePoint(entity.getPositionVector());
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
                    GuiConfigs.tab = GuiConfigs.SHAPES;
                    GuiBase.openGui(new GuiShapeManager());
                }
            }

            return true;
        }
    }
}
