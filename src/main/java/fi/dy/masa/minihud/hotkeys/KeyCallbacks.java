package fi.dy.masa.minihud.hotkeys;

import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.gui.config.BaseConfigScreen;
import fi.dy.masa.malilib.input.callback.AdjustableKeyCallback;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.InfoLine;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.gui.ConfigScreen;
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
        Configs.Generic.OPEN_CONFIG_GUI.getKeyBind().setCallback((a, k) -> {
            BaseScreen screen = BaseConfigScreen.getCurrentTab(Reference.MOD_ID) == ConfigScreen.SHAPES ? new GuiShapeManager() : ConfigScreen.create(null);
            BaseScreen.openScreen(screen);
            return true;
        });

        Configs.Generic.SET_DISTANCE_REFERENCE_POINT.getKeyBind().setCallback((a, k) -> DataStorage.getInstance().setDistanceReferencePoint());

        Configs.Generic.SHAPE_EDITOR.getKeyBind().setCallback((a, k) -> {
            ShapeBase shape = ShapeManager.INSTANCE.getSelectedShape();
            if (shape != null) { BaseScreen.openScreen(new GuiShapeEditor(shape)); }
            else { BaseScreen.openScreen(new GuiShapeManager()); }
            return true;
        });

        Configs.Generic.WOOL_COUNTER_TYPES.setValueChangeCallback((newValue, oldValue) -> DataStorage.getInstance().getWoolCounters().updateEnabledCounters(newValue));
        Configs.Generic.WOOL_COUNTER_TYPES.setValueLoadCallback((newValue) -> DataStorage.getInstance().getWoolCounters().updateEnabledCounters(newValue));

        Configs.Colors.BEACON_RANGE_LVL1_OVERLAY_COLOR.setValueChangeCallback((newValue, oldValue) -> OverlayRendererBeaconRange.setNeedsUpdate());
        Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.setValueChangeCallback((newValue, oldValue) -> OverlayRendererBeaconRange.setNeedsUpdate());
        Configs.Colors.BEACON_RANGE_LVL3_OVERLAY_COLOR.setValueChangeCallback((newValue, oldValue) -> OverlayRendererBeaconRange.setNeedsUpdate());
        Configs.Colors.BEACON_RANGE_LVL4_OVERLAY_COLOR.setValueChangeCallback((newValue, oldValue) -> OverlayRendererBeaconRange.setNeedsUpdate());
        Configs.Generic.LIGHT_LEVEL_RANGE.setValueChangeCallback((newValue, oldValue) -> OverlayRendererLightLevel.setNeedsUpdate());
        Configs.Generic.STRUCTURES_RENDER_THROUGH.setValueChangeCallback((newValue, oldValue) -> OverlayRendererStructures.instance.setRenderThrough(newValue));
        Configs.Generic.WOOL_COUNTER_ENABLE_ALL.setValueChangeCallback((newValue, oldValue) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());
        Configs.Generic.WOOL_COUNTER_TYPES.setValueChangeCallback((newValue, oldValue) -> {
            DataStorage.getInstance().getWoolCounters().updateEnabledCounters(newValue);
            CarpetPubsubPacketHandler.updatePubsubSubscriptions();
        });

        InfoLine.CARPET_WOOL_COUNTERS.setValueChangeCallback((newValue, oldValue) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());
        InfoLine.CHUNK_UNLOAD_ORDER.getHotkeyConfig().getKeyBind().setCallback(new AdjustableKeyCallback(InfoLine.CHUNK_UNLOAD_ORDER.getBooleanConfig(), null));
        InfoLine.CHUNK_UNLOAD_ORDER.setValueChangeCallback((newValue, oldValue) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());
        InfoLine.MOB_CAPS.setValueChangeCallback((newValue, oldValue) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());
        InfoLine.SERVER_TPS.setValueChangeCallback((newValue, oldValue) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());

        RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getHotkeyConfig().getKeyBind().setCallback(new AdjustableKeyCallback(RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getBooleanConfig(), new RendererToggleKeyCallback(RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getBooleanConfig())));
        RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.setValueChangeCallback((newValue, oldValue) -> CarpetPubsubPacketHandler.updatePubsubSubscriptions());
        RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getHotkeyConfig().getKeyBind().setCallback(new AdjustableKeyCallback(RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanConfig(), new RendererToggleKeyCallback(RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanConfig())));
    }
}
