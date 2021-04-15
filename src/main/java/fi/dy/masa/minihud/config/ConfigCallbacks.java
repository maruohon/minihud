package fi.dy.masa.minihud.config;

import fi.dy.masa.malilib.input.callback.AdjustableValueHotkeyCallback;
import fi.dy.masa.malilib.input.callback.HotkeyCallback;
import fi.dy.masa.malilib.listener.EventListener;
import fi.dy.masa.malilib.overlay.message.MessageUtils;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.hotkeys.Actions;
import fi.dy.masa.minihud.network.CarpetPubsubPacketHandler;
import fi.dy.masa.minihud.renderer.OverlayRendererBeaconRange;
import fi.dy.masa.minihud.renderer.OverlayRendererLightLevel;
import fi.dy.masa.minihud.renderer.OverlayRendererStructures;

public class ConfigCallbacks
{
    public static void init()
    {
        Configs.Generic.OPEN_CONFIG_GUI.getKeyBind().setCallback(HotkeyCallback.of(Actions.OPEN_CONFIG_SCREEN));
        Configs.Generic.OPEN_SHAPE_EDITOR.getKeyBind().setCallback(HotkeyCallback.of(Actions.OPEN_SHAPE_EDITOR));
        Configs.Generic.SET_DISTANCE_REFERENCE_POINT.getKeyBind().setCallback(HotkeyCallback.of(Actions.SET_DISTANCE_REFERENCE_POINT));

        InfoLine.CHUNK_UNLOAD_ORDER.getHotkeyConfig().getKeyBind().setCallback(AdjustableValueHotkeyCallback.createBitShifter(
                InfoLine.CHUNK_UNLOAD_ORDER.getBooleanConfig(), Configs.Generic.DROPPED_CHUNKS_HASH_SIZE)
                    .addAdjustListener(() -> MessageUtils.printCustomActionbarMessage("minihud.message.dropped_chunks_hash_size_set_to", Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue())));

        RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getKeyBind().setCallback(AdjustableValueHotkeyCallback.create(
                RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getBooleanConfig(), Configs.Internal.CHUNK_UNLOAD_BUCKET_OVERLAY_Y));

        RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeyBind().setCallback(AdjustableValueHotkeyCallback.create(
                RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanConfig(), Configs.Internal.SLIME_CHUNKS_OVERLAY_TOP_Y));

        EventListener beaconColorCallback = OverlayRendererBeaconRange::setNeedsUpdate;
        Configs.Colors.BEACON_RANGE_LVL1_OVERLAY_COLOR.addValueChangeListener(beaconColorCallback);
        Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.addValueChangeListener(beaconColorCallback);
        Configs.Colors.BEACON_RANGE_LVL3_OVERLAY_COLOR.addValueChangeListener(beaconColorCallback);
        Configs.Colors.BEACON_RANGE_LVL4_OVERLAY_COLOR.addValueChangeListener(beaconColorCallback);
        Configs.Generic.LIGHT_LEVEL_RANGE.addValueChangeListener(OverlayRendererLightLevel::setNeedsUpdate);
        Configs.Generic.STRUCTURES_RENDER_THROUGH.setValueChangeCallback((newValue, oldValue) -> OverlayRendererStructures.instance.setRenderThrough(newValue));
        Configs.Generic.WOOL_COUNTER_TYPES.setValueLoadCallback((newValue) -> DataStorage.getInstance().getWoolCounters().updateEnabledCounters(newValue));
        Configs.Generic.WOOL_COUNTER_TYPES.setValueChangeCallback((newValue, oldValue) -> {
            DataStorage.getInstance().getWoolCounters().updateEnabledCounters(newValue);
            CarpetPubsubPacketHandler.updatePubsubSubscriptions();
        });

        EventListener pubSubCallback = CarpetPubsubPacketHandler::updatePubsubSubscriptions;
        Configs.Generic.WOOL_COUNTER_ENABLE_ALL.addValueChangeListener(pubSubCallback);
        InfoLine.CARPET_WOOL_COUNTERS.addValueChangeListener(pubSubCallback);
        InfoLine.CHUNK_UNLOAD_ORDER.addValueChangeListener(pubSubCallback);
        InfoLine.MOB_CAPS.addValueChangeListener(pubSubCallback);
        InfoLine.SERVER_TPS.addValueChangeListener(pubSubCallback);
        RendererToggle.OVERLAY_BEACON_RANGE.addValueChangeListener(OverlayRendererBeaconRange::setNeedsUpdate);
        RendererToggle.OVERLAY_LIGHT_LEVEL.addValueChangeListener(OverlayRendererLightLevel::setNeedsUpdate);
        RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.addValueChangeListener(pubSubCallback);
    }
}
