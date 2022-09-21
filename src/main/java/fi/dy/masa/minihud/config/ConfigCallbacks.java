package fi.dy.masa.minihud.config;

import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.config.option.BooleanContainingConfig;
import fi.dy.masa.malilib.input.callback.AdjustableValueHotkeyCallback;
import fi.dy.masa.malilib.listener.EventListener;
import fi.dy.masa.malilib.overlay.message.MessageHelpers;
import fi.dy.masa.malilib.overlay.message.MessageUtils;
import fi.dy.masa.malilib.render.overlay.OverlayRendererContainer;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.malilib.util.game.wrap.EntityWrap;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.feature.Actions;
import fi.dy.masa.minihud.network.carpet.CarpetPubsubPacketHandler;
import fi.dy.masa.minihud.network.servux.ServuxInfoSubDataPacketHandler;
import fi.dy.masa.minihud.renderer.RenderContainer;
import fi.dy.masa.minihud.util.DebugInfoUtils;

public class ConfigCallbacks
{
    public static void init()
    {
        Configs.Generic.OVERLAYS_RENDERING_TOGGLE.addValueChangeListener(OverlayRendererContainer.INSTANCE::setEnabledRenderersNeedUpdate);

        Configs.Hotkeys.OPEN_CONFIG_GUI.createCallbackForAction(Actions.OPEN_CONFIG_SCREEN);
        Configs.Hotkeys.OPEN_SHAPE_EDITOR.createCallbackForAction(Actions.OPEN_SHAPE_EDITOR);
        Configs.Hotkeys.OPEN_SHAPE_MANAGER.createCallbackForAction(Actions.OPEN_SHAPE_MANAGER);
        Configs.Hotkeys.SET_DISTANCE_REFERENCE_POINT.createCallbackForAction(Actions.SET_DISTANCE_REFERENCE_POINT);

        InfoLineToggle.CHUNK_UNLOAD_ORDER.getHotkeyConfig().getKeyBind().setCallback(AdjustableValueHotkeyCallback.createBitShifter(
                        InfoLineToggle.CHUNK_UNLOAD_ORDER.getBooleanConfig(), Configs.Generic.DROPPED_CHUNKS_HASH_SIZE)
                    .addAdjustListener(() -> MessageUtils.printCustomActionbarMessage("minihud.message.info.dropped_chunks_hash_size_set_to", Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue())));

        RendererToggle.CHUNK_UNLOAD_BUCKET.getKeyBind().setCallback(AdjustableValueHotkeyCallback.create(
                RendererToggle.CHUNK_UNLOAD_BUCKET.getBooleanConfig(), Configs.Internal.CHUNK_UNLOAD_BUCKET_OVERLAY_Y));

        RendererToggle.SLIME_CHUNKS.getKeyBind().setCallback(AdjustableValueHotkeyCallback.create(
                RendererToggle.SLIME_CHUNKS.getBooleanConfig(), Configs.Internal.SLIME_CHUNKS_OVERLAY_TOP_Y));

        EventListener beaconUpdateCallback = RenderContainer.BEACON_OVERLAY::setNeedsUpdate;
        EventListener lightLevelUpdateCallback = RenderContainer.LIGHT_LEVEL_OVERLAY::setNeedsUpdate;
        Configs.Colors.BEACON_RANGE_LVL1_OVERLAY_COLOR.addValueChangeListener(beaconUpdateCallback);
        Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.addValueChangeListener(beaconUpdateCallback);
        Configs.Colors.BEACON_RANGE_LVL3_OVERLAY_COLOR.addValueChangeListener(beaconUpdateCallback);
        Configs.Colors.BEACON_RANGE_LVL4_OVERLAY_COLOR.addValueChangeListener(beaconUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_COLORED_NUMBERS.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_MARKER_MODE.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_MARKER_SIZE.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_NUMBER_MODE.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_BLOCK.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_NUMBER_OFFSET_SKY.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_NUMBER_ROTATION.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_RANGE.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_THRESHOLD.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_Z_OFFSET.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.STRUCTURES_RENDER_THROUGH.setValueChangeCallback((newValue, oldValue) -> RenderContainer.STRUCTURE_BOUNDING_BOXES_OVERLAY.setRenderThrough(newValue));
        Configs.Generic.WOOL_COUNTER_TYPES.setValueLoadCallback(DataStorage.getInstance().getWoolCounters()::updateEnabledCounters);
        Configs.Generic.WOOL_COUNTER_TYPES.setValueChangeCallback((newValue, oldValue) -> {
            DataStorage.getInstance().getWoolCounters().updateEnabledCounters(newValue);
            CarpetPubsubPacketHandler.INSTANCE.updatePubSubSubscriptions();
        });

        EventListener carpetCallback = CarpetPubsubPacketHandler.INSTANCE::updatePubSubSubscriptions;
        Configs.Generic.WOOL_COUNTER_ENABLE_ALL.addValueChangeListener(carpetCallback);
        InfoLineToggle.CARPET_WOOL_COUNTERS.addValueChangeListener(carpetCallback);

        // TODO move this to a proper method somewhere which only(?) updates the registration
        // TODO for the currently detected server side counterpart(s)
        EventListener syncCallback = () -> {
            CarpetPubsubPacketHandler.INSTANCE.updatePubSubSubscriptions();
            ServuxInfoSubDataPacketHandler.INSTANCE.updateSubscriptions();
        };
        InfoLineToggle.CHUNK_UNLOAD_ORDER.addValueChangeListener(syncCallback);
        InfoLineToggle.MOB_CAPS.addValueChangeListener(syncCallback);
        InfoLineToggle.SERVER_TPS.addValueChangeListener(syncCallback);
        RendererToggle.CHUNK_UNLOAD_BUCKET.addValueChangeListener(syncCallback);

        RendererToggle.DEBUG_COLLISION_BOXES.addValueChangeListener( () -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_COLLISION_BOXES));
        RendererToggle.DEBUG_HEIGHT_MAP.addValueChangeListener(      () -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_HEIGHT_MAP));
        RendererToggle.DEBUG_NEIGHBOR_UPDATES.addValueChangeListener(() -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_NEIGHBOR_UPDATES));
        RendererToggle.DEBUG_PATH_FINDING.addValueChangeListener(    () -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_PATH_FINDING));
        RendererToggle.DEBUG_SOLID_FACES.addValueChangeListener(     () -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_SOLID_FACES));
        RendererToggle.DEBUG_WATER.addValueChangeListener(           () -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_WATER));

        RendererToggle.BEACON_RANGE.addValueChangeListener(beaconUpdateCallback);
        RendererToggle.LIGHT_LEVEL.addValueChangeListener(lightLevelUpdateCallback);
        RendererToggle.STRUCTURE_BOUNDING_BOXES.addValueChangeListener(DataStorage.getInstance().getStructureStorage()::requestStructureDataUpdates);;

        RendererToggle.CHUNK_UNLOAD_BUCKET.addEnableListener(ConfigCallbacks::onChunkUnloadBucketOverlayEnabled);
        RendererToggle.RANDOM_TICKS_FIXED.addEnableListener(RenderContainer.RANDOM_TICKS_FIXED_OVERLAY::onEnabled);
        RendererToggle.SLIME_CHUNKS.addEnableListener(RenderContainer.SLIME_CHUNKS_OVERLAY::onEnabled);
        RendererToggle.SPAWNABLE_CHUNKS_FIXED.addEnableListener(RenderContainer.SPAWNABLE_CHUNKS_FIXED_OVERLAY::onEnabled);
        RendererToggle.SPAWNABLE_CHUNKS_PLAYER.addEnableListener(RenderContainer.SPAWNABLE_CHUNKS_PLAYER_OVERLAY::onEnabled);
        RendererToggle.SPAWN_CHUNKS_REAL.addEnableListener(RenderContainer.SPAWN_CHUNKS_REAL_OVERLAY::onEnabled);

        RendererToggle.RANDOM_TICKS_FIXED.setToggleMessageFactory(ConfigCallbacks::getRandomTicksMessage);
        RendererToggle.SPAWNABLE_CHUNKS_FIXED.setToggleMessageFactory(ConfigCallbacks::getSpawnableChunksMessage);
        RendererToggle.SPAWN_CHUNKS_REAL.setToggleMessageFactory(ConfigCallbacks::getSpawnChunksMessage);
    }

    private static void onChunkUnloadBucketOverlayEnabled()
    {
        Vec3d pos = EntityWrap.getCameraEntityPosition();
        Configs.Internal.CHUNK_UNLOAD_BUCKET_OVERLAY_Y.setDoubleValue(pos.y - 2);
    }

    private static String getSpawnChunksMessage(BooleanContainingConfig<?> config)
    {
        if (config.getBooleanValue())
        {
            BlockPos spawn = DataStorage.getInstance().getWorldSpawn();
            String name = config.getPrettyName();

            return StringUtils.translate("minihud.message.info.toggled_renderer_on_using_world_spawn",
                                         name, spawn.getX(), spawn.getY(), spawn.getZ());
        }

        return MessageHelpers.getBooleanConfigToggleMessage(config, null);
    }

    private static String getSpawnableChunksMessage(BooleanContainingConfig<?> config)
    {
        if (config.getBooleanValue())
        {
            Vec3d pos = EntityWrap.getCameraEntityPosition();
            BlockPos b = new BlockPos(pos);
            String name = config.getPrettyName();
            String key = "minihud.message.info.toggled_renderer_on_using_block_position";

            return StringUtils.translate(key, name, b.getX(), b.getY(), b.getZ());
        }

        return MessageHelpers.getBooleanConfigToggleMessage(config, null);
    }

    private static String getRandomTicksMessage(BooleanContainingConfig<?> config)
    {
        if (config.getBooleanValue())
        {
            Vec3d pos = EntityWrap.getCameraEntityPosition();
            String name = config.getPrettyName();
            String strPos = String.format("x: §b%.2f§r, y: §b%.2f§r, z: §b%.2f§r", pos.x, pos.y, pos.z);
            String key = "minihud.message.info.toggled_renderer_on_using_position";

            return StringUtils.translate(key, name, strPos);
        }

        return MessageHelpers.getBooleanConfigToggleMessage(config, null);
    }
}
