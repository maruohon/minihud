package fi.dy.masa.minihud.config;

import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.input.callback.AdjustableValueHotkeyCallback;
import fi.dy.masa.malilib.input.callback.HotkeyCallback;
import fi.dy.masa.malilib.input.callback.ToggleBooleanWithMessageKeyCallback;
import fi.dy.masa.malilib.listener.EventListener;
import fi.dy.masa.malilib.overlay.message.MessageHelpers;
import fi.dy.masa.malilib.overlay.message.MessageUtils;
import fi.dy.masa.malilib.render.overlay.OverlayRendererContainer;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.hotkeys.Actions;
import fi.dy.masa.minihud.network.CarpetPubsubPacketHandler;
import fi.dy.masa.minihud.renderer.RenderContainer;
import fi.dy.masa.minihud.util.DebugInfoUtils;

public class ConfigCallbacks
{
    public static void init()
    {
        Configs.Generic.OPEN_CONFIG_GUI.getKeyBind().setCallback(HotkeyCallback.of(Actions.OPEN_CONFIG_SCREEN));
        Configs.Generic.OPEN_SHAPE_EDITOR.getKeyBind().setCallback(HotkeyCallback.of(Actions.OPEN_SHAPE_EDITOR));
        Configs.Generic.OVERLAYS_RENDERING_TOGGLE.addValueChangeListener(OverlayRendererContainer.INSTANCE::setEnabledRenderersNeedUpdate);
        Configs.Generic.SET_DISTANCE_REFERENCE_POINT.getKeyBind().setCallback(HotkeyCallback.of(Actions.SET_DISTANCE_REFERENCE_POINT));

        InfoLine.CHUNK_UNLOAD_ORDER.getHotkeyConfig().getKeyBind().setCallback(AdjustableValueHotkeyCallback.createBitShifter(
                InfoLine.CHUNK_UNLOAD_ORDER.getBooleanConfig(), Configs.Generic.DROPPED_CHUNKS_HASH_SIZE)
                    .addAdjustListener(() -> MessageUtils.printCustomActionbarMessage("minihud.message.info.dropped_chunks_hash_size_set_to", Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue())));

        RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getKeyBind().setCallback(AdjustableValueHotkeyCallback.create(
                RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getBooleanConfig(), Configs.Internal.CHUNK_UNLOAD_BUCKET_OVERLAY_Y));

        RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeyBind().setCallback(AdjustableValueHotkeyCallback.create(
                RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getBooleanConfig(), Configs.Internal.SLIME_CHUNKS_OVERLAY_TOP_Y));

        EventListener beaconUpdateCallback = RenderContainer.BEACON_OVERLAY::setNeedsUpdate;
        EventListener lightLevelUpdateCallback = RenderContainer.LIGHT_LEVEL_OVERLAY::setNeedsUpdate;
        Configs.Colors.BEACON_RANGE_LVL1_OVERLAY_COLOR.addValueChangeListener(beaconUpdateCallback);
        Configs.Colors.BEACON_RANGE_LVL2_OVERLAY_COLOR.addValueChangeListener(beaconUpdateCallback);
        Configs.Colors.BEACON_RANGE_LVL3_OVERLAY_COLOR.addValueChangeListener(beaconUpdateCallback);
        Configs.Colors.BEACON_RANGE_LVL4_OVERLAY_COLOR.addValueChangeListener(beaconUpdateCallback);
        Configs.Generic.LIGHT_LEVEL_RANGE.addValueChangeListener(lightLevelUpdateCallback);
        Configs.Generic.STRUCTURES_RENDER_THROUGH.setValueChangeCallback((newValue, oldValue) -> RenderContainer.STRUCTURE_BOUNDING_BOXES_OVERLAY.setRenderThrough(newValue));
        Configs.Generic.WOOL_COUNTER_TYPES.setValueLoadCallback(DataStorage.getInstance().getWoolCounters()::updateEnabledCounters);
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

        RendererToggle.DEBUG_COLLISION_BOXES.addValueChangeListener( () -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_COLLISION_BOXES));
        RendererToggle.DEBUG_HEIGHT_MAP.addValueChangeListener(      () -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_HEIGHT_MAP));
        RendererToggle.DEBUG_NEIGHBOR_UPDATES.addValueChangeListener(() -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_NEIGHBOR_UPDATES));
        RendererToggle.DEBUG_PATH_FINDING.addValueChangeListener(    () -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_PATH_FINDING));
        RendererToggle.DEBUG_SOLID_FACES.addValueChangeListener(     () -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_SOLID_FACES));
        RendererToggle.DEBUG_WATER.addValueChangeListener(           () -> DebugInfoUtils.toggleDebugRenderer(RendererToggle.DEBUG_WATER));

        RendererToggle.OVERLAY_BEACON_RANGE.addValueChangeListener(beaconUpdateCallback);
        RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.addValueChangeListener(pubSubCallback);
        RendererToggle.OVERLAY_LIGHT_LEVEL.addValueChangeListener(lightLevelUpdateCallback);
        RendererToggle.OVERLAY_STRUCTURE_MAIN_TOGGLE.addValueChangeListener(DataStorage.getInstance().getStructureStorage()::requestStructureDataUpdates);;

        RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.addEnableListener(ConfigCallbacks::onChunkUnloadBucketOverlayEnabled);
        RendererToggle.OVERLAY_RANDOM_TICKS_FIXED.addEnableListener(RenderContainer.RANDOM_TICKS_FIXED_OVERLAY::onEnabled);
        RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.addEnableListener(RenderContainer.SLIME_CHUNKS_OVERLAY::onEnabled);
        RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED.addEnableListener(RenderContainer.SPAWNABLE_CHUNKS_FIXED_OVERLAY::onEnabled);
        RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_PLAYER.addEnableListener(RenderContainer.SPAWNABLE_CHUNKS_PLAYER_OVERLAY::onEnabled);
        RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL.addEnableListener(RenderContainer.SPAWN_CHUNKS_REAL_OVERLAY::onEnabled);

        RendererToggle.OVERLAY_RANDOM_TICKS_FIXED.getKeyBind().setCallback(new ToggleBooleanWithMessageKeyCallback(RendererToggle.OVERLAY_RANDOM_TICKS_FIXED.getBooleanConfig(), ConfigCallbacks::getRandomTicksMessage));
        RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED.getKeyBind().setCallback(new ToggleBooleanWithMessageKeyCallback(RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED.getBooleanConfig(), ConfigCallbacks::getSpawnableChunksMessage));
        RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL.getKeyBind().setCallback(new ToggleBooleanWithMessageKeyCallback(RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL.getBooleanConfig(), ConfigCallbacks::getSpawnChunksMessage));
    }

    private static void onChunkUnloadBucketOverlayEnabled()
    {
        Vec3d pos = EntityUtils.getCameraEntityPosition();
        Configs.Internal.CHUNK_UNLOAD_BUCKET_OVERLAY_Y.setDoubleValue(pos.y - 2);
    }

    private static String getSpawnChunksMessage(BooleanConfig config)
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

    private static String getSpawnableChunksMessage(BooleanConfig config)
    {
        if (config.getBooleanValue())
        {
            Vec3d pos = EntityUtils.getCameraEntityPosition();
            BlockPos b = new BlockPos(pos);
            String name = config.getPrettyName();
            String key = "minihud.message.info.toggled_renderer_on_using_block_position";

            return StringUtils.translate(key, name, b.getX(), b.getY(), b.getZ());
        }

        return MessageHelpers.getBooleanConfigToggleMessage(config, null);
    }

    private static String getRandomTicksMessage(BooleanConfig config)
    {
        if (config.getBooleanValue())
        {
            Vec3d pos = EntityUtils.getCameraEntityPosition();
            String name = config.getPrettyName();
            String strPos = String.format("x: §b%.2f§r, y: §b%.2f§r, z: §b%.2f§r", pos.x, pos.y, pos.z);
            String key = "minihud.message.info.toggled_renderer_on_using_position";

            return StringUtils.translate(key, name, strPos);
        }

        return MessageHelpers.getBooleanConfigToggleMessage(config, null);
    }
}
