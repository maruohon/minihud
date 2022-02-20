package fi.dy.masa.minihud.hotkeys;

import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.input.ActionResult;
import fi.dy.masa.malilib.input.KeyAction;
import fi.dy.masa.malilib.input.KeyBind;
import fi.dy.masa.malilib.input.callback.ToggleBooleanWithMessageKeyCallback;
import fi.dy.masa.malilib.overlay.message.MessageUtils;
import fi.dy.masa.malilib.util.EntityUtils;
import fi.dy.masa.malilib.util.GameUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.renderer.OverlayRendererRandomTickableChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnableChunks;

public class RendererToggleHotkeyCallback extends ToggleBooleanWithMessageKeyCallback
{
    public RendererToggleHotkeyCallback(BooleanConfig config)
    {
        super(config);
    }

    @Override
    public ActionResult onKeyAction(KeyAction action, KeyBind key)
    {
        if (GameUtils.getClientPlayer() != null && super.onKeyAction(action, key) == ActionResult.SUCCESS)
        {
            if (this.config.getBooleanValue() == false)
            {
                return ActionResult.SUCCESS;
            }

            Vec3d pos = EntityUtils.getCameraEntityPosition();

            if (key == RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getKeyBind())
            {
                Configs.Internal.CHUNK_UNLOAD_BUCKET_OVERLAY_Y.setDoubleValue(pos.y - 2);
            }
            else if (key == RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeyBind())
            {
                Configs.Internal.SLIME_CHUNKS_OVERLAY_TOP_Y.setDoubleValue(pos.y);
            }
            else if (key == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL.getKeyBind())
            {
                OverlayRendererSpawnChunks.setNeedsUpdate();

                BlockPos spawn = DataStorage.getInstance().getWorldSpawn();
                String message = StringUtils.translate("minihud.message.info.toggled_renderer_on_using_world_spawn",
                                                       this.config.getPrettyName(), spawn.getX(), spawn.getY(), spawn.getZ());

                MessageUtils.printCustomActionbarMessage(message);
            }
            else if (key == RendererToggle.OVERLAY_RANDOM_TICKS_FIXED.getKeyBind())
            {
                OverlayRendererRandomTickableChunks.newPos = pos;
                String strPos = String.format("x: %.2f, y: %.2f, z: %.2f", pos.x, pos.y, pos.z);
                String message = StringUtils.translate("minihud.message.info.toggled_renderer_on_using_position",
                                                       this.config.getPrettyName(), strPos);

                MessageUtils.printCustomActionbarMessage(message);
            }
            else if (key == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_PLAYER.getKeyBind())
            {
                OverlayRendererSpawnableChunks.overlayTopY = pos.y;
            }
            else if (key == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED.getKeyBind())
            {
                BlockPos b = new BlockPos(pos);
                OverlayRendererSpawnableChunks.newPos = b;
                OverlayRendererSpawnableChunks.overlayTopY = pos.y;
                String message = StringUtils.translate("minihud.message.info.toggled_renderer_on_using_block_position",
                                                       this.config.getPrettyName(), b.getX(), b.getY(), b.getZ());

                MessageUtils.printCustomActionbarMessage(message);
            }

            return ActionResult.SUCCESS;
        }

        return ActionResult.PASS;
    }
}
