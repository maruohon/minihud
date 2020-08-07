package fi.dy.masa.minihud.hotkeys;

import net.minecraft.client.Minecraft;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.gui.BaseScreen;
import fi.dy.masa.malilib.input.KeyBind;
import fi.dy.masa.malilib.input.KeyAction;
import fi.dy.masa.malilib.input.callback.ToggleBooleanWithMessageKeyCallback;
import fi.dy.masa.malilib.message.MessageUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.renderer.OverlayRenderer;
import fi.dy.masa.minihud.renderer.OverlayRendererBeaconRange;
import fi.dy.masa.minihud.renderer.OverlayRendererLightLevel;
import fi.dy.masa.minihud.renderer.OverlayRendererRandomTickableChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererSlimeChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnableChunks;

public class RendererToggleKeyCallback extends ToggleBooleanWithMessageKeyCallback
{
    public RendererToggleKeyCallback(BooleanConfig config)
    {
        super(config);
    }

    @Override
    public boolean onKeyAction(KeyAction action, KeyBind key)
    {
        Minecraft mc = Minecraft.getMinecraft();

        if (mc != null && mc.player != null && super.onKeyAction(action, key))
        {
            if (this.config.getBooleanValue() == false)
            {
                return true;
            }

            Entity entity = mc.getRenderViewEntity() != null ? mc.getRenderViewEntity() : mc.player;
            String green = BaseScreen.TXT_GREEN;
            String rst = BaseScreen.TXT_RST;
            String strStatus = green + StringUtils.translate("malilib.message.value.on") + rst;

            if (key == RendererToggle.OVERLAY_BEACON_RANGE.getKeyBind())
            {
                OverlayRendererBeaconRange.setNeedsUpdate();
            }
            else if (key == RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getKeyBind())
            {
                OverlayRenderer.chunkUnloadBucketOverlayY = entity.posY - 2;
            }
            else if (key == RendererToggle.OVERLAY_LIGHT_LEVEL.getKeyBind())
            {
                OverlayRendererLightLevel.setNeedsUpdate();
            }
            else if (key == RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeyBind())
            {
                OverlayRendererSlimeChunks.overlayTopY = entity.posY;
            }
            else if (key == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL.getKeyBind())
            {
                OverlayRendererSpawnChunks.setNeedsUpdate();

                BlockPos spawn = DataStorage.getInstance().getWorldSpawn();
                String strPos = String.format("x: %d, y: %d, z: %d", spawn.getX(), spawn.getY(), spawn.getZ());
                String message = StringUtils.translate("minihud.message.toggled_using_world_spawn", this.config.getPrettyName(), strStatus, strPos);

                MessageUtils.printActionbarMessage(message);
            }
            else if (key == RendererToggle.OVERLAY_RANDOM_TICKS_FIXED.getKeyBind())
            {
                Vec3d pos = entity.getPositionVector();
                OverlayRendererRandomTickableChunks.newPos = pos;
                String strPos = String.format("x: %.2f, y: %.2f, z: %.2f", pos.x, pos.y, pos.z);
                String message = StringUtils.translate("minihud.message.toggled_using_position", this.config.getPrettyName(), strStatus, strPos);

                MessageUtils.printActionbarMessage(message);
            }
            else if (key == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_PLAYER.getKeyBind())
            {
                OverlayRendererSpawnableChunks.overlayTopY = entity.posY;
            }
            else if (key == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED.getKeyBind())
            {
                BlockPos pos = new BlockPos(entity);
                OverlayRendererSpawnableChunks.newPos = pos;
                OverlayRendererSpawnableChunks.overlayTopY = entity.posY;
                String strPos = String.format("x: %d, y: %d, z: %d", pos.getX(), pos.getY(), pos.getZ());
                String message = StringUtils.translate("minihud.message.toggled_using_position", this.config.getPrettyName(), strStatus, strPos);

                MessageUtils.printActionbarMessage(message);
            }

            return true;
        }

        return false;
    }
}
