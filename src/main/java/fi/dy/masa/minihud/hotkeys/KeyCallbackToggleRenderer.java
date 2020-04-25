package fi.dy.masa.minihud.hotkeys;

import net.minecraft.client.MinecraftClient;
import net.minecraft.entity.Entity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;
import fi.dy.masa.malilib.config.IConfigBoolean;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.malilib.hotkeys.KeyCallbackToggleBooleanConfigWithMessage;
import fi.dy.masa.malilib.util.InfoUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.renderer.OverlayRendererLightLevel;
import fi.dy.masa.minihud.renderer.OverlayRendererRandomTickableChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererRegion;
import fi.dy.masa.minihud.renderer.OverlayRendererSlimeChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnChunks;
import fi.dy.masa.minihud.util.DataStorage;

public class KeyCallbackToggleRenderer extends KeyCallbackToggleBooleanConfigWithMessage
{
    public KeyCallbackToggleRenderer(IConfigBoolean config)
    {
        super(config);
    }

    @Override
    public boolean onKeyAction(KeyAction action, IKeybind key)
    {
        MinecraftClient mc = MinecraftClient.getInstance();

        if (mc != null && mc.player != null && super.onKeyAction(action, key))
        {
            if (this.config.getBooleanValue() == false)
            {
                return true;
            }

            Entity entity = mc.getCameraEntity() != null ? mc.getCameraEntity() : mc.player;
            String green = GuiBase.TXT_GREEN;
            String rst = GuiBase.TXT_RST;
            String strStatus = green + StringUtils.translate("malilib.message.value.on") + rst;

            if (key == RendererToggle.OVERLAY_LIGHT_LEVEL.getKeybind())
            {
                OverlayRendererLightLevel.setNeedsUpdate();
            }
            else if (key == RendererToggle.OVERLAY_RANDOM_TICKS_PLAYER.getKeybind())
            {
                OverlayRendererRandomTickableChunks.setNeedsUpdate();
            }
            else if (key == RendererToggle.OVERLAY_REGION_FILE.getKeybind())
            {
                OverlayRendererRegion.setNeedsUpdate();
            }
            else if (key == RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeybind())
            {
                OverlayRendererSlimeChunks.overlayTopY = entity.getY();
                OverlayRendererSlimeChunks.setNeedsUpdate();
            }
            else if (key == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER.getKeybind())
            {
                OverlayRendererSpawnChunks.setNeedsUpdate();
            }
            else if (key == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL.getKeybind())
            {
                OverlayRendererSpawnChunks.setNeedsUpdate();

                BlockPos spawn = DataStorage.getInstance().getWorldSpawn();
                String strPos = String.format("x: %d, y: %d, z: %d", spawn.getX(), spawn.getY(), spawn.getZ());
                String message = StringUtils.translate("minihud.message.toggled_using_world_spawn", this.config.getPrettyName(), strStatus, strPos);

                InfoUtils.printActionbarMessage(message);
            }
            else if (key == RendererToggle.OVERLAY_RANDOM_TICKS_FIXED.getKeybind())
            {
                Vec3d pos = entity.getPos();
                OverlayRendererRandomTickableChunks.newPos = pos;
                String strPos = String.format("x: %.2f, y: %.2f, z: %.2f", pos.x, pos.y, pos.z);
                String message = StringUtils.translate("minihud.message.toggled_using_position", this.config.getPrettyName(), strStatus, strPos);

                InfoUtils.printActionbarMessage(message);
            }

            return true;
        }

        return false;
    }
}
