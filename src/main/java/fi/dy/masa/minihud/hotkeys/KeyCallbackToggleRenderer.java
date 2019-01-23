package fi.dy.masa.minihud.hotkeys;

import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyAction;
import fi.dy.masa.malilib.hotkeys.KeyCallbackToggleBooleanConfigWithMessage;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.event.RenderHandler;
import fi.dy.masa.minihud.renderer.OverlayRenderer;
import fi.dy.masa.minihud.renderer.OverlayRendererRandomTickableChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererSlimeChunks;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnableChunks;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.resource.language.I18n;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Vec3d;

public class KeyCallbackToggleRenderer extends KeyCallbackToggleBooleanConfigWithMessage
{
    protected final RendererToggle rendererConfig;

    public KeyCallbackToggleRenderer(RendererToggle config)
    {
        super(config);

        this.rendererConfig = config;
    }

    @Override
    public boolean onKeyAction(KeyAction action, IKeybind key)
    {
        MinecraftClient mc = MinecraftClient.getInstance();

        if (mc != null && mc.player != null && super.onKeyAction(action, key))
        {
            if (key == RendererToggle.OVERLAY_CHUNK_UNLOAD_BUCKET.getKeybind())
            {
                OverlayRenderer.chunkUnloadBucketOverlayY = mc.player.y - 2;
            }
            else if (key == RendererToggle.OVERLAY_SLIME_CHUNKS_OVERLAY.getKeybind())
            {
                OverlayRendererSlimeChunks.overlayTopY = mc.player.y;
            }
            else if (key == RendererToggle.OVERLAY_SPAWN_CHUNK_OVERLAY_REAL.getKeybind() && this.rendererConfig.getBooleanValue())
            {
                final boolean enabled = this.config.getBooleanValue();
                String pre = enabled ? GuiBase.TXT_GREEN : GuiBase.TXT_RED;
                String status = I18n.translate("malilib.message.value." + (enabled ? "on" : "off"));
                String message = I18n.translate("malilib.message.toggled", this.config.getPrettyName(), pre + status + GuiBase.TXT_RST);

                BlockPos spawn = mc.world.getSpawnPos();
                RenderHandler.getInstance().getDataStorage().setWorldSpawn(spawn);
                String str = String.format(", using the world spawn x: %d, y: %d, z: %d", spawn.getX(), spawn.getY(), spawn.getZ());

                StringUtils.printActionbarMessage(message + str);
            }
            else if (key == RendererToggle.OVERLAY_RANDOM_TICKS_FIXED.getKeybind() && this.rendererConfig.getBooleanValue())
            {
                final boolean enabled = this.config.getBooleanValue();
                String pre = enabled ? GuiBase.TXT_GREEN : GuiBase.TXT_RED;
                String status = I18n.translate("malilib.message.value." + (enabled ? "on" : "off"));
                String message = I18n.translate("malilib.message.toggled", this.config.getPrettyName(), pre + status + GuiBase.TXT_RST);

                Vec3d pos = mc.player.getPosVector();
                OverlayRendererRandomTickableChunks.newPos = pos;
                String str = String.format(", using the position x: %.2f, y: %.2f, z: %.2f", pos.x, pos.y, pos.z);

                StringUtils.printActionbarMessage(message + str);
            }
            else if (key == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_PLAYER.getKeybind() && this.rendererConfig.getBooleanValue())
            {
                OverlayRendererSpawnableChunks.overlayTopY = mc.player.y;
            }
            else if (key == RendererToggle.OVERLAY_SPAWNABLE_CHUNKS_FIXED.getKeybind() && this.rendererConfig.getBooleanValue())
            {
                final boolean enabled = this.config.getBooleanValue();
                String pre = enabled ? GuiBase.TXT_GREEN : GuiBase.TXT_RED;
                String status = I18n.translate("malilib.message.value." + (enabled ? "on" : "off"));
                String message = I18n.translate("malilib.message.toggled", this.config.getPrettyName(), pre + status + GuiBase.TXT_RST);

                BlockPos pos = new BlockPos(mc.player);
                OverlayRendererSpawnableChunks.newPos = pos;
                OverlayRendererSpawnableChunks.overlayTopY = mc.player.y;
                String str = String.format(", using the position x: %d, y: %d, z: %d", pos.getX(), pos.getY(), pos.getZ());

                StringUtils.printActionbarMessage(message + str);
            }

            return true;
        }

        return false;
    }
}
