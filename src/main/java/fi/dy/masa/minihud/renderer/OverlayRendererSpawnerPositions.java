package fi.dy.masa.minihud.renderer;

import net.minecraft.client.Minecraft;

import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;

public class OverlayRendererSpawnerPositions extends BaseBlockPositionListOverlayRenderer
{
    public OverlayRendererSpawnerPositions()
    {
        super(RendererToggle.SPAWNER_POSITIONS::isRendererEnabled,
              Configs.Colors.SPAWNER_POSITIONS_OVERLAY_COLOR::getColor,
              DataStorage.getInstance()::areSpawnerPositionsDirty,
              DataStorage.getInstance()::getSpawnerPositions);
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        boolean render = this.enabledSupplier.getAsBoolean() && mc.world.provider.isSurfaceWorld();
        this.wasDisabled |= ! render;
        return render;
    }
}
