package minihud.renderer;

import malilib.util.game.wrap.GameUtils;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.data.DataStorage;

public class OverlayRendererSpawnerPositions extends BaseBlockPositionListOverlayRenderer
{
    public OverlayRendererSpawnerPositions()
    {
        super(RendererToggle.SPAWNER_POSITIONS::isRendererEnabled,
              Configs.Colors.SPAWNER_POSITIONS_OVERLAY_COLOR::getColor,
              DataStorage.INSTANCE.worldGenPositions::areSpawnerPositionsDirty,
              DataStorage.INSTANCE.worldGenPositions::getSpawnerPositions);
    }

    @Override
    public boolean shouldRender()
    {
        boolean render = this.enabledSupplier.getAsBoolean() && GameUtils.getClientWorld().provider.isSurfaceWorld();
        this.wasDisabled |= (render == false);
        return render;
    }
}
