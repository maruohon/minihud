package minihud.renderer;

import malilib.util.game.wrap.GameUtils;
import minihud.config.Configs;
import minihud.config.RendererToggle;
import minihud.data.DataStorage;

public class OverlayRendererWaterFalls extends BaseBlockPositionListOverlayRenderer
{
    public OverlayRendererWaterFalls()
    {
        super(RendererToggle.WATER_FALLS::isRendererEnabled,
              Configs.Colors.WATER_FALL_POSITIONS_OVERLAY_COLOR::getColor,
              DataStorage.INSTANCE.worldGenPositions::areWaterFallPositionsDirty,
              DataStorage.INSTANCE.worldGenPositions::getWaterFallPositions);
    }

    @Override
    public boolean shouldRender()
    {
        boolean render = this.enabledSupplier.getAsBoolean() && GameUtils.getClientWorld().provider.isSurfaceWorld();
        this.wasDisabled |= (render == false);
        return render;
    }
}
