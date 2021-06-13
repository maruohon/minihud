package fi.dy.masa.minihud.renderer;

import net.minecraft.client.Minecraft;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.data.DataStorage;

public class OverlayRendererWaterFalls extends BaseBlockPositionListOverlayRenderer
{
    public OverlayRendererWaterFalls()
    {
        super(RendererToggle.OVERLAY_WATER_FALLS::isRendererEnabled,
              Configs.Colors.WATER_FALL_POSITIONS_OVERLAY_COLOR::getColor,
              DataStorage.getInstance()::areWaterFallPositionsDirty,
              DataStorage.getInstance()::getWaterFallPositions);
    }

    @Override
    public boolean shouldRender(Minecraft mc)
    {
        boolean render = this.enabledSupplier.getAsBoolean() && mc.world.provider.isSurfaceWorld();
        this.wasDisabled |= ! render;
        return render;
    }
}
