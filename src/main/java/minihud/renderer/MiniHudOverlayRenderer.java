package minihud.renderer;

import java.nio.file.Path;
import javax.annotation.Nullable;

import malilib.render.overlay.BaseOverlayRenderer;
import malilib.util.data.ModInfo;
import minihud.Reference;
import minihud.config.Configs;
import minihud.event.ClientWorldChangeHandler;

public abstract class MiniHudOverlayRenderer extends BaseOverlayRenderer
{
    @Override
    public ModInfo getModInfo()
    {
        return Reference.MOD_INFO;
    }

    @Override
    public boolean isEnabled()
    {
        return Configs.Generic.OVERLAYS_RENDERING_TOGGLE.getBooleanValue();
    }

    @Override
    public void onEnabled()
    {
        if (this.shouldRender())
        {
            this.setNeedsUpdate();
        }
    }

    @Nullable
    @Override
    public Path getSaveFile(boolean isDimensionChangeOnly)
    {
        return ClientWorldChangeHandler.getCurrentStorageFile(! isDimensionChangeOnly);
    }
}
