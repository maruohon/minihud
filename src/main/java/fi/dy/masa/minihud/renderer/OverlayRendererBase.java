package fi.dy.masa.minihud.renderer;

import java.util.ArrayList;
import java.util.List;
import com.mojang.blaze3d.platform.GLX;
import com.mojang.blaze3d.platform.GlStateManager;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.util.math.BlockPos;

public abstract class OverlayRendererBase implements IOverlayRenderer
{
    protected static final BufferBuilder BUFFER_1 = new BufferBuilder(2097152);
    protected static final BufferBuilder BUFFER_2 = new BufferBuilder(2097152);
    protected static final BufferBuilder BUFFER_3 = new BufferBuilder(2097152);

    protected final List<RenderObjectBase> renderObjects = new ArrayList<>();
    protected float glLineWidth = 1f;
    protected BlockPos lastUpdatePos = BlockPos.ORIGIN;

    @Override
    public void draw()
    {
        GlStateManager.lineWidth(this.glLineWidth);

        for (RenderObjectBase obj : this.renderObjects)
        {
            obj.draw();
        }
    }

    @Override
    public void deleteGlResources()
    {
        for (RenderObjectBase obj : this.renderObjects)
        {
            obj.deleteGlResources();
        }

        this.renderObjects.clear();
    }

    /**
     * Allocates a new VBO or display list, adds it to the list, and returns it
     * @param glMode
     * @return
     */
    protected RenderObjectBase allocateBuffer(int glMode)
    {
        RenderObjectBase obj;

        if (GLX.useVbo())
        {
            obj = new RenderObjectVbo(glMode);
        }
        else
        {
            obj = new RenderObjectDisplayList(glMode);
        }

        this.renderObjects.add(obj);

        return obj;
    }
}
