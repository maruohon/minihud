package fi.dy.masa.minihud.renderer;

import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormat;

public abstract class RenderObjectBase
{
    protected final VertexFormat.class_5596 glMode;

    public RenderObjectBase(VertexFormat.class_5596 glMode)
    {
        this.glMode = glMode;
    }

    public VertexFormat.class_5596 getGlMode()
    {
        return this.glMode;
    }

    public abstract void uploadData(BufferBuilder buffer);

    public abstract void draw(net.minecraft.client.util.math.MatrixStack matrixStack);

    public abstract void deleteGlResources();
}
