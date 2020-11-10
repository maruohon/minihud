package fi.dy.masa.minihud.renderer;

import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormat;

public abstract class RenderObjectBase
{
    protected final VertexFormat.DrawMode glMode;

    public RenderObjectBase(VertexFormat.DrawMode glMode)
    {
        this.glMode = glMode;
    }

    public VertexFormat.DrawMode getGlMode()
    {
        return this.glMode;
    }

    public abstract void uploadData(BufferBuilder buffer);

    public abstract void draw(net.minecraft.client.util.math.MatrixStack matrixStack);

    public abstract void deleteGlResources();
}
