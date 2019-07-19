package fi.dy.masa.minihud.renderer;

import net.minecraft.client.renderer.BufferBuilder;

public abstract class RenderObjectBase
{
    protected final int glMode;

    public RenderObjectBase(int glMode)
    {
        this.glMode = glMode;
    }

    public int getGlMode()
    {
        return this.glMode;
    }

    public abstract void uploadData(BufferBuilder buffer);

    public abstract void draw();

    public abstract void deleteGlResources();
}
