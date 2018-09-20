package fi.dy.masa.minihud.renderer;

import net.minecraft.client.renderer.BufferBuilder;

public abstract class RenderObjectBase
{
    public abstract void uploadData(BufferBuilder buffer);

    public abstract void draw();

    public abstract void deleteGlResources();
}
