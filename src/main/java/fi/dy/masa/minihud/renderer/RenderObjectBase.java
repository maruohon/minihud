package fi.dy.masa.minihud.renderer;

import com.mojang.blaze3d.matrix.MatrixStack;
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

    public abstract void draw(MatrixStack matrixStack);

    public abstract void deleteGlResources();
}
