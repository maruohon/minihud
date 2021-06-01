package fi.dy.masa.minihud.renderer;

import java.util.function.Supplier;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.Shader;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.util.math.MatrixStack;
import net.minecraft.util.math.Matrix4f;

public abstract class RenderObjectBase
{
    protected final VertexFormat.DrawMode glMode;
    protected final Supplier<Shader> shader;

    public RenderObjectBase(VertexFormat.DrawMode glMode, Supplier<Shader> shader)
    {
        this.glMode = glMode;
        this.shader = shader;
    }

    public VertexFormat.DrawMode getGlMode()
    {
        return this.glMode;
    }

    public Supplier<Shader> getShader()
    {
        return shader;
    }

    public abstract void uploadData(BufferBuilder buffer);

    public abstract void draw(MatrixStack matrixStack, Matrix4f projMatrix);

    public abstract void deleteGlResources();
}
