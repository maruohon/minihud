package fi.dy.masa.minihud.renderer;

import java.util.function.Supplier;
import org.joml.Matrix4f;
import net.minecraft.client.gl.ShaderProgram;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.util.math.MatrixStack;

public abstract class RenderObjectBase
{
    protected final VertexFormat.DrawMode glMode;
    protected final Supplier<ShaderProgram> shader;

    public RenderObjectBase(VertexFormat.DrawMode glMode, Supplier<ShaderProgram> shader)
    {
        this.glMode = glMode;
        this.shader = shader;
    }

    public VertexFormat.DrawMode getGlMode()
    {
        return this.glMode;
    }

    public Supplier<ShaderProgram> getShader()
    {
        return this.shader;
    }

    public abstract void uploadData(BufferBuilder buffer);

    public abstract void draw(MatrixStack matrixStack, Matrix4f projMatrix);

    public abstract void deleteGlResources();
}
