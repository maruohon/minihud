package fi.dy.masa.minihud.renderer;

import java.util.function.Supplier;
import com.mojang.blaze3d.systems.RenderSystem;
import org.joml.Matrix4f;
import net.minecraft.client.gl.ShaderProgram;
import net.minecraft.client.gl.VertexBuffer;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.render.VertexFormatElement;
import net.minecraft.client.util.math.MatrixStack;

public class RenderObjectVbo extends RenderObjectBase
{
    protected final VertexBuffer vertexBuffer;
    protected final VertexFormat format;
    protected final boolean hasTexture;

    public RenderObjectVbo(VertexFormat.DrawMode glMode, VertexFormat format, Supplier<ShaderProgram> shader)
    {
        super(glMode, shader);

        this.vertexBuffer = new VertexBuffer(VertexBuffer.Usage.STATIC);
        this.format = format;

        boolean hasTexture = false;

        // This isn't really that nice and clean, but it'll do for now...
        for (VertexFormatElement el : this.format.getElements())
        {
            if (el.getType() == VertexFormatElement.Type.UV)
            {
                hasTexture = true;
                break;
            }
        }

        this.hasTexture = hasTexture;
    }

    @Override
    public void uploadData(BufferBuilder buffer)
    {
        BufferBuilder.BuiltBuffer builtBuffer = buffer.end();
        this.vertexBuffer.bind();
        this.vertexBuffer.upload(builtBuffer);
        VertexBuffer.unbind();
    }

    @Override
    public void draw(MatrixStack matrixStack, Matrix4f projMatrix)
    {
        RenderSystem.setShader(this.getShader());
        this.vertexBuffer.bind();
        this.vertexBuffer.draw(matrixStack.peek().getPositionMatrix(), projMatrix, this.getShader().get());

        VertexBuffer.unbind();
    }

    @Override
    public void deleteGlResources()
    {
        this.vertexBuffer.close();
    }
}
