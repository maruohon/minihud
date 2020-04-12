package fi.dy.masa.minihud.renderer;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.vertex.VertexBuffer;
import net.minecraft.client.renderer.vertex.VertexFormat;
import net.minecraft.client.renderer.vertex.VertexFormatElement;

public class RenderObjectVbo extends RenderObjectBase
{
    protected final VertexBuffer vertexBuffer;
    protected final VertexFormat format;

    public RenderObjectVbo(int glMode, VertexFormat format)
    {
        super(glMode);

        this.vertexBuffer = new VertexBuffer(format);
        this.format = format;
    }

    @Override
    public void uploadData(BufferBuilder buffer)
    {
        this.vertexBuffer.uploadLater(buffer);
    }

    @Override
    public void draw(MatrixStack matrixStack)
    {
        RenderSystem.pushMatrix();

        boolean texture = false;

        // This isn't really that nice and clean, but it'll do for now...
        for (VertexFormatElement el : this.format.getElements())
        {
            if (el.getUsage() == VertexFormatElement.Usage.UV)
            {
                RenderSystem.enableTexture();
                texture = true;
                break;
            }
        }

        this.vertexBuffer.bindBuffer();
        this.format.setupBufferState(0L);
        this.vertexBuffer.draw(matrixStack.getLast().getMatrix(), this.getGlMode());
        this.format.clearBufferState();

        if (texture)
        {
            RenderSystem.disableTexture();
        }

        VertexBuffer.unbindBuffer();
        RenderSystem.popMatrix();
    }

    /*
    protected void setupArrayPointers()
    {
        GlStateManager.vertexPointer(3, GL11.GL_FLOAT, 16, 0);
        GlStateManager.colorPointer(4, GL11.GL_UNSIGNED_BYTE, 16, 12);
    }
    */

    @Override
    public void deleteGlResources()
    {
        this.vertexBuffer.close();
    }
}
