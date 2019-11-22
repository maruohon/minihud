package fi.dy.masa.minihud.renderer;

import com.mojang.blaze3d.platform.GlStateManager;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.gl.VertexBuffer;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormat;
import net.minecraft.client.render.VertexFormatElement;

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
        this.vertexBuffer.submitUpload(buffer);
    }

    @Override
    public void draw(net.minecraft.client.util.math.MatrixStack matrixStack)
    {
        GlStateManager.pushMatrix();

        boolean texture = false;

        // This isn't really that nice and clean, but it'll do for now...
        for (VertexFormatElement el : this.format.getElements())
        {
            if (el.getType() == VertexFormatElement.Type.UV)
            {
                RenderSystem.enableTexture();
                texture = true;
                break;
            }
        }

        this.vertexBuffer.bind();
        this.format.startDrawing(0L);
        this.vertexBuffer.draw(matrixStack.peek().getModel(), this.getGlMode());
        this.format.endDrawing();

        if (texture)
        {
            RenderSystem.disableTexture();
        }

        VertexBuffer.unbind();
        GlStateManager.popMatrix();
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
