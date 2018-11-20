package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.VertexBufferUploader;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.client.renderer.vertex.VertexBuffer;

public class RenderObjectVbo extends RenderObjectBase
{
    protected static final VertexBufferUploader VERTEX_UPLOADER = new VertexBufferUploader();

    protected final VertexBuffer vertexBuffer;

    public RenderObjectVbo(int glMode)
    {
        super(glMode);

        this.vertexBuffer = new VertexBuffer(DefaultVertexFormats.POSITION_COLOR);
    }

    @Override
    public void uploadData(BufferBuilder buffer)
    {
        VERTEX_UPLOADER.setVertexBuffer(this.vertexBuffer);
        VERTEX_UPLOADER.draw(buffer);
    }

    @Override
    public void draw()
    {
        GlStateManager.pushMatrix();

        this.vertexBuffer.bindBuffer();
        this.setupArrayPointers();
        this.vertexBuffer.drawArrays(this.getGlMode());

        GlStateManager.popMatrix();
    }

    protected void setupArrayPointers()
    {
        GlStateManager.glVertexPointer(3, GL11.GL_FLOAT, 16, 0);
        GlStateManager.glColorPointer(4, GL11.GL_UNSIGNED_BYTE, 16, 12);
    }

    @Override
    public void deleteGlResources()
    {
        this.vertexBuffer.deleteGlBuffers();
    }
}
