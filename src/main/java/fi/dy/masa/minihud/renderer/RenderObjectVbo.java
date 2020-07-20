package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import com.mojang.blaze3d.platform.GlStateManager;
import net.minecraft.client.gl.GlBufferRenderer;
import net.minecraft.client.gl.VertexBuffer;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormats;

public class RenderObjectVbo extends RenderObjectBase
{
    protected static final GlBufferRenderer VERTEX_UPLOADER = new GlBufferRenderer(); // MCP: VertexBufferUploader

    protected final VertexBuffer vertexBuffer;

    public RenderObjectVbo(int glMode)
    {
        super(glMode);

        this.vertexBuffer = new VertexBuffer(VertexFormats.POSITION_COLOR);
    }

    @Override
    public void uploadData(BufferBuilder buffer)
    {
        VERTEX_UPLOADER.setGlBuffer(this.vertexBuffer); // MCP: setVertexBuffer
        VERTEX_UPLOADER.draw(buffer);
    }

    @Override
    public void draw()
    {
        GlStateManager.pushMatrix();

        this.vertexBuffer.bind();
        this.setupArrayPointers();
        this.vertexBuffer.draw(this.getGlMode());

        GlStateManager.popMatrix();
    }

    protected void setupArrayPointers()
    {
        GlStateManager.vertexPointer(3, GL11.GL_FLOAT, 16, 0);
        GlStateManager.colorPointer(4, GL11.GL_UNSIGNED_BYTE, 16, 12);
    }

    @Override
    public void deleteGlResources()
    {
        this.vertexBuffer.delete();
    }
}
