package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import com.mojang.blaze3d.platform.GlStateManager;
import net.minecraft.class_294;
import net.minecraft.client.gl.GlBuffer;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormats;

public class RenderObjectVbo extends RenderObjectBase
{
    protected static final class_294 VERTEX_UPLOADER = new class_294(); // MCP: VertexBufferUploader

    protected final GlBuffer vertexBuffer;

    public RenderObjectVbo(int glMode)
    {
        super(glMode);

        this.vertexBuffer = new GlBuffer(VertexFormats.POSITION_COLOR);
    }

    @Override
    public void uploadData(BufferBuilder buffer)
    {
        VERTEX_UPLOADER.method_1372(this.vertexBuffer); // MCP: setVertexBuffer
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
