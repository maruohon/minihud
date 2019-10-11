package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import com.mojang.blaze3d.platform.GlStateManager;
import net.minecraft.class_4587;
import net.minecraft.client.gl.GlBuffer;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.VertexFormats;

public class RenderObjectVbo extends RenderObjectBase
{
    protected final GlBuffer vertexBuffer;

    public RenderObjectVbo(int glMode)
    {
        super(glMode);

        this.vertexBuffer = new GlBuffer(VertexFormats.POSITION_COLOR);
    }

    @Override
    public void uploadData(BufferBuilder buffer)
    {
        this.vertexBuffer.method_22643(buffer);
    }

    @Override
    public void draw(class_4587 matrixQueue)
    {
        GlStateManager.pushMatrix();

        this.vertexBuffer.bind();
        this.setupArrayPointers();
        this.vertexBuffer.draw(matrixQueue.method_22910(), this.getGlMode());
        GlBuffer.unbind();

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
