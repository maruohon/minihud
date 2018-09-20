package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import net.minecraft.client.renderer.BufferBuilder;
import net.minecraft.client.renderer.GLAllocation;
import net.minecraft.client.renderer.GlStateManager;
import net.minecraft.client.renderer.WorldVertexBufferUploader;

public class RenderObjectDisplayList extends RenderObjectBase
{
    protected static final WorldVertexBufferUploader VERTEX_UPLOADER = new WorldVertexBufferUploader();

    protected final int baseDisplayList;

    public RenderObjectDisplayList()
    {
        this.baseDisplayList = GLAllocation.generateDisplayLists(1);
    }

    @Override
    public void uploadData(BufferBuilder buffer)
    {
        GlStateManager.glNewList(this.baseDisplayList, GL11.GL_COMPILE);
        GlStateManager.pushMatrix();

        VERTEX_UPLOADER.draw(buffer);

        GlStateManager.popMatrix();
        GlStateManager.glEndList();
    }

    @Override
    public void draw()
    {
        GlStateManager.pushMatrix();
        GlStateManager.callList(this.baseDisplayList);
        GlStateManager.popMatrix();
    }

    @Override
    public void deleteGlResources()
    {
        GLAllocation.deleteDisplayLists(this.baseDisplayList, 1);
    }
}
