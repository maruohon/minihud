package fi.dy.masa.minihud.renderer;

import org.lwjgl.opengl.GL11;
import com.mojang.blaze3d.platform.GlStateManager;
import net.minecraft.client.render.BufferBuilder;
import net.minecraft.client.render.BufferRenderer;
import net.minecraft.client.util.GlAllocationUtils;

public class RenderObjectDisplayList extends RenderObjectBase
{
    protected static final BufferRenderer VERTEX_UPLOADER = new BufferRenderer();

    protected final int baseDisplayList;

    public RenderObjectDisplayList(int glMode)
    {
        super(glMode);

        this.baseDisplayList = GlAllocationUtils.genLists(1);
    }

    @Override
    public void uploadData(BufferBuilder buffer)
    {
        GlStateManager.newList(this.baseDisplayList, GL11.GL_COMPILE);
        GlStateManager.pushMatrix();

        VERTEX_UPLOADER.draw(buffer);

        GlStateManager.popMatrix();
        GlStateManager.endList();
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
        GlAllocationUtils.deleteSingletonList(this.baseDisplayList);
    }
}
